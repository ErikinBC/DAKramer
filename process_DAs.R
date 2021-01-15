############################
# --- PROCESS DA FILES --- #

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','data.table') 
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}
library(bit64)

dir_base <- getwd()
dir_data <- file.path(dir_base, 'data')
dir_output <- file.path(dir_base, 'output')
dir_census <- file.path(dir_base, 'census')

################################################
# --- (1) LOAD EXISTING DATA AND DA SOURCE --- #

# (1) Existing population and DAs
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
u_DAs <- unique(df_pop$DA)
u_DAs_tor <- unique(filter(df_pop,city=='Toronto')$DA)
u_DAs_van <- unique(filter(df_pop,city=='Vancouver')$DA)

# (2) Load the DAs and the associated city/neighbourhood names
dat_DA <- fread(file.path(dir_data, 'DA_database.csv'))
# csd=Census Subdivision, ccsd=Census consolidation subdivisions
# cd = Census Divisions
dat_DA <- rename(dat_DA, DA=`DAUID,C,8`, prov=`PRNAME,C,100`,
                 csd=`CSDNAME,C,100`,ccsd=`CCSNAME,C,100`,
                 cd=`CDNAME,C,100`, cma=`CMANAME,C,100`) %>% 
            dplyr::select(DA, cma, csd, ccsd, cd)
# Make sure it has all of our DAs
stopifnot(length(setdiff(u_DAs, dat_DA$DA))==0)
dat_DA <- dat_DA %>% filter(DA %in% u_DAs)
# Clean up name1/name2
dat_DA <- mutate_at(dat_DA, vars(csd, ccsd),list(~str_replace_all(.,'\\s[0-9A-Z]$','')))
dat_DA[,-1] %>% apply(2,unique)

# (3) Load in the DA to PC Converter
dat_PC <- fread(file.path(dir_data, 'DA_to_PC.csv')) %>% 
  filter(!str_detect(DA_dauid,'[^0-9]')) %>% 
  mutate(DA_dauid=as.integer(DA_dauid)) %>% 
  dplyr::rename(DA=DA_dauid,csd=DA_csdname)
# Count the missing DAs
nmiss_PC <- sum(!(u_DAs %in% unique(dat_PC$DA)))
print(sprintf('A total of %i DAs (out of %i) are missing postal code matches',nmiss_PC,length(u_DAs)))
# Notice that DAs have multiple PCs associated with them
dat_nPC <- dat_PC %>% drop_na %>% group_by(DA) %>% count
dat_nPC %>% pull(n) %>% summary
dat_nPC %>% arrange(-n)
dat_PC %>% filter(DA==35190261)

# (3) Load the PC to city converter
dat_city <- fread(file.path(dir_data, 'zipcodeset.txt'), select=c('PostCode','City'))

# Add on the PC/Lat-Lon where possible
df_all <- dat_DA %>% 
  left_join(dat_PC,by='DA') %>% 
  left_join(dat_city,by='PostCode') %>% 
  mutate(csd.y=str_replace_all(csd.y,'\\s[0-9A-Z]$',''))
# Check that the Census Subdivisions line up between datasets
stopifnot(all(with(drop_na(df_all),csd.x == csd.y)))
df_all <- df_all %>% 
  dplyr::select(-csd.y) %>% dplyr::rename(csd=csd.x, csd_alt=City) %>% 
  mutate(csd_alt=ifelse(is.na(csd_alt), csd, csd_alt))

# (4) Break down Toronto (if missing or low count, then assign to Toronto)
tor_1b <- df_all %>% filter(csd == 'Toronto') %>% 
  group_by(csd_alt) %>% count %>% arrange(-n) %>% 
  filter(n>10) %>% drop_na %>% pull(csd_alt)
df_all <- mutate(df_all, csd_alt=ifelse(csd == 'Toronto', ifelse(csd_alt %in% tor_1b, csd_alt, 'Toronto'), csd))


####################################
# --- (2) LOAD THE CENSUS DATA --- #

# Source: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/page_dl-tc.cfm?Lang=E

# List the terms to extract from the census
vv_tt <- c('Population, 2016',
           'Single-detached house',
           'Semi-detached house',
           'Row house',
           'Apartment in a building that has five or more storeys',
           'Apartment or flat in a duplex',
           'Apartment in a building that has fewer than five storeys',
           'Median total income in 2015 among recipients ($)')
# Define the mapping
vv_map <- c('pop','singd','semd','row','apt1','apt2','apt3','income')

# This is the mapping for the column names
cn_new <- c('year','geo1','geo2','geo3','gnr','gnr2','qc','geo4','tt','mem','notes','total','male','female')
cn_drop <- c('geo2','geo3','gnr','gnr2','qc','geo4','mem','notes','notes','male','female')

lst_DAs <- list(BRITISH_COLUMBIA=u_DAs_van, ONTARIO=u_DAs_tor)

provinces <- c('BRITISH_COLUMBIA', 'ONTARIO')
holder <- list()
for (prov in provinces) { 
  u_DAs_prov <- lst_DAs[[prov]]
  print(sprintf('--------- Provice: %s ---------', prov))
  path_idx <- file.path(dir_census, str_c('Geo_starting_row_',prov,'_CSV.csv'))
  path_dat <- file.path(dir_census, str_c('98-401-X2016044_',prov,'_English_CSV_data.csv'))
  df_idx <- fread(path_idx)
  setnames(df_idx,old=c('Geo Code','Geo Name','Line Number'),new=c('geo','DA','nline'))
  idx <- summary(which(df_idx$DA %in% as.character(u_DAs_prov)))
  df_idx <- as_tibble(df_idx[idx['Min.']:idx['Max.']])
  df_idx <- mutate(df_idx, dline=nline-lag(nline), idx=row_number())
  u_nline <- unique(na.omit(df_idx$dline))
  stopifnot(length(u_nline) == 1)
  # Split into four chunks
  cn <- colnames(fread(path_dat,nrows = 1))
  m <- 4
  lst <- vector('list',m)
  df_idx <- df_idx %>% mutate(midx = (idx %/% (nrow(df_idx) %/% m + 1))+1)
  for (j in seq(m)) {
    print(sprintf('Chunk %i of %i',j,m))
    df_idx_j <- df_idx %>% filter(midx == j)
    idx <- summary(df_idx_j$nline)
    mi_rows <- as.numeric(idx['Min.'])
    mx_rows <- as.numeric(idx['Max.'])
    print(mx_rows - mi_rows)
    df_dat <- fread(path_dat,nrows=mx_rows-mi_rows+u_nline, skip=mi_rows-1)
    colnames(df_dat) <- cn
    setnames(df_dat, cn, cn_new)
    df_dat <- as_tibble(df_dat) %>% 
      dplyr::select(!!setdiff(cn_new,cn_drop)) %>% 
      mutate(ridx=row_number())
    # Make sure each geo has the right number of elements
    tmp <- df_dat  %>% group_by(geo1) %>% count()
    stopifnot( tmp %>% pull(n) %>% unique %>% equals(u_nline) )
    df_dat <- dplyr::rename(df_dat, DA=geo1) %>% filter(DA %in% u_DAs_prov)
    # Make the sthe rows line up with expectations
    tmp2 <- df_dat %>% mutate(midx = (ridx+1) %% u_nline) %>% 
      group_by(tt,midx) %>% count
    tmp2 %>% pull(n) %>% unique %>% length %>% equals(1) %>% stopifnot()
    df_dat <- df_dat %>% 
      filter(tt %in% vv_tt) %>% 
      mutate(vv_tt = factor(tt, levels=vv_tt, labels=vv_map)) %>% 
      dplyr::select(-c(ridx,tt)) %>% 
      pivot_wider(names_from='vv_tt',values_from='total') %>% 
      mutate_at(vars(!!vv_map), list(~as.integer(.))) %>% 
      mutate(apt=apt1+apt2+apt3) %>% 
      dplyr::select(!c(apt1, apt2, apt3))
    lst[[j]] <- df_dat
  }
  df_dat <- do.call('rbind',lst) %>% mutate(region = prov)
  df_dat %>% pull(DA) %>% length %>% equals(length(u_DAs_prov)) %>% stopifnot()
  holder[[prov]] <- df_dat
}
df_census <- do.call('rbind',holder)
# Clean up province
df_census <- mutate(df_census,region = ifelse(region=='ONTARIO','ON','BC')) %>% 
  dplyr::rename(prov=region)


###################################
# --- (3) SAVE DATA FOR LATER --- #

write_csv(df_census,file.path(dir_output,'df_census.csv'))
write_csv(df_all,file.path(dir_output,'df_PC.csv'))
write_csv(dat_DA,file.path(dir_output,'df_DA.csv'))

print('---------- End of process_DAs.R ----------')
