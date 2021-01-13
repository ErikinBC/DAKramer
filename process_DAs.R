############################
# --- PROCESS DA FILES --- #

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','data.table') 
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}

dir_base <- getwd()
dir_data <- file.path(dir_base, 'data')
dir_output <- file.path(dir_base, 'output')

# --- (1) LOAD EXISTING DATA AND DA SOURCE --- #

# (1) Existing population and DAs
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
u_DAs <- unique(df_pop$DA)

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

# --- (2) LOAD THE CENSUS DATA --- #


# --- (3) SAVE DATA FOR LATER --- #

write_csv(df_all,file.path(dir_output,'df_PC.csv'))
write_csv(dat_DA,file.path(dir_output,'df_DA.csv'))

print('---------- End of process_DAs.R ----------')