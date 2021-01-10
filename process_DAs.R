############################
# --- PROCESS DA FILES --- #

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','data.table') 
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}

dir_base <- getwd()
dir_data <- file.path(dir_base, 'DA_kramer')
dir_output <- file.path(dir_base, 'output')
dir_lda <- file.path(dir_base, 'lda_000b16a_e')

# --- (1) LOAD EXISTING DATA AND DA SOURCE --- #

# (1) Existing population and DAs
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
u_DAs <- unique(df_pop$DA)

# (2) Load the DAs and the associated city/neighbourhood names
dat_DA <- fread(file.path(dir_lda, 'DA_database.csv'))
dat_DA <- rename(dat_DA, DA=`DAUID,C,8`, prov=`PRNAME,C,100`,
                 city1=`CSDNAME,C,100`,city2=`CCSNAME,C,100`,
                 city3=`CDNAME,C,100`, cma=`CMANAME,C,100`) %>% 
            dplyr::select(DA, cma, city1, city2, city3)
# Make sure it has all of our DAs
stopifnot(length(setdiff(u_DAs, dat_DA$DA))==0)
dat_DA <- dat_DA %>% filter(DA %in% u_DAs)
# Clean up name1/name2
dat_DA <- mutate_at(dat_DA, vars(city1,city2),list(~str_replace_all(.,'\\s[0-9A-Z]$','')))

# (3) Load in the DA to PC Converter
dat_PC <- fread(file.path(dir_lda, 'DA_to_PC.csv')) %>% 
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
dat_city <- fread(file.path(dir_lda, 'zipcodeset.txt'), select=c('PostCode','City'))

# Add on the PC/Lat-Lon where possible
df_all <- dat_DA %>% 
  left_join(dat_PC,by='DA') %>% 
  left_join(dat_city,by='PostCode') %>% 
  mutate(csd=str_replace_all(csd,'\\s[0-9A-Z]$','')) %>% 
  rename(city1a=city1, city1b=City) %>% 
  mutate(city1b=ifelse(is.na(city1b), city1a, city1b))
# Clean up csd
stopifnot(all(na.omit(with(df_all, city1a == csd))))
df_all <- df_all[,-'csd']

# (4) Break down Toronto (if missing or low count, then assign to Toronto)
tor_1b <- df_all %>% filter(city1a == 'Toronto') %>% 
  group_by(city1b) %>% count %>% arrange(-n) %>% 
  filter(n>10) %>% drop_na %>% pull(city1b)
df_all <- df_all %>% mutate(city1b=ifelse(city1a == 'Toronto',
                                ifelse(city1b %in% tor_1b, city1b, 'Toronto'), city1a))

# --- (2) SAVE DATA FOR LATER --- #

write_csv(df_all,file.path(dir_output,'df_PC.csv'))
write_csv(dat_DA,file.path(dir_output,'df_DA.csv'))

