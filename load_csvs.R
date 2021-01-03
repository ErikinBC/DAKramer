# DAs from 2016
# https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/adaprof/search-recherche/results-resultats-CD-ADA.cfm?Lang=E&PRCODE=35&CD_UID=3520&TABID=0
# https://www12.statcan.gc.ca/census-recensement/2016/geo/ADA/alternative-eng.cfm?ADA=35200001

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr',
           'data.table',
           'ggplot2','cowplot') # 'rgdal','sf'
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}

dir_base <- getwd()
dir_data <- file.path(dir_base, 'DA_kramer')
dir_figres <- file.path(dir_base, 'figures')
dir_output <- file.path(dir_base, 'output')

##################################
# ------ (1) LOAD IN DATA ------ #

# Year folders
yy_folders <- list.files(dir_data) %>% str_subset('^[0-9]')

holder <- list()
for (yf in yy_folders) {
  fold <- file.path(dir_data, yf)
  fn_fold <- list.files(fold)
  fn_fold <- str_subset(fn_fold,'\\.csv$')
  # break
  if (yf == '2016 data') {
    cat('--- Doing special processing for 2016 ---\n')
    for (fn in fn_fold) {
      cat(sprintf('file: %s\n',fn))
      path <- file.path(fold, fn)
      tmp <- fread(path, select = c(1,2))
      colnames(tmp) <- c('geo','pop')
      tmp <- tmp %>%
        mutate(geo=str_split_fixed(geo,'\\s',2)[,1],pop=as.integer(pop)) %>%
        filter(str_detect(geo,'^[0-9]')) %>%
        mutate(city=ifelse(str_detect(geo,'^35'),'Toronto','Vancouver')) %>%
        dplyr::rename(DA=geo) %>%
        mutate(DA=as.integer(DA),year=2016)
      holder[[fn]] <- tmp
    }
  } else {
    cat(sprintf('--- Processing year: %s ---\n',yf))
    for (fn in fn_fold) {
      cat(sprintf('file: %s\n',fn))
      is_v2 <- str_detect(fn,'v2')
      path <- file.path(fold, fn)
      tmp <- read_csv(path,n_max=1, col_types = list(.default=col_character()))
      tmp <- tibble(DA=colnames(tmp), pop=as.vector(unlist(tmp[1,])))
      census_year <- as.character(unlist(tmp[1,'DA']))
      print(sprintf('Census year: %s',census_year))
      kk <- 2
      if (!is_v2) {
        city <- as.character(unlist(tmp[2,'DA']))
        agg_pop <- as.integer(tmp[2,'pop'])
        cat(sprintf('City: %s, pop: %i\n', city, agg_pop))
        tmp2 <- tibble(year=census_year, city=city, pop=agg_pop)
        kk <- 3
      }
      tmp <- mutate(tmp[kk:nrow(tmp),],year=census_year,
                    city=city, pop=as.integer(pop))
      holder[[fn]] <- tmp
    }
  }
}


df_pop <- do.call('rbind',holder)
df_pop <- df_pop %>% 
  mutate(year=as.integer(str_replace(year,'\\sCensus',''))) %>% 
  mutate(city=str_split_fixed(city,'\\s',2)[,1] %>% tolower() %>% str_to_title())
# Save copy
write_csv(df_pop,file.path(dir_output,'df_pop.csv'))


# Load if already run
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))


#################################
# ------ (2) DATA CHECKS ------ #

u_years <- unique(df_pop$year)
n_uyears <- length(u_years)
u_DAs <- unique(df_pop$DA)

# (i) Ensure no DA/year/city duplicates
df_pop %>% group_by(city, year, DA) %>% count() %>% pull(n) %>% equals(1) %>% all %>% stopifnot

# (i) Ratio of Toronto to Vancouver measurements growing over time
df_pop %>% group_by(year,city) %>% count() %>%
  pivot_wider(names_from = city,values_from = n) %>%
  mutate(ratio=Toronto / Vancouver)

# (iii) Are DAs unique over time?
dat_comp <- df_pop %>% 
  filter(city=='Toronto' & year %in% c(1971,2016)) %>%
  mutate(year=str_c('year_',year)) %>% 
  pivot_wider(id_cols=DA,names_from=year,values_from=pop)
#
ut_1971 <- dat_comp %>% filter(is.na(year_2016)) %>% pull(DA)
print(sprintf('These DAs are in 1971 but not in 2016: %s',
              ut_1971 %>% str_c(.,collapse=', ')))


# (iv) What years have the "missing" DAs
dat_missing <- df_pop %>% 
  pivot_wider(id_cols=c(city,DA),names_from=year,values_from=pop) %>% 
  pivot_longer(!c(city,DA),names_to='year',values_to='pop') %>% 
  group_by(city,year) %>% 
  summarise(n_miss=sum(is.na(pop)))
gg_n_miss <- ggplot(dat_missing,aes(x=year,y=n_miss,fill=city)) + theme_bw() + 
  geom_bar(stat='identity',position = position_dodge2(0.1),color='black') +
  facet_wrap(~city,scales='free_y') + guides(fill=F) + 
  labs(y='# of missing DAs') + ggtitle('Number of missing DAs (out of all unique)') + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=90))
save_plot(file.path(dir_figres,'gg_n_miss.png'),gg_n_miss,base_height=4,base_width = 6)

# (v) Calculate the number of pairwise comparisons possible
counts_DA <- df_pop %>% group_by(city, DA) %>% count() %>% arrange(city,-n)

holder <- list()
for (ii in seq(1,n_uyears-1)) {
  for (jj in seq(ii+1, n_uyears)) {
    # print(ii);print(jj)
    y1 <- u_years[ii]
    y2 <- u_years[jj]
    tmp_ij <- df_pop %>% filter(year %in% c(y1,y2)) %>%
      pivot_wider(id_cols=c(city,DA),names_from=year,values_from=pop) %>% 
      rename(m1=sym(as.character(y1)),m2=sym(as.character(y2))) %>% 
      mutate_at(vars(c('m1','m2')),list(~is.na(.))) %>% 
      group_by(city,m1,m2) %>% count() %>% mutate(y1=!!y1,y2=!!y2)
    holder[[str_c(y1,y2)]] <- tmp_ij
  }
}
dat_pair <- do.call('rbind',holder)
dat_pair <- counts_DA %>% group_by(city) %>% count() %>% 
  rename(tot=n) %>% right_join(dat_pair) %>% mutate(pct=n / tot)
gg_pair <- ggplot(filter(dat_pair,m1==m2),aes(x=as.character(y1),y=as.character(y2),fill=pct)) + 
  theme_bw() + geom_tile(color='black') + 
  facet_wrap(~city) + ggtitle('Percent of all unique DAs possible') + 
  theme(axis.title = element_blank()) + 
  scale_fill_gradient2(low='blue',high='red',mid = 'grey',midpoint = 0.75)
save_plot(file.path(dir_figres,'gg_pair.png'),gg_pair,base_height=5,base_width = 7)

# In other words, around 1991/1986 we get ~75-80% of all DAs

########################################
# ------ (3) SUMMARY STATISTICS ------ #

# (i) Population for all DAs vs DA-subset
df_pop %>% filter()

dat_sub <- counts_DA %>% filter(n==9) %>% dplyr::select(-n) %>% 
  left_join(df_pop) %>% group_by(city,year) %>% summarise(tot=sum(pop))
dat_tot <- df_pop %>% group_by(city,year) %>% summarise(tot=sum(pop))
dat_both <- rbind(mutate(dat_tot,tt='tot'),mutate(dat_sub,tt='sub'))

gg_pop_all <- ggplot(dat_both, aes(x=year,y=tot/1e6,color=tt)) + 
  theme_bw() + geom_point() + geom_line() + 
  labs(x='Year',y='Population (Millions)') + theme(legend.position = c(0.15,0.85)) + 
  scale_color_discrete(name='Method',labels=c('DA-intersect','Total')) + 
  ggtitle('City population over time') + 
  facet_wrap(~city,scales='free_y')
save_plot(file.path(dir_figres,'gg_pop_all.png'),gg_pop_all,base_height=4,base_width = 6)


# (ii) Count the pairwise population changes by years
holder <- list()
for (ii in seq(1,n_uyears-1)) {
  for (jj in seq(ii+1, n_uyears)) {
    y1 <- u_years[ii]
    y2 <- u_years[jj]
    # Put into wide format with DA level changes
    tmp_ij <- df_pop %>% filter(year %in% c(y1,y2)) %>% 
      pivot_wider(id_cols=c(city,DA),names_from=year,values_from=pop) %>% 
      rename(m1=sym(as.character(y1)),m2=sym(as.character(y2))) %>% 
      mutate(pct=m2/m1-1) %>% filter(!is.na(pct)) %>% 
      mutate(y1=y1, y2=y2)
    # Calculate aggregate change
    holder[[str_c(ii,jj)]] <- tmp_ij
  }
}
df_pop_comp <- do.call('rbind',holder)
write_csv(df_pop_comp,file.path(dir_output,'df_pop_comp.csv'))

########################################
# ------ (4) ESTABLISH BASELINE ------ #

df_pop_comp <- read_csv(file.path(dir_output,'df_pop_comp.csv'),col_types=list(city=col_character()))

# Load population according to wiki
dat_tor_wiki <- read_csv(file.path(dir_output,'toronto_pop_wiki.csv'),col_types = list(year=col_integer()))
dat_tor_wiki <- dat_tor_wiki %>% mutate_at(c('city','cma'),
            list(~as.integer(str_split_fixed(str_replace_all(.,'\\,',''),'\\[',n=2)[,1])))
dat_van_wiki <- read_csv(file.path(dir_output,'vancouver_pop_wiki.csv'),col_types = list(year=col_integer()))
dat_can_wiki <- read_csv(file.path(dir_output,'canada_pop_wiki.csv'),col_types = list(year=col_integer()))
dat_wiki <- rbind(mutate(dat_tor_wiki,tt='Toronto'),mutate(dat_van_wiki,tt='Vancouver'))
dat_wiki <- dat_wiki %>% dplyr::select(-cma) %>% rename(pop=city,city=tt)


# Get the aggregate populatio growth between periods
df_pop_agg <- df_pop_comp %>% group_by(city,y1,y2) %>% 
  summarise(m1=sum(m1),m2=sum(m2)) %>% 
  mutate(pct=m2/m1-1)

# Create the rolling indexes
holder <- list() 
for (y1 in seq(1,n_uyears-1)) {
  y1 <- u_years[y1]
  # --- (i) Get the wikipedia data --- #
  tmp_idx_wiki <- dat_wiki %>% filter(year >= y1) %>% 
    group_by(city) %>% mutate(idx=pop/pop[1]) %>% 
    mutate(tt='wiki') %>% dplyr::select(-pop)
  tmp_can <- dat_can_wiki %>% filter(year >= y1) %>% mutate(idx=canada/canada[1]) %>% 
    dplyr::select(-canada) %>% rename(canada=idx)
  # --- (ii) Cumulate DA data --- #
  tmp_idx_DA <- df_pop_agg %>% filter(y1 == !!y1) %>% ungroup %>% 
    dplyr::select(-y1,-m1,-m2) %>% 
    pivot_longer(c(-city,-y2,pct),values_to='idx') %>% 
    mutate(idx=1+idx,tt='DA') %>% dplyr::select(-name)
  tmp_top <- tibble(city=c('Toronto','Vancouver'),y2=!!y1,idx=c(1,1),tt='DA')
  tmp_idx_DA <- rbind(tmp_top,tmp_idx_DA) %>% arrange(city,y2) %>% rename(year=y2)
  tmp_idx <- rbind(tmp_idx_DA,tmp_idx_wiki)
  tmp_idx <- tmp_idx %>% left_join(tmp_can) %>% mutate(iyear=y1)
  holder[[as.character(y1)]] <- tmp_idx
}
dat_idx_growth <- do.call('rbind',holder)

gg_idx <- ggplot(dat_idx_growth,aes(x=year,y=idx,color=tt)) + 
  theme_bw() + geom_point() + geom_line() + 
  facet_grid(iyear~city,scales='free_y') + labs(y='Index',subtitle = 'Black line in Canada') + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=90)) + 
  scale_x_continuous(breaks=u_years) + 
  scale_color_discrete(name='Method', labels=c('DA','Census')) + 
  geom_line(aes(x=year,y=canada),color='black',inherit.aes = T)
save_plot(file.path(dir_figres,'gg_idx.png'),gg_idx,base_height=10,base_width = 7)

yy_min <- 1991
# Use the unique intersection of DAs since 1991
DAs_w_miss <- df_pop %>% filter(year >= yy_min) %>% 
  pivot_wider(c(city,DA),names_from='year',values_from='pop') %>% 
  pivot_longer(!c(city,DA)) %>% filter(is.na(value)) %>% 
  pull(DA) %>% unique
DAs_2_use <- df_pop %>% filter(year >= yy_min) %>%  pull(DA) %>% unique %>% setdiff(DAs_w_miss)
print(sprintf('Using a total of %i DAs',length(DAs_2_use)))

################################################
# ------ (5) DA DECOMPOSITION (NUMERIC) ------ #

df_pop_comp %>% filter((DA %in% DAs_2_use) & (y1 >= yy_min))
df_pop %>% filter((DA %in% DAs_2_use) & (year >= yy_min))



############################################
# ------ (6) DA DECOMPOSITION (MAP) ------ #