########################
# --- ANALYZE DATA --- #

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','forcats','data.table',
           'ggplot2','cowplot')
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}

dir_base <- getwd()
dir_data <- file.path(dir_base, 'DA_kramer')
dir_lda <- file.path(dir_base, 'lda_000b16a_e')
dir_figures <- file.path(dir_base, 'figures')
dir_output <- file.path(dir_base, 'output')

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colz2_alt <- gg_color_hue(3)[c(2,3)]

cmas <- c('Toronto','Vancouver')

##################################
# ------ (1) LOAD IN DATA ------ #

# Load population data
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
df_agg_pop <- read_csv(file.path(dir_output,'df_agg_pop.csv'), col_types = list(city=col_character()))

# Load geographic info
df_DA <- read_csv(file.path(dir_output,'df_DA.csv'), col_types = list(DA=col_integer()))
df_PC <- read_csv(file.path(dir_output,'df_PC.csv'), col_types = list(DA=col_integer()))
dat_latlon <- df_PC %>% group_by(DA) %>% summarise_at(vars(Latitude,Longitude),list(~mean(.)))
df_csd <- df_PC %>% group_by(DA,cma,csd,ccsd,cd,csd_alt) %>% count
# Select the most common one
df_csd <- df_csd %>% arrange(DA,-n) %>% group_by(DA) %>% filter(n==max(n)) %>% dplyr::select(-n)

#################################
# ------ (2) DATA CHECKS ------ #

u_years <- unique(df_pop$year)
n_uyears <- length(u_years)
u_DAs <- unique(df_pop$DA)
u_DAs_tor <- unique(filter(df_pop,city=='Toronto')$DA)

# (i) Ratio of Toronto to Vancouver measurements growing over time
dat_num_DAs <- df_pop %>% group_by(year,city) %>% count()
  # pivot_wider(names_from = city,values_from = n) %>% mutate(ratio=Toronto / Vancouver)
tmp <- df_pop %>% group_by(city) %>% summarise(nu=length(unique(DA)))

gg_n_DA <- ggplot(dat_num_DAs,aes(x=as.character(year),y=n,color=city)) + 
  theme_bw() + geom_point(size=2)  + 
  labs(y='# DAs',x='Census year') + 
  # ggtitle('Number of DAs in census year') + 
  theme(axis.text.x = element_text(angle=90)) + 
  scale_color_discrete(name='CMA')
save_plot(file.path(dir_figures,'gg_n_DA.png'),gg_n_DA,base_height=4,base_width = 6)

# (ii) Are DAs unique over time?
dat_comp <- df_pop %>% 
  filter(city=='Toronto' & year %in% c(1971,2016)) %>%
  mutate(year=str_c('year_',year)) %>% 
  pivot_wider(id_cols=DA,names_from=year,values_from=pop)
# Print the missing DAs
ut_1971 <- dat_comp %>% filter(is.na(year_2016)) %>% pull(DA)
print(sprintf('These DAs are in 1971 but not in 2016: %s', ut_1971 %>% str_c(.,collapse=', ')))

df_PC %>% filter(DA %in% ut_1971) %>% 
  group_by(DA) %>% summarise_at(vars(Latitude,Longitude),~(mean(.,na.rm=T))) %>% drop_na %>% 
  data.frame %>% print(digits=8)

# (iii) Calculate pairwise DAs
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
dat_pair <- rbindlist(holder,use.names=T)
dat_pair <- dat_pair %>% group_by(city,y1,y2) %>% 
  summarise(tot=sum(n)) %>%
  right_join(dat_pair,by=c('city','y1','y2')) %>% 
  mutate(pct=n / tot)
gg_pair <- ggplot(filter(dat_pair,m1==m2),aes(x=as.character(y1),y=as.character(y2),fill=pct)) + 
  theme_bw() + geom_tile(color='black') + 
  facet_wrap(~city) + #ggtitle('Percent of all unique DAs possible') + 
  theme(axis.title.y = element_blank()) + 
  scale_fill_gradient2(low='blue',high='red',mid = 'grey',midpoint = 0.75,name='Jaccard %',
                       breaks=seq(0.5,1,0.1),limits=c(0.5,1.0)) + 
  labs(x='Census Year')
save_plot(file.path(dir_figures,'gg_pair.png'),gg_pair,base_height=4.5,base_width = 7)
# In other words, around 1991/1986 we get ~75-80% of all DAs
DAs_delta <- dat_pair %>% group_by(y1) %>% filter(y2 == min(y2)) %>% 
  filter(m1 != m2) %>% dplyr::select(-c(tot,pct)) %>% 
  mutate(tt=ifelse(m1==F,'Lost','Gained'))

DAs_delta %>% pivot_wider(c(city,y1),names_from='tt',values_from='n') %>%
  mutate(ratio = Gained/Lost)

gg_DA_delta <- ggplot(DAs_delta, aes(x=as.character(y2), y=n, color=tt)) + 
  theme_bw() + geom_point(size=2) + 
  facet_wrap(~city) + 
  labs(x='Census Year', y='Change in # of DAs') + 
  # ggtitle('Gain/loss in # of DAs between census') + 
  theme(legend.position = c(0.8,0.8), axis.text.x=element_text(angle=90)) + 
  scale_color_discrete(name='Δ')
save_plot(file.path(dir_figures,'gg_DA_delta.png'),gg_DA_delta,base_height=4,base_width = 6)


#########################################
# ------ (3) POPULATION DYNAMICS ------ #

# (i) Descrepenancy between total population and sum of DAs
df_agg_pop <- df_agg_pop %>% group_by(city) %>% mutate(idx=pop/pop[1]*100)
df_agg_DA <- df_pop %>% group_by(city,year) %>% summarise(pop=sum(pop)) %>% mutate(idx=pop/pop[1]*100)
df_agg_both <- tibble(rbindlist(list(mutate(df_agg_pop,tt='agg'),mutate(df_agg_DA,tt='DA')),use.names=T))
dat_can_wiki <- read_csv(file.path(dir_output,'canada_pop_wiki.csv'),
                         col_types = list(year=col_double())) %>% 
  dplyr::rename(pop=canada) %>% mutate(idx=pop/pop[1]*100,city='Canada')

dat_can_city <- tibble(rbindlist(list(df_agg_pop,dat_can_wiki),use.names = T)) %>% 
  pivot_longer(c(pop,idx),names_to='tt') %>% ungroup %>% 
  filter(!(city=='Canada' & tt=='pop')) %>% 
  mutate(value=ifelse(tt=='pop',value/1e6,value)) %>% 
  mutate(tt=fct_recode(as.factor(tt),"Population (000,000's)"="pop",'Index [1971==100]'='idx'))

colz_can <- c('black',gg_color_hue(2))
gg_pop_hist <- ggplot(dat_can_city, aes(x=year,y=value,color=city)) + 
  geom_point() + facet_wrap(~tt,scales='free_y') + 
  theme_bw() + geom_line() + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle=90)) + 
  scale_color_manual(values=colz_can,name='Region: ') + 
  labs(x='Census Year', y='Population/Index') + 
  scale_x_continuous(breaks=seq(1971,2016,5))
save_plot(file.path(dir_figures,'gg_pop_hist.png'),gg_pop_hist,base_height=4,base_width = 6)

  
# Check that the sum of DAs is equal to overall population
dat_agg_err <- df_agg_both %>% 
  pivot_wider(id_cols=c(year,city), names_from=tt, values_from=pop) %>% 
  mutate(err = agg/DA-1) %>% arrange(err)  #pull(err) %>% hist

gg_pop_err <- ggplot(dat_agg_err, aes(x=as.character(year),y=err,color=city)) + 
  theme_bw() + geom_point(position = position_dodge2(1/2)) + 
  labs(x='Census Year',y='Error (%)') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme(axis.text.x = element_text(angle=90)) +  #legend.position = c(0.12,0.80),
  # ggtitle('Error between CMA census count and sums of DAs') + 
  scale_color_discrete(name='City')
save_plot(file.path(dir_figures,'gg_pop_err.png'),gg_pop_err,base_height=3.5,base_width = 5)


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
      mutate(pct=m2/m1-1) %>% 
      mutate(y1=y1, y2=y2)  #filter(!is.na(pct)) %>% 
    # Calculate aggregate change
    holder[[str_c(ii,jj)]] <- tmp_ij
  }
}
df_pop_comp <- rbindlist(holder,use.names=T)
# Subset to only the iterative years
df_pop_iter <- df_pop_comp %>% group_by(city,y1) %>% filter(y2 == min(y2))

# Calculate the implied growth rates vs the actual ones
dat_growth_DA <- df_pop_iter %>% 
  filter(!is.na(pct)) %>% 
  group_by(city,y1,y2) %>% 
  summarise(m1=sum(m1), m2=sum(m2)) %>% 
  mutate(growth=m2 / m1 - 1)

# Tells us the growth rate using only the intersection of DAs
dat_growth_act <- df_agg_pop %>% dplyr::select(-idx) %>% 
  dplyr::rename(y2=year, m2=pop) %>% group_by(city) %>% 
  mutate(m1=lag(m2), y1=lag(y2)) %>% 
  mutate(growth=m2 / m1 - 1) %>% 
  drop_na %>% dplyr::select(colnames(dat_growth_DA))

# Create the rolling indexes
holder <- list()
for (y1 in seq(1,n_uyears-1)) {
  y1 <- u_years[y1]
  # --- (ii) Cumulate DA data --- #
  tmp_idx <- df_pop %>% 
    pivot_wider(names_from='year',values_from='pop') %>% 
    dplyr::rename(iyear=!!as.character(y1)) %>% 
    filter(!is.na(iyear)) %>%
    pivot_longer(!c(DA,city),names_to='year') %>%
    mutate(value=ifelse(is.na(value),0,value)) %>% 
    group_by(city,year) %>% 
    summarise(pop=sum(value)) %>% ungroup %>% 
    mutate(iyear=y1, year=as.integer(ifelse(year=='iyear',as.character(y1),year)))
  holder[[as.character(y1)]] <- tmp_idx
}
dat_idx <- rbindlist(holder,use.names=T) %>% 
  group_by(iyear, city) %>% 
  arrange(iyear,city,year) %>% 
  mutate(idx=pop/pop[1]*100)

# Plot the index year
tmp <- tibble(rbindlist(list(mutate(dat_idx,iyear=as.character(iyear)),
              mutate(df_agg_pop,iyear='Actual')),use.names=T))
gg_idx <- ggplot(tmp, aes(x=year,y=idx,color=iyear)) + 
  theme_bw() + geom_point() + geom_line() + 
  labs(x='Census Year',y='Population (Indexed==100)',
       subtitle = 'Only non-missing DAs from index year used') + 
  facet_wrap(~city) + scale_y_continuous(breaks=seq(80,240,20),limits=c(80,240)) + 
  theme(axis.text.x = element_text(angle=90)) + 
  scale_x_continuous(breaks=seq(1971,2016,5)) + 
  scale_color_manual(name='Index Year',values=c(gg_color_hue(8),'black')) + 
  geom_hline(yintercept=100,linetype='dashed') + 
  geom_line(aes(x=year,y=idx),data=df_agg_pop,inherit.aes = F)
save_plot(file.path(dir_figures,'gg_idx.png'),gg_idx,base_height=4,base_width = 7)

# Decompose population growth into "densification" vs "new neighbourhoods"
dat_decompose <- df_pop_iter %>% 
  mutate(miss_m1=is.na(m1), miss_m2=is.na(m2)) %>%
  group_by(city,y1,y2,miss_m1, miss_m2) %>% 
  summarise(m1=sum(m1),m2=sum(m2)) %>% 
  mutate_at(vars(m1,m2),list(~ifelse(is.na(.),0,.))) %>% ungroup %>% 
  mutate(tt=ifelse(!miss_m1 & !miss_m2,'density',
                   ifelse(!miss_m1 & miss_m2,'lost','new'))) %>% 
  dplyr::select(-c(miss_m1,miss_m2)) %>% 
  mutate(dd=m2 - m1)
dat_decompose <- dat_decompose %>% group_by(city,y1,y2) %>% 
  summarise(change=sum(dd)) %>% right_join(dat_decompose) %>% 
  dplyr::select(-c(m1,m2)) %>% mutate(year=as.character(y2)) %>% ungroup %>% 
  mutate(tt=lvls_reorder(fct_recode(tt,'Density'='density','New DAs'='new','Lost DAs'='lost'),c(1,3,2)))

gg_decompose <-
  ggplot(dat_decompose,aes(x=year,y=dd/1e3,fill=tt)) + 
  theme_bw() + geom_bar(stat='identity',color='black') + 
  facet_wrap(~city) + 
  labs(x='Census Year',y='Δ in population', subtitle = 'Point shows net change') + 
  scale_fill_discrete(name='Share',labels=c('Density','New DAs','Lost DAs')) + 
  theme(axis.text.x = element_text(angle=90)) + 
  geom_point(aes(x=year,y=change/1e3),data=filter(dat_decompose,tt=='Density'),inherit.aes = F)
save_plot(file.path(dir_figures,'gg_decompose.png'),gg_decompose,base_height=4,base_width = 7)

tmp_decomp <- dplyr::select(dat_decompose,-dd) %>% 
  dplyr::rename(dd=change) %>% mutate(tt='Actual') %>% 
  distinct() %>% 
  list(., dplyr::select(dat_decompose,-change)) %>% 
  rbindlist(.,use.names=T) %>% tibble %>% 
  left_join(df_agg_DA,by=c('y1'='year','city')) %>% 
  dplyr::select(-idx) %>% 
  pivot_wider(names_from='tt',values_from='dd') %>% 
  mutate(`Net DAs`=`New DAs`+`Lost DAs`) %>% 
  dplyr::select(-c(`New DAs`,`Lost DAs`)) %>% 
  pivot_longer(c(Actual,Density,`Net DAs`),names_to='tt',values_to='dd') %>% 
  arrange(city,tt,y1) %>%
  group_by(city,tt) %>%
  mutate(cum_d=cumsum(dd),year=as.integer(year)) %>%
  mutate(pop=cum_d+pop[1]) %>% ungroup
tmp_decomp <- dplyr::select(df_agg_DA,-idx) %>%
  filter(year==1971) %>% mutate(tt=list(unique(tmp_decomp$tt))) %>% 
  unnest(cols=tt) %>% 
  list(.,dplyr::select(tmp_decomp,-c(y1,y2,dd,cum_d))) %>% 
  rbindlist(.,use.names=T) %>% tibble %>% 
  arrange(city,tt,year) %>% 
  mutate(tt=fct_relevel(tt,c('Density','Net DAs','Actual')))

# Make figure showing counterfactural
colz <- c(gg_color_hue(3)[1],gg_color_hue(4)[4], 'black')
gg_cf_decomp <- ggplot(tmp_decomp,aes(x=year,y=pop/1e6,color=tt)) +
  theme_bw() + geom_point() + geom_line() +
  labs(y='Population (millions)') + 
  scale_color_manual('Method',values=colz) + 
  facet_wrap(~city,scales='free_y') + 
  labs(x='Census Year') + 
  scale_x_continuous(breaks=seq(1971,2016,5)) + 
  theme(axis.text.x = element_text(angle=90))
save_plot(file.path(dir_figures,'gg_cf_decomp.png'),gg_cf_decomp,base_height=4,base_width = 7)


##############################################################
# ------ (4) DISTRIBUTION OF CHANGES IN DENSITICATION ------ #

# (i) BREAK DENSITY INTO INCREASE VS DECREASE TO GET NET CHANGE IN DENSITY
dat_DA_delta <- df_pop_iter %>% 
  dplyr::select(-pct) %>% drop_na %>% 
  # mutate_at(vars(m1,m2),list(~ifelse(is.na(.),0,.))) %>% 
  mutate(d_DA=m2 - m1) %>% mutate(tt=ifelse(d_DA<0,'Loss','Gain'))

gg_DA_delta_dist <- ggplot(dat_DA_delta,aes(x=log(abs(d_DA)+1),fill=tt,color=tt)) + theme_bw() + 
  geom_histogram(bins=15,alpha=0.5,position='identity') + 
  facet_grid(city~y2,scales='free_y') + 
  labs(x='log(Δ DA)',y='Count') + 
  scale_fill_discrete(name='Δ in DA') + 
  guides(color=F) + theme(axis.text.x = element_text(size=6))
save_plot(file.path(dir_figures,'gg_DA_delta_dist.png'),gg_DA_delta_dist,base_height=5,base_width = 8)

dat_DA_delta_net <- dat_DA_delta %>% group_by(city,y2,tt) %>% 
  summarise(d_DA = sum(d_DA))
dat_DA_delta_net <-
  dat_DA_delta_net %>% group_by(city,y2) %>% summarise(d_DA=sum(d_DA)) %>% 
  mutate(tt='Net') %>% list(.,dat_DA_delta_net) %>% rbindlist(.,use.names=T) %>% tibble %>% 
  arrange(city,y2,tt)

colz <- c(gg_color_hue(3)[c(3,1)],'black')
gg_DA_delta_net <- ggplot(dat_DA_delta_net,aes(x=as.character(y2),y=d_DA/1e3,color=tt,group=tt)) + 
  theme_bw() + geom_point(size=2) + geom_line() + 
  facet_wrap(~city) + geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle=90)) + 
  labs(x='Census Year',y="Δ in DAs ('000s)",subtitle = 'Net change within existing DAs') + 
  scale_color_manual(name='Change',values=colz) 
save_plot(file.path(dir_figures,'gg_DA_delta_net.png'),gg_DA_delta_net,base_height=4,base_width = 7)

# Now calculate the # of changes
dat_DA_delta_n <- dat_DA_delta %>% mutate(s_DA=sign(d_DA)) %>% 
  group_by(city,y2,s_DA) %>% 
  count %>% mutate(s_DA=factor(s_DA,levels=c(-1,0,1),labels=c('Loss','No Change','Gain')))

gg_DA_delta_n <- ggplot(dat_DA_delta_n,aes(x=as.character(y2),y=n,color=s_DA,group=s_DA)) + 
  theme_bw() + geom_point(size=2) + geom_line() + 
  facet_wrap(~city) + geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(angle=90)) + 
  labs(x='Census Year',y="# of DAs",subtitle = 'Net change within existing DAs') + 
  scale_color_manual(name='Change',values=colz[c(2,3,1)]) 
save_plot(file.path(dir_figures,'gg_DA_delta_n.png'),gg_DA_delta_n,base_height=4,base_width = 7)


cn_id <- c('city','DA')
dat_DA_rho <- dat_DA_delta %>% 
  pivot_wider(id_cols=cn_id,names_from='y2',values_from='d_DA') %>% 
  pivot_longer(!cn_id) %>% 
  drop_na %>% mutate(s_DA=sign(value)) %>% 
  group_by(city, DA,s_DA) %>% count
dat_DA_rho <- dat_DA_rho %>% group_by(DA) %>% 
  summarise(tot=sum(n)) %>% right_join(dat_DA_rho) %>% 
  mutate(pct=n/tot) %>% left_join(dat_latlon,by='DA') %>% 
  mutate(s_DA=factor(s_DA,levels=c(-1,0,1),labels=c('Loss','No Change','Gain')))


####################################
# ------ (5) CITY BREAKDOWN ------ #

# Calculate the changes into four categories: 
# (i) New DA, (ii) Lost DA, (iii) New Density, (iv) Lost Density
dat_delta_csd <- df_pop_iter %>% 
  mutate(tt=ifelse(!is.na(m1) & is.na(m2),'Lost DA',
            ifelse(is.na(m1) & !is.na(m2),'New DA',
            ifelse(m1 > m2, 'Lost Density', 'New Density')))) %>% 
  mutate(d_m = ifelse(is.na(m2),0,m2) - ifelse(is.na(m1),0,m1)) %>% 
  left_join(df_csd,by='DA') %>% 
  mutate(csd_lump=fct_lump_min(csd, 80)) %>% 
  mutate(csd_lump=ifelse(csd_lump=='Other',str_c('Other (',city,')'),as.character(csd_lump))) %>% 
  distinct()
# Calculate by time period
dat_delta_csd_sum <- dat_delta_csd %>% 
  group_by(city,y1,y2,tt,csd_lump) %>% 
  summarise(d_m=sum(d_m)) %>% 
  pivot_wider(names_from='tt',values_from='d_m') %>% 
  mutate(`Net Density`=`New Density`+`Lost Density`,
         `Net DAs`=`New DA`+`Lost DA`) %>% 
  dplyr::select(!matches('Lost|New',perl=T)) %>% 
  pivot_longer(starts_with('Net'),names_to='tt',values_to='d_m') %>% 
  mutate(d_m = ifelse(is.na(d_m),0,d_m))
ord_csd <- dat_delta_csd_sum %>% group_by(city,csd_lump) %>% summarise(tot=sum(d_m)) %>% 
  arrange(-tot) %>% pull(csd_lump) %>% unique %>% as.character()
dat_delta_csd_sum$csd_lump <- fct_relevel(dat_delta_csd_sum$csd_lump,ord_csd)

holder <- list()
for (cma in cmas) {
  tmp_df <- filter(dat_delta_csd_sum,city==cma)
  gg_tmp <- ggplot(tmp_df, aes(x=csd_lump,y=d_m/1e3,fill=tt)) + 
    theme_bw() + 
    geom_bar(stat='identity',color='black',width=0.7) + 
    facet_grid(y2~city,scales='free') + 
    labs(y='Net contribution (000s)') + 
    theme(axis.text.x = element_text(angle=90,vjust = -0.01),
          axis.title.x = element_blank(), legend.position = 'bottom') + 
    scale_fill_discrete(name='Composition: ')
  holder[[cma]] <- gg_tmp
}
tmp_legend <- get_legend(holder$Toronto)
holder <- lapply(holder,function(ll) ll + theme(legend.position = 'none'))
gg_csd_net <- plot_grid(plot_grid(holder$Toronto, holder$Vancouver, nrow=1),
          tmp_legend,rel_heights = c(1,0.1),ncol=1)
save_plot(file.path(dir_figures,'gg_csd_net.png'),gg_csd_net,base_height=10,base_width = 5)

#######################################
# ------ (6) CENSUS ANNOTATION ------ #




############################################
# ------ (X) SAVE DATA FOR GEO FILE ------ #

# DA by new DA/density change
write_csv(df_pop_iter,file.path(dir_base,'output','df_pop_iter.csv'))
write_csv(dat_delta_csd,file.path(dir_base,'output','dat_delta_csd.csv'))
# write_csv(dat_DA_rho,file.path(dir_base,'output','dat_DA_rho.csv'))

# (ii) BREAK DOWN THE NET DENSITY AND NET DAs BY THE NEIGHBOURHOOD LEVEL
# (iii) DO A SPECIAL TORONTO PRE-AMALGATION CALCULATION




