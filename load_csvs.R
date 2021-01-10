########################
# --- ANALYZE DATA --- #

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','forcats',
           'data.table',
           'ggplot2','cowplot','sf', 'rgdal') # 
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

##################################
# ------ (1) LOAD IN DATA ------ #

# Load population data
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
df_agg_pop <- read_csv(file.path(dir_output,'df_agg_pop.csv'), col_types = list(city=col_character()))

# Load geographic info
df_DA <- read_csv(file.path(dir_output,'df_DA.csv'), col_types = list(DA=col_integer()))
df_PC <- read_csv(file.path(dir_output,'df_PC.csv'), col_types = list(DA=col_integer()))


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
dat_pair <- do.call('rbind',holder)
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
df_agg_both <- rbind(mutate(df_agg_pop,tt='agg'),mutate(df_agg_DA,tt='DA'))
dat_can_wiki <- read_csv(file.path(dir_output,'canada_pop_wiki.csv'),
                         col_types = list(year=col_integer())) %>% 
  dplyr::rename(pop=canada) %>% mutate(idx=pop/pop[1]*100,city='Canada')
dat_can_city <- rbind(df_agg_pop,dat_can_wiki) %>% 
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
df_pop_comp <- do.call('rbind',holder)
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
dat_idx <- do.call('rbind',holder) %>% 
  group_by(iyear, city) %>% 
  arrange(iyear,city,year) %>% 
  mutate(idx=pop/pop[1]*100)
# Plot the index year

dat_idx %>% head
df_agg_pop %>% head

tmp <- rbind(mutate(dat_idx,iyear=as.character(iyear)),
      mutate(df_agg_pop,iyear='Actual'))
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
  dplyr::select(-c(m1,m2)) %>% mutate(year=as.character(y2)) %>% 
  mutate(tt=lvls_reorder(fct_recode(tt,'Density'='density','New DAs'='new','Lost DAs'='lost'),c(1,3,2)))

gg_decompose <- ggplot(dat_decompose,aes(x=year,y=dd/1e3,fill=tt)) + 
  theme_bw() + geom_bar(stat='identity',color='black') + 
  facet_wrap(~city) + 
  labs(x='Census Year',y='Δ in population') + 
  scale_fill_discrete(name='Share',labels=c('Density','New DAs','Lost DAs')) + 
  theme(axis.text.x = element_text(angle=90))
save_plot(file.path(dir_figures,'gg_decompose.png'),gg_decompose,base_height=4,base_width = 7)

##############################################################
# ------ (4) DISTRIBUTION OF CHANGES IN DENSITICATION ------ #

##################################################
# ------ (5) VISUALIZE THE MISSING VALUES ------ #

library(broom)
library(rgdal)
# tor_shp <- st_read(dsn='lda_000b16a_e/lda_000b16a_e.shp')
tor_shp <- readOGR('lda_000b16a_e/lda_000b16a_e.shp')
tor_shp <- tor_shp[tor_shp$DAUID %in% u_DAs_tor,]
plot(tor_shp)

tor_shp@bbox

tidy(tor_shp)

st_bbox(tor_shp)
class(tor_shp)

tidy(tor_shp)

gg_tor <- ggplot() + 
  geom_sf(data = tor_shp, size = 3, color = "black", fill = "cyan1") + 
  ggtitle('CMA Toronto') + 
  coord_sf(xlim = c(3689439, 3789439), ylim = c(659338, 669338), expand = FALSE)
gg_tor










# cn <- c('city','y1','y2')
# dat_growth_comp <- dat_growth_act %>% 
#   left_join(dat_growth_DA, by=cn, suffix=c('.act','.DA')) %>% 
#   pivot_longer(!cn,names_to='tmp') %>% 
#   separate(tmp, c('msr','tt'), '\\.') %>% 
#   arrange(city, msr, y1, y2) %>% 
#   filter(msr == 'growth') %>% 
#   dplyr::select(-msr)
# # pivot_wider(id_cols=c(cn,msr),names_from='tt',values_from='value') %>% 
# 
# # THIS IS PROBLEMATIC!!! 
# gg_growth_comp <- ggplot(dat_growth_comp,aes(x=as.character(y1),y=value, fill=tt)) + 
#   theme_bw() + facet_wrap(~city) + 
#   geom_bar(stat='identity',color='black',position = position_dodge2(0.5)) + 
#   labs(y='Inter-census growth (%)', x='Census Year') + 
#   scale_fill_manual(name=' ',labels=c('Actual','DA'), values=colz2_alt) + 
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
#   theme(legend.position = c(0.7,0.2),axis.text.x = element_text(angle=90))
# gg_growth_comp

# gg_pop_all <- ggplot(df_agg_both, aes(x=as.character(year),y=pop/1e6,color=tt)) + 
#   theme_bw() + geom_point(position = position_dodge2(1/2)) + 
#   labs(x='Census Year',y='Population (Millions)') + 
#   theme(legend.position = c(0.12,0.80),axis.text.x = element_text(angle=90)) + 
#   scale_color_discrete(name='Method',labels=c('Census','DA')) + 
#   ggtitle('CMA population over time') + 
#   facet_wrap(~city,scales='free_y')
# save_plot(file.path(dir_figures,'gg_pop_all.png'),gg_pop_all,base_height=4,base_width = 6)

# # (iii) What years have the "missing" DAs
# dat_missing <- df_pop %>% 
#   pivot_wider(id_cols=c(city,DA),names_from=year,values_from=pop) %>% 
#   pivot_longer(!c(city,DA),names_to='year',values_to='pop') %>% 
#   group_by(city,year) %>% 
#   summarise(n_miss=sum(is.na(pop)))
# gg_n_miss <- ggplot(dat_missing,aes(x=year,y=n_miss,fill=city)) + theme_bw() + 
#   geom_bar(stat='identity',position = position_dodge2(0.1),color='black') +
#   facet_wrap(~city,scales='free_y') + guides(fill=F) + 
#   labs(y='# of missing DAs',x='Census year') + 
#   ggtitle('Number of missing DAs (out of all unique)') + 
#   theme(axis.text.x = element_text(angle=90)) #axis.title.x = element_blank(),
# save_plot(file.path(dir_figures,'gg_n_miss.png'),gg_n_miss,base_height=4,base_width = 6)


# ########################################
# # ------ (4) ESTABLISH BASELINE ------ #
# 
# df_pop_comp <- read_csv(file.path(dir_output,'df_pop_comp.csv'),col_types=list(city=col_character()))
# 
# # Load population according to wiki
# dat_tor_wiki <- read_csv(file.path(dir_output,'toronto_pop_wiki.csv'),col_types = list(year=col_integer()))
# dat_tor_wiki <- dat_tor_wiki %>% mutate_at(c('city','cma'),
#                                            list(~as.integer(str_split_fixed(str_replace_all(.,'\\,',''),'\\[',n=2)[,1])))
# dat_van_wiki <- read_csv(file.path(dir_output,'vancouver_pop_wiki.csv'),col_types = list(year=col_integer()))
# dat_can_wiki <- read_csv(file.path(dir_output,'canada_pop_wiki.csv'),col_types = list(year=col_integer()))
# dat_wiki <- rbind(mutate(dat_tor_wiki,tt='Toronto'),mutate(dat_van_wiki,tt='Vancouver'))
# dat_wiki <- dat_wiki %>% dplyr::select(-cma) %>% rename(pop=city,city=tt)
# 
# 
# # Get the aggregate populatio growth between periods
# df_pop_agg <- df_pop_comp %>% group_by(city,y1,y2) %>% 
#   summarise(m1=sum(m1),m2=sum(m2)) %>% 
#   mutate(pct=m2/m1-1)

# 
# gg_idx <- ggplot(dat_idx_growth,aes(x=year,y=idx,color=tt)) + 
#   theme_bw() + geom_point() + geom_line() + 
#   facet_grid(iyear~city,scales='free_y') + labs(y='Index',subtitle = 'Black line in Canada') + 
#   theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=90)) + 
#   scale_x_continuous(breaks=u_years) + 
#   scale_color_discrete(name='Method', labels=c('DA','Census')) + 
#   geom_line(aes(x=year,y=canada),color='black',inherit.aes = T)
# save_plot(file.path(dir_figures,'gg_idx.png'),gg_idx,base_height=10,base_width = 7)
# 
# yy_min <- 1991
# # Use the unique intersection of DAs since 1991
# DAs_w_miss <- df_pop %>% filter(year >= yy_min) %>% 
#   pivot_wider(c(city,DA),names_from='year',values_from='pop') %>% 
#   pivot_longer(!c(city,DA)) %>% filter(is.na(value)) %>% 
#   pull(DA) %>% unique
# DAs_2_use <- df_pop %>% filter(year >= yy_min) %>%  pull(DA) %>% unique %>% setdiff(DAs_w_miss)
# print(sprintf('Using a total of %i DAs',length(DAs_2_use)))


############################################
# ------ (X) SURPLUS CODE ------ #

# tmp3 <- fread(file.path(dir_data,'toronto_and_vancouver_v2.csv'),select=c(1,2))
# colnames(tmp3) <- c('geo','pop')
# tmp3 <- mutate(tmp3, geo=str_split_fixed(geo,'\\s',2)[,1],pop=as.integer(pop))
# tmp3 %>% filter(str_detect(geo,'^[0-9]')) %>% mutate(geo=as.integer(geo)) %>% 
#   left_join(tmp,by=c('geo'='DA')) %>% 
#   mutate(dd = pop.x - pop.y) %>% pull(dd) %>% table
