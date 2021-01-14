#####################
# --- MAKE MAPS --- #

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','forcats','data.table','broom',
           'ggplot2','cowplot','ggmap','ggh4x','ggpubr','ggrepel',
           'sf', 'rgdal','maptools')
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}

gpclibPermit()

dir_base <- getwd()
dir_data <- file.path(dir_base, 'data')
dir_figures <- file.path(dir_base, 'figures')
dir_output <- file.path(dir_base, 'output')
dir_output_shp <- file.path(dir_output, 'shp')
dir_shp <- file.path(dir_base, 'shp')

##################################
# ------ (1) LOAD IN DATA ------ #

# Load population data
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
df_agg_pop <- read_csv(file.path(dir_output,'df_agg_pop.csv'), col_types = list(city=col_character()))

# Load geographic info
df_DA <- read_csv(file.path(dir_output,'df_DA.csv'), col_types = list(DA=col_integer()))
df_PC <- read_csv(file.path(dir_output,'df_PC.csv'), col_types = list(DA=col_integer()))
# Get most common mapping for pre-amalgation
df_csd_alt <- df_PC %>% 
  group_by(DA,cma,csd,ccsd,cd,csd_alt) %>% count %>% 
  arrange(DA,-n) %>% ungroup %>% group_by(DA) %>% 
  slice(1) %>% dplyr::select(-n) %>% dplyr::select(c(DA,csd_alt))


# Load analysis data
df_pop_iter <- read_csv(file.path(dir_base,'output','df_pop_iter.csv'))
dat_delta_csd <- read_csv(file.path(dir_base,'output','dat_delta_csd.csv')) %>% 
  dplyr::select(-c(cma,csd_alt)) %>% distinct()

df_pop_iter %>% mutate_at(vars(c(m1,m2)),list(~ifelse(is.na(.),0,.))) %>% 
  mutate(d_m=m2 - m1) %>% 
  left_join(df_DA,by='DA') %>% 
  group_by(csd) %>% summarise(tot=sum(d_m)) %>% arrange(-tot)
  

u_years <- unique(df_pop$year)
n_uyears <- length(u_years)
u_DAs <- unique(df_pop$DA)
u_DAs_tor <- unique(filter(df_pop,city=='Toronto')$DA)
u_DAs_van <- unique(filter(df_pop,city=='Vancouver')$DA)
u_csd <- unique(df_DA$csd)
u_csd_tor <- unique(filter(df_DA,cma=='Toronto')$csd)
u_csd_van <- unique(filter(df_DA,cma=='Vancouver')$csd)

# Load the shape files
shp_csd <- read_sf(file.path(dir_output_shp,'csd.shp'))
shp_DA <- read_sf(file.path(dir_output_shp,'DA.shp'))
path_shp_tor <- file.path(dir_shp,'toronto','NEIGHBORHOODS_WGS84_2.shp')
shp_tor <- read_sf(path_shp_tor) %>% 
  mutate(AREA_NAME = str_replace_all(AREA_NAME, '\\s\\(.*\\)','')) %>% 
  st_transform(crs='NAD83') %>% 
  mutate(area=as.numeric(units::set_units(st_area(shp_tor),km^2)))

#######################################
# ------ (2) MERGE & CALCULATE ------ #

# (i) Find DAs within the Toronto neighbourhoods
# Find what intersects
isec_tor <- st_intersects(shp_tor$geometry,shp_DA$geometry)
# Add on the empty column
holder <- rep('',nrow(shp_DA))
for (ii in seq_along(isec_tor)) {
  if (ii %% 25 == 0) { print(ii) }
  holder[isec_tor[[ii]]] <- shp_tor$AREA_NAME[ii]
}
shp_DA <- shp_DA %>% mutate(neighbourhood=holder)
shp_DA %>% filter(CSDNAME=='Toronto') %>% pull(neighbourhood) %>% equals('') %>% not %>% all %>% stopifnot()
# ggplot(filter(shp_DA,CSDNAME=='Toronto')) + geom_sf()
# Create a neigbourhood/DA dictionary
di_tor_neighbourhood <- shp_DA %>% 
  filter(CSDNAME=='Toronto') %>% as_tibble() %>% 
  dplyr::select(DAUID,neighbourhood) %>% 
  mutate(DAUID=as.integer(DAUID))
di_tor_neighbourhood <- df_csd_alt %>% 
  right_join(di_tor_neighbourhood,by=c('DA'='DAUID'))

# (ii) Calculate net density and net DAs
dat_delta_DA <- dat_delta_csd %>% 
  pivot_wider(c(DA,y1,y2),names_from='tt',values_from='d_m') %>%
  mutate_at(vars(matches('Lost|New')),list(~ifelse(is.na(.),0,.))) %>% 
  mutate(`Net Density`=`New Density`+`Lost Density`,
         `Net DAs`=`New DA`+`Lost DA`) %>% 
  dplyr::select(!matches('Lost|New',perl=T)) %>% 
  pivot_longer(starts_with('Net'),names_to='tt',values_to='d_m') %>% 
  mutate(d_m = ifelse(is.na(d_m),0,d_m))
# Merge with the neighbourhoods and sum
dat_delta_tor <- dat_delta_DA %>% 
  right_join(di_tor_neighbourhood,by=c('DA'='DAUID')) %>% 
  group_by(y1,y2,tt,neighbourhood) %>% 
  summarise(d_m=sum(d_m))

# Level population
dat_tor_level <- df_pop_iter %>% 
  right_join(di_tor_neighbourhood,by=c('DA'='DAUID')) %>% 
  mutate_at(vars(m1,m2),list(~ifelse(is.na(.),0,.))) %>% 
  group_by(neighbourhood,y1,y2) %>% 
  summarise_at(vars(m1,m2),list(~sum(.)))

# Make plot calculate net gains by region
dat_delta_tor_sum <- dat_delta_tor %>% 
  group_by(tt,neighbourhood) %>% 
  summarise(d_m=sum(d_m))

# Aggregate over both density and DAs
dat_delta_tor_tot <- dat_delta_tor_sum %>% 
  group_by(neighbourhood) %>% 
  summarise(d_m=sum(d_m))
dat_delta_tor_tot <- di_tor_neighbourhood %>% 
  group_by(csd_alt,neighbourhood) %>% count %>% 
  arrange(neighbourhood,csd_alt) %>% 
  ungroup %>% group_by(neighbourhood) %>% 
  slice(1) %>% dplyr::select(-n) %>% 
  right_join(dat_delta_tor_tot)


# (iii) Determine neighbourhoods by Old boundaries
gg_tor_preamalg_neighbourhoods <- dat_delta_tor_tot %>% 
  ggplot(aes(y=fct_reorder(neighbourhood,d_m),x=d_m/1e3,color=csd_alt)) + 
  geom_point() + theme_bw() + 
  theme(axis.title.y = element_blank(),legend.position = c(0.7,0.5), 
        axis.text.y = element_text(size=7)) + 
  labs(x='Change in population since 1971 (000s)') + 
  geom_vline(xintercept = 0) + 
  scale_color_discrete(name='Pre-amalgamation')

save_plot(file.path(dir_figures,'gg_tor_preamalg_neighbourhoods.png'),
          gg_tor_preamalg_neighbourhoods,base_height=13.5,base_width = 4.5)

tmp_txt <- dat_delta_tor_tot %>% group_by(csd_alt) %>% 
  summarise(q3=quantile(d_m,0.75),q1=quantile(d_m,0.25)) %>%
  mutate(iqr=q3-q1) %>% 
  left_join(dat_delta_tor_tot) %>% filter(d_m > 10000) # filter(d_m > q3 + 1.5*iqr)

# NOTICE 23 / 140 OF TORONTO'S NEIGHBOURHOODS ADED MORE THAN 10k PEOPLE SINCE 1971
gg_tor_preamalg_10k <- 
dat_delta_tor_tot %>% 
  ggplot(aes(x=csd_alt,y=d_m/1e3,color=csd_alt)) + 
  theme_bw() + geom_boxplot() + guides(color=F) + 
  theme(axis.title.x = element_blank()) + 
  labs(y='Change in population since 1971 (000s)',
       subtitle = 'Pre-amalgamation Toronto') + 
  geom_text_repel(aes(label=neighbourhood,y=d_m/1e3,x=csd_alt),data=tmp_txt,size=2.5) + 
  geom_point(aes(y=d_m/1e3,x=csd_alt),data=tmp_txt) 
save_plot(file.path(dir_figures,'gg_tor_preamalg_10k.png'),
          gg_tor_preamalg_10k,base_height=4,base_width = 6)


###############################
# ------ (3) MAP PLOTS ------ #

# (1) Figure 1: Net DA/Density changes for Toronto Neighbourhoods
tmp_shp <- shp_tor %>% right_join(dat_delta_tor,c('AREA_NAME'='neighbourhood'))

for (tt in c('Net Density','Net DAs')) {
  path <- file.path(dir_figures,str_c('gg_tor_',str_replace_all(tolower(tt),'\\s','_'),'.png'))
  gg_tor_gains <- ggplot(filter(tmp_shp,tt==!!tt)) + 
    geom_sf(aes(fill=d_m/1e3)) + theme_bw() + 
    scale_fill_gradient2(low='blue',high='red',mid='grey',midpoint = 0,
                         name='Change in population (000s): ',breaks=seq(-5000,20000,5000)/1e3) + 
    facet_wrap(~y2,ncol=4) + 
    theme(legend.position = 'bottom') + 
    labs(title=str_c('Population changes for Toronto: ',tt)) + 
    scale_x_continuous(breaks=c(-79.6,-79.4,-79.2)) + 
    scale_y_continuous(breaks=c(43.6,43.7,43.8))
  save_plot(path,gg_tor_gains,base_height = 6,base_width = 12)
}

# (2) Figure 2: Total Change from 1971-2016 for Toronto Neighbourhoods
tmp_shp2 <- shp_tor %>% right_join(dat_delta_tor_sum,c('AREA_NAME'='neighbourhood'))
# Pick top 20 from both
tmp_top <- dat_delta_tor_sum %>% mutate(s_m=sign(d_m)) %>% 
  group_by(s_m,tt) %>% arrange(-d_m*s_m) %>% 
  filter(s_m != 0) %>% mutate(s_m = ifelse(s_m == 1, 'Gain','Loss')) %>% 
  slice(1:10)
tmp_top <- right_join(shp_tor,tmp_top,c('AREA_NAME'='neighbourhood'))

gg_tor_tt <- ggplot(tmp_shp2) + 
  geom_sf(aes(fill=d_m/1e3)) + theme_bw() + 
  scale_fill_gradient2(low='blue',high='red',mid='grey',midpoint = 0,
                       name='Gain in population (000s): ') + 
  facet_wrap(~tt) + guides(color=F) + 
  theme(legend.position = 'bottom') + 
  labs(title='Aggregate population change by category (1971-2016)') + 
  scale_x_continuous(breaks=c(-79.6,-79.4,-79.2)) + 
  scale_y_continuous(breaks=c(43.6,43.7,43.8)) + 
  ggrepel::geom_text_repel(
    data = tmp_top,
    aes(label = AREA_NAME, geometry = geometry,color=s_m),
    stat = "sf_coordinates",
    min.segment.length = 0,size=3,max.overlaps = 50) + 
  scale_color_manual(values=c('darkred','darkblue'))
save_plot(file.path(dir_figures,'gg_tor_tt.png'),gg_tor_tt,base_height = 5,base_width = 10)

# (3) Figure 3: Population level/density by Toronto neighbourhood
tmp_shp3 <- shp_tor %>% right_join(dat_tor_level,c('AREA_NAME'='neighbourhood'))
gg_tor_level <- ggplot(tmp_shp3) + 
  geom_sf(aes(fill=m2/1e3)) + theme_bw() + 
  scale_fill_viridis_c(name='Population level (000s): ') + 
  facet_wrap(~y2,ncol=4) + 
  theme(legend.position = 'bottom') + 
  labs(title='Population level by neighbourhoods') + 
  scale_x_continuous(breaks=c(-79.6,-79.4,-79.2)) + 
  scale_y_continuous(breaks=c(43.6,43.7,43.8))
save_plot(file.path(dir_figures,'gg_tor_level.png'),gg_tor_level,base_height = 6,base_width = 12)

gg_tor_density <- ggplot(tmp_shp3) + 
  geom_sf(aes(fill=m2/(area*1000))) + theme_bw() + 
  scale_fill_viridis_c(name='Population Density (000s): ') + 
  facet_wrap(~y2,ncol=4) + 
  theme(legend.position = 'bottom') + 
  labs(title='Population level by neighbourhoods') + 
  scale_x_continuous(breaks=c(-79.6,-79.4,-79.2)) + 
  scale_y_continuous(breaks=c(43.6,43.7,43.8))
save_plot(file.path(dir_figures,'gg_tor_density.png'),gg_tor_density,base_height = 6,base_width = 12)

# (4) Figure 4: Gains by the census subdivision


##############################################
# ------ (4) STATISTICAL ASSOCIATIONS ------ #



df_tor_stats <- dat_delta_tor %>% 
  group_by(y1,y2,neighbourhood) %>% 
  summarise(d_m=sum(d_m)) %>% 
  left_join(as_tibble(shp_tor),by=c('neighbourhood'='AREA_NAME')) %>% 
  dplyr::select(-c(geometry,AREA_S_CD)) %>% 
  left_join(dat_tor_level,by=c('neighbourhood','y1','y2')) %>% 
  mutate(dens1=m1/(area*1000), dens2=m2/(area*1000)) %>% 
  pivot_longer(c(m1,m2,dens1,dens2),names_to='vv') %>% 
  filter(str_detect(vv,'2$')) %>% 
  mutate(vv=fct_recode(vv,'Pop. Level'='m2','Pop. Density'='dens2'))

# df_tor_stats %>% filter(vv == 'Pop. Level' & y2==2016) %>% with(.,plot(area,value))

ggplot(df_tor_stats,aes(x=value,y=d_m,color=as.character(y2))) + 
  geom_point(alpha=0.5,size=1) + theme_bw() + 
  geom_smooth(method='lm',color='black') + 
  facet_wrap(~vv,scales='free_x') + 
  labs(x='Population density/level',y='Level change in population',
       subtitle = 'Change between census years') + 
  scale_color_discrete(name='Census year')

dat_delta_tor %>% 
  group_by(tt,neighbourhood) %>% 
  summarise(d_m = sum(d_m)) %>% 
  pivot_wider(names_from = 'tt',values_from='d_m') %>% 
  ggplot(aes(x=(`Net DAs`),y=(`Net Density`))) + 
  theme_bw() + geom_point() + 
  geom_smooth(method = 'lm')




