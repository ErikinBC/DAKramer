#####################
# --- MAKE MAPS --- #

# https://geocompr.robinlovelace.net/spatial-cv.html
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

cmas <- c('Toronto','Vancouver')

##################################
# ------ (1) LOAD IN DATA ------ #

# Load population data
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
df_agg_pop <- read_csv(file.path(dir_output,'df_agg_pop.csv'), col_types = list(city=col_character()))

# Load Census annotations
df_census <- read_csv(file.path(dir_output,'df_census.csv'),col_types = list(year=col_integer()))

# Load the CSD data
dat_delta_csd_tot <- read_csv(file.path(dir_base,'output','dat_delta_csd_tot.csv'))

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
# Add on area
shp_DA <- mutate(shp_DA, area = as.numeric(units::set_units(st_area(shp_DA),km^2)))
shp_DA$DAUID <- as.integer(shp_DA$DAUID)
# ggplot(filter(shp_DA,CMANAME=='Vancouver')) + geom_sf(aes(fill=area),color=NA)

path_shp_tor <- file.path(dir_shp,'toronto','NEIGHBORHOODS_WGS84_2.shp')
shp_tor <- read_sf(path_shp_tor) %>% 
  mutate(AREA_NAME = str_replace_all(AREA_NAME, '\\s\\(.*\\)','')) %>% 
  st_transform(crs='NAD83')
shp_tor <- mutate(shp_tor, area = as.numeric(units::set_units(st_area(shp_tor),km^2)))

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
  right_join(di_tor_neighbourhood,by='DA') %>% 
  group_by(y1,y2,tt,neighbourhood) %>% 
  summarise(d_m=sum(d_m))

# Level population
dat_tor_level <- df_pop_iter %>% 
  right_join(di_tor_neighbourhood,by='DA') %>% 
  mutate_at(vars(m1,m2),list(~ifelse(is.na(.),0,.))) %>% 
  group_by(neighbourhood,y1,y2) %>% 
  summarise_at(vars(m1,m2),list(~sum(.))) %>% ungroup

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
  right_join(dat_delta_tor_tot)  %>% 
  arrange(-d_m) %>% ungroup %>% 
  mutate(gg=cut(d_m,breaks=c(-12000,0,5000,60000),labels=c('<0','0-5K','>5K'))) %>% 
  mutate(csd_alt = ifelse(csd_alt == 'Toronto', 'Old Toronto', csd_alt))
dat_delta_tor_tot 

# https://www.toronto.ca/city-government/data-research-maps/neighbourhoods-communities/neighbourhood-profiles/
# (iii) Determine neighbourhoods by Old boundaries
gg_tor_preamalg_neighbourhoods <- dat_delta_tor_tot %>% 
  ggplot(aes(y=fct_reorder(neighbourhood,d_m),x=d_m/1e3,color=csd_alt)) + 
  geom_point() + theme_bw() + 
  theme(axis.title.y = element_blank(),legend.position = 'bottom', 
        axis.text.y = element_text(size=7)) + 
  labs(x='Change in population since 1971 (000s)',subtitle = "Toronto's 140 Neighbourhoods") + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~gg,scales='free') + 
  scale_color_discrete(name='Pre-amalgamation')
save_plot(file.path(dir_figures,'gg_tor_preamalg_neighbourhoods.png'),
          gg_tor_preamalg_neighbourhoods,base_height=6,base_width = 10)
# Statistical enrichment?
dat_delta_tor_tot %>% 
  mutate(gg=fct_recode(gg,'pos'='>5K','pos'='0-5K','neg'='<0')) %>% 
  group_by(csd_alt,gg) %>%
  count %>% pivot_wider(names_from='gg',values_from='n') %>%
  mutate_if(is.numeric,list(~ifelse(is.na(.),0,.))) %>% 
  mutate(pct = neg / (neg+pos))
  

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
       subtitle = "Toronto's 140 neigbourhoods: pre-amalgamation") + 
  geom_text_repel(aes(label=neighbourhood,y=d_m/1e3,x=csd_alt),data=tmp_txt,size=2.5) + 
  geom_point(aes(y=d_m/1e3,x=csd_alt),data=tmp_txt) 
save_plot(file.path(dir_figures,'gg_tor_preamalg_10k.png'),
          gg_tor_preamalg_10k,base_height=4,base_width = 6)

# (iv) Merge census data with the Toronto-area regions
shp_tor_census <- shp_DA %>% 
  dplyr::select(DAUID,CSDNAME,geometry,neighbourhood) %>% 
  filter(neighbourhood != '') %>% dplyr::rename(DA=DAUID) %>% 
  mutate(DA=as.integer(DA)) %>% 
  left_join(df_census,by='DA') %>% dplyr::select(-c(prov))
shp_tor_census_mu <- shp_tor_census %>% 
  group_by(neighbourhood) %>% 
  summarise_at(vars(singd,semd,row,income,apt),list(~weighted.mean(x=.,w=pop,na.rm = T)))
##  Note the overlap is imprecise
# ggplot() + 
#   geom_sf(data=shp_tor_census_mu,color='red',fill=NA) + 
#   geom_sf(data=shp_tor,color='black',fill=NA) + 
#   scale_fill_viridis_b() 
  
# (v) Get DA level changes
# shp_DA_delta <- 

tmp1 <- df_pop %>% pivot_wider(names_from='year',values_from='pop') %>% 
  pivot_longer(!c(city,DA),names_to='year',values_to='pop')
# First get the census-wise changes
tmp2 <- tmp1 %>% mutate(pop=ifelse(is.na(pop),0,pop)) %>% 
  mutate(dpop=pop-lag(pop,1)) %>% group_by(DA)
# Then get the change from 2016
tmp3 <- tmp2 %>% filter(year %in% c(1971, 2016)) %>% 
  pivot_wider(c(DA,city),names_from='year',values_from='pop') %>% 
  mutate(d_m=`2016` - `1971`) %>% dplyr::select(DA,city,d_m)
# # Get the max density change: NOTE BASICALLY VISUALLY INDISTINGUISHABLE
# tmp4 <- tmp1 %>% group_by(DA) %>% mutate(ridx=row_number()) %>%
#   filter(year == 2016 | !is.na(pop)) %>% 
#   filter(year == 2016 | ridx == min(ridx)) %>% 
#   mutate(year = ifelse(year == 2016, 'y2', 'y1')) %>% 
#   pivot_wider(c(DA,city),names_from='year',values_from='pop') %>% 
#   mutate(y1 = ifelse(is.na(y1),0,y1)) %>% drop_na %>% 
#   mutate(d_m = y2 - y1) %>% ungroup
# Give the net change for the census
df_census_net <- df_census %>% left_join(tmp3,by='DA') %>% 
  left_join(dplyr::select(shp_DA,DAUID,area),by=c('DA'='DAUID'))

shp_DA_delta_tot <- shp_DA %>% dplyr::select(DAUID,geometry,area) %>% 
  left_join(tmp3,by=c('DAUID'='DA'))
shp_DA_delta_year <- shp_DA %>% dplyr::select(DAUID,geometry,area) %>% 
  left_join(tmp2,by=c('DAUID'='DA'))
# shp_DA_delta_dens <- shp_DA %>% dplyr::select(DAUID,geometry,area) %>% 
#   left_join(tmp4,by=c('DAUID'='DA'))


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
  theme(legend.position = 'bottom',axis.title = element_blank()) + 
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
  scale_fill_viridis_c(name='Population Density : ') + 
  facet_wrap(~y2,ncol=4) + 
  theme(legend.position = 'bottom') + 
  labs(title='Population level by neighbourhoods') + 
  scale_x_continuous(breaks=c(-79.6,-79.4,-79.2)) + 
  scale_y_continuous(breaks=c(43.6,43.7,43.8))
save_plot(file.path(dir_figures,'gg_tor_density.png'),gg_tor_density,base_height = 6,base_width = 12)

# (4) Figure 4: Gains highlighted by DA
for (cma in cmas) {
  tmp_gg <- ggplot(filter(shp_DA_delta_tot,city==cma)) + theme_bw() + 
    geom_sf(aes(fill=d_m),color=NA) + facet_wrap(~city) + 
    theme(axis.title = element_blank(),legend.position = 'bottom') + 
    labs(subtitle = 'Δ: 1971 to 2016') + 
    scale_fill_gradient2(name='Pop. change: ', low='blue',high='red',mid='grey',midpoint = 0) + 
    geom_sf(data=filter(shp_csd,CMANAME==cma),color='black',fill=NA) + 
    ggrepel::geom_text_repel(
      data = filter(shp_csd,CMANAME==cma),
      aes(label = CSDNAME, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0,size=3,max.overlaps = 50)
  ww <- 5
  if (cma == 'Vancouver') { 
    ww <- 7
  }
  save_plot(file.path(dir_figures,str_c('gg_DA_map_',cma,'.png')),tmp_gg,base_height = 6, base_width = ww)
}



##############################################
# ------ (4) STATISTICAL ASSOCIATIONS ------ #

cn_lbl <- c('apt'='Apartment (%)','area'='Area (km2)','income'='Median Income (000s)',
            'row'='Rowhouse (%)', 'semd'='Semi-detached (%)', 'singd'='Single-family (%)')

# # --- (i) ON THE DA BASIS --- #
# dat_census_DA <- df_census_net %>% 
#   mutate(tot=singd+semd+row+apt,income = income/1e3) %>% 
#   mutate_at(vars(singd,semd,row,apt),~(./tot)) %>% dplyr::select(-tot) %>% 
#   dplyr::select(city,DA,d_m,income,area,singd,semd,row,apt) %>% 
#   pivot_longer(!c(city,DA,d_m),names_to='vv')
# 
# dat_census_DA %>% 
#   ggplot(aes(x=value,y=d_m,color=city)) + geom_point() + 
#   facet_grid(~vv,scales='free',labeller=labeller(vv=cn_lbl)) + 
#   geom_smooth(method='lm',se = F)

# --- (ii) CITY OF TORONTO --- #
# Can we predict lifetime population increases as a function of....
df_tor_stats <-
  dat_delta_tor_tot %>% 
  left_join(as_tibble(shp_tor),by=c('neighbourhood'='AREA_NAME')) %>% 
  dplyr::select(-c(geometry,AREA_S_CD)) %>% 
  left_join(as_tibble(shp_tor_census_mu),'neighbourhood') %>% 
  dplyr::select(-geometry) %>% 
  left_join(dplyr::select(filter(dat_tor_level,y2==2016),neighbourhood,m2),by='neighbourhood') %>% 
  dplyr::rename(pop2016=m2) %>% 
  mutate(tot=singd+semd+row+apt) %>% 
  mutate_at(vars(singd,semd,row,apt),~(./tot)) %>% dplyr::select(-tot)
  # mutate(sfd = singd+semd, mfd=row+apt) %>% 
  # dplyr::select(-c(singd,semd,row,apt)) %>% 
  # mutate_at(vars(sfd, mfd),list(~./(sfd+mfd))) # population per housing type

Xvars <- c('area','income','singd','semd','row','apt')  #'sfd','mfd'
df_X <- data.frame(scale(df_tor_stats[,Xvars],center = T,scale = T)) %>% mutate(y = df_tor_stats$d_m/1e3)
colnames(df_X) <- c(Xvars,'y')
df_X <- as_tibble(df_X)
library(sjPlot)
mdl <- lm(y ~ area+income+I(singd+semd)+I(row+apt), data=df_X)
summary(mdl)

tab_model(mdl)

# Leaving apartment's out
tmp <- df_tor_stats %>% mutate(income=income/1e3, d_m=d_m/1e3) %>% 
  pivot_longer(!c(csd_alt, neighbourhood, d_m,pop2016,gg),names_to='vv') %>% 
  mutate(vv=factor(vv,levels=Xvars)) 
gg_statistical_assoc <- ggplot(tmp, aes(x=value,y=d_m)) + 
  theme_bw() + geom_point() + 
  facet_wrap(~vv,scales='free_x',labeller = labeller(vv=cn_lbl)) + 
  geom_smooth(method='lm') + 
  labs(y='Change in population since 1971', x='Value',
       subtitle = "Toronto's 140 neighbourhoods: feature values from 2016 census")
save_plot(file.path(dir_figures,'gg_statistical_assoc.png'),gg_statistical_assoc,
          base_height = 5,base_width = 8)


# df_tor_stats <- dat_delta_tor %>% 
#   group_by(y1,y2,neighbourhood) %>% 
#   summarise(d_m=sum(d_m)) %>% 
#   left_join(as_tibble(shp_tor),by=c('neighbourhood'='AREA_NAME')) %>% 
#   dplyr::select(-c(geometry,AREA_S_CD)) %>% 
#   left_join(dat_tor_level,by=c('neighbourhood','y1','y2')) %>% 
#   mutate(dens1=m1/(area*1000), dens2=m2/(area*1000)) %>% 
#   pivot_longer(c(m1,m2,dens1,dens2),names_to='vv') %>% 
#   filter(str_detect(vv,'2$')) %>% 
#   mutate(vv=fct_recode(vv,'Pop. Level'='m2','Pop. Density'='dens2'))

# df_tor_stats %>% filter(vv == 'Pop. Level' & y2==2016) %>% with(.,plot(area,value))





