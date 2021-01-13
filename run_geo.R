#####################
# --- MAKE MAPS --- #

# https://github.com/sjewo/cartogram
# https://cran.r-project.org/web/packages/tilegramsR/vignettes/UsingTilegramsInR.html

# https://everydayanalytics.ca/2016/03/plotting-choropleths-from-shapefiles-in-r-with-ggmap-toronto-neighbourhoods-by-population.html
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-eng.cfm

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','forcats','data.table','broom',
           'ggplot2','cowplot','ggmap','ggh4x',
           'sf', 'rgdal','maptools')
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}
# library(maps);library(mapdata);library(maptools)
# devtools::install_github("teunbrand/ggh4x")

gpclibPermit()

dir_base <- getwd()
dir_data <- file.path(dir_base, 'data')
dir_figures <- file.path(dir_base, 'figures')
dir_output <- file.path(dir_base, 'output')


##################################
# ------ (1) LOAD IN DATA ------ #

# Load population data
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
df_agg_pop <- read_csv(file.path(dir_output,'df_agg_pop.csv'), col_types = list(city=col_character()))

# Load geographic info
df_DA <- read_csv(file.path(dir_output,'df_DA.csv'), col_types = list(DA=col_integer()))
df_PC <- read_csv(file.path(dir_output,'df_PC.csv'), col_types = list(DA=col_integer()))

u_years <- unique(df_pop$year)
n_uyears <- length(u_years)
u_DAs <- unique(df_pop$DA)
u_DAs_tor <- unique(filter(df_pop,city=='Toronto')$DA)
u_DAs_van <- unique(filter(df_pop,city=='Vancouver')$DA)
u_csd <- unique(df_DA$csd)
u_csd_tor <- unique(filter(df_DA,cma=='Toronto')$csd)
u_csd_van <- unique(filter(df_DA,cma=='Vancouver')$csd)

# Load data from analysis script
dat_DA_rho <- read_csv(file.path(dir_output, 'dat_DA_rho.csv'),col_types=list(DA=col_integer()))

############################################
# ------ (2) SUBSET THE SHAPE FILES ------ #

# https://github.com/mylesmharrison/toronto_neighbourhoods
# (1) City of Toronto Boundary map
shp_tor <- readOGR('tor_neighbourhoods/NEIGHBORHOODS_WGS84_2.shp')
shp_tor_df <- fortify(shp_tor,region = 'AREA_S_CD') %>% 
  mutate(id=as.integer(id))

tmp_DA_delta <- df_DA %>% filter(city1=='Toronto') %>% 
  left_join(dat_DA_rho,by='DA') %>% filter(s_DA %in% c('Gain','Loss'))

gg_map_tor <- ggplot(shp_tor_df) + 
  geom_polygon(aes(x=long,y=lat, group=group, alpha=0.25), fill='grey') + 
  geom_polygon(aes(x=long,y=lat, group=group), color='black', fill=NA) + 
  coord_map() + guides(alpha=F) + 
  labs(x='Longitude',y='Latitude',subtitle='City of Toronto (not CMA)') + 
  theme_bw()

gg_map_tor_delta <- gg_map_tor + 
  geom_point(aes(x=Longitude,y=Latitude,color=pct),data=tmp_DA_delta,inherit.aes=F, size=1,alpha=0.5) + 
  scale_color_gradient2(low='blue',high='red',mid='grey',midpoint = 0.5
                        ,name='% of times density changed over the census years: ') + 
  theme(legend.position = 'bottom') + 
  facet_wrap(~s_DA)
save_plot(file.path(dir_figures,'gg_map_tor_delta.png'),gg_map_tor_delta,base_height=5,base_width=10)

# (2) Census Subdivision (CSD)
shp_csd <- readOGR('lcsd000b16a_e/lcsd000b16a_e.shp')
shp_csd <- shp_csd[shp_csd@data$CCSNAME %in% u_csd_tor & shp_csd@data$CMANAME == 'Toronto',]
# Tidy it up
shp_csd_df <- tidy(shp_csd) %>%
  left_join(shp_csd@data[,c('CSDUID','CSDNAME','CMANAME')] %>% tibble::rownames_to_column('id'),'id')

# https://gis.stackexchange.com/questions/377930/how-to-project-lambert-conformal-conic-to-wgs84-in-python
# https://gis.stackexchange.com/questions/252627/r-plotting-2-polygon-shapefiles-problems-with-crs-extent

st_crs(shp_csd)
st_crs(shp_tor)
shp_tor@proj4string
shp_csd@proj4string








# demo  <- read.csv('tor_neighbourhoods/WB-Demographics.csv')[,1:3]
# gg_map_tor <- ggplot(shp_tor_df) + 
#   geom_polygon(aes(x=long,y=lat, group=group, alpha=0.25), fill='grey') + 
#   geom_polygon(aes(x=long,y=lat, group=group), color='black', fill=NA) + 
#   coord_map() + guides(alpha=F) + 
#   labs(x='Longitude',y='Latitude') + theme_bw()
# gg_map_tor
# 
# points2 <- merge(shp_tor_df, demo, by.x='id', by.y='Neighbourhood.Id', all.x=TRUE)
# 
# gg_map_tor + 
#   geom_polygon(aes(x=long,y=lat, group=group, fill=Total.Area), data=points2, color='black',inherit.aes = F)
# 

# 
# # Get a rough mapping to lat/lon
# mi_sc <- apply(filter(shp_csd_df,CMANAME=='Toronto')[,c('long','lat')],2,min)
# mx_sc <- apply(filter(shp_csd_df,CMANAME=='Toronto')[,c('long','lat')],2,max)
# 
# mi_lon_sc <- mi_sc['long']
# mx_lon_sc <- mx_sc['long']
# mi_lat_sc <- mi_sc['lat']
# mx_lat_sc <- mx_sc['lat']
# 
# mi_lon_tor <- shp_tor@bbox['x','min']
# mx_lon_tor <- shp_tor@bbox['x','max']
# mi_lat_tor <- shp_tor@bbox['y','min']
# mx_lat_tor <- shp_tor@bbox['y','max']
# 
# shp_csd_df <- shp_csd_df %>% 
#   mutate(long2=mi_lon_tor+(long-mi_lon_sc)*(mx_lon_tor-mi_lon_tor)/(mx_lon_sc-mi_lon_sc),
#          lat2=mi_lat_tor+(lat-mi_lat_sc)*(mx_lat_tor-mi_lat_tor)/(mx_lat_sc-mi_lat_sc))
# 
# qq <- shp_csd_df %>% group_by(CSDNAME) %>% summarise_at(vars(c(long2,lat2)),~mean(.))
# 
# gg_cma_tor <- ggplot(shp_csd_df) + 
#   geom_polygon(aes(x=long2,y=lat2, group=group, alpha=0.25), fill='grey') + 
#   geom_polygon(aes(x=long2,y=lat2, group=group), color='black', fill=NA) + 
#   guides(alpha=F) + 
#   theme_bw() + coord_map()
# 
# gg_cma_tor + 
#   stat_midpoint(aes(x=long2,y=lat2, group=id,label = CSDNAME), geom = "text",size=3)
# 
# # Conic projections? (https://en.wikipedia.org/wiki/Lambert_conformal_conic_projection)
# # See if we can map the lat/lon to approximately what we get here...?
# # https://giscourses.cfans.umn.edu/sites/giscourses.cfans.umn.edu/files/5480chapter3_projections_excerpt.pdf



