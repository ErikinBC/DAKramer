#######################
# PROCESS GEOGRAPHIES #

# https://github.com/sjewo/cartogram
# https://cran.r-project.org/web/packages/tilegramsR/vignettes/UsingTilegramsInR.html

# https://everydayanalytics.ca/2016/03/plotting-choropleths-from-shapefiles-in-r-with-ggmap-toronto-neighbourhoods-by-population.html
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-eng.cfm


pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','forcats','data.table','broom',
           'ggplot2','cowplot','ggmap','ggh4x',
           'sf', 'rgdal','maptools')
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}
# devtools::install_github("teunbrand/ggh4x")

gpclibPermit()

dir_base <- getwd()
dir_data <- file.path(dir_base, 'data')
dir_figures <- file.path(dir_base, 'figures')
dir_output <- file.path(dir_base, 'output')
dir_output_shp <- file.path(dir_output, 'shp')
dir_shp <- file.path(dir_base, 'shp')

# Load population data
df_pop <- read_csv(file.path(dir_output,'df_pop.csv'), col_types = list(city=col_character()))
df_agg_pop <- read_csv(file.path(dir_output,'df_agg_pop.csv'), col_types = list(city=col_character()))

# Load geographic info
df_DA <- read_csv(file.path(dir_output,'df_DA.csv'), col_types = list(DA=col_integer()))
df_PC <- read_csv(file.path(dir_output,'df_PC.csv'), col_types = list(DA=col_integer()))

u_years <- unique(df_pop$year)
n_uyears <- length(u_years)
u_DAs <- unique(df_pop$DA)
u_csd <- unique(df_DA$csd)

cmas <- c('Toronto','Vancouver')

############################################
# ------ (2) SUBSET THE SHAPE FILES ------ #

# https://github.com/mylesmharrison/toronto_neighbourhoods
# (1) City of Toronto Boundary map
path_shp <- file.path(dir_shp,'toronto','NEIGHBORHOODS_WGS84_2.shp')
shp_tor <- read_sf(path_shp)
gg_map_tor <- ggplot(shp_tor) + theme_bw() +
  geom_sf() + 
  labs(x='Longitude',y='Latitude',subtitle='City of Toronto (not CMA)')
# gg_map_tor


# (2) Census Subdivision (CSD)
path_shp <- file.path(dir_shp,'gcsd_000b16a_e','gcsd000b16a_e.shp')
shp_csd <- read_sf(path_shp)
# Clean up
shp_csd$CSDNAME <- str_replace_all(shp_csd$CSDNAME,'\\s[0-9A-Z]$','')
stopifnot(all(u_csd %in% unique(shp_csd$CSDNAME)))
shp_csd <- filter(shp_csd,(CSDNAME %in% u_csd) & (CMANAME %in% cmas))
shp_csd <- dplyr::select(shp_csd,-c(CSDTYPE,PRNAME,CDUID,CDTYPE,CCSUID,ERUID,SACCODE,SACTYPE,CMAUID,CMAPUID,CMATYPE))
shp_csd$PRUID <- ifelse(shp_csd$PRUID=='59','BC','ON')

# (3) Dissemination Areas (DAs)
path_shp <- file.path(dir_shp,'gda_000b16a_e','gda_000b16a_e.shp')
shp_DA <- read_sf(path_shp)
# Clean up
stopifnot(all(u_DA %in% unique(shp_DA$DAUID)))
shp_DA <- filter(shp_DA,(DAUID %in% u_DAs) & (CMANAME %in% cmas))
shp_DA <- dplyr::select(shp_DA,-c(CSDTYPE,PRNAME,CDUID,CDTYPE,CCSUID,ERUID,SACCODE,SACTYPE,CMAUID,CMAPUID,CMATYPE,
                                  ERNAME,CTUID,CTNAME))
shp_DA$PRUID <- ifelse(shp_DA$PRUID=='59','BC','ON')


#############################
# ------ (3) FIGURES ------ #

# (i) Census subdivisions
holder_gg_csd <- list(Toronto=NULL,Vancouver=NULL)
nsamp=100
for (cma in cmas) {
  gg_csd <- ggplot(filter(shp_csd,CMANAME == cma)) + theme_bw() +  
    geom_sf() + 
    labs(x='Longitude',y='Latitude',subtitle='Census Subdivisions') + 
    geom_sf_text(aes(label=CSDNAME),size=3) + 
    facet_wrap(~CMANAME) + 
    theme(plot.margin = unit(c(0,0,0,0), "lines"))
  bounds <- st_bbox(filter(shp_csd,CMANAME == cma))
  xmin=bounds['xmin']; xmax=bounds['xmax']
  ymin=bounds['ymin']; ymax=bounds['ymax']
  df <- data.frame(lon=runif(nsamp,xmin,xmax),lat=runif(nsamp,ymin,ymax),val=rnorm(nsamp))
  df <- st_as_sf(df, coords=c('lon','lat'))
  df <- st_set_crs(df,4269)  # WGS84==NAD83==EPSG:4269
  gg_csd <- gg_csd + 
    geom_sf(data=df,aes(color=val)) + 
    scale_color_gradient2(low='blue',mid='white',high='red')
  holder_gg_csd[[cma]] <- gg_csd
}
gg_csd <- plot_grid(plotlist = holder_gg_csd,nrow=1, align='hv')
save_plot(file.path(dir_figures,'archive','gg_csd.png'),gg_csd,base_height = 5,base_width = 12)

# (ii) Random subset of DAs with CSD boundaries
shp_DA <- shp_DA %>% group_by(CMANAME) %>% mutate(idx=seq_along(DAUID)/length(DAUID)) %>% ungroup

holder_gg_DA <- list(Toronto=NULL,Vancouver=NULL)
for (cma in cmas) {
  gg_DA <- ggplot(sample_n(filter(shp_DA,CMANAME == cma),1000)) + theme_bw() +  
    geom_sf(aes(fill=idx),color=NA) + facet_wrap(~CMANAME) + 
    labs(x='Longitude',y='Latitude',subtitle='DAs') + 
    scale_fill_gradient2(low='blue',high='red',mid='grey',midpoint = 0.5) + 
    geom_sf(data=filter(shp_csd,CMANAME == cma),color='black',fill=NA) + 
    geom_sf_text(data=filter(shp_csd,CMANAME == cma),aes(label=CSDNAME),size=3)
  holder_gg_DA[[cma]] <- gg_DA
}
gg_DA <- plot_grid(plotlist = holder_gg_DA,nrow=1, align='hv')
save_plot(file.path(dir_figures,'archive','gg_DA.png'),gg_DA,base_height = 5,base_width = 12)

if ('idx' %in% colnames(shp_DA)) {
  shp_DA <- dplyr::select(shp_DA,-idx)
}

##############################################
# ------ (3) SAVE THE FILES FOR LATER ------ #

write_sf(shp_csd, file.path(dir_output_shp,'csd.shp'))
write_sf(shp_DA, file.path(dir_output_shp,'DA.shp'))

