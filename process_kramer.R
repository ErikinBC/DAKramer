################################
# --- PROCESS KRAMER FILES --- #

pckgs <- c('dplyr','tidyr','readr','magrittr','stringr','data.table') 
for (pp in pckgs) { library(pp,character.only=T,quietly = T,warn.conflicts = F)}

# DAs from 2016
# https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/adaprof/search-recherche/results-resultats-CD-ADA.cfm?Lang=E&PRCODE=35&CD_UID=3520&TABID=0
# For lda_000b16a_e
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm

dir_base <- getwd()
dir_data <- file.path(dir_base, 'DA_kramer')
dir_output <- file.path(dir_base, 'output')

# Year folders
yy_folders <- list.files(dir_data) %>% str_subset('^[0-9]')

holder <- list()
holder2 <- list()
for (yf in yy_folders) {
  fold <- file.path(dir_data, yf)
  fn_fold <- list.files(fold)
  fn_fold <- str_subset(fn_fold,'\\.csv$')
  # break
  if (yf == '2016 data') {
    cat('--- Doing special processing for 2016 ---\n')
    for (fn in fn_fold) {
      census_year <- 2016
      cat(sprintf('file: %s\n',fn))
      path <- file.path(fold, fn)
      tmp <- fread(path, select = c(1,2))
      colnames(tmp) <- c('geo','pop')
      tmp <- mutate(tmp, geo=str_split_fixed(geo,'\\s',2)[,1],pop=as.integer(pop))
      tmp2 <- tmp %>% filter(str_detect(geo,'[A-Z]')) %>%
        mutate(year=census_year) %>% rename(city=geo)
      tmp <-  tmp %>%
        filter(str_detect(geo,'^[0-9]')) %>%
        mutate(city=ifelse(str_detect(geo,'^35'),'Toronto','Vancouver')) %>%
        dplyr::rename(DA=geo) %>%
        mutate(DA=as.integer(DA),year=census_year)
      holder[[fn]] <- tmp
      holder2[[fn]] <- tmp2
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
      census_year <- as.integer(str_split(census_year,'\\s')[[1]][1])
      print(sprintf('Census year: %s',census_year))
      kk <- 2
      if (!is_v2) {
        city <- as.character(unlist(tmp[2,'DA']))
        city <- str_to_title(str_split(city,'\\s')[[1]][1])
        agg_pop <- as.integer(tmp[2,'pop'])
        cat(sprintf('City: %s, pop: %i\n', city, agg_pop))
        tmp2 <- tibble(year=census_year, city=city, pop=agg_pop)
        kk <- 3
        holder2[[fn]] <- tmp2
      }
      tmp <- mutate(tmp[kk:nrow(tmp),],year=census_year,
                    city=city, pop=as.integer(pop))
      holder[[fn]] <- tmp
    }
  }
}
# Bind data
df_agg_pop <- do.call('rbind',holder2) %>% arrange(city,year)
df_pop <- do.call('rbind',holder) %>% mutate(DA=as.integer(DA), year=as.integer(year))
# Save for later
write_csv(df_agg_pop,file.path(dir_output,'df_agg_pop.csv'))
write_csv(df_pop,file.path(dir_output,'df_pop.csv'))

# Ensure no DA/year/city duplicates
df_pop %>% group_by(city, year, DA) %>% count() %>% pull(n) %>% equals(1) %>% all %>% stopifnot

