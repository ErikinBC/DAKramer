
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