

library(sf)
nc2 = st_read(here("data/raw/map_data/external/kih1897m_region.shp"), stringsAsFactors = FALSE)

#installed https://github.com/IRkernel/repr
#following https://github.com/ropensci/plotly/issues/1104
#to get plotly properly working, otherwise was really slow

nc3 <- st_transform(nc2, 4326) %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs')

#fwrite(merged_coords_ISIK_corp,"merged_coords_ISIK_corp_temp.tsv",sep="\t")

merged_coords_ISIK_corp<- fread(here("code/old_code/temp_data/merged_coords_ISIK_corp_temp.tsv"),sep="\t")
merged_coords_ISIK_corp<- fread(here("data/interim/merged_coord_ISIK_corp.tsv"),sep=",")
merged_coords_ISIK_corp<- fread(here("data/interim/merged_coord_ISIK_corp.tsv"),sep="\t")
merged_coords_ISIK_corp<- fread(here("data/publish/tidy_ISIK_VEPER/data/ISIK_biographies_detailed.tsv"),sep="\t")
,,sep=",")

#also jitter here
merged_coords_ISIK_corp[,factor_name:=as.numeric(as.factor(ISIK.id))]
merged_coords_ISIK_corp[,color_nr:=factor_name %% 8]
merged_coords_ISIK_corp[,beginyear:=as.numeric(beginyear)]

#summary(merged_coords[,.(beginyear)])
#summary(merged_coords[,.(endyear)])
#merged_coords[endyear>2000]
#merged_coords[is.na(endyear)]

#merged_coords_ISIK_corp[beginyear<1700]
#merged_coords_ISIK_corp[beginyear>2000]
#merged_coords_ISIK_corp[years>2000]
#see <- merged_coords_ISIK_corp[years<1700&!is.na(years)]
# see <- merged_coords_ISIK_corp[order(synddate)]
# merged_coords_ISIK_corp[,birthyear:=as.numeric(str_extract(synddate,"[0-9]{4}"))]
# merged_coords_ISIK_corp[is.na(birthyear),birthyear:=as.numeric(str_extract(Synd,"[0-9]{4}"))]
# merged_coords_ISIK_corp[episode2=="synd"&is.na(birthyear),birthyear:=beginyear]
# merged_coords_ISIK_corp[order(-birthyear)]
# ,birthyear:=as.numeric(na.locf(birthyear,na.rm=F)),ISIK.id]
# merged_coords_ISIK_corp[is.na(birthyear)]
#merged_coords_ISIK_corp[endyear<1700,endyear:=beginyear]
#merged_coords_ISIK_corp[endyear>2020,endyear:=beginyear]

forplot <- merged_coords_ISIK_corp[!is.na(endyear),.(year=unlist(beginyear:endyear),episode=paste0(unique(episode2),collapse=",")),by=.(places,ISIK.id,Nime.normitud.kuju,lon2,lat2,beginyear,endyear,color_nr,birthyear)][,rnorm:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)][,rnorm2:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)][!is.na(year)][,.SD[1],by=.(year,ISIK.id)] #taking just the first one removes 1/3 of dataset


#forplot[episode=="synd"]
#forplot[year<1700]
#forplot <- forplot[!beginyear<birthyear]
#forplot[episode=="synd"]
forplot[,birthyear:=as.numeric(min(year)),by=ISIK.id]
forplot[,age:=year-birthyear]
forplot[,maxage:=max(age),by=ISIK.id]
forplot[maxage>100]

events_sf <- forplot[!is.na(lat2)] %>%
  #now adding jitter here too, might be a bit more in fact in these coordinates mb
  mutate(lon2_jit=lon2+rnorm,lat2_jit=lat2+rnorm2)  %>%
  st_as_sf(coords = c("lon2_jit", "lat2_jit"), crs = 4326)


sf::sf_use_s2(FALSE)
events_nc3 <- st_transform(events_sf, 4326) %>% 
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs')# %>%
res <- st_join(events_nc3,nc3)
res <-
  res %>%
  mutate(color_nr=factor(color_nr)) %>%
  filter(year<1930&year>1800)# %>%

# %>% filter(year%in%1898:1900)

p <- ggplot() +
  geom_sf(data=nc3,aes(),alpha=0.1) +
  #geom_sf(data=events_sf) +
  geom_sf(data=res,aes(text=paste(Nime.normitud.kuju,"<br>",episode,"<br>",places,"<br>",KIHELKOND,"<br>",MAAKOND),frame=year,ids=ISIK.id,color=age)) +
  #coord_sf(crs = 4326, datum = NA) +
  #scale_fill_distiller("Area", palette = "Greens") +
  ggtitle("Korpuse autorite elukäik") +
  #facet_wrap(~year)+
  coord_fixed(ratio=3.5/2)+ #
  scale_color_gradient2( low = muted("green"), mid = "yellow",
                         high = muted("red"), midpoint = 35, space = "Lab",
                         na.value = "grey50", guide = "colourbar")+
  theme_bw()

ggplotly(p)%>%animation_opts(frame = 300,
                             easing = "linear",
                             redraw = FALSE)


#data is right, but how to make the map...?
#(Map, Laineste 1997)