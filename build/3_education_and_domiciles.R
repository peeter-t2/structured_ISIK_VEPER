merged_coords_ISIK_all<- fread(here("data/interim/merged_coord_ISIK_all.tsv"),sep=",")

#also jitter here
merged_coords_ISIK_all[,factor_name:=as.numeric(as.factor(ISIK.id))]
merged_coords_ISIK_all[,color_nr:=factor_name %% 8]
merged_coords_ISIK_all[,beginyear:=as.numeric(beginyear)]
merged_coords_ISIK_corp <- merged_coords_ISIK_all

#summary(merged_coords[,.(beginyear)])
#summary(merged_coords[,.(endyear)])
#merged_coords[endyear>2000]
#merged_coords[is.na(endyear)]


#merged_coords_ISIK_all[!is.na(endyear),.(year=unlist(beginyear:endyear)),by=.(places,ISIK.id,Nime.normitud.kuju,lon2,lat2,beginyear,endyear,color_nr)]
forplot <- merged_coords_ISIK_corp[!is.na(endyear),.(year=unlist(beginyear:endyear),episode=paste0(unique(episode2),collapse="")),by=.(places,ISIK.id,Nime.normitud.kuju,lon2,lat2,beginyear,endyear,color_nr)][,rnorm:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)][,rnorm2:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)][!is.na(year)]


forplot[episode=="synd"]
forplot[,birthyear:=min(year),by=ISIK.id]
forplot[,age:=year-birthyear]




# an issue that some years occur twice...

#merged_coords[,episode_type]
#
forplot_edu <- merged_coords_ISIK_corp[!is.na(endyear)&episode_type=="kool",.(year=unlist(beginyear:endyear),episode=paste0(unique(episode2))),by=.(places,ISIK.id,Nime.normitud.kuju,lon2,lat2,beginyear,endyear,color_nr,episode_level,episode2)][,rnorm:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)][,rnorm2:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)][!is.na(year)][,.SD[1],by=.(year,ISIK.id)][!str_detect(tolower(episode),"eüs|auvil")]
forplot_edu[,.N,by=ISIK.id][N>15][order(N)]
#forplot_edu[,.N,by=ISIK.id][,.(nn=.N),by=N] %>%
#  ggplot(aes(x=N,y=nn))+
#  geom_bar(stat="identity")

forplot_edu[,.N,by=ISIK.id][N>15][order(N)]


forplot_edu[,unique(episode_level)]
forplot_edu[str_detect(episode_level,"algk|külak|koduk|vallak|linnak|kirikuk|vabr.k|mõisak|"),school_level:=1]
forplot_edu[str_detect(episode_level,"kreisk|khkk|gümn|kunstik|algk.õp|min.k|muusikak"),school_level:=2]
forplot_edu[str_detect(episode_level,"aleksk|k.õp|õp-sem|g.õp"),school_level:=3]
forplot_edu[str_detect(episode_level,"polütehn|maamõõduk|lipn.k|kaub.k"),school_level:=4]
forplot_edu[str_detect(episode_level,"ülik|ped.kurs|õp.inst"),school_level:=5]

forplot_edu[,max_school:=max(school_level,na.rm=T),by=ISIK.id]

forplot_edu[,.(max_school=max(school_level,na.rm=T)),by=ISIK.id][,.N,by=max_school][max_school>0]# %>%
#  ggplot(aes(x=max_school,y=N))+
 # geom_bar(stat="identity")

#crude solution to duplicates - simply take the first one
forplot <- forplot[,.SD[1],by=.(year,ISIK.id)]

#setkey(forplot,year,ISIK.id)
#forplot <- forplot[duplicated(year),.SD,by=ISIK.id]
#no duplicates
#forplottest <- forplot[duplicated(forplot, by = key(forplot))]
#forplot <- forplot[!forplot[duplicated(forplot, by = key(forplot))]]



#forplot[duplicated(forplot,by=c("ISIK.id"),year)]#,.SD,by=ISIK.id]

forplot[,years_in_place:=.N-1,by=.(ISIK.id,places)]
forplot[age>20,years_in_place_after_20:=.N,by=.(ISIK.id,places)]
forplot[,total_years:=max(year)-min(year),by=.(ISIK.id)]
forplot[,prop_in_place:=years_in_place/total_years]


#for dominant domicile of tartu or tallinn, very crude...
dominant_domicile <-unique(forplot[,.(ISIK.id,places,years_in_place,prop_in_place)])[places%in%c("Tallinn","Tartu%20linn")][order(-years_in_place)][,.SD[1],by=ISIK.id][prop_in_place>.1]


dominant_domicile <-unique(forplot[age>20,.(ISIK.id,places,years_in_place_after_20,prop_in_place)])[order(-years_in_place_after_20)][,.SD[1],by=ISIK.id]#[prop_in_place>.1]



place_migrations <- merge(merged_coords_ISIK_all[episode_type=="synd"][,rnorm:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)][,rnorm2:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju,lat2,lon2)],dominant_domicile,by="ISIK.id")



#221 vs 145, can work with 145 for now., because the other would need new conversions...
#ISIK_corpus[Sünnikoht!="",.(Sünnikoht)]
#ISIK_corpus[Synd!="",.(Synd)]

#merged_coords[episode_type=="synd"]#,.(places,koht,koordinaadid,]



#now create merged_origin into probability 0,1 based on dialect maps. mainly for pää-pea etc

#muinasjutud ja murded have dialect maps...
#with coords, can possibly do kihelkond level, but that's also for later...


#domiciles, where did i do that

#ok, got dominant_domicile and years after_20
#dominant_domicile# may take in case it was really long, or right props...


dominant_domicile_locations <- merge(dominant_domicile,placestable3,by.x="places",by.y="places",all.x=T)[order(ISIK.id)]
names(dominant_domicile_locations)[c(1,5:12)] <- paste("domicile",names(dominant_domicile_locations)[c(1,5:12)],sep="_")
#popsize domicile,

#ed_levels, could have them all in time vs events too, or simply total, both could be valid...
#lifetime ed mb is better prediction of interest in matters like standardization...

ed_levels <- forplot_edu[order(-school_level),.(max_school_name=episode2[1],max_school_level=episode_level[1],max_school=max(school_level,na.rm=T)),by=ISIK.id][max_school>0]
ed_years <- forplot_edu[,.(years_in_school=.N),by=ISIK.id]

#can have hierarchy of levels, and take highest
#and can count levels in ed for this purpose...
merged_coords_ISIK_all[episode_type=="synd",birthyear:=beginyear]
merged1 <- merge(merged_coords_ISIK_all[episode_type=="synd"],dominant_domicile_locations,by="ISIK.id",all=T)
merged2 <- merge(merged1,ed_levels,on="ISIK.id",all.x=T)
merged2 <- merge(merged2,ed_years,on="ISIK.id",all.x=T)
merged2[,rnorm:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju)][,rnorm2:=rnorm(1,0)/10,by=.(places,Nime.normitud.kuju)]


fwrite(forplot_edu,here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_education_detailed.tsv"),sep="\t")
fwrite(merge(ed_levels,ed_years,by="ISIK.id",all=T),here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_education_summary.tsv"),sep="\t")
fwrite(dominant_domicile_locations,here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_biographies_summary.tsv"),sep="\t")


