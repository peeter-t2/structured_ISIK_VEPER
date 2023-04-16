
#indeksis on siin vigu.
ISIK_all <- fread(here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_metasimple.tsv"),sep="\t")
education_summary <- fread(here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_education_summary.tsv"),sep="\t")
dominant_domicile_locations <- fread(here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_biographies_summary.tsv"),sep="\t")
ISIK_all2 <- merge(ISIK_all,education_summary,by="ISIK.id",all=T)
ISIK_all3 <- merge(ISIK_all2,dominant_domicile_locations,by="ISIK.id",all=T)
ISIK_all3[,set:="ISIK"]
ISIK_all3[,id:=ISIK.id]
ISIK_all3[,.(Haridus)]

#põhineb bioraw-l ja on õige
ISIK_VEPER_meta <- fread(here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_VEPER_metasimple.tsv"),sep="\t")
ISIK_VEPER_meta[,set:=str_extract(V3,"ISIK|VEPER")]
ISIK_VEPER_meta[,id:=as.numeric(str_extract(V3,"[0-9]+"))]
ISIK_VEPER_meta[,.(Haridus)]


#add also ISIK VEPER births ja ISIK VEPER births gis
ISIK_VEPER_births <- fread(here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_VEPER_births_gis.tsv"),sep="\t")
ISIK_VEPER_meta2 <- merge(ISIK_VEPER_meta,ISIK_VEPER_births,by=c("set","id"),all=T)

#bioraw_matcher <- bioraw[V1=="Nime normitud kuju",.(V1,`Nime normitud kuju`=V2,V3)
#testmatches <- merge(ISIK_all3[,.(`Nime normitud kuju`=Nime.normitud.kuju,set,id,Sünniaeg)],ISIK_VEPER_meta[,.(`Nime normitud kuju`,set,id,Sünniaeg)],by=c("Nime normitud kuju","Sünniaeg"))#[,id2:=as.numeric(str_extract(V3,"[0-9]+"))][,set2:=str_extract(V3,"ISIK|VEPER")]

#gustaf grünwald kus on

ISIK_VEPER_meta2[,`Nime normitud kuju`:=str_remove(`Nime normitud kuju`,"~.*$")]

match_false_ids <- merge(ISIK_all3[`Nime.normitud.kuju`!="",.(`Nime normitud kuju`=Nime.normitud.kuju,old_set=set,old_id=id,Sünniaeg)],ISIK_VEPER_meta2[`Nime normitud kuju`!="",.(`Nime normitud kuju`,new_set=set,new_id=id)],by=c("Nime normitud kuju"))#[,id2:=as.numeric(str_extract(V3,"[0-9]+"))][,set2:=str_extract(V3,"ISIK|VEPER")] 
#mb use also synniaeg for join
#,Sünniaeg

mistakes <- match_false_ids[new_id!=old_id][new_set==old_set]

ISIK_all4 <- merge(match_false_ids,ISIK_all3,by.x=c("old_id","old_set"),by.y=c("id","set"),all=T)
check <- ISIK_all4[is.na(new_set)]
#don't do. it'll cause errors and its effectively empty anyway
#ISIK_all4[is.na(new_set),new_set:=old_set]
#ISIK_all4[is.na(new_id),new_id:=old_id]
all_combined <- merge(ISIK_all4,ISIK_VEPER_meta2,by.x=c("new_set","new_id"),by.y=c("set","id"),all=T)
names(all_combined)
fwrite(all_combined,here("data/publish/tidy_ISIK_VEPER/data/all_combined_maximal.tsv"),sep="\t")



str(all_combined)
#name
all_combined[,.(name.norm=`Nime normitud kuju.x`,name.alts=`Nime erikujud`)]
#birthinfo
all_combined[,.(birthdate=Sünniaeg.x,birthplace=Sünnikoht.x,deathdate=Surmaaeg.x,deathplace=Surmakoht.x)]
all_combined[,.(birthplace1,birthplace2,birth_county=maakond,birth_parish=kihelkond,birth_area=vald,birth_village=küla,birth_lat=lat,birth_lon=lon,birth_level_used=level_used)]


#profession
all_combined[,.(Ametid,Tegevusala,Töö,Elukutsed)]
all_combined[,profession:=Tegevusala]
all_combined[is.na(profession)|profession==""&(!is.na(Ametid)&Ametid!=""),profession:=Ametid]
all_combined[is.na(profession)|profession==""&(!is.na(Elukutsed)&Elukutsed!=""),profession:=Elukutsed]
all_combined[is.na(profession)|profession==""&(!is.na(Töö)&Töö!=""),profession:=paste0("UNEDITED",Töö)]
all_combined[is.na(profession)|profession==""]
seethis <- all_combined[,.(Ametid,Tegevusala,Töö,Elukutsed,profession)]
#all_combined[Elukutsed!="",.(Elukutsed)]
#all_combined[Ametid!="",.(Ametid)]


seethis <- all_combined[,.(Harrastused,Ühiskondlik.tegevus,Poliitiline.tegevus,`Ühiskondlik/rahvuslik tegevus`)]

all_combined[,.(hobbies=Harrastused,societal_activity=`Ühiskondlik/rahvuslik tegevus`)]

#chosen domicile
all_combined[,.(domicile_places,domicile_years_in_place_after_20=years_in_place_after_20,domicile_prop_in_plac=prop_in_place,domicile_koht,domicile_linn,domicile_kyla,domicile_lat2,domicile_lon2,domicile_raw=Elukohad)]


#education
seethis <- all_combined[,.(Haridus.x,Haridus.y,Aukraadid,max_school,max_school_level,max_school_name,years_in_school,Kõrgharidus,`Teaduslik kraad`)]

all_combined[,.(edu_raw=Haridus.y,edu_max_school=max_school,edu_max_school_level=max_school_level,edu_max_school_name=max_school_name,edu_years_in_school=years_in_school,edu_higher_raw=Kõrgharidus,edu_higher_degree=`Teaduslik kraad`)]

#family
all_combined[,.(Vanemad)]
all_combined[,.(Vennadoed)]
all_combined[,.(Abielus)]
all_combined[,.(Lapsed)]

all_combined[,.(family_parents=Vanemad,family_siblings=Vennadoed,family_marriage=Abielus,family_kids=Lapsed)]


#see1 <- all_combined[Haridus.x==Haridus.y&Haridus.y!="",.(Haridus.x,Haridus.y)]


all_combined_compact <- all_combined[,.(set=new_set,id=new_id,name.norm=`Nime normitud kuju.x`,name.alts=`Nime erikujud`,profession,hobbies=Harrastused,societal_activity=`Ühiskondlik/rahvuslik tegevus`,birthdate=Sünniaeg.x,birthplace=Sünnikoht.x,deathdate=Surmaaeg.x,deathplace=Surmakoht.x,birthplace1,birthplace2,birth_county=maakond,birth_parish=kihelkond,birth_area=vald,birth_village=küla,birth_lat=lat,birth_lon=lon,birth_level_used=level_used,domicile_places,domicile_years_in_place_after_20=years_in_place_after_20,domicile_prop_in_plac=prop_in_place,domicile_koht,domicile_linn,domicile_kyla,domicile_lat2,domicile_lon2,domicile_raw=Elukohad, edu_raw=Haridus.y,edu_max_school=max_school,edu_max_school_level=max_school_level,edu_max_school_name=max_school_name,edu_years_in_school=years_in_school,edu_higher_raw=Kõrgharidus,edu_higher_degree=`Teaduslik kraad`,family_parents=Vanemad,family_siblings=Vennadoed,family_marriage=Abielus,family_kids=Lapsed)]

fwrite(all_combined_compact,here("data/publish/tidy_ISIK_VEPER/data/all_combined_compact.tsv"),sep="\t")



all_combined_compact <- fread(here("data/publish/tidy_ISIK_VEPER/data/all_combined_compact.tsv"),sep="\t")
links_allcoauthors <- fread(here("data/publish/tidy_ISIK_VEPER/data/manual/links_allcoauthors.csv"),sep="\t")
#around 200 have 2 entries
all_combined_compact[,join_id:=paste0(set,id)]

links_allcoauthors_forlinking <- links_allcoauthors[,.(join_id=unlist(str_split(all_isiks," "))),.(coauthor,all_viafs,localid)]

with_ENB <- merge(links_allcoauthors_forlinking,all_combined_compact,by.x="join_id",by.y="join_id",all.x=T)
with_ENB_simpleclean <- with_ENB[order(set,id),.SD[1],.(coauthor)]

fwrite(with_ENB_simpleclean, "data/publish/tidy_ISIK_VEPER/data/export/export_compact_data_to_ENB.tsv",sep="\t")
#ühinemisega sai 2712 matchi.
#with_ENB on kõik nimed 1800-1940. korpusega linkimine on järgmine.


## 2253 nime on tegelikult lingitud sealt.
#manualchecked_all[enbset==T&!is.na(id)]





#detailed for plotting
fwrite(forplot_edu,here("data/publish/tidy_ISIK_VEPER/data/ISIK_education_detailed.tsv"),sep="\t")
fwrite(merged_coords_ISIK_all,here("data/publish/tidy_ISIK_VEPER/data/ISIK_biographies_detailed.tsv"),sep=",")

