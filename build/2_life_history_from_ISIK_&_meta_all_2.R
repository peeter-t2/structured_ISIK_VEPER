
## Status

# Can plot the education details as a moving thing too...

# with each book added to that particular education group that they belong to...


# and or education attainment + how many years lived in there too...


# for each of these variables, for each of these data sources...

library(data.table)
library(stringr)
library(dplyr)
library(zoo)

#more cleaned

#this is then the subset of all authors with more than one work.,
#possibly the ones with just one work can be informative too, if they match another person.
#not enough for the community info but enough for other stuff...
#merged_only_fixed <- fread("data/raw/merged_only_manually_fixed.csv")
ISIK_all <- data.table(read.csv("data/raw/autorid/ISIK_basic_sorted_conv_manual_fixed_safecopy_changed.csv", header=TRUE, sep=",",stringsAsFactors = FALSE, fileEncoding="UTF-8", encoding="UTF-8"))
#ISIK_corpus <- fread("data/raw/ISIK_merged_only_manually_aligned.tsv",sep="\t")

ISIK_all[, ISIK.id:=as.factor(ISIK.id)]
ISIK_all[, surmdate:=as.Date(Surmaaeg, "%Y-%m-%d")]
ISIK_all[, synddate:=as.Date(Sünniaeg, "%Y-%m-%d")]
fwrite(ISIK_all,here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_metasimple.tsv"),sep="\t")


#split at comma, add together, make into sequences per person...
t88test <- ISIK_all
t88test <- t88test[,.(episode=unlist(strsplit(Töö,"[,;] ", fixed=F))), by= .(ISIK.id,Nime.normitud.kuju,synddate,surmdate,Synd, Sünnikoht,Surmakoht)] #,Töö_orig=Töö
t88test[,birthyear:=substr(synddate,1,4)]
t88test[,deathyear:=substr(surmdate,1,4)]
t88test[,birthplace:=str_match(Sünnikoht,"[^[, ]]+")]
t88test[,deathplace:=str_match(Surmakoht,"[^[, ]]+")]

#t88test[,years:=str_match(episode,"[^ ]+[0-9][^ ]+")]
t88test[,years:=str_match(episode,"[0-9]{4}[^ ]+")]
#t88test[,years:=str_match(episode,"[^ ]+[0-9][^$]+")]
#t88test[,years:=str_match(episode,"[^ ]+[0-9][^$]+")]
t88test[nchar(years)<4,years:=NA]
t88test[is.na(years),years:=str_match(episode,"[0-9]{4}")]
#t88test[endyear<200]
t88test[,beginyear:=as.numeric(str_match(years,"[0-9]{4}"))]
#t88test[str_detect(years,"[0-9]{4}[^0-9]+[0-9]{4}")]
#[[1]]==2),endyear:=str_match_all(years,"[0-9]{4}")[[1]][2]]
t88test[Nime.normitud.kuju=="Jürmann, Michael"]

t88test[,endyear:=str_match(years,"--[0-9].*")]
t88test[,endyear:=str_replace_all(endyear,"--","")]
t88test[,endyear:=str_replace_all(endyear,"\\.$","")]
t88test[nchar(endyear)==2,endyear:=paste0(substr(beginyear,1,2),endyear)]
t88test[str_detect(endyear,"[0-9]+\\.[A-Z]+"),endyear:=str_match(episode,"[0-9]{4}--.*([0-9]{4})")[,2]]

#t88test[,endyear:=NULL]
#t88test[endyear==1927]
#t88test[Nime.normitud.kuju=="Lattik, Jaan"]
#t88test[nchar(endyear)==1,endyear:=paste0(substr(beginyear,1,2),endyear)]
t88test[,endyear:=as.numeric(endyear)]
t88test[endyear<beginyear,endyear:=endyear+100]
t88test[,episode2:= trimws(gsub("[^ ]+[0-9][^ ]+","",episode))]
t88test[,episode2:= trimws(gsub("[^ ]+[0-9][^ ]+","",episode2))]
t88test[,episode_type:="töö"]

#just testing the eveents
t88test[(endyear-beginyear)>1] #and could add locations here.
t88test[(endyear-beginyear)>1,unique(Nime.normitud.kuju)]



temp <- t88test[,.(ISIK.id,Nime.normitud.kuju,beginyear=birthyear,place=birthplace,episode2="synd",episode_type="synd")][!duplicated(ISIK.id)]
t88test3 <- rbind(t88test,temp,fill=T)
temp2 <- t88test[,.(ISIK.id,Nime.normitud.kuju,beginyear=deathyear,place=deathplace,episode2="surm",episode_type="surm")][!duplicated(ISIK.id)]
t88test3 <- rbind(t88test3,temp2,fill=T)



haridustest <- ISIK_all[!duplicated(ISIK.id)] #this is the ones that were merged with isik.
haridustest <- haridustest[,.(episode=unlist(strsplit(Haridus,", ", fixed=TRUE))), by= .(ISIK.id,Nime.normitud.kuju,synddate,surmdate,Synd, Sünnikoht,Surmakoht)]
haridustest[,years:=str_match(episode," [^ ]+[0-9][^ ]+")]
haridustest[,episode2:= trimws(gsub(" [^ ]+[0-9][^ ]+","",episode))]


haridustest[,birthyear:=substr(synddate,1,4)]
haridustest[,deathyear:=substr(surmdate,1,4)]
haridustest[,birthplace:=str_match(Sünnikoht,"[^[, ]]+")]
haridustest[,deathplace:=str_match(Surmakoht,"[^[, ]]+")]

haridustest[,years:=str_match(episode,"[^ ]+[0-9][^ ]")]
haridustest[nchar(years)<4,years:=NA]
haridustest[is.na(years),years:=str_match(episode,"[0-9]{4}")]

haridustest[,beginyear:=as.numeric(str_match(years,"[0-9]{4}"))]
haridustest[,endyear:=str_match(years,"--[0-9]+")]
haridustest[,endyear:=str_replace_all(endyear,"--","")]
haridustest[nchar(endyear)<3,endyear:=paste0(substr(beginyear,1,2),endyear)]
haridustest[,endyear:=as.numeric(endyear)]
haridustest[,episode2:= trimws(gsub("[^ ]+[0-9][^ ]+","",episode))]
haridustest[,episode2:= trimws(gsub("[^ ]+[0-9][^ ]+","",episode2))]
haridustest[,episode_type:="kool"]

haridustest[str_detect(episode2,"algk")]
haridustest[str_detect(episode2,"[Aa]lgk"),episode_level:="algk"]
haridustest[str_detect(episode2,"khkk"),episode_level:="khkk"]
haridustest[str_detect(episode2,"kreisk"),episode_level:="kreisk"]
haridustest[str_detect(episode2,"vaestek"),episode_level:="vaestek"]
haridustest[str_detect(episode2,"vaeslastek"),episode_level:="vaeslastek"]
haridustest[str_detect(episode2,"lipn.k"),episode_level:="lipn.k"]
haridustest[str_detect(episode2,"[Kk]-k"),episode_level:="külak"]
haridustest[str_detect(episode2,"külak"),episode_level:="külak"]
haridustest[str_detect(episode2,"[Kk]oduk"),episode_level:="koduk"]
haridustest[str_detect(episode2,"[Vv]-k"),episode_level:="vallak"]
haridustest[str_detect(episode2,"vallak"),episode_level:="vallak"]
haridustest[str_detect(episode2,"kir.k"),episode_level:="kirikuk"]
haridustest[str_detect(episode2,"kirikuk"),episode_level:="kirikuk"]
haridustest[str_detect(episode2,"[Ee]rak"),episode_level:="erak"]
haridustest[str_detect(episode2,"vabr.k"),episode_level:="vabr.k"]
haridustest[str_detect(episode2,"kaub.k"),episode_level:="kaub.k"]
haridustest[str_detect(episode2,"maamõõduk"),episode_level:="maamõõduk"]
haridustest[str_detect(episode2,"käsit.k"),episode_level:="käsit.k"]
haridustest[str_detect(episode2,"polütehn"),episode_level:="polütehn"]
haridustest[str_detect(episode2,"metsand.k"),episode_level:="metsand.k"]
haridustest[str_detect(episode2,"min.k"),episode_level:="min.k"]
haridustest[str_detect(episode2,"kub.g"),episode_level:="gümn"]
haridustest[str_detect(episode2,"kõrgem tütarl.k"),episode_level:="gümn"]
haridustest[str_detect(episode2,"ühisg"),episode_level:="gümn"]
haridustest[str_detect(episode2,"reaalk"),episode_level:="gümn"]
haridustest[str_detect(episode2,"muusikak"),episode_level:="muusikak"]
haridustest[str_detect(episode2,"l-k"),episode_level:="linnak"]
haridustest[str_detect(episode2,"linnak"),episode_level:="linnak"]
haridustest[str_detect(episode2,"kunstik"),episode_level:="kunstik"]
haridustest[str_detect(episode2,"ms-k"),episode_level:="mõisak"]
haridustest[str_detect(episode2,"[Tt][Üü]"),episode_level:="ülik"]
haridustest[str_detect(episode2,"ülik"),episode_level:="ülik"]
haridustest[str_detect(episode2,"Rotalia"),episode_level:="ülik"]
haridustest[str_detect(episode2,"PEüs"),episode_level:="ülik"]
haridustest[str_detect(episode2,"E[Üü][Ss]"),episode_level:="ülik"]
haridustest[str_detect(episode2,"Aleks"),episode_level:="aleksk"]
haridustest[str_detect(episode2,"[Õö]S"),episode_level:="õp-sem"]
haridustest[str_detect(episode2,"ped.kurs"),episode_level:="ped.kurs"]
haridustest[str_detect(episode2," g "),episode_level:="gümn"]
haridustest[str_detect(episode2," g$"),episode_level:="gümn"]
haridustest[str_detect(episode2,"\\bg "),episode_level:="gümn"]
haridustest[str_detect(episode2,"^g$"),episode_level:="gümn"]
haridustest[str_detect(episode2,"TG"),episode_level:="gümn"]
haridustest[str_detect(episode2,"kõrg.tütarlastek"),episode_level:="gümn"]
haridustest[str_detect(episode2," ü "),episode_level:="ülik"]
haridustest[str_detect(episode2,"dr"),episode_level:="ülik"]
haridustest[str_detect(episode2,"med"),episode_level:="ülik"]
haridustest[str_detect(episode2,"philol"),episode_level:="ülik"]
haridustest[str_detect(episode2,"phil"),episode_level:="ülik"]
haridustest[str_detect(episode2,"cand"),episode_level:="ülik"]
haridustest[str_detect(episode2,"algk.õp"),episode_level:="algk.õp"]
haridustest[str_detect(episode2,"vallak.õp"),episode_level:="vallak.õp"]
haridustest[str_detect(episode2,"kreisk.õp"),episode_level:="kreis.õp"]
haridustest[str_detect(episode2,"kooliõp"),episode_level:="kooliõp"]
haridustest[str_detect(episode2,"[Kk]oduõp"),episode_level:="koduõp"]
haridustest[str_detect(episode2,"Pb õp.inst"),episode_level:="õp.inst"]
haridustest[str_detect(episode2,"^k.õp"),episode_level:="k.õp"]
haridustest[str_detect(episode2,"õp eksam"),episode_level:="k.õp"]
haridustest[str_detect(episode2,"^g-õp"),episode_level:="g.õp"]

haridustest[,unique(episode2)]


haridustest[,.N,by=episode_level]
haridustest[is.na(episode_level),unique(episode2)]

#ok, got almost all of them...

#haridustest[is.na(endyear),beginyearnext:=lead(beginyear,1),by=.(ISIK.id,Nime.normitud.kuju)]#,endyear:=beginyearnext]
haridustest[is.na(endyear),endyear:=beginyear]

#no longer adding beginnextyear based on prev, because it is not accurate for education.
#possibly this kind of interpolation can be done for plots, but ruins counting of years in edu.


t88test3 <- t88test3[order(ISIK.id,beginyear)][,beginyearnext:=lead(beginyear,1),by=.(ISIK.id,Nime.normitud.kuju)]
t88test3[is.na(endyear),endyear:=beginyearnext]
t88test3 <- rbind(t88test3,haridustest,fill=T)
t88test3 <- t88test3[order(ISIK.id,beginyear)]

#t88test3 <- t88test3[!is.na(beginyear)]




t88test3[str_detect(episode2,"Treffn"),listing:="Treffn"]
t88test3[str_detect(episode2,"Tü"),listing:="TÜ"]
t88test3[str_detect(episode2,"TÜ"),listing:="TÜ"]
t88test3[str_detect(episode2,"EÜS"),listing:="EÜS"]
t88test3[str_detect(episode2,"Sakala"),listing:="Sakala"]
t88test3[str_detect(episode2,"Virulase"),listing:="Virulane"]
t88test3[str_detect(episode2,"Riigikoh"),listing:="Riigikohus"]
t88test3[str_detect(episode2,"Vanemui"),listing:="Vanemuine"]
t88test3[str_detect(episode2,"Vaba"),listing:="Vaba"]


t88test3[str_detect(episode2,"Trt"),place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Tall "),place:="Tallinn"]
t88test3[str_detect(episode2,"Tall-"),place:="Tallinn"]
t88test3[str_detect(episode2,"Saarem"),place:="Saaremaa"]
t88test3[str_detect(episode2,"Kuress"),place:="Kuressaare%20linn"]
t88test3[str_detect(episode2,"Rakv"),place:="Rakvere%20linn"]
t88test3[str_detect(episode2,"Paistu"),place:="Paistu"]
t88test3[str_detect(episode2,"Pärnu"),place:="Pärnu%20linn"]
t88test3[str_detect(episode2,"Pb"),place:="Peterburi"]
t88test3[str_detect(episode2,"Hels"),place:="Helsingi"]
t88test3[str_detect(episode2,"Treffn"),place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Tü"),place:="Tartu%20linn"]
t88test3[str_detect(episode2,"TÜ"),place:="Tartu%20linn"]
t88test3[str_detect(episode2,"EÜS"),place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Vilj"),place:="Viljandi%20linn"]
t88test3[str_detect(episode2,"Võru"),place:="Võru%20linn"]
t88test3[str_detect(episode2,"Põltsamaa"),place:="Põltsamaa%20linn"]
t88test3[str_detect(episode2,"Antsla"),place:="Antsla"]
t88test3[str_detect(episode2,"Valga"),place:="Valga"]
t88test3[str_detect(episode2,"Audru"),place:="Audru"]
t88test3[str_detect(episode2,"Sm-"),place:="Soome"]
t88test3[str_detect(episode2,"Venem"),place:="Venemaa"]
t88test3[str_detect(episode2,"Vn"),place:="Venemaa"]
t88test3[str_detect(episode2,"Haaps"),place:="Haapsalu%20linn"]
t88test3[str_detect(episode2,"Järvam"),place:="Järvamaa"]
t88test3[str_detect(episode2,"Sakala"),place:="Viljandi%20linn"]
t88test3[str_detect(episode2,"Virulase"),place:="Tallinn"]
t88test3[str_detect(episode2,"TallTeat"),place:="Tallinn"]
t88test3[str_detect(episode2,"Pealinna Teataja"),place:="Tallinn"]
t88test3[str_detect(listing,"Oma")&beginyear<1900,place:="Tartu%20linn"]
t88test3[str_detect(listing,"Oma")&beginyear>1900,place:="Tallinn"]
t88test3[str_detect(episode2,"Pm")&beginyear<1998,place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Olevik"),place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Riigikoh")&beginyear<1935,place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Riigikoh")&beginyear>1935,place:="Tallinna%20linn"]
t88test3[str_detect(episode2,"Vanemui"),place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Vaba")&beginyear>1912&beginyear<1918,place:="Tartu%20linn"]
t88test3[str_detect(episode2,"Oma Maa")&beginyear>1883&beginyear<1891,place:="Tartu%20linn"]


t88test3[episode_type=="töö"&is.na(place),unique(episode2)]

str_extract("Põlma k-k","[A-Z][a-zõäöü]+")
#takes the first capitalized word... [A-Z][^ ]
t88test3[is.na(place),place:=str_extract(episode2,"[A-Z][a-zõäöü-]{3,}+")]
t88test3[is.na(listing),listing:=str_extract(episode2,"[A-Z][a-zõäöü-]{3,}+")]

#t88test[is.na(place)]


#t88test4 <- t88test3[order(beginyear),.SD,by=ISIK.id]

t88test3[str_detect(episode," ja ")&str_detect(episode,"[0-9]{4}.*[0-9]{4}")]

#the ones that contain a "ja" and two numbers, not too many, will leave for now
t88test3[str_detect(episode," ja ")&str_detect(episode,"[0-9]{4}.*[^-]+.*[0-9]{4}")]


#fwrite(t88test3,"data/raw/t88test3.csv")

#t88test3 <- fread("data/raw/t88test3_manually_fixed.csv")



places <- t88test3[!is.na(place),unlist(place)]
places <- str_replace_all(places,"s$","")
places <- str_replace_all(places,"l$","")
places <- places[!duplicated(places)] #only the ones that appear at least twice
#places <- str_replace_all(places,"s$","")
#places <- str_replace_all(places,"l$","")
#places <- unique(places)
places <- places[!is.na(places)]


#MAKING OF PLACESTABLE2

#placestable <- rbindlist(Map(`cbind`,lapply(places,find_coords),places=places))
#placestable <- placestable[!is.na(places)]
#placestable <- placestable[!is.na(koordinaadid)]
#placestable2 <- placestable[,.SD[1:5],by=places]
#placestable2[str_detect(koht,"linn"),linn:=T]
#placestable2[str_detect(koht,"Tallinn"),linn:=F]
#placestable2[str_detect(koht," k$"),kyla:=T]
#placestable2 <- placestable2[order(places,linn,kyla)]


#fwrite(placestable,"data/interim/placestable_alsouniques_5k_rows.tsv",sep="\t")
#fwrite(placestable2,"data/interim/placestable_alsouniques_top5_5k_rows.tsv",sep="\t")
#places <- fread("places.tsv",sep="\t")
#placestable2 <- fread("placestable2.tsv",sep="\t")
placestable2 <- fread("data/interim/placestable_alsouniques_top5_5k_rows.tsv",sep="\t")
#placestable2 <- fread("data/interim/placestable_top5_5k_rows.tsv",sep="\t")

placestable2[str_detect(koht,"linn"),linn:=T]
placestable2[str_detect(koht,"Tallinn"),linn:=F]
placestable2 <- placestable2[order(places,linn)]

placestable3 <- placestable2[,.SD[1],by=places]


#str_match("koordinaadid: 58°22’48_N : 26°43’21_E","[0-9][^ ]{8}N")
#str_split_fixed(placestable2$koordinaadid,":",3)

placestable3[,lat:=str_match(koordinaadid,"[0-9][^ ]{9}N")]
placestable3[,lon:=str_match(koordinaadid,"[0-9][^ ]{9}E")]

#these functions do the job
source("code/1_datasets_build/5_build_GIS/0_functions.R")
placestable3[,lat2:=-dms2dec(lat),by=places]
placestable3[,lon2:=-dms2dec(lon),by=places]

#merged the names
t88test3[,place:=na.locf(place,na.rm=F),by=ISIK.id]
t88test3[,place:=na.locf(place,na.rm=F,fromLast=T),by=ISIK.id]
merged_coords_ISIK_all <- merge(placestable3,t88test3,by.x="places",by.y="place",all.y=T)[order(ISIK.id,beginyear)]
#merged_coords[,place:=na.locf(place,na.rm=F),by=ISIK.id]
#merged_coords[,place:=na.locf(place,na.rm=F,fromLast=T),by=ISIK.id]
merged_coords_ISIK_all[,lat2:=na.locf(lat2,na.rm=F),by=ISIK.id]
merged_coords_ISIK_all[,lat2:=na.locf(lat2,na.rm=F,fromLast=T),by=ISIK.id]
merged_coords_ISIK_all[,lon2:=na.locf(lon2,na.rm=F),by=ISIK.id]
merged_coords_ISIK_all[,lon2:=na.locf(lon2,na.rm=F,fromLast=T),by=ISIK.id]
merged_coords_ISIK_all <- merged_coords_ISIK_all[beginyear<2000&beginyear>1700]

merged_coords_ISIK_all[is.na(endyear),endyear:=beginyear]
merged_coords_ISIK_all[episode_type=="surm",endyear:=NA]

#fwrite(merged_coords_ISIK_all,here("data/interim/merged_coord_ISIK_all.tsv"),sep=",")
fwrite(merged_coords_ISIK_all,here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_biographies_detailed.tsv"),sep="\t")

#haridustest[beginyear<1700]
#haridustest[endyear<1700]
#t88test3[beginyear<1700]
#t88test3[endyear<1700]