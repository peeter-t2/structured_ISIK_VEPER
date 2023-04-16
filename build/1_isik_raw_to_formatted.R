library(data.table)
library(tidyverse)
library(zoo)
library(here)

#bioraw <- fread(here("data/raw//autorid/ISIK_VEPER_long_raw_v7_all.tsv"),sep="\t",header=F)
bioraw <- fread(here("data/raw/autorid/ISIK_VEPER_long_raw_v7_all_fromzip.tsv"),sep="\t",header=F)

#same data, different order.
#identical(bioraw[order(V3)],bioraw2[order(V3)])

bioraw[,exists:=1]
bioraw[str_detect(V1,"[0-9]"),separator:=V1]
bioraw[str_detect(V1,paste0(c("Nimi", "Nime normitud kuju", "Eludaatumid", "Sünniaeg","Sünnikoht",  "Surmaaeg", "Surmakoht", "Üldandmed",  "Sugulus","Seotud andmed"),collapse="|")),separator:=V1]

bioraw[,separator:=na.locf(separator)]
bioraw[,num_head:=str_extract(separator,"[0-9]")]
bioraw[,clean_head:=str_replace(separator," \\([0-9]+\\)","")]
bioraw2 <- bioraw[V1!=V2]


#these are the new separators...
bioraw2[,seq:=cumsum(exists),by=.(V3,separator)]
bioraw2[str_detect(separator,"[0-9]"),separator2:=ifelse(V1==V1[1],V1,no=NA),by=.(V3,separator)]
bioraw2[!is.na(separator2),exists_separator2:=1]
bioraw2[!is.na(separator2),sep_id:=cumsum(exists_separator2),by=.(V3,separator)]
bioraw2[,sep_id:=na.locf(sep_id),by=.(V3,separator)]


#ok, do this for all clean head with numbers, and aggregate back together,
#change names according to this...
#first name order



#test2[,.(V3,sep_id,Eriala,Allikas)]
#event per line seems better than the whole thing
test2 <- bioraw2[clean_head=="Haridus"] %>% 
  dcast(V3+clean_head+sep_id~V1,value.var="V2",fun.aggregate=function(x) paste(x, collapse = "~"))
#all_seps ids
bioraw[str_detect(V1,"[0-9]"),unique(str_replace(V1," \\([0-9]+\\)",""))]
test2 <- bioraw2[str_detect(clean_head,"Üldandmed")] %>% 
  dcast(V3+clean_head+sep_id~V1,value.var="V2",fun.aggregate=function(x) paste(x, collapse = "~"))
names(test2)

see<-cast_bio[str_detect(clean_head,"Üldandmed"),.(V3,clean_head,sep_id,`Biograafilised andmed`,`Poliitiline tegevus`,`Ühiskondlik tegevus`,`Vangistused ja repressioonid`,Tegevusala,Aukraadid)]
#eludaatumid?
#üldandmed... peaaegu...,
#üldandmete separate query.,
#andmed ja infohulk.
#andmed ja infohulk lingitud nimede puhul.. järjestiku kaudu saab.


cast_bio <- bioraw2 %>% #[clean_head=="Haridus"] %>% 
  dcast(V3+clean_head+sep_id~V1,value.var="V2",fun.aggregate=function(x) paste(x, collapse = "~"))
#colnames(urc_training_norm) <- c(paste0("var", 1:8), "label")



#just these.
paste0(c("Nimi", "Nime normitud kuju", "Eludaatumid", "Sünniaeg","Sünnikoht",  "Surmaaeg", "Surmakoht", "Üldandmed",  "Sugulus","Seotud andmed"))
# they don't work so good just yet....

#Üldandmetel on ametid ja biograafilised andmed



  
  cast_bio[str_detect(clean_head,"Nime normi"),.(V3,clean_head,sep_id,`Nime normitud kuju`,Allikas,Allikad)]
  cast_bio[clean_head=="Nimi",.(V3,clean_head,sep_id,Allikad)]
  cast_bio[clean_head=="Üldandmed",.(V3,clean_head,sep_id,`Biograafilised andmed`,Tegevusala)]
  
  cast_bio[clean_head=="Haridus",.(V3,clean_head,sep_id,Kool,`Õpingute algus`,`Õpingute lõpp`,Kraad,Eriala,Teaduskond,Märkus,Allikas)]
  cast_bio[clean_head=="Haridus",.(V3,clean_head,sep_id,Kool,`Õpingute algus`,`Õpingute lõpp`,Kraad,Eriala,Teaduskond,Märkus,Allikas)]
  
  
  #maybe process üldandmed separately
  cast_bio[clean_head=="Üldandmed",.(V3,clean_head,sep_id,`Biograafilised andmed`,Tegevusala)] %>% 
    set_names(paste0("var", 1:5))#, 
  
  cast_bio[,unique(clean_head)]
  names(cast_bio)
  
all_seps <- rbind(cast_bio[str_detect(clean_head,"Nime normi"),.(V3,clean_head,sep_id,`Nime normitud kuju`,Allikas,Allikad)] %>% 
                    set_names(paste0("var", 1:6)),     
   
  cast_bio[clean_head=="Haridus",.(V3,clean_head,sep_id,Kool,`Õpingute algus`,`Õpingute lõpp`,Kraad,Eriala,Teaduskond,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:11)),
cast_bio[clean_head=="Kõrgharidus",.(V3,clean_head,sep_id,Kõrgkool,Sisseastumine,Lõpetamine,Eriala,Teaduskond,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:10)),
cast_bio[clean_head=="Elukohad",.(V3,clean_head,sep_id,Elukoht,Algus,Lõpp,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:8)),
cast_bio[clean_head=="Harrastused",.(V3,clean_head,sep_id,Ala,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:6)),
cast_bio[clean_head=="Tunnustused",.(V3,clean_head,sep_id,Tunnustus,Aeg,`Mille eest`,Allikas,Märkus)] %>% 
  set_names(paste0("var", 1:8)),
cast_bio[str_detect(clean_head,"kohta"),.(V3,clean_head,sep_id,`Kirje(-d)`,Link,Märkus)] %>% 
  set_names(paste0("var", 1:6)),
cast_bio[str_detect(clean_head,"Eestist"),.(V3,clean_head,sep_id,Riik,Aeg,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:7)),
cast_bio[str_detect(clean_head,"Ühiskondlik"),.(V3,clean_head,sep_id,Amet,Algus,Lõpp,Organisatsioon,`Liikmeks astumine`,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:10)),
cast_bio[clean_head=="Teoseid",.(V3,clean_head,sep_id,`Kirje(-d)`,Link,Retsensioon,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:8)),
cast_bio[clean_head=="Elukutsed",.(V3,clean_head,sep_id,Eriala,Algus,Lõpp,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:8)),
cast_bio[clean_head=="Nimekujud"|clean_head=="Nime erikujud",.(V3,clean_head,sep_id,`Nime erikuju`,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:6)),
cast_bio[str_detect(clean_head,"Teaduslik"),.(V3,clean_head,sep_id,`Teaduslik kraad`,Kaitsmisaeg,Kaitsmiskoht,`Teadustöö pealkiri`,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:9)),
cast_bio[clean_head=="Pseudonüümid",.(V3,clean_head,sep_id,Pseudonüüm,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:6)),
cast_bio[clean_head=="Väljaanded",.(V3,clean_head,sep_id,Väljaanne,Märkus)] %>% 
  set_names(paste0("var", 1:5)),
cast_bio[str_detect(clean_head,"erikujud"),.(V3,clean_head,sep_id,`Nime erikuju`,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:6)),
cast_bio[clean_head=="Ametid",.(V3,clean_head,sep_id,Amet,Algus,Lõpp,Asukoht,Asutus,Märkus,Allikas)] %>% 
  set_names(paste0("var", 1:10)),
cast_bio[clean_head=="Perekond",.(V3,clean_head,sep_id,Isik)] %>% 
  set_names(paste0("var", 1:4)),
cast_bio[clean_head=="Publikatsioonid",.(V3,clean_head,sep_id,Publikatsioon,Link,Märkus)] %>% 
  set_names(paste0("var", 1:6)),fill=T)


#sünnikoht, sünniaeg, sünniaeg (vkj)



#set as boundary, kool

see <- bioraw2[,.(V1,V2,V3,clean_head)] %>% 
  dcast(V3~clean_head,value.var="V2",fun.aggregate=function(x) paste(x, collapse = "~"))
fwrite(see,here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_VEPER_metasimple.tsv"),sep="\t")



#################          

# To link isik with ENB

stored_names<- see[,.(file=V3,set=str_extract(V3,"ISIK|VEPER"),id=str_extract(V3,"[0-9]+"),`Nime normitud kuju`,Sünniaeg,Surmaaeg)]
#fwrite(stored_names,"isik_veper_ids.tsv",sep="\t")


combined_matcher = rbind(author_list[,enbset:=T],stored_names,fill=T)
combined_matcher[is.na(name),both:=paste0(`Nime normitud kuju`)]
combined_matcher[is.na(`Nime normitud kuju`),both:=paste0(name)]
combined_matcher <- merge(combined_matcher,unique(authors_enriched[,.(viaf,coauthor)]),by="coauthor",all.x=T)

combined_matcher = combined_matcher[order(tolower(both))]

formanualcheck = combined_matcher[,.(enbset,set,id,both,name,`Nime normitud kuju`,Sünniaeg,Surmaaeg,date,coauthor,viaf)]

#to check the links with ENB manually.
#fwrite(formanualcheck,"isik_manual_linking2.csv")
#manual linked and saved here:
#autorid_ids4  <- fread(here("data/raw/linking_data/isik_manual_linking4.csv"))




#can later check if linking all the previous list

see2 <- bioraw2[,.(V1,V2,V3,clean_head)] %>%
  group_by(V3,clean_head) %>% 
  nest()
save3 <- see2 %>% 
  spread(clean_head,data)


names(save3)

see_n <- save3%>% 
  filter(!map_lgl(Haridus, is.null)) %>% 
  unnest() %>% 
  right_join(select(save3, -Haridus))

test <- save3 %>% 
  unnest(Haridus,.drop = F)
df <- tibble(
  x = 1:2,
  y = list(
    list("a", "b"),
    list("c", "d")
  )
)

df %>% unnest(y)

bioraw[V1==V2,section:=V2]
bioraw[V1=="Nime normitud kuju",nimi:=V2]

dput(bioraw[,unique(V1)])


#"Biograafilised andmed" - see on konvertimiseks

#one row per life even

#. by topic, 
#; by event,
# first need to classify the topics.
#sequence is stable, but missing some of them, 
test1 <- cSplit(table, "Biograafilised andmed", "\\. ")

#ignore ISIK/index.php_id_7050 and ISIK/index.php_id_7052 as they do not follow the same structure
see<-cast_bio[str_detect(clean_head,"Üldandmed")&!V3%in%c("ISIK/index.php_id_7050","ISIK/index.php_id_7052"),.(V3,clean_head,sep_id,`Biograafilised andmed`,`Poliitiline tegevus`,`Ühiskondlik tegevus`,`Vangistused ja repressioonid`,Tegevusala,Aukraadid)][,str_split_fixed(`Biograafilised andmed`, "(?<=(?<!((~| )[1-9]|(~| )[A-Z]))\\. )", n=30)] %>%
  data.table() %>% 
  cbind(cast_bio[str_detect(clean_head,"Üldandmed")&!V3%in%c("ISIK/index.php_id_7050","ISIK/index.php_id_7052"),.(id=V3)]) %>% 
  filter(V1!=""&V2!=""&V3!="") %>%
  data.table()



seebirths_simple <- see[,str_split_fixed(V2,"(?<=([0-9]{4}[\\) ]))",2)] %>% 
  data.table()%>% 
  cbind(see[,.(file=id)]) %>% 
 # filter(V1!=""&V2!=""&V3!="") %>%
  data.table()

seebirths_simple = cbind(seebirths_simple,seebirths_simple[,.(set=str_extract(file,"ISIK|VEPER"),id=as.numeric(str_extract(file,"[0-9]+")))])
birthplaces  <- cast_bio[str_detect(clean_head,"Sünnikoht"),.(V3,clean_head,sep_id,Sünnikoht)]
seebirths_simple2 = merge(seebirths_simple,birthplaces,by.x="file",by.y="V3",all=T)

seebirths_names <- cast_bio[str_detect(clean_head,"Nime normitud kuju")&!V3%in%c("ISIK/index.php_id_7050","ISIK/index.php_id_7052"),.(V3,`Nime normitud kuju`)]

seebirths_simple2 = merge(seebirths_simple2,seebirths_names,by.x="file",by.y="V3",all=T)
seebirths_simple2[,set:=str_extract(file,"ISIK|VEPER")][,id:=as.numeric(str_extract(file,"[0-9]+"))]
tofile_births <- seebirths_simple2[,.(set,id,birthplace1=Sünnikoht,birthplace2=V2,birthplace3=V1,name=`Nime normitud kuju`)]
fwrite(tofile_births,here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_VEPER_births.tsv"),sep="\t")

#see[!duplicated(viaf)]
#should add one internal person-identifier.
#that lumps together both the ones with the same viaf, as the ones with the same isik/veper.
#still name per line, but consolidate the known ids, and give a new unique id...



library(data.table)

manualchecked_all = fread(here("data/raw/linking_data/isik_manual_linking4.csv"))
#got 1000 linked names from authorlist now..., so probably 1000 more ahead
manualchecked_all[,all_links:=paste0(set,id," ",V5,V6," ",V7,V8)]
manualchecked_all[,all_links:=str_remove_all(all_links," NA")]
#checkthese <- manualchecked_all[,.(isik_id=str_remove_all(unlist(str_split(all_links," ")),"NA")),.(enbset,V2,both,name,`Nime normitud kuju`,Sünniaeg,Surmaaeg,date,coauthor,viaf,all_links)]
#checkthis <- manualchecked_all[viaf!="",all_links2:=trimws(paste(unique(str_remove_all(unlist(str_split(all_links," ")),"NA")),collapse=" ")),by=viaf]

manualchecked_all[viaf!="",all_links2:=trimws(paste(unique(str_remove_all(unlist(str_split(all_links," ")),"NA")),collapse=" ")),by=viaf]
manualchecked_all[,rowid:=.I]
manualchecked_all[viaf=="",all_links2:=trimws(paste(unique(str_remove_all(unlist(str_split(all_links," ")),"NA")),collapse=" ")),by=rowid]

checkthese <- manualchecked_all[,.(isik_id=str_remove_all(unlist(str_split(all_links2," ")),"NA")),.(enbset,V2,both,name,`Nime normitud kuju`,Sünniaeg,Surmaaeg,date,coauthor,viaf,all_links,rowid)]

checkthese[isik_id!="",all_viafs:=trimws(paste(unique(str_remove_all(unlist(str_split(viaf," ")),"NA")),collapse=" ")),by=isik_id]

#all groups with any matching isik_ids, viafs, or others
#all the overlaps
#all the links here
overlaps <- unique(merge(checkthese[,.(isik_id,rowid)],checkthese[,.(isik_id,rowid)],by="rowid")[isik_id.x!=isik_id.y][,.(isik_id.x,isik_id.y)])
#all the links added
checkthese_m <- merge(checkthese,overlaps,by.x="isik_id",by.y="isik_id.x",all.x=T)
checkthese_m[,all_isiks:=paste(unique(sort(c(isik_id,isik_id.y))),collapse= " "),by=rowid]

checkthese_m[str_detect(all_isiks," ")]

see <- checkthese_m[,.N,by=.(viaf,all_isiks)]
see[duplicated(viaf)&viaf!=""]
#no duplicates == successfully got all viaf information to isiks and vice versa, but also - viafs were complete with that anyway.

checkthese_m[,localid:=.GRP,by=.(all_viafs,all_isiks)]
#if there is no viaf and no isik, then there is nothing to work with really...


#11320 people linked
unique(checkthese_m[,.(all_isiks,all_viafs,localid)])

#21507 entries linked
links_allcoauthors <- unique(checkthese_m[,.(coauthor,all_isiks,all_viafs,localid)])
#merge this with corpus.
links_allcoauthors[,n:=.N,all_isiks]


#for person informatoin analysis, stick with identifiable people with coordinates. only that 11k will suffice...


#savefile - alllinks for relevant authors
#coauthor, all_isiks, all_viafs, all_errr? but actually not needed..., do give localid though, as comb from viaf and all_isiks
savefile <- links_allcoauthors[n==1|coauthor!=""][all_isiks!=""][coauthor!=""|all_viafs!=""][,.(coauthor,all_isiks,all_viafs,localid)]

# 2263 name instances have a link here.
fwrite(savefile,here("data/publish/tidy_ISIK_VEPER/data/manual/links_allcoauthors.csv"),sep="\t")



#ISIK1438
sort(c("b","a"))
#debugging
#checkthese[10001,all_viafs]
#checkthese[10001,isik_id]
#works! revealed one mistake and no overlaps. mistake fixed now.
checkthese[str_detect(all_viafs," ")]




#checkthese[is.na(isik_id),all_viafs:=viaf]
#VEPER1429



#name, isik, viaf --- if any of those matches, then make it the same..




# töötab küll, eg http://viaf.org/viaf/266694205 was unlinked.
#not sure if this is in fact better quality though...
checkthis2 <- checkthis[str_detect(viaf,"viaf"),.N,by=viaf][order(-N)]
                  
manualchecked_all[enbset==T&set!=""]
str(manualchecked_all)
#got 586 matches here w placeinfo
#seebirths = merge(seebirths_simple2,manualchecked_all[enbset==T&set!=""][!is.na(coauthor)&coauthor!="",.(id,set,coauthor)],by=c("id","set"),all.x=T)
#

seebirths = seebirths_simple2


#seebirths[str_detect(Sünnikoht,"[A-Z][a-zõäöü]{1,2} ")]

seebirths[,maakond:=str_extract(V2,"[A-Z][a-zõäöü]{1,2} ")]
seebirths[is.na(maakond),maakond:=trimws(str_extract(V2,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +mk[ \\.]"))]
seebirths[,kihelkond:=str_extract(V2,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +khk[ \\.]")]
seebirths[,vald:=str_extract(V2,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +v[ \\.]")]
seebirths[,vald:=str_replace(vald," +v[ \\.]"," vald")]
seebirths[,vald2:=vald]
seebirths[,küla:=str_extract(V2,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +k[ \\.]")]
seebirths[str_detect(V2," +ms?[ \\.]"),küla:=str_extract(V2,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +ms?[ \\.]")]
seebirths[,küla:=str_replace(küla," +k[ \\.]"," küla")]
seebirths[,küla2:=str_remove_all(küla," +ms?\\.?$| +k\\.?$$| küla")]

seebirths[is.na(maakond),maakond:=str_extract(Sünnikoht,"[A-Z][a-zõäöü]{1,2} ")]
seebirths[is.na(maakond),maakond:=trimws(str_extract(Sünnikoht,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +mk[ \\.]?"))]
seebirths[is.na(kihelkond),kihelkond:=str_extract(Sünnikoht,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +khk[ \\.]?")]
seebirths[is.na(vald),vald:=str_extract(Sünnikoht,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +v[ \\.]?")]
seebirths[,vald:=str_replace(vald," +v[ \\.]"," vald")]
seebirths[is.na(vald2),vald2:=vald]
seebirths[is.na(küla),küla:=str_extract(Sünnikoht,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +k[ \\.]?")]
seebirths[str_detect(Sünnikoht," +ms?[ \\.]"),küla:=str_extract(Sünnikoht,"[a-zõäöüA-ZÕÄÖÜ\\-]+ +ms?[ \\.]?")]
seebirths[,küla:=str_replace(küla," +k[ \\.]"," küla")]
seebirths[,küla2:=str_remove_all(küla," +ms?\\.?$| +k\\.?$$| küla")]
seebirths[,vald2:=str_remove(vald2," vald")]
seebirths[,vald2:=str_remove(vald2," v$")]



# linn is not very helpful in fact, does not work very well., wont' use
seebirths[str_count(V2," ")==1,linn:=str_remove(V2,"[A-Z][a-zõäöü]{1,2} ")]
seebirths[str_count(Sünnikoht," ")==1&is.na(linn),linn:=str_remove(Sünnikoht,"[A-Z][a-zõäöü]{1,2} ")]
seebirths[str_count(V2," ")==0&is.na(linn),linn:=V2]
seebirths[str_count(Sünnikoht," ")==0&is.na(linn),linn:=Sünnikoht]
seebirths[,linn:=str_remove(linn,",.+")]

check <-seebirths[!is.na(linn)]

#linnad saab eraldi heuristikaga...
#ja siis otsida kohanime maakonna lähedusest, cross-validate seal???
#5231 sünnikohta veel.
#aga ilma geoloc-ta
birthplaces  <- cast_bio[str_detect(clean_head,"Sünnikoht"),.(V3,clean_head,sep_id,Sünnikoht)]



#linnad eraldi sealt...




nrow(seebirths[!is.na(vald)])#877 valda, 1934 valda nüüd
seebirths[,V2:=trimws(V2)]
seebirths[str_detect(V2,"^Rakv")]
seebirths[,V2:=str_remove_all(V2,"\\-s|\\-l")]
seebirths[,V2:=str_remove_all(V2,"s\\.|l\\.")]
seebirths[str_detect(V2,"^Trt"),küla:="Tartu"]
seebirths[str_detect(V2,"^Tal"),küla:="Tallinn"]
seebirths[str_detect(V2,"^Rakv"),küla:="Rakvere"]
seebirths[str_detect(V2,"^Riia"),küla:="Riga"]
seebirths[str_detect(V2,"^Kures[s$]"),küla:="Kuressaare"]
seebirths[str_detect(V2,"^Haap[$]"),küla:="Haapsalu"]
seebirths[str_detect(V2,"^Saarem"),maakond:="Saaremaa"]
seebirths[str_detect(V2,"^Vilj"),küla:="Viljandi"]
seebirths[str_detect(V2,"^Kiviõli"),küla:="Kiviõli"]
seebirths[str_detect(V2,"^Riia"),küla:="Riia"]
seebirths[str_detect(V2,"^Mõisaküla"),küla:="Mõisaküla"]
seebirths[str_detect(V2,"^Pärnu"),küla:="Pärnu"]
seebirths[str_detect(V2,"^Pb"),küla:="Sankt-Peterburg"]
seebirths[str_detect(V2,"^Valga"),küla:="Valga"]
seebirths[str_detect(V2,"^Võru"),küla:="Võru"]
seebirths[!str_detect(trimws(V2)," "),küla:=trimws(V2)]
seebirths[str_detect(Sünnikoht,"^Trt"),küla:="Tartu"]
seebirths[str_detect(Sünnikoht,"^Tal"),küla:="Tallinn"]
seebirths[str_detect(Sünnikoht,"^Rakv"),küla:="Rakvere"]
seebirths[str_detect(Sünnikoht,"^Kures[s$]"),küla:="Kuressaare"]
seebirths[str_detect(Sünnikoht,"^Haap[$]"),küla:="Haapsalu"]
seebirths[str_detect(Sünnikoht,"^Saarem"),maakond:="Saaremaa"]
seebirths[str_detect(Sünnikoht,"^Vilj"),küla:="Viljandi"]
seebirths[str_detect(Sünnikoht,"^Kiviõli"),küla:="Kiviõli"]
seebirths[str_detect(Sünnikoht,"^Riia"),küla:="Riia"]
seebirths[str_detect(Sünnikoht,"^Mõisaküla"),küla:="Mõisaküla"]
seebirths[str_detect(Sünnikoht,"^Pärnu"),küla:="Pärnu"]
seebirths[str_detect(Sünnikoht,"^Pb"),küla:="Sankt-Peterburg"]
seebirths[str_detect(Sünnikoht,"^Valga"),küla:="Valga"]
seebirths[str_detect(Sünnikoht,"^Võru"),küla:="Võru"]
seebirths[is.na(küla)&!str_detect(trimws(Sünnikoht)," "),küla:=trimws(Sünnikoht)]
seebirths[str_detect(küla,"^Pb"),küla:="Peterburg"]

seebirths[,birthyear:=as.numeric(str_extract(V1,"[0-9]{4}"))]

#1224 still missing
seemore <- seebirths[is.na(küla)&is.na(vald)&is.na(maakond)&is.na(kihelkond)]

seebirths[,maakond:=trimws(maakond)]

seebirths[,unique(maakond)]

seebirths[maakond=="Jä",maakond:="Järvamaa"]
seebirths[maakond=="Ha",maakond:="Harjumaa"]
seebirths[maakond=="Lä",maakond:="Läänemaa"]
seebirths[maakond=="Pb",maakond:="Peterburg"]
seebirths[maakond=="Pä",maakond:="Pärnumaa"]
seebirths[maakond=="Ta",maakond:="Tartumaa"]
seebirths[maakond=="Va",maakond:="Valgamaa"]
seebirths[maakond=="Vir",maakond:="Virumaa"]
seebirths[maakond=="Vil",maakond:="Viljandimaa"]
seebirths[maakond=="Võ",maakond:="Võrumaa"]
seebirths[maakond=="Sa",maakond:="Saaremaa"]
seebirths[maakond=="Pe",maakond:="Petseri"]

seebirths[,alt_place:=str_remove(Sünnikoht,",.+")]

seebirths_m <- merge(seebirths,all_places_all[,.(V2,lat1=V5,lon1=V6)][,.SD[1],V2],by.x="küla",by.y="V2",all.x=T)
seebirths_m <- merge(seebirths_m,all_places_all[,.(V2,lat2=V5,lon2=V6)][,.SD[1],V2],by.x="vald",by.y="V2",all.x=T)
seebirths_m <- merge(seebirths_m,all_places_all[,.(V2,lat3=V5,lon3=V6)][,.SD[1],V2],by.x="küla2",by.y="V2",all.x=T)
seebirths_m <- merge(seebirths_m,all_places_all[,.(V2,lat4=V5,lon4=V6)][,.SD[1],V2],by.x="vald2",by.y="V2",all.x=T)
seebirths_m <- merge(seebirths_m,all_places_all[,.(V2,lat5=V5,lon5=V6)][,.SD[1],V2],by.x="kihelkond",by.y="V2",all.x=T)
seebirths_m <- merge(seebirths_m,all_places_all[,.(V2,lat6=V5,lon6=V6)][,.SD[1],V2],by.x="maakond",by.y="V2",all.x=T)
seebirths_m <- merge(seebirths_m,all_places_all[,.(V2,lat7=V5,lon7=V6)][,.SD[1],V2],by.x="alt_place",by.y="V2",all.x=T)
#maakond letters to placename.

seebirths_m[!is.na(lat7),level_used:=7][!is.na(lat6),level_used:=6][!is.na(lat5),level_used:=5][!is.na(lat4),level_used:=4][!is.na(lat3),level_used:=3][!is.na(lat2),level_used:=2][!is.na(lat1),level_used:=1]
seebirths_m[,lat:=lat1]
seebirths_m[,lon:=lon1]
seebirths_m[is.na(lat),lat:=lat2]
seebirths_m[is.na(lon),lon:=lon2]
seebirths_m[is.na(lat),lat:=lat3]
seebirths_m[is.na(lon),lon:=lon3]
seebirths_m[is.na(lat),lat:=lat4]
seebirths_m[is.na(lon),lon:=lon4]
seebirths_m[is.na(lat),lat:=lat5]
seebirths_m[is.na(lon),lon:=lon5]
seebirths_m[is.na(lat),lat:=lat6]
seebirths_m[is.na(lon),lon:=lon6]
seebirths_m[is.na(lat),lat:=lat7]
seebirths_m[is.na(lon),lon:=lon7]

seemore2 <- seebirths_m[is.na(lat1)&is.na(lat2)&is.na(lat3)&is.na(lat4)]#&is.na(lat5)]
seemore2 <- seebirths_m[is.na(lat1)&is.na(lat2)&is.na(lat3)&is.na(lat4)&is.na(lat5)]

tofile_births_gis <- seebirths_m[,.(set,id,birthplace1=Sünnikoht,birthplace2=V2,maakond,kihelkond,vald,küla,lat,lon,level_used,name=`Nime normitud kuju`)]
fwrite(tofile_births_gis,here("data/publish/tidy_ISIK_VEPER/data/elements/ISIK_VEPER_births_gis.tsv"),sep="\t")

#5600 names have some lat.
#now 6888 even, but some/many mismatches for sure.
seebirths_m[!is.na(lat)]

####
# stop here with ISIK_VEPER.
####
# 
# 
# #now all structured, up to 30 columns from each. basic values are less though.
# 
# #ideally:
# names(test2) <- c("Nimi2",	"Synd",	"Vanemad",	"Vennadoed",	"Abielus",	"Lapsed",	"Haridus",	"Töö",	"Surnud",	"Allikad")
# 
# 
# #now move some critical cases frame by frame
# test3[grep("Ae",test3[,21]),22:33] <- test3[grep("Ae",test3[,21]),21:32]
# test3[grep("Ae",test3[,21]),21] <- NA
# test3[grep("Ae",test3[,22]),23:33] <- test3[grep("Ae",test3[,22]),22:32]
# test3[grep("Ae",test3[,22]),22] <- NA
# #test3[,21]
# 
# #check for education options
# #length1 <- test3[grep("(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(Tü)|(cand)",test3[,24]),22:32]
# #length2 <- test3[grep("(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(Tü)|(cand)",test3[,25]),22:32]
# pattern  <- "(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(cand)|(õhtug)|(TTü)|(kk)|(algk)|(komm\\.g)|( g )|([Aa]lghar)|kihelkonnakool" #removed (Tü)
# test3[grep(pattern,test3[,21]),22:33] <- test3[grep(pattern,test3[,21]),21:32]
# test3[grep(pattern,test3[,21]),21] <- NA
# test3[grep(pattern,test3[,22]),23:33] <- test3[grep(pattern,test3[,22]),22:32]
# test3[grep(pattern,test3[,22]),22] <- NA
# test3[grep(pattern,test3[,23]),24:33] <- test3[grep(pattern,test3[,23]),23:32]
# test3[grep(pattern,test3[,23]),23] <- NA
# test3[grep(pattern,test3[,24]),25:33] <- test3[grep(pattern,test3[,24]),24:32]
# test3[grep(pattern,test3[,24]),24] <- NA
# 
# 
# 
# 
# #write.csv(table, "ISIK7000.csv")
# #write.table(table,"ISIK70002nd.csv",sep="\t",col.names=TRUE, quote=FALSE)
# #fwrite(table, "ISIK70003rd.csv")
# 
# #table <- read.csv("ISIK7000.csv")
# table <- read.csv("source_data/ISIK_basic_sorted.csv", header=TRUE, sep=",",stringsAsFactors = FALSE, fileEncoding="UTF-8", encoding="UTF-8")
# #grep("(?<!( [1-9]| [A-Z]))f", " 1fae Tfae", perl=T)
# 
# #grep("[^ ][^0-9A-Z]f", "19faeTfae")
# #grep(", (?=^[A-Z]| ^[0-9])",",  eest",perl=T)
# #grep("(^[[:upper:]]|^[ [[:digit:]]])\\.", c("a. asfga","asge"))
# library(stringr)
# #still not getting the right thing as it captures
# test2 <- data.frame(str_split_fixed(table$Biograafilised.andmed, "(?<=(?<!((~| )[1-9]|(~| )[A-Z]))\\. )", n=15)) #http://www.regular-expressions.info/lookaround.html # is perl = t by default?
# #test2 <- data.frame(str_split_fixed(table$Biograafilised.andmed, "(?<=[ [^A-Z0-9]]\\. )", n=15))
# names(test2) <- c("Nimi2",	"Synd",	"Vanemad",	"Vennadoed",	"Abielus",	"Lapsed",	"Haridus",	"Töö",	"Surnud",	"Allikad")
# 
# test3 <- cbind(table,test2)
# 
# #library(dplyr)
# #test3 %>%  mutate_all(as.character)
# test3 <- data.frame(lapply(test3, as.character),stringsAsFactors = FALSE)
# str(test3)
# 
# #test2 <- data.frame(test2[!duplicated(test2),])
# 
# #for lines with AE at 
# 
# #test3$Vennadoed <- as.character(test3$Vennadoed)
# length1 <- test3[grep("Ae",test3[,21]),21:32]
# length2 <- test3[grep("Ae",test3[,23]),22:32]
# test3[grep("Ae",test3[,21]),22:33] <- test3[grep("Ae",test3[,21]),21:32]
# test3[grep("Ae",test3[,21]),21] <- NA
# test3[grep("Ae",test3[,22]),23:33] <- test3[grep("Ae",test3[,22]),22:32]
# test3[grep("Ae",test3[,22]),22] <- NA
# #test3[,21]
# length1 <- test3[grep("(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(Tü)|(cand)",test3[,24]),22:32]
# length2 <- test3[grep("(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(Tü)|(cand)",test3[,25]),22:32]
# pattern  <- "(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(cand)|(õhtug)|(TTü)|(kk)|(algk)|(komm\\.g)|( g )|([Aa]lghar)|kihelkonnakool" #removed (Tü)
# 
# test3[grep(pattern,test3[,21]),22:33] <- test3[grep(pattern,test3[,21]),21:32]
# test3[grep(pattern,test3[,21]),21] <- NA
# test3[grep(pattern,test3[,22]),23:33] <- test3[grep(pattern,test3[,22]),22:32]
# test3[grep(pattern,test3[,22]),22] <- NA
# test3[grep(pattern,test3[,23]),24:33] <- test3[grep(pattern,test3[,23]),23:32]
# test3[grep(pattern,test3[,23]),23] <- NA
# test3[grep(pattern,test3[,24]),25:33] <- test3[grep(pattern,test3[,24]),24:32]
# test3[grep(pattern,test3[,24]),24] <- NA
# test3 <- data.table(test3)
# haridustest <- test3[,.(episode=unlist(strsplit(Haridus,", ", fixed=TRUE))), by= .(X,Nime.normitud.kuju)]
# 
# 
# haridustest[,years:=str_match(episode," [^ ]+[0-9][^ ]+")]
# haridustest[,episode2:= trimws(gsub(" [^ ]+[0-9][^ ]+","",episode))]
# sort(table(haridustest$episode2),decreasing = T)[1:100]
# 
# #should use intervals and first year and last year to get the actually matching numbers...
# #vead: Biogr.: Oma Maa I, Tartu, 1911, 76 (J. Lintrop).
# 
# #write.csv(test2, "ISIK7000conv.csv")
# #write.csv(test3, "ISIK7000_proc.csv")
# #write.csv(test3, "ISIK_basic_sorted_conv.csv")
# 
# 
# bstr_split_fixed(metaandmed3$Biograafilised.andmed, "(?<=(?<!((~| )[1-9]|(~| )[A-Z]))\\. )", n=15)
# 
# metaandmed3 <- read.csv("source_data/ISIK_basic_sorted.csv", header=TRUE, sep=",",stringsAsFactors = FALSE, fileEncoding="UTF-8", encoding="UTF-8") 
# library(stringr)
# test2 <- data.frame(str_split_fixed(metaandmed3$Biograafilised.andmed, "(?<=(?<!((~| )[1-9]|(~| )[A-Z]))\\. )", n=15))
# names(test2) <- c("Nimi2",	"Synd",	"Vanemad",	"Vennadoed",	"Abielus",	"Lapsed",	"Haridus",	"Töö",	"Surnud",	"Allikad")
# test3 <- cbind(table,test2)
# test3 <- data.frame(lapply(test3, as.character),stringsAsFactors = FALSE)
# test3[grep("Ae",test3[,21]),22:33] <- test3[grep("Ae",test3[,21]),21:32]
# test3[grep("Ae",test3[,21]),21] <- NA
# test3[grep("Ae",test3[,22]),23:33] <- test3[grep("Ae",test3[,22]),22:32]
# test3[grep("Ae",test3[,22]),22] <- NA
# pattern  <- "(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(cand)|(õhtug)|(TTü)|(kk)|(algk)|(komm\\.g)|( g )|([Aa]lghar)|kihelkonnakool" #removed (Tü)
# 
# test3[grep(pattern,test3[,21]),22:33] <- test3[grep(pattern,test3[,21]),21:32]
# test3[grep(pattern,test3[,21]),21] <- NA
# test3[grep(pattern,test3[,22]),23:33] <- test3[grep(pattern,test3[,22]),22:32]
# test3[grep(pattern,test3[,22]),22] <- NA
# test3[grep(pattern,test3[,23]),24:33] <- test3[grep(pattern,test3[,23]),23:32]
# test3[grep(pattern,test3[,23]),23] <- NA
# test3[grep(pattern,test3[,24]),25:33] <- test3[grep(pattern,test3[,24]),24:32]
# test3[grep(pattern,test3[,24]),24] <- NA
# test3 <- data.table(test3)
# 
# #write.csv(test3, "ISIK_basic_sorted_conv.csv")
