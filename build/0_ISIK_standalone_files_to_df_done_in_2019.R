#setwd("/media/kalmaar/ADATA SD700/ownCloud/Documents/R/R_corpus_analysis/aux datasets/ISIK")
filesisik1 <- paste("VEPER/",list.files("VEPER/"),sep="")#,collapse="")
filesisik2 <- paste("ISIK/",list.files("ISIK/"),sep="")
#filesisik <-  filesisik2
filesisik <- c(filesisik1,filesisik2)

library(stringr)
filesisik1<-unzip("ISIK.zip",list=TRUE)[,1]
filesisik1<-filesisik1[str_detect(filesisik1,"\\.php")]
filesisik2<-unzip("VEPER.zip",list=TRUE)[,1]
filesisik2<-filesisik2[str_detect(filesisik2,"\\.php")]
filesisik <- c(filesisik1,filesisik2)

#easycsv::fread_zip("xxxx\\alldata.zip", extension="CSV", sep=",")

#finally used 
#sed -i 's|<table cellpadding=3>.*</table>||' * 
# on the files



library(htmltab)
library(data.table)
#setDTthreads(threads=1)
#?getDTthreads
#should take 15 min total.
#i=50
#j=5007

#for (i in 50:100){
#tables <- data.table()
#start_time <- Sys.time()
#for (j in (1:length(filesisik))[(1:100)+i*100]){
  
library(xml2)


see <- data.table(filesisik)
#rm(filesisik,filesisik1,filesisik2)
#j=4715
#filesisik[j]
#j=5000
for (j in 1:nrow(see)){
  filename <- see[j,filesisik]
  connection <- unzip(paste0(str_extract(filename,"ISIK|VEPER"),".zip"),filename)
  testt <- data.table(htmltab(connection, which=3,header=0, encoding = "UTF-8",flatten=T))
  testt[,filename := filename]
  #testt <- data.table(testt)
  #testt[,filename:=filename]
  fwrite(testt,paste0("ISIK_VEPER_long_raw_v7_all_fromzip.tsv"),sep="\t",append=T,col.names=F)
  rm(testt)
  unlink(connection)
  #tables <- rbind(tables,testt)
}
#end_time <- Sys.time()
#end_time - start_time

#}