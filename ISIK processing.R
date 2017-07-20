library(data.table)

table <- read.csv("ISIK main.csv")

#grep("(?<!( [1-9]| [A-Z]))f", " 1fae Tfae", perl=T)
library(stringr)
test2 <- data.frame(str_split_fixed(table$Biograafilised.andmed, "(?<=(?<!((~| )[1-9]|(~| )[A-Z]))\\. )", n=15)) #http://www.regular-expressions.info/lookaround.html # is perl = t by default?
names(test2) <- c("Nimi2",	"Synd",	"Vanemad",	"Vennadoed",	"Abielus",	"Lapsed",	"Haridus",	"Töö",	"Surnud",	"Allikad")

test3 <- cbind(table,test2)

test3 <- data.frame(lapply(test3, as.character),stringsAsFactors = FALSE)


#length1 <- test3[grep("Ae",test3[,21]),21:32]
#length2 <- test3[grep("Ae",test3[,23]),22:32]
test3[grep("Ae",test3[,21]),22:33] <- test3[grep("Ae",test3[,21]),21:32]
test3[grep("Ae",test3[,21]),21] <- NA
test3[grep("Ae",test3[,22]),23:33] <- test3[grep("Ae",test3[,22]),22:32]
test3[grep("Ae",test3[,22]),22] <- NA
#test3[,21]

#length1 <- test3[grep("(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(Tü)|(cand)",test3[,24]),22:32]
#length2 <- test3[grep("(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(Tü)|(cand)",test3[,25]),22:32]
pattern  <- "(khkk)|(v\\-k)|(l\\-k)|(kurs)|(phil)|(kreisk)|(cand)|(õhtug)|(TTü)|(kk)|(algk)|(komm\\.g)|( g )|([Aa]lghar)|kihelkonnakool" #removed (Tü)
test3[grep(pattern,test3[,21]),22:33] <- test3[grep(pattern,test3[,21]),21:32]
test3[grep(pattern,test3[,21]),21] <- NA
test3[grep(pattern,test3[,22]),23:33] <- test3[grep(pattern,test3[,22]),22:32]
test3[grep(pattern,test3[,22]),22] <- NA
test3[grep(pattern,test3[,23]),24:33] <- test3[grep(pattern,test3[,23]),23:32]
test3[grep(pattern,test3[,23]),23] <- NA
test3[grep(pattern,test3[,24]),25:33] <- test3[grep(pattern,test3[,24]),24:32]
test3[grep(pattern,test3[,24]),24] <- NA



#vead: Biogr.: Oma Maa I, Tartu, 1911, 76 (J. Lintrop).

#write.csv(test2, "ISIK7000conv.csv")




#library(splitstackshape)
#test1 <- cSplit(table, "Biograafilised.andmed", "\\. ",fixed=FALSE)
#test1 <- cSplit(table, "Biograafilised.andmed", ". ",direction="long",fixed=TRUE)


library(zoo)

table[, synddate:=as.Date(Sünniaeg, "%Y-%m-%d")]
table[, surmdate:=as.Date(Surmaaeg, "%Y-%m-%d")]
