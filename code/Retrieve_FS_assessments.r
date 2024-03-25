#library(readxl)
#library(gdata)
library(plyr)
library(stringr)
library(fmsb)
library(readtext)
library(quanteda)
library(tidyr)
library(lattice)
library(scales)
#library(tm)
library(treemapify)
library(ggplot2)
library(pals)
library(mgcv)
install.packages(c('readtext','quanteda'),dependencies=TRUE)


#FIRST DOWNLOAD TABLES CONTAINING ALL REPORTS FROM:http://www.isdm-gdsi.gc.ca/csas-sccs/applications/Publications/search-recherche-eng.asp
#THEN BATCH CONVERT ALL EXCEL FILES TO CSV: https://www.extendoffice.com/documents/excel/5537-excel-batch-convert-to-csv.html
#THEN USE BELOW TO IMPORT AND FORMAT
#library(readxl)
#library(gdata)

tabsdir<-'N:/data/OceansNorth_CC/data/DFO_fisheries'


#################################################

#FIRST PART FORMATS 'MASTER SPREADSHEET' CONTAINING IMPORTANT INFORMATION OF CSAS DOCUMENTS; IDENTIFIES THOSE WE WANT

#################################################

##IMPORTS DFO FISHERIES SURVEY AND FORMATS
fs<-read.csv('N:/data/OceansNorth_CC/data/DFO_fisheries/2020-sustainability-survey-for-fisheries-eng.csv',header=TRUE,encoding='UTF-8')
names(fs)<-trimws(names(fs),which='both')

##RETAIN COLUMNS OF INTEREST
fs<-subset(fs,select=c("X.U.FEFF.Stock.Information...Q1..Stock...Stock",
                       'stock',
                       "Stock.Information...Q2..Species.Group",
                       "Stock.Information...Q3..Region",
                       "Stock.Information...Q9..Provide.the.link.to.the.Scientific.Report..if.available."))

##STOCK.ORIG IS ORIGINAL UNCHANGED STOCK, STOCK HAS BEEN EDITED TO HAVE '-' SEPARATIGN SPECIES AND STOCK AREA
names(fs)<-c('stock.orig','stock','speciesg','region','url')

##sp<-word(fs$stock,start=1,end=2)

fs$speciesg<-ifelse(fs$speciesg=='other','Other',as.character(fs$speciesg))
##fs$stock<-gsub('\\?','-',fs$stock)

##STANDARDIZE LONG DASHES TO SHORT
fs$stock<-gsub("-","-",fs$stock)

##CREATE SEPARATE COLUMNS FOR STOCK AREAS AND SPECEIS
##FORMAT SPECIES NAMES
fs$cname<-gsub(" -.*","",fs$stock)
##fs$cname<-gsub(" \\(.*","",fs$cname)
##fs$cname<-str_remove_all(fs$cname,pattern=c('2J3KL|3Ps'))
fs$cname<-trimws(fs$cname,which='both')

fs$sarea<-gsub(".*- ","",fs$stock)

##FORMAT STOCK AREAS
fs$sarea<-gsub('\\(Fall Spawner\\)','',fs$sarea)
fs$sarea<-gsub('\\(Spring Spawner\\)','',fs$sarea)
fs$sarea<-gsub("\\(Fall Spawner\\) \\/ \\(Spring Spawner\\)","",fs$sarea)
fs$sarea<-gsub("Atlantic \\(NAFO 3-4\\)","NAFO 3-4",fs$sarea)
fs$sarea<-gsub("Scotian Shelf \\(4X\\)","4X",fs$sarea)
fs$sarea<-gsub("Inshore SFA 28 \\(Bay of Fundy\\)",'Inshore SFA 28',fs$sarea)
fs$sarea<-gsub("4X5 \\(Western Component\\)","4X5",fs$sarea)
fs$sarea<-gsub("5Y, 5Z \\(weirs\\)","5Y, 5Z",fs$sarea)
fs$sarea<-gsub("Southern Gulf of St. Lawrence \\(SFA 21a, b, c, 22, 23, 24\\)","SFA 21a, b, c, 22, 23, 24",fs$sarea)
fs$sarea<-gsub("4T \\(Fall Spawner\\)","4T",fs$sarea)
fs$sarea<-gsub("4T \\(Spring Spawner\\)","4T",fs$sarea)
fs$sarea<-gsub("Southern Gulf of St. Lawrence \\(4T\\)","4T",fs$sarea)

fs$sarea<-gsub("Northern \\(2J3KL\\)","2J3KL",fs$sarea)
fs$sarea<-gsub("Atlantic \\(3Ps\\)","3Ps",fs$sarea)
fs$sarea<-gsub("Scotian Shelf \\(SFA 13-15\\)","SFA 13-15",fs$sarea)
fs$sarea<-gsub("Offshore SFA 26 German, Browns","Offshore SFA 26",fs$sarea)
fs$sarea<-gsub("Offshore SFA 27, Georges","Offshore SFA 27",fs$sarea)
fs$sarea<-gsub("Southern Gulf \\(LFA 23, 24, 25, 26A, 26B\\)","LFA 23, 24, 25, 26A, 26B",fs$sarea)
fs$sarea<-gsub("Southern Gulf of St. Lawrence \\(4TVn\\)","4TVn",fs$sarea)
fs$sarea<-gsub('\\/','',fs$sarea)
fs$sarea<-trimws(fs$sarea,which='both')

fs$sarea<-ifelse(fs$stock %in% c("Common Clam","Eel \\(Large\\)","Elvers","Stimpson's Surfclam","Grey Seal","Gulf Shrimp"),NA,fs$sarea)

##REMOVE STOCKS WITH UNCERTAIN AREAS
##fs<-subset(fs,is.na(sarea)==FALSE)

##a<-subset(fs,is.na(sarea)==TRUE)
##a<-subset(fs,region %in% c('Pacific') & !(speciesg %in% c('Salmonids')))
##b<-subset(area,country=='Canada')


##REMOVE THESE FRESHWATER SPECIES
fs<-subset(fs,!(cname %in% c("Lake Trout","Lake Whitefish","North Slope Dolly Varden")) &
             !(speciesg %in% c('Salmonids')))


##FORMAT SPECIES NAMES
##fs$sname<-ifelse(fs$cname %in% c("Redfish"),'Sebastes Marinus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Arctic Char"),'Salvelinus alpinus',NA)
fs$sname<-ifelse(fs$cname %in% c("Atlantic Walrus"),'Odobenus rosmarus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Harp Seal"),'Pagophilus groenlandicus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Whelk"),'Buccinum undatum',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Striped Bass"),'Morone saxatilis',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Sea Cucumber"),'Cucumaria frondosa',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Skate"),'Amblyraja radiata',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Beluga"),'Delphinapterus leucas',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Bowhead"),'Balaena mysticetus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Narwhal"),'Monodon monoceros',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Atlantic Salmon"),'Salmo salar',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Eel (Large)"),'Anguilla rostrata',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Gaspereau"),'Alosa pseudoharengus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("American Plaice"),'Hippoglossoides platessoides',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Scallop","Sea Scallop"),'Placopecten magellanicus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Icelandic Scallop"),'Chlamys islandica',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Rock Crab"),'Cancer irroratus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Snow Crab","Queen / Snow Crab"),'',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("White Hake"),'Urophycis tenuis',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Winter Flounder"),'Pseudopleuronectes americanus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Witch Flounder"),'Glyptocephalus cynoglossus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Atlantic Canada Dogfish"),'Squalus acanthias',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Haddock"),'Melanogrammus aeglefinus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Pollock"),'Pollachius pollachius',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Silver Hake"),'Merluccius bilinearis',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Surf Clam"),'Spisula solidissima',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Yellowtail Flounder"),'Pleuronectes ferruginea',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Bluefin Tuna"),'Thunnus thynnus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Mackerel"),'Scomber scombrus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Capelin"),'Mallotus villosus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Atlantic Cod","Cod"),'Gadus morhua',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Lobster","American Lobster"),'Homarus americanus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Swordfish"),'Xiphias gladius',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Herring"),'Clupea harengus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Greenland Halibut (Turbot)","Greenland Halibut"),'Reinhardtius hippoglossoides',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Atlantic Halibut"),'Hippoglossus hippoglossus',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Shrimp","Northern Shrimp","Northern Shrimp (Borealis)"),'Pandalus borealis',fs$sname)
fs$sname<-ifelse(fs$cname %in% c("Striped Shrimp (Montagui)","Northern Shrimp (Montagui)"),'Pandalus montagui',fs$sname)




##############################################

#GO THROUGH FILE, AND DOWNLOAD PDFS OF DFO REPORTS

##############################################
library(XML)
library(RCurl)
l<-list()
for(i in 1:dim(dat)[1]){
print(i)
d<-fs[i,]

#NAME OF OUTPUT PDF
cnm<-gsub(' ','_',d$cname)
anm<-gsub(' ','_',d$sarea)
nm<-gsub(' ','',paste(cnm,'_',anm,'.pdf'))

#nm<-gsub(' ','',paste(d$region,'.',d$series,'.',d$num,'.pdf'))
d$filen<-nm

#DOWNLOAD FILE AND SAVE
setwd('N:/data/CAFF/data/FSS_assessments')
#setwd('N:/data/OceansNorth_CC/literature/DFO_assessments/reports/reduced')
url1<-as.character(d$url)

#IF PDF LINK IN URL THEN DOWNLOAD, ELSE SCRAPE PAGE
if(grepl('pdf',url1)){
try(download.file(url1,destfile=paste(nm),method='auto',mode='wb'),TRUE)
  
} else {
NULL
  }
}



