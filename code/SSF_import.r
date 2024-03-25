############################################################

##IMPORTS AND FORMATS 2020 DFO SUSTAINABILITY SURVEY FOR FISHERIES
##FORMATS STOCK, SPECIES, MANAGEMENT AREA NAMES
##ADDS COMMON NAMES
##ENSURES THAT IT CAN BE MERGED WITH ZIFF LANDINGS DATA

#############################################################
library(data.table)
library(stringr)
library(plyr)

fs<-read.csv('N:/data/OceansNorth_CC/data/DFO_fisheries/2020-sustainability-survey-for-fisheries-eng.csv',header=TRUE,encoding='UTF-8')
names(fs)<-trimws(names(fs),which='both')

##RETAIN COLUMNS OF INTEREST
fs<-subset(fs,select=c("Stock.Information...Q1..Stock...Stock",
                       'stock',
                       "Stock.Information...Q2..Species.Group",
                       "Stock.Information...Q3..Region",
                       "Section.1...Q36..2.1a..What.is.the.current.status.zone.for.this.stock.",
                       "Section.1...Q41..2.3a..Where.the.stock.status.is.uncertain..what.is.the.possibility.of.serious.harm.occurring.to.the.stock."))

names(fs)<-c('stock.orig','stock','speciesg','region','status','harm')

##FORMAT STOCK STATUS CATEGORIES
fs$status<-ifelse(fs$status=='Cautious Zone','cautious',as.character(fs$status))
fs$status<-ifelse(fs$status=='Critical Zone','critical',as.character(fs$status))
fs$status<-ifelse(fs$status=='Healthy Zone','healthy',as.character(fs$status))
fs$status<-ifelse(fs$status=='Uncertain','uncertain',as.character(fs$status))

##SPECIES GROUP
fs$speciesg<-ifelse(fs$speciesg=='other','Other',as.character(fs$speciesg))
##fs$stock<-gsub('\\?','-',fs$stock)

##STANDARDIZE LONG DASHES TO SHORT
fs$stock<-gsub("'", "", fs$stock)
fs$stock<-gsub("\\p{Pd}", "-", fs$stock, perl=TRUE)
##fs$stock<-sub("[[:punct:]]", "-", fs$stock)
##fs$stock<-gsub("-","-",fs$stock)
##fs$stock<-gsub("-","-",fs$stock)

##FORMAT SPECIES NAMES
fs$cname<-gsub(" -.*","",fs$stock)
##fs$cname<-gsub(" \\(.*","",fs$cname)
##fs$cname<-str_remove_all(fs$cname,pattern=c('2J3KL|3Ps'))
fs$cname<-gsub('Atlantic Canada','Atlantic',fs$cname)
fs$cname<-gsub('Shrimp Trawl','Shrimp',fs$cname)
fs$cname<-gsub('\\(Large\\)','',fs$cname)
fs$cname<-gsub('\\(Turbot\\)','',fs$cname)
fs$cname<-gsub('Gulf Shrimp','Shrimp',fs$cname)
fs$cname<-trimws(fs$cname,which='both')


######################################
##FORMAT STOCK AREAS
fs$sarea<-gsub(".*- ","",fs$stock)

##REMOVE SUPERFLUOUS STOCK AREA NAMES
fs$sarea<-str_remove_all(fs$sarea, pattern=c('\\(Spring Spawner\\)|\\(Fall Spawner\\)|\\(Early Stuart\\)|\\(Early Summer\\)|\\(Late\\)|\\(Summer\\)|\\(Pacific\\)|\\(Nunavik\\)|\\(Gasp?\\)|\\(weirs\\)|\\(Large\\)|Southern Gulf of St. Lawrence|Scotian Shelf|mixed stock,|Southern Gulf'))
fs$sarea<-str_remove_all(fs$sarea,pattern=c('\\(|\\)'))
fs$sarea<-trimws(fs$sarea,which='both')

##SET MISSING STOCK AREAS TO NA
fs$sarea<-ifelse(fs$sarea %in% c('Grey Seal','Gaspereau','Elvers','Spot Prawn','Rougheye Rockfish','Giant Red Sea Cucumber','Dungeness Crab','Bocaccio',"Stimpson's Surfclam",'Common Clam','Canary Rockfish','Geosuch','Green Sea Urchin','Longspine Thornyhead','Pacific Oyster','Red Sea Urchin','Sablefish','Shrimp Trawl','Yellowmouth Rockfish','Geoduck','Euphausiids','Pink and Spiny Scallop','Pacific Halibut','Eel'),NA,fs$sarea)

##DENOTE FRESHWATER/MARINE/ANADRAMOUS
fs$habitat<-ifelse(fs$cname %in% c("North Slope Dolly Varden",'Gaspereau','Eulachon','Elvers') | fs$speciesg %in% c('Salmonids'),'ANAD','MAR')
fs$habitat<-ifelse(fs$cname %in% c("Lake Trout","Lake Whitefish"),'FW',fs$habitat)


##FORMAT SCIENTIFIC SPECIES NAMES (NON PACIFIC)
##fs$sname<-ifelse(fs$cname %in% c("Redfish"),'Sebastes Marinus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Arctic Char"),'Salvelinus alpinus',NA)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Atlantic Walrus"),'Odobenus rosmarus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Harp Seal"),'Pagophilus groenlandicus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Whelk"),'Buccinum undatum',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Striped Bass"),'Morone saxatilis',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Sea Cucumber"),'Cucumaria frondosa',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Skate"),'Amblyraja radiata',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Beluga"),'Delphinapterus leucas',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Bowhead"),'Balaena mysticetus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Narwhal"),'Monodon monoceros',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Atlantic Salmon"),'Salmo salar',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Eel"),'Anguilla rostrata',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Gaspereau"),'Alosa pseudoharengus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("American Plaice"),'Hippoglossoides platessoides',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Scallop","Sea Scallop"),'Placopecten magellanicus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Icelandic Scallop"),'Chlamys islandica',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Rock Crab"),'Cancer irroratus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Snow Crab","Queen / Snow Crab"),'Chionoecetes opilio',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("White Hake"),'Urophycis tenuis',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Winter Flounder"),'Pseudopleuronectes americanus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Witch Flounder"),'Glyptocephalus cynoglossus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Atlantic Canada Dogfish"),'Squalus acanthias',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Haddock"),'Melanogrammus aeglefinus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Pollock"),'Pollachius virens',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Silver Hake"),'Merluccius bilinearis',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Surf Clam"),'Mactromeris polynyma',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Yellowtail Flounder"),'Limanda ferruginea',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Bluefin Tuna"),'Thunnus thynnus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Mackerel"),'Scomber scombrus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Capelin"),'Mallotus villosus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Atlantic Cod","Cod"),'Gadus morhua',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Lobster","American Lobster"),'Homarus americanus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Swordfish"),'Xiphias gladius',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Herring"),'Clupea harengus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Greenland Halibut","Greenland Halibut"),'Reinhardtius hippoglossoides',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Atlantic Halibut"),'Hippoglossus hippoglossus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Shrimp","Northern Shrimp","Northern Shrimp (Borealis)"),'Pandalus borealis',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c("Striped Shrimp (Montagui)","Northern Shrimp (Montagui)"),'Pandalus montagui',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Redfish (Sebastes fasciatus)'),'Sebastes fasciatus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Redfish (Sebastes mentella)'),'Sebastes mentella',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Elvers'),'Anguilla rostrata',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Stimpsons Surfclam'),'Mactromeris polynyma',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Atlantic Dogfish'),'Squalus acanthias',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Grey Seal'),'Halichoerus grypus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Lake Trout'),'Salvelinus namaycush',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Lake Whitefish'),'Coregonus clupeaformis',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('North Slope Dolly Varden'),'Salvelinus malma malma',fs$sname)
##TWO SPECIES OF REDFISH CAPTURED - SELECTED FASCIATUS BECAUSE DISTRIBUTION SEEMS MORE ALIGNED WITH THAT OF FISHERY
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Redfish'),'Sebastes fasciatus',fs$sname)
fs$sname<-ifelse(!(fs$region %in% c('Pacific')) & fs$cname %in% c('Common Clam'),'Mya arenaria',fs$sname)

##FORMAT SCIENTIFIC SPECIES NAMES (NON PACIFIC)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Yelloweye Rockfish'),'Sebastes Ruberrimus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Yellowmouth Rockfish'),'Sebastes reedi',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Dogfish'),'Squalus acanthias',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Albacore Tuna'),'Thunnus alalunga',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Dungeness Crab'),'Cancer magister',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Eulachon'),'Thaleichthys pacificus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Herring'),'Clupea Pallasii',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Intertidal Clams'),NA,fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Longspine Thornyhead'),'Saxidomus gigantea',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Pacific Halibut'),'Hippoglossus stenolepis',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Pacific Ocean Perch'),'Sebastes alutus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Pacific Oyster'),'Crassostrea Gigas',fs$sname)
##ALSO CHLAMYS HASTATA
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Pink and Spiny Scallop'),'Chlamys Rubida',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Quillback Rockfish'),'Sebastes Maliger',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Red Sea Urchin'),'Mesocentrotus franciscanus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Rougheye Rockfish'),'Sebastes Aleutianus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Giant Red Sea Cucumber'),'Parastichopus californicus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Geoduck'),'Panopea generosa',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Green Sea Urchin'),'Strongylocentrotus droebachiensis',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Lingcod'),'Ophiodon elongatus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Pacific Hake'),'Merluccius productus',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Pink Salmon'),'Oncorhynchus gorbuscha',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Sablefish'),'Anoplopoma fimbria',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Sardine'),'Sardinops Sagax',fs$sname)
##ALSO PANDALOPSIS DISPAR
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Shrimp'),'Pandalus jordani',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Sockeye Salmon'),'Oncorhynchus nerka',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Southern Inside chum'),'Oncorhynchus keta',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Spot Prawn'),'Pandalus platyceros',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Bocaccio'),'Sebastes paucispinis',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Canary Rockfish'),'Sebastes pinniger',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Chinook Salmon'),'Oncorhynchus tshawytscha',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Coho Salmon'),'Oncorhynchus kisutch',fs$sname)
fs$sname<-ifelse(fs$region %in% c('Pacific') & fs$cname %in% c('Euphausiids'),NA,fs$sname)



fs$stock<-trimws(fs$stock,which='both')
fs$cname<-tolower(fs$cname)
fs$sname<-tolower(fs$sname)
