##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
library(fossil)
library(gstat)
library(sp)
library(maptools)
library(rgeos)
library(mapdata)
library(rgdal)
library(landscapemetrics)
library(viridis)
library(raster)
library(tidyr)
library(gridExtra)
library(plotrix)
library(ggplot2)
library(plyr)
library(data.table)
library(pals)
library(dplyr)
library(maps)
library(VoCC)
library(repmis)
library(R.matlab)
library(circular)
library(abind)
library(DescTools)
library(ncdf4)
library(foreign)
library(stringdist)
library(fuzzyjoin)
library(ggrepel)
library(ggExtra)
library(Hmisc)
library(hrbrthemes)
library(stringr)
library(reshape2)
library(gplots)
library(fields)
library(ggplot2)
library(raster)
library(maps)
require(maptools)
library(sp)
library(plyr)
library(gdata)
library(rgeos)
library(rgdal)
require(devtools)
##require(cleangeo)
library(sf)


##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir,'GPARAMs.r',sep=''))
 
###################################################################
##IMPORT FISHERIES SUSTAINABILITY SURVEY
source('C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/SSF_import.r')

##########################################################################

##IMPORT SEVERAL SHAPEFILES NEEDED TO RESOLVE FISHERIES MANAGEMENT BOUNDS

##########################################################################
##NAFO SHAPEFILE
mcrt<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
nafo<-readOGR('N:/data/stock_assessments/data/fishing_shapefiles/nafo','Divisions')#works for Alaska - most others don't
nafo<-spTransform(nafo,CRS(mcrt))
##plot(nafo)
##text(nafo,labels=nafo$ZONE)

#########################
##CLAM FISHING AREAS (PACIFIC)
clamfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/clam/pacific','CMA')
clamfa<-spTransform(clamfa,CRS(mcrt))
#plot(clamfa,axes=TRUE,xlim=c(-130,-122),ylim=c(48,51))
plot(clamfa,axes=TRUE)
maps::map('world',fill=TRUE,col='gray',add=TRUE)
text(clamfa,labels=clamfa$LABEL,col='red',cex=2)
text(clamfa,labels=clamfa$Location,col='red')

#########################
##CRAB FISHING AREAS
cfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas','CrabManagementAreas')#works for Alaska - most others don't
cfa<-spTransform(cfa,CRS(mcrt))
plot(cfa)
text(cfa,labels=sfa$ZONE)

#########################
##LOBSTER FISHING AREAS
lfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/lobster','LFA_poly')
lfa<-spTransform(lfa,CRS(mcrt))

#########################
##SHRIMP FISHING AREAS
sfa2<-readOGR('N:/data/shapefiles/DFO_fishing_areas','ShrimpManagementAreas')
sfa2<-spTransform(sfa2,CRS(mcrt))
plot(sfa2)
text(sfa2,labels=sfa2$ZONE)

#########################
##SHRIMP FISHING AREAS
sfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/shrimp/SFAs_Manon_Cassista','SFAs_All_2020_Validated')
sfa<-spTransform(sfa,CRS(mcrt))
sfa$ZONE[8]<-'3'
sfa$ZONE[9]<-'2'
plot(sfa)
text(sfa,labels=sfa$ZONE)



#########################
##SNOW CRAB FISHING AREAS
scfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/snow_crab','dfo_east_snow_crab_areas_erase')
scfa<-spTransform(scfa,CRS(mcrt))
scfa$NAME<-c("17","16","12A","15","16A","12C","12B","12E","12F","14","13","3A","3B","3C","3D","4","5","7A","7B","6A","6B","6C","7C","9","8","10","11","23","22","21","20","19","24","4X","D","12")
plot(scfa)
text(scfa,labels=sfa$ZONE)

#########################
##MARINE ECOREGIONS AREAS
meow<-readOGR('N:/data/shapefiles/meow/MEOW','meow_ecos')
meow<-spTransform(meow,CRS(mcrt))
meow<-subset(meow,PROVINCE %in% c("Cold Temperate Northwest Atlantic",'Arctic'))
names(meow)<-tolower(names(meow))

#########################
##SCALLOP FISHING AREAS (MARITIMES AND NFLD)
sclfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/scallop','ScallopFishingAreas_Trim')
sclfa<-spTransform(sclfa,CRS(mcrt))
##par(mfrow=c(2,2))
##plot(sclfa)
##text(sclfa,labels=sclfa$ID)


##SCALLOP FISHING AREAS (GULF)
sclfag<-readOGR('N:/data/shapefiles/DFO_fishing_areas/scallop/gulf','ScallopFishingAreas')
sclfag<-spTransform(sclfag,CRS(mcrt))
par(mfrow=c(2,2))
plot(sclfag)
text(sclfag,labels=sclfag$Unite_Gest)

#########################
##MARITIMES SCALLOP - CONTAINED IN THE ABOVE FILE
##sclfa2<-readOGR('N:/data/shapefiles/DFO_fishing_areas/scallop/maritimes',layer='SFF_MAR_Scallop_Areas')
##plot(sclfa2)
##text(sclfa2,labels=sclfa2$Area)

##PACIFIC MANAGEMENT AREAS (OYSTER)
pacfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/pacific_groundifsh_areas','DFO_BC_PFMA_SUBAREAS_50K_V3_1')
pacfa<-spTransform(pacfa,CRS(mcrt))
plot(pacfa,axes=TRUE,xlim=c(-127,-122),ylim=c(48,51))
maps::map('world',add=TRUE)
text(pacfa,labels=pacfa$MGNT_AREA)

#########################
##PACIFIC HERRING
herfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/herring/pacific','SectionsIntegrated')
herfa<-spTransform(herfa,CRS(mcrt))
plot(herfa)
text(herfa,labels=herfa$Section)
map('world',add=TRUE,col='red',fill=TRUE)

##NAFO SUBDIVISIONS
nafos<-readOGR('N:/data/shapefiles/NAFO/subdiv_mcmahon',layer='NAFO_Best')
plot(nafos)
maps::map('world',add=TRUE,fill=TRUE,col='gray')
text(nafos,labels=nafos$NAFO_BEST,cex=0.5)

##CRAB AREAS - PACIFIC
##shrmp<-readOGR('N:/data/shapefiles/DFO_fishing_areas/crab/pacific/Crab_Management_Areas_50k.gdb')
##shrmp<-spTransform(shrmp,CRS(mcrt))
##plot(shrmp)
##maps::map('world',add=TRUE,fill=TRUE,col='gray')
##text(shrmp,labels=shrmp$MGNT_AREA,cex=0.5)

#########################
##NAFO SUBDIVISIONS - FOR POLLOCK - NEED TO CROP OUT US PORTION
pollfa0<-readOGR('N:/data/shapefiles/NAFO',layer='NAFOsubdiv_WGS84')
pollfa0<-spTransform(pollfa0,CRS(mcrt))
pollfa0<-st_as_sf(pollfa0)
pollfa1<-readOGR('N:/data/shapefiles/NAFO',layer='NAFOsubdiv_WGS84')
plot(pollfa1)
maps::map('world',add=TRUE,fill=TRUE,col='gray')
text(pollfa1,labels=pollfa1$FID)

##CLIP TO EXCLUDE US
eez<-st_read('N:/data/shapefiles/eez/v10/eez_v10.shp')
eez<-subset(eez,Sovereign1=='Canada')
##paceez<-as(eez,'Spatial')
##paceez<-raster::crop(paceez,extent(-140,-90,39,50))
##paceez<-raster::crop(as(eez,'Spatial'),extent(-140,-90,39,50))
##plot(paceez)

##eez<-subset(eez,Sovereign1=='United States')
##eez<-spTransform(eez,CRS(mcrt))
st_crs(pollfa0)==st_crs(eez)
pollfai<-st_intersection(st_make_valid(pollfa0), st_make_valid(eez))
pollfa<-as(pollfai,'Spatial')

options(sf_max.plot=1)
ggplot(pollfai)+
  geom_sf()+
  geom_sf_label(aes(label=FID))


#########################
##OCEAN PERCH - PACIFIC GROUNDFISH AREAS - USED FOR DOGFISH, OCEAN PERCH
library(tidyverse)
library(maptools)
(load('N:/data/shapefiles/DFO_fishing_areas/ocean_perch/gma.popymr.rda'))
##plot(gma.popymr)
a<-PolySet2SpatialPolygons(gma.popymr,close_polys=TRUE)
names(a)

popfa<-st_as_sf(a)
nms<-c('3C','3D','4B','5A','5B','5C','5D','5E')
popfa<-add_column(popfa,nms=nms)
popfa<-as(popfa,'Spatial')
popfa<-spTransform(popfa,CRS(mcrt))
##plot(popfa)

#########################
##BOWHEAD
bowfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/bowhead',layer='EC-WG-bowhead-EPSG4326')
plot(bowfa)

#########################
##NARWHAL
##NEED TO READ IN MULTIPLE INDIVIDUAL FILES, STANDARDIZE, AND COMBINE
f<-function(d){
dat<-st_read(paste('N:/data/shapefiles/DFO_fishing_areas/narwhal/',d,'.shp',sep=''))
dat<-st_transform(dat,mcrt)
dat<-subset(dat,select=c('Name'))  
dat$Name<-d
return(dat)
}
narfa<-f('Admiralty_Inlet')
narfb<-f('Dove_Bay')
narfc<-f('East_Baffin_Island')
narfd<-f('Eclipse_Sound')
narfe<-f('Greenland_Sea')
narff<-f('Inglefield_Bredning')
narfg<-f('Jones_Sound')
narfh<-f('Jong_Sound')
narfi<-f('Melville_Bay')
narfj<-f('Smith_Sound')
narfk<-f('Somerset_Island')
narfl<-f('East_Greenland')
narfa<-rbind(narfa,narfb,narfc,narfd,narfe,narff,narfg,narfh,narfi, narfj,narfk,narfl)
narfa<-as(narfa,'Spatial')
plot(narfa)
maps::map('world',add=TRUE,fill=TRUE,col='gray')
text(narfa,labels=narfa$Name)
plot(narfa,add=TRUE, border='red')
a<-subset(narfa,Name %in% c('Somerset_Island'))
plot(a,add=TRUE,col='green')

bior<-readOGR('N:/data/shapefiles/DFO_Marine_Bioregions/DFO_Marine_Bioregions.gdb')
names(bior)<-tolower(names(bior))
bior<-subset(bior,label %in% c(8,10,11,12))
bior<-spTransform(bior, CRS=(mcrt))

bior$label2<-ifelse(bior$label==11,'Scotian Shelf',NA)
bior$label2<-ifelse(bior$label==12,'Estuary and Gulf of St. Lawrence',bior$label2)
bior$label2<-ifelse(bior$label==10,'Newfoundland and Labrador Shelves',bior$label2)
bior$label2<-ifelse(bior$label==8,'Eastern Arctic',bior$label2)
 
plot(bior)
text(bior,labels=bior$label2)



#############################################################

##FIRST CODE BELOW CREATES RASTERS FOR EACH STOCK

#############################################################
##FULL RASTER ACROSS AOI
##GEOGRAPHIC DOMAIN
latd<-c(38,85)
lond<-c(-41,-150)
mct<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
##GLOBAL 0.25 DEGREE RASTER
crds<-expand.grid(lon=seq(-179.875,179.875,0.25),
                  lat=seq(-89.875,89.875,0.25))
crds0.25<-subset(crds,lon<=lond[1] & lon>=lond[2] & lat>=latd[1] & lat<=latd[2])
r0.25deg<-rasterFromXYZ(crds)
crs(r0.25deg) <- mct


####################################################
dt<-subset(fs,stock %in% c("American Lobster - LFA 3-14c"))
dt<-fs[21,]
dt<-subset(fs,stock %in% c("Snow Crab - 12A"))
dt<-subset(fs,stock %in% c("American Lobster - LFA 3-14c"))

###############################################
areafun<-function(dt){
  sarea<-ifelse(is.na(dt$sarea)==TRUE,'No',unique(dt$sarea))#SELECTS UNIQUE AREACODE
  cname<-tolower(unique(dt$cname))
  sname<-tolower(unique(dt$sname))
  stock<-unique(dt$stock)
  print(unique(dt$stock))

pac_grfish<-tolower(c('pacific ocean perch','quillback rockfish','dogfish','yelloweye rockfish'))
crabs<-tolower(c('rock crab','queen / snow crab'))
##  crabs<-tolower(c('Rock crab','Snow Crab','Queen / Snow Crab'))
lobsters<-tolower(c('lobster','american lobster'))
  shrimps<-tolower(c('shrimp','Shrimp','northern shrimp','northern shrimp (borealis)','northern shrimp (montagui)','gulf shrimp','striped shrimp (montagui)'))
scallops<-tolower(c('sea scallop','scallop'))
redfish<-c('redfish','redfish (sebastes fasciatus)','redfish (sebastes mentella)')

#SLICES UP POLYGONS ACCORDING TO REGIONS GIVEN IN RAM DB
  if (sarea=="3NOPs4VWX+5"){plg<-subset(nafo,ZONE %in% c('3N','3O','3Ps','4Vn','4Vs','4W','4X','5Ze','5Zw'))
  } else if (sarea %in% c("Western Atlantic","Northwest Atlantic","North Atlantic","Western Atlantic")){plg<-nafo
  } else if (sarea=="3NOPs"){plg<-subset(nafo,ZONE %in% c('#n','3O','3Ps'))
} else if (sarea=="3NO"){plg<-subset(nafo,ZONE %in% c('3N','3O'))
} else if (sarea=="3LNO"){plg<-subset(nafo,ZONE %in% c('3L','3N','3O'))
} else if (sarea=="3LN"){plg<-subset(nafo,ZONE %in% c('3L','3N'))
} else if (sarea=="2J3KL"){plg<-subset(nafo,ZONE %in% c('2J','3K','3L'))
} else if (sarea=="2J3IKLPs"){plg<-subset(nafo,ZONE %in% c('2J','3I','3K','3L','3Ps'))
} else if (sarea=="2-3KLMNO"){plg<-subset(nafo,ZONE %in% c('2G','2H','2J','3K','3L','3M','3N','3O'))
} else if (sarea %in% c('2 + 3KLMNO',"2-3KLMNO")){plg<-subset(nafo,ZONE %in% c('2G','2H','2J','3K','3L','3M','3N','3O'))

} else if (sarea=="5Zjm"){plg<-subset(nafo,ZONE %in% c('5Ze','5Zw'))
} else if (sarea=="5Z"){plg<-subset(nafo,ZONE %in% c("5Ze","5Zw"))
} else if (sarea=="5Y, 5Z"){plg<-subset(nafo,ZONE %in% c("5Ze","5Zw",'5Y'))
} else if (sarea=='5'){plg<-subset(nafo,ZONE %in% c("5Ze","5Zw",'5Y'))
} else if (sarea=="4X5Y"){plg<-subset(nafo,ZONE %in% c('4X','5Y'))
} else if (sarea=="4X5"){plg<-subset(nafo,ZONE %in% c('4X','5Ze','5Zw'))
} else if (sarea=="4X"){plg<-subset(nafo,ZONE %in% c("4X"))
} else if (sarea=="4VWX"){plg<-subset(nafo,ZONE %in% c('4Vn','4Vs','4W','4X'))
} else if (sarea=="4TVn"){plg<-subset(nafo,ZONE %in% c('4T','4Vn'))
} else if (sarea=="4T"){plg<-subset(nafo,ZONE %in% c('4T'))
} else if (sarea=="4S"){plg<-subset(nafo,ZONE %in% c('4S'))
} else if (sarea=="4RST"){plg<-subset(nafo,ZONE %in% c('4R','4S','4T'))
} else if (sarea=="4RS-3Pn"){plg<-subset(nafo,ZONE %in% c('4R','4S','3Pn'))
} else if (sarea=="4R"){plg<-subset(nafo,ZONE %in% c('4R'))
} else if (sarea=="3PS"){plg<-subset(nafo,ZONE %in% c('3Ps'))
} else if (sarea=="3Ps"){plg<-subset(nafo,ZONE %in% c('3Ps'))
} else if (sarea=="3O"){plg<-subset(nafo,ZONE %in% c('3O'))
} else if (sarea=="Scotian Shelf (ENS-N)"){plg<-subset(nafo,ZONE %in% c('4W'))
} else if (sarea=="Scotian Shelf (ENS-S)"){plg<-subset(nafo,ZONE %in% c('4X'))
} else if (sarea=="West Greenland (ECWG)"){plg<-subset(nafo,ZONE %in% c('1A','1B','1C','1D','1E','1F'))
} else if (sarea=="NAFO 0A and 0B"){plg<-subset(nafo,ZONE %in% c('0A','0B'))
} else if (sarea=="Atlantic NAFO 3-4"){plg<-subset(nafo,ZONE %in% c('3K','3L','3M','3N','3O','3Pn','3Ps','4R','4S','4T','4Vn','4Vs','4X','4W'))
} else if (sarea=="Grand Bank"){plg<-subset(nafo,ZONE %in% c('3L'))
} else if (sarea=="Gulf"){plg<-subset(nafo,ZONE %in% c('4S','4T','4R'))
} else if (sarea=="Bay of Fundy"){plg<-subset(nafo,ZONE %in% c('4X'))
} else if (is.na(sarea)==TRUE & stock %in% c('Grey Seal')){plg<-subset(nafo,ZONE %in% c('4X','4T','4W','4Vn','4Vs'))
} else if (is.na(sarea)==TRUE & stock %in% c('Elvers')){plg<-subset(nafo,ZONE %in% c('4X','4W','4Vn','4Vs'))

} else if (sarea=='SA2+3KLPs'){plg<-subset(nafo,ZONE %in% c('2J','3K','3K','3L'))

} else if (cname %in% c('sea scallop','scallops') & sarea=="Inshore SFA 28 Bay of Fundy"){plg<-subset(sclfa,ID %in% c('28'))
} else if (cname %in% c('sea scallop','scallops') & sarea=="Inshore SFA 29W"){plg<-raster::crop(subset(sclfa,ID %in% c('29')),extent(-70,-63,39,50))
} else if (cname %in% c('sea scallop','scallops') & sarea=="Offshore SFA 26 German, Browns"){plg<-subset(sclfa,ID %in% c('26'))
} else if (cname %in% c('sea scallop','scallops') & sarea=="Offshore SFA 27, Georges"){plg<-subset(sclfa,ID %in% c('27'))
} else if (cname %in% c('sea scallop','scallops','scallop') & sarea=="SFA 21a, b, c, 22, 23, 24"){plg<-subset(sclfag,Unite_Gest %in% c('21a','22','23','24'))
} else if (cname %in% c('sea scallop','scallops','scallop') & sarea=="Area 20"){plg<-subset(sclfag,Unite_Gest %in% c('20A','20B','20C','20E','20F'))

} else if (cname %in% lobsters & sarea=="Areas 19Offshore LFA 41"){plg<-subset(lfa,ZONE %in% c('41'))

} else if (cname %in% lobsters & sarea=="Offshore LFA 41"){plg<-subset(lfa,ZONE %in% c('41'))
} else if (cname %in% lobsters & sarea=="Inshore LFA 35-38"){plg<-subset(lfa,ZONE %in% c('35','36','37','38'))
} else if (cname %in% lobsters & sarea=="Inshore LFA 34"){plg<-subset(lfa,ZONE %in% c('34'))
} else if (cname %in% lobsters & sarea=="Inshore LFA 27-33"){plg<-subset(lfa,ZONE %in% c('27','28','29','30','31A','31B','32','33'))
} else if (cname %in% lobsters & sarea=="LFA 23, 24, 25, 26A, 26B"){plg<-subset(lfa,ZONE %in% c('23','24','25','26A','26B'))
} else if (cname %in% lobsters & sarea=="LFA 3-14c"){plg<-subset(lfa,ZONE %in% c('3','4','5','6','7','8','9','10','11','12','13','14A','14B','14C'))
} else if (cname %in% lobsters & sarea=="17"){plg<-subset(lfa,ZONE %in% c('17'))
} else if (cname %in% lobsters & sarea=="Areas 19-20-21"){plg<-subset(lfa,ZONE %in% c('19','20A','20B','21'))
} else if (cname %in% lobsters & sarea=="Zone 22 MI"){plg<-subset(lfa,ZONE %in% c('22'))


} else if (cname %in% crabs & sarea=="Areas 19-20-21"){plg<-subset(lfa,ZONE %in% c('19','20A','20B','21'))
} else if (cname %in% crabs & sarea=="Zone 22 (MI)"){plg<-subset(lfa,ZONE %in% c('22'))
} else if (cname %in% crabs & sarea=="Scotian Shelf (ENS-S)"){plg<-subset(cfa,ZONE %in% c('S-ENS'))
} else if (cname %in% crabs & sarea=="Scotian Shelf (ENS-N)"){plg<-subset(cfa,ZONE %in% c('N-ENS'))
} else if (cname %in% crabs & sarea=="LFA 23, 24, 25, 26A"){plg<-subset(lfa,ZONE %in% c('23','24','25','26A'))
} else if (cname %in% c('snow crab') & sarea=="12A"){plg<-subset(scfa,NAME %in% c('12A'))
} else if (cname %in% c('snow crab') & sarea=="12B"){plg<-subset(scfa,NAME %in% c('12B'))
} else if (cname %in% c('snow crab') & sarea=="12C"){plg<-subset(scfa,NAME %in% c('12C'))
} else if (cname %in% c('snow crab') & sarea=="13"){plg<-subset(scfa,NAME %in% c('13'))
} else if (cname %in% c('snow crab') & sarea=="14"){plg<-subset(scfa,NAME %in% c('14'))
} else if (cname %in% c('snow crab') & sarea=="16"){plg<-subset(scfa,NAME %in% c('16A',''))
} else if (cname %in% c('snow crab') & sarea=="16A"){plg<-subset(scfa,NAME %in% c('16A'))
} else if (cname %in% c('snow crab') & sarea=="17"){plg<-subset(scfa,NAME %in% c('17'))
} else if (cname %in% c('snow crab') & sarea=="15"){plg<-subset(scfa,NAME %in% c('15'))
} else if (cname %in% c('snow crab') & sarea=="CFA 12 12, 18, 25, 26, 12E, 12F, 19"){plg<-subset(scfa,NAME %in% c('12','18','25','26','19'))
} else if (cname %in% c('snow crab') & sarea=="ENS-N"){plg<-subset(cfa,Area %in% c('N-ENS'))
} else if (cname %in% c('snow crab') & sarea=="ENS-S"){plg<-subset(cfa,Area %in% c('S-ENS'))

} else if (cname %in% c('sardine','albacore tuna','pacific halibut','yellowmouth rockfish','rougheye rockfish','red sea urchin','pacific hake','canary rockfish','giant red sea cucumber','green sea urchin','longspine thornyhead','sablefish','geoduck','bocaccio','dungeness crab','shrimp trawl','spot prawn','euphausids')){plg<-popfa

} else if (cname %in% c('eel (large)') & is.na(sarea)==TRUE){plg<-subset(bior,label2 %in% c('Scotian Shelf'))

} else if (cname=='surf clam' & sarea=="Banquereau"){plg<-raster::crop(subset(bior,label2 %in% c('Scotian Shelf')),extent(-60,-56,43,45.5))

} else if (cname %in% c('pacific oyster') & is.na(sarea)==TRUE){plg<-popfa

} else if (cname %in% c('pink and spiny scallop')){plg<-subset(pacfa, MGNT_AREA %in% c('13','14','15','16','17','18','19','20','29'))

} else if (cname %in% c('intertidal clams') & sarea %in% c('Central Coast-Heiltsuk Manila')){plg<-subset(clamfa, LABEL %in% c("Heiltsuk JMP"))
} else if (cname %in% c('intertidal clams') & sarea %in% c('North Coast Haida Gwaii Razor')){plg<-subset(clamfa, LABEL %in% c("Haida JMP" ))
} else if (cname %in% c('intertidal clams') & sarea %in% c('South Coast-Vancouver Island')){plg<-subset(clamfa, LABEL %in% c("CMA E"))

} else if (cname %in% pac_grfish & sarea %in% c('PMFC 3CD-WCVI')){plg<-subset(popfa, nms %in% c("3C",'3D'))
} else if (cname %in% pac_grfish & sarea %in% c('PMFC 5ABC-QCS')){plg<-subset(popfa, nms %in% c('5A','5B','5C'))
} else if (cname %in% pac_grfish & sarea %in% c('PMFC 5DE-HS/DE/WHG')){plg<-subset(popfa, nms %in% c('5D','5E'))

} else if (cname %in% pac_grfish & sarea %in% c('Inside','inside','Inside Population')){plg<-subset(popfa, nms %in% c("4B"))
} else if (cname %in% pac_grfish & sarea %in% c('Outside','outside','Outside Population')){plg<-subset(popfa, nms %in% c('3C','3D','5A','5B','5C','5D','5E'))

} else if (cname %in% c('lingcod') & sarea %in% c('Outside')){plg<-subset(popfa, nms %in% c('3C','3D','5A','5B','5C','5D','5E'))

} else if (cname %in% c('pollock') & sarea=="4X5 Western Component"){plg<-subset(pollfa,FID %in% c('84','85','87','81','82','83','94','95'))

} else if (cname %in% c('herring') & sarea=="Central Coast"){plg<-subset(herfa,Assessment %in% c('Central Coast'))
} else if (cname %in% c('herring') & sarea=="Haida Gwaii"){plg<-subset(herfa,Assessment %in% c('HG WEST','HG EAST'))
} else if (cname %in% c('herring') & sarea=="Prince Rupert District"){plg<-subset(herfa,Assessment %in% c('Prince Rupert'))
} else if (cname %in% c('herring') & sarea=="Strait of Georgia Pacific"){plg<-subset(herfa,Assessment %in% c('Strait of Georgia'))
} else if (cname %in% c('herring') & sarea=="WCVI"){plg<-subset(herfa,Assessment %in% c('W.C. Vancouver Is.'))

} else if (cname %in% shrimps & sarea=="Eastern Assessment Zone"){plg<-subset(sfa,ZONE %in% c('2'))
} else if (cname %in% shrimps & sarea=="WAZ"){plg<-subset(sfa,ZONE %in% c('3'))
} else if (cname %in% shrimps & sarea=="SFA 1"){plg<-subset(sfa,ZONE %in% c('1'))
} else if (cname %in% shrimps & sarea=="SFA 2"){plg<-subset(sfa,ZONE %in% c('2'))
} else if (cname %in% shrimps & sarea=="SFA 3"){plg<-subset(sfa,ZONE %in% c('3'))
} else if (cname %in% shrimps & sarea=="SFA 4"){plg<-subset(sfa,ZONE %in% c('4'))
} else if (cname %in% shrimps & sarea=="SFA 5"){plg<-subset(sfa,ZONE %in% c('5'))
} else if (cname %in% shrimps & sarea=="SFA 6"){plg<-subset(sfa,ZONE %in% c('6'))
} else if (cname %in% shrimps & sarea=="SFA 7"){plg<-subset(sfa,ZONE %in% c('7'))
} else if (cname %in% shrimps & sarea=="SFA 8"){plg<-subset(sfa,ZONE %in% c('8'))
} else if (cname %in% shrimps & sarea=="SFA 9"){plg<-subset(sfa,ZONE %in% c('9'))
} else if (cname %in% shrimps & sarea=="SFA 10"){plg<-subset(sfa,ZONE %in% c('10'))
} else if (cname %in% shrimps & sarea=="SFA 12"){plg<-subset(sfa,ZONE %in% c('12'))
} else if (cname %in% shrimps & sarea=="SFA 13-15"){plg<-subset(sfa,ZONE %in% c('13','14','15'))
} else if (sarea=="Baffin Bay (High Arctic)"){plg<-subset(meow,ecoregion %in% c('Baffin Bay - Davis Strait'))
} else if (sarea=="Penny Strait-Lancaster Sound (High Arctic)"){plg<-subset(meow,ecoregion %in% c('Lancaster Sound'))
} else if (sarea=="Cumberland Sound"){plg<-raster::crop(subset(meow,ecoregion %in% c('Northern Labrador')),extent(-100,-63,65,69))
} else if (sarea=="Foxe Basin (Central Arctic)"){plg<-raster::crop(subset(meow,ecoregion %in% c('Hudson Complex')),extent(-84,-72,65,71))
} else if (sarea=="Hudson Bay-Davis Strait (Central Arctic)"){plg<-raster::crop(subset(meow,ecoregion %in% c('Northern Labrador')),extent(-100,-50,62,80))
##} else if (sarea=="West Jones Sound (High Arctic)"){plg<-raster::crop(subset(meow,ecoregion %in% c('High Arctic Archipelago')),extent(-90,-80,75,77))

} else if (cname=='beluga' & sarea=="Northern Quebec"){plg<-raster::crop(subset(meow,ecoregion %in% c('Northern Labrador')),extent(-100,-63,65,69))

} else if (cname %in% redfish & sarea %in% c("3LN")){plg<-subset(nafos,NAFO_BEST %in% c("3LA", "3LB",  "3LC", "3LD", "3LE", "3LF","3LG","3LH","3LI","3LJ","3LQ","3LR","3LS","3LT","3NA",  "3NB","3NC","3ND","3NE","3NF","3NN")) 
} else if (cname %in% redfish & sarea %in% c("3O")){plg<-subset(nafos,NAFO_BEST %in% c("3OA","3OB","3OC","3OD","3OE","3OF")) 
} else if (cname %in% redfish & sarea %in% c("Unit 1 + 2")){plg<-subset(nafos,NAFO_BEST %in% c('4RA','4RB','4RC','4RD','4SI','4SS','4SV','4SW','4SX','4SY','4SZ',"4TF","4TG", "4TH", "4TJ","4TK","4TL","4TM","4TN","4TO","4TP","4TQ", '3PN','4VN'))
} else if (cname %in% redfish & sarea %in% c("unit 2")){plg<-subset(nafos,NAFO_BEST %in% c("3PSA", "3PSB", "3PSC", "3PSD", "3PSE", "3PSF", "3PSG", "3PSH","4VSB", "4VSC", "4VSE", "4VSV",'4WF','4WG','4WJ','3PN','3PSA','3PSB','3PSC','3PSD','3PSE','3PSF','4VSB','4VSC','4VSE','4VSE','4VSV','3PN','4VN'))
} else if (cname %in% redfish & sarea=="Unit 3"){plg<-subset(nafos,NAFO_BEST %in% c('4WD','4WE','4WH','4WK','4WL','4XL','4XM','4XN','4XO','4XP','4XQ','4XR','4XS','4XX'))

} else if (cname=='bowhead' & sarea=="West Greenland ECWG"){plg<-bowfa

} else if (cname=='narwhal' & sarea=="EHA BB Admiralty Inlet"){plg<-subset(narfa,Name %in% c('Admiralty_Inlet'))
} else if (cname=='narwhal' & sarea=="East Baffin"){plg<-subset(narfa,Name %in% c('East_Baffin_Island'))
} else if (cname=='narwhal' & sarea=="Eclipse Sound"){plg<-subset(narfa,Name %in% c('Eclipse_Sound'))
} else if (cname=='narwhal' & sarea=="Smith/Jones/Parry"){plg<-subset(narfa,Name %in% c('Smith_Sound','Jones_Sound'))
} else if (cname=='narwhal' & sarea=="Somerset"){plg<-subset(narfa,Name %in% c('Somerset_Island'))
##} else if (cname=='narwhal' & sarea=="Northern Hudson Bay"){plg<-subset(narfa,Name %in% c(''))

} else {
  plg<-data.frame(x=NULL,y=NULL)
##print(subset(dt,select=c('cname','sarea')))
}

##IF NO MATCHING AREA, DOES NOT PLOT OR EXPORT COORDINATES
if(dim(plg)[1]==0){
    maps::map('world',lwd=.1)
    outd<-data.frame(stock=stock,
                     sarea=sarea,
                     sname=sname,
                     cname=cname,
                     lon=NA,
                     lat=NA,
                     harm=unique(dt$harm),
                     status=unique(dt$status))
} else {
##print('overlay')
crds2<-SpatialPoints(data.frame(x=crds0.25$lon,y=crds0.25$lat),proj4string=CRS(mct))
#gint<-gIntersection(plg,bathymetry)
#crds.out<-as(gint,'data.frame');names(crds.out)<-c('lon','lat')

#ALTERNATE METHOD WHICH IS FASTER
plgb <- gBuffer(plg, width=.05, byid=TRUE)
gint2<-over(crds2,plgb)
crds.out<-data.frame(lon=crds0.25$lon,lat=crds0.25$lat,area=gint2[,1])
crds.out<-subset(crds.out,is.na(area)==FALSE)
##points(crds.out$lon,crds.out$lat,pch=16,col='red')

#PLOTS SPATIAL BOUNDARIES

plot(plg,axes=TRUE,col='lightblue',lwd=.1,las=1)
plot(plgb,axes=TRUE,col='blue',lwd=.1,las=1,add=TRUE)
maps::map('world',add=TRUE,fill=TRUE,col='gray',lwd=.1)
points(crds.out$lon,crds.out$lat,pch='.',col='red3')
mtext(as.character(stock),3,line=-1,cex=.75)
mtext(paste(cname, gsub(' +','',paste('(',as.character(unique(dt$areacode)),')'))),3,line=0,cex=.75)

#OUTPUTS SPATIAL COORDINATES
  outd<-data.frame(stock=stock,
                   sarea=sarea,
                   sname=sname,
                   cname=cname,
                   lon=crds.out$lon,
                   lat=crds.out$lat,
                   harm=unique(dt$harm),
                   status=unique(dt$status))
}
return(outd)
}

pdf('C:/Users/sailfish/Downloads/pracfs.pdf',height=10,width=8)
par(mfrow=c(4,2))
ot<-ddply(fs,.(stock),.fun=areafun)
dev.off()

a<-subset(ot,is.na(lon)==TRUE)
a<-a[order(a$cname),]
subset(a,select=c('cname','sarea'))

sort(unique(clamfa$Location))

b<-subset(fs,stock %in% a$stock)
subset(b,select=c('cname','sarea','region'))
library(raster)

################################################################
################################################################

##CREATES POLYGON DATABASE OF MANAGEMENT AREAS

################################################################
 
################################################################
##CREATE A NONSENSE SF OBJECT TO START
q <- st_as_sf(sfa2) %>% # select the central parts
st_make_valid() %>%   
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(centrum = T) # return back the data value 
##q<-spTransform(q,CRS(mcrt))
q$stock<-NA
q$sarea<-NA
q$sname<-NA
q$cname<-NA
q$speciesg<-NA
q$region<-NA

stocks<-subset(fs,!(stock %in% c("")))$stock[1:3]
stocks<-subset(fs,!(stock %in% c("")))$stock
##i<-120

##################################################################
##LOOP THROUGH EACH INDIVIDUAL STOCK AND CREATE POLYGON
for(i in 1:length(stocks)){
 dt<-subset(fs,stock %in% stocks[i]) 
  sarea<-ifelse(is.na(dt$sarea)==TRUE,'No',unique(dt$sarea))#SELECTS UNIQUE AREACODE
  cname<-tolower(unique(dt$cname))
  sname<-tolower(unique(dt$sname))
  stock<-unique(dt$stock)
  speciesg<-unique(dt$speciesg)
  print(stock)  

   
pac_grfish<-tolower(c('pacific ocean perch','quillback rockfish','dogfish','yelloweye rockfish'))
crabs<-tolower(c('rock crab','queen / snow crab'))
##  crabs<-tolower(c('Rock crab','Snow Crab','Queen / Snow Crab'))
lobsters<-tolower(c('lobster','american lobster'))
  shrimps<-tolower(c('shrimp','Shrimp','northern shrimp','northern shrimp (borealis)','northern shrimp (montagui)','gulf shrimp','striped shrimp (montagui)'))
scallops<-tolower(c('sea scallop','scallop'))
redfish<-c('redfish','redfish (sebastes fasciatus)','redfish (sebastes mentella)')

#SLICES UP POLYGONS ACCORDING TO NAFO DIVISIONS
  if (sarea=="3NOPs4VWX+5"){plg<-subset(nafo,ZONE %in% c('3N','3O','3Ps','4Vn','4Vs','4W','4X','5Ze','5Zw'))
  } else if (sarea %in% c("Western Atlantic","Northwest Atlantic","North Atlantic","Western Atlantic")){plg<-nafo
  } else if (sarea=="3NOPs"){plg<-subset(nafo,ZONE %in% c('#n','3O','3Ps'))
} else if (sarea=="3NO"){plg<-subset(nafo,ZONE %in% c('3N','3O'))
} else if (sarea=="3LNO"){plg<-subset(nafo,ZONE %in% c('3L','3N','3O'))
} else if (sarea=="3LN"){plg<-subset(nafo,ZONE %in% c('3L','3N'))
} else if (sarea=="2J3KL"){plg<-subset(nafo,ZONE %in% c('2J','3K','3L'))
} else if (sarea=="2J3IKLPs"){plg<-subset(nafo,ZONE %in% c('2J','3I','3K','3L','3Ps'))
} else if (sarea=="2-3KLMNO"){plg<-subset(nafo,ZONE %in% c('2G','2H','2J','3K','3L','3M','3N','3O')) 
} else if (sarea %in% c('2 + 3KLMNO',"2-3KLMNO")){plg<-subset(nafo,ZONE %in% c('2G','2H','2J','3K','3L','3M','3N','3O'))

} else if (sarea=="5Zjm"){plg<-subset(nafo,ZONE %in% c('5Ze','5Zw'))
} else if (sarea=="5Z"){plg<-subset(nafo,ZONE %in% c("5Ze","5Zw"))
} else if (sarea=="5Y, 5Z"){plg<-subset(nafo,ZONE %in% c("5Ze","5Zw",'5Y'))
} else if (sarea=='5'){plg<-subset(nafo,ZONE %in% c("5Ze","5Zw",'5Y'))
} else if (sarea=="4X5Y"){plg<-subset(nafo,ZONE %in% c('4X','5Y'))
} else if (sarea=="4X5"){plg<-subset(nafo,ZONE %in% c('4X','5Ze','5Zw'))
} else if (sarea=="4X"){plg<-subset(nafo,ZONE %in% c("4X"))
} else if (sarea=="4VWX"){plg<-subset(nafo,ZONE %in% c('4Vn','4Vs','4W','4X'))
} else if (sarea=="4TVn"){plg<-subset(nafo,ZONE %in% c('4T','4Vn'))
} else if (sarea=="4T"){plg<-subset(nafo,ZONE %in% c('4T'))
} else if (sarea=="4S"){plg<-subset(nafo,ZONE %in% c('4S'))
} else if (sarea=="4RST"){plg<-subset(nafo,ZONE %in% c('4R','4S','4T'))
} else if (sarea=="4RS-3Pn"){plg<-subset(nafo,ZONE %in% c('4R','4S','3Pn'))
} else if (sarea=="4R"){plg<-subset(nafo,ZONE %in% c('4R'))
} else if (sarea=="3PS"){plg<-subset(nafo,ZONE %in% c('3Ps'))
} else if (sarea=="3Ps"){plg<-subset(nafo,ZONE %in% c('3Ps'))
} else if (sarea=="3O"){plg<-subset(nafo,ZONE %in% c('3O'))
} else if (sarea=="Scotian Shelf (ENS-N)"){plg<-subset(nafo,ZONE %in% c('4W'))
} else if (sarea=="Scotian Shelf (ENS-S)"){plg<-subset(nafo,ZONE %in% c('4X'))
} else if (sarea=="West Greenland (ECWG)"){plg<-subset(nafo,ZONE %in% c('1A','1B','1C','1D','1E','1F'))
} else if (sarea=="NAFO 0A and 0B"){plg<-subset(nafo,ZONE %in% c('0A','0B'))
} else if (sarea=="Atlantic NAFO 3-4"){plg<-subset(nafo,ZONE %in% c('3K','3L','3M','3N','3O','3Pn','3Ps','4R','4S','4T','4Vn','4Vs','4X','4W'))
} else if (sarea=="Grand Bank"){plg<-subset(nafo,ZONE %in% c('3L'))
} else if (sarea=="Gulf"){plg<-subset(nafo,ZONE %in% c('4S','4T','4R'))
} else if (sarea=="Bay of Fundy"){plg<-subset(nafo,ZONE %in% c('4X'))
} else if (is.na(sarea)==TRUE & stock %in% c('Grey Seal')){plg<-subset(nafo,ZONE %in% c('4X','4T','4W','4Vn','4Vs'))
} else if (is.na(sarea)==TRUE & stock %in% c('Elvers')){plg<-subset(nafo,ZONE %in% c('4X','4W','4Vn','4Vs'))

} else if (sarea=='SA2+3KLPs'){plg<-subset(nafo,ZONE %in% c('2J','3K','3K','3L'))

} else if (cname %in% c('sea scallop','scallops') & sarea=="Inshore SFA 28 Bay of Fundy"){plg<-subset(sclfa,ID %in% c('28'))
} else if (cname %in% c('sea scallop','scallops') & sarea=="Inshore SFA 29W"){plg<-raster::crop(subset(sclfa,ID %in% c('29')),extent(-70,-63,39,50))
} else if (cname %in% c('sea scallop','scallops') & sarea=="Offshore SFA 26 German, Browns"){plg<-subset(sclfa,ID %in% c('26'))
} else if (cname %in% c('sea scallop','scallops') & sarea=="Offshore SFA 27, Georges"){plg<-subset(sclfa,ID %in% c('27'))
} else if (cname %in% c('sea scallop','scallops','scallop') & sarea=="SFA 21a, b, c, 22, 23, 24"){plg<-subset(sclfag,Unite_Gest %in% c('21a','22','23','24'))
} else if (cname %in% c('sea scallop','scallops','scallop') & sarea=="Area 20"){plg<-subset(sclfag,Unite_Gest %in% c('20A','20B','20C','20E','20F'))

} else if (cname %in% lobsters & sarea=="Areas 19Offshore LFA 41"){plg<-subset(lfa,ZONE %in% c('41'))

} else if (cname %in% lobsters & sarea=="Offshore LFA 41"){plg<-subset(lfa,ZONE %in% c('41'))
} else if (cname %in% lobsters & sarea=="Inshore LFA 35-38"){plg<-subset(lfa,ZONE %in% c('35','36','37','38'))
} else if (cname %in% lobsters & sarea=="Inshore LFA 34"){plg<-subset(lfa,ZONE %in% c('34'))
} else if (cname %in% lobsters & sarea=="Inshore LFA 27-33"){plg<-subset(lfa,ZONE %in% c('27','28','29','30','31A','31B','32','33'))
} else if (cname %in% lobsters & sarea=="LFA 23, 24, 25, 26A, 26B"){plg<-subset(lfa,ZONE %in% c('23','24','25','26A','26B'))
} else if (cname %in% lobsters & sarea=="LFA 3-14c"){plg<-subset(lfa,ZONE %in% c('3','4','5','6','7','8','9','10','11','12','13','14A','14B','14C'))
} else if (cname %in% lobsters & sarea=="17"){plg<-subset(lfa,ZONE %in% c('17'))
} else if (cname %in% lobsters & sarea=="Areas 19-20-21"){plg<-subset(lfa,ZONE %in% c('19','20A','20B','21'))
} else if (cname %in% lobsters & sarea=="Zone 22 MI"){plg<-subset(lfa,ZONE %in% c('22'))


} else if (cname %in% crabs & sarea=="Areas 19-20-21"){plg<-subset(lfa,ZONE %in% c('19','20A','20B','21'))
} else if (cname %in% crabs & sarea=="Zone 22 (MI)"){plg<-subset(lfa,ZONE %in% c('22'))
} else if (cname %in% crabs & sarea=="Scotian Shelf (ENS-S)"){plg<-subset(cfa,ZONE %in% c('S-ENS'))
} else if (cname %in% crabs & sarea=="Scotian Shelf (ENS-N)"){plg<-subset(cfa,ZONE %in% c('N-ENS'))
} else if (cname %in% crabs & sarea=="LFA 23, 24, 25, 26A"){plg<-subset(lfa,ZONE %in% c('23','24','25','26A'))
} else if (cname %in% c('snow crab') & sarea=="12A"){plg<-subset(scfa,NAME %in% c('12A'))
} else if (cname %in% c('snow crab') & sarea=="12B"){plg<-subset(scfa,NAME %in% c('12B'))
} else if (cname %in% c('snow crab') & sarea=="12C"){plg<-subset(scfa,NAME %in% c('12C'))
} else if (cname %in% c('snow crab') & sarea=="13"){plg<-subset(scfa,NAME %in% c('13'))
} else if (cname %in% c('snow crab') & sarea=="14"){plg<-subset(scfa,NAME %in% c('14'))
} else if (cname %in% c('snow crab') & sarea=="16"){plg<-subset(scfa,NAME %in% c('16A',''))
} else if (cname %in% c('snow crab') & sarea=="16A"){plg<-subset(scfa,NAME %in% c('16A'))
} else if (cname %in% c('snow crab') & sarea=="17"){plg<-subset(scfa,NAME %in% c('17'))
} else if (cname %in% c('snow crab') & sarea=="15"){plg<-subset(scfa,NAME %in% c('15'))
} else if (cname %in% c('snow crab') & sarea=="CFA 12 12, 18, 25, 26, 12E, 12F, 19"){plg<-subset(scfa,NAME %in% c('12','18','25','26','19'))
} else if (cname %in% c('snow crab') & sarea=="ENS-N"){plg<-subset(cfa,Area %in% c('N-ENS'))
} else if (cname %in% c('snow crab') & sarea=="ENS-S"){plg<-subset(cfa,Area %in% c('S-ENS'))

} else if (cname %in% c('sardine','albacore tuna','pacific halibut','yellowmouth rockfish','rougheye rockfish','red sea urchin','pacific hake','canary rockfish','giant red sea cucumber','green sea urchin','longspine thornyhead','sablefish','geoduck','bocaccio','dungeness crab','shrimp trawl','spot prawn','euphausids')){plg<-popfa

} else if (cname %in% c('eel (large)') & is.na(sarea)==TRUE){plg<-subset(bior,label2 %in% c('Scotian Shelf'))

} else if (cname=='surf clam' & sarea=="Banquereau"){plg<-raster::crop(subset(bior,label2 %in% c('Scotian Shelf')),extent(-60,-56,43,45.5))

} else if (cname %in% c('pacific oyster') & is.na(sarea)==TRUE){plg<-popfa

} else if (cname %in% c('pink and spiny scallop')){plg<-subset(pacfa, MGNT_AREA %in% c('13','14','15','16','17','18','19','20','29'))

} else if (cname %in% c('intertidal clams') & sarea %in% c('Central Coast-Heiltsuk Manila')){plg<-subset(clamfa, LABEL %in% c("Heiltsuk JMP"))
} else if (cname %in% c('intertidal clams') & sarea %in% c('North Coast Haida Gwaii Razor')){plg<-subset(clamfa, LABEL %in% c("Haida JMP" ))
} else if (cname %in% c('intertidal clams') & sarea %in% c('South Coast-Vancouver Island')){plg<-subset(clamfa, LABEL %in% c("CMA E"))

} else if (cname %in% pac_grfish & sarea %in% c('PMFC 3CD-WCVI')){plg<-subset(popfa, nms %in% c("3C",'3D'))
} else if (cname %in% pac_grfish & sarea %in% c('PMFC 5ABC-QCS')){plg<-subset(popfa, nms %in% c('5A','5B','5C'))
} else if (cname %in% pac_grfish & sarea %in% c('PMFC 5DE-HS/DE/WHG')){plg<-subset(popfa, nms %in% c('5D','5E'))

} else if (cname %in% pac_grfish & sarea %in% c('Inside','inside','Inside Population')){plg<-subset(popfa, nms %in% c("4B"))
} else if (cname %in% pac_grfish & sarea %in% c('Outside','outside','Outside Population')){plg<-subset(popfa, nms %in% c('3C','3D','5A','5B','5C','5D','5E'))

} else if (cname %in% c('lingcod') & sarea %in% c('Outside')){plg<-subset(popfa, nms %in% c('3C','3D','5A','5B','5C','5D','5E'))

} else if (cname %in% c('pollock') & sarea=="4X5 Western Component"){plg<-subset(pollfa,FID %in% c('84','85','87','81','82','83','94','95'))

} else if (cname %in% c('herring') & sarea=="Central Coast"){plg<-subset(herfa,Assessment %in% c('Central Coast'))
} else if (cname %in% c('herring') & sarea=="Haida Gwaii"){plg<-subset(herfa,Assessment %in% c('HG WEST','HG EAST'))
} else if (cname %in% c('herring') & sarea=="Prince Rupert District"){plg<-subset(herfa,Assessment %in% c('Prince Rupert'))
} else if (cname %in% c('herring') & sarea=="Strait of Georgia Pacific"){plg<-subset(herfa,Assessment %in% c('Strait of Georgia'))
} else if (cname %in% c('herring') & sarea=="WCVI"){plg<-subset(herfa,Assessment %in% c('W.C. Vancouver Is.'))

} else if (cname %in% shrimps & sarea=="Eastern Assessment Zone"){plg<-subset(sfa,ZONE %in% c('2'))
} else if (cname %in% shrimps & sarea=="WAZ"){plg<-subset(sfa,ZONE %in% c('3'))
} else if (cname %in% shrimps & sarea=="SFA 1"){plg<-subset(sfa,ZONE %in% c('1'))
} else if (cname %in% shrimps & sarea=="SFA 2"){plg<-subset(sfa,ZONE %in% c('2'))
} else if (cname %in% shrimps & sarea=="SFA 3"){plg<-subset(sfa,ZONE %in% c('3'))
} else if (cname %in% shrimps & sarea=="SFA 4"){plg<-subset(sfa,ZONE %in% c('4'))
} else if (cname %in% shrimps & sarea=="SFA 5"){plg<-subset(sfa,ZONE %in% c('5'))
} else if (cname %in% shrimps & sarea=="SFA 6"){plg<-subset(sfa,ZONE %in% c('6'))
} else if (cname %in% shrimps & sarea=="SFA 7"){plg<-subset(sfa,ZONE %in% c('7'))
} else if (cname %in% shrimps & sarea=="SFA 8"){plg<-subset(sfa,ZONE %in% c('8'))
} else if (cname %in% shrimps & sarea=="SFA 9"){plg<-subset(sfa,ZONE %in% c('9'))
} else if (cname %in% shrimps & sarea=="SFA 10"){plg<-subset(sfa,ZONE %in% c('10'))
} else if (cname %in% shrimps & sarea=="SFA 12"){plg<-subset(sfa,ZONE %in% c('12'))
} else if (cname %in% shrimps & sarea=="SFA 13-15"){plg<-subset(sfa,ZONE %in% c('13','14','15'))
} else if (sarea=="Baffin Bay (High Arctic)"){plg<-subset(meow,ecoregion %in% c('Baffin Bay - Davis Strait'))
} else if (sarea=="Penny Strait-Lancaster Sound (High Arctic)"){plg<-subset(meow,ecoregion %in% c('Lancaster Sound'))
} else if (sarea=="Cumberland Sound"){plg<-raster::crop(subset(meow,ecoregion %in% c('Northern Labrador')),extent(-100,-63,65,69))
} else if (sarea=="Foxe Basin (Central Arctic)"){plg<-raster::crop(subset(meow,ecoregion %in% c('Hudson Complex')),extent(-84,-72,65,71))
} else if (sarea=="Hudson Bay-Davis Strait (Central Arctic)"){plg<-raster::crop(subset(meow,ecoregion %in% c('Northern Labrador')),extent(-100,-50,62,80))
##} else if (sarea=="West Jones Sound (High Arctic)"){plg<-raster::crop(subset(meow,ecoregion %in% c('High Arctic Archipelago')),extent(-90,-80,75,77))

} else if (cname=='beluga' & sarea=="Northern Quebec"){plg<-raster::crop(subset(meow,ecoregion %in% c('Northern Labrador')),extent(-100,-63,65,69))

} else if (cname %in% redfish & sarea %in% c("3LN")){plg<-subset(nafos,NAFO_BEST %in% c("3LA", "3LB",  "3LC", "3LD", "3LE", "3LF","3LG","3LH","3LI","3LJ","3LQ","3LR","3LS","3LT","3NA",  "3NB","3NC","3ND","3NE","3NF","3NN")) 
} else if (cname %in% redfish & sarea %in% c("3O")){plg<-subset(nafos,NAFO_BEST %in% c("3OA","3OB","3OC","3OD","3OE","3OF")) 
} else if (cname %in% redfish & sarea %in% c("Unit 1 + 2")){plg<-subset(nafos,NAFO_BEST %in% c('4RA','4RB','4RC','4RD','4SI','4SS','4SV','4SW','4SX','4SY','4SZ',"4TF","4TG", "4TH", "4TJ","4TK","4TL","4TM","4TN","4TO","4TP","4TQ", '3PN','4VN'))
} else if (cname %in% redfish & sarea %in% c("unit 2")){plg<-subset(nafos,NAFO_BEST %in% c("3PSA", "3PSB", "3PSC", "3PSD", "3PSE", "3PSF", "3PSG", "3PSH","4VSB", "4VSC", "4VSE", "4VSV",'4WF','4WG','4WJ','3PN','3PSA','3PSB','3PSC','3PSD','3PSE','3PSF','4VSB','4VSC','4VSE','4VSE','4VSV','3PN','4VN'))
} else if (cname %in% redfish & sarea=="Unit 3"){plg<-subset(nafos,NAFO_BEST %in% c('4WD','4WE','4WH','4WK','4WL','4XL','4XM','4XN','4XO','4XP','4XQ','4XR','4XS','4XX'))

} else if (cname=='bowhead' & sarea=="West Greenland ECWG"){plg<-bowfa

} else if (cname=='narwhal' & sarea=="EHA BB Admiralty Inlet"){plg<-subset(narfa,Name %in% c('Admiralty_Inlet'))
} else if (cname=='narwhal' & sarea=="East Baffin"){plg<-subset(narfa,Name %in% c('East_Baffin_Island'))
} else if (cname=='narwhal' & sarea=="Eclipse Sound"){plg<-subset(narfa,Name %in% c('Eclipse_Sound'))
} else if (cname=='narwhal' & sarea=="Smith/Jones/Parry"){plg<-subset(narfa,Name %in% c('Smith_Sound','Jones_Sound'))
} else if (cname=='narwhal' & sarea=="Somerset"){plg<-subset(narfa,Name %in% c('Somerset_Island'))
##} else if (cname=='narwhal' & sarea=="Northern Hudson Bay"){plg<-subset(narfa,Name %in% c(''))
} else { 
  plg<-NULL
}
  
  if(is.null(plg)==FALSE){

##DISSOLVE INTERNAL
library(terra)
    is.valid(plg)
plg<-st_repair_geometry(plg)
 
plg2<-gUnaryUnion(plg)
plg<-buffer(plg,width=0.00001)

  b <- st_as_sf(plg)
  st_is_valid(b)   
  b<-st_make_valid(b,s2_options=sf_use_s2(FALSE))
s2_options=sf_use_s2(FALSE)

b <- st_as_sf(plg2) # select the central parts
b<- st_make_valid(b,s2_options=sf_use_s2(FALSE))   
##  st_buffer(0.05) %>% # make a buffer of half a meter around all parts (to avoid slivers)
b<- st_union(b,by_feature=FALSE) # unite to a geometry object
b<- st_make_valid(b)   
b<- st_sf(b) # make the geometry a data frame object
b<- mutate(b,centrum = T) # return back the data value 

  
  b <- st_as_sf(plg) %>% # select the central parts
  st_make_valid() %>%   
##  st_buffer(0.05) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union(by_feature=FALSE) %>% # unite to a geometry object
  st_make_valid() %>%   
  st_sf() %>% # make the geometry a data frame object
  mutate(centrum = T) # return back the data value 
  sarea<-ifelse(is.na(dt$sarea)==TRUE,'No',unique(dt$sarea))#SELECTS UNIQUE AREACODE
  cname<-tolower(unique(dt$cname))
  sname<-tolower(unique(dt$sname))
  stock<-unique(dt$stock)
  speciesg<-unique(dt$speciesg)
  region<-unique(dt$region)

b$stock<-stock
b$sarea<-sarea
b$sname<-sname
b$cname<-cname
b$speciesg<-speciesg
b$region<-region

##HARMONIZE CRS
st_crs(b)<-st_crs(q)

q<-rbind(q,b)
} else NULL

}

q<-q[-1,]
stockpoly<-q

a<-stockpoly[!st_is_empty(stockpoly),,drop=FALSE]


########################################################################

##IMPORT WORMS TAXONOMIES- NEEDED TO MERGE SPECIES NAMES TO FISHERIES

########################################################################
(load('N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/worms_form.RData'))
worms$synonym<-tolower(worms$synonym)


######################################################################
##IMPORT VULNERABILITY SCORES
(load('N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/vdata_form.RData'))
vdata$spname<-tolower(vdata$spname)
vdata$acceptedname<-tolower(vdata$acceptedname)

a<-unique(subset(vdata,rcp %in% c('8.5'),select=c('acceptedname','kingdom')))
z<-merge(stockpoly,a,by.x='sname',by.y='acceptedname',all.x=TRUE,all.y=FALSE)
zz<-subset(z,is.na(kingdom)==TRUE)

##MERGE WORMS TAXONOMY TO STOCK POLYGON
z<-merge(stockpoly,worms,by.x='sname',by.y='synonym',all.x=TRUE,all.y=FALSE)
zz<-subset(z,is.na(acceptedname)==TRUE)



#####################################################################

##WRITE PORT/SPECIES POLYGONS FOR LATER

#####################################################################
st_write(stockpoly,paste(datadir,'/shapefiles/stocks/stockpoly.shp',sep=''),append=FALSE)
stockpoly<-st_read('N:/data/CAFF/data/shapefiles/stocks/stockpoly.shp')

st_write(stockpoly,paste(datadir,'/shapefiles/stocks/stockpoly_nobuffer.shp',sep=''),append=FALSE)

z<-data.frame(stock=stocks)
a<-subset(z,!(stocks %in% stockpoly$stock))



summary(as.factor(stockpoly$speciesg)) 





###################################################################
###################################################################

##CALCULATE ECOLOGICAL CLIMATE RISK FOR EACH STOCK

###################################################################

######################################################################

##STOCK RISK: LOOP THROUGH STOCKS, OVERLAY FOOTPRINT WITH VULNERABILITY POINTS, CALCULATE VULN PER STOCK
##DEPENDS ON 'RISKFUN' SOURCED EXTERNALLY

#######################################################################
##d<-subset(fleetpoly,fleetc %in% unique(fleetpoly$fleetc)[5])
##d<-subset(portpoly,portc %in% c('0_american plaice'))
stck<-sort(unique(stockpoly$stock))
##prt<-prt[1:10]
l<-list()
for(i in 1:length(prt)){

d<-subset(stockpoly,stock %in% stck[i])  
print(i)
print(unique(d$stock))
##print(class(d))
  
##RETRIEVE VULN FOR SPECIES OF INTEREST
dvuln<-subset(vdata,acceptedname %in% unique(d$acceptedname))

##IF VULNERABILITY AVAILABLE, PROCEED
if(dim(dvuln)[1]>1){
  
##OVERLAY FLEET POLYGON WITH VULNERABILITY MAP
crds.sf<-st_as_sf(dvuln,coords=c('lon','lat'),crs=mcrt)
dti<-st_intersection(d, crds.sf)

##CONVERT TO DATA.FRAME
dvuln<-st_drop_geometry(dti)

##CALCULATE VULN STATS FOR EACH RCP
##b<-subset(dvuln,rcp=='2.6')
f2<-function(b){
##  print(unique(d$stock))
##CELLS WITH >0 MISSING INDEX OF 12
dmiss<-subset(b,is.na(vulnsd)==TRUE)
##PROPORTION OF ALL CELLS THAT ARE MISSING >0 INDEX OF 12
pmiss<-(dim(dmiss)[1]/dim(b)[1])*100

##FVULN CALCULATES AVERAGE VULN/RISK PER SPECIES AND PORT
a<-riskfun(b)

##ADDS ADDITIONAL VARIABLES
a<-cbind(a,data.frame(portc=unique(d$portc),
                      homeport=unique(d$homeport),
                      sciname=unique(d$sciname),
                      spgroup=unique(d$spgroup),
                      spec=unique(d$spec),
                      weight=unique(d$weight),
                      acceptedname=unique(dvuln$acceptedname),
                      speciesid=unique(dvuln$speciesid),
                      comname=unique(dvuln$comname),
                      spname=unique(dvuln$spname)))

return(a)
}
pspecvuln<-ddply(dvuln,.(rcp),.fun=f2)

##IF VULN NOT PRESENT, RETURN ONLY FLEET AND SPECIES
} else {
  pspecvuln<-data.frame(portc=unique(d$portc),
                    acceptedname=unique(d$acceptedname))
}
##return(fvuln)
l[[i]]<-pspecvuln
}

pspecrisk<-rbind.fill(l)

##################################################################

##SAVE PORT LEVEL ECOLOGICAL RISK

###################################################################
save(pspecrisk,file=paste(datadir,'/rfiles/pspecrisk.RData',sep=''))

















ssf_sars<-ddply(fs,.(stock),.fun=areafun2)

z<-dlply(subset(fs,stock %in% unique(fs$stock)[1:3]),.(stock),.fun=areafun2)
zz<-rbind(z)


a<-st_as_sf(sfa)
a<-st_union(a)
options(sf_max.plot=1)
plot(a)

b <- st_as_sf(sfa) %>% # select the central parts
  st_buffer(0.05) %>% # make a buffer of half a meter around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(centrum = T) # return back the data value 
b$stock<-'dan'

d$stock<-'tania'

options(sf_max.plot=1)
plot(b,col='green')
plot(sfa,add=TRUE)

ggplot()+
  geom_sf(data=b,col='green')+
  geom_sf(data=a,col='red')

z<-rbind(b,d)

plg<-sfa
plg$nm<-seq(1,dim(sfa)[1],1)
lu <- data.frame()
lu <- rbind(lu, plg@data)
lu$ICES_area <- 'dan'
lu$Area_km2<- as.character(lu$Area_km2)
lu$country <- NA
lu$country <- as.character(stckid)
plg@data$ICES_area<- as.character(plg@data$ICES_area)
plg@data <- full_join(plg@data, lu, by = "ICES_area")
plg@data <- select(plg@data, -Area_km2.x)



















summary(as.factor(fs$region))

ot<-subset(ot,is.na(lon)==FALSE)
ot$spname<-str_to_sentence(ot$sname)

##save(ot,file=paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','ot_fss.RData',sep=''))
(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','ot_fss.RData',sep='')))


##IMPORT WORMS TAXONOMIES- NEEDED TO MERGE SPECIES NAMES TO FISHERIES
(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','worms_form.RData',sep='')))

ot<-left_join(ot,worms,by=c('spname'='synonym'))

##VULNERABILITY SCORES
(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vdata_form.RData',sep='')))

a1<-ot
a1$rcp<-'2.6'
a2<-ot
a2$rcp<-'8.5'
ot<-rbind(a1,a2)

vdat<-subset(vdata,is.na(acceptedname)==FALSE)
vdatafs<-left_join(ot,vdat,by=c('rcp','acceptedname','lon','lat'))


d<-subset(vdatafs,rcp %in% c('8.5') & stock %in% unique(a2$stock)[1])
plot(d$lon,d$lat,pch=16)
map('world',add=TRUE,fill=TRUE,col='gray20')
b<-subset(d,is.na(vuln)==TRUE)
points(b$lon,b$lat,pch=16,col='red')



####################################################

##FUNCTION CALCULATES VULNERABILITY PER STOCK

####################################################
##d<-subset(vdatafs,is.na(prob)==FALSE & rcp %in% c('2.6') & stock %in% c("American Lobster - LFA 3-14c"))
fvspec<-function(d){
  print(unique(d$stock))
  ##CELLS WITH >0 MISSING INDEX OF 12
  dmiss<-subset(d,is.na(vulnsd)==TRUE)
  ##PROPORTION OF ALL CELLS THAT ARE MISSING >0 INDEX OF 12
  pmiss<-(dim(dmiss)[1]/dim(d)[1])*100

a<-(data.frame(lat=mean(d$lat,na.rm=TRUE),
                      aname=unique(d$acceptedname),
                      pmiss=pmiss,
                      S.HII=mean(d$S.HII,na.rm=TRUE),
                      S.TSMr=mean(d$S.TSMr,na.rm=TRUE),
                      S.vind=mean(d$S.vind,na.rm=TRUE),
                      S.rlstatus=mean(d$S.rlstatus,na.rm=TRUE),
                      E.nrchng=mean(d$E.nrchng),
                      E.plost=mean(d$E.plost,na.rm=TRUE),
                      E.toe=mean(d$E.toe,na.rm=TRUE),
                      E.vel=mean(d$E.vel,na.rm=TRUE),
                      AC.hfrag=mean(d$AC.hfrag,na.rm=TRUE),
                      AC.hrange=mean(d$AC.hrange,na.rm=TRUE),
                      AC.lmax=mean(d$AC.lmax,na.rm=TRUE),
                      AC.tvar=mean(d$AC.tvar,na.rm=TRUE),
                      adcap=wtd.mean(d$adcap,w=1/(d$adcap.sd/d$adcap),na.rm=TRUE),
  sens=wtd.mean(d$sens,w=1/(d$sens.sd/d$sens),na.rm=TRUE),
  expo=wtd.mean(d$expo,w=1/(d$expo.sd/d$expo),na.rm=TRUE),
  vuln=wtd.mean(d$vuln,w=1/(d$vulnsd/d$vuln),na.rm=TRUE),
  adcapsd=sqrt(wtd.var(d$adcap,w=1/(d$adcap.sd/d$adcap),na.rm=TRUE)),
  senssd=sqrt(wtd.var(d$sens,w=1/(d$sens.sd/d$sens),na.rm=TRUE)),
  exposd=sqrt(wtd.var(d$expo,w=1/(d$expo.sd/d$expo),na.rm=TRUE)),
  vulnsd=sqrt(wtd.var(d$vuln,w=1/(d$vulnsd/d$vuln),na.rm=TRUE)),
  vuln.m=mean(d$vuln.uw,na.rm=TRUE),
  n=dim(d)[1],
##  tl=unique(d$tl),
##  importance=unique(d$importance),
##  importancen=unique(d$importancen),
##  importancec=unique(d$importancec),
  lmax=unique(d$lmax),
  harm=unique(d$harm),
  status=unique(d$status)))
}
vspecfs<-ddply(subset(vdatafs,is.na(prob)==FALSE),.(rcp,stock),.fun=fvspec,.progress='text')

vspecfs$statusf<-factor(vspecfs$status,levels=c("uncertain","critical","cautious","healthy"))
vspecfs$harmf<-factor(vspecfs$harm,levels=c("Serious harm unlikely", "Serious harm possible", "Serious harm likely"))


##SAVES FILE FOR USE IN FIGURES
save(vspecfs,file=paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecfs.RData',sep=''))


#####
#####
#####
tapply(a$vuln,a$harm,length)

a<-subset(vspecfs,rcp %in% c('8.5'))
plot(a$statusf,a$vuln,pch=15)
plot(a$harmf,a$vuln,pch=15)
plot(a$statusf,a$sens,pch=15)
plot(a$statusf,a$expo,pch=15)
plot(a$statusf,a$adcap,pch=15)

a<-subset(vspecfs,rcp %in% c('2.6'))
plot(a$statusf,a$vuln,pch=15)

f<-function(d){
  dd<-subset(d,select=c("S.TSMr","S.rlstatus","S.vertrange", "S.depthmax","S.HII", "AC.tauc", "AC.trng", "AC.hfrag","AC.rarea",  "AC.lrange", "AC.lmax", "E.toe", "E.vel", "E.plost", "E.nrchng", "AC.hrange", "AC.tvar", "S.vind","sens", 'vuln'))

  z<-colMeans(dd,na.rm=TRUE)

'))return(data.frame())
}


vdata8<-subset(vdata,acceptedname %in% ot$acceptedname & rcp %in% c('8.5'))

length(unique(aa$acceptedname))
length(unique(a$acceptedname))
length(unique(ot$sname))

b<-unique(subset(vdata2,select=c('acceptedname','lon','lat')))
b<-unique(subset(vdata2,select=c('speciesid','acceptedname','lon','lat')))
dt<-data.frame(speciesid=sort(unique(b$speciesid)),
               n=tapply(b$acceptedname,b$speciesid,function(x) length(unique(x))))

dt<-data.frame(acceptedname=sort(unique(b$acceptedname)),
               n=tapply(b$speciesid,b$acceptedname,function(x) length(unique(x))))

d<-subset(ot2,stock %in% unique(ot2$stock)[1])
f<-function(d){
dt<-subset(vdata,acceptedname %in% unique(d$acceptedname))

subset(fs,cname %in% c('Lobster','American Lobster'),select=c('sarea'))

plot(cfa)
text(cfa,labels=cfa$Area)

plot(sfa)
text(sfa,labels=sfa$ZONE,col='red')

par(mfrow=c(2,2))
plot(sfa)
text(sfa,labels=sfa$ZONE,col='red')

plot(lfa)
text(lfa,labels=lfa$ZONE,col='red')

sfa2<-readOGR('N:/data/shapefiles/DFO_fishing_areas/shrimp','SFAs_PANOMICS_Fall2020')
##names(sfa2)<-tolower(names(sfa2))
plot(sfa2)
text(sfa2,labels=sfa2$ZONE,col='red')

cfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/snow_crab','dfo_east_snow_crab_areas_erase')
cfa$NAME<-c("17","16","12A","15","16A","12C","12B","12E","12F","14","13","3A","3B","3C","3D","4","5","7A","7B","6A","6B","6C","7C","9","8","10","11","23","22","21","20","19","24","4X","D","12")
plot(cfa)
text(cfa,labels=cfa$NAME,col='red')

plg<-readOGR('N:/data/shapefiles/meow/MEOW','meow_ecos')
meow<-subset(plg,PROVINCE %in% c("Cold Temperate Northwest Atlantic",'Arctic'))
names(meow)<-tolower(names(meow))
plot(meow,border='red')
map('world',add=TRUE,fill=TRUE,col='gray80')
text(meow,labels=meow$ecoregion,col='red',adj=0,cex=.75)
map.axes()
abline(h=75)
abline(h=77)
abline(v=-90)
abline(v=-80)

sclfa<-readOGR('N:/data/shapefiles/DFO_fishing_areas/scallop','ScallopFishingAreas_Trim')
plot(sclfa)
text(sclfa,labels=sclfa$ID,col='red')

plot(a,col='green',add=TRUE)
a<-subset(meow,name %in% c('Lancaster Sound'))

        cname         sarea
1 Capelin SA2+3KLPs
              cname    sarea
1 Icelandic Scallop 16EF-18A
    cname                  sarea
1 Lobster Areas 19-20-21 (Gasp )
    cname        sarea
1 Lobster Zone 22 (MI)
    cname                    sarea
                       cname                   sarea
1 Northern Shrimp (Borealis) Eastern Assessment Zone
                       cname sarea
1 Northern Shrimp (Borealis)   WAZ
                       cname sarea
1 Northern Shrimp (Montagui)   WAZ
              cname    sarea
1 Queen / Snow Crab CFA 1-12
    cname  sarea
1 Redfish Unit 1
    cname  sarea
1 Redfish Unit 2
    cname  sarea
1 Redfish Unit 3
    cname                     sarea
1 Scallop SFA 21a, b, c, 22, 23, 24
        cname   sarea
1 Sea Scallop Area 20
        cname          sarea
1 Sea Scallop Inshore SFA 28
        cname           sarea
1 Sea Scallop Inshore SFA 29W
        cname           sarea
1 Sea Scallop Offshore SFA 26
        cname           sarea
1 Sea Scallop Offshore SFA 27
                      cname                   sarea
1 Striped Shrimp (Montagui) Eastern Assessment Zone
      cname      sarea
1 Surf Clam Banquereau



  unique(subset(fs,cname %in% c('Shrimp','Northern Shrimp','Northern Shrimp (Borealis)','Northern Shrimp (Montagui)'),select=c('sarea')))


##CRAB AREAS
  30  CFA 12 (12, 18, 25, 26), 12E, 12F, 19
  57                                     4X
  86                               CFA 1-12
  167                                   12A
  168                                   12B
  169                                   12C
  170                                    13
  171                                    14
  172                                    15
  173                                    16
  174                                   16A
  175                                    17

  unique(subset(fs,cname %in% c('Rock Crab','Snow Crab','Queen / Snow Crab'),select=c('sarea')))


  55                      Shrimp                                  SFA 13-15
  67             Northern Shrimp                                      SFA 5
  68             Northern Shrimp                                      SFA 6
  69             Northern Shrimp                                      SFA 7
  70                      Eastern Assessment Zone
  71  Northern Shrimp (Borealis)                                      SFA 1
  72  Northern Shrimp (Borealis)                                      SFA 4
  73  Northern Shrimp (Borealis)                                        WAZ
  74                                        SFA 4
  75  Northern Shrimp (Montagui)                                        WAZ
  76   Striped Shrimp (Montagui)                    Eastern Assessment Zone

29                     Scallop                  SFA 21a, b, c, 22, 23, 24
51                 Sea Scallop                             Inshore SFA 28
52                 Sea Scallop                            Inshore SFA 29W
53                 Sea Scallop                            Offshore SFA 26
54                 Sea Scallop                            Offshore SFA 27
61                   Surf Clam                                 Banquereau
62                   Surf Clam                                 Grand Bank
92                       Whelk                                        3PS
161          Icelandic Scallop                                   16EF-18A
166                Sea Scallop                                    Area 20




 [1] "01ABCDEF"     "1"            "23K"          "23KLMNO"      "2G-3K"
 [6] "2HJ"          "2HJ3KLNOP4R"  "2J"           "2J3K"         "2J3K-3LNO"
[11] "2J3KL"        "2J3KLNOPs"    "3K"           "3LN"          "3LNO"
[16] "3LNO-UT12"    "3LNOPs"       "3M"           "3NO"          "3NOPs"
[21] "3NOPs4VWX5Zc" "3Pn4RS"       "3Pn4RSTVn"    "3Ps"          "4R3Pn"
[26] "4RFS"         "4RS"          "4RSS"         "4RST"         "4S"
[31] "4SFS"         "4SSS"         "4T"           "4TFS"         "4TSS"
[36] "4VsW"         "4VWX"         "4VWX5"        "4X"           "4X5Y"
[41] "4X5Yb"        "5Z"           "ATL"          "BANQ"         "LFA15-18"
[46] "LFA19-21"     "LFA22"        "LFA23-26AB"   "LFA27-33"     "LFA3-14"
[51] "LFA34"        "LFA35-38"     "LFA41"        "NBB"          "NFLDESC"
[56] "NWATLSA3-4"   "Q4RST"        "QCW"          "SCFA16-20"    "SCMA12-17"
[61] "SFA13-15"     "SFA2-3"       "SFA4"         "SFA5"         "SFA6"
[66] "SPA1-6"       "UT1"          "UT12"         "UT2"          "UT3"
[71] "WSFA29"
