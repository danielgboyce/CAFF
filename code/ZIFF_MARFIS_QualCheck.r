##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir,'GPARAMs.r',sep=''))
library(ggplot2)
library(plyr)
library(maps)
library(scales)
library(data.table)
library(raster)
library(maps)
library(plyr)
library(lubridate)
library(sp)
library(rgdal)
library(maptools)
library(dplyr)
library(raster)
library(tidyr)
library(biogeo)

#HIGH RESOLUTION COASTLINE
##cst<-readOGR('N:/data/shapefiles/naturalearthdata_ne_10m_ocean/v4.1',layer='ne_10m_ocean')
##cst<-crop(cst,extent(-90,-30,35,80),proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
cst<-st_read('N:/data/shapefiles/naturalearthdata_ne_10m_ocean/v4.1/ne_10m_ocean.shp')
cst<-st_transform(cst,mcrt)
cst<-st_make_valid(cst)
cst<-st_crop(cst,c(xmin=-90,xmax=-30,ymin=35,ymax=80))
##plot(st_geometry(cst))

##crds<-unique(subset(data,select=c('lon','lat')))
##plot(crds$lon,crds$lat,pch='.',col=as.character(crds$cl))
##map('world',add=TRUE,border=TRUE,col='green')
##crds$cl<-ifelse(crds$lon>0,'red','blue')
##crds$lon<-ifelse(crds$lon>0,crds$lon*-1,crds$lon)
##points(crds$lon,crds$lat,pch=16,cex=0.5)


#############################################################

##IMPORT ZIFF LANDINGS DATA YEAR BY YEAR

##############################################################
setwd('N:/data/landings/DFO_logbooks/ZIFF/raw')
fls<- list.files(pattern = '\\.csv')
##data<-read.csv(fls[1],header=TRUE)

l<-list()
for(i in 1:length(fls)){
  d<-fread(fls[i],header=TRUE)
  names(d)<-tolower(names(d))
names(d)<-ifelse(names(d) %in% c('lat'),'latitude',names(d))
names(d)<-ifelse(names(d) %in% c('long'),'longitude',names(d))
names(d)<-ifelse(names(d) %in% c('mainsps'),'mainspst',names(d))
print(dim(d))
print(sort(names(d)))
l[[i]]<-d
}
data<-data.frame(rbindlist(l,use.names=TRUE))
  
##SHORTEN COORDINATE NAMES
names(data)<-ifelse(names(data)%in% c('longitude'),'lon',names(data))
names(data)<-ifelse(names(data)%in% c('latitude'),'lat',names(data))




(load(paste(datadir,'/rfiles/ZIFF_form.RData',sep='')))
a<-subset(dat,spec=='mackerel')
a<-subset(dat,spec=='herring')
a$lat<-round(a$lat,digits=1)
a$lon<-round(a$lon,digits=1)

library(viridis)
xlm2<-c(-60,-45)
ylm2<-c(45.75,54)
ggplot()+
  geom_tile(data=a,aes(x=lon,y=lat,fill=weight),color='black')+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
scale_fill_gradientn(colors=kovesi.rainbow(100),name='')

  


datadir<-'N:/data/landings/MARFIS/'
figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/MPA_pseudo_mpas/figs'


#######################################################
#READ IN AND FORMAT MARFIS LANDINGS DATA (2000 ONWARD)
#UNITS ARE IN KGS
setwd(datadir)
mdata<-fread('pro_spec_info.csv',header=TRUE)
names(mdata)<-tolower(names(mdata))
dt<-strptime(mdata$landed_date,"%y-%m-%d")
mdata$year<-as.numeric(substr(dt,1,4))

lond<-substr(mdata$longitude,1,2)
lonm<-substr(mdata$longitude,3,4)
lons<-substr(mdata$longitude,6,6)
mdata$lon<-dms2dd(as.numeric(lond),as.numeric(lonm),as.numeric(lons),'W')

latd<-substr(mdata$latitude,1,2)
latm<-substr(mdata$latitude,3,4)
lats<-substr(mdata$latitude,6,6)
mdata$lat<-dms2dd(as.numeric(latd),as.numeric(latm),as.numeric(lats),'N')





datadir<-'N:/data/landings/MARFIS'
setwd(datadir)
(load('marfis_2013_oc.RData'))
mar<-d
length(unique(mar$mon_doc_id))
length(unique(mar$community_code))
length(unique(mar$sum_doc_id))




f<-function(d){
  return(data.frame(weight=sum(d$weight,na.rm=TRUE)))
}
dlport<-ddply(data,.(portland),.fun=f,.progress='text')
dlport<-subset(dlport,is.na(portland)==FALSE & weight>0)
dhport<-ddply(data,.(homeport),.fun=f,.progress='text')
dhport<-subset(dhport,is.na(homeport)==FALSE & weight>0)
save(dlport,file=paste(datadir,'/rfiles/ZIFF_portland_full_nofilter.RData',sep=''))
save(dhport,file=paste(datadir,'/rfiles/ZIFF_homeport_full_nofilter.RData',sep=''))

###########################################################

##LOOP THROUGH YEARS, OVERLAY COASTLINE MAP TO REMOVE OBS ON LAND

############################################################
f<-function(d){
    d$lon<-ifelse(d$lon>=0,d$lon*-1,d$lon)
    gc()
    n1<-dim(d)[1]
    crds<-na.omit(unique(subset(d,select=c('lon','lat'))))
##CONVERT TO SF OBJECT
    crds_sf<- crds %>%
  mutate_at(vars(lon,lat),as.numeric) %>%
  st_as_sf(coords=c('lon','lat'),crs = mcrt,stringsAsFactors = FALSE)
##    crds<-SpatialPoints(data.frame(lon=dat$lon,lat=dat$lat),proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
##    gint<-over(crds,cst)
##    dat$dum<-gint[,1]
    gint<-st_intersects(cst,crds_sf,sparse=FALSE)
##crds<-subset(data,select=c('lon','lat'))
    crds$gint<-t(gint)
    crds<-subset(crds,gint==TRUE)
    d<-left_join(d,crds,by=c('lon','lat'))
    d<-subset(d,is.na(gint)==FALSE)
    d$nmissing<-((n1-dim(d)[1])/n1)*100
##    save(d,file=paste(gsub(' ','',paste('ziff_',unique(d$year),'_oc.RData'))))
    return(d)
    gc()
}
z<-dlply(data,.(year),.fun=f,.progress='text')
dat<-rbindlist(z)

spc<-615
m<-subset(dat,species==spc)
dim(m)

##REMOVE MISSING SPECIES (404) OR MISSING GEAR (1145) OR WEIGHT==0 (11000)
dat<-subset(dat,is.na(species)==FALSE & 
              !(gclass==0) &
              weight>0)
 
##RETAIN OBSERVATIONS WHERE SPECIES CAUGHT ARE THE SAME AS THE SPECIES SOUGHT
dat<-subset(dat,species==mainspst)

m<-subset(dat,species==spc)
dim(m)

##CREATE CODE FOR EEZ AND GEAR
dat$eez<-ifelse(dat$limcode==0,'eez','hsea')
dat$gearcat<-ifelse(dat$gclass==1,'fix','mob')

##ADD VARIABLES FOR MONTH, DAY
dat$month<-as.numeric(substr(dat$ctchdate,5,6))
dat$day<-as.numeric(substr(dat$ctchdate,7,8))

names(dat)<-ifelse(names(dat) %in% c('species'),'species_code',names(dat))

###############################################
#ADD SPECIES NAMES

##SPECIES NAMES FROM MARFIS
spec<-read.csv('N:/data/landings/MARFIS/MARFIS_Spp_Abbrev.csv',header=TRUE)
names(spec)<-tolower(names(spec))
spec<-na.omit(subset(spec,select=c('species_code','category_desc','scientif','species')))
names(spec)<-c('species_code','spgroup','sciname','spec')

##SPECIES NAMES FROM ZIFF
spec2<-read.csv('N:/data/landings/DFO_logbooks/ZIFF/speciescodes.csv',header=TRUE)
names(spec2)<-c('species_code','spec.raw')

##JOIN MARFIS AND ZIFF SPECIES CODES
spec<-left_join(spec2,spec,by=c('species_code'))

spec$spec<-tolower(spec$spec)
spec$spec.raw<-tolower(spec$spec.raw)
spec$spec<-trimws(spec$spec,which='both')

##FORMAT MISSING/INCORRECT NAMES TO ENABLE JOIN WITH SSF - SCIENTIFIC NAMES NEED TO MATCH
spec$sciname<-ifelse(spec$spec.raw %in% c('eels','elvers'),'Anguilla rostrata',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('clams, stimpson surf'),'Mactromeris polynyma',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('sea cucumber'),'Cucumaria frondosa',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('scallop, icelandic'),'chlamys islandica',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('scallop, sea'),'placopecten magellanicus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('whelk'),'Buccinum undatum',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('skate'),'amblyraja radiata',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('capelin'),'mallotus villosus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('arctic charr'),'Salvelinus alpinus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('dogfish'),'Squalus acanthias',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('salmon, atlantic'),'Salmo salar',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('beaked redfish'),'Sebastes mentella',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('acadian redfish','redfish'),'Sebastes fasciatus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c("clam, stimpson's surf","clams, stimpson's surf, mantle"),'Mactromeris polynyma',spec$sciname)

spec$spec<-ifelse(spec$spec.raw %in% c('capelin'),'capelin',spec$spec)

##LOWER CASE FOR VARIABLES
spec$spgroup<-tolower(spec$spgroup)
spec$sciname<-tolower(spec$sciname)

##STANDARDIZE JOIN KEY ('SPECIES_CODE')
names(dat)<-ifelse(names(dat) %in% c('species'),'species_code',names(dat))
##JOIN
dat<-merge(dat,spec,by=c('species_code'),all.x=TRUE,all.y=FALSE)

######################################################
##RETAIN VARIABLES OF INTEREST TO THE ANALYSIS
dat<-subset(dat,select=c('species_code','year','month','day','nafodiv','portland','homeport','lon','lat','eez','gearcat','gearcode','lclass','fleet','spgroup','sciname','spec','loa','weight'))

##ADD FISHING GEAR TYPES
gear<-read.csv('N:/data/landings/DFO_logbooks/ZIFF/gearcodes.csv',header=TRUE,skip=1)
names(gear)<-tolower(names(gear))
gear<-subset(gear,!(gear.code==''))
dat<-merge(dat,gear,by=c('gearcode'),all.x=TRUE,all.y=FALSE)

##ADD LENGTH CLASS FOR FLEET
dat$lenclass<-ifelse(dat$loa<=35,'sml','med')
dat$lenclass<-ifelse(dat$loa>=100,'lrg',dat$lenclass)

##################################################################

##SAVE DATA FOR LATER USE

##################################################################
save(dat,file=paste(datadir,'/rfiles/ZIFF_form.RData',sep=''))
##(load(paste(datadir,'/rfiles/ZIFF_form.RData',sep='')))



##EXTRA CODE

###########################################################################
###########################################################################
###########################################################################

##FUNCTION CALCULATES BIOMASS OF EACH SPECIES LANDED IN EACH PORT FOR EACH FLEET

############################################################################
d<-subset(datt,fleetc %in% unique(datt$fleetc)[1])
f2<-function(d){
print(unique(d$fleetc))
return(data.frame(portland=sort(unique(d$portland)),
                  weight=tapply(d$weight,d$portland,function(x) sum(x,na.rm=TRUE))))
}
fcivi<-ddply(datt,.(fleetc),.fun=f2,.progress='text')



f2<-function(d){
return(data.frame(weight=sum(d$weight,na.rm=TRUE)))
}
a<-ddply(datt,.(spec,gearcat,lenclass),.fun=f2,.progress='text')
a$lweight<-log10(a$weight+1)

ggplot()+
  geom_tile(data=a,aes(x=lenclass,y=gearcat,fill=weight),color='white')+
##  scale_fill_manual(values=tol.rainbow(100))+
  facet_wrap(~spec)




a<-subset(fs,!(sname %in% unique(dat$sciname)))
b<-unique(subset(a,!(region %in% c('Pacific')),select=c('cname','sname')))
b<-b[order(b$sname),]

m<-subset(dat,species_code==spc)
dim(m)

m<-subset(dat,spec=='eels')
m<-subset(dat,species_code==352)
unique(subset(fs,select=c('cname','sname')))
buccinum undatum  
buccinum undatum
a<-subset(dat,sciname %in% unique(fs$sname))
length(unique(a$sciname))






