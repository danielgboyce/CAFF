##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir,'GPARAMs.r',sep=''))

 
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

##SHORTEN NAMES
names(data)<-ifelse(names(data)%in% c('longitude'),'lon',names(data))
names(data)<-ifelse(names(data)%in% c('latitude'),'lat',names(data))


########################################################################
##LOOK INTO WHICH SPECIES ARE MISSING LOCATION COORDS (MOSTLY SEA RAVEN, SEAWEED, SEALS)
f<-function(){
  a<-data
a$loc<-ifelse(is.na(a$lon)==TRUE | is.na(a$lat)==TRUE,0,1)
f<-function(x){
  return(data.frame(pmiss=(dim(subset(x,loc==0))[1]/dim(x)[1])*100))
}
b<-ddply(a,.(species),.fun=f,.progress='text')
b<-b[order(b$pmiss,decreasing=TRUE),]

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
b<-left_join(b,spec,by=c('species'='species_code'))
}



##CALCULATE TOTAL CATCH FOR EACH PORT OF LANDING AND HOME PORT
f<-function(d){
  return(data.frame(weight=sum(d$weight,na.rm=TRUE)))
}
dlport<-ddply(data,.(portland),.fun=f,.progress='text')
dlport<-subset(dlport,is.na(portland)==FALSE & weight>0)##2285 LANDINGS PORTS
dhport<-ddply(data,.(homeport),.fun=f,.progress='text')
dhport<-subset(dhport,is.na(homeport)==FALSE & weight>0)##1545 HOME PORTS
save(dlport,file=paste(datadir,'/rfiles/ZIFF_portland_full_nofilter.RData',sep=''))
save(dhport,file=paste(datadir,'/rfiles/ZIFF_homeport_full_nofilter.RData',sep=''))
(load(paste(datadir,'/rfiles/ZIFF_portland_full_nofilter.RData',sep='')))
(load(paste(datadir,'/rfiles/ZIFF_homeport_full_nofilter.RData',sep='')))
 
##PLOT ON MAP
##a<-unique(subset(data,select=c('lon','lat')))
##library(maps)
##map('world',xlim=c(-80,-40),ylim=c(30,90))
##map.axes()
##points(a$lon,a$lat,pch='.',col='red')

a<-unique(subset(dat,select=c('lon','lat')))
library(maps)
map('world',xlim=c(-80,-40),ylim=c(30,90))
map.axes()
points(a$lon,a$lat,pch='.',col='red')
plot(a$lon,a$lat,pch=16,col='red')


##REVERSE BUFFER COASTLINE TO AVOID OMITTING OBS VERY CLOSE TO SHORELINE
cstb<-as(
  spTransform(
    gBuffer(
      as(cst,'Spatial')
      ,width=-0.05)
    , mcrt)
  , 'sf')

##plot(st_geometry(cst),col='red',xlim=c(-70,-60),ylim=c(40,50))
##plot(st_geometry(cstb),add=TRUE,col='blue')


###########################################################

##LOOP THROUGH YEARS, OVERLAY COASTLINE MAP TO REMOVE OBS ON LAND

############################################################
##REMOVE OBS WITH MISSING LON OR LAT
data<-subset(data,is.na(lon)==FALSE & is.na(lat)==FALSE)
##d<-subset(data,year==2010)
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
##    gint<-st_difference(cst2,crds_sf)
    gint<-st_intersects(cstb,crds_sf,sparse=FALSE)
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
dat<-subset(dat,select=names(data))
dat<-anti_join(data,dat)##48745

##BOX TO ELIMINATE IMPOSSIBLE POINTS
dat<-subset(dat,(lon<=-30 & lon>=-80) & (lat>=30 & lat<=80))

##dim(data)[1]-dim(a)[1]
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
###############################################
#ADD SPECIES NAMES

##SPECIES NAMES FROM MARFIS
spec<-read.csv('N:/data/landings/MARFIS/MARFIS_Spp_Abbrev.csv',header=TRUE,encoding="UTF-8")
names(spec)<-tolower(names(spec))
spec<-na.omit(subset(spec,select=c('species_code','category_desc','scientif','species')))
names(spec)<-c('species_code','spgroup','sciname','spec')

##SPECIES NAMES FROM ZIFF
spec2<-read.csv('N:/data/landings/DFO_logbooks/ZIFF/speciescodes.csv',header=TRUE,encoding="UTF-8")
names(spec2)<-c('species_code','spec.raw')

##JOIN MARFIS AND ZIFF SPECIES CODES
spec<-left_join(spec2,spec,by=c('species_code'))
##z<-spec$spec.raw[17]

##FIX ODD CHARACTERS
spec$spec.raw<-str_replace_all(spec$spec.raw,"\\\\x96","")
spec$spec.raw<-str_replace_all(spec$spec.raw,'�','')

##FORMAT TEXT STRINGS
spec$spec<-gsub('  ',' ',spec$spec)
spec$spec.raw<-gsub('  ',' ',spec$spec.raw)
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
spec$sciname<-ifelse(spec$spec.raw %in% c('skate, barndoor'),'Dipturus laevis',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('butter fish/dollarfish'),'Peprilus triacanthus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('windowpane flounder'),'Scophthalmus aquosus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('grenadier, rough-head'),'Macrourus berglax',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('hake, blue'),'Macruronus novaezelandiae',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('shark, thresher'),'Alopias vulpinus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('northern wolffish'),'Anarhichas denticulatus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('shark, dusky'),'Carcharhinus obscurus',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('clam, propeller'),'Cyrtodaria siliqua',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('clam, stimpsons surf'),'Mactromeris polynyma',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('cockle'),'Dinocardium robustum',spec$sciname)
spec$sciname<-ifelse(spec$spec.raw %in% c('arctic wedgeclam'),'Donax hanleyanus',spec$sciname)


##spec$spec<-ifelse(spec$spec.raw %in% c('capelin'),'capelin',spec$spec)

##LOWER CASE FOR VARIABLES
spec$spgroup<-tolower(spec$spgroup)
spec$sciname<-tolower(spec$sciname)

##a<-subset(spec,species_code %in% unique(dat$species_code))

##STANDARDIZE JOIN KEY ('SPECIES_CODE')
names(dat)<-ifelse(names(dat) %in% c('species'),'species_code',names(dat))
##JOIN SPECIES INFO
dat<-merge(dat,spec,by=c('species_code'),all.x=TRUE,all.y=FALSE)

dat$sciname<-gsub(' s.c.','',dat$sciname)
dat$sciname<-gsub(' \\(obsolete)','',dat$sciname)

######################################################
##RETAIN VARIABLES OF INTEREST TO THE ANALYSIS
dat<-subset(dat,select=c('species_code','year','month','day','nafodiv','portland','homeport','lon','lat','eez','gearcat','gearcode','lclass','fleet','spgroup','sciname','spec','loa','weight','value'))

##ADD FISHING GEAR TYPES
gear<-read.csv('N:/data/landings/DFO_logbooks/ZIFF/gearcodes.csv',header=TRUE,skip=1)
names(gear)<-tolower(names(gear))
gear<-subset(gear,!(gear.code==''))
dat<-merge(dat,gear,by=c('gearcode'),all.x=TRUE,all.y=FALSE)

##ADD LENGTH CLASS FOR FLEET
dat$lenclass<-ifelse(dat$loa<=35,'sml','med')
dat$lenclass<-ifelse(dat$loa>=100,'lrg',dat$lenclass)


##ADD TSNS
zif.taxon<-unique(subset(dat,is.na(sciname)==FALSE & sciname!='',select=c('sciname','spec')))
zif.taxon<-cbind(zif.taxon,data.frame(get_tsn(zif.taxon$sciname,accepted=TRUE,ask=TRUE)))
zif.taxon<-zif.taxon[,1:3]
##TRY TO RETRREIVE MISSING TSNS BASED ON COMMON NAME
zif.taxon$ids<-ifelse(zif.taxon$spec %in% c('marlin white'),get_tsn('white marlin',accepted=TRUE)[1],zif.taxon$ids)
zif.taxon$ids<-ifelse(zif.taxon$spec %in% c('tuna, skipjack'),get_tsn('skipjack tuna',accepted=TRUE)[1],zif.taxon$ids)
dat<-left_join(dat,unique(subset(zif.taxon,select=c('sciname','ids'))),by=c('sciname'))


##################################################################

##SAVE DATA FOR LATER USE

##################################################################
save(dat,file=paste(datadir,'/rfiles/ZIFF_form.RData',sep=''))
##(load(paste(datadir,'/rfiles/ZIFF_form.RData',sep='')))

plot(dat$lon,dat$lat,pch='.')
map('world',add=TRUE,fill=TRUE,col='gray80')

wcm<-st_read('N:/data/shapefiles/naturalearthdata_ne_50m_land_poly/ne_50m_land.shp')


dt<-subset(dat,spec %in% c('haddock','herring','marlin blue','pollock','tuna, bluefin','cod','mackerel','red hake','sculpin','shad','silver hake','summer flounder','crab, snow','capelin','halibut','lobster','redfish','shrimp, pandalus borealis','swordfish','white hake'))

dt$lon<-round(dt$lon,digits=1)
dt$lat<-round(dt$lat,digits=1)

f<-function(d){
  return(data.frame(weight=sum(d$weight,na.rm=TRUE)))
}
dt<-ddply(dt,.(spec,lon,lat),.fun=f,.progress='text')

xlm2<-c(-80,-45)
ylm2<-c(40,70)
xlm2<-c(-70,-50)
ylm2<-c(40,50)

library(pals)
ggplot()+
  geom_raster(data=dt,aes(x=lon,y=lat,fill=weight))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
scale_fill_gradientn(colors=kovesi.rainbow(100),trans='log10')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
          legend.position="right",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(1, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="white",fill=NA,size=0.5))+
  xlab('')+
  ylab('')+
  facet_wrap(~spec,ncol=7)




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






