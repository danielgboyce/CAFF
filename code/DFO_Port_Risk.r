
##############################################################################

##CALCULATES FISHING FOOTPRINT FOR EACH HARBOUR FOR EACH SPECIES AND THEN CALCULATES ECOLOGICAL VULNERABILITY

###############################################################################

##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir,'GPARAMs.r',sep=''))


##LOAD FORMATTED DATA ON THRESHOLDS
##LOAD FORMATTED DATA ON THRESHOLDS
(load(paste("N:/data/CC_vulnerability/data/",'analysis_outputs_local/one_deg/figures_data/','thdats_form.RData',sep='')))

##################################################################
##LOAD FORMATTED ZIFF LANDINGS DATA (FROM 'ZIFF_import.r')
(load(paste(datadir,'/rfiles/ZIFF_form.RData',sep='')))


###################################################################
##IMPORT FISHERIES SUSTAINABILITY SURVEY
source('C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/SSF_import.r')

########################################################################

##IMPORT WORMS TAXONOMIES- NEEDED TO MERGE SPECIES NAMES TO FISHERIES

########################################################################
(load('N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/worms_form.RData'))
worms$synonym<-tolower(worms$synonym)


######################################################################
##IMPORT VULNERABILITY SCORES
(load('N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/vdata_form.RData'))

##GET TSN NUMBERS FOR SUSTAINABILITY SURVEY AND ZIFF DB TO MERGE
fs.taxon<-unique(subset(fs,is.na(sname)==FALSE,select=c('sname','cname')))
fs.taxon<-cbind(fs.taxon,data.frame(get_tsn(fs.taxon$sname,accepted=TRUE,ask=TRUE)))
fs.taxon<-fs.taxon[,1:3]
##TRY TO RETRREIVE MISSING TSNS BASED ON COMMON NAME
fs.taxon$ids<-ifelse(fs.taxon$cname %in% c('giant red sea cucumber'),get_tsn('red sea cucumber',accepted=TRUE)[1],fs.taxon$ids)
fs<-left_join(fs,unique(subset(fs.taxon,select=c('sname','ids'))),by=c('sname'))


 


######################################################################

##ANALYSES: FIRST CODE CHUNK CALCULATES PORT/SPECIES FISHING FOOTPRINT AND CREATES POLYGON AROUND EACH

######################################################################

##LANDINGS FOR ONLY SPECIES IN SUSTAINABILITY SURVEY - BASED ON ABOVE RETREIVAL OF TSNS
datt<-subset(dat,ids %in% unique(fs$ids) | sciname %in% unique(fs$sname))##76 SPECIES

##MAKE VARIABLE TO ID PORT AND SPECIES 
datt$portc<-paste(datt$homeport,'_',datt$spec,sep='')

##EXTRACT DATA WHERE SPECIES IS IDENTIFIED AND HOMEPORT IS PRESENT 
##USING HOMEPORT RATHER THAN LANDINGS PORT BECAUSE FISHERS MAY SIMPLY OFFLOAD CATCH AT LANDINGS PORT BUT ACTUALLY USE THEIR HOME PORT MORE FREQUENTLY
datt<-subset(datt,is.na(spec)==FALSE & is.na(homeport)==FALSE)

##a<-subset(datt,select=c('portc','homeport','spec'))
##b<-subset(portpoly,select=c('portc','homeport','spec'))
##d<-left_join(b,a,by=c('portc'))

###########################################################################

##PORT/SPECIES FOOTPRINT: FUNCTION MAKES BUFFERED POLYGON AROUND LANDINGS FOOTPRINT FOR EACH PORT AND SPECIES - LOOPS THROUGH LANDINGS FROM EACH PORTCODE (UNIQUE HOME PORT ADN SPECIES)

############################################################################
##d<-subset(datt,portc %in% unique(datt$portc)[1])
##d<-subset(datt,portc %in% c("53301_halibut"))
#maps::map('world',xlim=c(-70,-40),ylim=c(40,60))
#points(d$lon,d$lat,pch=16,col='red')
##maps::map('world',xlim=c(-65,-60),ylim=c(48,52))
##plot(hull,add=TRUE,col='red')

f<-function(d){
print(unique(d$portc))
crds<-unique(subset(d,select=c('lon','lat')))
print(dim(crds))

##IF N COORDS IS SMALL, DOUBLE IT WITH JITTERED VERSIONS
if(dim(crds)[1]<=10){
  crds<-rbind(data.frame(lon=jitter(crds$lon,amount=0.1),
                         lat=jitter(crds$lat,amount=0.1)),
              data.frame(lon=jitter(crds$lon,amount=0.1),
                         lat=jitter(crds$lat,amount=0.1)),
              crds)
}
##SF POINTS OBJECT
crds.sf<- st_as_sf(crds, coords = c("lon", "lat"), crs = mcrt)


##CREATE HULL AROUND POINTS
hull<-  st_simplify(
    st_convex_hull(
      st_union(
        st_geometry(crds.sf)
        )
      )
    ,dTolerance=0.01)
st_crs(hull)<-mcrt

##ST_BUFFER CREATES JAGGED EDGES, SO NEED TO USE GBUFFER INSTEAD
hull<-as(
  spTransform(
    gBuffer(
      as(hull,'Spatial')
      ,width=0.05)
    , mcrt)
  , 'sf')


##SF WAY, BUT FOR SOME REASON MAKES EDGES JAGGED
##BUFFERED CONVEX HULL AROUND POINTS
ff<-function(){
  hull<-st_simplify(
  st_buffer(
    st_convex_hull(
      st_union(
        st_geometry(crds.sf)
        )
      )
    , dist = 1)
  , dTolerance = 0.01)
}
##plot(st_geometry(hull),axes=TRUE)
##plot(st_geometry(cst),axes=TRUE)
##plot(st_geometry(hull),add=TRUE,col='red')

##REMOVE LAND
##hull<-st_transform(hull,mcrt)
st_crs(hull)<-mcrt
st_crs(hull)==st_crs(crds.sf)
hull<-st_difference(hull, st_geometry(cst))
##hull<-st_intersection(hull, st_geometry(cst))

if(dim(hull)[1]>0){
##ADD VARIABLE NAMES
hull<-cbind(hull,unique(subset(d,select=c('homeport','species_code','spgroup','sciname','spec','portc','ids'))))
hull$spweight<-sum(d$weight,na.rm=TRUE)
hull$spvalue<-sum(d$value,na.rm=TRUE)
##plot(st_geometry(st_as_sf(hull,crs = mcrt)),add=TRUE,border='green')
##plot(st_geometry(st_as_sf(hull,crs = mcrt)))
 
##l[[i]]<- st_as_sf(hull,crs = mcrt)
return(st_as_sf(hull,crs = mcrt))
} else NULL

}
l<-dlply(datt,.(portc),.fun=f,.progress='text')
portpoly<-do.call('rbind',l)
##z<-portpoly

##MERGE WORMS TAXONOMY TO FLEET POLYGON
portpoly<-merge(portpoly,worms,by.x='sciname',by.y='synonym',all.x=TRUE,all.y=FALSE)

plot(st_geometry(portpoly),col=alpha('red',0.05),border='gray')
q<-portpoly

#####################################################################

##WRITE PORT/SPECIES POLYGONS FOR LATER

#####################################################################
##st_write(portpoly,paste(datadir,'/shapefiles/ports/portpoly.shp',sep=''),append=FALSE)
portpoly<-st_read('N:/data/CAFF/data/shapefiles/ports/portpoly.shp')
names(portpoly)<-c('sciname','homeport','species_id','spgroup','spec','portc','ids','spweight','spvalue','acceptedname','taxonrank','txnmcst','geometry')


##LISTS SPECIES IN PORTPOLY THAT ARE NOT IN VULNERABILITY DB - MOSTLY UNRESOLVED TAXONOMIC GROUPS (E.G., 'SEA URCHINS', 'PELAGIC, UNSPECIFIED') - ALSO SHOWS THAT SCINAME MATCHES WELL TO SPNAME; ACCEPTEDNAME DOESN'T WORK SO WELL
d<-subset(portpoly,!(sciname %in% unique(tolower(vdata$spname))))
dd<-unique(subset(data.frame(unique(subset(d,select=c('sciname','spec')))),select=c('sciname','spec')))



###################################################################
###################################################################

##CALCULATE ECOLOGICAL CLIMATE RISK FOR EACH HARBOUR

###################################################################

######################################################################

##PORT RISK: LOOP THROUGH PORTS AND SPECIES, OVERLAY FOOTPRINT WITH VULNERABILITY POINTS, CALCULATE VULN PER PORT AND STOCK
##DEPENDS ON 'RISKFUN' SOURCED EXTERNALLY

##RETAIN ONLY POLYGONS WITH VULN MATCH
vdata$spname<-tolower(vdata$spname)
vdata<-subset(vdata,is.na(spname)==FALSE)

portpoly<-subset(portpoly,sciname %in% unique(vdata$spname))

##CALCULATES RISK METRICS FOR EACH SPECIES FISHED FROM EACH PORT
#######################################################################
##d<-subset(fleetpoly,fleetc %in% unique(fleetpoly$fleetc)[5])
##d<-subset(portpoly,portc %in% c('0_american plaice'))
##d<-subset(portpoly,portc %in% unique(portpoly$portc)[2])
prt<-sort(unique(portpoly$portc))
##prt<-prt[1:10]
l<-list()
for(i in 1:length(prt)){

##EXTRACT FISHING FOOTPRINT FOR PORT/SPECIES COMBO  
d<-subset(portpoly,portc %in% prt[i])  
print(i)
print(unique(d$portc))
##print(class(d))
  
##RETRIEVE VULN/RISK FOR SPECIES OF INTEREST; IF NAME MISSING, LOOK TO TSN
##dvuln<-subset(vdata,acceptedname %in% unique(d$acceptedname))
##if(is.na(unique(d$acceptedname))==FALSE){
##  dvuln<-subset(vdata,acceptedname %in% unique(d$acceptedname))
##} else if (is.na(unique(d$ids))==FALSE) {
##  dvuln<-subset(vdata,ids %in% unique(d$ids))
##} else dvuln<-data.frame()

##if(is.na(unique(d$acceptedname))==FALSE){
##  dvuln<-subset(vdata,acceptedname %in% unique(d$acceptedname))
##} else 
##  dvuln<-subset(vdata,ids %in% unique(d$ids))

dvuln<-subset(vdata,spname %in% unique(d$sciname))
print(unique(d$sciname))
print(dim(dvuln))
  
##IF VULNERABILITY AVAILABLE, PROCEED
if(dim(dvuln)[1]>1){
##if(length(unique(dvuln$acceptedname))<=1 & (is.na(unique(dvuln$acceptedname))[1]==FALSE | is.na(unique(dvuln$ids))[1]==FALSE)){
  
##OVERLAY FISHING FOOTPRINT POLYGON WITH VULNERABILITY MAP
crds.sf<-st_as_sf(dvuln,coords=c('lon','lat'),crs=mcrt)
system.time(dti<-st_filter(crds.sf,d))
##system.time(dti<-st_intersection(d, crds.sf))
##system.time(dti2<-st_intersects(d, crds.sf))
##dti2<-crds.sf[which(unlist(st_intersects(d, crds.sf))==1)]

##CONVERT TO DATA.FRAME
##dvuln<-st_drop_geometry(dti)
dvuln<-dti

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
a<-catfun(a)

##ADDS ADDITIONAL VARIABLES
a<-cbind(a,data.frame(portc=unique(d$portc),
                      homeport=unique(d$homeport),
                      sciname=unique(d$sciname),
                      spgroup=unique(d$spgroup),
                      spec=unique(d$spec),
                      spweight=unique(d$spweight),
                      spvalue=unique(d$spvalue),
                      acceptedname=unique(dvuln$acceptedname),
                      speciesid=unique(dvuln$speciesid),
                      comname=unique(dvuln$comname),
                      spname=unique(dvuln$spname),
                      footprint.areakm2=round(as.numeric(st_area(d))/1e+06,digits=0)))

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


vdat<-st_as_sf(subset(vdata, acceptedname %in% unique(portpoly$acceptedname) |
                        ids %in% unique(portpoly$ids)),coords=c('lon','lat'),remove=FALSE)
st_crs(vdat)<-mcrt
vdatp<-st_join(portpoly,vdat, join=st_within)
               
##################################################################

##SAVE PORT LEVEL ECOLOGICAL RISK: MEAN RISK EACH SPECIES FISHED FROM EACH HARBOUR ACROSS ITS FISHING FOOTPRINT

###################################################################
save(pspecrisk,file=paste(datadir,'/rfiles/pspecrisk_SEP2023.RData',sep=''))
(load(paste(datadir,'/rfiles/pspecrisk_SEP2023.RData',sep='')))





length(unique(pspecrisk$homeport))
library(Hmisc)

f<-function(d){
  return(data.frame(nspec=dim(d)[1],
                    tl=mean(d$tl,na.rm=TRUE),
                    sensw=wtd.mean(d$sens,weights=d$spweight,na.rm=TRUE),
                    expow=wtd.mean(d$expo,weights=d$spweight,na.rm=TRUE),
                    adcapw=wtd.mean(d$adcap,weights=d$spweight,na.rm=TRUE),
                    vulnw=wtd.mean(d$vuln,weights=d$spweight,na.rm=TRUE),
                    sens=mean(d$sens,na.rm=TRUE),
                    expo=mean(d$expo,na.rm=TRUE),
                    adcap=mean(d$expo,na.rm=TRUE),
                    vuln=mean(d$vuln,na.rm=TRUE)))                    
}
prisk<-ddply(pspecrisk,.(rcp,homeport),.fun=f,.progress='text')



#####################################################
##ADD CIVI INFO IN - HARBOUR NAMES, LOCATIONS
civi<-read.csv('N:/data/CAFF/data/CIVI/CIVIData_Atlantic_20220920.csv',header=TRUE)
names(civi)<-tolower(names(civi))
##prisk2<-unique(subset(prisk,rcp %in% c('8.5')))
civi2<-unique(subset(civi,select=c('ziffharbourcode','long','lat','province','civi')))
civi2$dum<-1
prisk<-left_join(prisk,civi2,by=c('homeport'='ziffharbourcode'))##116/1204 (10%) ZIFF HARBOURS 


plot(prisk$nspec,prisk$vuln)
library(lattice)
xyplot(vulnw ~ nspec |rcp,data=prisk)
xyplot(vulnw ~ nspec |rcp,data=prisk)

xyplot(vulnw ~ civi |rcp,data=prisk2)
xyplot(lat ~ vulnw |rcp,data=prisk2)
xyplot(lat ~ civi |rcp,data=prisk2)



##TRYING TO SEE HOW PORT CODES HERE MATCH WITH THOSE IN CIVI
##pcodes<-read.csv('N:/data/landings/DFO_logbooks/ZIFF/portcodes.csv',header=TRUE,skip=1)
##names(pcodes)<-c('pcode','portziff')
##pcodes<-subset(pcodes,select=c('pcode','portziff'))
##civi<-subset(civi,select=c('ziffharbourcode','titlecasename','province','lat','long','civi'))
##names(civi)<-c('pcode','portcivi','prov')
prisk2<-unique(subset(prisk,rcp %in% c('8.5')))
civi2<-unique(subset(civi,select=c('ziffharbourcode','province','civi')))
civi2$dum<-1
b<-left_join(prisk2,civi2,by=c('homeport'='ziffharbourcode'))##116/1204 (10%) ZIFF HARBOURS NOT IN CIVI
d<-left_join(civi2,prisk2,by=c('ziffharbourcode'='homeport'))##
summary(as.factor(d$rcp))
dd<-subset(d,is.na(rcp)==TRUE)

##LOAD FULL ZIFF DB - NO FILTERING
(load(paste(datadir,'/rfiles/ZIFF_homeport_full_nofilter.RData',sep='')))
(load(paste(datadir,'/rfiles/ZIFF_portland_full_nofilter.RData',sep='')))

civi2<-unique(subset(civi,select=c('ziffharbourcode','province','civi')))
civi2$dum<-1
bh<-left_join(dhport,civi2,by=c('homeport'='ziffharbourcode'))##294/1545 (19%) ZIFF HARBOURS NOT IN CIVI
bl<-left_join(dlport,civi2,by=c('portland'='ziffharbourcode'))##609/2285 (27%) ZIFF HARBOURS NOT IN CIVI


bh<-left_join(civi2,dhport,by=c('ziffharbourcode'='homeport'))##855/2179 (39%) ZIFF HARBOURS NOT IN CIVI
bl<-left_join(civi2,dlport,by=c('ziffharbourcode'='portland'))##383/2179 (18%) ZIFF HARBOURS NOT IN CIVI


a<-subset(civi,!(ziffharbourcode %in% dhport$homeport))
b<-subset(civi,!(ziffharbourcode %in% dlport$portland))

write.csv(a,file='C:/Users/sailfish/Downloads/CIVI_nonmatched_homeports.csv',row.names=FALSE)
write.csv(b,file='C:/Users/sailfish/Downloads/CIVI_nonmatched_landingports.csv',row.names=FALSE)


d<-left_join(civi2,prisk2,by=c('ziffharbourcode'='homeport'))##
summary(as.factor(d$rcp))
dd<-subset(d,is.na(rcp)==TRUE)


a<-subset(civi,!(ziffharbourcode %in% prisk2$homeport))
b<-subset(civi,!(ziffharbourcode %in% dat$portland))
write.csv(a,file='C:/Users/sailfish/Downloads/CIVI_nonmatched_harbours.csv',row.names=FALSE)
plot(civi$long,civi$lat,pch=16)
points(a$long,a$lat,pch=16,col='red')


b<-left_join(prisk,civi,by=c('homeport'='ziffharbourcode'))
plot(subset(b,rcp %in% c('8.5'),select=c('sens','expo','adcap','vuln','civi')),pch=16)
plot(subset(b,rcp %in% c('8.5'),select=c('sensw','expow','adcapw','vulnw','civi')),pch=16)
round(cor(subset(b,rcp %in% c('8.5'),select=c('sensw','expow','adcapw','vulnw','civi')),use='pairwise.complete.obs'),digits=2)

plot(subset(b,rcp %in% c('8.5'),select=c('sensw','expow','adcapw','vulnw','civi','cmscore','sival','siscore','slcval','slcscore','whval','dop','hcscore','trcscore','calcisi','percentincomefish','perscore','pop','popscore','valueperves','valuescore','calcsesi')),pch=16)

plot(subset(b,rcp %in% c('8.5'),select=c('sensw','expow','adcapw','vulnw','civi','sival','slcval','whval','dop','calcisi','percentincomefish','pop','valueperves','calcsesi')),pch=16)

bb<-subset(b,is.na(portziff)==TRUE)##806

pcodes<-read.csv('N:/data/landings/DFO_logbooks/ZIFF/portcodes.csv',header=TRUE,skip=1)
names(pcodes)<-c('pcode','portziff')
pcodes<-subset(pcodes,select=c('pcode','portziff'))

z<-subset(civi,!(ziffharbourcode %in% unique(data$homeport)))
##PORTS IN CIVI THAT HAVE NO MATCH IN THE ZIFF LOOKUP TABLE
z<-subset(civi,!(ziffharbourcode %in% unique(pcodes$pcode)))
write.csv(z,file='C:/Users/sailfish/Downloads/CIVI_nonmatched_harbours2.csv',row.names=FALSE)


