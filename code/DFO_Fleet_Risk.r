##############################################################################

##CALCULATES FOOTPRINT OF FISHING FOR EACH FLEET (SPECIES, GEAR, VESSEL LENGTH) AND THEN CALCULATES ECOLOGICAL VULNERABILITY

###############################################################################

##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir,'GPARAMs.r',sep=''))


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



#######################################################################

##ANALYSES

#######################################################################
##RETAIN ONLY FLEET POLYGONS FOR SPECIES IN SUSTAINABILITY SURVEY
##'DAT' IS ZIFF DATA
datt<-subset(dat,sciname %in% unique(fs$sname))
datt$fleetc<-paste(datt$spec,'_',datt$gearcat,'_',datt$lenclass,sep='')
datt<-subset(datt,is.na(spec)==FALSE)

###########################################################################

##FUNCTION MAKES BUFFERED POLYGON AROUND THE LANDINGS FOR EACH DEFINED FLEET; CREATES A FLEET FOOTPRINT (POLYGON)

############################################################################
##d<-subset(datt,fleetc %in% unique(datt$fleetc)[1])
f<-function(d){
print(unique(d$fleetc))
  ##hull <- crds %>%
##  st_as_sf(coords = c("lon", "lat")) %>%
##  summarize(geometry = st_union(geometry)) %>%
##  st_convex_hull()
crds<-unique(subset(d,select=c('lon','lat')))

##SF POINTS OBJECT
crds.sf<- st_as_sf(crds, coords = c("lon", "lat"), crs = mcrt)

##BUFFERED CONVEX HULL AROUND POINTS
hull<-st_simplify(
  st_buffer(
    st_convex_hull(
      st_union(
        st_geometry(crds.sf)
        )
      )
    , dist = 1)
  , dTolerance = 0.01)

##REMOVE LAND
##hull<-st_transform(hull,mcrt)
st_crs(hull)<-mcrt
st_crs(hull)==st_crs(crds.sf)
hull<-st_intersection(hull, st_geometry(cst))

##ADD VARIABLE NAMES
hull<-cbind(hull,unique(subset(d,select=c('species_code','gearcat','spgroup','sciname','spec','lenclass','fleetc'))))
##plot(st_geometry(st_as_sf(hull,crs = mcrt)),add=TRUE,border='green')
##plot(st_geometry(st_as_sf(hull,crs = mcrt)))
 
##l[[i]]<- st_as_sf(hull,crs = mcrt)
return(st_as_sf(hull,crs = mcrt))

}
##l<-dlply(subset(datt,fleetc %in% unique(datt$fleetc)[1:10]),.(fleetc),.fun=f,.progress='text')
l<-dlply(datt,.(fleetc),.fun=f,.progress='text')
fleetpoly<-do.call('rbind',l)


##MERGE WORMS TAXONOMY TO FLEET POLYGON
fleetpoly<-merge(fleetpoly,worms,by.x='sciname',by.y='synonym',all.x=TRUE,all.y=FALSE)

##st_write(fleetpoly,paste(datadir,'/shapefiles/fleets/fleetpoly.shp',sep=''),append=FALSE)
fleetpoly<-st_read('N:/data/CAFF/data/shapefiles/fleets/fleetpoly.shp')

 
a<-subset(fleetpoly,fleetc=='capelin_fix_sml')
plot(st_geometry(fleetpoly),col=alpha('red',0.05),border='gray')
plot(st_geometry(a))



###############################################################

##FLEET VULNERABILITY: LOOP THROUGH FLEETS, OVERLAY FLEET FOOTPRINT WITH SPATIAL VULNERABILITY POINTS, CALCULATE VULNERABILITY PER FLEET
##DEPENDS ON 'RISKFUN' FUNCTION WHICH IS SOURCED

################################################################
##d<-subset(fleetpoly,fleetc %in% unique(fleetpoly$fleetc)[5])
##d<-subset(fleetpoly,fleetc %in% c('american plaice_fix_med'))
flt<-sort(unique(fleetpoly$fleetc))
flt<-flt[1:10]
l<-list()
for(i in 1:length(flt)){

d<-subset(fleetpoly,fleetc %in% flt[i])  
print(unique(d$fleetc))
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

##FVULN CALCULATES AVERAGE VULN/RISK 
a<-riskfun(b)

##ADDS ADDITIONAL VARIABLES
a<-cbind(a,data.frame(fleetc=unique(b$fleetc),
                      sciname=unique(b$sciname),
                      gearcat=unique(b$gearcat),
                      spgroup=unique(b$spgroup),
                      spec=unique(b$spec),
                      lenclass=unique(b$lenclss),
                      acceptedname=unique(b$acceptedname),
                      taxonrank=unique(b$taxonrank),
                      speciesid=unique(b$speciesid),
                      comname=unique(b$comname),
                      kingdom=unique(b$kingdom),
                      phylum=unique(b$phylum),
                      class=unique(b$class),
                      order=unique(b$order),
                      family=unique(b$family),
                      spname=unique(b$spname),
                      aname=unique(b$acceptedname),
                      lmax=unique(b$lmax)))

return(a)
}
fvuln<-ddply(dvuln,.(rcp),.fun=f2)

##IF VULN NOT PRESENT, RETURN ONLY FLEET AND SPECIES
} else {
  fvuln<-data.frame(fleetc=unique(d$fleetc),
                    acceptedname=unique(d$acceptedname))
}
##return(fvuln)
l[[i]]<-fvuln
}
fleetrisk<-rbind.fill(l)


####################################################################

##SAVE FLEET LEVEL ECOLOGICAL VULNERABILITY

#####################################################################
save(fleetrisk,file=paste(datadir,'/rfiles/fleetrisk.RData',sep=''))

a<-unique(subset(ot,is.na(adcap)==TRUE,select=c('fleetc','acceptedname')))
b<-unique(subset(ot,is.na(adcap)==FALSE))


