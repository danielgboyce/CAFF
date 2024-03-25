library(ncdf4)
library(raster)
path<-'C:/Users/sailfish/Downloads/'
setwd(path)
nm<-'CMIPnew_1970s_1.nc'
dat<-nc_open(nm)
lon<-ncvar_get(dat,'lon')
lat<-ncvar_get(dat,'lat')
#lon2<-ncvar_get(dat,'rlat')
sst<-ncvar_get(dat,'tasmin')
xy<-expand.grid(lon,lat)
sstr<-brick(lon,lat,sst)
a<-diff(lat)

sst1<-sst[,,1]
library(pals)
library(lattice)
library(RColorBrewer)
library(ggplot2)
image(lon,lat,sst1)
levelplot(sst1 ~ lon * lat,at=seq(200,300,10),cuts=10,col.regions=(rev(brewer.pal(10,"RdBu"))))

grid <- expand.grid(lon=lon, lat=lat)
sstd<-as.vector(sst[,,1])




library(ncdf4)
library(pals)
library(ggplot2)
path<-'C:/Users/sailfish/Downloads/'
setwd(path)
nm<-'CMIPnew_1970s_1.nc'
dat<-nc_open(nm)
dat
lon<-ncvar_get(dat,'lon')
lat<-ncvar_get(dat,'lat')
sst<-ncvar_get(dat,'tasmin')
sst1<-sst[,,1]

##DATA FRAME OF TEMPERATURE AT FIRST TIME POINT
d<-data.frame(lon=as.vector(lon),
              lat=as.vector(lat),
              sst=as.vector(sst1))

##PLOT
xlm<-c(-75,-44)
ylm<-c(40,61)
ggplot()+
  geom_point(data=d,aes(x=lon,y=lat,col=sst),pch=15)+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.text.x = element_text(size = 9),
        panel.border=element_rect(colour="black",fill=NA))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value='gray90',limits=c(270,291))



cbind(grid,as.vector(sst)))

d<-data.frame(cbind(grid,as.vector(sst)))
  ncfname<-paste(path,nm,sep='')
  sstr<-brick(ncfname, varname='tasmin')
  sstr<-brick(sst)
  
  
##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CC_vulnerability_regional/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CC_vulnerability_regional/code/')
source(paste(codedir,'GPARAMs_CC_Vuln_reg.r',sep=''))

figsdir<-"C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs"

##extrafont::font_import()
extrafont::loadfonts()

library(sf)
wcm<-st_read('N:/data/shapefiles/naturalearthdata_ne_50m_land_poly/ne_50m_land.shp')
wch<-st_read('N:/data/shapefiles/naturalearthdata_ne_10m_land_poly/ne_10m_land.shp')

clb<-'gray80'
clb<-'white'
##COLOURS FOR RISK CATEGORIES
clvh<-'gray60'
clh<-'gray70'
cll<-'gray80'
clvl<-'gray90'

##IN CASE WE NEED TO REFORMAT DATA- SOURCE THIS
##source(paste(codedir,'Figures_data_format.r',sep=''))
 
##################################################################

##STOCK MANAGEMENT POLYGONS FOR EACH STOCK

###################################################################
stockpoly<-st_read('N:/data/CAFF/data/shapefiles/stocks/stockpoly.shp')

a<-subset(stockpoly,speciesg %in% c('Groundfish'))
a<-subset(stockpoly,speciesg %in% c('Large Pelagics'))

atl<-subset(stockpoly,!(region %in% c('Pacific', 'Central and Arctic')))
xlm<-c(-83,-42)
ylm<-c(35,80)
lw<-0.5


speciesgid<-unique(atl$speciesg)
l<-list()
for(i in 1:length(speciesgid)){
  b<-subset(atl,speciesg %in% speciesgid[i])
print(i)
print(unique(b$speciesg))
l[[i]]<-ggplot()+
##  geom_sf(data=a,fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
  geom_sf(data=b,fill='firebrick3',inherit.aes=FALSE,color=NA,lwd=lw)+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
##  geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=2.5)+
##  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='white')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        strip.text.x = element_text(size = 9),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')+
  facet_wrap(~stock)+ 
  ggtitle(unique(b$speciesg))
}


setwd(figsdir)
pdf('stock_areas_mapped.pdf',height=10,width=7)
l
dev.off()

 

##################################################

##LOAD DATA - PREVIOUSLY FORMATTED

###################################################
##LOAD FISHERIES VULNERABILITY DATA
##(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecnd.RData',sep='')))
##(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecnt.RData',sep='')))
##(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecny.RData',sep='')))
##(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecfs.RData',sep='')))
(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecram.RData',sep='')))
vspecram<-subset(vspecram,!(stockid %in% c('PORSHARATL') & is.na(lmax)==TRUE))
vspecram<-subset(vspecram,!(stockid %in% c('BSKATCANATL') & is.na(lmax)==TRUE))


##VDATA
(load(paste(datadirout,'/E_ToEdata_0.25deg_85_TANmax.RData',sep='')))
toedata<-subset(toedata,select=c('speciesid','cell','ToE'))
names(toedata)<-c('speciesid','cell','E.toe.raw')

(load(paste(datadirout,'/S_TSM_0.25deg.RData',sep='')))
smdata<-subset(smdata,select=c('speciesid','lon','lat','TSMr'))


(load('N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/vdata_form.RData'))
##(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vdata_form.RData',sep='')))
vdata<-subset(vdata,speciesid %in% spids)
names(vdata)<-ifelse(names(vdata)%in% c('comname.x'),'comname',names(vdata))
vdata<-left_join(vdata,toedata,by=c('speciesid','cell'))
vdata<-left_join(vdata,smdata,by=c('speciesid','lon','lat'))
gc()



##LOAD FORMATTED DATA ON THRESHOLDS
(load(paste("N:/data/CC_vulnerability/data/",'analysis_outputs_local/one_deg/figures_data/','thdats_form.RData',sep='')))


##MAP DOMAIN
mxlm<-c(-80,-42)
mylm<-c(39,80)
backcl<-'white'
mapcl<-'white'
polcl<-'black'

library(sf)
nplgsf<-st_as_sf(nplg)

##LOAD BATHYMETRY GRID
library(marmap)
bth<-getNOAA.bathy(lon1=mxlm[1],lon2=mxlm[2],lat1=mylm[1],lat2=mylm[2], resolution=12)
##load('N:/data/CMIP_historical/data_output/marmap_bathy.RData')
##bthsf<-as.SpatialGridDataFrame(bth)
##bthsf<-st_as_sf(bthsf)

mcrt<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
##nafos<-readOGR('N:/data/stock_assessments/data/fishing_shapefiles/nafo','Divisions')#works for Alaska - most others don't
nafos<-st_read('N:/data/stock_assessments/data/fishing_shapefiles/nafo','Divisions')#works for Alaska - most others don't
nafos<-st_transform(nafos,CRS(mcrt))

catfun<-function(x){
  ##CATEGORIZE VULNERABILITY USING THRESHOLDS
  x$vcat<-ifelse(x$vuln>=subset(thdats,ind %in% c('Vulnerability'))$tmed,'H','L')
  x$vcat<-ifelse(x$vuln>=subset(thdats,ind %in% c('Vulnerability'))$thigh,'VH',x$vcat)
  x$vcat<-ifelse(x$vuln<=subset(thdats,ind %in% c('Vulnerability'))$tlow,'VL',x$vcat)
##CATEGORIZE SENSITIVITIES USING THRESHOLDS
x$scat<-ifelse(x$sens>=subset(thdats,ind %in% c('Sensitivity'))$tmed,'H','L')
x$scat<-ifelse(x$sens>=subset(thdats,ind %in% c('Sensitivity'))$thigh,'VH',x$scat)
x$scat<-ifelse(x$sens<=subset(thdats,ind %in% c('Sensitivity'))$tlow,'VL',x$scat)
##CATEGORIZE EXPOSURES USING THRESHOLDS
x$ecat<-ifelse(x$expo>=subset(thdats,ind %in% c('Exposure'))$tmed,'H','L')
x$ecat<-ifelse(x$expo>=subset(thdats,ind %in% c('Exposure'))$thigh,'VH',x$ecat)
x$ecat<-ifelse(x$expo<=subset(thdats,ind %in% c('Exposure'))$tlow,'VL',x$ecat)
##CATEGORIZE ADAPTIVE CAPACITIES USING THRESHOLDS
x$acat<-ifelse(x$adcap>=subset(thdats,ind %in% c('Adaptive capacity'))$tmed,'L','H')
x$acat<-ifelse(x$adcap>=subset(thdats,ind %in% c('Adaptive capacity'))$thigh,'VL',x$acat)
  x$acat<-ifelse(x$adcap<=subset(thdats,ind %in% c('Adaptive capacity'))$tlow,'VH',x$acat)

  ##SENSITIVITY DIMENSIONS
  x$S.vind.cat<-ifelse(x$S.vind>=subset(thdats,ind %in% c('S.vind'))$tmed,'H','L')
  x$S.vind.cat<-ifelse(x$S.vind>=subset(thdats,ind %in% c('S.vind'))$thigh,'VH',x$S.vind.cat)
  x$S.vind.cat<-ifelse(x$S.vind<=subset(thdats,ind %in% c('S.vind'))$tlow,'VL',x$S.vind.cat)
  x$S.HII.cat<-ifelse(x$S.HII>=subset(thdats,ind %in% c('S.HII'))$tmed,'H','L')
  x$S.HII.cat<-ifelse(x$S.HII>=subset(thdats,ind %in% c('S.HII'))$thigh,'VH',x$S.HII.cat)
  x$S.HII.cat<-ifelse(x$S.HII<=subset(thdats,ind %in% c('S.HII'))$tlow,'VL',x$S.HII.cat)
  x$S.rlstatus.cat<-ifelse(x$S.rlstatus>=subset(thdats,ind %in% c('S.rlstatus'))$tmed,'H','L')
  x$S.rlstatus.cat<-ifelse(x$S.rlstatus>=subset(thdats,ind %in% c('S.rlstatus'))$thigh,'VH',x$S.rlstatus.cat)
  x$S.rlstatus.cat<-ifelse(x$S.rlstatus<=subset(thdats,ind %in% c('S.rlstatus'))$tlow,'VL',x$S.rlstatus.cat)
  x$S.TSMr.cat<-ifelse(x$S.TSMr>=subset(thdats,ind %in% c('S.TSMr'))$tmed,'H','L')
  x$S.TSMr.cat<-ifelse(x$S.TSMr>=subset(thdats,ind %in% c('S.TSMr'))$thigh,'VH',x$S.TSMr.cat)
  x$S.TSMr.cat<-ifelse(x$S.TSMr<=subset(thdats,ind %in% c('S.TSMr'))$tlow,'VL',x$S.TSMr.cat)

  ##EXPOSURE DIMENSIONS
  x$E.nrchng.cat<-ifelse(x$E.nrchng>=subset(thdats,ind %in% c('E.nrchng'))$tmed,'H','L')
  x$E.nrchng.cat<-ifelse(x$E.nrchng>=subset(thdats,ind %in% c('E.nrchng'))$thigh,'VH',x$E.nrchng.cat)
  x$E.nrchng.cat<-ifelse(x$E.nrchng<=subset(thdats,ind %in% c('E.nrchng'))$tlow,'VL',x$E.nrchng.cat)
  x$E.plost.cat<-ifelse(x$E.plost>=subset(thdats,ind %in% c('E.plost'))$tmed,'H','L')
  x$E.plost.cat<-ifelse(x$E.plost>=subset(thdats,ind %in% c('E.plost'))$thigh,'VH',x$E.plost.cat)
  x$E.plost.cat<-ifelse(x$E.plost<=subset(thdats,ind %in% c('E.plost'))$tlow,'VL',x$E.plost.cat)
  x$E.toe.cat<-ifelse(x$E.toe>=subset(thdats,ind %in% c('E.toe'))$tmed,'H','L')
  x$E.toe.cat<-ifelse(x$E.toe>=subset(thdats,ind %in% c('E.toe'))$thigh,'VH',x$E.toe.cat)
  x$E.toe.cat<-ifelse(x$E.toe<=subset(thdats,ind %in% c('E.toe'))$tlow,'VL',x$E.toe.cat)
  x$E.vel.cat<-ifelse(x$E.vel>=subset(thdats,ind %in% c('E.vel'))$tmed,'H','L')
  x$E.vel.cat<-ifelse(x$E.vel>=subset(thdats,ind %in% c('E.vel'))$thigh,'VH',x$E.vel.cat)
  x$E.vel.cat<-ifelse(x$E.vel<=subset(thdats,ind %in% c('E.vel'))$tlow,'VL',x$E.vel.cat)

##CATEGORIZE ADAPTIVE CAPACITIES USING THRESHOLDS - HIGH RISK FOR LOW ADAPTIVITY SCORES
  x$AC.hfrag.cat<-ifelse(x$AC.hfrag>=subset(thdats,ind %in% c('AC.hfrag'))$tmed,'L','H')
  x$AC.hfrag.cat<-ifelse(x$AC.hfrag>=subset(thdats,ind %in% c('AC.hfrag'))$thigh,'VL',x$AC.hfrag.cat)
  x$AC.hfrag.cat<-ifelse(x$AC.hfrag<=subset(thdats,ind %in% c('AC.hfrag'))$tlow,'VH',x$AC.hfrag.cat)

  x$AC.hrange.cat<-ifelse(x$AC.hrange>=subset(thdats,ind %in% c('AC.hrange'))$tmed,'L','H')
  x$AC.hrange.cat<-ifelse(x$AC.hrange>=subset(thdats,ind %in% c('AC.hrange'))$thigh,'VL',x$AC.hrange.cat)
  x$AC.hrange.cat<-ifelse(x$AC.hrange<=subset(thdats,ind %in% c('AC.hrange'))$tlow,'VH',x$AC.hrange.cat)

  x$AC.lmax.cat<-ifelse(x$AC.lmax>=subset(thdats,ind %in% c('AC.lmax'))$tmed,'L','H')
  x$AC.lmax.cat<-ifelse(x$AC.lmax>=subset(thdats,ind %in% c('AC.lmax'))$thigh,'VL',x$AC.lmax.cat)
  x$AC.lmax.cat<-ifelse(x$AC.lmax<=subset(thdats,ind %in% c('AC.lmax'))$tlow,'VH',x$AC.lmax.cat)
  x$AC.tvar.cat<-ifelse(x$AC.tvar>=subset(thdats,ind %in% c('AC.tvar'))$tmed,'L','H')
  x$AC.tvar.cat<-ifelse(x$AC.tvar>=subset(thdats,ind %in% c('AC.tvar'))$thigh,'VL',x$AC.tvar.cat)
  x$AC.tvar.cat<-ifelse(x$AC.tvar<=subset(thdats,ind %in% c('AC.tvar'))$tlow,'VH',x$AC.tvar.cat)

  x$vmincat<-ifelse(x$minvuln>=subset(thdats,ind %in% c('Vulnerability'))$tmed,'H','L')
  x$vmincat<-ifelse(x$minvuln>=subset(thdats,ind %in% c('Vulnerability'))$thigh,'VH',x$vmincat)
  x$vmincat<-ifelse(x$minvuln<=subset(thdats,ind %in% c('Vulnerability'))$tlow,'VL',x$vmincat)

  x$vmaxcat<-ifelse(x$maxvuln>=subset(thdats,ind %in% c('Vulnerability'))$tmed,'H','L')
  x$vmaxcat<-ifelse(x$maxvuln>=subset(thdats,ind %in% c('Vulnerability'))$thigh,'VH',x$vmaxcat)
  x$vmaxcat<-ifelse(x$maxvuln<=subset(thdats,ind %in% c('Vulnerability'))$tlow,'VL',x$vmaxcat)

  return(x)
}
##vspec<-catfun(vspec)
vspecram<-catfun(vspecram)



 
#################################################################

##IMPORT CIVI

#################################################################
library(compositions)
civi<-read.csv('N:/data/CAFF/data/CIVI/CIVIData_Atlantic_20220920.csv',header=TRUE)
names(civi)<-tolower(names(civi))

##EXPOSURE AS GEOMETRIC MEAN
civi$expo<-ifelse(is.na(civi$siscore)==FALSE & is.na(civi$slcscore)==FALSE & is.na(civi$wsscore)==FALSE & is.na(civi$cmscore)==FALSE,
                  geometricmeanRow(subset(civi,select=c('siscore','slcscore','wsscore','whscore','cmscore'))),
                  NA)

##SENSITIVITY AS GEOMETRIC MEAN
civi$sens<-ifelse(is.na(civi$hcscore)==FALSE & is.na(civi$dop)==FALSE & is.na(civi$trcscore)==FALSE,
                  geometricmeanRow(subset(civi,select=c('hcscore','dop','trcscore'))),
                  NA)

##ADAPTIVITY AS GEOMETRIC MEAN
civi$adcap<-ifelse(is.na(civi$trcscore)==FALSE,
                   geometricmeanRow(subset(civi,select=c('trcscore'))),
                   NA)

##ADAPTIVITY AS GEOMETRIC MEAN
civi$vuln<-ifelse(is.na(civi$expo)==FALSE & is.na(civi$sens)==FALSE & is.na(civi$adcap)==FALSE,
                  geometricmeanRow(subset(civi,select=c('expo','sens','adcap'))),
                  NA)
summary(civi$vuln)
summary(civi$civi)

##RANGE OF DATA
rmin<-1
rmax<-5
##RANGE TO BE MAPPED
tmin<-0
tmax<-1
civi$expo<-(civi$expo-rmin)/(rmax-rmin)*(tmax-tmin)+tmin
civi$sens<-(civi$sens-rmin)/(rmax-rmin)*(tmax-tmin)+tmin
civi$adcap<-(civi$adcap-rmin)/(rmax-rmin)*(tmax-tmin)+tmin
civi$vuln<-(civi$vuln-rmin)/(rmax-rmin)*(tmax-tmin)+tmin







###########################################################
###########################################################

##CRIB EXAMPLE FOR COD STOCK 

############################################################

##'DAT' IS ZIFF DATA
vdatac<-subset(vdata,comname %in% c('Atlantic cod') & rcp %in% c('8.5'))
nplg2<-subset(nplg,zone %in% c('2J','3K','3L'))
nplg3<-st_buffer(st_union(st_as_sf(nplg2)),0.09)
nplg2<-as(nplg3,'Spatial')
crds2<-SpatialPoints(data.frame(x=vdatac$lon,y=vdatac$lat),proj4string=CRS(mct))
##vdatac$nafod<-over(crds2,nplg2)
##vdatac<-subset(vdatac,is.na(nafod)==FALSE)


plot(vdatac$lon,vdatac$lat,pch=16,col='red')
map.axes()
points(-64.625,43.375,col='green')

plot(nplg,add=TRUE)


##xlm2<-c(-60,-45)
##ylm2<-c(45,56)


xlm2<-c(-69,-43)
ylm2<-c(42,57)
xlm2<-c(-67,-59)
ylm2<-c(43,48)

##vdatac$id<-ifelse(vdatac$lon==-51.625 & vdatac$lat==48.375,1,0)
vdatac$id<-ifelse(vdatac$lon==-61.625 & vdatac$lat==43.375,1,0)
vdatac$id<-as.character(vdatac$id)

p0<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=id),color=NA,alpha=0.5,lwd=0.000001)+
  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=id),color=alpha('black',alpha=0.5),lwd=0.000001)+
##  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=id))+
##  geom_point(data=z,aes(x=lon,y=lat),pch=22,,color='red',fill='red')+
##  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  geom_sf(data=wcm,fill='gray20',inherit.aes = FALSE,color=NA)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
##            scale_fill_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
  scale_fill_manual(values=c('0'='gray85','1'='red'),name='')+
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
      panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')

pp0<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=id),color=NA,alpha=0.5,lwd=0.000001)+
  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=id),color=alpha('dodgerblue4',alpha=0.5),lwd=0.000001)+
##  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=id))+
##  geom_point(data=z,aes(x=lon,y=lat),pch=22,,color='red',fill='red')+
##  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  geom_sf(data=wcm,fill='gray20',inherit.aes = FALSE,color=NA)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
##            scale_fill_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
  scale_fill_manual(values=c('0'='lightskyblue1','1'='red'),name='')+
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
      panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


p1<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=vuln),col=alpha('white',alpha=0.5),lwd=0.00001)+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=vuln))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
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
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


vdatac$vcat<-factor(vdatac$vcat,levels=c('VH','H','L','VL'))
p2<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=vcat),col='white')+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=vcat))+
  scale_fill_manual(values=c('VH'='darkred','H'='orangered','L'='orange1','VL'='gold1'),name='')+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
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
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


vdatac$sens<-ifelse(vdatac$sens>0.75,0.75,vdatac$sens)
p3<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=vuln),col=alpha('white',alpha=0.5),lwd=0.00001)+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=sens))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=brewer.blues(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
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
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


p4<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=expo))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
            scale_fill_gradientn(colors=brewer.reds(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.60),breaks=c(0.25,0.60))+
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
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')

vdatac$adcap<-ifelse(vdatac$adcap>0.6,0.6,vdatac$adcap)
p5<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=adcap))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=rev(heat.colors(100))[5:60],guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.45,0.6),breaks=c(0.45,0.6))+
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
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


p6<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=E.toe.raw))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=rev(viridis(100)),guide='colourbar',name='',na.value=clb,limits=c(55,80),breaks=c(55,80))+
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
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


p7<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=TSMr))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=rev(viridis(100)),guide='colourbar',name='',na.value=clb,limits=c(-3,9),breaks=c(-3,9))+
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
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


setwd(figsdir)
pdf('crib_cod_example.pdf',height=8,width=6)
grid.arrange(p0,p1,p2,ncol=1)
grid.arrange(p3,p4,p5,ncol=1)
grid.arrange(p6,p7,p7,ncol=1)
dev.off()






plot(civi$long,civi$lat,pch=16)
map.axes()
z<-subset(civi,long> -53 & long< -50 & lat> 46 & lat< 47)
points(z$long,z$lat,pch=16,col='red')


xlm2<-c(-66.5,-62)
ylm2<-c(43,46)

##z<-subset(civi,titlecasename=='Renews')
z<-subset(civi,titlecasename=='Halifax')

pp8<-ggplot()+
  geom_sf(data=wch,fill='gray20',inherit.aes = FALSE,color=NA,lwd=0.000001)+
  geom_point(data=civi,aes(x=long,y=lat),color=alpha('lightskyblue',0.9),pch=16,stroke=0.00001,size=1.25)+
  geom_point(data=z,aes(x=long,y=lat),color='red',pch=16,size=2)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
##            scale_fill_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
          legend.position="none",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(1, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')

p8<-ggplot()+
  geom_sf(data=wch,fill='gray20',inherit.aes = FALSE,color=NA,lwd=0.000001)+
  geom_point(data=civi,aes(x=long,y=lat),color=alpha('gray5',0.9),fill='gray85',pch=21,stroke=0.00001,size=1.5)+
  geom_point(data=z,aes(x=long,y=lat),color='red',pch=16,size=2)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
##            scale_fill_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
          legend.position="none",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(1, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')

setwd(figsdir)
pdf('civi_cod_example.pdf',height=8,width=6)
grid.arrange(p8,p8,p8,ncol=1)
dev.off()









#####################################################################
#####################################################################

##BASEMAP SHOWING COD STOCK ASSESSMENT AREAS

#####################################################################

##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CC_vulnerability_regional/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CC_vulnerability_regional/code/')
source(paste(codedir,'GPARAMs_CC_Vuln_reg.r',sep=''))


(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecram.RData',sep='')))
vspeccod<-subset(vspecram,!(stockid %in% c("COD4X5Yb")) & is.na(vuln)==FALSE & rcp %in% c('8.5') & spname %in% c('Gadus morhua'))

#############################################################
##GET POLYGONS FOR EACH COD STOCK
##dt<-subset(b,stockid==sort(unique(b$stockid))[1])
areafun<-function(dt){
  acode<-gsub('COD','',dt$stockid)
  print(acode)
stock<-unique(dt$stockid)
##print(unique(dt$stockid))

#SLICES UP POLYGONS ACCORDING TO REGIONS GIVEN IN RAM DB
if(acode=='4TVn'){plg<-subset(nafos,ZONE %in% c('4T','4Tn'))
} else if(acode=='4X'){plg<-subset(nafos,ZONE %in% c('4X'))
} else if(acode=='4X5Yb'){plg<-subset(nafos,ZONE %in% c('4X','5Y'))
} else if(acode=='4VsW'){plg<-subset(nafos,ZONE %in% c('4Vs','4W'))
} else if(acode=='3Ps'){plg<-subset(nafos,ZONE %in% c('3Ps'))
} else if(acode=='3NO'){plg<-subset(nafos,ZONE %in% c('3N','3O'))
} else if(acode=='3Pn4RS'){plg<-subset(nafos,ZONE %in% c('3Pn','4R','4S'))
} else if(acode=='2J3KL'){plg<-subset(nafos,ZONE %in% c('2J','3K','3L'))
} else if(acode=='3M'){plg<-subset(nafos,ZONE %in% c('3M'))
} else {
  plg<-data.frame(x=NULL,y=NULL)
  print(subset(dt,select=c('areacode','stockid')))
}
##  plot(plg)
  plgd<-plg %>%
    ##  st_buffer(dist=0.001) %>% # make a buffer of half a meter around all parts (to avoid slivers)
    st_union() %>% # unite to a geometry object
    st_sf() %>%# make the geometry a data frame object
    mutate(centrum = T) # return back the data value
  ##    plot(plgd,col='green',add=TRUE)
  plgd$stockid<-unique(dt$stockid)
  plgd$areacode<-unique(dt$areacode)
  return(plgd)
}
plgr<-ddply(vspeccod,.(stockid),.fun=areafun,.progress='none')
plgr<-st_as_sf(plgr)


##xlm<-c(-70,-41)
##ylm<-c(38,56)
lw<-0.25
xlm2<-c(-69,-41)
ylm2<-c(38,56)


p1<-ggplot()+
  geom_sf(data=plgr,color='gray10',inherit.aes=FALSE,fill=alpha('lightskyblue',0.4),lwd=lw)+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill='red3',inherit.aes=FALSE,color=NA,lwd=lw)+
  geom_sf(data=wch,color='gray20',fill='gray20',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
  geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=2.5)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='white')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')



setwd(figsdir)
pdf('crib_cod_example_base.pdf',height=8,width=6)
grid.arrange(p1,p1,p1,ncol=1)
dev.off()







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#####################################################################
#####################################################################

##BASEMAP SHOWING NWAtlantic

#####################################################################


xlm<-c(-75,-41)
ylm<-c(42,65)
lw<-0.5

p<-ggplot()+
##  geom_sf(data=plgr,fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
##  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill='black',inherit.aes=FALSE,color=NA,lwd=lw)+
  geom_sf(data=wcm,col=NA,fill='gray60',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
##  geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=2.5)+
##  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='white')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')


figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs/presfigs'
setwd(figsdir)
pdf('NWAtlantic_basemap.pdf',height=8,width=6)
grid.arrange(p,p,p,ncol=1)
dev.off()




xlm<-c(-180,180)
ylm<-c(-62,90)
lw<-0.5

p<-ggplot()+
  geom_sf(data=wcm,col=NA,fill='black',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
   theme(axis.line.x=element_blank(),
         axis.text.x=element_blank(),
         axis.line.y=element_blank(),
         axis.text.y=element_blank(),
##        panel.background = element_rect(fill = 'white'),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="white",fill=NA,size=lw))+
  xlab('')+
  ylab('')


figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs/presfigs'
setwd(figsdir)
pdf('world_basemap.pdf',height=8,width=6)
grid.arrange(p,p,p,ncol=1)
dev.off()






#####################################################################
#####################################################################

##BASEMAP SHOWING COD STOCK ASSESSMENT AREAS

#####################################################################

##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CC_vulnerability_regional/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CC_vulnerability_regional/code/')
source(paste(codedir,'GPARAMs_CC_Vuln_reg.r',sep=''))


(load(paste(datadir,'analysis_outputs_local/quarter_deg/figures_data/','vspecram.RData',sep='')))
vspeccod<-subset(vspecram,!(stockid %in% c("COD4X5Yb")) & is.na(vuln)==FALSE & rcp %in% c('8.5') & spname %in% c('Gadus morhua'))

#############################################################
##GET POLYGONS FOR EACH COD STOCK
##dt<-subset(b,stockid==sort(unique(b$stockid))[1])
areafun<-function(dt){
  acode<-gsub('COD','',dt$stockid)
  print(acode)
stock<-unique(dt$stockid)
##print(unique(dt$stockid))

#SLICES UP POLYGONS ACCORDING TO REGIONS GIVEN IN RAM DB
if(acode=='4TVn'){plg<-subset(nafos,ZONE %in% c('4T','4Tn'))
} else if(acode=='4X'){plg<-subset(nafos,ZONE %in% c('4X'))
} else if(acode=='4X5Yb'){plg<-subset(nafos,ZONE %in% c('4X','5Y'))
} else if(acode=='4VsW'){plg<-subset(nafos,ZONE %in% c('4Vs','4W'))
} else if(acode=='3Ps'){plg<-subset(nafos,ZONE %in% c('3Ps'))
} else if(acode=='3NO'){plg<-subset(nafos,ZONE %in% c('3N','3O'))
} else if(acode=='3Pn4RS'){plg<-subset(nafos,ZONE %in% c('3Pn','4R','4S'))
} else if(acode=='2J3KL'){plg<-subset(nafos,ZONE %in% c('2J','3K','3L'))
} else if(acode=='3M'){plg<-subset(nafos,ZONE %in% c('3M'))
} else {
  plg<-data.frame(x=NULL,y=NULL)
  print(subset(dt,select=c('areacode','stockid')))
}
##  plot(plg)
  plgd<-plg %>%
    ##  st_buffer(dist=0.001) %>% # make a buffer of half a meter around all parts (to avoid slivers)
    st_union() %>% # unite to a geometry object
    st_sf() %>%# make the geometry a data frame object
    mutate(centrum = T) # return back the data value
  ##    plot(plgd,col='green',add=TRUE)
  plgd$stockid<-unique(dt$stockid)
  plgd$areacode<-unique(dt$areacode)
  return(plgd)
}
plgr<-ddply(vspeccod,.(stockid),.fun=areafun,.progress='none')
plgr<-st_as_sf(plgr)


xlm<-c(-70,-41)
ylm<-c(38,56)
lw<-0.5

p1<-ggplot()+
  geom_sf(data=plgr,fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill='black',inherit.aes=FALSE,color=NA,lwd=lw)+
  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=2.5)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='white')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')


p2<-ggplot()+
  geom_sf(data=plgr,fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=2.5)+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')

p3<-ggplot()+
  geom_sf(data=plgr,fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill=NA,inherit.aes=FALSE,color='firebrick3',lwd=lw)+
  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=2.5)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='white')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')

p4<-ggplot()+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill=NA,inherit.aes=FALSE,color='firebrick3',lwd=lw)+
  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='red')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')

xlm2<-c(-60,-41)
ylm2<-c(45,56)

p5<-ggplot()+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill=NA,inherit.aes=FALSE,color='firebrick3',lwd=lw)+
  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='red')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')

p6<-ggplot()+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill=NA,inherit.aes=FALSE,color='firebrick3',lwd=lw)+
##  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='red')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')

p7<-ggplot()+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill=NA,inherit.aes=FALSE,color='red',lwd=lw)+
##  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='firebrick3')+
   theme(axis.line.x=element_blank(),
         axis.line.y=element_blank(),
         axis.text.x=element_blank(),
##        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.background=element_blank())+
  xlab('')+
  ylab('')

p8<-ggplot()+
  geom_sf(data=subset(plgr,areacode %in% c('2J3KL')),fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
##  geom_sf(data=wcm,col='black',fill='gray70',inherit.aes = FALSE,lwd=0.001)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
  geom_sf_text(data=subset(plgr,areacode %in% c('2J3KL')),aes(label = areacode),alpha=1,size=3,col='black')+
   theme(axis.line.x=element_blank(),
         axis.line.y=element_blank(),
         axis.text.x=element_blank(),
##        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.background=element_blank())+
  xlab('')+
  ylab('')


figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs/presfigs'
setwd(figsdir)
pdf('crib_cod_example_base.pdf',height=8,width=6)
grid.arrange(p1,p2,p3,ncol=1)
grid.arrange(p4,p5,p6,ncol=1)
grid.arrange(p7,p8,p8,ncol=1)
dev.off()





 
#########################################################
#########################################################


##########################################################

##################################################################
##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir,'GPARAMs.r',sep=''))


(load(paste(datadir,'/rfiles/ZIFF_form.RData',sep='')))
dattsf<-st_as_sf(subset(dat,sciname %in% c('gadus morhua')),coords=c('lon','lat'))
st_crs(dattsf)<-'+proj=longlat +datum=WGS84 +no_defs'

l<-list()
stck<-sort(unique(plgr$stockid))
for(i in 1:length(stck)){
  pl<-subset(plgr,stockid %in% stck[i])
system.time(dint<-st_join(dattsf,pl,join=st_intersects))
a<-subset(dint,is.na(stockid)==FALSE)
points(a$lon,a$lat,pch=16)

dint2<-data.frame(dint)
plot(st_geometry(pl))
}


#####################################################
##SUM OF COD LANDINGS PER HARBOUR FOR EACH STOCK
datt<-subset(dat,sciname %in% c('gadus morhua'))
plgrs<-as(plgr,'Spatial') 
l<-list()
stck<-sort(unique(plgrs$stockid))
for(i in 1:length(stck)){
  pl<-subset(plgrs,stockid %in% stck[i])
dt<-datt
crds2<-SpatialPoints(data.frame(x=dt$lon,y=dt$lat),proj4string=CRS(mct))
dt$stockid<-over(crds2,pl)$stockid
dt<-subset(dt,is.na(stockid)==FALSE)

f<-function(d){  
return(data.frame(weight=sum(d$weight,na.rm=TRUE)))
}
dout<-ddply(dt,.(portland),.fun=f)
dout$stockid<-unique(pl$stockid)
l[[i]]<-dout
}
cweight<-data.frame(do.call('rbind',l))


civi2<-subset(civi,select=c('ziffharbourcode','vuln','expo','sens','adcap'))
cweight2<-left_join(cweight,civi2,by=c('portland'='ziffharbourcode'))


##MEAN CIVI VULN PER STOCK
f<-function(d){
  return(data.frame(vuln=weighted.mean(d$vuln,w=d$weight,na.rm=TRUE),
                    expo=weighted.mean(d$expo,w=d$weight,na.rm=TRUE),
                    sens=weighted.mean(d$sens,w=d$weight,na.rm=TRUE),
                    adcap=weighted.mean(d$adcap,w=d$weight,na.rm=TRUE),
                    vulnmin=min(d$vuln,na.rm=TRUE),
                    vulnmax=max(d$vuln,na.rm=TRUE),
                    vulnsd=sd(d$vuln,na.rm=TRUE),
                    exposd=sd(d$expo,na.rm=TRUE),
                    senssd=sd(d$sens,na.rm=TRUE),
                    adcapsd=sd(d$adcap,na.rm=TRUE)))
}
dout<-ddply(cweight2,.(stockid),.fun=f,.progress='text')

dout$vhigh<-dout$vuln+(1.96*dout$vulnsd)
dout$vlow<-dout$vuln-(1.96*dout$vulnsd)
dout$vhigh<-dout$vulnmax
dout$vlow<-dout$vulnmin
dout<-dout[order(dout$vuln,decreasing=FALSE),]
dout$id<-seq(1,dim(dout)[1],1)
xlm<-c(0,0.8)

figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs/presfigs'
setwd(figsdir)
##pdf.options(encoding='ISOLatin2.enc')
grDevices::cairo_pdf('CIVI_COD_metaplot_allstocks.pdf',height=3.5,width=4.5)
ggplot()+
  geom_vline(xintercept=0.5,linetype='dashed',color='gray50',lwd=0.2)+
  geom_vline(xintercept=0.25,linetype='dashed',color='gray50',lwd=0.2)+
  geom_vline(xintercept=0.75,linetype='dashed',color='gray50',lwd=0.2)+
  geom_segment(data=dout,aes(x=vlow,y=id,xend=vhigh,yend=id),size=0.5,color='gray20')+
  geom_point(data=dout,aes(x=vlow,y=id,color=vlow),pch=-9668,size=2)+
  geom_point(data=dout,aes(x=vhigh,y=id,color=vhigh),pch=-9658,size=2)+
  geom_point(data=dout,aes(x=vuln,y=id,color=vuln),shape=16,size=2.5)+
  geom_point(data=dout,aes(x=vuln,y=id),shape=1,color='white',size=3)+
  scale_color_gradientn(colors=rev(heat.colors(100))[40:100],guide='colourbar',name='',na.value='gray',limits=xlm,breaks=c(xlm[1],xlm[2]))+
##  scale_color_manual(values=c('VH'='firebrick3','H'='orangered','L'='orange1','VL'='gold1'),name='Risk')+
  geom_text(data=dout,aes(x=vlow-.0075,y=id,label=stockid),hjust='right',size=2.5,color='gray20')+
  xlab('')+
  ylab('')+
  scale_x_continuous(expand=c(0,0),breaks=seq(xlm[1],xlm[2],0.2),labels=seq(xlm[1],xlm[2],0.2),limits=c(-0.05,xlm[2]))+
  scale_y_continuous(expand=c(0.08,0.08),breaks=dout$id,labels=NULL)+
  theme_ipsum()+
  guides(colour=guide_legend(title.position="top"))+
  theme(legend.position='none',
        axis.text.x = element_text(size =10),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
       axis.line.y=element_blank(),
       axis.line.x=element_line(color='black',size=0.0001),
        axis.ticks.x=element_line(size=.1),
        ##panel.border=element_rect(colour="black",fill=NA,size=0.2),
        plot.background=element_blank())
dev.off()








###
###
###


stockpoly<-st_read('N:/data/CAFF/data/shapefiles/stocks/stockpoly.shp')
plg<-subset(stockpoly,cname %in% c('atlantic cod'))
plg<-subset(plg,stock %in% c('Atlantic Cod - 2J3KL'))
plot(st_geometry(plg))



##POLYGON FOR EACH SPECIES LANDINGS
##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir2<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir2,'GPARAMs.r',sep=''))





###########################################################################

##SPECIES FOOTPRINT: FUNCTION MAKES BUFFERED POLYGON AROUND LANDINGS FOOTPRINT FOR EACH SPECIES

############################################################################
##d<-subset(datt,portc %in% unique(datt$portc)[1])
f<-function(d){
print(unique(d$sciname))
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
hull<-cbind(hull,unique(subset(d,select=c('homeport','species_code','spgroup','sciname','spec','portc'))))
hull$weight<-sum(d$weight,na.rm=TRUE)
##plot(st_geometry(st_as_sf(hull,crs = mcrt)),add=TRUE,border='green')
##plot(st_geometry(st_as_sf(hull,crs = mcrt)))
 
##l[[i]]<- st_as_sf(hull,crs = mcrt)
return(st_as_sf(hull,crs = mcrt))

}
l<-dlply(datt,.(portc),.fun=f,.progress='text')
portpoly<-do.call('rbind',l)


##MERGE WORMS TAXONOMY TO FLEET POLYGON
portpoly<-merge(portpoly,worms,by.x='sciname',by.y='synonym',all.x=TRUE,all.y=FALSE)

plot(st_geometry(portpoly),col=alpha('red',0.05),border='gray')


#################################################################


#################################################################
portpoly<-st_read('N:/data/CAFF/data/shapefiles/ports/portpoly.shp')
names(portpoly)<-c('sciname','homeport','species_id','spgroup','spec','portc','weight','acceptedname','taxonrank','txnmcst','geometry')
prtp<-subset(portpoly,sciname %in% c('gadus morhua'))
plot(st_geometry(prtp))

(load(paste(datadir,'/rfiles/pspecrisk.RData',sep='')))







##################################################################
##################################################################

##CIVI PLOTS FOR COD EXAMPLE

###################################################################


##################################################################

##LANDED WEIGHT OF COD IN EACH PORT

##################################################################
##LOAD FORMATTED ZIFF LANDINGS DATA (FROM 'ZIFF_import.r')
(load(paste(datadir,'/rfiles/ZIFF_form.RData',sep='')))

##'DAT' IS ZIFF DATA
datt<-subset(dat,sciname %in% c('gadus morhua') &
               nafodiv %in% c('2J','3K','3L'))
nplg2<-subset(nplg,zone %in% c('2J','3K','3L'))
crds2<-SpatialPoints(data.frame(x=datt$lon,y=datt$lat),proj4string=CRS(mct))
datt$nafod<-over(crds2,nplg2)$zone
datt<-subset(datt,is.na(nafod)==FALSE)


plot(datt$lon,datt$lat,pch=16,col='red')
plot(nplg,add=TRUE)

##TOTAL LANDED WEIGHT OF COD IN EACH PORT
f<-function(d){
  return(data.frame(weight=sum(d$weight,na.rm=TRUE)))
}
dattw<-ddply(datt,.(portland),.fun=f,.progress='text')



#################################################################

##IMPORT CIVI

#################################################################
library(compositions)
civi<-read.csv('N:/data/CAFF/data/CIVI/CIVIData_Atlantic_20220920.csv',header=TRUE)
names(civi)<-tolower(names(civi))

##EXPOSURE AS GEOMETRIC MEAN
civi$expo<-ifelse(is.na(civi$siscore)==FALSE & is.na(civi$slcscore)==FALSE & is.na(civi$wsscore)==FALSE & is.na(civi$cmscore)==FALSE,
                  geometricmeanRow(subset(civi,select=c('siscore','slcscore','wsscore','whscore','cmscore'))),
                  NA)

##SENSITIVITY AS GEOMETRIC MEAN
civi$sens<-ifelse(is.na(civi$hcscore)==FALSE & is.na(civi$dop)==FALSE & is.na(civi$trcscore)==FALSE,
                  geometricmeanRow(subset(civi,select=c('hcscore','dop','trcscore'))),
                  NA)

##ADAPTIVITY AS GEOMETRIC MEAN
civi$adcap<-ifelse(is.na(civi$trcscore)==FALSE,
                   geometricmeanRow(subset(civi,select=c('trcscore'))),
                   NA)

##ADAPTIVITY AS GEOMETRIC MEAN
civi$vuln<-ifelse(is.na(civi$expo)==FALSE & is.na(civi$sens)==FALSE & is.na(civi$adcap)==FALSE,
                  geometricmeanRow(subset(civi,select=c('expo','sens','adcap'))),
                  NA)
summary(civi$vuln)
summary(civi$civi)

##RANGE OF DATA
rmin<-1
rmax<-5
##RANGE TO BE MAPPED
tmin<-0
tmax<-1
civi$expo<-(civi$expo-rmin)/(rmax-rmin)*(tmax-tmin)+tmin
civi$sens<-(civi$sens-rmin)/(rmax-rmin)*(tmax-tmin)+tmin
civi$adcap<-(civi$adcap-rmin)/(rmax-rmin)*(tmax-tmin)+tmin
civi$vuln<-(civi$vuln-rmin)/(rmax-rmin)*(tmax-tmin)+tmin


a<-left_join(civi,dattw,by=c('ziffharbourcode'='portland'))
a<-subset(a,is.na(weight)==FALSE)
a$weight<-a$weight/1000
a$lon1<-as.numeric(rep(-46,dim(a)[1]))
a$lat1<-as.numeric(rep(54,dim(a)[1]))

library(rnaturalearth)
wcm<-ne_download(scale = 50, type = 'land', category = 'physical',returnclass='sf')
nplg3<-st_buffer(st_union(st_as_sf(nplg2)),0.05)

xlm<-c(-67,-41)
ylm<-c(42,60)
lw<-0.5

p1<-ggplot()+
  geom_sf(data=nplg3,fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=lw))+
  xlab('')+
  ylab('')

xlm2<-c(-60,-45)
ylm2<-c(45.75,54)

p2<-ggplot()+
##  geom_tile(data=d,aes(x=lon,y=lat,fill=y),color='black')+
##  geom_hline(yintercept=seq(ylm[1],ylm[2],1), linetype="solid",lwd=0.0001,color='black')+
##  geom_vline(xintercept=seq(xlm[1],xlm[2],1), linetype="solid",lwd=0.0001,color='black')+
##  geom_sf(data=nplg3,fill=NA,inherit.aes=FALSE,color='black',lwd=lw)+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
  geom_segment(data=a,aes(x=lon1,xend=long,y=lat1,yend=lat),size=0.25,alpha=0.15,col='firebrick4')+
##  geom_curve(data=a,aes(x=lon1,xend=long,y=lat1,yend=lat,size=weight),alpha=0.25,col='royalblue3',ncp=1,curvature=-0.7,angle=20)+
  geom_point(data=a,aes(x=long,y=lat,size=weight),col='firebrick4',alpha=0.5)+
  scale_size(range=c(0.25,3))+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
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
  ylab('')



clp<-kovesi.rainbow(100)
clp<-brewer.blues(100)
p3<-ggplot()+
##  geom_sf(data=nplg3,fill=NA,inherit.aes=FALSE,color='black',lwd=0.5)+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
  geom_point(data=subset(a,is.na(vuln)==FALSE),aes(x=long,y=lat,col=vuln),size=1)+
##  scale_size(range=c(1,7))+
scale_color_gradientn(colors=clp,limits=c(0,0.6),breaks=c(0,0.6),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
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
  ylab('')




figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs/presfigs'
setwd(figsdir)
pdf('civi_cod_example.pdf',height=8,width=6)
grid.arrange(p1,p2,p3,ncol=1)
dev.off()
    
dcivi<-data.frame(vuln=weighted.mean(a$vuln,w=a$weight,na.rm=TRUE),
                  sens=weighted.mean(a$sens,w=a$weight,na.rm=TRUE),
                  expo=weighted.mean(a$expo,w=a$weight,na.rm=TRUE),
                  adcap=weighted.mean(a$adcap,w=a$weight,na.rm=TRUE),
                  minvuln=min(a$vuln,na.rm=TRUE),
                  maxvuln=max(a$vuln,na.rm=TRUE),
                  minsens=min(a$sens,na.rm=TRUE),
                  maxsens=max(a$sens,na.rm=TRUE),
                  minexpo=min(a$expo,na.rm=TRUE),
                  maxexpo=max(a$expo,na.rm=TRUE),
                  minadcap=min(a$adcap,na.rm=TRUE),
                  maxadcap=max(a$adcap,na.rm=TRUE),
                  sdvuln=sd(a$vuln,na.rm=TRUE),
                  sdsens=sd(a$sens,na.rm=TRUE),
                  sdexpo=sd(a$expo,na.rm=TRUE),
                  sdadcap=sd(a$adcap,na.rm=TRUE))
dcivi1<-gather(subset(dcivi,select=c('vuln','sens','expo','adcap')),key=dimension,value='y')
dcivi2<-gather(subset(dcivi,select=c('sdvuln','sdsens','sdexpo','sdadcap')),key=dimension,value='sdy')

dcivil<-cbind(dcivi1,subset(dcivi2,select=c('sdy')))
dcivil$upy<-dcivil$y+(1.96*dcivil$sdy)
dcivil$dny<-dcivil$y-(1.96*dcivil$sdy)

dcivil$id<-seq(1,4,1)
dcivil$cl<-c('black','royalblue3','firebrick3','gold')

xlm<-c(0,0.9)
setwd(figsdir)
##pdf.options(encoding='ISOLatin2.enc')
grDevices::cairo_pdf('civi_cod__metaplot.pdf',height=3,width=4)
ggplot()+
##  geom_vline(xintercept=thdatsv$tmed,linetype='dashed',color='gray50',lwd=0.2)+
##  geom_vline(xintercept=thdatsv$thigh,linetype='dashed',color='gray50',lwd=0.2)+
##  geom_vline(xintercept=thdatsv$tlow,linetype='dashed',color='gray50',lwd=0.2)+
  geom_segment(data=dcivil,aes(x=dny,y=id,xend=upy,yend=id),size=0.5,color='gray60')+
  geom_point(data=dcivil,aes(x=dny,y=id,color=cl),pch=-9668,size=2)+
  geom_point(data=dcivil,aes(x=upy,y=id,color=cl),pch=-9658,size=2)+
  geom_point(data=dcivil,aes(x=y,y=id,color=cl),shape=16,size=2.5)+
  geom_point(data=dcivil,aes(x=y,y=id),shape=1,color='white',size=3)+
  scale_color_identity(name='Risk')+
##  scale_color_manual(values=c('firebrick'='firebrick3','H'='orangered','L'='orange1','VL'='gold1'),name='Risk')+
##  geom_text(data=b,aes(x=minvuln-.0075,y=id,label=areacode),hjust='right',size=2.5,color='gray20')+
  xlab('')+
  ylab('')+
  scale_x_continuous(expand=c(0,0),breaks=seq(xlm[1],xlm[2],0.2),labels=seq(xlm[1],xlm[2],0.2),limits=xlm)+
  scale_y_continuous(expand=c(0.09,0.09),breaks=dcivil$id,labels=c('Vulnerability','Sensitivity','Exposure','Adaptivity'))+
  theme_ipsum()+
  guides(colour=guide_legend(title.position="top"))+
  theme(legend.position='none',
        axis.text.x = element_text(size =12),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y = element_text(size =10),
##        axis.line=element_line(color='black',size=0.0001),
        axis.ticks.x=element_line(size=.01),
##        axis.ticks.y=element_line(size=.1),
        ##panel.border=element_rect(colour="black",fill=NA,size=0.2),
        plot.background=element_blank())
dev.off()






###########################################################
###########################################################

##CRIB EXAMPLE FOR COD STOCK 

############################################################

##'DAT' IS ZIFF DATA
vdatac<-subset(vdata,comname %in% c('Atlantic cod') & rcp %in% c('8.5'))
nplg2<-subset(nplg,zone %in% c('2J','3K','3L'))
nplg3<-st_buffer(st_union(st_as_sf(nplg2)),0.09)
nplg2<-as(nplg3,'Spatial')
crds2<-SpatialPoints(data.frame(x=vdatac$lon,y=vdatac$lat),proj4string=CRS(mct))
##vdatac$nafod<-over(crds2,nplg2)
##vdatac<-subset(vdatac,is.na(nafod)==FALSE)


plot(vdatac$lon,vdatac$lat,pch=16,col='red')
map.axes()
plot(nplg,add=TRUE)


##xlm2<-c(-60,-45)
##ylm2<-c(45,56)


xlm2<-c(-69,-43)
ylm2<-c(42,57)
vdatac$id<-ifelse(vdatac$lon==-51.625 & vdatac$lat==48.375,1,0)
vdatac$id<-as.character(vdatac$id)

p0<-ggplot()+
  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=id),color=alpha('gray20',alpha=0.5),lwd=0.000001)+
##  geom_point(data=z,aes(x=lon,y=lat),pch=22,,color='red',fill='red')+
##  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  geom_sf(data=wcm,col=NA,fill='gray21',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
##            scale_fill_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
  scale_fill_manual(values=c('0'='gray90','1'='red'),name='')+
   theme(axis.line=element_blank(),axis.text.x=element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
          legend.position="none",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(1, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="white",fill=NA,size=0.5))+
  xlab('')+
  ylab('')
p0

p1<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=vuln),col=alpha('white',alpha=0.5),lwd=0.00001)+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=vuln))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=kovesi.rainbow(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
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
  ylab('')


vdatac$vcat<-factor(vdatac$vcat,levels=c('VH','H','L','VL'))
p2<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=vcat),col='white')+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=vcat))+
  scale_fill_manual(values=c('VH'='darkred','H'='orangered','L'='orange1','VL'='gold1'),name='')+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
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
  ylab('')


vdatac$sens<-ifelse(vdatac$sens>0.75,0.75,vdatac$sens)
p3<-ggplot()+
##  geom_tile(data=vdatac,aes(x=lon,y=lat,fill=vuln),col=alpha('white',alpha=0.5),lwd=0.00001)+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=sens))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=brewer.blues(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.75),breaks=c(0.25,0.75))+
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
  ylab('')


p4<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=expo))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
            scale_fill_gradientn(colors=brewer.reds(100),guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.25,0.60),breaks=c(0.25,0.60))+
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
  ylab('')

vdatac$adcap<-ifelse(vdatac$adcap>0.6,0.6,vdatac$adcap)
p5<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=adcap))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=rev(heat.colors(100))[5:60],guide='colourbar',name='',na.value=clb,trans='log10',limits=c(0.45,0.6),breaks=c(0.45,0.6))+
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
  ylab('')


p6<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=E.toe.raw))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=rev(viridis(100)),guide='colourbar',name='',na.value=clb,limits=c(55,80),breaks=c(55,80))+
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
  ylab('')


p7<-ggplot()+
  geom_raster(data=vdatac,aes(x=lon,y=lat,fill=TSMr))+
  geom_sf(data=wcm,col=NA,fill='gray70',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm2)+
  scale_y_continuous(expand=c(0,0),limits=ylm2)+
##scale_fill_gradientn(colors=clp,limits=c(0,1), breaks=c(0,1),name='')+
##  guides(fill = guide_colourbar(barwidth = 12, barheight = 0.5))+
            scale_fill_gradientn(colors=rev(viridis(100)),guide='colourbar',name='',na.value=clb,limits=c(-3,9),breaks=c(-3,9))+
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
  ylab('')




setwd(figsdir)
pdf('crib_cod_example.pdf',height=8,width=6)
grid.arrange(p1,p2,p6,ncol=1)
grid.arrange(p3,p4,p5,ncol=1)
grid.arrange(p7,p7,p7,ncol=1)
dev.off()







