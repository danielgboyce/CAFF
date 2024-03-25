library(sf)
library(scales)
library(taxize)
library(ggridges)
library(hrbrthemes)
library(rnaturalearth)
library(Hmisc)
library(grid)
library(fossil)
library(gstat)
library(sp)
library(maptools)
library(rgeos)
library(mapdata)
library(rgdal)
library(landscapemetrics)
library(viridis)
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
library(circular)
library(abind)
library(DescTools)
library(ncdf4)
library(foreign)
library(stringdist)
library(fuzzyjoin)
library(ggrepel)
library(ggExtra)
library(stringi)
library(stringr)
library(rcompanion)
library(bestNormalize)
library(R.matlab)
library(raster)

datadir<-'N:/data/CAFF/data'
mcrt<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#HIGH RESOLUTION COASTLINE
##cst<-readOGR('N:/data/shapefiles/naturalearthdata_ne_10m_ocean/v4.1',layer='ne_10m_ocean')
##cst<-crop(cst,extent(-90,-30,35,80),proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
cst<-st_read('N:/data/shapefiles/naturalearthdata_ne_10m_ocean/v4.1/ne_10m_ocean.shp')
cst<-st_transform(cst,mcrt)
cst<-st_make_valid(cst)
cst<-st_crop(cst,c(xmin=-90,xmax=-30,ymin=35,ymax=80))
##plot(st_geometry(cst))


##AVERAGE VULN/RISK STATS
riskfun<-function(b){
##CELLS WITH >0 MISSING INDEX OF 12
dmiss<-subset(b,is.na(vulnsd)==TRUE)
##PROPORTION OF ALL CELLS THAT ARE MISSING >0 INDEX OF 12
pmiss<-(dim(dmiss)[1]/dim(b)[1])*100
a<-(data.frame(pmiss=pmiss,
               S.HII=mean(b$S.HII,na.rm=TRUE),
               S.TSMr=mean(b$S.TSMr,na.rm=TRUE),
               S.vind=mean(b$S.vind,na.rm=TRUE),
               S.rlstatus=mean(b$S.rlstatus,na.rm=TRUE),
               E.nrchng=mean(b$E.nrchng),
               E.plost=mean(b$E.plost,na.rm=TRUE),
               E.toe=mean(b$E.toe,na.rm=TRUE),
               E.vel=mean(b$E.vel,na.rm=TRUE),
               AC.hfrag=mean(b$AC.hfrag,na.rm=TRUE),
               AC.hrange=mean(b$AC.hrange,na.rm=TRUE),
               AC.lmax=mean(b$AC.lmax,na.rm=TRUE),
               AC.tvar=mean(b$AC.tvar,na.rm=TRUE),
               adcap=wtd.mean(b$adcap,w=1/(b$adcap.sd/b$adcap),na.rm=TRUE),
  sens=wtd.mean(b$sens,w=1/(b$sens.sd/b$sens),na.rm=TRUE),
  expo=wtd.mean(b$expo,w=1/(b$expo.sd/b$expo),na.rm=TRUE),
  vuln=wtd.mean(b$vuln,w=1/(b$vulnsd/b$vuln),na.rm=TRUE),
  adcapsd=sqrt(wtd.var(b$adcap,w=1/(b$adcap.sd/b$adcap),na.rm=TRUE)),
  senssd=sqrt(wtd.var(b$sens,w=1/(b$sens.sd/b$sens),na.rm=TRUE)),
  exposd=sqrt(wtd.var(b$expo,w=1/(b$expo.sd/b$expo),na.rm=TRUE)),
  vulnsd=sqrt(wtd.var(b$vuln,w=1/(b$vulnsd/b$vuln),na.rm=TRUE)),
  vuln.uw=mean(b$vuln.uw,na.rm=TRUE),
  n=dim(b)[1]))
return(a)
}


##FUNCTION CALCULATES RISK CATEGORIES FROM THRESHOLDS
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

  return(x)
}
