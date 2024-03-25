library(rgdal)
library(raster)
library(plyr)
library(sf)
library(usdm)

datadir<-'N:/data/CAFF/data/CMIP_CIS_comparison/Data'
setwd(datadir)
(load("CISMeanStack.Rdata"))
(load("CNRMMeanstack.Rdata"))
(load("GFDLMeanstack.Rdata"))
(load("HadMeanstack.Rdata"))

cis<-CISMean_stack
cnrm<-CNRMMean_stack
gfdl<-GFDLMean_stack
had<-HADMean_stack

plot(subset(had,1))
a<-subset(cis,3)
plot(a)
vg<-Variogram(a,cutoff=150)
plot(vg)

install.packages('geoR')
library(geoR)
xy<-data.frame(coordinates(cis))
##    xy$x<-ifelse(xy$x>180,-360+xy$x,xy$x)
xy$cell<-gsub(' ','',paste(xy$x,'_',xy$y))
dbx<-data.frame(values(cis),
               cell=xy$cell,
               lon=xy$x,
               lat=xy$y)

xy<-data.frame(coordinates(cis))
db<-data.frame(z=values(subset(cis,1)),
               lon=xy$x,
               lat=xy$y)
db<-na.omit(db)
crds<-matrix(0,dim(db)[1],2)
crds[,1]<-db$lon
crds[,2]<-db$lat
gb<-list(data=db$z,
         coords=crds)
vg<-variog(gb,max.dist=1e5)
plot(vg)

crds<-matrix(0,dim(xy)[1],2)
crds[,1]<-xy$x
crds[,2]<-xy$y
gb<-list(data=na.omit(values(subset(cis,3))),
         coords=crds)
vg<-variog(gb,max.dist=1e5)


xy<-data.frame(coordinates(cis))
##    xy$x<-ifelse(xy$x>180,-360+xy$x,xy$x)
xy$cell<-gsub(' ','',paste(xy$x,'_',xy$y))
dby<-data.frame(values(cis),
               cell=xy$cell,
               lon=xy$x,
               lat=xy$y)
    
a<-expand.grid(dbx$cell,dbx$cell)