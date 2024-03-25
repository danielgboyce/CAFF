##LOAD REQUIRED PACKAGES, PATHNAMES, PARAMETERS
codedir<-ifelse(grepl('sailfish',getwd(), fixed = TRUE),
                'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/',
                'C:/Users/danie/Documents/aalldocuments/literature/research/active/CAFF/code/')
source(paste(codedir,'GPARAMs.r',sep=''))


##BASEMAPS
wch<-ne_download(scale = 10, type = 'land', category = 'physical',returnclass='sf')
wcm<-ne_download(scale = 50, type = 'land', category = 'physical',returnclass='sf')
wcl<-ne_download(scale = 110, type = 'land', category = 'physical',returnclass='sf')
##wc <- st_transform(wc, crs=mcrt)
##st_bbox(wc)

clb<-'gray60'
##COLOURS FOR RISK CATEGORIES
clvh<-'gray60'
clh<-'gray70'
cll<-'gray80'
clvl<-'gray90'

######################################################################
##IMPORT VULNERABILITY SCORES
######################################################################
(load('N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/vdata_form.RData'))

######################################################################
##SHAPEFILE OF STOCK BOUNDARIES
######################################################################
stockpoly<-st_read('N:/data/CAFF/data/shapefiles/stocks/stockpoly.shp')
stockpoly<-st_read('N:/data/CAFF/data/shapefiles/stocks/stockpoly_nobuffer.shp')


##GEOMETRIC MEAN
##exp(mean(log(c(5,5,3))))
##geometricmeanRow(subset(civi,select=c('cmscore','siscore','slcscore','wsscore'))[2,])

######################################################################
##CIVI DATA FORMATTED BY PHIL
######################################################################
library(compositions)
civi<-read.csv('N:/data/CIVI/CIVIDataForWebsite_WithDataDictionary_20200831.csv',header=TRUE)
names(civi)<-tolower(names(civi))
##civi$ww<-rowMeans(subset(civi,select=c('wsscore','whscore')))
##civi$expoa<-rowMeans(subset(civi,select=c('siscore','slcscore','wsscore','whscore','cmscore')))

##EXPOSURE AS GEOMETRIC MEAN (sea ice, sea level)
civi$expo<-ifelse(is.na(civi$siscore)==FALSE & is.na(civi$slcscore)==FALSE,
                  geometricmeanRow(subset(civi,select=c('siscore','slcscore'))),
                  NA)

##SENSITIVITY AS GEOMETRIC MEAN (harbour condition, degree of protection, replacementn cost)
civi$sens<-ifelse(is.na(civi$hcscore)==FALSE & is.na(civi$dop)==FALSE & is.na(civi$cmscore)==FALSE,
                  geometricmeanRow(subset(civi,select=c('hcscore','dop','cmscore'))),
                  NA)


##ADAPTIVITY AS GEOMETRIC MEAN (TOTAL REPLACEMENT COST, QUANTITY FISH LANDED); PERCENT INCOME FROM FISHING COULD BE REPLACED WITH LANDED SPECIES VALUE DIVERSITY
civi$adcap<-ifelse(is.na(civi$trcscore)==FALSE & is.na(civi$quantscore) & is.na(civi$perscore),
                   geometricmeanRow(subset(civi,select=c('trcscore','quantscore','perscore'))),
                   NA)

##VULNERABILITY AS GEOMETRIC MEAN
civi$vuln<-ifelse(is.na(civi$expo)==FALSE & is.na(civi$sens)==FALSE & is.na(civi$adcap)==FALSE,
                  geometricmeanRow(subset(civi,select=c('expo','sens','adcap'))),
                  NA)
summary(civi$vuln)
summary(civi$civi)


##STANDARDIZES INDICES FROM 0-1
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



    
library(scales)
mapcl<-'gray95'
cxlm<-c(-68,-57)
cylm<-c(42,48)
zlm<-c(0.2,0.8)

mapcl<-'gray75'
mapcl<-'black'
clb<-'white'
cls<-'gray85'

d<-subset(vdata,rcp %in% c('8.5') & comname %in% c('American lobster'))
p1<-ggplot()+
         geom_tile(data=d,aes(x=lon,y=lat),fill=cls,color='black',lwd=0.0000001,stroke=0.0000001)+
         geom_sf(data=wcm,fill=mapcl,inherit.aes=FALSE,colour=NA,lwd=0.2,stroke=0.2)+
         coord_sf(xlim=cxlm,ylim=cylm, expand=FALSE,clip='on')+
         scale_x_continuous(expand=c(0,0),breaks=c(cxlm[1]+0.01,cxlm[2]-0.01),labels=c(cxlm[1],cxlm[2]))+
         scale_y_continuous(expand=c(0,0),breaks=c(cylm[1]+0.1,cylm[2]-0.1),labels=c(cylm[1],cylm[2]))+
    theme(panel.grid.major=element_blank(),
          panel.background = element_rect(fill=clb),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.text.y   = element_text(size=8),
          axis.text.x   = element_text(size=8),
          axis.title.y  = element_text(size=8),
          axis.title.x  = element_text(size=8),
          axis.line=element_line(color='black',size=0.0001),
          legend.position="right",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(0.75, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
          text = element_text(size=10),
          axis.ticks.x=element_line(size=.1),
          axis.ticks.y=element_line(size=.1),
          panel.border=element_rect(color="black",fill=NA,size=0.5))+
    xlab('')+
ylab('')



d<-subset(civi,province %in% c('NS','NB'))
p2<-ggplot()+
         geom_sf(data=wcm,fill=mapcl,inherit.aes=FALSE,colour=NA,lwd=0.2,stroke=0.2)+
         geom_point(data=d,aes(x=long,y=lat),fill=cls,color='gray25',pch=21,size=2,lwd=0.0000001,stroke=0.0000001)+
         coord_sf(xlim=cxlm,ylim=cylm, expand=FALSE,clip='on')+
         scale_x_continuous(expand=c(0,0),breaks=c(cxlm[1]+0.01,cxlm[2]-0.01),labels=c(cxlm[1],cxlm[2]))+
         scale_y_continuous(expand=c(0,0),breaks=c(cylm[1]+0.1,cylm[2]-0.1),labels=c(cylm[1],cylm[2]))+
    theme(panel.grid.major=element_blank(),
          panel.background = element_rect(fill=clb),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.text.y   = element_text(size=8),
          axis.text.x   = element_text(size=8),
          axis.title.y  = element_text(size=8),
          axis.title.x  = element_text(size=8),
          axis.line=element_line(color='black',size=0.0001),
          legend.position="right",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(0.75, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
          text = element_text(size=10),
          axis.ticks.x=element_line(size=.1),
          axis.ticks.y=element_line(size=.1),
          panel.border=element_rect(color="black",fill=NA,size=0.5))+
    xlab('')+
ylab('')


plg<-subset(polymar, stock %in% c('Lobster - Inshore LFA 27-33'))
sf_use_s2(FALSE)
sf_use_s2()
##plg<-st_set_precision(plg, 0.001)
plgb<-st_buffer(plg,0.05)
sf_use_s2(TRUE)

p3<-ggplot()+
  geom_sf(data=plgb,fill=cls,color='black')+
         geom_sf(data=wcm,fill=mapcl,inherit.aes=FALSE,colour='black',lwd=0.2,stroke=0.2)+
         coord_sf(xlim=cxlm,ylim=cylm, expand=FALSE,clip='on')+
         scale_x_continuous(expand=c(0,0),breaks=c(cxlm[1]+0.01,cxlm[2]-0.01),labels=c(cxlm[1],cxlm[2]))+
         scale_y_continuous(expand=c(0,0),breaks=c(cylm[1]+0.1,cylm[2]-0.1),labels=c(cylm[1],cylm[2]))+
    theme(panel.grid.major=element_blank(),
          panel.background = element_rect(fill=clb),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.text.y   = element_text(size=8),
          axis.text.x   = element_text(size=8),
          axis.title.y  = element_text(size=8),
          axis.title.x  = element_text(size=8),
          axis.line=element_line(color='black',size=0.0001),
          legend.position="right",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(0.75, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
          text = element_text(size=10),
          axis.ticks.x=element_line(size=.1),
          axis.ticks.y=element_line(size=.1),
          panel.border=element_rect(color="black",fill=NA,size=0.5))+
    xlab('')+
ylab('')


setwd(figsdir)
pdf('methods_schem.pdf',height=10,width=8)
grid.arrange(p1,p2,p3)
dev.off()




polymar<-subset(stockpoly,region %in% c('Maritimes'))

ggplot()+
  geom_sf(data=polymar,fill='dodgerblue3',alpha=0.1,color='dodgerblue4')+
  geom_sf(data=wch,fill='white',inherit.aes=FALSE,colour='gray20',lwd=0.0001,stroke=0.2)+
           coord_sf(xlim=cxlm,ylim=cylm, expand=FALSE,clip='on')+
         scale_x_continuous(expand=c(0,0),breaks=c(cxlm[1]+0.01,cxlm[2]-0.01),labels=c(cxlm[1],cxlm[2]))+
         scale_y_continuous(expand=c(0,0),breaks=c(cylm[1]+0.1,cylm[2]-0.1),labels=c(cylm[1],cylm[2]))+
facet_wrap(~stock,nrow=3)
  
##polymar<-subset(stockpoly,cname %in% c('swordfish'))
ggplot()+
  geom_sf(data=polymar,fill='dodgerblue3',alpha=0.1,color='dodgerblue4')+
  geom_sf(data=wch,fill='white',inherit.aes=FALSE,colour='gray20',lwd=0.0001,stroke=0.2)+
           coord_sf(xlim=cxlm,ylim=cylm, expand=FALSE,clip='on')+
         scale_x_continuous(expand=c(0,0),breaks=c(cxlm[1]+0.01,cxlm[2]-0.01),labels=c(cxlm[1],cxlm[2]))+
         scale_y_continuous(expand=c(0,0),breaks=c(cylm[1]+0.1,cylm[2]-0.1),labels=c(cylm[1],cylm[2]))+
    theme(panel.grid.major=element_blank(),
          panel.background = element_rect(fill=clb),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.text.y   = element_text(size=8),
          axis.text.x   = element_text(size=8),
          axis.title.y  = element_text(size=8),
          axis.title.x  = element_text(size=8),
          axis.line=element_line(color='black',size=0.0001),
          legend.position="right",
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(0.75, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
          text = element_text(size=10),
          axis.ticks.x=element_line(size=.1),
          axis.ticks.y=element_line(size=.1),
          panel.border=element_rect(color="black",fill=NA,size=0.5))+
    xlab('')+
ylab('')
 

  
pfun<-function(d){
  names(d)[1]<-c('y')
  d<-na.omit(d)
  d$y<-ifelse(d$y>zlm[2],zlm[2],d$y)
  d$y<-ifelse(d$y<zlm[1],zlm[1],d$y)
  d<-d[order(d$y,decreasing=FALSE),]
  return(
    
    ggplot()+
##         geom_sf(data=plgr,fill=NA,inherit.aes=FALSE,colour=alpha('black',0.2),lwd=0.0001,stroke=0.000001)+
         geom_sf(data=wch,fill=mapcl,inherit.aes=FALSE,colour=NA,lwd=0.2,stroke=0.2)+
         geom_point(data=d,aes(x=long,y=lat,color=y),pch=16,size=2,lwd=0.0000001,stroke=0.0000001)+
         geom_sf(data=wch,fill=NA,inherit.aes=FALSE,colour=alpha('black',0.1),lwd=0.0001,stroke=0.0001)+
         coord_sf(xlim=cxlm,ylim=cylm, expand=FALSE,clip='on')+
         scale_x_continuous(expand=c(0,0),breaks=c(cxlm[1]+0.01,cxlm[2]-0.01),labels=c(cxlm[1],cxlm[2]))+
         scale_y_continuous(expand=c(0,0),breaks=c(cylm[1]+0.1,cylm[2]-0.1),labels=c(cylm[1],cylm[2]))+
##         geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=3)+
    theme(panel.grid.major=element_blank(),
##          panel.background = element_rect(fill=clb),
          panel.background = element_rect(fill=clb),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.text.y   = element_text(size=8),
          axis.text.x   = element_text(size=8),
          axis.title.y  = element_text(size=8),
          axis.title.x  = element_text(size=8),
          axis.line=element_line(color='black',size=0.0001),
          legend.position="right",
##          legend.key.size =  unit(0.15, "in"),
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(0.75, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
          text = element_text(size=10),
          axis.ticks.x=element_line(size=.1),
          axis.ticks.y=element_line(size=.1),
          panel.border=element_rect(color="black",fill=NA,size=0.5))+
    xlab('')+
ylab(''))
}

library(ggpubr)
pl<-get_palette(c('white','yellow','gold','gold1','gold4'),20)
pa<-pfun(subset(civi,select=c('adcap','long','lat')))+
  scale_color_gradientn(colours=pl,limits=zlm,breaks=zlm)+
  ggtitle('Adaptivity')

ps<-pfun(subset(civi,select=c('sens','long','lat')))+
  scale_color_gradientn(colours=brewer.blues(100),limits=zlm,breaks=zlm)+
  ggtitle('Sensitivity')

pe<-pfun(subset(civi,select=c('expo','long','lat')))+
  scale_color_gradientn(colours=brewer.reds(100),limits=zlm,breaks=zlm)+
  ggtitle('Exposure')

pv<-pfun(subset(civi,select=c('vuln','long','lat')))+
  scale_color_gradientn(colours=kovesi.rainbow(100),limits=zlm,breaks=zlm)+
  ggtitle('Vulnerability')

setwd(figsdir)
pdf('CIVI_dims_maps_V2_R1.pdf',height=8,width=8)
grid.arrange(ps,pa,pe,pv,ncol=2, nrow=2)
dev.off()






library(marmap)
bth<-getNOAA.bathy(lon1=mxlm[1],lon2=mxlm[2],lat1=mylm[1],lat2=mylm[2], resolution=12)
mxlm<-c(-80,-42)
mylm<-c(39,80)
cxlm<-c(-70,mxlm[2])
cylm<-c(40,56)
polcl<-'gray10'
mapcl<-'white'
mapcl<-'gray75'
zlm<-c(0.2,0.8)
 
library(scales)
mxlm<-c(-80,-42)
mylm<-c(39,80)
cxlm<-c(-70,-50)
cylm<-c(43,56)
mapcl<-'gray95'
cxlm<-c(-70,mxlm[2])
cxlm<-c(-70,-50)
cylm<-c(42,56)
zlm<-c(0.2,0.8)

mapcl<-'gray75'
  
pfun<-function(d){
  names(d)[1]<-c('y')
  d<-na.omit(d)
  d$y<-ifelse(d$y>zlm[2],zlm[2],d$y)
  d$y<-ifelse(d$y<zlm[1],zlm[1],d$y)
  d<-d[order(d$y,decreasing=FALSE),]
  return(
    
    ggplot()+
##         geom_sf(data=plgr,fill=NA,inherit.aes=FALSE,colour=alpha('black',0.2),lwd=0.0001,stroke=0.000001)+
         geom_sf(data=wch,fill=mapcl,inherit.aes=FALSE,colour=NA,lwd=0.2,stroke=0.2)+
         geom_point(data=d,aes(x=long,y=lat,color=y),pch=16,size=2,lwd=0.0000001,stroke=0.0000001)+
         geom_sf(data=wch,fill=NA,inherit.aes=FALSE,colour=alpha('black',0.1),lwd=0.0001,stroke=0.0001)+
         coord_sf(xlim=cxlm,ylim=cylm, expand=FALSE,clip='on')+
         scale_x_continuous(expand=c(0,0),breaks=c(cxlm[1]+0.01,cxlm[2]-0.01),labels=c(cxlm[1],cxlm[2]))+
         scale_y_continuous(expand=c(0,0),breaks=c(cylm[1]+0.1,cylm[2]-0.1),labels=c(cylm[1],cylm[2]))+
##         geom_sf_text(data=plgr,aes(label = areacode),alpha=1,size=3)+
    theme(panel.grid.major=element_blank(),
##          panel.background = element_rect(fill=clb),
          panel.background = element_rect(fill=clb),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.text.y   = element_text(size=8),
          axis.text.x   = element_text(size=8),
          axis.title.y  = element_text(size=8),
          axis.title.x  = element_text(size=8),
          axis.line=element_line(color='black',size=0.0001),
          legend.position="right",
##          legend.key.size =  unit(0.15, "in"),
          legend.key.width =  unit(0.25, "cm"),
          legend.key.height =  unit(0.75, "cm"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
          text = element_text(size=10),
          axis.ticks.x=element_line(size=.1),
          axis.ticks.y=element_line(size=.1),
          panel.border=element_rect(color="black",fill=NA,size=0.5))+
    xlab('')+
ylab(''))
}

library(ggpubr)
pl<-get_palette(c('white','yellow','gold','gold1','gold4'),20)
pa<-pfun(subset(civi,select=c('adcap','long','lat')))+
  scale_color_gradientn(colours=pl,limits=zlm,breaks=zlm)+
  ggtitle('Adaptivity')

ps<-pfun(subset(civi,select=c('sens','long','lat')))+
  scale_color_gradientn(colours=brewer.blues(100),limits=zlm,breaks=zlm)+
  ggtitle('Sensitivity')

pe<-pfun(subset(civi,select=c('expo','long','lat')))+
  scale_color_gradientn(colours=brewer.reds(100),limits=zlm,breaks=zlm)+
  ggtitle('Exposure')

pv<-pfun(subset(civi,select=c('vuln','long','lat')))+
  scale_color_gradientn(colours=kovesi.rainbow(100),limits=zlm,breaks=zlm)+
  ggtitle('Vulnerability')

setwd(figsdir)
pdf('CIVI_dims_maps_V2_R1.pdf',height=8,width=8)
grid.arrange(ps,pa,pe,pv,ncol=2, nrow=2)
dev.off()







##
##
##
setwd(figsdir)
##pdf('basemap_for_schematic_R4.pdf',height=7,width=9)
xlm<-c(-67,-52)
ylm<-c(42,52)
map('world',xlim=xlm,ylim=ylm)
map.axes()

##GET FOR ALL NATL
spe2<-subset(vdata, rcp %in% c('8.5') & lon<xlm[2] & lon> xlm[1] & lat> ylm[1] & lat< ylm[2])

a<-subset(spe2,speciesid %in% unique(spe2$speciesid)[91:100])
ggplot()+
  geom_raster(data=a,aes(x=lon,y=lat,fill=vuln))+
  facet_wrap(~comname)
'Flying gurnard','Winter flounder','Bullet tuna','Atlantic bonito'

##a<-str_subset(string=unique(spe2$comname),pattern='Caribbean')
sp<-c('Flying gurnard')
sp<-c('American lobster')
##b<-subset(spe2,comname %in% a)

spe2<-subset(spe2,is.na(vuln)==FALSE & comname %in% sp)
##spe2$vuln<-rescale(log10(spe2$vuln),newrange=c(0.01,0.99))
##spe2$sens<-rescale((spe2$sens),newrange=c(0.02,0.98))
##spe2$expo<-rescale(log10(spe2$expo),newrange=c(0.02,0.98))
##spe2$adcap<-rescale((spe2$adcap),newrange=c(0.02,0.98))
spe2$vuln<-rescale(log10(spe2$vuln),newrange=c(0.01,0.9))
spe2$sens<-rescale((spe2$sens),newrange=c(0.02,0.9))
spe2$expo<-rescale(log10(spe2$expo),newrange=c(0.02,0.9))
spe2$adcap<-rescale((spe2$adcap),newrange=c(0.02,0.9))

##GET FOR SINGLE EXAMPLE SPECIES
spe<-subset(vdata, lon<xlm[2] & lon> xlm[1] & lat> ylm[1] & lat< ylm[2])
##a<-str_subset(string=unique(spe$comname),pattern='Caribbean')
##spe<-subset(spe,comname %in% 'Caribbean reef shark')
spe<-subset(spe,is.na(vuln)==FALSE & comname %in% sp)


pfun2<-function(cl){
return(ggplot()+
  geom_tile(data=spe,aes(x=lon,y=lat),fill=cl,color='black')+
  geom_sf(data=wcm,col=NA,fill='black',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background = element_rect(fill = 'white'),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
ylab(''))
}
p1<-pfun2('gray70')
p1
p2<-pfun2('mediumseagreen')
p3<-pfun2('thistle')
p4<-pfun2('lightgreen')
p6<-pfun2('pink')
p6<-pfun2('slategray2')
grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)
##grid.arrange(p5,p6,ncol=2,nrow=2)

p2<-ggplot()+
  geom_hline(yintercept=seq(ylm[1],ylm[2],1), linetype="solid",lwd=0.0001,color='gray5')+
  geom_vline(xintercept=seq(xlm[1],xlm[2],1), linetype="solid",lwd=0.0001,color='gray5')+
  geom_sf(data=wch,col=NA,fill='white',inherit.aes = FALSE)+
  ##    scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray75'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.key.size =  unit(0.1, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')

grid.arrange(p1,p2,p1,p2,ncol=2,nrow=2)

ggplot()+
    geom_sf(data=wcl,col=NA,fill='white',inherit.aes=FALSE)+
    coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
    scale_x_continuous(expand=c(0,0),breaks=c(-179.8,-90,0,90,179.8),labels=seq(mxlm[1],mxlm[2],90))+
    scale_y_continuous(expand=c(0,0),breaks=c(-89.8,-60,-30,0,30,60,89.8),labels=seq(mylm[1],mylm[2],30))+
    geom_rect(mapping=aes(xmin=xlm[1],xmax=xlm[2],ymin=ylm[1],ymax=ylm[2]),fill=NA,col='black')+
    theme(panel.grid.major=element_blank(),
          panel.background = element_rect(fill = 'gray75'),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          axis.line=element_line(color='black',size=0.0001),
          legend.key.size =  unit(0.1, "in"),
          legend.text=element_text(size=6),
          legend.direction='vertical',
          text = element_text(size=10),
          axis.ticks.x=element_line(size=.1),
          axis.ticks.y=element_line(size=.1),
          panel.border=element_rect(colour="black",fill=NA,size=0.1))+
    xlab('')+
    ylab('')


ggplot()+
  geom_hline(yintercept=seq(ylm[1],ylm[2],1), linetype="solid",lwd=0.0001,color='black')+
  geom_vline(xintercept=seq(xlm[1],xlm[2],1), linetype="solid",lwd=0.0001,color='black')+
  geom_sf(data=wcm,col=NA,fill='black',inherit.aes = FALSE)+
  scale_x_continuous(expand=c(0,0),limits=xlm)+
  scale_y_continuous(expand=c(0,0),limits=ylm)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background = element_rect(fill = "white"),
        ##          panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')


ggplot()+
  geom_sf(data=wcl,col=NA,fill='black',inherit.aes=FALSE)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  scale_x_continuous(expand=c(0,0),breaks=c(-179.8,-90,0,90,179.8),labels=seq(mxlm[1],mxlm[2],90))+
  scale_y_continuous(expand=c(0,0),breaks=c(-89.8,-60,-30,0,30,60,89.8),labels=seq(mylm[1],mylm[2],30))+
  geom_rect(mapping=aes(xmin=xlm[1],xmax=xlm[2],ymin=ylm[1],ymax=ylm[2]),fill=NA,col='red3')+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.border=element_rect(colour="black",fill=NA,size=0.5))+
  xlab('')+
  ylab('')
dev.off()




