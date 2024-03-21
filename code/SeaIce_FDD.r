library(ggplot2)
library(rnaturalearth)
library(paletteer)
library(scico)
library(pals)
library(mgcv)
library(dplyr)
library(plyr)
library(stringr)
library(raster)
library(sf)
library(drc)
library(aomisc)

datadir<-'N:/data/CAFF/data/FDD_Ice'
figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs'

##IMPORT DATA
##dat<-read.csv(paste(datadir,'/ProcessedCIS_FDD_20230919.csv',sep=''),header=TRUE)
##ERA dataset
dat<-read.csv(paste(datadir,'/ProcessedCIS_FDD_20230926.csv',sep=''),header=TRUE)

names(dat)<-tolower(names(dat))
dat$loc<-gsub(' ','',paste(dat$lon,'_',dat$lat))
names(dat)<-ifelse(names(dat) %in% c('seaiceweekspt1'),'seaiceweeks',names(dat))
dat$prp<-dat$seaiceweeks/max(dat$seaiceweeks,na.rm=TRUE)
dat$lfdd<-log10(dat$fdd)
dat<-subset(dat,is.na(seaiceweeks)==FALSE & is.na(fdd)==FALSE)##BUNCH OF NA FDD AROUND ANTICOSTI, INNER ST LAWRENCE RIVER


##ADD MAX AND VARIANCE OF SEA ICE
f<-function(d){
return(data.frame(maxice=max(d$seaiceweeks),
                  varice=max(d$seaiceweeks)-min(d$seaiceweeks),
                  maxiceprp=max(d$prp),
                  variceprp=max(d$prp)-min(d$prp)))
}
dd<-ddply(dat,.(loc),.fun=f,.progress='text')
dat<-left_join(dat,dd,by=c('loc'))

##EXPLORE DISTRIBUTION OF VARIABLES - SEE IF TRANSFORMATION CAN NORMALIZE
hist((dat$prp))
hist(log10(dat$prp))
hist(sqrt(dat$prp))
hist(dat$fdd)
hist(log10(dat$fdd))
hist(sqrt(dat$fdd))

mod1<-gam(prp~lfdd,data=dat)#68%
mod2<-gam(prp~lfdd,data=subset(dat,region %in% c('Gulf')))#19%
mod3<-gam(prp~lfdd,data=subset(dat,region %in% c('Atlantic')))#15%
mod4<-gam(prp~lfdd,data=subset(dat,region %in% c('Labrador/NL')))#82

mod1<-gam(prp~lfdd + variceprp,data=dat)#70%
mod2<-gam(prp~lfdd + variceprp,data=subset(dat,region %in% c('Gulf')))#20%
mod3<-gam(prp~lfdd + variceprp,data=subset(dat,region %in% c('Atlantic')))#44%
mod4<-gam(prp~lfdd + variceprp,data=subset(dat,region %in% c('Labrador/NL')))#83

##FDD EXPLAINS SEA ICE WELL IN NL/LAB (82%), LESS WELL IN OTHER REGIONS; 
##ADDING IN SEA ICE VARIANCE IMPROVES FIT MARKEDLY IN ATLANTIC (44%) BUT NOT IN GULF
ggplot()+
  geom_point(data=dat,aes(x=lfdd,y=seaiceweeks))+
  facet_wrap(~region,ncol=2)

ggplot()+
  geom_point(data=dat,aes(x=fdd,y=seaiceweeks),alpha=0.01)+
  facet_wrap(~region,ncol=2)

a<-subset(dat,seaiceweeks <=17)
ggplot()+
  geom_point(data=a,aes(x=fdd,y=seaiceweeks),alpha=0.01)+
  facet_wrap(~region,ncol=2)


#############################

##REYNOLDS OISST

#############################

#############################################################
##REINTERPOLATES REYNOLDS OISST DATA TO 1DEGREE FOR LATER PROCESSING
############################################################
##fls<-list.files(paste(datadir,'reynolds_OISST/',sep=''))
latd<-c(38,85)
lond<-c(-41,-90)
ddir<-'N:/data/CC_vulnerability/data/'
fls<-list.files(paste(ddir,'reynolds_OISST/',sep=''))
yrs<-seq(1980,2020,1)
pt<-paste(yrs,collapse = '|')
fls<-str_subset(fls,pattern=pt)
##d<-fls[[1]]
f<-function(d){
  print(d)
  ncfname<-paste(paste(ddir,'reynolds_OISST/',sep=''), d, sep="")
  r<-brick(ncfname, varname='sst')
  r<-dropLayer(r,seq(152,243,1))  
  ##CROP TO AREA OF INTEREST - ADDS BUFFER BECAUSE DATA ARE IN DIFFERENT COORD SYSTEM AND WANT O ENSURE WE GET IT ALL
  return(crop(r,extent((360-abs(lond[2]))-1,(360-abs(lond[1]))+1,latd[1],latd[2])))
  gc()
}
l<-llply(fls,.fun=f,.progress='text')
sstr<-stack(l)

##MEAN OF VALUES IN EACH 1DEGREE SST BIN
system.time(sstr<-calc(sstr, function(x) mean(x,na.rm=TRUE)))##14 min

##CONVERT TO DATA.FRAME
xy<-data.frame(coordinates(sstr))
sstrdf <-  data.frame(lon=xy$x,
                      lat=xy$y,
                      sst=values(sstr))
sstrdf$sst<-ifelse(sstrdf$sst %in% c(Inf, -Inf),NA,sstrdf$sst)
sstrdf$lon<-ifelse(sstrdf$lon > 180, -360 + sstrdf$lon, sstrdf$lon)
sstrdf<-subset(sstrdf,is.na(sst)==FALSE)
sstrdf$id<-seq(1,dim(sstrdf)[1],1)
##save(sstr,file=paste(datadir,'sstr.rda',sep=''))
##save(sstrdf,file=paste(datadir,'sstrdf.rda',sep=''))
(load(paste(datadir,'sstrdf.rda',sep='')))


##COMBINE SST TO DATA FRAME
sstrsf<-st_as_sf(sstrdf,coords=c('lon','lat'),crs=crs(sstr))
datsf<-st_as_sf(dat,coords=c('lon','lat'),crs=crs(sstr))
a<-st_nearest_feature(datsf,sstrsf,pairwise=FALSE)
dat$id<-a
dat<-left_join(dat,sstrdf,by=c('id'))
##save(dat,file=paste(datadir,'FDD_SeaIce_SST.rda',sep=''))
(load(paste(datadir,'FDD_SeaIce_SST.rda',sep='')))
names(dat)<-ifelse(names(dat) %in% c('lon.x'),'lon',names(dat))
names(dat)<-ifelse(names(dat) %in% c('lat.x'),'lat',names(dat))


gl<-subset(dat,region %in% c('Gulf'))
mod2<-gam(prp~lfdd,data=gl)#21%
plot(gl$sst,residuals(mod2))

mod1<-gam(prp~lfdd,data=dat)#68%/71%
mod2<-gam(prp~lfdd,data=subset(dat,region %in% c('Gulf')))#21%
mod3<-gam(prp~lfdd,data=subset(dat,region %in% c('Atlantic')))#18%
mod4<-gam(prp~lfdd,data=subset(dat,region %in% c('Labrador/NL')))#87

mod1<-gam(prp~lfdd + sst,data=dat)#68%/71%
mod2<-gam(prp~lfdd + sst,data=subset(dat,region %in% c('Gulf')))#21%
mod3<-gam(prp~lfdd + sst,data=subset(dat,region %in% c('Atlantic')))#18%
mod4<-gam(prp~lfdd + sst,data=subset(dat,region %in% c('Labrador/NL')))#87

mod1<-gam(prp~sst,data=dat)#68%/58%
mod2<-gam(prp~sst,data=subset(dat,region %in% c('Gulf')))#21%/3%
mod3<-gam(prp~sst,data=subset(dat,region %in% c('Atlantic')))#18%/3%
mod4<-gam(prp~sst,data=subset(dat,region %in% c('Labrador/NL')))#83%

plot(b$sst,b$prp)

ggplot()+
  geom_point(data=b,aes(x=sst,y=seaiceweeks),alpha=0.01)+
  facet_wrap(~region,ncol=2)

##FIT LOGISTIC MODEL TO EACH LOCATION
f<-function(d){
mod<-gam(prp~lfdd,data=d)
s<-summary(mod)
return(data.frame(n=dim(d)[1],
                  maxice=max(d$prp),
                  varice=max(d$prp)-min(d$prp),
                  r2=round(s$r.sq,digits=2),
                  lfdd=round(s$p.table[2,1],digits=3),
                  pv=round(s$p.table[2,4],digits=3)))
}
mdat<-ddply(dat,.(loc,lon,lat),.fun=f,.progress='text')

##PLOT MODEL STATS ON MAP
wcm<-ne_download(scale = 50, type = 'land', category = 'physical',returnclass='sf')
clb<-'gray60'
mxlm<-c(-72,-50)
mylm<-c(42,58)

p1<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=mdat,aes(x=lon,y=lat,color=r2*100),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
##  scale_x_continuous(expand=c(0,0),breaks=c(-179.8,-90,0,90,179.8),labels=seq(mxlm[1],mxlm[2],90))+
##  scale_y_continuous(expand=c(0,0),breaks=c(-89.8,-60,-30,0,30,60,89.8),labels=seq(mylm[1],mylm[2],30))+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
##        legend.key.width =  unit(0.1, "in"),
##        legend.key.height =  unit(0.25, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=tol.rainbow(100),guide='colourbar',name='Variance in seaice \nexplained by FDD \n(logistic model)',na.value=clb,limits=c(0,100))

mdat$pvsig<-ifelse(mdat$pv<=0.05,'Significant','Nonsignificant')
p2<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=mdat,aes(x=lon,y=lat,color=pvsig),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.25,0.7),
        legend.key.size =  unit(0.09, "in"),
##        legend.key.width =  unit(0.1, "in"),
##        legend.key.height =  unit(0.25, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_manual(values=c('green3','magenta4'),name='Significance of relationship \n between sea ice and \n FDD (logistic model)',na.value=clb)


p3<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=mdat,aes(x=lon,y=lat,color=maxice),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.9),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=tol.rainbow(100),guide='colourbar',name='Max Ice',na.value=clb,limits=c(0,1))


p4<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=mdat,aes(x=lon,y=lat,color=varice),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.9),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=tol.rainbow(100),guide='colourbar',name='Max Ice range',na.value=clb,limits=c(0,1))




p5<-ggplot()+
  geom_point(data=mdat,aes(x=varice,y=r2,color=maxice),size=2)+
    scale_color_gradientn(colors=tol.rainbow(100),guide='colourbar',na.value=clb,limits=c(0,1),name='Max weeks of ice cover')+
  xlab('Spread in ice cover [% of maximum possible ice cover]')+
  ylab("Variance explained by model (logistic)")+
  theme_light()+ 
  theme(legend.position=c(0.2,0.9),
                legend.key.size =  unit(0.09, "in"))

  
library(gridExtra)
setwd(figsdir)
pdf('seaice_FDD_logisticmod_maps.pdf',height=10,width=10)
grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)
grid.arrange(p5,ncol=2,nrow=2)
dev.off()

setwd(figsdir)
pdf('seaice_FDD_logisticmod_var.v.r2.pdf',height=10,width=10)
ggplot()+
  geom_point(data=mdat,aes(x=varice,y=r2),pch=16)+
  xlab('Variance in sea ice')+
  ylab('Variance explained  by model')
dev.off()

#############################################
##LOGARITHMIC MODEL
lmod<-lm(prp~lfdd,data=dat)
lmod<-gam(prp~lfdd,data=dat)
lmod2<-gam(prp~lfdd + lat,data=dat)
lmod3<-gam(prp~lfdd + variceprp,data=dat)
lmod4<-gam(prp~lfdd + variceprp + lat,data=dat)
AIC(lmod,lmod2,lmod3, lmod4)
s<-summary(lmod2)

lmod<-gam(prp~lfdd,data=dat)
pdat<-data.frame(lfdd=seq(min(dat$lfdd),max(dat$lfdd),length.out=1000))
p<-predict(lmod,newdata=pdat,se.fit=TRUE,type='response')
pdat$p<-p$fit
pdat$up<-pdat$p+(1.96*p$se.fit)
pdat$dn<-pdat$p-(1.96*p$se.fit)
plot(dat$lfdd,dat$prp)
lines(pdat$lfdd,pdat$p,col='red')

plot(dat$fdd,dat$prp)
lines(10^pdat$lfdd,pdat$p,col='red')


##QUASIBINOMIAL MODEL
mod<-gam(prp~fdd,family=quasibinomial,data=dat)
pdat<-data.frame(fdd=seq(min(dat$fdd),max(dat$fdd),length.out=1000))
p<-predict(mod,newdata=pdat,se.fit=TRUE,type='response')
pdat$p<-p$fit
pdat$up<-pdat$p+(1.96*p$se.fit)
pdat$dn<-pdat$p-(1.96*p$se.fit)
plot(dat$fdd,dat$prp)
lines(pdat$fdd,pdat$p,col='red')

mod<-gam(prp~s(fdd,k=4),family=quasibinomial,data=dat,gamma=1.4)
pdat<-data.frame(fdd=seq(min(dat$fdd),max(dat$fdd),length.out=1000))
p<-predict(mod,newdata=pdat,se.fit=TRUE,type='response')
pdat$p<-p$fit
pdat$up<-pdat$p+(1.96*p$se.fit)
pdat$dn<-pdat$p-(1.96*p$se.fit)
plot(dat$fdd,dat$prp)
lines(pdat$fdd,pdat$p,col='red')

lines(pdat$fdd,pdat$p,col='magenta')


##POISSON MODEL
mod<-gam(seaiceweeks~fdd,family=poisson,data=dat)
pdat<-data.frame(fdd=seq(min(dat$fdd),max(dat$fdd),length.out=1000))
p<-predict(mod,newdata=pdat,se.fit=TRUE,type='response')
pdat$p<-p$fit
pdat$up<-pdat$p+(1.96*p$se.fit)
pdat$dn<-pdat$p-(1.96*p$se.fit)

plot(dat$fdd,dat$seaiceweeks)
lines(pdat$fdd,pdat$p,col='red')

###############################################
##MICHAELIS MENTON MODEL
modm<-drm(dat$prp ~ dat$fdd,fct=MM.2())
TSS <- sum((dat$prp - mean(dat$prp))^2)
##RESIDUAL SUM OF SQUARES - VARIANCE NOT EXPLAINED BY THE MODEL
RSS <- sum(residuals(modm)^2)
##R2: PROPORTION OF TOTAL VARIABLITIY EXPLAINED BY MODEL
r2 <- 1 - (RSS / TSS)
pdatm<-data.frame(fdd=seq(min(dat$fdd),max(dat$fdd),length.out=1000))
p<-predict(modm,newdata=pdatm)
pdatm$p<-p

lines(pdatm$fdd,pdatm$p,col='green')


##ASYMPTOTIC NONLINEAR MODEL 
###########################################################################
##SELF-STARTING ASYMPTOTIC MODEL - RETRIEVE PLAUSIBLE PARAMETERS
##devtools::install_github("onofriAndreaPG/aomisc")
library(drc)
library(aomisc)
mod<-drm(dat$prp ~ dat$fdd,fct=DRC.asymReg())
s<-summary(mod)
plateaustart<-s$coefficients[3,1]
asymptotestart<-s$coefficients[1,1]
ratestart<-s$coefficients[2,1]

nlmod1<-nls(prp ~ asymptote + (plateau - asymptote) * exp(-rate * fdd),data=dat,start=list(asymptote=asymptotestart,plateau=plateaustart,rate=ratestart),algorithm='port',nls.control(maxiter=500))
s<-summary(nlmod1)

##TOTAL VARIABILITY IN RESPONSE
TSS <- sum((dat$prp - mean(dat$prp))^2)
##RESIDUAL SUM OF SQUARES - VARIANCE NOT EXPLAINED BY THE MODEL
RSS <- sum(residuals(nlmod1)^2)
##R2: PROPORTION OF TOTAL VARIABLITIY EXPLAINED BY MODEL
r2 <- 1 - (RSS / TSS)

pdat<-data.frame(fdd=seq(min(dat$fdd),max(dat$fdd),length.out=1000))
p<-predict(nlmod1,newdata=pdat,se.fit=TRUE,interval='confidence',level=0.95)
pdat$p<-p
plot(dat$fdd,dat$prp)
lines(pdat$fdd,pdat$p,col='green',lwd=2)

nldat1<-dat
nldat1$resid<-residuals(nlmod1)


##RESIDUALS ARE THE SAME AS DIFFERECE BETWEEN OBSERVED AND PREDICTED - NEGATIVE VALUES ARE WHERE MODEL IS OVERPREDICTING SEA ICE
f<-function(d){
  return(data.frame(delt=mean(d$delt),
                    resid=mean(d$resid)))
}
dt1<-ddply(nldat1,.(lon,lat),.fun=f,.progress='text')

p1<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=dt1,aes(x=lon,y=lat,color=resid),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Residuals',na.value=clb,limits=c(-0.5,0.5))



############################################
##MODEL WITH SEAICE VARIANCE ADDED
mod<-drm(dat$prp ~ dat$fdd,fct=DRC.asymReg())
s<-summary(mod)
plateaustart<-s$coefficients[3,1]
asymptotestart<-s$coefficients[1,1]
ratestart<-s$coefficients[2,1]

nlmod2<-nls(prp ~ asymptote + (plateau - asymptote) * exp(-rate * fdd) + beta*variceprp,data=dat,start=list(asymptote=asymptotestart,plateau=plateaustart,rate=ratestart,beta=0.1),algorithm='port',nls.control(maxiter=500))
s<-summary(nlmod2)

##TOTAL VARIABILITY IN RESPONSE
TSS <- sum((dat$prp - mean(dat$prp))^2)
##RESIDUAL SUM OF SQUARES - VARIANCE NOT EXPLAINED BY THE MODEL
RSS <- sum(residuals(nlmod2)^2)
##R2: PROPORTION OF TOTAL VARIABLITIY EXPLAINED BY MODEL
r2 <- 1 - (RSS / TSS)


pdat<-data.frame(fdd=seq(min(dat$fdd),max(dat$fdd),length.out=1000),
                 variceprp=mean(dat$variceprp))
p<-predict(nlmod2,newdata=pdat,se.fit=TRUE,interval='confidence',level=0.95)
pdat$p<-p
plot(dat$fdd,dat$prp)
lines(pdat$fdd,pdat$p,col='green',lwd=2)

nldat2<-dat
nldat2$resid<-residuals(nlmod2)

##RESIDUALS ARE THE SAME AS DIFFERECE BETWEEN OBSERVED AND PREDICTED - NEGATIVE VALUES ARE WHERE MODEL IS OVERPREDICTING SEA ICE
f<-function(d){
  return(data.frame(delt=mean(d$delt),
                    resid=mean(d$resid)))
}
dt2<-ddply(nldat2,.(lon,lat),.fun=f,.progress='text')

p2<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=dt2,aes(x=lon,y=lat,color=resid),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Residuals',na.value=clb,limits=c(-0.5,0.5))


####################################################################
############################################
##LOGARITHMIC MODEL WITH SPACE VARIANCE ADDED
#############################################
##LOGARITHMIC MODEL
lmod<-gam(prp~lfdd,data=dat)
lmod2<-gam(prp~lfdd + variceprp,data=dat)
lmod3<-gam(prp~lfdd + variceprp + s(lon,lat),data=dat)
AIC(lmod,lmod2,lmod3)
s<-summary(lmod2)

lmod<-gam(prp~lfdd + variceprp + s(lon,lat),data=dat)
pdat<-data.frame(lfdd=seq(min(dat$lfdd),max(dat$lfdd),length.out=1000),
                 variceprp=mean(dat$variceprp),
                 lon=mean(dat$lon),
                 lat=mean(dat$lat))
p<-predict(lmod,newdata=pdat,se.fit=TRUE,type='response')
pdat$p<-p$fit
pdat$up<-pdat$p+(1.96*p$se.fit)
pdat$dn<-pdat$p-(1.96*p$se.fit)
plot(dat$lfdd,dat$prp)
lines(pdat$lfdd,pdat$p,col='green')

plot(dat$fdd,dat$prp)
lines(10^pdat$lfdd,pdat$p,col='red')

ldat<-dat
ldat$resid<-residuals(lmod)

##RESIDUALS ARE THE SAME AS DIFFERECE BETWEEN OBSERVED AND PREDICTED - NEGATIVE VALUES ARE WHERE MODEL IS OVERPREDICTING SEA ICE
f<-function(d){
  return(data.frame(delt=mean(d$delt),
                    resid=mean(d$resid)))
}
ldt<-ddply(ldat,.(lon,lat),.fun=f,.progress='text')

p3<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=ldt,aes(x=lon,y=lat,color=resid),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Residuals',na.value=clb,limits=c(-0.5,0.5))


setwd(figsdir)
pdf('seaice_FDD_asymptotic_resids_maps.pdf',height=10,width=10)
grid.arrange(p1,p2,p3,ncol=2,nrow=2)
dev.off()



###
###
###
###
############################################
##LOGARITHMIC MODEL WITH SPACE VARIANCE ADDED
#############################################
##LOGARITHMIC MODEL
lmod<-gam(prp~lfdd,data=dat)
lmod2<-gam(prp~lfdd + variceprp,data=dat)
lmod3<-gam(prp~lfdd + variceprp + s(lon,lat),data=dat)
lmod4<-gam(prp~ variceprp + s(lon,lat),data=dat)
AIC(lmod,lmod2,lmod3)
s<-summary(lmod2)

ldat<-dat
ldat$resid<-residuals(lmod)
##RESIDUALS ARE THE SAME AS DIFFERECE BETWEEN OBSERVED AND PREDICTED - NEGATIVE VALUES ARE WHERE MODEL IS OVERPREDICTING SEA ICE
f<-function(d){
  return(data.frame(delt=mean(d$delt),
                    resid=mean(d$resid)))
}
ldt<-ddply(ldat,.(lon,lat),.fun=f,.progress='text')

p1<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=ldt,aes(x=lon,y=lat,color=resid),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Residuals',na.value=clb,limits=c(-0.5,0.5))




ldat2<-dat
ldat2$resid<-residuals(lmod2)
##RESIDUALS ARE THE SAME AS DIFFERECE BETWEEN OBSERVED AND PREDICTED - NEGATIVE VALUES ARE WHERE MODEL IS OVERPREDICTING SEA ICE
f<-function(d){
  return(data.frame(delt=mean(d$delt),
                    resid=mean(d$resid)))
}
ldt2<-ddply(ldat2,.(lon,lat),.fun=f,.progress='text')

p2<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=ldt2,aes(x=lon,y=lat,color=resid),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Residuals',na.value=clb,limits=c(-0.5,0.5))



ldat3<-dat
ldat3$resid<-residuals(lmod3)
##RESIDUALS ARE THE SAME AS DIFFERECE BETWEEN OBSERVED AND PREDICTED - NEGATIVE VALUES ARE WHERE MODEL IS OVERPREDICTING SEA ICE
f<-function(d){
  return(data.frame(delt=mean(d$delt),
                    resid=mean(d$resid)))
}
ldt3<-ddply(ldat3,.(lon,lat),.fun=f,.progress='text')

p3<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=ldt3,aes(x=lon,y=lat,color=resid),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Residuals',na.value=clb,limits=c(-0.5,0.5))



ldat4<-dat
ldat4$resid<-residuals(lmod4)
##RESIDUALS ARE THE SAME AS DIFFERECE BETWEEN OBSERVED AND PREDICTED - NEGATIVE VALUES ARE WHERE MODEL IS OVERPREDICTING SEA ICE
f<-function(d){
  return(data.frame(delt=mean(d$delt),
                    resid=mean(d$resid)))
}
ldt4<-ddply(ldat4,.(lon,lat),.fun=f,.progress='text')

p4<-ggplot()+
  geom_sf(data=wcm,fill='gray85',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=ldt3,aes(x=lon,y=lat,color=resid),size=1)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'gray90'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Residuals',na.value=clb,limits=c(-0.5,0.5))

setwd(figsdir)
pdf('seaice_FDD_asymptotic_resids_maps_02202024.pdf',height=10,width=10)
grid.arrange(p1,p2,p3,p4,ncol=2,nrow=2)
dev.off()






##########################################
##LOOK AT CORRELATION IN SEA ICE BETWEEN TWO TIME ERAS TO SEE IF THERE'S A LARGE DIFFERENCE
f<-function(d){
  return(data.frame(seaiceweeks=mean(d$seaiceweeks,na.rm=TRUE)))
}
a<-ddply(subset(dat,year<1990),.(lon,lat),.fun=f)
b<-ddply(subset(dat,year>=1990),.(lon,lat),.fun=f)
z<-left_join(a,b,by=c('lon','lat'))
names(z)<-c('lon','lat','x70','x20')
plot(z$x70,z$x20)
cor(z$x70,z$x20)






#####
#####
##ASYMPTOTIC NONLINEAR MODEL 
###########################################################################
##SELF-STARTING ASYMPTOTIC MODEL - RETRIEVE PLAUSIBLE PARAMETERS
##devtools::install_github("onofriAndreaPG/aomisc")
mod<-drm(dat$prp ~ dat$fdd,fct=DRC.asymReg())
s<-summary(mod)
plateaustart<-s$coefficients[3,1]
asymptotestart<-s$coefficients[1,1]
ratestart<-s$coefficients[2,1]

nlmod1<-nls(prp ~ asymptote + (plateau - asymptote) * exp(-rate * fdd),data=dat,start=list(asymptote=asymptotestart,plateau=plateaustart,rate=ratestart),algorithm='port',nls.control(maxiter=500))
s<-summary(nlmod1)

##TOTAL VARIABILITY IN RESPONSE
TSS <- sum((dat$prp - mean(dat$prp))^2)
##RESIDUAL SUM OF SQUARES - VARIANCE NOT EXPLAINED BY THE MODEL
RSS <- sum(residuals(nlmod1)^2)
##R2: PROPORTION OF TOTAL VARIABLITIY EXPLAINED BY MODEL
r2 <- 1 - (RSS / TSS)



#################################

##GETS SAMPLE OF DATA, FITS MODEL, USES MODEL TO PREDICT SEA ICE OVER INDEPENDENT DATA ERA, COMPARES TO OBSERVED
##ASYMPTOTIC NONLINEAR MODEL

#################################
###MOST RECENT DATA ONLY
rd<-subset(dat,year>=2000)
modr<-drm(rd$prp ~ rd$fdd,fct=DRC.asymReg())
s<-summary(modr)
plateaustart<-s$coefficients[3,1]
asymptotestart<-s$coefficients[1,1]
ratestart<-s$coefficients[2,1]

nlmodrd<-nls(prp ~ asymptote + (plateau - asymptote) * exp(-rate * fdd),data=rd,start=list(asymptote=asymptotestart,plateau=plateaustart,rate=ratestart),algorithm='port',nls.control(maxiter=500))
s<-summary(nlmodrd)

##TOTAL VARIABILITY IN RESPONSE
TSS <- sum((rd$prp - mean(rd$prp))^2)
##RESIDUAL SUM OF SQUARES - VARIANCE NOT EXPLAINED BY THE MODEL
RSS <- sum(residuals(nlmodrd)^2)
##R2: PROPORTION OF TOTAL VARIABLITIY EXPLAINED BY MODEL
r2 <- 1 - (RSS / TSS)

##PREDICT FROM RECENT DATA
hd<-subset(dat,year<2000)
##pdat<-data.frame(fdd=seq(min(hd$fdd),max(hd$fdd),length.out=1000))
hd$prd<-predict(nlmodrd,newdata=hd,se.fit=FALSE)
plot(hd$prp,hd$prd)
lines(pdat$fdd,pdat$p,col='green',lwd=2)


rds<-data.frame(year=sort(unique(dat$year)),
                prp=tapply(dat$prp,dat$year,mean))
hds<-data.frame(year=sort(unique(hd$year)),
                prp=tapply(hd$prd,hd$year,mean))

p1<-ggplot()+
  geom_point(data=rds,aes(x=year,y=prp),col='black')+
  geom_line(data=rds,aes(x=year,y=prp),col='black')+
  geom_point(data=hds,aes(x=year,y=prp),col='firebrick3')+
  geom_line(data=hds,aes(x=year,y=prp),col='firebrick3')+
  ggtitle('Hindcast')



##############################################
##############################################
###SAMPLE OF DATA
###(load(paste(datadir,'FDD_SeaIce_SST.rda',sep='')))
dat$id<-seq(1,dim(dat)[1],1)
n<-ceiling(dim(dat)[1]*0.25)
rd<-subset(dat,id %in% sample(dat$id,size=n))
modr<-drm(rd$prp ~ rd$fdd,fct=DRC.asymReg())
s<-summary(modr)
plateaustart<-s$coefficients[3,1]
asymptotestart<-s$coefficients[1,1]
ratestart<-s$coefficients[2,1]

nlmodrd<-nls(prp ~ asymptote + (plateau - asymptote) * exp(-rate * fdd),data=rd,start=list(asymptote=asymptotestart,plateau=plateaustart,rate=ratestart),algorithm='port',nls.control(maxiter=500))
s<-summary(nlmodrd)

##TOTAL VARIABILITY IN RESPONSE
TSS <- sum((rd$prp - mean(rd$prp))^2)
##RESIDUAL SUM OF SQUARES - VARIANCE NOT EXPLAINED BY THE MODEL
RSS <- sum(residuals(nlmodrd)^2)
##R2: PROPORTION OF TOTAL VARIABLITIY EXPLAINED BY MODEL
r2 <- 1 - (RSS / TSS)

##PREDICT FROM RECENT DATA
hd<-subset(dat,!(id %in% rd$id))
##pdat<-data.frame(fdd=seq(min(hd$fdd),max(hd$fdd),length.out=1000))
hd$prd<-predict(nlmodrd,newdata=hd,se.fit=FALSE)

rds<-data.frame(year=sort(unique(rd$year)),
                prp=tapply(rd$prp,rd$year,mean))

hds<-data.frame(year=sort(unique(hd$year)),
                prp=tapply(hd$prd,hd$year,mean))

p2<-ggplot()+
  geom_point(data=rds,aes(x=year,y=prp),col='black')+
  geom_line(data=rds,aes(x=year,y=prp),col='black')+
  geom_point(data=hds,aes(x=year,y=prp),col='firebrick3')+
  geom_line(data=hds,aes(x=year,y=prp),col='firebrick3')+
  ggtitle('Random Sample [25%]')


setwd(figsdir)
pdf('seaice_prediction_TimeTrend_Reconstruct.pdf',height=10,width=10)
grid.arrange(p1,p2,ncol=1)
dev.off()





#################################
#################################
#################################

##GETS SAMPLE OF DATA, FITS MODEL, USES MODEL TO PREDICT SEA ICE OVER INDEPENDENT DATA ERA, COMPARES TO OBSERVED
##LOGISTIC MODEL
#################################
###MOST RECENT DATA ONLY
rd<-subset(dat,year>=2000)
modr<-gam(prp~lfdd,data=rd)
s<-summary(modr)

##PREDICT FROM RECENT DATA
hd<-subset(dat,year<2000)
##pdat<-data.frame(fdd=seq(min(hd$fdd),max(hd$fdd),length.out=1000))
hd$prd<-predict(modr,newdata=hd,se.fit=FALSE)
plot(hd$prp,hd$prd)
lines(pdat$fdd,pdat$p,col='green',lwd=2)


rds<-data.frame(year=sort(unique(dat$year)),
                prp=tapply(dat$prp,dat$year,mean))
hds<-data.frame(year=sort(unique(hd$year)),
                prp=tapply(hd$prd,hd$year,mean))

p1<-ggplot()+
  geom_point(data=rds,aes(x=year,y=prp),col='black')+
  geom_line(data=rds,aes(x=year,y=prp),col='black')+
  geom_point(data=hds,aes(x=year,y=prp),col='firebrick3')+
  geom_line(data=hds,aes(x=year,y=prp),col='firebrick3')+
  ggtitle('Hindcast')


#################################
#################################
###SAMPLE OF DATA
###(load(paste(datadir,'FDD_SeaIce_SST.rda',sep='')))
dat$id<-seq(1,dim(dat)[1],1)
n<-ceiling(dim(dat)[1]*0.25)
rd<-subset(dat,id %in% sample(dat$id,size=n))
modr<-gam(prp~lfdd,data=rd)
s<-summary(modr)

##PREDICT FROM RECENT DATA
hd<-subset(dat,!(id %in% rd$id))
##pdat<-data.frame(fdd=seq(min(hd$fdd),max(hd$fdd),length.out=1000))
hd$prd<-predict(modr,newdata=hd,se.fit=FALSE)

rds<-data.frame(year=sort(unique(rd$year)),
                prp=tapply(rd$prp,rd$year,mean))

hds<-data.frame(year=sort(unique(hd$year)),
                prp=tapply(hd$prd,hd$year,mean))

p2<-ggplot()+
  geom_point(data=rds,aes(x=year,y=prp),col='black')+
  geom_line(data=rds,aes(x=year,y=prp),col='black')+
  geom_point(data=hds,aes(x=year,y=prp),col='firebrick3')+
  geom_line(data=hds,aes(x=year,y=prp),col='firebrick3')+
  ggtitle('Random Sample [25%]')


setwd(figsdir)
pdf('seaice_prediction_TimeTrend_Reconstruct_02202024.pdf',height=10,width=10)
grid.arrange(p1,p2,ncol=1)
dev.off()







#################################################

##TREND AT EACH LOCATION
names(dat)<-ifelse(names(dat) %in% c('lon.x'),'lon',names(dat))
names(dat)<-ifelse(names(dat) %in% c('lat.x'),'lat',names(dat))

d<-subset(dat,ptid==1)
f<-function(d){
  mod<-lm(seaiceweeks~year,data=d)
  mods<-gam(prp~s(year,k=4),data=d,gamma=1.4)
  s<-summary(mod)
  ss<-summary(mods)

return(data.frame(b=s$coefficients[2,1],
                  pv=s$coefficients[2,4],
                  bs=predict(mods,newdata=data.frame(year=max(d$year)))-predict(mods,newdata=data.frame(year=min(d$year))),                    
                  pvs=ss$s.table[1,4]))
}
mdat<-ddply(dat,.(ptid,lon,lat),.fun=f,.progress='text')


##PLOT MODEL STATS ON MAP
wcm<-ne_download(scale = 50, type = 'land', category = 'physical',returnclass='sf')
clb<-'gray60'
mxlm<-c(-72,-50)
mylm<-c(42,58)

sig<-subset(mdat,pv<=0.05)
ns<-subset(mdat,pv>0.05)
dim(sig)[1]/dim(mdat)[1]

p1<-ggplot()+
  geom_sf(data=wcm,fill='gray10',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=sig,aes(x=lon,y=lat,color=b),shape=16,size=2)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Change in seaice',na.value=clb,limits=c(-0.33,0.33))+
  ggtitle('Monotonic trend')


sig<-subset(mdat,pvs<=0.05)
ns<-subset(mdat,pvs>0.05)
dim(sig)[1]/dim(mdat)[1]

p2<-ggplot()+
  geom_sf(data=wcm,fill='gray10',inherit.aes=FALSE,color='gray80',lwd=0.000001,stroke=0.000001)+
  geom_point(data=sig,aes(x=lon,y=lat,color=bs),shape=16,size=2)+
  coord_sf(xlim=mxlm,ylim=mylm, expand=FALSE,clip='on')+
  theme(panel.grid.major=element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        axis.line=element_line(color='black',size=0.0001),
        legend.position=c(0.2,0.7),
        legend.key.size =  unit(0.09, "in"),
        legend.text=element_text(size=6),
        legend.direction='vertical',
        text = element_text(size=10),
        axis.ticks.x=element_line(size=.1),
        axis.ticks.y=element_line(size=.1),
        panel.border=element_rect(colour="black",fill=NA,size=0.1))+
  xlab('')+
  ylab('')+
  scale_color_gradientn(colors=coolwarm(100),guide='colourbar',name='Change in seaice',na.value=clb,limits=c(-0.33,0.33))+
    ggtitle('Nonmonotonic trend')


library(gridExtra)
setwd(figsdir)
pdf('seaice_TimeTrend_maps.pdf',height=10,width=10)
grid.arrange(p1,p2,ncol=1)
dev.off()


mods<-gam(seaiceweeks~s(year,k=4),data=dat,gamma=1.5)
pdats<-data.frame(year=seq(min(dat$year),max(dat$year),length.out=1000))
p<-predict(mods,newdata=pdats,se.fit=TRUE)
pdats$p<-p$fit
pdats$upr<-pdats$p+(1.96*p$se.fit)
pdats$lwr<-pdats$p-(1.96*p$se.fit)

modf<-gam(seaiceweeks~as.factor(year),data=dat)
pdatf<-data.frame(year=sort(unique(dat$year)))
p<-predict(modf,newdata=pdatf,se.fit=TRUE)
pdatf$p<-p$fit
pdatf$upr<-pdatf$p+(1.96*p$se.fit)
pdatf$lwr<-pdatf$p-(1.96*p$se.fit)

plot(pdatf$year,pdatf$p,pch=15)
lines(pdats$year,pdats$p)
lines(pdats$year,pdats$upr,lty=2)
lines(pdats$year,pdats$lwr,lty=2)

plot(dat$year,dat)

######