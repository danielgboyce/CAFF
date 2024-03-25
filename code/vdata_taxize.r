##ADDS TSN NUMBERS TO SPECIES IN VDATA
library(taxize)
library(dplyr)

######################################################################
##IMPORT VULNERABILITY SCORES
(load('N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/vdata_form.RData'))


vdat.taxon<-unique(subset(vdata,is.na(spname)==FALSE,select=c('spname','comname')))
vdat.taxon<-cbind(vdat.taxon,data.frame(get_tsn(vdat.taxon$spname,accepted=TRUE,ask=TRUE)))
vdat.taxon<-vdat.taxon[,1:3]
names(vdat.taxon)[3]<-'idsp'
vdata<-left_join(vdata,unique(subset(vdat.taxon,select=c('spname','ids'))),by=c('spname'))


vdat.taxon2<-unique(subset(vdata,is.na(acceptedname)==FALSE,select=c('acceptedname','comname')))
vdat.taxon2<-cbind(vdat.taxon2,data.frame(get_tsn(vdat.taxon2$acceptedname,accepted=TRUE,ask=TRUE)))
vdat.taxon2<-vdat.taxon2[,1:3]
names(vdat.taxon2)[3]<-'idsv'
vdata<-left_join(vdata,unique(subset(vdat.taxon2,select=c('acceptedname','idsv'))),by=c('acceptedname'))


vdata<-left_join(vdata,unique(subset(vdat.taxon2,select=c('acceptedname','idsv'))),by=c('acceptedname'))

names(vdata)[77]<-'idsp'

##TAKE WHICHEVER TSN IS PRESENT
vdata$ids<-ifelse(is.na(vdata$idsp)==TRUE,vdata$idsv,vdata$idsp)

vdata<-select(vdata, -idsp, -idsv)


save(vdata,file='N:/data/CC_vulnerability_reg/data/analysis_outputs_local/quarter_deg/figures_data/vdata_form.RData')

