##ISSUES: 'MSE' IS DUPLICATED IN COLUMN 'VAR' - USED AS NUMERIC AND TEXXT RESPONSE
##GENERAL APPROACH: TRY TO FORMAT SURVEY RESPONSES IN 'LONG FORMAT' SO THAT EACH ROW CORRESPONDS TO ONE QUESTION-ANSWER FOR A PARTICULAR STOCK; ONE STOCK HAS MANY ROWS
##SURVEY RESPONSES CAN BE ANALYZED ON A QUESTION BY QUESTION OR STOCK BY STOCK BASIS
##USES DATA THAT HAS BEEN 'CLEANED' BY JENN
##NOTES: THIS APPROACH TO HARVESTING/IMPORTING SURVEY DATA SUCKS. NEED TO THINK ABOUT STREAMLINING AND STANDARDIZING TO MAKE THIS CODE REPRODUCIBLE - SHOULD NOT NEED TO 'CLEAN' THE DATA PRIOR TO MACHINE READING. 

##LOCATION OF DATA
datadir<-'N:/data/CAFF/data/Surveys/'
figsdir<-'C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/figs'

##PACKAGES
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(hrbrthemes)
library(pals)

#####################################################################
##IMPORT 'METADATA' FILE THAT CONTAINS LIST OF QUESTIONS AND VARIABLES
#####################################################################
##ANCILIARY SPREADSHEET OF DATA USED TO MAKE SENSE OF CODED RESPONSES
meta<-read.csv(paste(datadir,'FiMACC_Survey_Outline_DGB.csv',sep=''))
meta<-meta[,1:9]
names(meta)<-tolower(names(meta))

##FILLS IN SOME MISSING ROWS
f<-function(d){
print(unique(d$qid))
  d$page<-rep(sort(unique(d$page),decreasing=TRUE)[1],dim(d)[1])
  d$question<-rep(sort(unique(d$question),decreasing=TRUE)[1],dim(d)[1])
  return(d)
}
meta<-ddply(meta,.(qid),.fun=f,.progress='text')
meta$value<-tolower(meta$value)

##STANDARDIZE TEXT INDENTIFIERS
meta$value<-ifelse(meta$value %in% c('txt','text','Text'),'txt',meta$value)

##CONVERT COLUMNS TO ALL CHARACTER
meta<-as.data.frame(lapply(meta, as.character))

##View(meta)


#########################################################################
##IMPORT 'DATA': ACTUAL SURVEY RESPONSES
#########################################################################
##IMPORT SHORT FORM DATA
datj<-read.csv(paste(datadir,'Full FiMACC Data - JB_Cleaned.csv',sep=''))
names(datj)<-tolower(names(datj))
##OMIT COLUMNS
datj<-subset(datj,select=-c(user_browser,time_in,time_out,user_last_section,subsid,name))
##CREATE ALL AS CHARACTER
datj2<-as.data.frame(lapply(datj, as.character))

##STOCKS DUPLICATED OVER ROWS BY SCIENCE/MAGAGEMENT - COMBINE SO SINGLE STOCK PER ROW;
##NOTE: 30 MANAGEMENTN STOCKS BUT ONLY 25 SCIENCE
##MANAGEMENT QUESTIONS
mg<-subset(datj2,branch_name %in% c('mgmt'),select=c('stock_name',names(datj)[397:length(names(datj))]))
##SCIENCE QUESTIONS
sci<-subset(datj2,branch_name %in% c('science'),select=c(names(datj)[1:396]))
datj2<-full_join(sci,mg,by=c('stock_name'))

##PIVOT TO LONG FORMAT
dat<-pivot_longer(datj2,cols=-c('stock_name','branch_name','id','completed','date','title'),names_to = c("var"), values_to=c('value'),names_repair='unique')

##REMOVE LEADING X'S ON SOME VARIABLES
dat$var<-str_remove(dat$var, "^x")

##REMOVE STOCK DROPDOWNS THAT HAVE 0 ENTRIES
dat<-subset(dat,!(str_detect(dat$var,'stock_dropdown')==TRUE & value==0))

##DENOTE TEXT V NUMERIC RESPONSES; TXT=TEXT RESPONSE (WRITTEN); CODE=CODED RESPONSE (NEED TO USE LOOKUP TABLE); NUM=NUMERIC (AS IS)
dat$rsptype<-ifelse(grepl("[A-Za-z]",dat$value)==TRUE,'txt','code')
dat$rsptype<-ifelse(grepl("_yrs",dat$var)==TRUE,'num',dat$rsptype)
dat$rsptype<-ifelse(dat$var %in% c('stockcomplet_indivariab','stockupdate_indivariab','stockcomplet_divariab','stockupdated_divariab'),'num',dat$rsptype)

##a<-subset(datf,var %in% c('access_supportscie'))
##a<-subset(dat2,var %in% c('access_supportscie'))
##View(a)

##JOIN TO ADD IN CODED RESPONSES
dat2<-left_join(dat,subset(meta,select=c('var','value','options')),by=c('var','value'))

##ADD QUESTIONS/SUBQUESTIONS TO THE RESPONSES
mt<-unique(subset(meta,select=c('qid','question','varlong','var')))
f<-function(d){
  d$varlong<-sort(unique(d$varlong),decreasing=TRUE)[1]
  return(d)
}
mt<-ddply(mt,.(var),.fun=f)
mt<-mt[order(mt$qid),]
mt<-unique(mt)
dat2<-left_join(dat2,mt,by=c('var'))

##CREATE ANSWER COLUMN DEPENDING ON IF RESPONSES ARE NUMERIC, TEXT, DROPDOWN, MULTIPLE CHOICE OR CODED
dat2$ans<-ifelse(dat2$rsptype %in% c('txt','num'),dat2$value,dat2$options)

##SUBSET COLUMNS AND RENAME
datf<-subset(dat2,select=c('id','stock_name','branch_name','title','qid','question','varlong','var','rsptype','ans'))
names(datf)<-c('id','stock','branch','title','qid','question1','question2','var','rsptype','ans')

##a<-subset(datf,var %in% c('stockcomplet_divariab'))
##a<-subset(datf,var %in% c('access_supportscie'))
##View(a)


##TRIED TO FIX ENCODING BUT SOME ANSWERS CONVERTED TO NAS; NOT SURE HOW TO STANDARDIZE ENCODINGS
##datf$ans<-iconv(datf$ans, to='UTF-8')
##datf$ans<-stri_enc_tonative(datf$ans)
##datf$ans<-trimws(datf$ans,which='both')

##CATEGORIZE RESPONSES
datf$qid<-as.numeric(datf$qid)
datf$qtype<-ifelse(datf$qid >=6 & datf$qid<=22,'data',NA)
datf$qtype<-ifelse(datf$qid >=23 & datf$qid<=57,'science',datf$qtype)
datf$qtype<-ifelse(datf$qid >=58,'management',datf$qtype)

##USELESS
datf<-subset(datf,!(question1 %in% c('Region','Species group')))

##FOR SOME REASON, THESE QUESTIONS SPECIFY 'ENVIRONMENTAL' RATHER THAN 'ECOLOGICAL'
datf$question1<-ifelse(datf$qid %in% c(37),'Which ecological variables are indirectly included in teh formal stock assessment?',datf$question1)
datf$question1<-ifelse(datf$qid %in% c(27),'Which ecological variables are included in the formal stock assessment?',datf$question1)

##VAR='DEDATA' SEEMS TO BE EMPTY, SO OMITTING
datf<-datf[!grepl("dedata", datf$var),]




source('C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/code/SSF_import_ExpandedInfo.r')

datf<-as.data.frame(datf)
fs<-as.data.frame(fs)
datf<-left_join(datf,fs,by=c('stock'))

save(datf,file='N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2024_Formatted.RData')


##SAVE
save(datf,file='N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted.RData')
write.csv(datf,'N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted.csv',row.names=FALSE)
(load('N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted.RData'))
datf<-read.csv('N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted.csv',header=TRUE)

df<-subset(datf,id==4755)
write.csv(df,'N:/data/CAFF/data/Surveys/formatted/prac.csv',row.names=FALSE)

df<-unique(subset(datf,select=c('qid','question1','question2')))
write.csv('C:/Users/sailfish/Documents/aalldocuments/literature/research/active/CAFF/tables/survey_main_qs.csv',row.names=FALSE)





#############################################################################

############################################
##LOAD DATA
############################################
(load('N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted.RData'))

#a<-unique(subset(datf,select=c('stock')))
##write.csv(a,file='N:/data/CAFF/data/Surveys/PostedFisheryDecisions/PostedFisheryDecisions_2022.csv',row.names=FALSE)

##########################################################
##DATA TRANSPARENCY
##########################################################
##DATA AVAILABILITY - STOCK
##d<-subset(bb,qid==8 & stock %in% unique(datf$stock)[1])
f<-function(d){
d$stock.avail<-ifelse(d$ans %in% c('Data exists internally within DFO but is not shared across divisions / departments'),0,0.5)
d$stock.avail<-ifelse(d$ans %in% c('Data is publicly accessible'),1,d$stock.avail)
d<-subset(d,is.na(ans)==FALSE)  
return(data.frame(stock.data.avail=round(mean(d$stock.avail,na.rm=TRUE),digits=2)))
}
bb1<-ddply(subset(datf,qid %in% c(8) & question2 %in% c("Total catch of target species","Age of catch", "Weight of catch","Size (ie. length) of catch", "Discards, target and non-target species", "Fishing location (GPS) ", "Fishing date and times")),.(stock),.fun=f)
##d<-subset(datf,qid==8 & stock %in% unique(datf$stock[1]))


##DATA AVAILABILITY - CLIMATE
f<-function(d){
d$stock.avail<-ifelse(d$ans %in% c('Data exists internally within DFO but is not shared across divisions / departments'),0,0.5)
d$stock.avail<-ifelse(d$ans %in% c('Data is publicly accessible'),1,d$stock.avail)
d<-subset(d,is.na(ans)==FALSE)  
return(data.frame(clim.data.avail=round(mean(d$stock.avail,na.rm=TRUE),digits=2)))
}
bb2<-ddply(subset(datf,qid %in% c(12) & question2 %in% c("Temperature", "Oxygen", "Carbonate chemistry (pH)", "Salinity","Primary productivity", "Nutrients zooplankton/Secondary productivity","Upwelling", "Ocean currents", "Sea ice")),.(stock),.fun=f)
##d<-subset(datf,qid==12 & stock %in% unique(datf$stock[1]))


##DATA AVAILABILITY - ECO
f<-function(d){
d$stock.avail<-ifelse(d$ans %in% c('Data exists internally within DFO but is not shared across divisions / departments'),0,0.5)
d$stock.avail<-ifelse(d$ans %in% c('Data is publicly accessible'),1,d$stock.avail)
d<-subset(d,is.na(ans)==FALSE)  
return(data.frame(eco.data.avail=round(mean(d$stock.avail,na.rm=TRUE),digits=2)))
}
bb3<-ddply(subset(datf,qid %in% c(17) & question2 %in% c("Spatial structure of the stock", "Physical habitat of the stock", "Abundance of predators","Abundance of prey", "Abundance of competitors", "Composition of prey")),.(stock),.fun=f)
##d<-subset(datf,qid==17 & stock %in% unique(datf$stock[1]))

##AVAILABILITY OF FISHERIES DECISIONS
bb4<-read.csv('N:/data/CAFF/data/Surveys/PostedFisheryDecisions/PostedFisheryDecisions_2022.csv',header=TRUE)

##PRESENCE AND AVAILABILITY OF IFMP
bb<-unique(subset(datf,select=c('stock','ifmp')))
bb$ifmp<-ifelse(bb$ifmp %in% c('Yes, Full IFMP'),1,bb$ifmp)
bb$ifmp<-ifelse(bb$ifmp %in% c('Yes, Summary of IFMP'),0.5,bb$ifmp)
bb$ifmp<-ifelse(bb$ifmp %in% c(''),0,bb$ifmp)
bb$ifmp<-as.numeric(bb$ifmp)

##INTEGRATE
blist<-list(bb,bb1,bb2,bb3,bb4)
btransp<-reduce(blist,join,by='stock')
 
#RELIABILITY BASES ON NUMBER OF INDICES AVAILABLE
btransp$transpreliab<-round((5-rowSums(is.na(btransp[,2:6])))/5,digits=2)
btransp$transp<-rowMeans(subset(btransp,select=c('ifmp','stock.data.avail','decisionposted')),na.rm=TRUE)



##########################################################
##STOCK RIGOUR
##########################################################

###############
##'DATA AVAILABILITY': OVERALL AVAILABILITY OF STOCK DATA
bb<-subset(datf,qid==6,select=c('stock','ans'))
names(bb)[2]<-'data.avail'
bb$data.avail<-ifelse(bb$data.avail %in% c('Data poor'),0,bb$data.avail)
bb$data.avail<-ifelse(bb$data.avail %in% c('Data moderate'),1,bb$data.avail)
bb$data.avail<-ifelse(bb$data.avail %in% c('Data rich'),2,bb$data.avail)
bb$data.avail<-as.numeric(bb$data.avail)
bb$data.avail<-round(bb$data.avail/2,digits=2)
bb1<-bb

###############
##'DATA RICHNESS': NUMBER OF 5 'CORE' STOCK VARIABLES NEEDED FOR GOOD STOCK MANAGEMENT: BIOMASS, AGE, WEIGHT, LENGTH, LOCATION (WOULD LIKE RECRUITMENT TOO BUT NO DATA ON) AND LENGT OVER WHICH THEY ARE AVAILABLE
#d<-subset(datf,qid %in% c(7) & rsptype==c('code') & stock %in% datf$stock[1])
#d<-subset(datf,qid %in% c(7) & rsptype==c('code') & stock %in% c('Haddock - 5Zjm'))
datf$question2<-trimws(datf$question2,which='both')
f<-function(d){
d$var<-gsub('_dedata','',d$var)
d$var<-gsub('_indata','',d$var)

##x<-subset(d,var %in% unique(d$var)[2])  
f2<-function(x){
x$question2<-sort(unique(x$question2),decreasing=TRUE)[1]
##IF ONE VAR COLLECTED, BOTH COLLECTED
if('Collected' %in% x$ans){ x$ans<-'Collected'
} else NULL
return(unique(subset(x,select=c('stock','question2','ans'))))
}
dd<-ddply(d,.(var),.fun=f2)

dd$ans<-ifelse(dd$ans %in% c('Collected'),1,0)  
return(data.frame(nstock.data=sum(dd$ans)))
}
bb<-ddply(subset(datf,qid %in% c(7) & rsptype==c('code') & question2 %in% c('Age of catch','Size (ie. length) of catch','Fishing location (GPS)')),.(stock),.fun=f)

##ADD AVAILABILITY OF BIOMASS AND MORTALITY FROM FSS
bb2<-unique(subset(datf,select=c('stock','biomass','mortality')))
bb2$mortality<-ifelse(bb2$mortality %in% c('Yes'),1,0)
bb2$biomass<-ifelse(bb2$biomass %in% c('Yes'),1,0)
bb2$ans2<-rowSums(bb2[,2:3],na.rm=TRUE)

bb<-left_join(bb,subset(bb2,select=c('stock','ans2')),by=c('stock'))
bb$nstock.data<-rowSums(bb[,2:3],na.rm=TRUE)

bb$nstock.data<-bb$nstock.data/5
bb$nstock.data<-ifelse(bb$nstock.data>=1,1,bb$nstock.data)
bb2<-bb

##NUMBER OF YEARS OVER WHICH STOCK VARIABLES ARE AVAILABLE
d<-subset(datf,qid==9 & stock %in% unique(datf$stock)[1])
f<-function(d){
d$ans<-gsub("[^0-9]", "", d$ans)
d$ans<-ifelse(is.na(d$ans),0,d$ans)
d$ans<-as.numeric(d$ans)
return(data.frame(lenstock.data=max(d$ans)))
}
bb<-ddply(subset(datf,qid==9),.(stock),.fun=f)
bb$lenstock.data<-bb$lenstock.data/40
bb$lenstock.data<-ifelse(bb$lenstock.data>=1,1,bb$lenstock.data)
bb3<-bb

bb2<-left_join(subset(bb2,select=c('stock','nstock.data')),bb3,by=c('stock'))
bb2$rich.data<-rowMeans(bb2[,2:3],na.rm=TRUE)
bb2<-subset(bb2,select=c('stock','rich.data'))



###############
##'ABILITY TO DETECT STATUS': LRP AVAILABLE?
bb<-unique(subset(datf,select=c('stock','lrp')))
bb3<-bb

###############
##'STOCK STATUS': PRECAUTIONARY APPROACH STATUS
bb<-unique(subset(datf,select=c('stock','status')))
bb$stock.status<-ifelse(bb$status %in% c('healthy'),1,0)
bb$stock.status<-ifelse(bb$status %in% c('cautious'),0.5,bb$stock.status)
bb4<-subset(bb,select=c('stock','stock.status'))


blist<-list(bb1,bb2,bb3,bb4)
brigor<-reduce(blist,join,by='stock')

#RELIABILITY BASES ON NUMBER OF INDICES AVAILABLE
brigor$rigor<-round(rowMeans(brigor[,2:5],na.rm=TRUE),digits=2)
brigor$rigorreliab<-round((4-rowSums(is.na(brigor[,2:5])))/4,digits=2)




##########################################################
##CLIMATE DATA INCORPORATION IN STOCK ASSESSMENTS
##########################################################
##NUMBER OF CLIMATE VARIABLES DIRECTLY INCLUDED IN STOCK ASSESSMENTS
f<-function(d){
d$ans<-ifelse(d$ans %in% c(''), NA, d$ans)
d$ans<-ifelse(is.na(d$ans), 0, 1)
return(data.frame(nclimate.dir=sum(d$ans)))
}
bb1<-ddply(subset(datf,qid %in% c(25) & question2 %in% c("Temperature","Oxygen", "Carbonate chemistry (pH)","Salinity","Primary productivity",    "Nutrients zooplankton/Secondary productivity", "Basin-scale climate indices", "Upwelling", "Ocean currents", "Sea ice")),.(stock),.fun=f)
d<-subset(datf,qid %in% c(25))
unique(d$question1)

##NUMBER OF YEARS OVER WHICH CLIMATE VARIABLES DIRECTLY INCLUDED IN STOCK ASSESSMENTS
f<-function(d){
d$ans<-as.numeric(d$ans)
d$ans<-ifelse(is.na(d$ans),0,d$ans)
return(data.frame(lenclimate.dir=max(d$ans)))
}
bb2<-ddply(subset(datf,qid %in% c(26)),.(stock),.fun=f)


##NUMBER OF CLIMATE VARIABLES INDIRECTLY INCLUDED IN STOCK ASSESSMENTS
f<-function(d){
d$ans<-ifelse(d$ans %in% c(''), NA, d$ans)
d$ans<-ifelse(is.na(d$ans), 0, 1)
return(data.frame(nclimate.indir=sum(d$ans)))
}
bb3<-ddply(subset(datf,qid %in% c(35)),.(stock),.fun=f)


##NUMBER OF YEARS OVER WHICH CLIMATE VARIABLES INDIRECTLY INCLUDED IN STOCK ASSESSMENTS
f<-function(d){
d$ans<-as.numeric(d$ans)
d$ans<-ifelse(is.na(d$ans),0,d$ans)
return(data.frame(lenclimate.indir=max(d$ans)))
}
bb4<-ddply(subset(datf,qid %in% c(36)),.(stock),.fun=f)


blist<-list(bb1,bb2,bb3,bb4)
bclim<-reduce(blist,join,by='stock')

bclim$climinc<-ifelse(bclim$nclimate.dir>=1,2,0)
bclim$climinc<-ifelse(bclim$nclimate.indir>=1,1,bclim$climinc)
bclim$climinc<-bclim$climinc/2

bclim$lenclimate.indir<-ifelse(bclim$nclimate.indir==0,0,bclim$lenclimate.indir)
bclim$climlen.dir<-round(bclim$lenclimate.dir/40,digits=2)
bclim$climlen.indir<-round(bclim$lenclimate.indir/40,digits=2)

bclim$climstock<-round(rowMeans(bclim[,6:8]),digits=2)
bclim$climreliab<-round((3-rowSums(is.na(bclim[,6:8])))/3,digits=2)

 
unique(datf$status)
##NO VARIABLES INDIRECTLY INCLUDED IN STOCK ASSESSMENTS, BUT MAX LENGTH OF VARIABLES INDIRECTLY INCLUDED > 0



blist<-list(subset(bclim,select=c('stock','climstock','climreliab')),
            subset(brigor,select=c('stock','rigor','rigorreliab')),
            subset(btransp,select=c('stock','transp','transpreliab')))
mdat<-reduce(blist,join,by='stock')

a1<-pivot_longer(subset(mdat,select=c('stock','climstock','rigor','transp')),
                  cols=-c('stock'),names_to = c("var"), values_to=c('value'),names_repair='unique')
names(a1)[3]<-'score'
a2<-pivot_longer(subset(mdat,select=c('stock','climreliab','rigorreliab','transpreliab')),
                  cols=-c('stock'),names_to = c("var"), values_to=c('value'),names_repair='unique')
a1$reliability<-a2$value

windows()
##extrafont::font_import()
extrafont::loadfonts()
library(gridExtra)

setwd(figsdir)
pdf('survey_scoring_stoplight.pdf',height=8,width=5)
ggplot()+
  geom_tile(data=a1,aes(x=var,y=stock,fill=score,alpha=reliability),col='white')+
  theme_ipsum()+
  scale_fill_gradientn(colors=brewer.ylorrd(100),guide='colourbar',name='',na.value='white',limits=c(0,1),breaks=c(0,0.5,1))
dev.off()


##############################################################

##BARRIERS RELATED TO COLLECTING CLIMATE DATA
##a<-unique(subset(datf,is.na(ans)==FALSE & qid %in% c(14,19,22,33,34,43,45,46,54,56,57,90,91),select=c('qid','question1','question2','ans')))
##write.csv(a,'N:/data/CAFF/data/Surveys/BarriersResources/BarriersResources2.csv',row.names=FALSE)
a<-read.csv('N:/data/CAFF/data/Surveys/BarriersResources/BarriersResources_filled.csv',header=TRUE)
datf<-left_join(datf,a,by=c('qid','question1','ans'))
datf$question1<-ifelse(datf$question1 %in% "",NA,datf$question1)
library(uchardet)

##STANDARDIZE ENCODING
##d<-subset(z,id==163)
f<-function(d){
if(is.na(d$question1)==FALSE){
ce<-detect_str_enc(d$question1)
##print(ce)
d$question1<-iconv(d$question1,from=ce,to="UTF-8")
} else NULL
return(d)  
}
datf<-adply(datf,1,.fun=f)

datf$qtype2<-ifelse((str_detect(datf$question1,pattern=c('barriers')))==TRUE,'Barriers',NA)
datf$qtype2<-ifelse(str_detect(datf$question1,'resources')==TRUE,'Resources',datf$qtype2)


##SAVE
save(datf,file='N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted_wBarriers.RData')
write.csv(datf,'N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted_wBarriers.csv',row.names=FALSE)
(load('N:/data/CAFF/data/Surveys/formatted/FiMACC_Survey_2023_Formatted_wBarriers.RData'))

a<-subset(datf,qid==21)
a<-subset(datf,question1 %in% c("Which of these options best describes how the existing monitoring program contributes to detecting changes in the stock associated with climate, oceanographic or ecological events?"))
View(a)




a<-unique(subset(datf,is.na(ans)==FALSE & 
                   qtype2 %in% c('Barriers','Resources') & 
                   qid %in% c(14,19,22,33,34,43,45,46,54,56,57,90,91)))
a$id<-gsub(' ','',paste(a$qtype2,'_',a$qtype))

a<-subset(a,select=c('qtype','qtype2','id','Data','Knowledge','Funding','Human.resources','Skills','Strategic','Industry','Communication','Ship.time'))

f<-function(d){
return(colSums(d[,4:12],na.rm=TRUE))
}
dt<-ddply(subset(a,select=c('qtype','qtype2','id','Data','Knowledge','Funding','Human.resources','Skills','Strategic','Industry','Communication','Ship.time')),.(id,qtype,qtype2),.fun=f)

dtl<-gather(dt,key=var,value=y,-c(id,qtype,qtype2))

setwd(figsdir)
##pdf('survey_scoring_stoplight.pdf',height=8,width=5)
windows()
ggplot()+
  geom_tile(data=dtl,aes(x=var,y=id,fill=y),col='white')+
  theme_ipsum()+
  scale_fill_gradientn(colors=brewer.ylorrd(100),guide='colourbar',name='',na.value='white',limits=c(0,64),breaks=c(0,64))
dev.off()



a<-subset(dtl,qtype2 %in% c('Barriers'))
a<-dtl
a$var<-factor(a$var,levels=c('Communication','Industry','Strategic','Ship.time','Funding','Knowledge','Human.resources','Data','Skills'))

setwd(figsdir)
pdf('survey_barriers_barplot.pdf',height=8,width=5)
ggplot()+
  geom_bar(data=a,aes(x=y,y=var,fill=qtype),stat='identity',width=0.75)+
##  geom_col()+
##  scale_fill_manual(values=c('taxo'='dodgerblue','spatial'='firebrick3'))+
##coord_flip()+
##  theme_ipsum()+
scale_fill_brewer(palette="Blues",name='Sector')+
facet_wrap(~qtype2)
dev.off()


d<-unique(subset(datf,question1 %in% c('Are you aware of any barriers to including environmental variables directly in the stock assessment?\n[Check the\xa0three\xa0most relevant barriers that apply]'),select=c('qid','question1')))



str_detect(c('Are you aware of any barriers to including environmental variables directly in the stock assessment?\n[Check the\xa0three\xa0most relevant barriers that apply]'),pattern=c('variables'))


mt<-ddply(mt,.(var),.fun=f)



###########################################

###########################################
library(sf)
mcrt<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
ws<-st_read('N:/data/shapefiles/eez/v8','World_EEZ_v8_2014_HR')
st_crs(ws)<-mcrt
names(ws)<-tolower(names(ws))
ws$sovereign<-ifelse(ws$sovereign %in% c('Democratic Republic of the Congo',"RÃ©publique du Congo"),'Congo',as.character(ws$sovereign))

am<-subset(ws,country %in% c('Canada','United States'))
ca<-subset(ws,country %in% c('Canada'))

eu<-subset(ws,country %in% c('Spain','France','Germany', 'Belgium','Denmark','Greece','Croatia','Ireland','Italy','Netherlands','Ireland','United Kingdom','Portugal'))
pdf('C:/Users/sailfish/Documents/aalldocuments/presentations/2023_WoodsHole/basemaps.pdf',height=3, width=3)
plot(st_geometry(am),axes=FALSE,border='magenta3',col='magenta3',lwd=0.001)
plot(st_geometry(eu),axes=FALSE,col='green3',border='green3',lwd=0.001)
plot(st_geometry(ca),axes=FALSE,col='dodgerblue3',border='dodgerblue3',lwd=0.001)
dev.off()
graphics.off() 
############################################

############################################






#####
#####
TROUBLESHOOT DUPLICATE QUESTIONS
a<-unique(subset(meta,select=c('page','question')))
a$dup<-duplicated(a$question)
b<-subset(a,dup==TRUE)

b<-subset(a,question %in% c('How accessible are the data collected? \n(Table1)'))
b<-subset(a,question %in% c('How many times was a stock assessment updated in the last 10 years?'))
b<-subset(a,question %in% c('Are you aware of any barriers to including environmental variables directly in the stock assessment?\n[Check the\xa0three\xa0most relevant barriers that apply]'))
b<-subset(a,question %in% c('Are you aware of any resources that would enable environmental variables to be directly included in the stock assessment? \n[Check the\xa0three\xa0most relevant resources that apply] '))
                            


