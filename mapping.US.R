library(RCurl)
library(usmap)
library(ggplot2)
library(viridis)
library(geosphere)
library(doParallel)
library(foreach)

############# load/clean data #############

###demography data

raw.demog<-read.csv(text=getURL("https://raw.githubusercontent.com/ianfmiller/covid19-burden-mapping/master/us.demog.data.csv"),skip=1) #read from github
demog<-raw.demog[,c(1,2,3,grep("2018",colnames(raw.demog)))] #pull out 2018 data
colnames(demog)<-gsub("Population.Estimate..as.of.July.1....2018...","",colnames(demog)) #make column names easier to read
fips<-as.character(unlist(strsplit(as.character(demog$Id),"US"))[seq(2,nrow(demog)*2,2)]) #pull out fips codes
demog<-data.frame("fips"=fips,demog) #add on fips column
demog<-demog[order(demog$fips),] #sort by fips code

###hospital data

hosp.data<-read.csv(text=getURL("https://raw.githubusercontent.com/ianfmiller/covid19-burden-mapping/master/US.hosp.bed.data.csv")) #read from github
hosp.data$fips[which(nchar(hosp.data$fips)==4)]<-paste0("0",hosp.data$fips[which(nchar(hosp.data$fips)==4)]) #correct fips with leading 0s
hosp.data<-hosp.data[order(hosp.data$fips),]

#all(demog$fips==hosp.data$fips) check to make sure order matches

############## calculate case allocation ##########

geo<-read.csv(text=getURL("https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt")) #csv of fips center of populations
geo$STATEFP[which(nchar(geo$STATEFP)==1)]<-paste0("0",geo$STATEFP[which(nchar(geo$STATEFP)==1)]) #cleanup fips codes
geo$COUNTYFP[which(nchar(geo$COUNTYFP)==1)]<-paste0("00",geo$COUNTYFP[which(nchar(geo$COUNTYFP)==1)]) #cleanup fips codes
geo$COUNTYFP[which(nchar(geo$COUNTYFP)==2)]<-paste0("0",geo$COUNTYFP[which(nchar(geo$COUNTYFP)==2)]) #cleanup fips codes
geo<-cbind("fips"=paste0(geo$STATEFP,geo$COUNTYFP),geo) #add fips column
geo$fips<-as.character(geo$fips) #cleanup fips codes
geo<-geo[order(geo$fips),]

## correct fips codes for counties that changed names https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
#which(!demog$fips %in% geo$fips) #check which need to be corrected
geo$fips[93]<-"02158"
geo$fips[2418]<-"46102"

geo.sub.index<-which(geo$fips %in% fips)
all.x<-geo$LONGITUDE[geo.sub.index]
all.y<-geo$LATITUDE[geo.sub.index]

all.dists<-distm(cbind(all.x,all.y)) #get pairwise distances between fips--takes a few minutes
all.dists<-all.dists/1000 #scale to km

dist.mat<-matrix(all.dists,length(fips),length(fips))
colnames(dist.mat)<-fips
rownames(dist.mat)<-fips


dist.curve<-function(x,dist.50) 
{
  (1/dist.50)*exp(-x/dist.50)
}

allocate.cases<-function(county.fips,dist.50=20,max.dist=400,weight.dist=.1,bed.weight.metric="icu.beds") #function to allocate cases from one county to another based off of distances between county centers of population
{
  icu.hosp<-hosp.data[,c("fips",bed.weight.metric)]
  dists<-dist.mat[which(fips==county.fips),]
  icu.hosp<-data.frame(hosp.data,dists)
  icu.hosp<-subset(icu.hosp,icu.beds>0)
  cand.hosps<-subset(icu.hosp,dists<=max.dist)
  cand.hosps<-icu.hosp[which(as.numeric(icu.hosp[,"dists"])<=max.dist),]
  bed.weights<-cand.hosps$icu.beds/sum(cand.hosps$icu.beds)
  dist.weights<-dist.curve(cand.hosps$dists,dist.50)/sum(dist.curve(cand.hosps$dists,dist.50))
  weights<-bed.weights*dist.weights
  cand.hosps<-cbind(cand.hosps,"rel.weights"=weights/sum(weights))
  cand.hosps
}

age.classes<-c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80plus")
age.class.columns<-list(c(8,11),c(14,17),c(20,23),c(26,29),c(32,35),c(38,41),c(44,47),c(50,53),c(56,59))#columns of demog corresponding to N people within each age class

transfer.mat<-matrix(NA,length(fips),length(fips)) #rows are source fips, columns are destination fips
colnames(transfer.mat)<-fips
rownames(transfer.mat)<-fips

for (f in 1:length(fips))
{
  source.fips<-fips[f]
  p.out.cases<-allocate.cases(county.fips = source.fips)
  out.cases.row<-rep((0),times=length(fips))
  out.cases.row[c(match(p.out.cases$fips,fips))]<-p.out.cases$rel.weights
  transfer.mat[f,]<-out.cases.row
}

incoming<-function(dest.fips,data) #function to calculate total number of cases transfered to a fips area, including from within that fips area
{
  ##dest fips is fips of desination
  ##data is to vector of data to be spread between counties
  fips.index<-which(fips==dest.fips)
  incoming.cases<-matrix(rep(NA,times=length(fips)),1) #keeping with previous formatting convention, rows are sources, column is destination
  for (s in 1:length(fips))
  {
    incoming.cases[s]<-data[s]*transfer.mat[s,][fips.index]
  }
  sum(incoming.cases)
}

############# calculate severe cases #############

demog.binned<-c()
for (i in age.class.columns) 
{
  new.col<-rowSums(demog[,c(i[1],i[2])])
  demog.binned<-cbind(demog.binned,new.col)
}
colnames(demog.binned)<-age.classes

p.infected<-.4 #ballpark estimate
CFR <- c(.000094,.0000022,.0000091,.0018,.004,.013,.046,.098,.18) #data from https://www.medrxiv.org/content/10.1101/2020.03.04.20031104v1?__cf_chl_jschl_tk__=16ce8eb7d0f21ba0cf7f0e5c1e9ab3f05537f09d-1584455815-0-ATLBRAXQNjuKRpD1hwkdoXSA3GaNegxuQ5uze61iEmwJmJdpq81lzSyQYylBjmm21ep3aMVJPHjCPXJ03OuxPYSiQY-vlQmWcGumX9M1bPOangVdOhpyV7RbqI1Bo9xIuZDi6uqKf9ww2cM446ZJ4WdlJ7frJkQRLCqYREJaJgSXLh55iziDQpg0MEZcQsRr2SVw-Vnfea_IUb5bNMva6Q1iOAxBSD-71HBfOzIgOhatk1rlDQ_fZT79XEa21cDuYf_xfoYbyaaEfuuqMNRa6m48hGYdIQ42f-EaKyBMBINrVf8vlBbzThuhd1lzqLUfMNUMg_HyFeOcZxOb-uwj_JA
CFR.severe.cases<-.49 #data point from https://jamanetwork.com/journals/jama/article-abstract/2762130

severe.cases<-rowSums(demog.binned*p.infected*CFR*(1/CFR.severe.cases))

severe.cases.spread<-unlist(lapply(fips, incoming,data=severe.cases))

#sum(severe.cases); sum(severe.cases.spread) ##check to make sure that spread preserved severe case number


############# visualize #############

###relative burden of severe cases before allocation to healthsystems
rel.severe.cases<-severe.cases/mean(severe.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.severe.cases))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 0,na.value='grey60',name="log10(severe cases / mean(severe cases))\nbefore allocation to critical care facilities",limits=c(-plot.limits - 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")


###relative burden of severe cases after allocation to healthsystems
rel.severe.cases.spread<-severe.cases.spread/mean(severe.cases.spread)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.severe.cases.spread))
plot.data[which(plot.data$da< -1e6),"dat"]<-NA
plot.limits <- max(abs(plot.data$dat),na.rm=T)
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 0,na.value='grey60',name="log10(severe cases / mean(severe cases))\nafter allocation to critical care facilities",limits=c(-plot.limits- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")

###icu beds per severe case
spread.severe.cases.per.icu.beds<-severe.cases.spread/hosp.data$icu.beds
spread.severe.cases.per.icu.beds[which(spread.severe.cases.per.icu.beds==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(spread.severe.cases.per.icu.beds))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = mid.point,na.value='grey60',name="log10(severe cases per icu bed)\nafter allocation to critical care facilities",limits=c(1.5- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")

###total severe cases
plot.data<-data.frame("fips"=fips,"dat"=log10(severe.cases))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = mid.point,na.value='grey60',name="log10 (severe cases)",limits=c(-0- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")

###total ICU beds
plot.data<-data.frame("fips"=fips,"dat"=log10(hosp.data$icu.beds))
plot.data[which(hosp.data$icu.beds==0),"dat"]<-NA
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="#e5f5e0",mid="mediumseagreen",high="darkgreen",midpoint =mid.point,na.value='grey60',name="log10 (ICU beds)",limits=c(-0- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")

### total population
plot.data<-data.frame("fips"=fips,"dat"=log10(demog$Both.Sexes..Total))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="#e5f5e0",mid="mediumseagreen",high="darkgreen",midpoint =mid.point,na.value='grey60',name="log10 (population)",limits=c(2- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")

### % population over 60
plot.data<-data.frame("fips"=fips,"dat"=(demog$Both.Sexes..Total...60.to.64.years+demog$Both.Sexes..Total...65.to.69.years+demog$Both.Sexes..Total...70.to.74.years+demog$Both.Sexes..Total...75.to.79.years+demog$Both.Sexes..Total...80.to.84.years+demog$Both.Sexes..Total...85.years.and.over)/demog$Both.Sexes..Total)
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="#e5f5e0",mid="mediumseagreen",high="darkgreen",midpoint =mid.point,na.value='grey60',name="% population over 60",limits=c(0,plot.limits+.02))+
  theme(legend.position = "top")

