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

hosp.data<-read.csv("US.hosp.bed.data.csv") #output of clean.hosp.data.R
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

get.transfer.mat<-function(bed.weight.metric="icu.beds")
{  
  transfer.mat<-matrix(NA,length(fips),length(fips)) #rows are source fips, columns are destination fips
  colnames(transfer.mat)<-fips
  rownames(transfer.mat)<-fips

  for (f in 1:length(fips))
    {
      source.fips<-fips[f]
      p.out.cases<-allocate.cases(county.fips = source.fips,bed.weight.metric = bed.weight.metric)
      out.cases.row<-rep((0),times=length(fips))
      out.cases.row[c(match(p.out.cases$fips,fips))]<-p.out.cases$rel.weights
      transfer.mat[f,]<-out.cases.row
    }
  transfer.mat
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
p.symptomatic<-.8 #ballpark estimate, same for all age classes
hosp.rates<-c(.001,.003,.012,.032,.049,.102,.166,.243,.273) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
icu.rates.given.hosp<-c(.05,.05,.05,.05,.063,.122,.274,.432,.709) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
IFR <- c(.00002,.00006,.0003,.0008,.0015,.006,.022,.051,.093) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf

population<-rowSums(demog.binned)
hosp.cases<-rowSums(p.infected*demog.binned*hosp.rates)
ICU.cases<-rowSums(p.infected*demog.binned*hosp.rates*icu.rates.given.hosp)
fatalities<-rowSums(p.infected*demog.binned*IFR)

transfer.mat<-get.transfer.mat(bed.weight.metric="calc.tot.beds")
hosp.cases.spread<-unlist(lapply(fips, incoming,data=hosp.cases))
transfer.mat<-get.transfer.mat(bed.weight.metric="icu.beds")
ICU.cases.spread<-unlist(lapply(fips, incoming,data=ICU.cases))

#sum(hosp.cases.spread); sum(hosp.cases.spread) ##check to make sure that spread preserved severe case number


############# visualize #############
save.plots<-T

### total population
if(save.plots) {png("population.png")}
plot.data<-data.frame("fips"=fips,"dat"=log10(demog$Both.Sexes..Total))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="#e5f5e0",mid="mediumseagreen",high="darkgreen",midpoint =mid.point,na.value='grey60',name="log10 (population)",limits=c(2- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

### fraction population over 60
if(save.plots) {png("p.pop.60.plus.png")}
plot.data<-data.frame("fips"=fips,"dat"=(demog$Both.Sexes..Total...60.to.64.years+demog$Both.Sexes..Total...65.to.69.years+demog$Both.Sexes..Total...70.to.74.years+demog$Both.Sexes..Total...75.to.79.years+demog$Both.Sexes..Total...80.to.84.years+demog$Both.Sexes..Total...85.years.and.over)/demog$Both.Sexes..Total)
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="#e5f5e0",mid="mediumseagreen",high="darkgreen",midpoint =mid.point,na.value='grey60',name="fraction population over 60",limits=c(0,plot.limits+.02))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###total hospital beds
if(save.plots) {png("hosp.beds.png")}
plot.data<-data.frame("fips"=fips,"dat"=log10(hosp.data$calc.tot.beds))
plot.data[which(hosp.data$icu.beds==0),"dat"]<-NA
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="#e5f5e0",mid="mediumseagreen",high="darkgreen",midpoint =mid.point,na.value='grey60',name="log10 (hospital beds)",limits=c(-0- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###total ICU beds
if(save.plots) {png("icu.beds.png")}
plot.data<-data.frame("fips"=fips,"dat"=log10(hosp.data$icu.beds))
plot.data[which(hosp.data$icu.beds==0),"dat"]<-NA
plot.limits <- max(abs(plot.data$dat),na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="#e5f5e0",mid="mediumseagreen",high="darkgreen",midpoint =mid.point,na.value='grey60',name="log10 (ICU beds)",limits=c(-0- 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###relative burden of hospitalizations before allocation to healthsystems
if(save.plots) {png("rel.hosp.pre.alloc.png")}
rel.hosp.cases<-hosp.cases/mean(hosp.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.hosp.cases))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 0,na.value='grey60',name="projected log10(hospitalizations / mean(hospitalizations))\n",limits=c(-plot.limits - 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###burden of hospitalizations per capita before allocation to healthsystems
if(save.plots) {png("hosp.per.capita.png")}
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.data<-data.frame("fips"=fips,"dat"=hosp.per.capita)
plot.limits <- max(plot.data$dat,na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = mid.point,na.value='grey60',name="projected hospitalizations per capita\nassuming 40% cumulative infection rate, 80% symptom rate  ",limits=c(min(plot.data$dat),plot.limits))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###relative burden of ICU admits before allocation to healthsystems
if(save.plots) {png("rel.icu.pre.alloc.png")}
rel.severe.cases<-ICU.cases/mean(ICU.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.severe.cases))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 0,na.value='grey60',name="projected log10(ICU admits / mean(ICU admits))",limits=c(-plot.limits - 0.2,plot.limits + 0.2))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###burden of ICU admits per capita before allocation to healthsystems
if(save.plots) {png("icu.per.capita.png")}
icu.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.data<-data.frame("fips"=fips,"dat"=icu.per.capita)
plot.limits <- max(plot.data$dat,na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = mid.point,na.value='grey60',name="projected ICU admits per capita\nassuming 40% cumulative infection rate, 80% symptom rate     ",limits=c(min(plot.data$dat),plot.limits))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###relative burden of hospitalizations after allocation to healthsystems
if(save.plots) {png("rel.hosp.post.alloc.png")}
rel.hosp.cases.spread<-hosp.cases.spread/mean(hosp.cases.spread)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.hosp.cases.spread))
plot.data[which(plot.data$da< -1e6),"dat"]<-NA
plot.limits <- max(abs(plot.data$dat),na.rm=T)
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 0,na.value='grey60',name="projected log10(hospitalizations / mean(hospitalizations))\nafter allocation to hospitals",limits=c(min(plot.data$dat,na.rm=T),plot.limits))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###relative burden of ICU admits after allocation to healthsystems
if(save.plots) {png("rel.icu.post.alloc.png")}
rel.ICU.cases.spread<-ICU.cases.spread/mean(ICU.cases.spread)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.ICU.cases.spread))
plot.data[which(plot.data$da< -1e6),"dat"]<-NA
plot.limits <- max(abs(plot.data$dat),na.rm=T)
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 0,na.value='grey60',name="projected log10(ICU admits / mean(ICU admits))\nafter allocation to critical care facilities",limits=c(min(plot.data$dat,na.rm=T),plot.limits))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###hospitalizations per hospital bed
if(save.plots) {png("hosp.per.bed.png")}
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(spread.hospitalizations.per.bed))
plot.max <- max(plot.data$dat,na.rm=T)
plot.min<-min(plot.data$dat,na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = mid.point,na.value='grey60',name="projected log10(hospitalizaitons per hospital bed)\nafter allocation to hospitals,\nassuming 40% cumulative infection rate, 80% symptom rate",limits=c(plot.min,plot.max))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}

###ICU admits per ICU bed
if(save.plots) {png("icu.per.bed.png")}
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(spread.ICU.per.bed))
plot.max <- max(plot.data$dat,na.rm=T)
plot.min<-min(plot.data$dat,na.rm=T)
mid.point<-min(plot.data$dat,na.rm=T)+ (max(plot.data$dat,na.rm=T)-min(plot.data$dat,na.rm=T))/2
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = mid.point,na.value='grey60',name="projected log10(ICU admits per ICU bed)\nafter allocation to critical care facilities,\nassuming 40% cumulative infection rate, 80% symptom rate",limits=c(plot.min,plot.max))+
  theme(legend.position = "top")
if(save.plots) {dev.off()}





