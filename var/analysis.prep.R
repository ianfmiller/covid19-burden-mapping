library(RCurl)
library(usmap)
library(geosphere)
library(doParallel)
library(foreach)
library(deSolve)

############# load/clean data #############

###demography data

raw.demog<-read.csv("us.demog.data.csv",skip=1) #read from github
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

###urban/rural data https://data.census.gov/cedsci/table?q=urban%20area%20deliniation&hidePreview=true&tid=DECENNIALSF12010.P2&vintage=2010&g=0100000US.050000
urban.rural<-read.csv("US.pop.urban.rural.csv")
urban.rural$fips[which(nchar(urban.rural$fips)==4)]<-paste0("0",urban.rural$fips[which(nchar(urban.rural$fips)==4)]) #correct fips with leading 0s

### correct fips codes for counties that changed names https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
urban.rural$fips[93]<-"02158"
urban.rural$fips[2418]<-"46102"
############## calculate case allocation #############

### get coordinates of county centers of population
geo<-read.csv("geo.csv") #csv of fips center of populations from https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt
geo$STATEFP[which(nchar(geo$STATEFP)==1)]<-paste0("0",geo$STATEFP[which(nchar(geo$STATEFP)==1)]) #cleanup fips codes
geo$COUNTYFP[which(nchar(geo$COUNTYFP)==1)]<-paste0("00",geo$COUNTYFP[which(nchar(geo$COUNTYFP)==1)]) #cleanup fips codes
geo$COUNTYFP[which(nchar(geo$COUNTYFP)==2)]<-paste0("0",geo$COUNTYFP[which(nchar(geo$COUNTYFP)==2)]) #cleanup fips codes
geo<-cbind("fips"=paste0(geo$STATEFP,geo$COUNTYFP),geo) #add fips column
geo$fips<-as.character(geo$fips) #cleanup fips codes
geo<-geo[order(geo$fips),]

### correct fips codes for counties that changed names https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
#which(!demog$fips %in% geo$fips) #check which need to be corrected
geo$fips[93]<-"02158"
geo$fips[2418]<-"46102"

### get distances between county centers of population
geo.sub.index<-which(geo$fips %in% fips)
all.x<-geo$LONGITUDE[geo.sub.index]
all.y<-geo$LATITUDE[geo.sub.index]

all.dists<-distm(cbind(all.x,all.y)) #get pairwise distances between fips--takes a few minutes
all.dists<-all.dists/1000 #scale to km

dist.mat<-matrix(all.dists,length(fips),length(fips))
colnames(dist.mat)<-fips
rownames(dist.mat)<-fips

### define function for weighting case destinations by distance
dist.curve<-function(x,dist.50) 
{
  (1/dist.50)*exp(-x/dist.50)
}

### function for allocating cases based off of distance and bed number--for a given county (county.fips) this function returns a matrix of destination locations and their proportion of cases allocated to each
allocate.cases<-function(county.fips,dist.50=20,max.dist=400,weight.dist=.1,bed.weight.metric="icu.beds") #function to allocate cases from one county to another based off of distances between county centers of population
{
  hosp<-hosp.data[,c("fips",bed.weight.metric)]
  dists<-dist.mat[which(fips==county.fips),]
  hosp<-data.frame(hosp.data,dists)
  hosp<-subset(hosp,bed.weight.metric>0)
  cand.hosps<-subset(hosp,dists<=max.dist)
  cand.hosps<-hosp[which(as.numeric(hosp[,"dists"])<=max.dist),]
  bed.weights<-cand.hosps[,bed.weight.metric]/sum(cand.hosps[,bed.weight.metric])
  dist.weights<-dist.curve(cand.hosps$dists,dist.50)/sum(dist.curve(cand.hosps$dists,dist.50))
  weights<-bed.weights*dist.weights
  cand.hosps<-cbind(cand.hosps,"rel.weights"=weights/sum(weights))
  cand.hosps
}

age.classes<-c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80plus")
age.class.columns<-list(c(8,11),c(14,17),c(20,23),c(26,29),c(32,35),c(38,41),c(44,47),c(50,53),c(56,59))#columns of demog corresponding to N people within each age class

### function for maxing matrix of case sources and destinations
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

### function to calculate total number of cases transfered to a fips area, including from within that fips area
incoming<-function(dest.fips,data)
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

############# county demographics #############

demog.binned<-c() #bind demography
for (i in age.class.columns) 
{
  new.col<-rowSums(demog[,c(i[1],i[2])])
  demog.binned<-cbind(demog.binned,new.col)
}
colnames(demog.binned)<-age.classes



############# function for scaling R0 by pop. density #############

R0.urban.func<-function(x,R0.max,R0.min)
{
  R0.min+(R0.max-R0.min)*(x/quantile(urban.rural$Total.Urban/urban.rural$Total,1))
}

############# load parameters #############

hosp.rates<-c(.001,.003,.012,.032,.049,.102,.166,.243,.273) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
icu.rates.given.hosp<-c(.05,.05,.05,.05,.063,.122,.274,.432,.709) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
IFR <- c(.00002,.00006,.0003,.0008,.0015,.006,.022,.051,.093) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
beta.mod.A<-0.5
