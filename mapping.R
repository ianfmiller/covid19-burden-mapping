library(RCurl)
library(usmap)
library(ggplot2)
library(viridis)

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

############# calculate severe cases #############

age.classes<-c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80plus")
age.class.columns<-list(c(7,10),c(13,16),c(19,22),c(25,28),c(31,34),c(37,40),c(43,46),c(49,52),c(55,58)) #columns of dataset corresponding to N people within each age class
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

############# visualize #############

###relative burden of severe cases
rel.severe.cases<-severe.cases/mean(severe.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.severe.cases))
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_continuous(low="white",high="darkred",name="log10\n(severe cases /\nmean severe\ncases)")

###icu beds per severe case--assuming 
icu.beds.per.severe.case<-hosp.data$icu.beds/severe.cases
icu.beds.per.severe.case[which(icu.beds.per.severe.case==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(icu.beds.per.severe.case))
usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_continuous(low="darkred",high="white",name="log10\n(ICU beds : \nsevere cases)\n -cumulative\n -assuming\n  40% infected")

###total severe cases
plot.data<-data.frame("fips"=fips,"severe.cases"=log10(severe.cases))
usmap::plot_usmap(data=plot.data,values = "severe.cases",col=NA)+scale_fill_continuous(low="white",high="red",name="log10\nsevere cases)\n -cumulative\n -assuming\n  40% infected")

###total ICU beds
plot.data<-data.frame("fips"=fips,"icu.beds"=log10(hosp.data$icu.beds))
usmap::plot_usmap(data=plot.data,values = "icu.beds",col=NA)+scale_fill_continuous(type="viridis",name="log10\nICU beds")

### total population
plot.data<-data.frame("fips"=fips,"pop"=log10(demog$Both.Sexes..Total))
usmap::plot_usmap(data=plot.data,values = "pop",col=NA)+scale_fill_continuous(type="viridis",name="log10\npopulation")

### % population over 60
plot.data<-data.frame("fips"=fips,"p.60.plus"=(demog$Both.Sexes..Total...60.to.64.years+demog$Both.Sexes..Total...65.to.69.years+demog$Both.Sexes..Total...70.to.74.years+demog$Both.Sexes..Total...75.to.79.years+demog$Both.Sexes..Total...80.to.84.years+demog$Both.Sexes..Total...85.years.and.over)/demog$Both.Sexes..Total)
usmap::plot_usmap(data=plot.data,values = "p.60.plus",col=NA)+scale_fill_continuous(type="viridis",name="% over 60")



plot.data<-data.frame("fips"=fips,"n.severe"=log10(severe.cases))
usmap::plot_usmap(data=plot.data,values = "n.severe",col=NA)+scale_fill_continuous(type='viridis',name="log10 severe cases")

######## plot hospital data
plot.data<-data.frame("fips"=hosp.data$fips,"n.hosp"=as.numeric(hosp.data$n.hosp))
usmap::plot_usmap(data=plot.data,values = "n.hosp",col=NA)+scale_fill_continuous(type="viridis",name="N hosp")

plot.data<-data.frame("fips"=hosp.data$fips,"n.hosp"=as.numeric(hosp.data$icu.beds))
usmap::plot_usmap(data=plot.data,values = "n.hosp",col=NA)+scale_fill_continuous(type="viridis",name="ICU beds")

###MO only
usmap::plot_usmap(data=plot.data,include=county.fips[grep("missouri",county.fips$polyname),1],values = "dat",col=NA)+scale_fill_continuous(type="viridis",name="log 10 ( ICU beds\n per severe case)")

