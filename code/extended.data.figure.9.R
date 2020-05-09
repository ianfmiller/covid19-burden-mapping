library(cowplot)
library(gridExtra)
library(grid)
library(ggplot2)

setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code")
source("analysis.prep.R")
setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code/data")

########## Extended data figure 1 ########## 

R0<-3
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
top.10.index.opt<-which(hosp.per.capita>=quantile(hosp.per.capita,na.rm = T,.9))

R0<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
top.10.index.pess<-which(hosp.per.capita>=quantile(hosp.per.capita,na.rm = T,.9))

US.p.rural<-urban.rural$Total.Rural/urban.rural$Total

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)
top.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[index]

hist.dat<-data.frame("p.rural"=c(US.p.rural,top.p.rural),"sub"=c(rep("US",times=length(US.p.rural)),rep("top10",times=length(top.p.rural))))
p1<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population resideing in rural area")+ylab("N counties")+ggtitle("a")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile hospitalizations per capita","US"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.35,.85),legend.title = element_blank(),legend.text = element_text(size=12))

####################################

R0<-3
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.opt<-which(spread.hospitalizations.per.bed>=quantile(spread.hospitalizations.per.bed,na.rm = T,.9))

R0<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.pess<-which(spread.hospitalizations.per.bed>=quantile(spread.hospitalizations.per.bed,na.rm = T,.9))

US.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[which(urban.rural$fips %in% hosp.data[which(hosp.data$calc.tot.beds>0),"fips"])]

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)
top.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[index]

hist.dat<-data.frame("p.rural"=c(US.p.rural,top.p.rural),"sub"=c(rep("US",times=length(US.p.rural)),rep("top10",times=length(top.p.rural))))
p2<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population resideing in rural area")+ylab("N counties")+ggtitle("b")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile hospitalizations per hospital bed","US counties with hospital beds"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.35,.85),legend.title = element_blank(),legend.text = element_text(size=12))
####################################

R0<-3
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
top.10.index.opt<-which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9))

R0<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
top.10.index.pess<-which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9))

US.p.rural<-urban.rural$Total.Rural/urban.rural$Total

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)
top.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[index]

hist.dat<-data.frame("p.rural"=c(US.p.rural,top.p.rural),"sub"=c(rep("US",times=length(US.p.rural)),rep("top10",times=length(top.p.rural))))
p3<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population resideing in rural area")+ylab("N counties")+ggtitle("c")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile ICU admissions per capita","US"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.35,.85),legend.title = element_blank(),legend.text = element_text(size=12))
####################################
R0<-3
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.opt<-which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9))

R0<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.pess<-which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9))

US.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[which(urban.rural$fips %in% hosp.data[which(hosp.data$icu.beds>0),"fips"])]

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)
top.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[index]

hist.dat<-data.frame("p.rural"=c(US.p.rural,top.p.rural),"sub"=c(rep("US",times=length(US.p.rural)),rep("top10",times=length(top.p.rural))))
p4<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population resideing in rural area")+ylab("N counties")+ylim(0,100)+ggtitle("d")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile ICU admissions per ICU bed","US counties with ICU beds"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.35,.85),legend.title = element_blank(),legend.text = element_text(size=12))

grid.arrange(p1,p2,p3,p4)

