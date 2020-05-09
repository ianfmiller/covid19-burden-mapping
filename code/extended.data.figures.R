library(cowplot)
library(gridExtra)
library(grid)
library(ggplot2)

setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code")
source("analysis.prep.R")
setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code/data")

########## Extended data figure 1 ########## 

R0.US<-2
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
top.10.index.opt<-which(hosp.per.capita>=quantile(hosp.per.capita,na.rm = T,.9))

R0.US<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
top.10.index.pess<-which(hosp.per.capita>=quantile(hosp.per.capita,na.rm = T,.9))

US.p.rural<-urban.rural$Total.Rural/urban.rural$Total

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)
top.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[index]

hist.dat<-data.frame("p.rural"=c(US.p.rural,top.p.rural),"sub"=c(rep("US",times=length(US.p.rural)),rep("top10",times=length(top.p.rural))))
p1<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population residing in rural area")+ylab("N counties")+ggtitle("a")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile hospitalizations per capita","US"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.35,.875),legend.title = element_blank(),legend.text = element_text(size=12))

####################################

R0.US<-2
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.opt<-which(spread.hospitalizations.per.bed>=quantile(spread.hospitalizations.per.bed,na.rm = T,.9))

R0.US<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
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
p2<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population residing in rural area")+ylab("N counties")+ggtitle("b")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile hospitalizations per hospital bed","US counties with hospital beds"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.4,.875),legend.title = element_blank(),legend.text = element_text(size=12))
####################################

R0.US<-2
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
top.10.index.opt<-which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9))

R0.US<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
top.10.index.pess<-which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9))

US.p.rural<-urban.rural$Total.Rural/urban.rural$Total

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)
top.p.rural<-(urban.rural$Total.Rural/urban.rural$Total)[index]

hist.dat<-data.frame("p.rural"=c(US.p.rural,top.p.rural),"sub"=c(rep("US",times=length(US.p.rural)),rep("top10",times=length(top.p.rural))))
p3<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population residing in rural area")+ylab("N counties")+ggtitle("c")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile ICU admissions per capita","US"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.35,.875),legend.title = element_blank(),legend.text = element_text(size=12))
####################################
R0.US<-2
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.opt<-which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9))

R0.US<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
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
p4<-ggplot(hist.dat,aes(x=p.rural))+xlab("fraction of population residing in rural area")+ylab("N counties")+ylim(0,100)+ggtitle("d")+
  geom_histogram(data=subset(hist.dat,sub=="US"),aes(fill=sub))+
  geom_histogram(data=subset(hist.dat,sub=="top10"),aes(fill=sub))+
  theme_bw()+
  scale_fill_manual(name="sub", values=c("purple","grey60"),labels=c("90% quantile ICU admissions per ICU bed","US counties with ICU beds"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=12),axis.title = element_text(size=12),legend.position = c(.35,.875),legend.title = element_blank(),legend.text = element_text(size=12))

grid.arrange(p1,p2,p3,p4)

########## Extended data figure 2 ########## 

R0.US.levels<-c(2,3,5)
theta.levels<-c(0,.5,1)
beta.mod.C.levels<-c(.1,.5,1)


index<-1
all.top.10<-c()
for(i in 1:3) #add nested loops if needed for other models, subsets, etc.
{
  for (j in 1:3)
  {
    for (k in 1:3)
    {
      
      R0.US<-R0.US.levels[i]
      theta<-theta.levels[j]
      beta.mod.C<-beta.mod.C.levels[k]
      
      analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
      hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
      hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
      assign(paste0("top.10.",index),which(hosp.per.capita>=quantile(hosp.per.capita,na.rm = T,.9)))
      all.top.10<-c(all.top.10,which(hosp.per.capita>=quantile(hosp.per.capita,na.rm = T,.9)))
      index<-index+1
    }
  }
}

par(mfrow=c(1,1))

ID.index<-rep(0,times=length(fips))
for(i in 1:27)
{
  sub.1.index<-which(table(all.top.10)==i)
  sub.2.index<-names(table(all.top.10))[sub.1.index]
  ID.index[as.numeric(sub.2.index)]<-i/27
}

plot.data<-data.frame("fips"=fips,"dat"=ID.index)
plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("white",viridis::magma(27,direction = -1)),values=c(0,(1:27)/27),guide="legend",name="",limits=c(0,1),breaks=c(0,(1:27)/27),labels=c(0:27))+
  theme(legend.key=element_rect(color="black"),legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=10),plot.title = element_text(hjust = .5,size=20))+
  labs(title="N transmisison scenarios identifying\ncounty as being at or above 90% quantile\n of per capita hospitalizations")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

########## Extended data figure 3 ########## 

R0.US.levels<-c(2,3,5)
theta.levels<-c(0,.5,1)
beta.mod.C.levels<-c(.1,.5,1)


index<-1
all.top.10<-c()
for(i in 1:3) #add nested loops if needed for other models, subsets, etc.
{
  for (j in 1:3)
  {
    for (k in 1:3)
    {
      
      R0.US<-R0.US.levels[i]
      theta<-theta.levels[j]
      beta.mod.C<-beta.mod.C.levels[k]
      
      analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
      hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
      hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
      spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
      spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
      assign(paste0("top.10.",index),which(spread.hospitalizations.per.bed>=quantile(spread.hospitalizations.per.bed,na.rm = T,.9)))
      all.top.10<-c(all.top.10,which(spread.hospitalizations.per.bed>=quantile(spread.hospitalizations.per.bed,na.rm = T,.9)))
      index<-index+1
    }
  }
}

par(mfrow=c(1,1))

ID.index<-rep(1/28,times=length(fips))
ID.index[which(hosp.data$calc.tot.beds==0)]<-0
for(i in 1:27)
{
  sub.1.index<-which(table(all.top.10)==i)
  sub.2.index<-names(table(all.top.10))[sub.1.index]
  ID.index[as.numeric(sub.2.index)]<-(i+1)/28
}

plot.data<-data.frame("fips"=fips,"dat"=ID.index)
plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("grey60","white",viridis::magma(27,direction=-1)),values=c(0,(1:28)/28),guide="legend",name="",limits=c(0,1),breaks=c(0,(1:28)/28),labels=c("no hosp. beds",0:27))+
  theme(legend.key=element_rect(color="black"),legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=10),plot.title = element_text(hjust = .5,size=20))+
  labs(title="N transmission scenarios identifying\ncounty as being at or above 90% quantile\n of hospitalizations per hospital bed")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

########## Extended data figure 4 ########## 

R0.US.levels<-c(2,3,5)
theta.levels<-c(0,.5,1)
beta.mod.C.levels<-c(.1,.5,1)


index<-1
all.top.10<-c()
for(i in 1:3) #add nested loops if needed for other models, subsets, etc.
{
  for (j in 1:3)
  {
    for (k in 1:3)
    {
      
      R0.US<-R0.US.levels[i]
      theta<-theta.levels[j]
      beta.mod.C<-beta.mod.C.levels[k]
      
      analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
      ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
      ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
      assign(paste0("top.10.",index),which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9)))
      all.top.10<-c(all.top.10,which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9)))
      index<-index+1
    }
  }
}

par(mfrow=c(1,1))

ID.index<-rep(0,times=length(fips))
for(i in 1:27)
{
  sub.1.index<-which(table(all.top.10)==i)
  sub.2.index<-names(table(all.top.10))[sub.1.index]
  ID.index[as.numeric(sub.2.index)]<-i/27
}

plot.data<-data.frame("fips"=fips,"dat"=ID.index)
plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("white",viridis::magma(27,direction=-1)),values=c(0,(1:27)/27),guide="legend",name="",limits=c(0,1),breaks=c(0,(1:27)/27),labels=c(0:27))+
  theme(legend.key=element_rect(color="black"),legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=10),plot.title = element_text(hjust = .5,size=20))+
  labs(title="N transmission scenarios identifying\ncounty as being at or above 90% quantile\n of per capita ICU admissions")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

########## Extended data figure 5 ########## 

R0.US.levels<-c(2,3,5)
theta.levels<-c(0,.5,1)
beta.mod.C.levels<-c(.1,.5,1)


index<-1
all.top.10<-c()
for(i in 1:3) #add nested loops if needed for other models, subsets, etc.
{
  for (j in 1:3)
  {
    for (k in 1:3)
    {
      
      R0.US<-R0.US.levels[i]
      theta<-theta.levels[j]
      beta.mod.C<-beta.mod.C.levels[k]
      
      analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
      ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
      ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
      spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
      spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
      assign(paste0("top.10.",index),which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9)))
      all.top.10<-c(all.top.10,which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9)))
      index<-index+1
    }
  }
}

par(mfrow=c(1,1))

ID.index<-rep(1/28,times=length(fips))
ID.index[which(hosp.data$icu.beds==0)]<-0
for(i in 1:27)
{
  sub.1.index<-which(table(all.top.10)==i)
  sub.2.index<-names(table(all.top.10))[sub.1.index]
  ID.index[as.numeric(sub.2.index)]<-(i+1)/28
}

plot.data<-data.frame("fips"=fips,"dat"=ID.index)
plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("grey60","white",viridis::magma(27,direction=-1)),values=c(0,(1:28)/28),guide="legend",name="",limits=c(0,1),breaks=c(0,(1:28)/28),labels=c("no ICU beds",0:27))+
  theme(legend.key=element_rect(color="black"),legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=10),plot.title = element_text(hjust = .5,size=20))+
  labs(title="N transmission scenarios identifying\ncounty as being at or above 90% quantile\n of ICU admissions per ICU bed")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

