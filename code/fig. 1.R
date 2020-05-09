library(cowplot)
library(gridExtra)
library(grid)
library(ggplot2)
library(viridis)

setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code")
source("analysis.prep.R")
setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code/data")

########## A ########## 
plot.data<-data.frame("fips"=fips,"dat"=log10(demog$Both.Sexes..Total))
plot.limits <- max(abs(plot.data$dat),na.rm=T)
p1<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_viridis(direction=-1,na.value='grey60',name="",limits=c(2- 0.2,plot.limits + 0.2),guide="colorbar",labels=c(expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6),expression(10^7)))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=17))+labs(title="\nPopulation")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

########## B ########## 
plot.data<-data.frame("fips"=fips,"dat"=(demog$Both.Sexes..Total...60.to.64.years+demog$Both.Sexes..Total...65.to.69.years+demog$Both.Sexes..Total...70.to.74.years+demog$Both.Sexes..Total...75.to.79.years+demog$Both.Sexes..Total...80.to.84.years+demog$Both.Sexes..Total...85.years.and.over)/demog$Both.Sexes..Total)
plot.limits <- max(abs(plot.data$dat),na.rm=T)
p2<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_viridis(direction=-1,na.value='grey60',name="",limits=c(0,plot.limits+.02))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=17))+labs(title="\nFraction of population over 60")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

########## C ########## 
plot.da
ta<-data.frame("fips"=fips,"dat"=urban.rural$Total.Rural/urban.rural$Total)
p3<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_viridis(direction=-1,na.value='grey60',name="")+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=17))+labs(title="Fraction of population\nresiding in rural area")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

########## D ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
rel.hosp.cases<-hosp.cases/mean(hosp.cases)
plot.dat1<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.hosp.cases),"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
rel.hosp.cases<-hosp.cases/mean(hosp.cases)
plot.dat2<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.hosp.cases),"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p4<-ggplot(plot.dat,aes(pop,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("total population")+ylab(expression(paste("projected   ",frac("cumulative hospitalizations","mean(cumulative hospitalizations)"))))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = c(.9,.1),legend.justification=c(1,0),legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))


########## E ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=hosp.per.capita,"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=hosp.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p5<-ggplot(plot.dat,aes(p.60.plus,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population over 60")+ylab("projected\ncumulative hospitalizations per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## F ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=hosp.per.capita,"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=hosp.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p6<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative hospitalizations per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## G ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.hospitalizations.per.bed,"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.hospitalizations.per.bed,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p7<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative hospitalizations per bed")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))


########## H ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
rel.ICU.cases<-ICU.cases/mean(ICU.cases)
plot.dat1<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.ICU.cases),"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
rel.ICU.cases<-ICU.cases/mean(ICU.cases)
plot.dat2<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.ICU.cases),"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p8<-ggplot(plot.dat,aes(pop,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("total population")+ylab(expression(paste("projected   ",frac("cumulative ICU admissions","mean(cumulative ICU admissions)"))))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = c(.9,.1),legend.justification=c(1,0),legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## I ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=ICU.per.capita,"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=ICU.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p9<-ggplot(plot.dat,aes(p.60.plus,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population over 60")+ylab("projected\ncumulative ICU admissions per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))


########## J ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=ICU.per.capita,"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=ICU.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p10<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative ICU admissions per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## K ##########

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICUitalizations.per.bed<-ICU.cases.spread/hosp.data$calc.tot.beds
spread.ICUitalizations.per.bed[which(spread.ICUitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.ICUitalizations.per.bed,"sub"="optimistic scenario")
R0.US<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICUitalizations.per.bed<-ICU.cases.spread/hosp.data$calc.tot.beds
spread.ICUitalizations.per.bed[which(spread.ICUitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.ICUitalizations.per.bed,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p11<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative ICU admissions per ICU bed")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))



########## plot ########## 
layout.mat<-matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10,11,11,11),3,12,byrow = T)

grid.arrange(
  arrangeGrob(
    grobs=list(
      arrangeGrob(p1,left = textGrob("a", x = unit(1, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p2,left = textGrob("b", x = unit(1, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p3,left = textGrob("c", x = unit(1, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p4,left = textGrob("d", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p5,left = textGrob("e", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p6,left = textGrob("f", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p7,left = textGrob("g", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p8,left = textGrob("h", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p9,left = textGrob("i", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p10,left = textGrob("j", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18))),
      arrangeGrob(p11,left = textGrob("k", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18)))
    )
    ,layout_matrix = layout.mat)
)
                      
