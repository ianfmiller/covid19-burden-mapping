library(cowplot)
library(gridExtra)
library(grid)
library(ggplot2)
library(viridis)

setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code")
source("analysis.prep.R")
setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code/data")

########## A ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
rel.hosp.cases<-hosp.cases/mean(hosp.cases)
plot.dat1<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.hosp.cases),"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
rel.hosp.cases<-hosp.cases/mean(hosp.cases)
plot.dat2<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.hosp.cases),"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p1<-ggplot(plot.dat,aes(pop,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c(expression(R["0 max"]*" = 3"),expression(R["0 max"]*" = 5")))+
  xlab("total population")+ylab(expression(paste("projected   ",frac("cumulative hospitalizations","mean(cumulative hospitalizations)"))))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = c(.9,.1),legend.justification=c(1,0),legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))


########## B ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=hosp.per.capita,"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=hosp.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p2<-ggplot(plot.dat,aes(p.60.plus,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c(expression(R["0 max"]*" = 3"),expression(R["0 max"]*" = 5")))+
  xlab("fraction population over 60")+ylab("projected\ncumulative hospitalizations per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## C ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=hosp.per.capita,"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=hosp.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p3<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c(expression(R["0 max"]*" = 3"),expression(R["0 max"]*" = 5")))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative hospitalizations per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## D ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.hospitalizations.per.bed,"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.hospitalizations.per.bed,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p4<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative hospitalizations per bed")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))


########## E ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
rel.ICU.cases<-ICU.cases/mean(ICU.cases)
plot.dat1<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.ICU.cases),"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
rel.ICU.cases<-ICU.cases/mean(ICU.cases)
plot.dat2<-data.frame("pop"=log10(rowSums(demog.binned)),"dat"=log10(rel.ICU.cases),"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p5<-ggplot(plot.dat,aes(pop,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c(expression(R["0 max"]*" = 3"),expression(R["0 max"]*" = 5")))+
  xlab("total population")+ylab(expression(paste("projected   ",frac("cumulative ICU admissions","mean(cumulative ICU admissions)"))))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = c(.9,.1),legend.justification=c(1,0),legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## F ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=ICU.per.capita,"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.60.plus"=rowSums(demog.binned[,7:9])/rowSums(demog.binned),"dat"=ICU.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p6<-ggplot(plot.dat,aes(p.60.plus,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population over 60")+ylab("projected\ncumulative ICU admissions per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))


########## G ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=ICU.per.capita,"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=ICU.per.capita,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p7<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative ICU admissions per capita")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))

########## H ##########

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICUitalizations.per.bed<-ICU.cases.spread/hosp.data$calc.tot.beds
spread.ICUitalizations.per.bed[which(spread.ICUitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat1<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.ICUitalizations.per.bed,"sub"="optimistic scenario")
R0<-5
theta<-0
beta.mod.C<-1
beta.mod.A<-.5
analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICUitalizations.per.bed<-ICU.cases.spread/hosp.data$calc.tot.beds
spread.ICUitalizations.per.bed[which(spread.ICUitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.dat2<-data.frame("p.rural"=urban.rural$Total.Rural/urban.rural$Total,"dat"=spread.ICUitalizations.per.bed,"sub"="pessimsitic scenario")
plot.dat<-rbind(plot.dat1,plot.dat2)

p8<-ggplot(plot.dat,aes(p.rural,dat))+geom_point(aes(color=sub),size=1.2)+scale_color_manual(values=c(rgb(0,0,1,.4),rgb(1,0,0,.4)),labels=c("optimistic\nscenario","pessimistic\nscenario"))+
  xlab("fraction population residing in rural area")+ylab("projected\ncumulative ICU admissions per ICU bed")+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=10),axis.title = element_text(size=10),legend.position = "none",legend.title = element_blank(),legend.text = element_text(size=10),plot.margin = margin(l=1,r=10,t=20,b=10))


layout.mat<-matrix(c(1,2,3,4,5,6,7,8),2,4,byrow = T)

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
      arrangeGrob(p8,left = textGrob("h", x = unit(.4, "npc"),  y  = unit(.9, "npc"), gp=gpar(col="black", fontsize=18)))
    )
    ,layout_matrix = layout.mat)
)



