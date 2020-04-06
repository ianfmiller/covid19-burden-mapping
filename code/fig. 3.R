####################
###### fig. 2 ######
####################
setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code")
source("analysis.prep.R")

setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code/data")
library(ggplot2)
library(ggplotify)
library(gridExtra)
library(grid)
library(tiff)

R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5

analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)

ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
rel.ICU.cases<-ICU.cases/mean(ICU.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.ICU.cases))

p1<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = -.5,na.value='grey60',name="",limits=c(-3,2),breaks=seq(-3,2,1),labels=c(".001","0.01","0.1","1","10","100"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title=expression(paste("projected   ",frac("cumulative ICU admissions","mean(cumulative ICU admissions)"))))+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.data<-data.frame("fips"=fips,"dat"=log10(ICU.per.capita))

p2<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = -2.575,na.value='grey60',name="",limits=c(-3.1,-2),breaks=seq(-3,-2,.5),labels=round(10^seq(-3,-2,.5),3))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative ICU admissions per capita\n")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(spread.ICU.per.bed))

p3<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+ geom_tile(aes(x = x, y = y, group = group,colour=''))+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 1.25,na.value='grey60',name="",limits=c(.5,2.015),breaks=seq(.5,2.5,.5),labels=round(10^seq(.5,2.5,.5),3))+
  guides(colour=guide_legend("No\nICU beds", title.theme=element_text(size=6),override.aes=list(color="grey60",fill="grey60"),title.position = "right"))+theme(legend.position = "left",legend.spacing=unit(0.01, 'cm'),legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative ICU admissions per ICU bed\n after allocation to healthcare systems")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

R0.US<-6
theta<-0
beta.mod.C<-1

analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)

ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
rel.ICU.cases<-ICU.cases/mean(ICU.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.ICU.cases))

p4<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = -.5,na.value='grey60',name="",limits=c(-3,2),breaks=seq(-3,2,1),labels=c(".001","0.01","0.1","1","10","100"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title=expression(paste("projected   ",frac("cumulative ICU admissions","mean(cumulative ICU admissions)"))))+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
plot.data<-data.frame("fips"=fips,"dat"=log10(ICU.per.capita))

p5<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = -2.575,na.value='grey60',name="",limits=c(-3.1,-2),breaks=seq(-3,-2,.5),labels=round(10^seq(-3,-2,.5),3))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative ICU admissions per capita\n,")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(spread.ICU.per.bed))

p6<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+ geom_tile(aes(x = x, y = y, group = group,colour=''))+scale_fill_gradient2(low="palegoldenrod",mid="orangered",high="darkred",midpoint = 1.25,na.value='grey60',name="",limits=c(.5,2.015),breaks=seq(.5,2.5,.5),labels=round(10^seq(.5,2.5,.5),3))+
  guides(colour=guide_legend("No\nICU beds", title.theme=element_text(size=6),override.aes=list(color="grey60",fill="grey60"),title.position = "right"))+theme(legend.position = "left",legend.spacing=unit(0.01,"cm"),legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative ICU admissions per ICU bed\n after allocation to healthcare systems")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

dummy.plot<-function() 
{
  par(mar=c(5.1,5.1,1,1))
  R0.US<-2
  theta<-1
  beta.mod.C<-.1
  analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
  SEIR.output<-readRDS(paste0(analysis.name,".SEIR.output.RDS"))
  plot(0,0,type="n",xlim=c(1,9),ylim=c(0,.04),axes=F,xlab="age",ylab="cumulative ICU admission rate",cex.lab=2)
  axis(1,at=1:9,labels = c(age.classes[1:8],"80+"),cex.axis=1.5)
  axis(2,cex.axis=1.5)
  legend(1,.038,legend=c("optimisitic scenario","pessimistic scenario"),col=c("blue","red"),lty=1,lwd=4,cex=1.5)
  for (i in seq(1,nrow(demog.binned),10))
  {
    infections<-SEIR.output[i,20:28]+SEIR.output[i,29:37]+SEIR.output[i,47:55]
    points(infections*hosp.rates*icu.rates.given.hosp/demog.binned[i,],type="l",col=hsv(.7,1,1,alpha = .5))
  }
  
  R0.US<-6
  theta<-0
  beta.mod.C<-1
  analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
  SEIR.output<-readRDS(paste0(analysis.name,".SEIR.output.RDS"))
  for (i in seq(1,nrow(demog.binned),10))
  {
    infections<-SEIR.output[i,20:28]+SEIR.output[i,29:37]+SEIR.output[i,47:55]
    points(infections*hosp.rates*icu.rates.given.hosp/demog.binned[i,],type="l",col=hsv(0,1,1,alpha = .5))
  }
}

tiff("dummy.plot.tiff",compression="none")
dummy.plot()
dev.off()
p7<-rasterGrob(readTIFF("dummy.plot.tiff"))


R0.US<-2
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
top.10.index.opt<-which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9))

R0.US<-6
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases<-readRDS(paste0(analysis.name,".ICU.cases.RDS"))
ICU.per.capita<-ICU.cases/demog$Both.Sexes..Total
top.10.index.pess<-which(ICU.per.capita>=quantile(ICU.per.capita,na.rm = T,.9))

#315 counties in both top.10.index
#258 overlap

ID.index<-rep(.25,times=length(fips))
ID.index[intersect(top.10.index.opt,top.10.index.pess)]<-.75 
ID.index[top.10.index.opt[which(!top.10.index.opt %in% top.10.index.pess)]]<-.5
ID.index[top.10.index.pess[which(!top.10.index.pess %in% top.10.index.opt)]]<-1
plot.data<-data.frame("fips"=fips,"dat"=ID.index)
p8<-plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("grey60","blue","purple","red"),values=c(.25,.5,.75,1),guide="legend",name="",limits=c(0,1),breaks=c(.25,.5,.75,1),labels=c("neither","optimistic\nonly","both","pessimistic\nonly"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=6),plot.title = element_text(hjust = .5,size=10))+
  labs(title="counties at or above 90% quantile\ncumulative ICU admissions per capita")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)


R0.US<-2
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.opt<-which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9))

R0.US<-6
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
ICU.cases.spread<-readRDS(paste0(analysis.name,".ICU.cases.spread.RDS"))
ICU.cases.spread[which(is.nan(ICU.cases.spread))]<-0
spread.ICU.per.bed<-ICU.cases.spread/hosp.data$icu.beds
spread.ICU.per.bed[which(spread.ICU.per.bed==0)]<-NA #switch to NA for plotting purposes
top.10.index.pess<-which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9))

#136 counties in both top.10.index
#125 overlap

ID.index<-rep(.25,times=length(fips))
ID.index[intersect(top.10.index.opt,top.10.index.pess)]<-.75
ID.index[top.10.index.opt[which(!top.10.index.opt %in% top.10.index.pess)]]<-.5
ID.index[top.10.index.pess[which(!top.10.index.pess %in% top.10.index.opt)]]<-1
plot.data<-data.frame("fips"=fips,"dat"=ID.index)
p9<-plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("grey60","blue","purple","red"),values=c(.25,.5,.75,1),guide="legend",name="",limits=c(0,1),breaks=c(.25,.5,.75,1),labels=c("neither","optimistic\nonly","both","pessimistic\nonly"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=6),plot.title = element_text(hjust = .5,size=10))+
  labs(title="counties at or above 90% quantile\ncumulative ICU admissions per ICU bed")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

#title1<-textGrob(bquote(paste(R[0]," = 2, ",theta," = 1, ",b[c]," = 0.1")), gp=gpar(fontsize=20,font=8),hjust=.33)
#title2<-textGrob(bquote(paste(R[0]," = 6, ",theta," = 0, ",b[c]," = 1")), gp=gpar(fontsize=20,font=8),hjust=.33)
title1<-textGrob("optimistic scenario", gp=gpar(fontsize=20,font=8),hjust=.33)
title2<-textGrob("pessimistic scenario", gp=gpar(fontsize=20,font=8),hjust=.33)
title3<-textGrob("comparison", gp=gpar(fontsize=20,font=8),hjust=.33)

grid.arrange(
  grobs=list(
    arrangeGrob(
      grobs=list(
        arrangeGrob(p1,left = textGrob("A", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p2,left = textGrob("B", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p3,left = textGrob("C", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18)))
      ),top=title1)
    ,arrangeGrob(
      grobs=list(
        arrangeGrob(p4,left = textGrob("D", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p5,left = textGrob("E", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p6,left = textGrob("F", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18)))
      ),top=title2),
    arrangeGrob(
      grobs=list(
        arrangeGrob(p7,left = textGrob("G", x = unit(2.25, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p8,left = textGrob("H", x = unit(2.25, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p9,left = textGrob("I", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18)))
      ),top=title3)),
  nrow=1,ncol=3)
