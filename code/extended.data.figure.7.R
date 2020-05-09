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
library(viridis)

R0<-3
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5

analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)

hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
rel.hosp.cases<-hosp.cases/mean(hosp.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.hosp.cases))

p1<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradientn(colors=viridis_pal(option="magma",direction = -1)(9),na.value='grey60',name="",limits=c(-3,2),breaks=seq(-3,2,1),labels=c("0.001","0.01","0.1","1","10","100"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title=expression(paste("projected   ",frac("cumulative hospitalizations","mean(cumulative hospitalizations)"))))+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.data<-data.frame("fips"=fips,"dat"=log10(hosp.per.capita))

p2<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradientn(colors=viridis_pal(option="magma",direction = -1)(9),na.value='grey60',name="",limits=c(-2.75,-1.68),breaks=log10(c(.001,.0025,.005,.01,.02)),labels=c(.001,.0025,.005,.01,.02))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative hospitalizations per capita\n")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(spread.hospitalizations.per.bed))

p3<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+ geom_tile(aes(x = x, y = y, group = group,colour=''))+scale_fill_gradientn(colors=viridis_pal(option="magma",direction = -1)(9),na.value='grey60',name="",limits=c(-.75,1),breaks=seq(-.75,1,.5),labels=round(10^seq(-.75,1,.5),1))+
  guides(colour=guide_legend("No\nhospitals", title.theme=element_text(size=6),override.aes=list(color="grey60",fill="grey60"),title.position = "right"))+theme(legend.position = "left",legend.spacing=unit(0.01, 'cm'),legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative hospitalizations per bed\n after allocation to healthcare systems")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

R0<-5
theta<-0
beta.mod.C<-1

analysis.name<-paste0("R0.dens.adj.",R0,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
rel.hosp.cases<-hosp.cases/mean(hosp.cases)
plot.data<-data.frame("fips"=fips,"dat"=log10(rel.hosp.cases))

p4<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradientn(colors=viridis_pal(option="magma",direction = -1)(9),na.value='grey60',name="",limits=c(-3,2),breaks=seq(-3,2,1),labels=c("0.001","0.01","0.1","1","10","100"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title=expression(paste("projected   ",frac("cumulative hospitalizations","mean(cumulative hospitalizations)"))))+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

hosp.cases<-readRDS(paste0(analysis.name,".hosp.cases.RDS"))
hosp.per.capita<-hosp.cases/demog$Both.Sexes..Total
plot.data<-data.frame("fips"=fips,"dat"=log10(hosp.per.capita))

p5<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+scale_fill_gradientn(colors=viridis_pal(option="magma",direction = -1)(9),na.value='grey60',name="",limits=c(-2.75,-1.68),breaks=log10(c(.001,.0025,.005,.01,.02)),labels=c(.001,.0025,.005,.01,.02))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative hospitalizations per capita\n,")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

hosp.cases.spread<-readRDS(paste0(analysis.name,".hosp.cases.spread.RDS"))
hosp.cases.spread[which(is.nan(hosp.cases.spread))]<-0
spread.hospitalizations.per.bed<-hosp.cases.spread/hosp.data$calc.tot.beds
spread.hospitalizations.per.bed[which(spread.hospitalizations.per.bed==0)]<-NA #switch to NA for plotting purposes
plot.data<-data.frame("fips"=fips,"dat"=log10(spread.hospitalizations.per.bed))

p6<-usmap::plot_usmap(data=plot.data,values = "dat",col=NA)+ geom_tile(aes(x = x, y = y, group = group,colour=''))+scale_fill_gradientn(colors=viridis_pal(option="magma",direction = -1)(9),na.value='grey60',name="",limits=c(-.75,1),breaks=seq(-.75,1,.5),labels=round(10^seq(-.75,1,.5),1))+
  guides(colour=guide_legend("No\nhospitals", title.theme=element_text(size=6),override.aes=list(color="grey60",fill="grey60"),title.position = "right"))+theme(legend.position = "left",legend.spacing=unit(0.01,"cm"),legend.key.width = unit(.5, "cm"),plot.title = element_text(hjust = .5,size=10))+labs(title="projected cumulative hospitalizations per bed\n after allocation to healthcare systems")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

R0.US<-3
theta<-1
beta.mod.C<-.1
analysis.name<-paste0("R0.dens.adj.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
SEIR.output<-readRDS(paste0(analysis.name,".SEIR.output.RDS"))
new.dat1<-c()
for (i in seq(1,nrow(demog.binned),10))
{
  infections<-SEIR.output[i,20:28]+SEIR.output[i,29:37]+SEIR.output[i,47:55]
  new.dat1<-rbind(new.dat1,infections*hosp.rates/demog.binned[i,])
}
new.dat1<-data.frame(new.dat1,"index"=1:dim(new.dat1)[1],"scenario"="optimistic")
new.dat1<-melt(new.dat1,id.vars = c("index","scenario"))
new.dat1$variable<-as.character(new.dat1$variable)
new.dat1$variable[which(new.dat1$variable=="Ip.0.9")]<-0
new.dat1$variable[which(new.dat1$variable=="Ip.10.19")]<-1
new.dat1$variable[which(new.dat1$variable=="Ip.20.29")]<-2
new.dat1$variable[which(new.dat1$variable=="Ip.30.39")]<-3
new.dat1$variable[which(new.dat1$variable=="Ip.40.49")]<-4
new.dat1$variable[which(new.dat1$variable=="Ip.50.59")]<-5
new.dat1$variable[which(new.dat1$variable=="Ip.60.69")]<-6
new.dat1$variable[which(new.dat1$variable=="Ip.70.79")]<-7
new.dat1$variable[which(new.dat1$variable=="Ip.80plus")]<-8

R0.US<-5
theta<-0
beta.mod.C<-1
analysis.name<-paste0("R0.dens.adj.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
SEIR.output<-readRDS(paste0(analysis.name,".SEIR.output.RDS"))
new.dat2<-c()
for (i in seq(1,nrow(demog.binned),10))
{
  infections<-SEIR.output[i,20:28]+SEIR.output[i,29:37]+SEIR.output[i,47:55]
  new.dat2<-rbind(new.dat2,infections*hosp.rates/demog.binned[i,])
}
new.dat2<-data.frame(new.dat2,"index"=((dim(new.dat2)[1]+1):(2*dim(new.dat2)[1])),"scenario"="pessimistic")
new.dat2<-melt(new.dat2,id.vars = c("index","scenario"))
new.dat2$variable<-as.character(new.dat2$variable)
new.dat2$variable[which(new.dat2$variable=="Ip.0.9")]<-0
new.dat2$variable[which(new.dat2$variable=="Ip.10.19")]<-1
new.dat2$variable[which(new.dat2$variable=="Ip.20.29")]<-2
new.dat2$variable[which(new.dat2$variable=="Ip.30.39")]<-3
new.dat2$variable[which(new.dat2$variable=="Ip.40.49")]<-4
new.dat2$variable[which(new.dat2$variable=="Ip.50.59")]<-5
new.dat2$variable[which(new.dat2$variable=="Ip.60.69")]<-6
new.dat2$variable[which(new.dat2$variable=="Ip.70.79")]<-7
new.dat2$variable[which(new.dat2$variable=="Ip.80plus")]<-8

plot.dat<-rbind(new.dat1,new.dat2)

p7<-ggplot(plot.dat,aes(variable,value,group=index,color=scenario))+geom_line()+scale_color_manual(values=c(rgb(0,0,1,.25),rgb(1,0,0,.25)))+
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text = element_text(size=8),axis.title = element_text(size=12),legend.position = c(.3,.8),legend.title = element_blank(),legend.text = element_text(size=12),plot.margin = margin(l=40,r=30,t=20,b=10))+
  labs(x="age",y="cumulative hospitalization rate")+
  scale_x_discrete(labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))+
  guides(color=guide_legend(override.aes=list(color=c("blue","red"),size=1.1)))

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

#315 counties in both top.10.index
#244 overlap

ID.index<-rep(.25,times=length(fips))
ID.index[intersect(top.10.index.opt,top.10.index.pess)]<-.75 
ID.index[top.10.index.opt[which(!top.10.index.opt %in% top.10.index.pess)]]<-.5
ID.index[top.10.index.pess[which(!top.10.index.pess %in% top.10.index.opt)]]<-1
plot.data<-data.frame("fips"=fips,"dat"=ID.index)
p8<-plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("grey60","blue","purple","red"),values=c(.25,.5,.75,1),guide="legend",name="",limits=c(0,1),breaks=c(.25,.5,.75,1),labels=c("neither","optimistic\nonly","both","pessimistic\nonly"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=6),plot.title = element_text(hjust = .5,size=10))+
  labs(title="counties at or above 90% quantile\ncumulative hospitalizations per capita")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)

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

#248 counties in both top.10.index
#223 overlap

ID.index<-rep(.25,times=length(fips))
ID.index[intersect(top.10.index.opt,top.10.index.pess)]<-.75
ID.index[top.10.index.opt[which(!top.10.index.opt %in% top.10.index.pess)]]<-.5
ID.index[top.10.index.pess[which(!top.10.index.pess %in% top.10.index.opt)]]<-1
plot.data<-data.frame("fips"=fips,"dat"=ID.index)
p9<-plot_usmap(data=plot.data,values = "dat",col=NA)+
  scale_fill_gradientn(colours=c("grey60","blue","purple","red"),values=c(.25,.5,.75,1),guide="legend",name="",limits=c(0,1),breaks=c(.25,.5,.75,1),labels=c("neither","optimistic\nonly","both","pessimistic\nonly"))+
  theme(legend.position = "left",legend.key.width = unit(.5, "cm"),legend.key.height = unit(.5, "cm"),legend.text = element_text(size=6),plot.title = element_text(hjust = .5,size=10))+
  labs(title="counties at or above 90% quantile\ncumulative hospitalizations per hospital bed")+geom_polygon(data =us_map(regions="states"),fill="NA",aes(x=x,y=y,group=group),color="black",size=.2)
panel.background = element_blank()

#title1<-textGrob(bquote(paste(R[0]," = 2, ",theta," = 1, ",b[c]," = 0.1")), gp=gpar(fontsize=20,font=8),hjust=.33)
#title2<-textGrob(bquote(paste(R[0]," = 6, ",theta," = 0, ",b[c]," = 1")), gp=gpar(fontsize=20,font=8),hjust=.33)
title1<-textGrob("alternate optimistic scenario", gp=gpar(fontsize=20),hjust=.33)
title2<-textGrob("alternate pessimistic scenario", gp=gpar(fontsize=20),hjust=.33)
title3<-textGrob("comparison", gp=gpar(fontsize=20),hjust=.33)

grid.arrange(
  grobs=list(
    arrangeGrob(
      grobs=list(
        arrangeGrob(p1,left = textGrob("a", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p2,left = textGrob("b", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p3,left = textGrob("c", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18)))
      ),top=title1)
    ,arrangeGrob(
      grobs=list(
        arrangeGrob(p4,left = textGrob("d", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p5,left = textGrob("e", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p6,left = textGrob("f", x = unit(3, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18)))
      ),top=title2),
    arrangeGrob(
      grobs=list(
        arrangeGrob(p7,left = textGrob("g", x = unit(2.25, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p8,left = textGrob("h", x = unit(2.25, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18))),
        arrangeGrob(p9,left = textGrob("i", x = unit(2.25, "npc"),  y  = unit(.85, "npc"), gp=gpar(col="black", fontsize=18)))
      ),top=title3)
  ),nrow=1,ncol=3)

