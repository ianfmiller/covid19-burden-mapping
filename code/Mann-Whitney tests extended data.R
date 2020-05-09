#### Mann-Whitney U Tests
setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code")
source("analysis.prep.R")
setwd("~/Dropbox (Princeton)/Mapping the Burden of COVID-19 in the U.S./code/data")

### per capita hospitalizations
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

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)

x<-(urban.rural$Total.Rural/urban.rural$Total)[index]
y<-urban.rural$Total.Rural/urban.rural$Total

median(x)
median(y)
wilcox.test(x,y,alternative = "two.sided")

### per capita ICU admissions
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

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)

x<-(urban.rural$Total.Rural/urban.rural$Total)[index]
y<-urban.rural$Total.Rural/urban.rural$Total

median(x)
median(y)
wilcox.test(x,y,alternative = "two.sided")

### hospitalizations per hospital bed
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

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)

x<-(urban.rural$Total.Rural/urban.rural$Total)[index]
y<-(urban.rural$Total.Rural/urban.rural$Total)[which(hosp.data$calc.tot.beds>0)]

median(x)
median(y)
wilcox.test(x,y,alternative = "two.sided")

### ICU admission per ICU bed
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
top.10.index.pess<-which(spread.ICU.per.bed>=quantile(spread.ICU.per.bed,na.rm = T,.9))

pull.fips<-fips[intersect(top.10.index.opt,top.10.index.pess)]
index<-match(pull.fips,urban.rural$fips)

x<-(urban.rural$Total.Rural/urban.rural$Total)[index]
y<-(urban.rural$Total.Rural/urban.rural$Total)[which(hosp.data$icu.beds>0)]

median(x)
median(y)
wilcox.test(x,y,alternative = "two.sided")
