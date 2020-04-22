R0.US<-2
theta<-1
beta.mod.C<-.1
beta.mod.A<-.5

source("analysis.prep.R")

analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)
      
home.dir<-getwd()
setwd("~/SEIR/data")
SEIR.output<-readRDS(paste0(analysis.name,".SEIR.output.RDS"))
setwd(home.dir)

dim<-9
totalS<-SEIR.output[,2:(dim+1)]
totalE<-SEIR.output[,(dim+2):(2*dim+1)]
totalIp<-SEIR.output[,(2*dim+2):(3*dim+1)]
totalIc<-SEIR.output[,(3*dim+2):(4*dim+1)]
totalIa<-SEIR.output[,(4*dim+2):(5*dim+1)]
totalRs<-SEIR.output[,(5*dim+2):(6*dim+1)]
totalRa<-SEIR.output[,(6*dim+2):(7*dim+1)]

hosp.rates<-c(.001,.003,.012,.032,.049,.102,.166,.243,.273) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
icu.rates.given.hosp<-c(.05,.05,.05,.05,.063,.122,.274,.432,.709) #data https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf

hosp.cases<-c()
for(i in 1:nrow(totalS))
  {
    new.hosp.cases<-sum((totalIp[i,]+totalIc[i,]+totalRs[i,])*hosp.rates)
    hosp.cases<-c(hosp.cases,new.hosp.cases)
  }
        
  ICU.cases<-c()
  for(i in 1:nrow(totalS))
    {
      new.ICU.cases<-sum((totalIp[i,]+totalIc[i,]+totalRs[i,])*hosp.rates*icu.rates.given.hosp)
      ICU.cases<-c(ICU.cases,new.ICU.cases)
    }
        
saveRDS(hosp.cases,file=paste0(analysis.name,".hosp.cases.RDS"))
saveRDS(ICU.cases,file=paste0(analysis.name,".ICU.cases.RDS"))
        
transfer.mat<-get.transfer.mat(bed.weight.metric="calc.tot.beds")
hosp.cases.spread<-unlist(lapply(fips, incoming,data=hosp.cases))
saveRDS(hosp.cases.spread,file=paste0(analysis.name,".hosp.cases.spread.RDS"))
transfer.mat<-get.transfer.mat(bed.weight.metric="icu.beds")
ICU.cases.spread<-unlist(lapply(fips, incoming,data=ICU.cases))
saveRDS(ICU.cases.spread,file=paste0(analysis.name,".ICU.cases.spread.RDS"))
