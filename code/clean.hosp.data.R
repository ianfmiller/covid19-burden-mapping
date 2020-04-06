raw.hosp.data<-read.csv("~/2018 AHA survey/COMMA/AS2018FULLFILE.csv") ### data from american hospital associaiton 2018 survey, aggregated by fips code
cols<-c("FCOUNTY","GENBD","PEDBD","MSICBD","NICBD","PEDICBD","HOSPBD","LBEDSA","BDTOT","BDH","IPDTOT","IPDH")
temp.hosp.data<-raw.hosp.data[,match(cols,colnames(raw.hosp.data))]
colnames(temp.hosp.data)<-c("fips","med.surg.beds","ped.med.surg.bed","icu.beds","nicu.beds","ped.icu.beds","calc.tot.beds","lisc.beds","tot.staffed.facility.beds","tot.staffed.hosp.beds","facility.inpatient.days","hosp.inpatiend.days")

hosp.data<-c() 
for (f in unique(temp.hosp.data$fips)) #aggregate by fips
{
  rows<-subset(temp.hosp.data,fips==f)
  new.row<-apply(rows,2,sum,na.rm=T)
  new.row["fips"]<-f
  new.row<-c("n.hosp"=nrow(rows),new.row)
  hosp.data<-rbind(hosp.data,new.row)
}

hosp.data<-as.data.frame(hosp.data,row.names = NA) #clean up
hosp.data$fips<-as.character(hosp.data$fips) 
hosp.data$fips[which(nchar(hosp.data$fips)==4)]<-paste0("0",hosp.data$fips[which(nchar(hosp.data$fips)==4)]) #add on 0s for fips with leading 0

missing.hosp.fips<-fips[which(!fips %in% hosp.data$fips)] #which fips have no hospitals

for(mf in missing.hosp.fips) #add in 0s for fips with no hospitals
{
  new.row<-c(0,mf,rep(0,times=ncol(hosp.data)-2))
  hosp.data<-rbind(hosp.data,new.row)
}

hosp.data<-hosp.data[-which(!hosp.data$fips %in% fips),] #cut out hospitals not in 50 US states 
write.csv(hosp.data,"US.hosp.bed.data.csv")