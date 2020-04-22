dim<-9 #set dimensions of contact matrix

### load and format contact data--use UK paterns for US
require(socialmixr)
data(polymod)
age.limits<-c(0,10,20,30,40,50,60,70,80,90)
contact.mat <- contact_matrix(polymod, countries = "United Kingdom", age.limits = age.limits)$matrix
#contact data only available for 70+, need to synthesize for [70.80), 80+
#divide contacts to to 70+ category to [70.80), 80+ categories in proportion to (N adults [70,80)/N adults 70+) and (N adults 80+ /N adults 70+) using data from entire US
#assume everyone 80+ has same contact patterns as everyone [70,80)
contact.mat<-cbind(contact.mat,contact.mat[,8]) 
colnames(contact.mat)[8]<-"[70,80)"
colnames(contact.mat)[9]<-"80+"
p.contacts.70.80<-contact.mat[,8]*(colSums(demog.binned)[8]/(colSums(demog.binned)[8]+colSums(demog.binned)[9]))
p.contacts.80.plus<-contact.mat[,8]*(colSums(demog.binned)[9]/(colSums(demog.binned)[8]+colSums(demog.binned)[9]))
contact.mat[,8]<-p.contacts.70.80
contact.mat[,9]<-p.contacts.80.plus
contact.mat<-rbind(contact.mat,contact.mat[8,])
rownames(contact.mat)[8]<-"[70,80)"
rownames(contact.mat)[9]<-"80+"


### rescale contact matrix to reflect various degrees of mixing
for (i in 1:dim)
{
  for (j in 1:dim)
  {
    contact.mat[i,j]<-(1-theta)*sum(contact.mat[i,])/dim + theta*contact.mat[i,j]
  }
}

### get symptomatic rates for each age class--data/method from https://cmmid.github.io/topics/covid19/current-patterns-transmission/reports/age_hypotheses/Main%20text%20-%20COVID-19%20dynamics%20in%20children_%20implications%20for%20policy%20-%20no_line_numbers.pdf
cm_interpolate_cos = function(x, x0, y0, x1, y1)
{
  ifelse(x < x0, y0, ifelse(x > x1, y1, y0 + (y1 - y0) * (0.5 - 0.5 * cos(pi * (x - x0) / (x1 - x0)))))
}
symp.pars<-data.frame("age_y"=14,"age_m"=55,"age_o"=64,"symp_y"=.056,"symp_m"=.49,"symp_o"=.74)
ages.bin.centers<-c(5,15,25,35,45,55,65,75,85)
young  = cm_interpolate_cos(ages.bin.centers, symp.pars$age_y, 1, symp.pars$age_m, 0);
old    = cm_interpolate_cos(ages.bin.centers, symp.pars$age_m, 0, symp.pars$age_o, 1);
middle = 1 - young - old;

rel.symp<-young * symp.pars$symp_y + middle * symp.pars$symp_m + old * symp.pars$symp_o

### set parameters, units in days. I added in beta.mod.C to account for effects of quarantining. From https://cmmid.github.io/topics/covid19/current-patterns-transmission/reports/age_hypotheses/Main%20text%20-%20COVID-19%20dynamics%20in%20children_%20implications%20for%20policy%20-%20no_line_numbers.pdf
dE<-4
dP<-2.4
dC<-3.2
dA<-7
beta1 <-0 ## seasonal forcing should be modest here--not included in model for now
phase <- 0 ## when should seasonal forcing peak?
b <- 0 ## set births to be zero currently
d<- 0 ## set natural death rate to be zero currently--UNITS FOR THIS IN 1/DAYS to avoid 1/0

### find value for beta that makes R0 equal to a certain value for the whole US using numerical methods with next-gen matrix
find.beta<-function(betax,R0)
{
  V.minus<-matrix(0,dim*4,dim*4)
  for (i in 1:dim)
  {
    V.minus[i,i]<-dE
  }
  for (i in 1:dim)
  {
    V.minus[i+dim,i+dim]<-dP
  }
  for (i in 1:dim)
  {
    V.minus[i+2*dim,i+2*dim]<-dC
  }
  for (i in 1:dim)
  {
    V.minus[i+3*dim,i+3*dim]<-dA
  }
  
  V.plus<-matrix(0,dim*4,dim*4)
  for (i in 1:dim)
  {
    V.plus[i+dim,i]<-(1-rel.symp[i])*dE
  }
  for (i in 1:dim)
  {
    V.plus[i+2*dim,i+dim]<-dP
  }
  for (i in 1:dim)
  {
    V.plus[i+3*dim,i]<-rel.symp[i]*dE
  }
  
  Fmat<-matrix(0,dim*4,dim*4)
  for (i in 1:dim)
  {
    for(j in 1:dim)
    {
      Fmat[i,dim+j]<-betax
    }
  }
  
  for (i in 1:dim)
  {
    for(j in 1:dim)
    {
      Fmat[i,2*dim+j]<-betax*beta.mod.C
    }
  }
  
  for (i in 1:dim)
  {
    for(j in 1:dim)
    {
      Fmat[i,3*dim+j]<-betax*beta.mod.A
    }
  }
  
  Vmat<-V.minus-V.plus
  
  V.inv<-solve(Vmat)
  R0.calc<-max(Re(eigen(Fmat %*% V.inv)$val))
  R0-R0.calc
}

beta<-uniroot(find.beta,c(0,100),R0=R0.US)$root

### set initial conditions
ICs <- c(
  S = sub.demog -4, #susceptible
  E = sub.demog/sub.demog, #exposed/latent
  Ip = sub.demog/sub.demog, #infected, symptomatic, preclinical
  Ic = sub.demog/sub.demog, #infected, symptomatic, clinical
  Ia = sub.demog/sub.demog, #infected, asymptomatic
  Rs = sub.demog*0, #recovered from symptomatic infection, immune
  Ra = sub.demog*0 #recovered from asymptomatic infection, immune
)

ICs[which(ICs<0)]<-0 #fix for extremely small counties
ICs[which(is.nan(ICs))]<-0 #fix for extremely small counties

### model
SEIR.mod <- function(t,y,parms) {
  
  dim<-9
  ## susceptible individuals
  for(i in 1:dim)
  {assign(paste0("S",class.names[i]),unname(y[i]),envir = .GlobalEnv)}
  ## exposed individuals
  for(i in 1:dim)
  {assign(paste0("E",class.names[i]),unname(y[dim+i]),envir = .GlobalEnv)}
  ## Infected, presymptomatic
  for(i in 1:dim)
  {assign(paste0("Ip",class.names[i]),unname(y[2*dim+i]),envir = .GlobalEnv)}
  ## Infected, clinical
  for(i in 1:dim)
  {assign(paste0("Ic",class.names[i]),unname(y[3*dim+i]),envir = .GlobalEnv)}
  ## Infected, asymptomatic
  for(i in 1:dim)
  {assign(paste0("Ia",class.names[i]),unname(y[4*dim+i]),envir = .GlobalEnv)}
  ## recovered from symptomatic infection
  for(i in 1:dim)
  {assign(paste0("Rs",class.names[i]),unname(y[5*dim+i]),envir = .GlobalEnv)}
  ## recovered from asymptomatic infectiona
  for(i in 1:dim)
  {assign(paste0("Ra",class.names[i]),unname(y[6*dim+i]),envir = .GlobalEnv)}
  ## total pop
  N<-sum(y)
  
  contact.mat=params["contact.mat"][[1]]
  rel.symp=params["rel.symp"][[1]]
  beta=params["beta"][[1]]
  dE=params["dE"][[1]]
  dP=params["dP"][[1]]
  dC=params["dC"][[1]]
  dA=params["dA"][[1]]
  beta.mod.C=params["beta.mod.A"][[1]]
  beta.mod.C=params["beta.mod.C"][[1]]
  beta1=params["beta1"][[1]]
  b=params["b"][[1]]
  d=params["b"][[1]]
  phase=d["phase"][[1]]
  p.infected.fin=d["p.infected.fin"][[1]]

  #### S classes
  for(i in 1:dim)
  {
    foi<-NA
    for (j in 1:dim)
    {
      Ipj<-eval(as.symbol(paste0("Ip",class.names[j])))
      Icj<-eval(as.symbol(paste0("Ic",class.names[j])))
      Iaj<-eval(as.symbol(paste0("Ia",class.names[j])))
      Nj<-eval(as.symbol(paste0("S",class.names[j])))+eval(as.symbol(paste0("E",class.names[j])))+Ipj+Icj+Iaj+eval(as.symbol(paste0("Rs",class.names[j])))+eval(as.symbol(paste0("Ra",class.names[j])))
      foi<-sum(foi,beta*contact.mat[i,j]*(sum(Ipj+Icj*beta.mod.C+Iaj*beta.mod.A,na.rm = T)/Nj),na.rm=T)
    }
    Si<-eval(as.symbol(paste0("S",class.names[i])))
    if (i==1) {births<-b*N} else {births<-0}
    rate<-births - Si * (foi  + d)
    assign(paste0("dS",class.names[i]),rate,envir = .GlobalEnv)
  }
  
  ### E classes
  for(i in 1:dim)
  {
    foi<-NA
    for (j in 1:dim)
    {
      Ipj<-eval(as.symbol(paste0("Ip",class.names[j])))
      Icj<-eval(as.symbol(paste0("Ic",class.names[j])))
      Iaj<-eval(as.symbol(paste0("Ia",class.names[j])))
      Nj<-eval(as.symbol(paste0("S",class.names[j])))+eval(as.symbol(paste0("E",class.names[j])))+Ipj+Icj+Iaj+eval(as.symbol(paste0("Rs",class.names[j])))+eval(as.symbol(paste0("Ra",class.names[j])))
      foi<-sum(foi,beta*contact.mat[i,j]*(sum(Ipj+Icj*beta.mod.C+Iaj*beta.mod.A,na.rm = T)/Nj),na.rm=T)
    }
    Ei<-eval(as.symbol(paste0("E",class.names[i])))
    Si<-eval(as.symbol(paste0("S",class.names[i])))
    rate<- Si*foi - Ei * (1/dE + d)
    assign(paste0("dE",class.names[i]),rate,envir = .GlobalEnv)
  }
  
  ### Ip classes
  for(i in 1:dim)
  {
    Ei<-eval(as.symbol(paste0("E",class.names[i])))
    Ipi<-eval(as.symbol(paste0("Ip",class.names[i])))
    rate<- rel.symp[i]*(1/dE)*Ei - Ipi * (1/dP + d)
    assign(paste0("dIp",class.names[i]),rate,envir = .GlobalEnv)
  }
  
  ### Ic classes
  for(i in 1:dim)
  {
    Ipi<-eval(as.symbol(paste0("Ip",class.names[i])))
    Ici<-eval(as.symbol(paste0("Ic",class.names[i])))
    rate<- (1/dP)*Ipi - Ici * (1/dC + d)
    assign(paste0("dIc",class.names[i]),rate,envir = .GlobalEnv)
  }
  
  ### Ia classes
  for(i in 1:dim)
  {
    Ei<-eval(as.symbol(paste0("E",class.names[i])))
    Iai<-eval(as.symbol(paste0("Ia",class.names[i])))
    rate<- (1-rel.symp[i])*(1/dE)*Ei - Iai * (1/dA + d)
    assign(paste0("dIa",class.names[i]),rate,envir = .GlobalEnv)
  }
  
  ### Rs classes
  for(i in 1:dim)
  {
    Ici<-eval(as.symbol(paste0("Ic",class.names[i])))
    Rsi<-eval(as.symbol(paste0("Rs",class.names[i])))
    rate<- (1/dC)*Ici - Rsi *d
    assign(paste0("dRs",class.names[i]),rate,envir = .GlobalEnv)
  }
  
  ### Ra classes
  for(i in 1:dim)
  {
    Iai<-eval(as.symbol(paste0("Ia",class.names[i])))
    Rai<-eval(as.symbol(paste0("Ra",class.names[i])))
    rate<-(1/dA)*Iai - Rai *d
    assign(paste0("dRa",class.names[i]),rate,envir = .GlobalEnv)
  }
  
  #### define output
  output<-c()
  for(i in 1:dim)
  {
    output<-c(output,eval(as.symbol(paste0("dS",class.names[i]))))
  }
  for(i in 1:dim)
  {
    output<-c(output,eval(as.symbol(paste0("dE",class.names[i]))))
  }
  for(i in 1:dim)
  {
    output<-c(output,eval(as.symbol(paste0("dIp",class.names[i]))))
  }
  for(i in 1:dim)
  {
    output<-c(output,eval(as.symbol(paste0("dIc",class.names[i]))))
  }
  for(i in 1:dim)
  {
    output<-c(output,eval(as.symbol(paste0("dIa",class.names[i]))))
  }
  for(i in 1:dim)
  {
    output<-c(output,eval(as.symbol(paste0("dRs",class.names[i]))))
  }
  for(i in 1:dim)
  {
    output<-c(output,eval(as.symbol(paste0("dRa",class.names[i]))))
  }
  list(output)
  
}

rootfun <- function (t, y, parms) 
{ 
  p.infected.fin=params["p.infected.fin"][[1]]
  return(sum(y[19:63]) - sum(y)*p.infected.fin) 
}

params<-list("contact.mat"=contact.mat,"rel.symp"=rel.symp,"beta"=beta,"dE"=dE,"dP"=dP,"dC"=dC,"dA"=dA,"beta.mod.A"=beta.mod.A,"beta.mod.C"=beta.mod.C,"beta1"=beta1,"b"=b,"d"=d,"phase"=phase,"p.infected.fin"=p.infected.fin)
times<-seq(0,100,.25)
out<-ode(y=ICs,times=times,fun=SEIR.mod,parms=params,rootfun=rootfun)


totalS<-rowSums(out[,2:(dim+1)])
totalE<-rowSums(out[,(dim+2):(2*dim+1)])
totalIp<-rowSums(out[,(2*dim+2):(3*dim+1)])
totalIc<-rowSums(out[,(3*dim+2):(4*dim+1)])
totalIa<-rowSums(out[,(4*dim+2):(5*dim+1)])
totalRs<-rowSums(out[,(5*dim+2):(6*dim+1)])
totalRa<-rowSums(out[,(6*dim+2):(7*dim+1)])

end.time<-which.min(abs(p.infected.fin*sum(out[1,2:(7*dim+1)])-(totalIp+totalIc+totalIa+totalRs+totalRa)))

sim.output<-out[end.time,]
#end.S<-out[end.time,2:(dim+1)]
#end.E<-out[end.time,(dim+2):(2*dim+1)]
#end.Ip<-out[end.time,(2*dim+2):(3*dim+1)]
#end.Ic<-out[end.time,(3*dim+2):(4*dim+1)]
#end.Ia<-out[end.time,(4*dim+2):(5*dim+1)]
#end.R<-out[end.time,(5*dim+2):(6*dim+1)]




#### summary plots
#times<-out[,"time"]
#plot(times,totalS,type="l",ylab="N",ylim=c(0,60000))
#points(times,totalE,type="l",col="green")
#points(times,totalIp,type="l",col="red")
#points(times,totalIc,type="l",col="darkred")
#points(times,totalIa,type="l",col="pink")
#points(times,totalR,type="l",col="blue")
#abline(h=p.infected*sum(ICs),lty=2)
#abline(v=times[end.time],lty=2)
#legend("topleft",legend=c("S","E","Ip","Ic","Ia","R"),col=c("black","green","red","darkred","pink","blue"),lty=1)

#plot(end.S/sum(end.S+end.E+end.Ip+end.Ic+end.Ia+end.R),axes=F,xlab="age classes",ylab="% susceptible",ylim=c(0,1),type="l")
#points(sub.demog/sum(end.S+end.E+end.Ip+end.Ic+end.Ia+end.R))
#axis(1,at=1:9,labels=age.classes)
#axis(2)

#plot(sub.demog/sub.demog,axes=F,xlab="age classes",ylab="% susceptible",ylim=c(0,1),col="blue",type="b")
#points(end.S/sub.demog,col="red",type="b")
#axis(1,at=1:9,labels=age.classes)
#axis(2)
#legend("bottomright",legend=c("pre-epidemic","40% cumulative population infected"),col=c("blue","red"),pch=1)

#plot(0/sub.demog,axes=F,xlab="age classes",ylab="% infected",ylim=c(0,1),col="blue",type="b")
#points((sub.demog-end.S)/sub.demog,col="red",type="b")
#axis(1,at=1:9,labels=age.classes)
#axis(2)
#legend("topright",legend=c("pre-epidemic","40% cumulative population infected"),col=c("blue","red"),pch=1)


