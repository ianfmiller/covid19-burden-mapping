source("analysis.prep.R")

### Simulate epi dynamics for each county. Sets parameters, runs SEIR model.

R0.US<-2.5 #R0 for entire US
theta<-1 #Mixing parameter. 1=polymod 0=uniform mixing
beta.mod.A<-.5 #Infectivity of asymptomatic cases relative to pre-clinical cases
beta.mod.C<-.5 #Effects of quarintine--infectivity of clinical cases relative to pre-clinical cases
analysis.name<-paste0("R0.",R0.US,".theta.",theta,".beta.mod.A.",beta.mod.A,".beta.mod.C.",beta.mod.C)

if(!file.exists(paste0(analysis.name,".SEIR.output.RDS")))
{  
  cl<-makeCluster(20)
  registerDoParallel(cl)
  foreach (z=1:nrow(demog.binned),.combine = rbind,.inorder = T,.packages = 'deSolve') %dopar% 
  {
    sub.demog<-demog.binned[z,]
    class.names<-c("0","10","20","30","40","50","60","70","80plus") 
    p.infected.fin<-.2
    R0.US<-R0.US
    theta<-theta
    beta.mod.A<-beta.mod.A
    beta.mod.C<-beta.mod.C
    source("age.structured.model.R",local=T)
    sim.output
    #sum(sim.output[20:55])/sum(sim.output[2:55]) check to make sure that the % of all Ip Ic Ia and R is p.infected.fin
  }->SEIR.out
  saveRDS(SEIR.out,file=paste0(analysis.name,".SEIR.output.RDS"))
  stopCluster(cl)
}
