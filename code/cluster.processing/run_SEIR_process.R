rm(list=ls())

setwd("~/SEIR")
analysis.setup.lines<-readLines("SEIR.outuput.processing.cluster.R")

R0.US.levels<-c(2,3,5)
R0.min<-2
R0.max.levels<-c(3,5)
theta.levels<-c(0,.5,1)
beta.mod.C.levels<-c(.1,.5,1)

index<-1
for(i in 1:5) #add nested loops if needed for other models, subsets, etc.
{
  for (j in 1:3)
  {
    for (k in 1:3)
    {
      
      if (i %in% c(1,2,3))
      {
        R0.adj<-F
        R0.US<-R0.US.levels[i]
        R0.max<-0 #dummy value
      }
      
      if(i %in% c(4,5))
      {
        R0.adj<-T
        R0.max<-R0.max.levels[i-3]
        R0.US<-0 #dummy value
      }
      
      theta<-theta.levels[j]
      beta.mod.C<-beta.mod.C.levels[k]
      
      setwd("~/SEIR")
      
      analysis.setup.lines[min(grep('R0.adj<-',analysis.setup.lines))]<-paste0("R0.adj<-",R0.adj)
      analysis.setup.lines[min(grep('R0.max<-',analysis.setup.lines))]<-paste0("R0.max<-",R0.max)
      analysis.setup.lines[min(grep('R0.min<-',analysis.setup.lines))]<-paste0("R0.min<-",R0.min)
      analysis.setup.lines[min(grep('R0.US<-',analysis.setup.lines))]<-paste0("R0.US<-",R0.US) #doesn't matter if R0.adj=T
      analysis.setup.lines[min(grep('theta<-',analysis.setup.lines))]<-paste0("theta<-",theta)
      analysis.setup.lines[min(grep('beta.mod.C<-',analysis.setup.lines))]<-paste0("beta.mod.C<-",beta.mod.C)
      
      job.name<-paste0("run.",index)
      if (!dir.exists(job.name)) {dir.create(job.name)}
      setwd(job.name)
      writeLines(analysis.setup.lines,con="SEIR.outuput.processing.R")
      file.copy("~/SEIR/US.hosp.bed.data.csv",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/geo.csv",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/US.pop.urban.rural.csv",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/analysis.prep.R",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/us.demog.data.csv",paste0("~/SEIR/",job.name))
      index<-index+1
    }
  }
}

setwd("~/SEIR")

writeLines(
  c(
    "#!/bin/bash",
    "#",
    "#SBATCH -o SEIR.process.out",
    "#SBATCH -e SEIR.process.err",
    "#SBATCH -J SEIR", 
    "#SBATCH --array=1-45",
    "#SBATCH --cpus-per-task=1",
    "#SBATCH --mem-per-cpu=1G",
    "#SBATCH -t 0-02:00:00",
    "#SBATCH --mail-type=END",
    "#SBATCH --mail-user=ifmiller@princeton.edu",
    "cd ~/SEIR/run.$SLURM_ARRAY_TASK_ID",
    "srun R CMD BATCH SEIR.outuput.processing.R"
  ),
  con="SEIR.process.batch.q"
)

cmd<-paste0("sbatch SEIR.process.batch.q")
system(cmd)

