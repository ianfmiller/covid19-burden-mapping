rm(list=ls())

setwd("~/SEIR")
analysis.setup.lines<-readLines("analysis.R")

R0.US.levels<-c(2,4,6)
theta.levels<-c(0,.5,1)
beta.mod.C.levels<-c(.1,.5,1)

index<-1
for(i in 1:3) #add nested loops if needed for other models, subsets, etc.
{
  for (j in 1:3)
  {
    for (k in 1:3)
    {
      R0.US<-R0.US.levels[i]
      theta<-theta.levels[j]
      beta.mod.C<-beta.mod.C.levels[k]
      
      setwd("~/SEIR")
      
      analysis.setup.lines[min(grep('R0.US<-',analysis.setup.lines))]<-paste0("R0.US<-",R0.US)
      analysis.setup.lines[min(grep('theta<-',analysis.setup.lines))]<-paste0("theta<-",theta)
      analysis.setup.lines[min(grep('beta.mod.C<-',analysis.setup.lines))]<-paste0("beta.mod.C<-",beta.mod.C)
      
      job.name<-paste0("run.",index)
      if (!dir.exists(job.name)) {dir.create(job.name)}
      setwd(job.name)
      writeLines(analysis.setup.lines,con="analysis.R")
      
      file.copy("~/SEIR/analysis.prep.R",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/age.structured.model.R",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/geo.csv",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/us.demog.data.csv",paste0("~/SEIR/",job.name))
      file.copy("~/SEIR/US.hosp.bed.data.csv",paste0("~/SEIR/",job.name))
      index<-index+1
    }
  }
}

setwd("~/SEIR")

writeLines(
  c(
    "#!/bin/bash",
    "#",
    "#SBATCH -o SEIR.analysis.out",
    "#SBATCH -e SEIR.analysis.err",
    "#SBATCH -J SEIR", 
    "#SBATCH --array=1-27",
    "#SBATCH --cpus-per-task=10",
    "#SBATCH --mem-per-cpu=1G",
    "#SBATCH -t 0-06:00:00",
    "#SBATCH --mail-type=END",
    "#SBATCH --mail-user=ifmiller@princeton.edu",
    "cd ~/SEIR/run.$SLURM_ARRAY_TASK_ID",
    "srun R CMD BATCH analysis.R"
  ),
  con="SEIR.batch.q"
)

cmd<-paste0("sbatch SEIR.batch.q")
system(cmd)

