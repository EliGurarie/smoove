library(smoove)

# load data
data(simSweep, package="smoove")

# perform analysis
simCP.table <- simSweep |> 
  findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) |> 
  getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")

# obtain phases
simPhaseList <- estimatePhases(simCP.table)

#plot phases
plotPhaseList(simPhaseList)
