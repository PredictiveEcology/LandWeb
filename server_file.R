# THIS IS THE MAIN "SIMULATION FUNCTION"
# THE FOLLOWING OBJECT IS A LIST OF 1 simList,
# A simList is a rich data structure that comes with the SpaDES.core package
mySimOut <- Cache(runExperiment, mySim, experimentReps,
                   debugCache = "complete",
                   objects = objectsToHash)#,
                   #sideEffect = TRUE)

message("  Finished Experiment")
