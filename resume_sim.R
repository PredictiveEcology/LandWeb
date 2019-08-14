restartIteration <- restartIteration + 1
end(mySimOut) <- min(restartIteration * restartInterval, endTime)
mySimOut <- spades(mySimOut)
simFile <- file.path(Paths$outputPath, 'mySimOut.rds')
saveRDS(mySimOut, simFile)
message("Saving simulation to: ", simFile)

if (end(mySimOut) < endTime) {
  ## restarts R but keep attached packages and .GlobalEnv intact
  rstudioapi::restartSession("source('resume_sim.R')")
}
