library(DMwR)
data(Glass, package = "mlbench")
ac <- rpartXse(Type ~ ., Glass)
prettyTree(ac)

# as factored variable, doesn't work
res2 <- performanceEstimation(
  PredTask(Type ~ ., Glass),
  workflowVariants("standardWF", learner = "rpartXse"),
  BootSettings(type=".632", nReps=200))

#as factored variable, works
Glass.nf <- Glass
Glass.nf$Type <- as.numeric(Glass.nf$Type)
res2 <- performanceEstimation(
  PredTask(Type ~ ., Glass.nf),
  workflowVariants("standardWF", learner = "rpartXse"),
  BootSettings(type=".632", nReps=200))