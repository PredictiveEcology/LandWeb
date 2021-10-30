## sample code for state-transition model.
## Dave would need to provide the transition table.

if (!require("Require")) {install.packages("Require"); require("Require")}
Require(c("data.table", "raster", "quickPlot"))
transitionProbs <- data.frame(
  from = c(1, 1, 2, 2),
  to = c(1, 2, 1, 2),
  prob = c(0.9, 0.1, 0.05, 0.95)
) ## TODO: add age and region cols

r <- raster(vals = sample(1:2, replace = TRUE, size = 100), extent(0, 10, 0, 10), res = 1)
rasts <- list()
rasts[[1]] <- r
for (i in 2:10) {
  dt <- data.table(init = rasts[[i - 1]][], pixel = seq(ncell(r)))
  rtmp <- dt[transitionProbs, on = c("init" = "from"), allow.cartesian = TRUE]
  setkeyv(rtmp, c("pixel", "init", "to"))
  rtmp <- rtmp[, list(from = init[1], to = sample(1:2, prob = prob, size = 1)), by = "pixel"]
  rasts[[i]] <- raster(r)
  rasts[[i]][] <- rtmp$to
}
stk <- stack(rasts)
clearPlot()
Plot(stk)
