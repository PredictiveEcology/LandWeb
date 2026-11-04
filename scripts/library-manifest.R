## The R library this host runs LandWeb with: package, version, GitHub SHA and build date, one row per
## package. CRAN packages are in no stage fingerprint, so two runs are comparable only if their manifests
## agree. Run from the project root:
##   Rscript-4.6.1 scripts/library-manifest.R <out csv>
out <- commandArgs(TRUE)[1]
ip <- as.data.frame(installed.packages(fields = "RemoteSha")[, c(
  "Package",
  "Version",
  "RemoteSha",
  "Built"
)])
ip <- ip[order(ip$Package), ]
ip <- ip[!duplicated(ip$Package), ]
## the host, so a manifest written elsewhere does not pass as the run host's
ip$Host <- Sys.info()[["nodename"]]
data.table::fwrite(ip, out)
cat("library manifest:", nrow(ip), "packages ->", out, "\n")
