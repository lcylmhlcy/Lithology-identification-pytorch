rm(list = ls())
source("Global.R")

curveDir = "data/curve/select"
selectCurveNames = c(
  "GR", "AC", "RMN", "RMG", "RLLS", "RLLD"
)
wellNames = c(
  "ню2-231-221╪с", "ню2-24-25", "ню2-26-27", "ню2-27-27",
  "ню2-28-28", "ню2-29-28", "ню2-30-23", "ню2-32-38",
  "ню2-33-20", "ню2-33-28", "ню2-35-27", "ню2-36-26",
  "ню2-36-27", "ню2-37-24", "ню2-38-29", "ню2-40-19",
  "ню2-8-17",  "ню211", "ню212", "ню213", "ню25",
  "ню262", "л╚108", "л╚121", "л╚23"
)

for (wellName in wellNames)
{
  cat(sprintf("process %s ...\n", wellName)) 
  curves = read.csv(sprintf("%s/%s.csv", curveDir, wellName), header = TRUE)
  outputDir = sprintf("data/test/%s", wellName)
  if (! file.exists(outputDir))
    dir.create(outputDir)

  depths = curves$DEPTH
  for (curveName in selectCurveNames) {
    cat(sprintf("  process %s ...\n", curveName))
    spects = CurveToSpectrum(curves[[curveName]])
    spects = cbind("DEPTH" = depths, spects)
    colnames(spects) = c("DEPTH", sprintf("%s%d", curveName, 1:(ncol(spects)-1)))
    write.csv(spects, sprintf("%s/%s.csv", outputDir, curveName), row.names = FALSE)
  }
}