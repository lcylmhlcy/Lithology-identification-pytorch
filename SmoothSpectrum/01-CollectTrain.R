rm(list = ls())
source("Global.R")

lithoPath  = "data/litho-seg.csv"
curveDir   = "data/curve/select"
outputPath = "data/train.json"
selectCurveNames = c(
  "GR", "AC", "RMN", "RMG", "RLLS", "RLLD"
)

lithoTable = read.csv(lithoPath, as.is = TRUE)
lithoNames = c("SH", "SS", "SA", "GA")
lithoSample = lapply(unique(lithoTable$WELL), function(wellName) 
{
  cat(sprintf("collect %s ...\n", wellName)) 

  curves = read.csv(sprintf("%s/%s.csv", curveDir, wellName), header = TRUE)
  spects = lapply(curves[selectCurveNames], CurveToSpectrum)
  
  lithoColumn = rep(NA, nrow(curves))
  lithoTable  = subset(lithoTable, WELL == wellName)
  for (lithoName in lithoNames) 
  {
    subLithoTable = subset(lithoTable, LITHO == lithoName)
    if (nrow(subLithoTable) <= 0) next

    for (ir in 1:nrow(subLithoTable)) {
      startDepth = as.numeric(subLithoTable$START[ir])
      stopDepth  = as.numeric(subLithoTable$STOP[ir])
      if (lithoName == "SS" && abs(stopDepth - startDepth) >= 3.0) {
        startDepth = startDepth + 1.0
        stopDepth  = stopDepth - 1.0
      }
      if (abs(stopDepth - startDepth) >= 1.0) {
        startDepth = startDepth + 0.25
        stopDepth  = stopDepth - 0.25
      }
      mask = with(curves, startDepth < DEPTH & DEPTH < stopDepth)
      lithoColumn[mask] = lithoName
    }
  }
  samples = SingleWellSamples(spects, lithoColumn)
  return(samples)
})
lithoSample = do.call(c, lithoSample)

outputFile <- file(outputPath, "w")
cat(file = outputFile, "[\n")
for (i in 1:length(lithoSample)) {
  text = toJSON(lithoSample[[i]])
  writeLines(text, con = outputFile, sep = ",\n")
}
cat(file = outputFile, "]\n\n")
close(outputFile)
