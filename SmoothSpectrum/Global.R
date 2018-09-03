library("rjson")
source("R/qWavelet.R")

CurveToSpectrum = function(curve)
{
  mDWT  = qDWT(curve, nlevel = 8)
  spectrum = qWaveSmooth(mDWT)$values
  spectrum = apply(spectrum, 2, round, digits = 2)
  spectrum
}

SingleWellSamples = function(spectrums, litho) 
{
  samples = lapply(9:(length(litho)-8), function(ipos) 
  {
    if (is.na(litho[ipos]))
      return(NULL)

    sample = lapply(spectrums, function(spectrum) {
      spectrum[(ipos-8):(ipos+8), ]
    })
    sample = c("LITHO" = litho[ipos], sample)
    return(sample)
  })
  mask = sapply(samples, is.null)
  samples[! mask]
}
