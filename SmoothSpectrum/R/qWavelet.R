library("wavelets")
library("WaveletComp")

qAvgFreqPower = function(object, ...) {
  UseMethod("qAvgFreqPower", object)
}

qAvgTimePower = function(object, ...) {
  UseMethod("qAvgTimePower", object)
}

qPowerSpectrum = function(object, ...) {
  UseMethod("qPowerSpectrum", object)
}

qWaveComponent = function(object, ...) {
  UseMethod("qWaveComponent", object)
}

qWaveSmooth = function(object, ...) {
  UseMethod("qWaveSmooth", object)
}

################################################################################
#
#                               Untility Functions
#
################################################################################

qWindow = function(
  type = c("kaiser", "rect", "trgl", "welch", "gaussian", "sine", 
           "hann", "hamm",  "blackman", "nuttall", "harris", 
           "flattop", "tukey"),
  wsize = 7, ...)
{
  .WindowRect = function(wsize) {
    rep(1.0, wsize)
  }

  .WindowTrgl = function(wsize) {
    numer = (wsize - 1.0) / 2.0
    denom = (wsize + 1.0) / 2.0
    window = double(wsize)
    for (i in 1:wsize)
      window[i] = 1.0 - abs((i-1-numer)/denom)
    window
  }

  .WindowWelch = function(wsize) {
    numer = (wsize - 1.0) / 2.0
    denom = (wsize + 1.0) / 2.0
    window = double(wsize)
    for (i in 1:wsize)
      window[i] = 1.0 - ((i-1-numer)/denom)^2
    window
  }

  .WindowGaussian = function(wsize, sigma = 0.4) {
    factor = (wsize-1.0)/2.0
    window = double(wsize)
    for (i in 1:wsize) {
      window[i] = exp(-1.0 * ((i-1-factor)/(sigma*factor))^2 / 2.0)
    }
    window
  }

  .WindowKaiser = function(wsize, alpha = 3.0) {
    beta  = pi * alpha
    denom = besselI(beta, 0)
    window = double(wsize)
    for (i in 1:wsize) {
      x = beta * sqrt(1.0 - ((2.0*(i-1))/(wsize-1) - 1.0)^2)
      window[i] = besselI(x, 0) / denom
    }
    window
  }

  .WindowSine = function(wsize) {
    window = double(wsize)
    for (i in 1:wsize)
      window[i] = sin(pi * (i-1)/(wsize-1))
    window
  }

  .WindowHann = function(wsize) {
    window = double(wsize)
    for (i in 1:wsize)
      window[i] = (1.0 - cos(2.0 * pi * (i-1)/(wsize-1))) / 2.0
    window
  }

  .WindowHamm = function(wsize) {
    alpha = 0.53836
    beta  = 0.46164
    window = double(wsize)
    for (i in 1:wsize)
      window[i] = alpha - beta * cos(2.0 * pi* (i-1)/(wsize-1))
    window
  }

  .WindowBlackman = function(wsize) {
    A = c(0.42659, -0.49656, 0.076849)
    window = double(wsize)
    for (i in 1:wsize) {
      angle = pi * (i-1)/(wsize-1)
      window[i] = A[1] +
                  A[2] * cos(2.0 * angle) +
                  A[3] * cos(4.0 * angle)
    }
    window
  }

  .WindowNuttall = function(wsize) {
    A = c(0.355768, -0.487396, 0.144232, -0.012604)
    window = double(wsize)
    for (i in 1:wsize) {
      angle = pi * (i-1)/(wsize-1)
      window[i] = A[1] +
                  A[2] * cos(2.0 * angle) +
                  A[3] * cos(4.0 * angle) +
                  A[4] * cos(6.0 * angle)
    }
    window
  }

  .WindowHarris = function(wsize) {
    A = c(0.35875, -0.48829, 0.14128, -0.01168)
    window = double(wsize)
    for (i in 1:wsize) {
      angle = pi * (i-1)/(wsize-1)
      window[i] = A[1] +
                  A[2] * cos(2.0 * angle) +
                  A[3] * cos(4.0 * angle) +
                  A[4] * cos(6.0 * angle)
    }
    window
  }

  .WindowFlattop = function(wsize) {
    A = c(1.0, -1.93, 1.29, -0.388, 0.028)
    window = double(wsize)
    for (i in 1:wsize) {
      angle = pi * (i-1)/(wsize-1)
      window[i] = A[1] +
                  A[2] * cos(2.0 * angle) +
                  A[3] * cos(4.0 * angle) +
                  A[4] * cos(6.0 * angle) +
                  A[5] * cos(8.0 * angle)
    }
    window
  }

  .WindowTukey = function(wsize, alpha = 0.5) {
    if (wsize == 3) return(c(0.0, 1.0, 0.0))
    ilow = floor((wsize-1.0) * alpha/2.0)
    ihgh = floor((wsize-1.0) * (1.0-alpha/2.0))
    factor = 2.0/((wsize-1.0) * alpha)
    window = double(wsize)
    for(i in 1:ilow)
        window[i] = (1.0 + cos( pi * ((i-1.0)*factor - 1.0) )) / 2.0
    for(i in (ilow+1):ihgh)
        window[i] = 1.0
    for(i in (ihgh+1):wsize)
        window[i] = (1.0 + cos( pi*((i-1.0)*factor - 2.0/alpha + 1.0) )) / 2.0;
    window[1:wsize]
  }

  wsize = 2*(wsize %/% 2) + 1
  window = switch(
    toupper(substr(type[[1]], 1, 3)),
    "REC" = .WindowRect(wsize),
    "TRG" = .WindowTrgl(wsize),
    "WEL" = .WindowWelch(wsize),
    "GAU" = .WindowGaussian(wsize, ...),
    "KAI" = .WindowKaiser(wsize, ...),
    "SIN" = .WindowSine(wsize),
    "HAN" = .WindowHann(wsize),
    "HAM" = .WindowHamm(wsize),
    "BLA" = .WindowBlackman(wsize),
    "NUT" = .WindowNuttall(wsize),
    "HAR" = .WindowHarris(wsize),
    "FLA" = .WindowFlattop(wsize),
    "TUK" = .WindowTukey(wsize, ...),
    stop(sprintf("no window type in name \"%s\"", type))
  )
  return (window)
}

qSmooth = function(wave, window = qWindow("kaiser", 7)) 
{
  window = (window / sum(window))
  half.wsize = as.integer(length(window) %/% 2)

  wave = c(rev(head(wave, half.wsize)), wave, rev(tail(wave, half.wsize)))
  result = wave
  for (i in (half.wsize+1):(length(wave)-half.wsize) ) {
    sub = wave[(i-half.wsize):(i+half.wsize)]
    result[i] = crossprod(sub, window)
  }
  istart = half.wsize + 1
  istop  = length(wave) - half.wsize
  return(result[istart:istop])
}

################################################################################
#
#                            Fast Fourier Transform
#
################################################################################

qFFT = function(wave) {
  fft(wave, inverse = FALSE)
}

qInvFFT = function(freq) {
  fft(freq, inverse = TRUE) / length(freq)
}

qHilbert = function(wave) {
  .ConjFFT = function(freq) {
    n = length(freq)
    m = n %/% 2
    if (n %% 2 == 1)
      freq = freq * c(rep(-1i, m), 0.0, rep(1i, m))
    else
      freq = freq * c(rep(-1i, m), rep(1i, m))
    return(freq)
  }
  qInvFFT(.ConjFFT(qFFT(wave)))
}

################################################################################
#
#                       Short Time Fourier Transform
#
################################################################################

qSTFT = function(wave, window = qWindow("rect", length(wave)/32))
{
  wsize = length(window)
  hsize = wsize %/% 2
  wsize = hsize*2 + 1
  # etwave = c(wave, wave[length(wave)-(1:wsize)])
  etwave = c(wave[1:(hsize+1)], wave, wave[length(wave)-(1:hsize)])
  trans = sapply(1:length(wave), function(i) {
    stwave = etwave[i:(i+wsize)]
    stwave = stwave * window
    fft(stwave, inverse = FALSE)
  })

  return(structure(list(
    trans  = trans,
    window = window,
    wave   = wave,
    period = (0.5 * length(wave)) / ((hsize+1):1)
  ), class = "qSTFT"))
}

qInvSTFT = function(stft) 
{
  window = stft$window
  waves = apply(stft$trans, 2, function(freq) {
    fft(freq, inverse = TRUE) / length(freq)
  })

  wsize  = length(window)
  etwave = rep(0.0, length(stft$wave) + wsize)
  etsums = rep(0.0, length(stft$wave) + wsize)
  for (ic in 1:ncol(waves)) {
    etwave[ic:(ic+wsize)] = etwave[ic:(ic+wsize)] + as.numeric(waves[, ic])
    etsums[ic:(ic+wsize)] = etsums[ic:(ic+wsize)] + window
  }
  etwave = Re(etwave / etsums)
  hsize = wsize %/% 2
  wave  = etwave[(hsize + 2):(length(etwave) - hsize)]
  return(wave)
}

qAvgFreqPower.qSTFT = function(wt, ...)
{
  power    = qPowerSpectrum.qSTFT(stft, ...)
  avgPower = colMeans(power$values)
  return(structure(data.frame(
    power  = avgPower,
    period = power$period
  ), class = c("qFreqSpectrum", "data.frame")))
}

qAvgTimePower.qSTFT = function(wt, ...)
{
  power    = qPowerSpectrum.qSTFT(stft, ...)
  avgPower = rowMeans(power$values)
  return(structure(data.frame(
    power  = avgPower,
    time   = 1:length(avgPower)
  ), class = c("qTimeSpectrum", "data.frame")))
}

qPowerSpectrum.qSTFT = function(stft, ...) 
{
  trans = t(stft$trans)
  power = Mod(trans)^2

  hnc = ncol(power) %/% 2
  power[, 1:hnc] = 0.5*power[, 1:hnc] + 0.5*power[, ncol(power) + 1 - (1:hnc)]
  power = power[, 1:hnc]
  power = power[, ncol(power):1]

  return(structure(list(
    period = stft$period[1:ncol(power)],
    values = power
  ), class = "qTimeShiftSpectrum"))
}

qWaveComponent.qSTFT = function(stft, ...)
{
  hnc      = nrow(stft$trans) %/% 2
  componet = lapply(1:hnc, function(i) {
    sstft = stft
    for (ir in 1:nrow(sstft$trans)) {
      if (! ir %in% c(i, nrow(sstft$trans)-i))
        sstft$trans[ir, ] = 0.0
    }
    qInvSTFT(sstft)
  })
  componet = do.call(cbind, componet)
  
  return(structure(list(
    period = stft$period[1:ncol(componet)],
    values = componet
  ), class = "qTimeShiftSpectrum"))
}

qWaveSmooth.qSTFT = function(stft, ...)
{
  hnc    = nrow(stft$trans) %/% 2
  smooth = lapply(1:hnc, function(i) {
    sstft = stft
    sstft$trans[(hnc-i):(hnc+i), ] = 0.0
    qInvSTFT(sstft)
  })
  smooth = do.call(cbind, smooth)
    
  return(structure(list(
    period = stft$period[1:ncol(smooth)],
    values = smooth
  ), class = "qTimeShiftSpectrum"))
}

################################################################################
#
#                       Discrete Wavelet Transform
#
################################################################################

qDWT = function(wave, wavelet = "la8", nlevel = 8, boundary = "periodic") 
{
  trans = modwt(
    as.numeric(wave),
    filter = wt.filter(wavelet, modwt = TRUE),
    n.levels = nlevel,
    boundary = boundary
  )

  return(structure(list(
    trans  = trans,
    wave   = wave,
    nlevel = nlevel,
    period = 2^(1:nlevel)
  ), class = "qDWT"))
}

qInvDWT = function(wt) {
  imodwt(wt$trans)
}

qFilterDWT = function(wt, levels = c(3:wt$nlevel)) 
{
  trans = wt$trans
  for (i in 1:trans@level) {
    if (i %in% levels) next
    trans@W[[i]] = matrix(0.0, nrow = nrow(trans@W[[i]]), ncol = ncol(trans@W[[i]]))
    trans@V[[i]] = matrix(0.0, nrow = nrow(trans@V[[i]]), ncol = ncol(trans@V[[i]]))
  }
  imodwt(trans)
}

qAvgFreqPower.qDWT = function(wt, ...)
{
  power    = qPowerSpectrum.qDWT(wt, ...)
  avgPower = colMeans(power$values)
  return(structure(data.frame(
    power  = avgPower,
    period = power$period
  ), class = c("qFreqSpectrum", "data.frame")))
}

qAvgTimePower.qDWT = function(wt, ...)
{
  power    = qPowerSpectrum.qDWT(wt, ...)
  avgPower = rowMeans(power$values)
  return(structure(data.frame(
    power = avgPower,
    time  = 1:length(avgPower)
  ), class = c("qTimeSpectrum", "data.frame")))
}

qPowerSpectrum.qDWT = function(wt, ...) 
{
  zeros = matrix(0.0, nrow = nrow(wt$trans@W[[1]]), ncol = ncol(wt$trans@W[[1]]))
  power = sapply(1:wt$nlevel, function(ilevel) {
    swt = wt$trans
    for (is in 1:swt@level) {
      if (is == ilevel) next
      swt@W[[is]] = zeros
      swt@V[[is]] = zeros
    }
    rwave = imodwt(swt)
    hwave = Re(qHilbert(rwave))
    return(rwave^2 + hwave^2)
  })

  return(structure(list(
    period = wt$period[1:ncol(power)],
    values = power
  ), class = "qTimeShiftSpectrum"))
}

qWaveComponent.qDWT = function(wt, ...)
{
  zeros    = matrix(0.0, nrow = nrow(wt$trans@W[[1]]), ncol = ncol(wt$trans@W[[1]]))
  componet = sapply(1:wt$nlevel, function(ilevel) {
    swt = wt$trans
    for (is in 1:swt@level) {
      if (is == ilevel) next
      swt@W[[is]] = zeros
      swt@V[[is]] = zeros
    }
    return(imodwt(swt))
  })

  return(structure(list(
    period = wt$period[1:ncol(componet)],
    values = componet
  ), class = "qTimeShiftSpectrum"))
}

qWaveSmooth.qDWT = function(wt, ...)
{
  smooth = qWaveComponent.qDWT(wt, ...)$values
  for (i in (ncol(smooth)-1):1)
    smooth[, i] = smooth[, i] + smooth[, i+1]
  
  return(structure(list(
    period = wt$period[1:ncol(smooth)],
    values = smooth
  ), class = "qTimeShiftSpectrum"))
}

################################################################################
#
#                          Morlet Wavelet Transform
#
################################################################################

qMorlet = function(
  wave,
  df     = 0.1,
  period = c(2.0, floor(length(wave)/3)), ...)
{
  trans = WaveletComp::analyze.wavelet(
    my.data     = data.frame("wave" = wave),
    my.series   = "wave",
    loess.span  = 0.0,
    dt          = 1.0,
    dj          = df,
    lowerPeriod = min(period),
    upperPeriod = max(period),
    make.pval   = FALSE,
    verbose     = FALSE
  )

  return(structure(list(
    trans  = trans,
    wave   = wave,
    period = trans$Period
  ), class = "qMorlet"))
}

qConjMorlet = function(morlet)
{
  mWave = complex(real = Im(morlet$Wave), imag = Re(morlet$Wave))
  morlet$Wave = matrix(mWave, nrow = nrow(morlet$Wave), ncol = ncol(morlet$Wave))
  morlet$Phase = -1.0 * morlet$Phase
  return(morlet)
}

qInvMorlet = function(morlet) 
{
  parList = list(
    morlet$trans, 
    lvl        = 0.0,
    only.coi   = FALSE,
    only.sig   = FALSE,
    siglvl     = 0.0,
    only.ridge = FALSE,
    sel.period = NULL,
    sel.lower  = NULL,
    sel.upper  = NULL,
    plot.waves = FALSE, 
    plot.rec   = FALSE,
    verbose    = FALSE
  )
  series = do.call(WaveletComp::reconstruct, parList)$series
  return(series[[ncol(series)]])
}

qFilterMorlet = function(
  morlet,
  auto     = TRUE,
  byCone   = FALSE,
  byRidge  = FALSE,
  minPower = NA,
  period   = NULL,
  band     = NULL, ...)
{
  parList = list(
    morlet$trans, 
    lvl        = if (is.na(minPower)) 0.0 else minPower,
    only.coi   = byCone,
    only.sig   = FALSE,
    siglvl     = 0.0,
    only.ridge = byRidge,
    sel.period = period,
    sel.lower  = if (is.null(band)) NULL else min(band),
    sel.upper  = if (is.null(band)) NULL else max(band),
    plot.waves = FALSE, 
    plot.rec   = FALSE,
    verbose    = FALSE
  )
  if (auto) {
    pw = as.numeric(morlet$trans$Power)
    q  = quantile(pw, prob = seq(0.0, 1.0, by = 0.01))
    k  = ecdf(pw)(q) - q * max(q)
    parList$lvl = q[which.max(k)] / 10.0
  }
  series = do.call(WaveletComp::reconstruct, parList)$series
  return(series[[ncol(series)]])
}

qAvgFreqPower.qMorlet = function(morlet, ...)
{
  avgPower = morlet$trans$Power.avg
  return(structure(data.frame(
    power  = avgPower,
    period = morlet$period[1:length(avgPower)]
  ), class = c("qFreqSpectrum", "data.frame")))
}

qAvgTimePower.qMorlet = function(morlet, ...)
{
  power    = qPowerSpectrum.qMorlet(morlet, ...)
  avgPower = rowMeans(power$values)
  return(structure(data.frame(
    power = avgPower,
    time  = 1:length(avgPower)
  ), class = c("qTimeSpectrum", "data.frame")))
}

qPowerSpectrum.qMorlet = function(morlet)
{
  spectrum = t(morlet$trans$Power)

  return(structure(list(
    period = morlet$period[1:ncol(spectrum)],
    values = spectrum
  ), class = "qTimeShiftSpectrum"))
}

qWaveComponent.qMorlet = function(morlet)
{
  trans     = morlet$trans
  spectrum  = sapply(trans$Period, function(period) {
    qFilterMorlet(morlet, period = period)
  })

  return(structure(list(
    period = morlet$period[1:ncol(spectrum)],
    values = spectrum
  ), class = "qTimeShiftSpectrum"))
}

qWaveSmooth.qMorlet = function(morlet)
{
  trans     = morlet$trans
  maxPeriod = trans$Period[length(trans$Period)]
  spectrum  = sapply(trans$Period, function(minPeriod) {
    qFilterMorlet(morlet, band = c(minPeriod, maxPeriod))
  })

  return(structure(list(
    period = morlet$period[1:ncol(spectrum)],
    values = spectrum
  ), class = "qTimeShiftSpectrum"))
}

################################################################################
#
#                        Relative Visualization Methods
#
################################################################################

plot.qTimeShiftSpectrum = function(
  spectrum,
  colors    = rev(rainbow(256, start = 0.0, end = 0.7)),
  equalize  = TRUE,
  logPeriod = FALSE, ...)
{
  period   = spectrum$period
  spectrum = spectrum$values

  if (equalize)
    spectrum = matrix(rank(spectrum), nrow = nrow(spectrum), ncol = ncol(spectrum))

  if (logPeriod) 
  {
    image(
      t(spectrum), 
      x = 0:length(period), xlab = "period", xaxt = "n",
      y = 0:nrow(spectrum), ylab = "time",   xaxt = "n",
      col = colors
    )
    
    xperiod = 2^pretty(log2(period), n = min(c(5, ncol(spectrum))))
    vperiod = approx(x = period, y = 1:length(period), xout = xperiod)$y
    axis(side = 1, at = vperiod, labels = xperiod)
    for (v in vperiod)
      abline(v = vperiod, col = "#FFFFFF88", lwd = 1, lty = 2)

    htime = pretty(0:nrow(spectrum), n = 10)
    axis(side = 2, at = htime, labels = htime)
    for (h in htime)
      abline(h = h, col = "#FFFFFF88", lwd = 1, lty = 2)
  }
  else
  {
    image(
      t(spectrum), 
      x = c(0, period),     xlab = "period",
      y = 0:nrow(spectrum), ylab = "time",
      col = colors
    )

    vperiod = pretty(period, n = min(c(5, ncol(spectrum))))
    axis(side = 1, at = vperiod, labels = vperiod)
    for (v in vperiod)
      abline(v = vperiod, col = "#FFFFFF88", lwd = 1, lty = 2)

    htime = pretty(0:nrow(spectrum), n = 10)
    axis(side = 2, at = htime, labels = htime)
    for (h in htime)
      abline(h = h, col = "#FFFFFF88", lwd = 1, lty = 2)
  }
  box()
}


plot.qMorlet = function(
  morlet,
  colors   = rev(rainbow(256, start = 0.0, end = 0.66)),
  equalize = TRUE, ...)
{
  morlet = morlet$trans
  par(mfcol = c(1, 4))
  
  par(mai = c(0.5, 0.5, 0.5, 0.1))
  if (ncol(morlet$series) == 1) {
    wave = morlet$series[[1]]
    time = (1:nrow(morlet$series)) * morlet$dt
    plot(x = wave, y = time, type = "l", lwd = 2, col = "black", 
         ylab = "time", xlab = "", main = "raw", yaxs = "i")
  } else if (ncol(morlet$series) == 2) {
    wave  = morlet$series[[1]]
    trend = morlet$series[[2]]
    time  = (1:nrow(morlet$series)) * morlet$dt
    plot(x = wave, y = time, type = "l", lwd = 2, col = "black", 
         ylab = "time", xlab = "", main = "raw", yaxs = "i")
    lines(x = trend, y = time, lwd = 2, col = "orange")
  } else {
    wave  = morlet$series[[2]]
    trend = morlet$series[[3]]
    time  = (1:nrow(morlet$series)) * morlet$dt
    plot(x = wave, y = time, type = "l", lwd = 2, col = "black", 
         ylab = "time", xlab = "", main = "raw", yaxs = "i")
    lines(x = trend, y = time, lwd = 2, col = "orange")
  }
  abline(v = 0, col = "black", lwd = 1, lty = 2)
  htime = pretty(time, n = 10)
  for (h in htime)
    abline(h = h, col = "#00000088", lwd = 1, lty = 2)
  
  par(mai = c(0.5, 0.1, 0.5, 0.1))
  iperiod = 1:length(morlet$Period)
  tperiod = morlet$Period
  xperiod = 2^pretty(log2(tperiod), n = 10)
  vperiod = approx(x = tperiod, y = 1:length(tperiod), xout = xperiod)$y
  

  if (equalize)
    morlet$Power = matrix(rank(morlet$Power), nrow = morlet$nr, ncol = morlet$nc)
  image(Re(morlet$Power), x = iperiod, y = c(0, time), 
        ylab = "", xlab = "", main = "power",
        yaxt = "n", xaxt = "n", col = colors)
  axis(side = 1, at = vperiod, labels = xperiod)
  for (h in htime)
    abline(h = h, col = "#00000088", lwd = 1, lty = 2)
  box()

  image(Re(morlet$Ampl), x = iperiod, y = c(0, time), 
        ylab = "", xlab = "", main = "amplitude", 
        yaxt = "n", xaxt = "n", col = colors)
  axis(side = 1, at = vperiod, labels = xperiod)
  for (h in htime)
    abline(h = h, col = "#00000088", lwd = 1, lty = 2)
  box()

  image(Re(morlet$Phase), x = iperiod, y = c(0, time), 
        ylab = "", xlab = "", main = "phase", 
        yaxt = "n", xaxt = "n", col = colors)
  axis(side = 1, at = vperiod, labels = xperiod)
  for (h in htime)
    abline(h = h, col = "#00000088", lwd = 1, lty = 2)
  box()
}
