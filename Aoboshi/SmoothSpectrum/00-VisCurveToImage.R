source("R/qColor.R")
source("R/qWavelet.R")

nlevel = 8
curves = read.csv("data/curve/select/ню2-8-17.csv")
curve = curves$GR
depth = curves$DEPTH
mDWT  = qDWT(curve, nlevel = nlevel)
spect = qWaveSmooth(mDWT)

expDepth = 1237.0
ipos = which(depth == expDepth)[1]

# scheme = qScheme(
#   scale = c(0.0, 0.5, 1.0),
#   color = c("#FFFFFF", "#FFCC00", "#222200")
# )
# img = t(spect$values[(ipos-8):(ipos+8), ])
# img[,] = rank(img[,])
# par(mar = c(0.1, 0.1, 0.1, 0.1))
# image(
#   z = img, xaxt = "n", yaxt = "n",
#   col = scheme(0:255, 0, 255)
# )
# box()

par(mfcol = c(1, 3), mai = c(0.6, 0.6, 0.75, 0.0))
plot(
  x = curve, y = depth, 
  xlab = "GR (API)",  xlim = c(40, 160),
  ylab = "Depth (M)", ylim = rev(range(depth)), yaxs = "i", 
  main = "Raw Curve",
  lwd = 1, type = "l", col = "blue"
)
# points(x = curve[ipos], y = depth[ipos], col = "black", pch = 4, cex = 2.0)
abline(h = expDepth + 1.0, lty = 2)
abline(h = expDepth - 1.0, lty = 2)

plot(
  x = curve, y = depth, 
  xlab = "GR (API)",  xlim = c(40, 160),
  ylab = "Depth (M)", ylim = rev(range(depth)), yaxs = "i", 
  main = "Wavelet Smoothed Curves",
  lwd = 1, type = "n", col = "red"
)
colors = qScheme("flower")(1:nlevel, 1, nlevel)
for (ic in 1:nlevel)
  lines(x = spect$values[, ic], y = depth, lwd = 1, col = colors[[ic]])
# points(x = curve[ipos], y = depth[ipos], col = "black", pch = 4, cex = 2.0)
abline(h = expDepth + 1.0, lty = 2)
abline(h = expDepth - 1.0, lty = 2)

img = t(spect$values)
image(
  z = img, x = 1:nlevel, y = depth,
  ylab = "Depth (M)", ylim = rev(range(depth)), yaxs = "i", 
  xlab = "Level",
  main = "Gary Image",
  zlim = c(40, 160),
  col = qScheme("gray")(0:255, 0, 255)
)
abline(h = expDepth + 1.0, lty = 2, col = "white")
abline(h = expDepth - 1.0, lty = 2, col = "white")
box()
