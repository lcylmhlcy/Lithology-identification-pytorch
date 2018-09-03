library("compiler", quietly = TRUE)

################################################################################
#
#
#
################################################################################

qColor = function(...)
{
  arqCount = length(match.call(expand.dots = FALSE)$`...`)
  if (arqCount == 3) {
    # R G B 
    r = ..1; r[r < 0] = 0; r[r > 255] = 255
    g = ..2; g[g < 0] = 0; g[g > 255] = 255
    b = ..3; b[b < 0] = 0; b[b > 255] = 255
    return(rgb(r, g, b, maxColorValue = 255))
  }

  # color given by string
  sapply(..1, function(name) {
    firstChar = substr(name, 1, 1)
    if (firstChar == "#" && nchar(name) == 7) {
      # "#FFFFFF"
      return(name)
    } else {
      # "red", "blue"
      mRGB = col2rgb(name)
      name = rgb(mRGB[1, 1], mRGB[2, 1], mRGB[3, 1], maxColorValue = 255)
      return(name)
    }
  })
}
qColor = compiler::cmpfun(qColor)

################################################################################
#
#
#
################################################################################

.ColorGet = function(color, from, to)  {
  result = sapply(color, function(color) {
    strtoi(substr(color, from, to), base=16)
  })
  names(result) = NULL
  result
}
.ColorGet = compiler::cmpfun(.ColorGet)

qRed   = function(color) { .ColorGet(color, 2, 3) }
qGreen = function(color) { .ColorGet(color, 4, 5) }
qBlue  = function(color) { .ColorGet(color, 6, 7) }

.ColorSet = function(color, values, from, to)
{
  values = as.integer(values)
  values[values > 255] = 255
  values[values < 0]   = 0
  substr(color, from, to) = sapply(values, function(value) {
    if(value < 15)
      sprintf("0%X", value)
    else
      sprintf("%X", value)
  })
  color
}
.ColorSet = compiler::cmpfun(.ColorSet)

`qRed<-`   = function(color, value) { .ColorSet(color, value, 2, 3) }
`qGreen<-` = function(color, value) { .ColorSet(color, value, 4, 5) }
`qBlue<-`  = function(color, value) { .ColorSet(color, value, 6, 7) }

################################################################################
#
#
#
################################################################################

qLighter = function(color, ratio = 1.5)
{
  mhsv = rgb2hsv(col2rgb(color))
  apply(mhsv, 2, function(column) {
    s = column[2]
    v = column[3] * ratio
    if (v > 1.0) {
      s = (s - (v - 1.0))
      s = max(c(s, 0.0))
      v = 1.0
    }
    hsv(column[1], s, v)
  })
}
qLighter = compiler::cmpfun(qLighter)

################################################################################
#
#
#
################################################################################

qScheme = function(
  name   = "flower",
  colors = NULL,
  scales = seq(0.0, 1.0, length.out = length(colors)) )
{
  .CreateSchemeFn = function(colors, scales)
  {
    colors = qColor(colors)
    rFn = approxfun(scales, qRed(colors),   rule = 2)
    qFn = approxfun(scales, qGreen(colors), rule = 2)
    bFn = approxfun(scales, qBlue(colors),  rule = 2)
    .SchemeFn = function(x, min = 0.0, max = 1.0)
    {
      x[x < min] = min
      x[x > max] = max
      x = (x-min)/(max-min)
      sapply(x, function(v) {
        qColor(round(rFn(v)), round(qFn(v)), round(bFn(v)))
      })
    }
    return(.SchemeFn)
  }

  if (! is.null(colors)) {
    stopifnot(length(colors) == length(scales))
    .CreateSchemeFn(colors, scales)
  } else if (name[[1]] == "flower") {
    .CreateSchemeFn(c("#0000FF", "#00C0FF", "#00C000", "#FFC000", "#FF0000", "#DD00DD"),
                c(0.0, 0.20, 0.4, 0.6, 0.8, 1.0) )
  } else if (name[[1]] == "dark") {
    .CreateSchemeFn(c("#000080", "#006080", "#006000", "#806000", "#800000", "#800080"),
                c(0.0, 0.20, 0.4, 0.6, 0.8, 1.0) )
  } else if (name[[1]] == "water") {
    .CreateSchemeFn(c("#CBF3FB", "#70E1FF", "#049FF1", "#060FB9", "#040444"),
                c(0.0, 0.25, 0.4, 0.7, 1.0) )
  } else if (name[[1]] == "sky") {
    .CreateSchemeFn(c("#FFFFFF", "#F4F8DA", "#DCEDC8", "#42B3D5", "#1A237E", "#170527"),
                    c(0.0, 0.05, 0.25, 0.5, 0.75, 1.0) )
  } else if (name[[1]] == "sunset") {
    .CreateSchemeFn(c("#FFFFFF", "#FFFFDA", "#FFECB3", "#E85285", "#6A1B9A", "#000044"),
                    c(0.0, 0.05, 0.20, 0.45, 0.65, 1.0) )
  } else if (name[[1]] == "warm") {
    .CreateSchemeFn(c("#FFFFFF", "#FFFFD5", "#FEEB65", "#E4521B", "#4D342F", "#191515"),
                    c(0.0, 0.05, 0.30, 0.65, 0.85, 1.0) )
  } else if (name[[1]] == "spring") {
    .CreateSchemeFn(c("#95D9DF", "#DDDD88", "#C5DA01", "#88B700", "#008800"),
                c(0.0, 0.25, 0.5, 0.7, 1.0))
  } else if (name[[1]] == "summer") {
    .CreateSchemeFn(c("#04693C", "#3CB64C", "#DCE234", "#FCB245", "#E12066", "#642E94"),
                seq(from=0.0, to=1.0, length.out=6) )
  } else if (name[[1]] == "autumn") {
    .CreateSchemeFn(c("#75AF48", "#9CBF41", "#C4CA34", "#E6DE35", "#E7B329", "#E38724",
                  "#DA5F24", "#D1242A", "#A62328", "#7D1A1D"),
                seq(from=0.0, to=1.0, length.out=10) )
  } else if (name[[1]] == "winter") {
    .CreateSchemeFn(c("#AAAAAA", "#EDF3F3", "#72CFD7", "#0091A9", "#002244"),
                seq(from=0.0, to=1.0, length.out=5) )
  } else if (name[[1]] == "blood") {
    .CreateSchemeFn(c("#F3ECCF", "#FAD6A2", "#F49F80", "#E85C41", "#C02626", "#902221", "#440000"),
                seq(from=0.0, to=1.0, length.out=7) )
  } else if (name[[1]] == "rainbow") {
    .CreateSchemeFn(rainbow(33), (0:32)/32)
  } else if (name[[1]] == "terrain") {
    .CreateSchemeFn(terrain.colors(33), (0:32)/32)
  } else if (name[[1]] == "heat") {
    .CreateSchemeFn(heat.colors(33), (0:32)/32)
  } else if (name[[1]] == "topo") {
    .CreateSchemeFn(topo.colors(33), (0:32)/32)
  } else if (name[[1]] == "cm") {
    .CreateSchemeFn(cm.colors(33), (0:32)/32)
  } else if (name[[1]] == "gray") {
    .CreateSchemeFn(rev(gray((0:10)/10)), (0:10)/10)
  } else {
    stop(sprintf("no color scheme in name \"%s\"", name))
  }
}
