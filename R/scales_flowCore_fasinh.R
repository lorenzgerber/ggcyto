#' Add a flowCore inverse hyperbolic sine scale to the x or y axes of a ggcyto plot.
#'
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @param a,b,c see 'help(arcsinhTransform')
#' @return ScaleContinuous object
#' @examples
#' data(GvHD)
#' fr <- GvHD[[1]]
#' p <- ggcyto(fr, aes(x = `FL1-H`)) + geom_density()
#' #display at raw scale
#' p
#' #display at transformed scale
#' p + scale_x_flowCore_fasinh(a = 2)
#' @export
scale_x_flowCore_fasinh <- function(..., a=1, b=1, c=0){
  myTrans <- flowCore_asinht_trans(a = a, b = b, c = c)
  scale_x_continuous(..., trans = myTrans)

}

#' @rdname scale_x_flowCore_fasinh
#' @export
scale_y_flowCore_fasinh <- function(..., a=1, b=1, c=0){
  myTrans <- flowCore_asinht_trans(a = a, b = b, c = c)
  scale_y_continuous(..., trans = myTrans)

}

## Hyperbolic sin transformation constructor
sinhTransform <- function(transformationId="defaultsinhTransform",
                             a=1, b=1, c=0)
{
  t <- new("transform", .Data=function(x) (sinh(x-c)-a)/b)
  t@transformationId <- transformationId
  t
}


#' Inverse hyperbolic sine transformation(flowCore version).
#'
#' Used to construct inverse hyperbolic sine transform object.
#'
#' @param n desired number of breaks (the actual number will be different depending on the data range)
#' @param equal.space whether breaks at equal-spaced intervals
#' @param ... parameters passed to arcsinhTransform
#' @return asinht transformation object
#' @examples
#' trans.obj <- flowCore_asinht_trans(equal.space = TRUE)
#' data <- 1:1e3
#' brks.func <- trans.obj[["breaks"]]
#' brks <- brks.func(data)
#' brks # fasinh space displayed at raw data scale
#'
#' #transform it to verify it is equal-spaced at transformed scale
#' trans.func <- trans.obj[["transform"]]
#' brks.trans <- trans.func(brks)
#' brks.trans
#' @export
flowCore_asinht_trans <- function(..., n = 6, equal.space = FALSE){
  trans <- flowCore::arcsinhTransform(...)
  inv <- sinhTransform(...)
  flow_trans(name = "asinht", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)
}

#' generate a trans objects
#' Used by other specific trans constructor
#' @param name transformation name
flow_trans <- function(name, trans.fun, inverse.fun, equal.space = FALSE, n = 6){

  brk <- function(x){
    flow_breaks(x, n = n, equal.space = equal.space, trans.fun, inverse.fun)
  }

  if(equal.space){
    fmt <- format_format(digits = 0)
  }else{
    fmt <- function(x){
      pretty10exp(as.numeric(x),drop.1=TRUE)
    }

  }

  scales::trans_new(name, transform = trans.fun, inverse = inverse.fun, breaks = brk, format = fmt)
}

pretty10exp <-function (x, drop.1 = FALSE, digits.fuzz = 7)
{
  eT <- floor(log10(abs(x)) + 10^-digits.fuzz)
  mT <- signif(x/10^eT, digits.fuzz)
  ss <- vector("list", length(x))
  for (i in seq(along = x)) ss[[i]] <- if (is.na(x[i]))
    quote(NA)
  else if (x[i] == 0)
    quote(0)
  else if (drop.1 && mT[i] == 1)
    substitute(10^E, list(E = eT[i]))
  else if (drop.1 && mT[i] == -1)
    substitute(-10^E, list(E = eT[i]))
  else substitute(A %*% 10^E, list(A = mT[i], E = eT[i]))

  do.call("expression", ss)
}

#' flow_breaks
#' breaks for flow
#' @param x data
#' @param n number of breaks
#' @param equal.space should it be equal sapced
#' @param trans.fun actual transform function
#' @param inverse.fun inverse transform function
#' @export
flow_breaks <- function(x, n = 6, equal.space = FALSE, trans.fun, inverse.fun){
  rng.raw <- range(x, na.rm = TRUE)
  if(equal.space){

    rng <- trans.fun(rng.raw)
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min)
      return(inverse.fun(min))
    by <- (max - min)/(n-1)

    myBreaks <- inverse.fun(seq(min, max, by = by))

  }else{
    #log10 (e.g. 0, 10, 1000, ...)
    base10raw <- unlist(lapply(2:n,function(e)10^e))
    base10raw <- c(0,base10raw)
    myBreaks <- base10raw[base10raw>rng.raw[1]&base10raw<rng.raw[2]]

  }
  myBreaks
}
