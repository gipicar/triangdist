#' Density function for the triangular distribution
#' @param x Vector of quantiles
#' @param min Lower limit of the distribution (a)
#' @param max Upper limit of the distribution (b)
#' @param mode Mode of the distribution (c)
#' @return A numeric vector of densities
#' @export
dtriang <- function(x, min, max, mode) {
  if (any(min >= max)) stop("min must be strictly less than max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")

  res <- numeric(length(x))

  idx1 <- x >= min & x < mode
  idx2 <- x >= mode & x <= max

  res[idx1] <- 2 * (x[idx1] - min) / ((max - min) * (mode - min))
  res[idx2] <- 2 * (max - x[idx2]) / ((max - min) * (max - mode))

  is_mode_min <- mode == min
  res[is_mode_min & x == mode] <- 2 / (max[is_mode_min] - min[is_mode_min])

  return(res)
}

#' Distribution function for the triangular distribution
#' @param q Vector of quantiles
#' @param min Lower limit of the distribution
#' @param max Upper limit of the distribution
#' @param mode Mode of the distribution
#' @return A numeric vector of cumulative probabilities
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min >= max)) stop("min must be strictly less than max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")

  p <- numeric(length(q))
  idx1 <- q >= min & q <= mode
  idx2 <- q > mode & q <= max
  idx3 <- q > max

  p[idx1] <- (q[idx1] - min) ^ 2 / ((max - min) * (mode - min))
  p[idx2] <- 1 - (max - q[idx2]) ^ 2 / ((max - min) * (max - mode))
  p[idx3] <- 1

  p[mode == min & q >= mode] <- 1 - (max[mode == min & q >= mode] - q[mode == min & q >= mode])^2 /
    ((max[mode == min & q >= mode] - min[mode == min & q >= mode])^2)

  return(p)
}

#' Quantile function for the triangular distribution
#' @param p Vector of probabilities
#' @param min Lower limit of the distribution
#' @param max Upper limit of the distribution
#' @param mode Mode of the distribution
#' @return A numeric vector of quantiles
#' @export
qtriang <- function(p, min, max, mode) {
  if (any(min >= max)) stop("min must be strictly less than max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")
  if (any(p < 0 | p > 1)) stop("p must be between 0 and 1") # Validación Fuente [2]

  q <- numeric(length(p))
  p_mode <- (mode - min) / (max - min)

  idx1 <- p <= p_mode
  idx2 <- p > p_mode

  # Inversa de la CDF (Fuente [5])
  q[idx1] <- min + sqrt(p[idx1] * (max - min) * (mode - min))
  q[idx2] <- max - sqrt((1 - p[idx2]) * (max - min) * (max - mode))

  return(q)
}

#' Random generation for the triangular distribution
#'
#' @param n Number of observations
#' @param min Lower limit of the distribution
#' @param max Upper limit of the distribution
#' @param mode Mode of the distribution
#' @return A numeric vector of random variates
#' @export
#' @importFrom stats runif
rtriang <- function(n, min, max, mode) {
  if (any(min >= max)) stop("min must be strictly less than max")
  if (any(mode < min | mode > max)) stop("mode must be within [min, max]")

  # Implementación mediante el Método de Inversión
  p <- runif(n)
  return(qtriang(p, min, max, mode))
}
