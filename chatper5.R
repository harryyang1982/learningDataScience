sc <- function(x, y, verbose=T) {
  n <- length(x)
  if(n <=1 || n!=length(y)) stop("Arguments x and y have different lenghts: ", length(x), " and ", length(y), ".")
  
  if(TRUE%in%is.na(x)||TRUE%in%is.na(y)) stop(" Arguments x and y must not have missing values.")
  
  cv <- var(x,y)
  if(verbose) cat("Covariance = ", round(cv, 4), ".\n", sep= "")
  return(cv)
}

CalculateSampleVariance <- function(x, y, verbose=T) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  # x: One of two vectors whose sample covariance is to be calculated.
  # y: The other vector. x and y must have the same length, greater than one,
  #    with no missing values.
  # verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  # The sample covariance between x and y.
  n <- length(x)
  # Error handling
  if (n <= 1 || n != length(y)) {
    stop("Argument x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  if (TRUE %in% is.na(x) || TRUE %in% is.na(y)) {
    stop(" Arguments x and y must not have missing values.")
  }
  covariance <- var(x, y)
  if (verbose)
    cat("Covariance = ", round(covariance, 4), ".\n", sep = "")
  return(covariance)
}
