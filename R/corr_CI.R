#' Correlation Coefficient with Confidence Interval
#'
#' Computes the correlation coefficient and a confidence interval for two vectors.
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector.
#' @return A character string with the correlation coefficient and confidence interval.
#' @examples
#' vec1 <- c(1, 2, 3, 4, 5)
#' vec2 <- c(2, 4, 6, 8, 10)
#' corr.CI(vec1, vec2)
#' @export
corr.CI <- function(vec1, vec2) {
  sum.product = sum((vec1 - mean(vec1)) * (vec2 - mean(vec2)))
  print(paste("Sum of products:", round(sum.product, 3)))

  var1 = sum((vec1 - mean(vec1))^2)
  print(paste("var1 =", round(var1, 3)))

  var2 = sum((vec2 - mean(vec2))^2)
  print(paste("var2 =", round(var2, 3)))

  r = sum.product / (sqrt(var1) * sqrt(var2))
  print(paste("r =", round(r, 3)))

  z = 0.5 * log((1 + r) / (1 - r))
  print(paste("z =", round(z, 3)))

  sd = sqrt(1 / (length(vec1) - 3))
  print(paste("sd =", round(sd, 3)))

  z1 = z - 1.96 * sd
  print(paste("z1 =", round(z1, 3)))

  z2 = z + 1.96 * sd
  print(paste("z2 =", round(z2, 3)))

  low.CI = round((exp(1)^(2 * z1) - 1) / (exp(1)^(2 * z1) + 1), 2)
  print(paste("Lower CI =", low.CI))

  high.CI = round((exp(1)^(2 * z2) - 1) / (exp(1)^(2 * z2) + 1), 2)
  print(paste("Higher CI =", high.CI))

  if (low.CI < 0 & high.CI > 0) {
    print("The samples are not correlated")
  } else {
    print("The samples are correlated")
  }

  return(paste("Confidence Interval:", low.CI, "< rho < ", high.CI))
}
