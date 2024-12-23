#' Correlation Analysis
#'
#' This function performs correlation analysis based on the normality of the input vectors.
#' It generates histograms, Q-Q plots, and chooses Pearson or Spearman correlation test based on Shapiro-Wilk test results.
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector.
#' @return The result of either Pearson or Spearman correlation test.
#' @examples
#' corr.analysis(rnorm(100), rnorm(100))
#' @export
corr.analysis <- function(vec1, vec2){
  par(mfrow = c(2,2))
  hist(vec1)
  hist(vec2)
  qqnorm(vec1)
  qqline(vec1)
  qqnorm(vec2)
  qqline(vec2)
  s1 <- shapiro.test(vec1)
  s2 <- shapiro.test(vec2)
  if(s1$p.value > 0.05 & s2$p.value > 0.05){
    cor.test(vec1, vec2, method = 'pearson')
  }
  else{
    cor.test(vec1,vec2, method = 'spearman')
  }
}
