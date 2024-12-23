#' Linear Analysis
#'
#' This function performs linear regression analysis between a predictor and response variable,
#' plots a scatterplot with a regression line, a residuals plot, and returns prediction intervals
#' if the predictor's p-value is significant.
#'
#' @param data A data frame with two columns: predictor and response.
#' @param predict_val A numeric vector of predictor values for prediction.
#' @return A list containing prediction intervals if the predictor's p-value is significant; otherwise, NULL.
#' @examples
#' df <- data.frame(x = rnorm(100), y = rnorm(100))
#' Linear.analysis(df, predict_val = c(1, 2, 3))
#' @export
Linear.analysis <- function(data, predict_val) {
  # Validate the input
  if (!is.data.frame(data) || ncol(data) != 2) {
    stop("The input must be a data frame with exactly two columns.")
  }

  # Assign column names for clarity
  colnames(data) <- c("predictor", "response")

  # Fit the linear model
  model <- lm(response ~ predictor, data = data)

  # Set up the plotting layout
  par(mfrow = c(1, 2))

  # Scatterplot with regression line
  plot(data$predictor, data$response, pch = 19, col = 'cornflowerblue',
       main = "Scatterplot with Regression Line", xlab = "Predictor", ylab = "Response")
  abline(model, col = 'red')

  # Residuals plot
  plot(model$residuals, pch = 19, col = 'darkgreen',
       main = "Residuals Plot", xlab = "Index", ylab = "Residuals")
  abline(h = 0, col = 'blue', lty = 2)

  # Shapiro-Wilk test
  shapiro_test <- shapiro.test(model$residuals)
  cat("\nShapiro-Wilk Test for Residuals:\n")
  print(shapiro_test)

  # Model summary
  summary_results <- summary(model)

  # Check if the predictor's p-value is significant
  predictor_pval <- summary_results$coefficients[2, 4] # Extract p-value of the predictor
  if (predictor_pval < 0.05) {
    # Perform prediction if the p-value is significant
    prediction_data <- data.frame(predictor = predict_val)
    predicted_interval <- predict(model, prediction_data, interval = "prediction")

    return(list(intervals = predicted_interval))
  } else {
    # Return message if p-value is not significant
    cat("\nThe t-test p-value of the predictor is not significant (p =", predictor_pval, "). No prediction performed.\n")
    return(NULL)
  }
}
