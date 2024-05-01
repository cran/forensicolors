#' Plot Weighted Distribution of Log10(LR)
#'
#' This function plots the weighted distribution of the log10 of likelihood ratios (LR),
#' using the probabilities provided in 'numerators' for H1 and 'f_h_s_y' for H2 as weights.
#' The densities are shown for two hypotheses, H1 and H2.
#'
#' @param data A dataframe that must contain the columns 'LR', 'numerators', and 'f_h_s_y'.
#' @return A ggplot object showing the weighted density plot.
#' @import ggplot2
#' @examples
#' data <- forensicolors::simRef()
#' conditioned <- conditionedProp(data, 1, 1, 1, 0.01, 0.01, 0.01)
#' unconditioned <- forensicolors::refProp(data)
#' likelihoods <- forensicolors::compute_LRs(conditioned, unconditioned)
#' plotLR(likelihoods)
#' @export
plotLR <- function(data) {
  required_cols <- c("LR", "numerators", "f_h_s_y")
  if (!all(required_cols %in% names(data))) {
    stop("Dataframe must contain LR, numerators, and f_h_s_y columns")
  }
 
  log10_LR <- Condition <- NA

  data$log10_LR <- log10(data$LR)
  
  weights_h1 <- round(data$numerators / min(data$numerators))
  weights_h2 <- round(data$f_h_s_y / min(data$f_h_s_y))
  
  expanded_df <- data.frame(
    log10_LR = rep(data$log10_LR, times = weights_h1),
    Condition = 'H1'
  )
  expanded_df2 <- data.frame(
    log10_LR = rep(data$log10_LR, times = weights_h2),
    Condition = 'H2'
  )
  
  plot_data <- rbind(expanded_df, expanded_df2)
  
  p <- ggplot(plot_data, aes(x = log10_LR, fill = Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("H1" = "red", "H2" = "blue")) +
    labs(title = "",
         x = "Log10(LR)", y = "Density") +
    theme_minimal()
  
  return(p)
}
