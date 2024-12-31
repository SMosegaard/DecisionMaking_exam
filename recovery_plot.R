recov_plot <- function(true, infer, plot_lab, plot_col) {
  
  df <- data.frame(true, infer)
  
  pl <- ggplot(df, aes(x = true, y = infer, color = plot_col)) +
              geom_point() + 
              geom_smooth(method = "lm", se = T, formula = "y ~ x") +
              theme_minimal() +
              xlab(plot_lab[1]) +
              ylab(plot_lab[2]) +
              labs(color = "") +
              ggtitle(paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'"))
  
  return(pl)
  
}