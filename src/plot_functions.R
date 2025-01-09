
recovery_plots <- function(df, parameter_filter) {
  df_filtered <- df[df$parameter %in% parameter_filter, ]
  ggplot(df_filtered, aes(x = true, y = infer)) +
    geom_point(color = "cornflowerblue") +
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "cornflowerblue") +
    facet_wrap(~parameter, nrow = 1, scales = "free",
               labeller = labeller(parameter = c("mu_alpha" = "μα", "mu_rho" = "μρ",
                                                 "sigma_alpha" = "σα", "sigma_rho" = "σρ"))) +
    theme_minimal() +
    xlab("True") +
    ylab("Recovered") +
    theme(strip.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))}


# old function, from Andreas
recov_plot <- function(true, infer, plot_lab, plot_col) {
  df <- data.frame(true, infer)
  pl <- ggplot(df, aes(x = true, y = infer, color = plot_col)) +
    geom_point() + 
    geom_smooth(method = "lm", se = T, formula = "y ~ x") +
    theme_bw() +
    xlab(plot_lab[1]) +
    ylab(plot_lab[2]) +
    labs(color = "") +
    ggtitle(paste0("'", plot_lab[2], "' compared to '", plot_lab[1], "'"))
  return(pl)}


combined_posterior_plots <- function(df) {
  ggplot(df, aes(x = samples, color = type)) +
    geom_density() +
    facet_wrap(~parameter, nrow = 2, scales = "free", 
               labeller = labeller(parameter = c("alpha" = "μα", "rho" = "μρ"))) +
    scale_color_manual(values = c("mix" = "darkseagreen4", "same" = "cornflowerblue")) +
    labs(x = "", y = "Density", color = "type") +
    theme_bw() + 
    theme(axis.title.y = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          strip.background = element_blank(),
          plot.background = element_rect(fill = "white"))}


# "basic plot function", with and without rho
posterior_plots <- function(samples, title) {
  par(mfrow = c(2, 2))
  plot(density(samples$BUGSoutput$sims.list$mu_alpha), main = paste("mu_alpha, ", title))
  plot(density(samples$BUGSoutput$sims.list$mu_rho), main = paste("mu_rho, ", title))}

posterior_plots_no_rho <- function(samples, title) {
  par(mfrow = c(2, 2))
  plot(density(samples$BUGSoutput$sims.list$mu_alpha), main = paste("mu_alpha, ", title))}


diff_plot <- function(df) {
  ggplot(df, aes(x = samples)) +
    geom_density() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "cornflowerblue") +
    facet_wrap(~parameter, ncol = 1, scales = "free",
               labeller = labeller(parameter = c("alpha" = "Δα", "rho" = "Δρ"))) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_rect(fill = "transparent", colour = NA),
          strip.text = element_text(size = 12, face = "bold")) +
    labs(x = "", y = "Density")
}



