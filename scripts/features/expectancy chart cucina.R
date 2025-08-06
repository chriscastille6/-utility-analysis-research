#–– 1. Load required libraries ––
library(mvtnorm)   # for pmvnorm integration
library(ggplot2)   # for plotting

#–– 2. Define Expectancyfunc exactly as in Cucina et al. ––
Expectancyfunc <- function(Validity,
                           PredLowerCut, PredUpperCut,
                           CritLowerCut, CritUpperCut) {
  mean <- c(0, 0)
  lower <- c(PredLowerCut, CritLowerCut)
  upper <- c(PredUpperCut, CritUpperCut)
  corr <- diag(2)
  corr[lower.tri(corr)] <- Validity
  corr[upper.tri(corr)] <- Validity
  jtprob <- pmvnorm(
    lower     = lower,
    upper     = upper,
    mean      = mean,
    sigma     = corr,
    algorithm = Miwa(steps = 128)
  )
  xprob <- pnorm(PredUpperCut) - pnorm(PredLowerCut)
  round(100 * as.numeric(jtprob) / xprob, 1)
}

#–– 3. Define quartile cut-offs and labels ––
quartileCuts   <- c(-Inf,
                    qnorm(0.25),  # 25th percentile ≈ –0.6745
                    qnorm(0.50),  # 50th percentile = 0
                    qnorm(0.75),  # 75th percentile ≈ +0.6745
                    Inf)
quartile_labels <- c("Bottom 25%", "25–50%", "50–75%", "Top 25%")
pct75 <- qnorm(0.75)   # criterion cutoff: top 25%

#–– 4. Validity coefficients for each adjustment ––
validities <- c(
  "Uncorrected"      = 0.457,  # raw Pearson r
  "Reliability only" = 0.511,  # corr. for criterion unreliability
  "Full correction"  = 0.600   # corr. for unreliability & range restriction
)

#–– 5. CI parameters (Fisher’s z, n = 500 for visibility) ––
n_ci   <- 500     # meta-analytic n set to 500 for discernible CIs
alpha  <- 0.05    # 95% CI
n_sims <- 5000    # Monte Carlo draws

#–– 6. Silent wrapper to suppress Expectancyfunc prints ––
silent_expectancy <- function(...) {
  capture.output(val <- Expectancyfunc(...))
  val
}

#–– 7. Function to compute point estimate + 95% CI ––
get_exp_ci <- function(rho, pl, pu, cl, cu) {
  # Fisher’s z transform and se
  z0   <- atanh(rho)
  se_z <- 1 / sqrt(n_ci - 3)
  # simulate z, back-transform to r
  z_samps <- rnorm(n_sims, mean = z0, sd = se_z)
  r_samps <- tanh(z_samps)
  # compute simulated expectancies
  exp_samps <- sapply(r_samps, function(r) 
    silent_expectancy(r, pl, pu, cl, cu)
  )
  # point estimate
  pt <- silent_expectancy(rho, pl, pu, cl, cu)
  # 95% CI bounds
  ci <- quantile(exp_samps, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  c(pt = pt, lower = ci[1], upper = ci[2])
}

#–– 8. Build data frame of all expectancies + CIs ––
df_all <- do.call(rbind, lapply(names(validities), function(adj) {
  rho    <- validities[adj]
  ci_mat <- t(sapply(1:4, function(i) 
    get_exp_ci(rho,
               quartileCuts[i], quartileCuts[i+1],  # predictor quartile
               pct75,             Inf)              # criterion = top 25%
  ))
  colnames(ci_mat) <- c("pt", "lower", "upper")
  data.frame(
    Quartile   = factor(quartile_labels, levels = quartile_labels),
    Adjustment = adj,
    Expectancy = ci_mat[, "pt"],    # point estimates
    CI_low     = ci_mat[, "lower"], # lower 95% bound
    CI_high    = ci_mat[, "upper"], # upper 95% bound
    stringsAsFactors = FALSE
  )
}))
df_all$Adjustment <- factor(df_all$Adjustment, levels = names(validities))

#–– 9. Define bar fill and text colors ––
fill_colors <- c(
  "Uncorrected"      = "white",
  "Reliability only" = "grey70",
  "Full correction"  = "steelblue"
)
df_all$text_color <- ifelse(df_all$Adjustment == "Full correction",
                            "white", "black")

#–– 10. Plot grouped bar chart with 95% CIs ––
ggplot(df_all, aes(x = Quartile, y = Expectancy, fill = Adjustment)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.6, color = "black") +            # bars with outline
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                position = position_dodge(width = 0.8),
                width = 0.2) +                         # CI error bars
  geom_text(aes(label = paste0(Expectancy, "%"),
                y     = Expectancy / 2,
                color = text_color),
            position = position_dodge(width = 0.8),
            size = 4, show.legend = FALSE) +           # centered percent labels
  scale_fill_manual(values = fill_colors, guide = FALSE) + # white/grey/blue fills, no legend
  scale_color_identity() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(df_all$CI_high) * 1.1)) +
  labs(
    x = "Predictor Quartile",
    y = "Pct. in Top 25% of Criterion"
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(),   # keep horizontal grid lines
    panel.grid.major.x = element_blank(),  # remove vertical grid lines
    panel.grid.minor   = element_blank(),  # remove minor grid lines
    axis.line          = element_line()     # solid x & y axes
  )
