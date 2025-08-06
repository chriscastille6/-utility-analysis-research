# Rebuild Ock & Oswald Figure 2 and Apply to Enhanced Data
# Uses GMA + Structured Interview as the predictor set

library(dplyr)
library(ggplot2)
library(MASS)
library(scales)

# Parameters matching Ock & Oswald Figure 2
SR <- 0.40
N_selected <- 100
cost_per_applicant_comp <- 1600
cost_per_applicant_mh <- 160
n_applicants <- N_selected / SR
SDy_range <- seq(5000, 60000, by = 5000)
iterations <- 500
mean_salary <- 50000

# Predictor set
predictors <- c("GMA", "Structured_Interview")

# Load Berry matrix and validities
berry_cor_matrix <- matrix(c(
  1.00, 0.13, 0.54, 0.21, 0.25, 0.42,
  0.13, 1.00, 0.03, 0.18, 0.01, 0.29,
  0.54, 0.03, 1.00, 0.08, 0.28, 0.23,
  0.21, 0.18, 0.08, 1.00, -0.02, 0.45,
  0.25, 0.01, 0.28, -0.02, 1.00, 0.16,
  0.42, 0.29, 0.23, 0.45, 0.16, 1.00
), nrow=6, byrow=TRUE)
var_names <- c("Biodata", "GMA", "Conscientiousness", "Structured_Interview", "Integrity", "SJT")
colnames(berry_cor_matrix) <- rownames(berry_cor_matrix) <- var_names
berry_validities <- c(0.38, 0.31, 0.19, 0.42, 0.31, 0.26)
names(berry_validities) <- var_names

# Sturman economic adjustment parameters
sturman_params <- list(
  discount_rate = 0.10,
  tax_rate = 0.35,
  variable_cost_ratio = 0.20,
  time_horizon = 5
)

# Utility functions
ux <- function(selection_ratio) {
  dnorm(qnorm(1 - selection_ratio)) / selection_ratio
}

calculate_traditional_utility <- function(validity, selection_ratio, n_selected, sdy, cost_per_applicant) {
  utility <- n_selected * sdy * validity * ux(selection_ratio) * sturman_params$time_horizon - n_applicants * cost_per_applicant
  return(utility)
}

calculate_sturman_utility <- function(validity, selection_ratio, n_selected, sdy, cost_per_applicant) {
  annual_benefit <- n_selected * validity * ux(selection_ratio) * sdy
  annual_variable_costs <- annual_benefit * sturman_params$variable_cost_ratio
  annual_net_benefit <- annual_benefit - annual_variable_costs
  pv_factor <- (1 - (1 + sturman_params$discount_rate)^(-sturman_params$time_horizon)) / sturman_params$discount_rate
  pv_benefits <- annual_net_benefit * pv_factor
  after_tax_pv <- pv_benefits * (1 - sturman_params$tax_rate)
  after_tax_pv - n_applicants * cost_per_applicant
}

generate_berry_data <- function(n_applicants, predictors) {
  pred_indices <- which(var_names %in% predictors)
  cor_matrix <- berry_cor_matrix[pred_indices, pred_indices]
  predictor_scores <- mvrnorm(n_applicants, mu = rep(0, length(predictors)), Sigma = cor_matrix)
  criterion_scores <- rnorm(n_applicants, 0, 1)
  validities <- berry_validities[pred_indices]
  for (i in 1:length(predictors)) {
    criterion_scores <- criterion_scores + validities[i] * predictor_scores[, i]
  }
  criterion_scores <- scale(criterion_scores)[, 1]
  list(predictors = predictor_scores, criterion = criterion_scores)
}

compensatory_selection <- function(scores, selection_ratio) {
  composite_score <- rowMeans(scores)
  cutoff <- quantile(composite_score, 1 - selection_ratio)
  selected <- composite_score >= cutoff
  list(selected = selected, composite_scores = composite_score)
}

multiple_hurdle_selection <- function(scores, selection_ratio) {
  n_candidates <- nrow(scores)
  n_predictors <- ncol(scores)
  hurdle_ratio <- 1 - (1 - selection_ratio)^(1/n_predictors)
  cutoffs <- apply(scores, 2, function(x) quantile(x, 1 - hurdle_ratio))
  selected <- rep(TRUE, n_candidates)
  for (i in 1:n_predictors) {
    selected <- selected & (scores[, i] >= cutoffs[i])
  }
  list(selected = selected)
}

# Simulate for each SDy value
results <- data.frame()
for (sdy in SDy_range) {
  comp_utils_trad <- c()
  hurdle_utils_trad <- c()
  comp_utils_stur <- c()
  hurdle_utils_stur <- c()
  for (i in 1:iterations) {
    data <- generate_berry_data(n_applicants, predictors)
    comp_sel <- compensatory_selection(data$predictors, SR)
    hurdle_sel <- multiple_hurdle_selection(data$predictors, SR)
    comp_validity <- cor(data$criterion, rowMeans(data$predictors))
    hurdle_validity <- cor(data$criterion[hurdle_sel$selected], rowMeans(data$predictors[hurdle_sel$selected, , drop = FALSE]))
    comp_utils_trad <- c(comp_utils_trad, calculate_traditional_utility(comp_validity, SR, sum(comp_sel$selected), sdy, cost_per_applicant_comp))
    hurdle_utils_trad <- c(hurdle_utils_trad, calculate_traditional_utility(hurdle_validity, SR, sum(hurdle_sel$selected), sdy, cost_per_applicant_mh))
    comp_utils_stur <- c(comp_utils_stur, calculate_sturman_utility(comp_validity, SR, sum(comp_sel$selected), sdy, cost_per_applicant_comp))
    hurdle_utils_stur <- c(hurdle_utils_stur, calculate_sturman_utility(hurdle_validity, SR, sum(hurdle_sel$selected), sdy, cost_per_applicant_mh))
  }
  results <- rbind(results, data.frame(
    SDy = sdy,
    Method = "Compensatory (Traditional)",
    Mean = mean(comp_utils_trad),
    Q1 = quantile(comp_utils_trad, 0.25),
    Q3 = quantile(comp_utils_trad, 0.75)
  ))
  results <- rbind(results, data.frame(
    SDy = sdy,
    Method = "Multiple Hurdle (Traditional)",
    Mean = mean(hurdle_utils_trad),
    Q1 = quantile(hurdle_utils_trad, 0.25),
    Q3 = quantile(hurdle_utils_trad, 0.75)
  ))
  results <- rbind(results, data.frame(
    SDy = sdy,
    Method = "Compensatory (Economic)",
    Mean = mean(comp_utils_stur),
    Q1 = quantile(comp_utils_stur, 0.25),
    Q3 = quantile(comp_utils_stur, 0.75)
  ))
  results <- rbind(results, data.frame(
    SDy = sdy,
    Method = "Multiple Hurdle (Economic)",
    Mean = mean(hurdle_utils_stur),
    Q1 = quantile(hurdle_utils_stur, 0.25),
    Q3 = quantile(hurdle_utils_stur, 0.75)
  ))
}

# Plot
p <- ggplot(results, aes(x = SDy, y = Mean, color = Method, linetype = Method, fill = Method)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), alpha = 0.18, color = NA) +
  scale_color_manual(values = c(
    "Compensatory (Traditional)" = "#222222",
    "Multiple Hurdle (Traditional)" = "#888888",
    "Compensatory (Economic)" = "#0072B2",
    "Multiple Hurdle (Economic)" = "#E69F00"
  )) +
  scale_fill_manual(values = c(
    "Compensatory (Traditional)" = "#222222",
    "Multiple Hurdle (Traditional)" = "#888888",
    "Compensatory (Economic)" = "#0072B2",
    "Multiple Hurdle (Economic)" = "#E69F00"
  )) +
  scale_linetype_manual(values = c(
    "Compensatory (Traditional)" = "solid",
    "Multiple Hurdle (Traditional)" = "dashed",
    "Compensatory (Economic)" = "solid",
    "Multiple Hurdle (Economic)" = "dashed"
  )) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1, big.mark = ",")) +
  labs(title = "Utility of Selection Methods: Traditional vs. Economic Adjustments",
       subtitle = "GMA + Structured Interview, SR = 0.40, N = 100, Cost/applicant: Comp = $1,600, MH = $160",
       x = "SDy",
       y = "Utility in Dollars (Millions)") +
  theme_bw() +
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
        legend.box.background = element_rect(color = "black", linewidth = 0.5),
        legend.box.margin = margin(4, 4, 4, 4),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave("figures/ock_oswald_figure2_trad_vs_econ.png", p, width = 8, height = 6)

cat("Overlay plot saved as figures/ock_oswald_figure2_trad_vs_econ.png\n") 