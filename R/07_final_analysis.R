# =============================================================================
# 07_final_analysis.R
# Week 4: integrates the outputs of Weeks 1-3 into final summaries.
#   - driver evidence table: association (Week 1), hypothesis tests and
#     logistic odds ratios (Week 3) and random-forest importance, side by side
#   - customer segments with the highest and lowest observed churn
#   - predicted-risk deciles on the test set (how concentrated churn is)
#   - project-level headline numbers for the final report
# Requires the outputs of 01-06 (run them first, e.g. with run_all.R).
# =============================================================================
source("R/00_setup.R")

telco    <- as_tibble(readRDS(file.path(paths$processed, "telco_clean.rds")))
cramers  <- read_csv(file.path(paths$tables, "w1_cramers_v.csv"))
cor_p    <- read_csv(file.path(paths$tables, "w1_correlation_pearson.csv"))
or_tab   <- read_csv(file.path(paths$tables, "w3_logistic_odds_ratios.csv"))
rf_imp   <- read_csv(file.path(paths$tables, "w3_rf_permutation_importance.csv"))
preds    <- read_csv(file.path(paths$tables, "w3_test_predictions.csv"))
test_met <- read_csv(file.path(paths$tables, "w3_test_metrics.csv"))

# ---- 7.1 Driver evidence across the project ----
# Map dummy-variable terms back to their source variable
source_var <- function(term, vars) {
  hit <- vars[vapply(vars, function(v) startsWith(term, v), logical(1))]
  if (length(hit) == 0) NA_character_ else hit[which.max(nchar(hit))]
}
vars <- setdiff(names(telco), c("customerID", "Churn"))

or_by_var <- or_tab %>%
  mutate(variable = vapply(term, source_var, character(1), vars = vars),
         level = str_remove(term, fixed(variable))) %>%
  group_by(variable) %>%
  slice_max(abs(log(estimate)), n = 1, with_ties = FALSE) %>%   # strongest level per variable
  ungroup() %>%
  transmute(variable, lr_level = if_else(level == "", "(per month)", level),
            lr_odds_ratio = estimate, lr_ci_low = conf.low, lr_ci_high = conf.high, lr_p = p.value)

rf_by_var <- rf_imp %>%
  mutate(variable = vapply(feature, source_var, character(1), vars = vars)) %>%
  group_by(variable) %>%
  summarise(rf_importance = max(importance)) %>%
  mutate(rf_rank = rank(-rf_importance, ties.method = "min"))

assoc <- bind_rows(
  cramers %>% transmute(variable, week1_measure = "Cramer's V", week1_value = cramers_v),
  cor_p %>% filter(variable %in% c("tenure", "MonthlyCharges", "TotalCharges")) %>%
    transmute(variable, week1_measure = "Point-biserial r", week1_value = ChurnYes))

driver_evidence <- assoc %>%
  left_join(or_by_var, by = "variable") %>%
  left_join(rf_by_var, by = "variable") %>%
  arrange(rf_rank)
print(driver_evidence %>% mutate(across(where(is.double), ~ signif(.x, 3))), n = Inf, width = 120)
save_table(driver_evidence, "w4_driver_evidence")

# Tenure effect expressed per 12 months (the per-month odds ratio compounds)
tenure_or <- or_tab %>% filter(term == "tenure")
tenure_or_12 <- exp(12 * log(c(tenure_or$estimate, tenure_or$conf.low, tenure_or$conf.high)))
round(tenure_or_12, 3)

# ---- 7.2 Customer segments by observed churn ----
segments <- telco %>%
  mutate(tenure_group = if_else(tenure <= 12, "Tenure 0-12 m", "Tenure 13+ m")) %>%
  group_by(Contract, InternetService, tenure_group) %>%
  summarise(customers = n(), churned = sum(Churn == "Yes"), churn_rate = churned / customers,
            .groups = "drop") %>%
  filter(customers >= 50) %>%
  arrange(desc(churn_rate))
print(segments %>% mutate(churn_rate = round(churn_rate, 3)), n = Inf)
save_table(segments, "w4_segments")
top_seg <- segments[1, ]
share_churners_top3 <- sum(segments$churned[1:3]) / sum(telco$Churn == "Yes")
share_customers_top3 <- sum(segments$customers[1:3]) / nrow(telco)
c(share_customers_top3 = share_customers_top3, share_churners_top3 = share_churners_top3)

seg_plot_data <- segments %>%
  mutate(label = paste(Contract, InternetService, tenure_group, sep = " | "),
         label = str_replace_all(label, " \\| ", ", "))
p_seg <- ggplot(seg_plot_data, aes(x = churn_rate, y = fct_reorder(label, churn_rate))) +
  geom_vline(xintercept = mean(telco$Churn == "Yes"), linetype = "dashed", colour = "#B0B0B0") +
  geom_col(fill = col_accent, width = 0.7) +
  geom_label(aes(label = sprintf("%s (n = %s)", pct(churn_rate, 0), comma(customers))),
             hjust = -0.03, size = 2.7, colour = col_text, fill = "white",
             label.size = 0, label.padding = unit(0.1, "lines")) +
  scale_x_continuous(labels = label_percent(), limits = c(0, 0.95),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = sprintf("Churn ranges from %s to %s across contract, internet and tenure segments",
                       pct(min(segments$churn_rate), 0), pct(max(segments$churn_rate), 0)),
       subtitle = "Segments with at least 50 customers; dashed line = overall churn rate",
       x = "Churn rate", y = NULL) +
  theme(axis.text.y = element_text(size = 7.5))
save_plot(p_seg, "w4_segments", width = 7, height = 5.4)

# ---- 7.3 Predicted-risk deciles on the test set ----
deciles <- preds %>%
  mutate(risk_decile = ntile(-prob_logistic, 10)) %>%        # decile 1 = highest predicted risk
  group_by(risk_decile) %>%
  summarise(customers = n(), churners = sum(actual == "Yes"),
            churn_rate = churners / customers, mean_predicted = mean(prob_logistic)) %>%
  mutate(cumulative_share_of_churners = cumsum(churners) / sum(churners),
         lift = churn_rate / mean(preds$actual == "Yes"))
print(deciles %>% mutate(across(where(is.double), ~ round(.x, 3))), width = 110)
save_table(deciles, "w4_risk_deciles")

p_dec <- ggplot(deciles, aes(x = factor(risk_decile), y = churn_rate)) +
  geom_col(fill = col_accent, width = 0.7) +
  geom_hline(yintercept = mean(preds$actual == "Yes"), linetype = "dashed", colour = col_muted) +
  geom_text(aes(label = pct(churn_rate, 0)), vjust = -0.4, size = 3, colour = col_text) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(title = sprintf("The top risk decile churned at %s, the bottom decile at %s",
                       pct(deciles$churn_rate[1], 0), pct(deciles$churn_rate[10], 0)),
       subtitle = "Test-set customers grouped into deciles of logistic-regression risk; dashed = average",
       x = "Predicted-risk decile (1 = highest risk)", y = "Observed churn rate")
save_plot(p_dec, "w4_risk_deciles", width = 6.8, height = 4)

p_dist <- ggplot(preds, aes(x = prob_logistic, fill = actual)) +
  geom_histogram(binwidth = 0.025, boundary = 0, colour = "white", linewidth = 0.2) +
  facet_wrap(~ factor(actual, levels = c("No", "Yes"), labels = c("Retained (actual)", "Churned (actual)")),
             ncol = 1, scales = "free_y") +
  scale_fill_manual(values = pal_churn, guide = "none") +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "Predicted churn probability for customers who stayed and who left",
       subtitle = "Logistic regression, test set; note the different y-axis scales of the two panels",
       x = "Predicted probability of churn", y = "Customers")
save_plot(p_dist, "w4_predicted_risk_distribution", width = 7, height = 4.4)

# ---- 7.4 Headline numbers for the final report ----
headline <- tibble(
  item = c("Customers analysed", "Overall churn rate", "Month-to-month churn rate",
           "Two-year contract churn rate", "Hypotheses rejected (of 8, Holm-adjusted)",
           "Test ROC-AUC, logistic regression", "Test ROC-AUC, random forest",
           "Churners reached in top 20% of predicted risk (logistic)"),
  value = c(comma(nrow(telco)), pct(mean(telco$Churn == "Yes")),
            pct(mean(telco$Churn[telco$Contract == "Month-to-month"] == "Yes")),
            pct(mean(telco$Churn[telco$Contract == "Two year"] == "Yes")),
            as.character(sum(read_csv(file.path(paths$tables, "w3_hypothesis_test_summary.csv"))$decision == "Reject H0")),
            sprintf("%.3f", test_met$roc_auc[test_met$model == "Logistic regression"][1]),
            sprintf("%.3f", test_met$roc_auc[test_met$model == "Random forest"][1]),
            pct(deciles$cumulative_share_of_churners[2], 0))
)
headline
save_table(headline, "w4_headline_numbers")

# ---- 7.5 Metrics for the report ----
record("w4_tenure_or12", tenure_or_12[1]); record("w4_tenure_or12_low", tenure_or_12[2])
record("w4_tenure_or12_high", tenure_or_12[3])
record("w4_top_seg_rate", top_seg$churn_rate); record("w4_top_seg_n", top_seg$customers)
record("w4_top_seg_label", paste(top_seg$Contract, top_seg$InternetService, top_seg$tenure_group, sep = ", "))
record("w4_n_segments", nrow(segments))
record("w4_seg_min_rate", min(segments$churn_rate)); record("w4_seg_max_rate", max(segments$churn_rate))
record("w4_share_customers_top3", share_customers_top3); record("w4_share_churners_top3", share_churners_top3)
record("w4_decile1_rate", deciles$churn_rate[1]); record("w4_decile10_rate", deciles$churn_rate[10])
record("w4_decile1_lift", deciles$lift[1]); record("w4_top2_deciles_share", deciles$cumulative_share_of_churners[2])
record("w4_top3_deciles_share", deciles$cumulative_share_of_churners[3])
for (i in seq_len(nrow(driver_evidence))) {
  record(paste0("w4_rf_rank_", driver_evidence$variable[i]), driver_evidence$rf_rank[i])
}
write_metrics("07_final_analysis")
