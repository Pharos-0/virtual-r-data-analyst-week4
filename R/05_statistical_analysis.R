# =============================================================================
# 05_statistical_analysis.R
# Week 3 - Part A: hypothesis testing on the cleaned Telco data.
#   - class balance, normality and equal-variance checks
#   - H1 Contract vs churn (chi-square test of independence)
#   - H2 MonthlyCharges by churn (Welch t-test, Wilcoxon as robustness check)
#   - H3 Tenure by churn (Wilcoxon rank-sum test)
#   - H4 MonthlyCharges across internet service types (ANOVA / Welch / Kruskal)
#   - H5 Tenure vs MonthlyCharges (correlation tests)
#   - H6 Payment method vs churn (chi-square)
#   - H7 Senior citizen vs churn (two-proportion test)
#   - H8 Gender vs churn (chi-square)
# Significance level: alpha = 0.05, with Holm adjustment across the 8 tests.
# =============================================================================
source("R/00_setup.R")
load_packages(c("car", "nortest"))

telco <- as_tibble(readRDS(file.path(paths$processed, "telco_clean.rds")))
alpha <- 0.05

# ---- 5.1 Class balance and summary statistics by churn status ----
table(telco$Churn)
round(prop.table(table(telco$Churn)), 4)

summary_by_churn <- telco %>%
  select(Churn, tenure, MonthlyCharges, TotalCharges) %>%
  pivot_longer(-Churn, names_to = "variable", values_to = "value") %>%
  group_by(variable, Churn) %>%
  summarise(n = n(), mean = mean(value), sd = sd(value), min = min(value),
            q1 = quantile(value, 0.25), median = median(value),
            q3 = quantile(value, 0.75), max = max(value), .groups = "drop")
print(summary_by_churn %>% mutate(across(where(is.double), ~ round(.x, 2))), width = 100)
save_table(summary_by_churn, "w3_summary_by_churn")
tapply(telco$tenure, telco$Churn, summary)

# ---- 5.2 Normality checks for the numeric variables ----
# Large samples make formal tests reject normality for trivial departures, so
# skewness and Q-Q plots are read alongside the Anderson-Darling test.
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
normality <- telco %>%
  select(Churn, tenure, MonthlyCharges) %>%
  pivot_longer(-Churn, names_to = "variable", values_to = "value") %>%
  group_by(variable, Churn) %>%
  summarise(n = n(), skewness = skew(value),
            ad_statistic = nortest::ad.test(value)$statistic,
            ad_p_value = nortest::ad.test(value)$p.value, .groups = "drop")
normality
save_table(normality, "w3_normality_tests")

# Shapiro-Wilk accepts at most 5,000 values, so it is run on a random sample
set.seed(SEED)
shapiro.test(sample(telco$MonthlyCharges, 5000))

qq_data <- telco %>%
  select(Churn, tenure, MonthlyCharges) %>%
  pivot_longer(-Churn, names_to = "variable", values_to = "value") %>%
  mutate(group = paste0(ifelse(variable == "tenure", "Tenure", "Monthly charges"), ", ",
                        ifelse(Churn == "Yes", "churned", "retained")))
p_qq <- ggplot(qq_data, aes(sample = value, colour = Churn)) +
  stat_qq(size = 0.5, alpha = 0.5) +
  stat_qq_line(colour = col_text, linewidth = 0.4) +
  facet_wrap(~ group, scales = "free_y") +
  scale_colour_manual(values = pal_churn, guide = "none") +
  labs(title = "Neither tenure nor monthly charges is normally distributed within churn groups",
       subtitle = "Normal Q-Q plots; points would follow the line if the data were normal",
       x = "Theoretical normal quantile", y = "Observed value")
save_plot(p_qq, "w3_qq_plots", width = 7, height = 5)

# ---- 5.3 Equal-variance checks (Levene / Brown-Forsythe) ----
levene_mc_churn     <- car::leveneTest(MonthlyCharges ~ Churn, data = telco)
levene_tenure_churn <- car::leveneTest(tenure ~ Churn, data = telco)
levene_mc_internet  <- car::leveneTest(MonthlyCharges ~ InternetService, data = telco)
levene_mc_churn
levene_tenure_churn
levene_mc_internet
telco %>% group_by(Churn) %>% summarise(var_monthly = var(MonthlyCharges), var_tenure = var(tenure))

# ---- 5.4 H1: Contract type and churn (chi-square test of independence) ----
# H0: churn is independent of contract type. H1: churn depends on contract type.
tab_contract <- table(telco$Contract, telco$Churn)
tab_contract
h1 <- chisq.test(tab_contract)
h1
min(h1$expected)                         # all expected counts must be >= 5
round(h1$stdres, 2)                      # standardised residuals by cell
h1_v <- sqrt(unname(h1$statistic) / (sum(tab_contract) * (min(dim(tab_contract)) - 1)))
h1_v

# ---- 5.5 H2: Monthly charges of churned vs retained customers ----
# H0: mean monthly charges are equal. H1: they differ.
h2 <- t.test(MonthlyCharges ~ Churn, data = telco, var.equal = FALSE)
h2
h2_means <- telco %>% group_by(Churn) %>%
  summarise(n = n(), mean = mean(MonthlyCharges), sd = sd(MonthlyCharges))
h2_means
pooled_sd <- sqrt(((h2_means$n[1] - 1) * h2_means$sd[1]^2 + (h2_means$n[2] - 1) * h2_means$sd[2]^2) /
                    (sum(h2_means$n) - 2))
h2_d <- (h2_means$mean[2] - h2_means$mean[1]) / pooled_sd     # Cohen's d (Yes - No)
h2_d
# Robustness check that does not assume normality
h2_w <- wilcox.test(MonthlyCharges ~ Churn, data = telco, conf.int = TRUE)
h2_w

# ---- 5.6 H3: Tenure of churned vs retained customers ----
# H0: the tenure distributions are the same. H1: they differ (location shift).
h3 <- wilcox.test(tenure ~ Churn, data = telco, conf.int = TRUE)
h3
n_no <- sum(telco$Churn == "No"); n_yes <- sum(telco$Churn == "Yes")
# Rank-biserial correlation: positive = retained customers tend to have longer tenure
h3_r <- 2 * unname(h3$statistic) / (n_no * n_yes) - 1
h3_r
telco %>% group_by(Churn) %>% summarise(median_tenure = median(tenure), mean_tenure = mean(tenure))

# ---- 5.7 H4: Monthly charges across internet service types ----
# H0: mean monthly charges are equal for DSL, Fiber optic and No internet.
h4_aov <- aov(MonthlyCharges ~ InternetService, data = telco)
summary(h4_aov)
h4_eta2 <- summary(h4_aov)[[1]][["Sum Sq"]][1] / sum(summary(h4_aov)[[1]][["Sum Sq"]])
h4_eta2
h4_welch <- oneway.test(MonthlyCharges ~ InternetService, data = telco, var.equal = FALSE)
h4_welch
h4_kw <- kruskal.test(MonthlyCharges ~ InternetService, data = telco)
h4_kw
pairwise.t.test(telco$MonthlyCharges, telco$InternetService, pool.sd = FALSE,
                p.adjust.method = "holm")
h4_tukey <- TukeyHSD(h4_aov)
h4_tukey

ragg::agg_png(file.path(paths$figures, "w3_anova_residuals.png"), width = 7, height = 3.4,
              units = "in", res = 300, background = "white")
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 1), cex = 0.8)
plot(h4_aov, which = 1, caption = "", sub.caption = "", main = "Residuals vs fitted")
plot(h4_aov, which = 2, caption = "", sub.caption = "", main = "Normal Q-Q of residuals")
invisible(dev.off())

# ---- 5.8 H5: Correlation between tenure and monthly charges ----
# H0: the correlation is zero. H1: it is not zero.
h5_pearson  <- cor.test(telco$tenure, telco$MonthlyCharges, method = "pearson")
h5_spearman <- cor.test(telco$tenure, telco$MonthlyCharges, method = "spearman", exact = FALSE)
h5_pearson
h5_spearman

# ---- 5.9 H6: Payment method and churn ----
tab_payment <- table(telco$PaymentMethod, telco$Churn)
h6 <- chisq.test(tab_payment)
h6
round(h6$stdres, 2)
h6_v <- sqrt(unname(h6$statistic) / (sum(tab_payment) * (min(dim(tab_payment)) - 1)))
h6_v

# ---- 5.10 H7: Senior citizens and churn (difference in proportions) ----
tab_senior <- table(telco$SeniorCitizen, telco$Churn)[c("Yes", "No"), c("Yes", "No")]
tab_senior
h7 <- prop.test(tab_senior)
h7

# ---- 5.11 H8: Gender and churn ----
tab_gender <- table(telco$gender, telco$Churn)
h8 <- chisq.test(tab_gender)
h8
round(prop.table(tab_gender, 1), 3)

# ---- 5.12 Summary of all tests with Holm adjustment ----
test_summary <- tibble(
  hypothesis = paste0("H", 1:8),
  question   = c("Contract type vs churn", "Monthly charges: churned vs retained",
                 "Tenure: churned vs retained", "Monthly charges across internet types",
                 "Tenure vs monthly charges", "Payment method vs churn",
                 "Senior citizen vs churn", "Gender vs churn"),
  test       = c("Pearson chi-square", "Welch two-sample t-test", "Wilcoxon rank-sum",
                 "Welch one-way ANOVA", "Pearson correlation", "Pearson chi-square",
                 "Two-proportion test (chi-square)", "Pearson chi-square (Yates)"),
  statistic  = c(h1$statistic, h2$statistic, h3$statistic, h4_welch$statistic,
                 h5_pearson$statistic, h6$statistic, h7$statistic, h8$statistic),
  p_value    = c(h1$p.value, h2$p.value, h3$p.value, h4_welch$p.value, h5_pearson$p.value,
                 h6$p.value, h7$p.value, h8$p.value),
  effect_size = c(sprintf("Cramer's V = %.3f", h1_v), sprintf("Cohen's d = %.2f", h2_d),
                  sprintf("rank-biserial r = %.2f", h3_r), sprintf("eta-squared = %.3f", h4_eta2),
                  sprintf("r = %.3f", h5_pearson$estimate), sprintf("Cramer's V = %.3f", h6_v),
                  sprintf("diff = %.3f", diff(rev(h7$estimate))),
                  sprintf("Cramer's V = %.3f", sqrt(unname(h8$statistic) / sum(tab_gender))))
) %>%
  mutate(p_holm = p.adjust(p_value, method = "holm"),
         decision = if_else(p_holm < alpha, "Reject H0", "Fail to reject H0"))
print(test_summary %>% select(hypothesis, test, statistic, p_value, p_holm, decision), width = 110)
test_summary %>% select(hypothesis, effect_size)
save_table(test_summary, "w3_hypothesis_test_summary")

# ---- 5.13 Metrics for the report ----
record("alpha", alpha)
for (i in seq_len(nrow(summary_by_churn))) {
  for (st in c("mean", "sd", "median", "q1", "q3")) {
    record(paste0("sum_", summary_by_churn$variable[i], "_", st, "_", summary_by_churn$Churn[i]),
           summary_by_churn[[st]][i])
  }
}
record("h1_chisq", unname(h1$statistic)); record("h1_df", unname(h1$parameter))
record("h1_p", h1$p.value); record("h1_cramers_v", h1_v); record("h1_min_expected", min(h1$expected))
record("h1_stdres_m2m_yes", h1$stdres["Month-to-month", "Yes"])
record("h1_stdres_2y_yes", h1$stdres["Two year", "Yes"])
record("h2_t", unname(h2$statistic)); record("h2_df", unname(h2$parameter)); record("h2_p", h2$p.value)
record("h2_ci_low", h2$conf.int[1]); record("h2_ci_high", h2$conf.int[2])
record("h2_mean_no", h2_means$mean[1]); record("h2_mean_yes", h2_means$mean[2])
record("h2_sd_no", h2_means$sd[1]); record("h2_sd_yes", h2_means$sd[2])
record("h2_cohens_d", h2_d); record("h2_w_p", h2_w$p.value)
record("h2_w_shift", unname(h2_w$estimate))
record("h2_w_ci_low", h2_w$conf.int[1]); record("h2_w_ci_high", h2_w$conf.int[2])
record("h3_w", unname(h3$statistic)); record("h3_p", h3$p.value)
record("h3_shift", unname(h3$estimate))
record("h3_ci_low", h3$conf.int[1]); record("h3_ci_high", h3$conf.int[2])
record("h3_rank_biserial", h3_r)
record("h4_f", summary(h4_aov)[[1]][["F value"]][1]); record("h4_p_aov", summary(h4_aov)[[1]][["Pr(>F)"]][1])
record("h4_welch_f", unname(h4_welch$statistic)); record("h4_welch_p", h4_welch$p.value)
record("h4_welch_df1", unname(h4_welch$parameter[1])); record("h4_welch_df2", unname(h4_welch$parameter[2]))
record("h4_kw", unname(h4_kw$statistic)); record("h4_kw_p", h4_kw$p.value); record("h4_eta2", h4_eta2)
tk <- h4_tukey$InternetService
record("h4_tukey_fiber_dsl", tk["Fiber optic-DSL", "diff"]); record("h4_tukey_no_dsl", tk["No-DSL", "diff"])
record("h4_tukey_no_fiber", tk["No-Fiber optic", "diff"])
record("h4_df_resid", summary(h4_aov)[[1]][["Df"]][2])
record("h5_r", unname(h5_pearson$estimate)); record("h5_p", h5_pearson$p.value)
record("h5_ci_low", h5_pearson$conf.int[1]); record("h5_ci_high", h5_pearson$conf.int[2])
record("h5_rho", unname(h5_spearman$estimate)); record("h5_t", unname(h5_pearson$statistic))
record("h6_chisq", unname(h6$statistic)); record("h6_df", unname(h6$parameter)); record("h6_p", h6$p.value)
record("h6_cramers_v", h6_v); record("h6_stdres_echeck_yes", h6$stdres["Electronic check", "Yes"])
record("h7_chisq", unname(h7$statistic)); record("h7_p", h7$p.value)
record("h7_p_senior", unname(h7$estimate[1])); record("h7_p_nonsenior", unname(h7$estimate[2]))
record("h7_ci_low", h7$conf.int[1]); record("h7_ci_high", h7$conf.int[2])
record("h8_chisq", unname(h8$statistic)); record("h8_p", h8$p.value)
record("h8_rate_female", prop.table(tab_gender, 1)["Female", "Yes"])
record("h8_rate_male", prop.table(tab_gender, 1)["Male", "Yes"])
record("h8_p_holm", test_summary$p_holm[8])
record("n_rejected", sum(test_summary$decision == "Reject H0"))
record("gender_cramers_v",
       sqrt(unname(suppressWarnings(chisq.test(tab_gender, correct = FALSE))$statistic) / sum(tab_gender)))
record("share_senior", mean(telco$SeniorCitizen == "Yes"))
record("share_churners_le12", mean(telco$tenure[telco$Churn == "Yes"] <= 12))
record("levene_mc_churn_F", levene_mc_churn$`F value`[1]); record("levene_mc_churn_p", levene_mc_churn$`Pr(>F)`[1])
record("levene_tenure_churn_F", levene_tenure_churn$`F value`[1]); record("levene_tenure_churn_p", levene_tenure_churn$`Pr(>F)`[1])
record("levene_mc_internet_F", levene_mc_internet$`F value`[1]); record("levene_mc_internet_p", levene_mc_internet$`Pr(>F)`[1])
for (i in seq_len(nrow(normality))) {
  key <- paste0(normality$variable[i], "_", normality$Churn[i])
  record(paste0("skew_", key), normality$skewness[i]); record(paste0("ad_p_", key), normality$ad_p_value[i])
  record(paste0("ad_stat_", key), unname(normality$ad_statistic[i]))
}
write_metrics("05_statistical_analysis")
