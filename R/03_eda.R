# =============================================================================
# 03_eda.R
# Week 1 - Step 3: preliminary analysis of the cleaned data.
#   - outlier detection (IQR rule, z-scores, boxplots, contextual checks)
#   - normalisation / scaling (min-max and z-score)
#   - categorical encoding (factor levels, dummy and one-hot encoding)
#   - descriptive statistics, frequencies and distributions
#   - correlation analysis and association screening
# =============================================================================
source("R/00_setup.R")
load_packages(c("skimr", "corrplot"))

telco <- readRDS(file.path(paths$processed, "telco_clean.rds"))
num_vars <- c("tenure", "MonthlyCharges", "TotalCharges")
var_labels <- c(tenure = "Tenure (months)", MonthlyCharges = "Monthly charges",
                TotalCharges = "Total charges")

# ---- 3.1 Outlier detection: IQR rule ----
iqr_summary <- function(x) {
  q <- quantile(x, c(0.25, 0.75)); iqr <- q[[2]] - q[[1]]
  lower <- q[[1]] - 1.5 * iqr; upper <- q[[2]] + 1.5 * iqr
  tibble(Q1 = q[[1]], Q3 = q[[2]], IQR = iqr, lower_fence = lower, upper_fence = upper,
         min = min(x), max = max(x), n_below = sum(x < lower), n_above = sum(x > upper))
}
outlier_iqr <- map_dfr(num_vars, ~ iqr_summary(telco[[.x]]) %>% mutate(variable = .x, .before = 1))
print(outlier_iqr, width = 100)
save_table(outlier_iqr, "w1_outliers_iqr")

# ---- 3.2 Outlier detection: z-scores ----
outlier_z <- map_dfr(num_vars, function(v) {
  z <- as.numeric(scale(telco[[v]]))
  tibble(variable = v, mean = mean(telco[[v]]), sd = sd(telco[[v]]),
         min_z = min(z), max_z = max(z), n_abs_z_gt_3 = sum(abs(z) > 3))
})
outlier_z
save_table(outlier_z, "w1_outliers_zscore")

# ---- 3.3 Boxplots of the numeric variables ----
num_long <- telco %>%
  select(customerID, Churn, all_of(num_vars)) %>%
  pivot_longer(all_of(num_vars), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(var_labels[variable], levels = var_labels))

p_box <- ggplot(num_long, aes(x = "", y = value)) +
  geom_boxplot(fill = "#DCE6F0", colour = col_accent, width = 0.45,
               outlier.colour = col_accent, outlier.size = 1.2) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "No points fall outside the 1.5 x IQR whiskers for any numeric variable",
       subtitle = "Boxplots of the three numeric variables, all 7,043 customers",
       x = NULL, y = "Value") +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
save_plot(p_box, "w1_boxplots_numeric", height = 3.6)

p_box_churn <- ggplot(num_long, aes(x = Churn, y = value, fill = Churn)) +
  geom_boxplot(width = 0.5, colour = col_text, outlier.size = 0.9, alpha = 0.9) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = pal_churn) +
  labs(title = "Churned customers have shorter tenure and higher monthly charges",
       subtitle = "Numeric variables split by churn status",
       x = "Churned", y = "Value") +
  theme(legend.position = "none")
save_plot(p_box_churn, "w1_boxplots_by_churn", height = 3.8)

# Points beyond the whiskers inside each churn group (group-wise IQR rule)
group_outliers <- num_long %>%
  group_by(variable, Churn) %>%
  summarise(upper_fence = quantile(value, 0.75) + 1.5 * IQR(value),
            n_above = sum(value > upper_fence), n = n(), .groups = "drop")
group_outliers
save_table(group_outliers, "w1_outliers_by_churn")

# ---- 3.4 Contextual check: TotalCharges against tenure x MonthlyCharges ----
# TotalCharges should be roughly tenure x current MonthlyCharges. Large gaps
# are unusual records worth checking even though no single variable is extreme.
telco_chk <- telco %>%
  mutate(expected_total = tenure * MonthlyCharges,
         charge_gap     = TotalCharges - expected_total,
         gap_pct        = if_else(expected_total > 0, 100 * charge_gap / expected_total, 0))
gap_fence <- iqr_summary(telco_chk$charge_gap)
gap_fence
summary(telco_chk$gap_pct)
telco_chk %>%
  filter(charge_gap < gap_fence$lower_fence | charge_gap > gap_fence$upper_fence) %>%
  summarise(n_flagged = n(), median_tenure = median(tenure),
            median_abs_gap_pct = median(abs(gap_pct)), max_abs_gap_pct = max(abs(gap_pct)))
cor(telco_chk$charge_gap, telco_chk$tenure)

telco_chk <- telco_chk %>%
  mutate(gap_flag = if_else(charge_gap < gap_fence$lower_fence | charge_gap > gap_fence$upper_fence,
                            "Outside IQR fences", "Within fences"))
p_gap <- ggplot(telco_chk, aes(x = tenure, y = charge_gap, colour = gap_flag)) +
  geom_hline(yintercept = c(gap_fence$lower_fence, gap_fence$upper_fence),
             linetype = "dashed", colour = col_muted) +
  geom_point(alpha = 0.45, size = 0.9, position = position_jitter(width = 0.3, height = 0, seed = SEED)) +
  scale_colour_manual(values = c("Outside IQR fences" = col_accent, "Within fences" = "#B8C0C8"),
                      name = NULL) +
  labs(title = sprintf("%s customers have a charge gap outside the IQR fences",
                       comma(sum(telco_chk$gap_flag == "Outside IQR fences"))),
       subtitle = "Charge gap = recorded TotalCharges - tenure x current MonthlyCharges; dashed lines = fences",
       x = "Tenure (months)", y = "Charge gap")
save_plot(p_gap, "w1_charge_gap_outliers", width = 7, height = 4.2)

# ---- 3.5 Within-group check: MonthlyCharges by internet service ----
mc_by_internet <- telco %>%
  group_by(InternetService) %>%
  summarise(n = n(), min = min(MonthlyCharges), median = median(MonthlyCharges),
            max = max(MonthlyCharges),
            lower_fence = quantile(MonthlyCharges, 0.25) - 1.5 * IQR(MonthlyCharges),
            upper_fence = quantile(MonthlyCharges, 0.75) + 1.5 * IQR(MonthlyCharges),
            n_outside = sum(MonthlyCharges < lower_fence | MonthlyCharges > upper_fence))
mc_by_internet
save_table(mc_by_internet, "w1_monthlycharges_by_internet")

p_mc_int <- ggplot(telco, aes(x = InternetService, y = MonthlyCharges)) +
  geom_boxplot(fill = "#DCE6F0", colour = col_accent, width = 0.5,
               outlier.colour = col_accent, outlier.size = 1.2) +
  labs(title = "Monthly charges depend strongly on the type of internet service",
       subtitle = "Points beyond the whiskers are flagged within each service group",
       x = "Internet service", y = "Monthly charges")
save_plot(p_mc_int, "w1_monthlycharges_by_internet", width = 6.5, height = 4)

# ---- 3.6 Outlier treatment decision ----
outlier_decision <- tibble(
  check = c("tenure (IQR, z)", "MonthlyCharges (IQR, z)", "TotalCharges (IQR, z)",
            "TotalCharges vs tenure x MonthlyCharges", "MonthlyCharges within internet type"),
  flagged = c(outlier_iqr$n_below[1] + outlier_iqr$n_above[1],
              outlier_iqr$n_below[2] + outlier_iqr$n_above[2],
              outlier_iqr$n_below[3] + outlier_iqr$n_above[3],
              sum(telco_chk$charge_gap < gap_fence$lower_fence |
                    telco_chk$charge_gap > gap_fence$upper_fence),
              sum(mc_by_internet$n_outside)),
  treatment = "Retained - values are plausible billing records, not entry errors"
)
outlier_decision
save_table(outlier_decision, "w1_outlier_decision")

# ---- 3.7 Normalisation and scaling ----
min_max <- function(x) (x - min(x)) / (max(x) - min(x))
telco_scaled <- telco %>%
  mutate(across(all_of(num_vars), min_max, .names = "{.col}_minmax"),
         across(all_of(num_vars), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

scaling_summary <- telco_scaled %>%
  select(starts_with(num_vars)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "value") %>%
  group_by(column) %>%
  summarise(mean = mean(value), sd = sd(value), min = min(value), max = max(value)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(match(column, c(num_vars, paste0(num_vars, "_minmax"), paste0(num_vars, "_z"))))
print(scaling_summary, n = Inf)
save_table(scaling_summary, "w1_scaling_summary")

scaling_examples <- telco_scaled %>%
  select(customerID, MonthlyCharges, MonthlyCharges_minmax, MonthlyCharges_z,
         tenure, tenure_minmax, tenure_z) %>%
  slice(1:6) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
scaling_examples
save_table(scaling_examples, "w1_scaling_examples")

# Scaling changes the units, not the shape of the distribution
p_scale <- telco_scaled %>%
  select(MonthlyCharges, MonthlyCharges_minmax, MonthlyCharges_z) %>%
  pivot_longer(everything(), names_to = "version", values_to = "value") %>%
  mutate(version = factor(version,
                          levels = c("MonthlyCharges", "MonthlyCharges_minmax", "MonthlyCharges_z"),
                          labels = c("Original", "Min-max [0, 1]", "Z-score"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = col_accent, colour = "white", linewidth = 0.2) +
  facet_wrap(~ version, scales = "free_x") +
  labs(title = "Scaling changes the units of MonthlyCharges but not the shape",
       subtitle = "Same 7,043 values shown on three scales", x = "Value", y = "Customers")
save_plot(p_scale, "w1_scaling_before_after", height = 3.4)

# ---- 3.8 Categorical encoding ----
cat_vars <- names(telco)[vapply(telco, is.factor, logical(1))]
encoding_summary <- tibble(
  variable      = cat_vars,
  n_levels      = vapply(telco[cat_vars], nlevels, integer(1)),
  reference     = vapply(telco[cat_vars], function(x) levels(x)[1], character(1)),
  dummy_columns = n_levels - 1L
)
print(encoding_summary, n = Inf)
save_table(encoding_summary, "w1_encoding_summary")

# Dummy (treatment) encoding: k - 1 indicator columns per factor
predictors <- telco %>% select(-customerID, -Churn)
x_design <- model.matrix(~ ., data = predictors)     # includes the intercept column
x_dummy  <- x_design[, -1]
dim(x_dummy)
head(colnames(x_dummy), 12)

# One-hot encoding of a single factor keeps all k columns
head(model.matrix(~ Contract - 1, data = telco), 4)

# The add-on service fields repeat information held in InternetService:
# "No internet service" appears exactly when InternetService == "No".
# Likewise "No phone service" in MultipleLines mirrors PhoneService == "No".
table(telco$InternetService, telco$OnlineSecurity)
table(telco$PhoneService, telco$MultipleLines)
# Rank of the regression design matrix (with intercept) reveals redundant columns
c(columns = ncol(x_design), matrix_rank = qr(x_design)$rank,
  redundant = ncol(x_design) - qr(x_design)$rank)

encoded <- bind_cols(select(telco, customerID, Churn), as_tibble(x_dummy))
write_csv(encoded, file.path(paths$processed, "telco_encoded.csv"))

# ---- 3.9 Descriptive statistics ----
skew <- function(x) mean((x - mean(x))^3) / sd(x)^3
describe <- function(x) {
  tibble(n = length(x), mean = mean(x), median = median(x), sd = sd(x),
         min = min(x), q1 = quantile(x, 0.25, names = FALSE),
         q3 = quantile(x, 0.75, names = FALSE), max = max(x), iqr = IQR(x),
         skewness = skew(x))
}
desc_numeric <- map_dfr(num_vars, ~ describe(telco[[.x]]) %>% mutate(variable = .x, .before = 1)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(desc_numeric, width = 100)
save_table(desc_numeric, "w1_descriptive_numeric")

desc_by_churn <- num_long %>%
  group_by(variable, Churn) %>%
  summarise(n = n(), mean = mean(value), median = median(value), sd = sd(value)) %>%
  mutate(across(c(mean, median, sd), ~ round(.x, 2)))
desc_by_churn
save_table(desc_by_churn, "w1_descriptive_by_churn")

# Base R equivalents on individual variables
mean(telco$MonthlyCharges); median(telco$MonthlyCharges); sd(telco$MonthlyCharges)
quantile(telco$tenure, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))

skim_out <- skimr::skim(telco %>% select(-customerID))
print(skim_out)

# ---- 3.10 Category frequencies and churn rate by category ----
table(telco$Contract)
table(telco$Contract, telco$Churn)
round(prop.table(table(telco$Contract, telco$Churn), margin = 1), 3)

cat_freq <- map_dfr(setdiff(cat_vars, "Churn"), function(v) {
  telco %>%
    group_by(level = .data[[v]]) %>%
    summarise(customers = n(), churned = sum(Churn == "Yes")) %>%
    mutate(variable = v, level = as.character(level), share = customers / sum(customers),
           churn_rate = churned / customers) %>%
    select(variable, level, customers, churned, share, churn_rate)
})
cat_freq %>%
  filter(variable %in% c("Contract", "InternetService", "PaymentMethod", "SeniorCitizen")) %>%
  mutate(across(c(share, churn_rate), ~ round(.x, 3))) %>%
  print(n = Inf)
save_table(cat_freq, "w1_category_frequencies")

tenure_bands <- telco %>%
  mutate(tenure_band = cut(tenure, breaks = c(-1, 12, 24, 36, 48, 60, 72),
                           labels = c("0-12", "13-24", "25-36", "37-48", "49-60", "61-72"))) %>%
  group_by(tenure_band) %>%
  summarise(customers = n(), churn_rate = mean(Churn == "Yes"))
tenure_bands
save_table(tenure_bands, "w1_churn_by_tenure_band")

# ---- 3.11 Distribution plots ----
p_hist <- ggplot(num_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = col_accent, colour = "white", linewidth = 0.2) +
  facet_wrap(~ variable, scales = "free") +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Tenure is U-shaped, monthly charges bimodal and total charges right-skewed",
       subtitle = "Histograms of the numeric variables (30 bins)", x = "Value", y = "Customers")
save_plot(p_hist, "w1_numeric_histograms", height = 3.5)

# ---- 3.12 Correlation analysis ----
cor_data <- telco %>%
  transmute(tenure, MonthlyCharges, TotalCharges,
            AddOnServices = rowSums(across(c(OnlineSecurity, OnlineBackup, DeviceProtection,
                                             TechSupport, StreamingTV, StreamingMovies),
                                           ~ .x == "Yes")),
            ChurnYes = as.integer(Churn == "Yes"))
cor_pearson  <- cor(cor_data, method = "pearson")
cor_spearman <- cor(cor_data, method = "spearman")
round(cor_pearson, 3)
round(cor_spearman, 3)
save_table(as_tibble(round(cor_pearson, 4), rownames = "variable"), "w1_correlation_pearson")
save_table(as_tibble(round(cor_spearman, 4), rownames = "variable"), "w1_correlation_spearman")

ragg::agg_png(file.path(paths$figures, "w1_correlation_matrix.png"),
              width = 5.6, height = 4.8, units = "in", res = 300, background = "white")
corrplot::corrplot(cor_pearson, method = "color", type = "lower", diag = TRUE,
                   col = colorRampPalette(c("#8B3A3A", "#F7F7F7", col_accent))(200),
                   addCoef.col = "black", number.cex = 0.8, number.digits = 2,
                   tl.col = col_text, tl.srt = 45, tl.cex = 0.85, cl.cex = 0.75,
                   mar = c(0, 0, 0, 0))
invisible(dev.off())

# Association of each categorical variable with churn (Cramer's V)
cramers_v <- map_dfr(setdiff(cat_vars, "Churn"), function(v) {
  tab <- table(telco[[v]], telco$Churn)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  tibble(variable = v, chi_sq = unname(chi$statistic),
         cramers_v = sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1))))
}) %>% arrange(desc(cramers_v))
cramers_v %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>% print(n = Inf)
save_table(cramers_v, "w1_cramers_v")

p_cv <- ggplot(cramers_v, aes(x = cramers_v, y = fct_reorder(variable, cramers_v))) +
  geom_col(fill = col_accent, width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", cramers_v)), hjust = -0.15, size = 3,
            colour = col_text) +
  scale_x_continuous(limits = c(0, max(cramers_v$cramers_v) * 1.12),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Contract type shows the strongest association with churn",
       subtitle = "Cramer's V between each categorical variable and churn (0 = none, 1 = perfect)",
       x = "Cramer's V", y = NULL)
save_plot(p_cv, "w1_cramers_v", height = 4.6)

# ---- 3.13 Metrics for the report ----
get_desc <- function(v, col) desc_numeric[[col]][desc_numeric$variable == v]
get_by <- function(v, ch, col) desc_by_churn[[col]][desc_by_churn$variable == var_labels[[v]] &
                                                       desc_by_churn$Churn == ch]
get_rate <- function(v, lvl) cat_freq$churn_rate[cat_freq$variable == v & cat_freq$level == lvl]
get_cnt <- function(v, lvl) cat_freq$customers[cat_freq$variable == v & cat_freq$level == lvl]

for (v in num_vars) {
  for (s in c("mean", "median", "sd", "min", "max", "q1", "q3", "iqr", "skewness")) {
    record(paste0("desc_", v, "_", s), get_desc(v, s))
  }
  record(paste0("iqr_upper_", v), outlier_iqr$upper_fence[outlier_iqr$variable == v])
  record(paste0("iqr_lower_", v), outlier_iqr$lower_fence[outlier_iqr$variable == v])
  record(paste0("max_abs_z_", v), max(abs(outlier_z$min_z[outlier_z$variable == v]),
                                      abs(outlier_z$max_z[outlier_z$variable == v])))
  for (ch in c("No", "Yes")) {
    record(paste0("by_churn_", v, "_mean_", ch), get_by(v, ch, "mean"))
    record(paste0("by_churn_", v, "_median_", ch), get_by(v, ch, "median"))
  }
}
record("n_iqr_outliers_numeric", sum(outlier_iqr$n_below + outlier_iqr$n_above))
record("n_z_outliers_numeric", sum(outlier_z$n_abs_z_gt_3))
record("n_group_outliers_by_churn", sum(group_outliers$n_above))
go <- function(v, ch, col) group_outliers[[col]][group_outliers$variable == var_labels[[v]] & group_outliers$Churn == ch]
record("group_out_tenure_Yes", go("tenure", "Yes", "n_above")); record("group_fence_tenure_Yes", go("tenure", "Yes", "upper_fence"))
record("group_out_total_Yes", go("TotalCharges", "Yes", "n_above")); record("group_fence_total_Yes", go("TotalCharges", "Yes", "upper_fence"))
record("gap_flagged_median_tenure",
       median(telco_chk$tenure[telco_chk$gap_flag == "Outside IQR fences"]))
nointernet <- filter(telco, InternetService == "No")
record("mc_nointernet_median_multi_Yes", median(nointernet$MonthlyCharges[nointernet$MultipleLines == "Yes"]))
record("mc_nointernet_median_multi_No", median(nointernet$MonthlyCharges[nointernet$MultipleLines == "No"]))
record("mc_nointernet_flagged_share_multi",
       mean(nointernet$MultipleLines[nointernet$MonthlyCharges > mc_by_internet$upper_fence[mc_by_internet$InternetService == "No"]] == "Yes"))
record("gap_n_flagged", outlier_decision$flagged[4])
record("gap_lower_fence", gap_fence$lower_fence)
record("gap_upper_fence", gap_fence$upper_fence)
record("gap_min", gap_fence$min)
record("gap_max", gap_fence$max)
record("gap_cor_tenure", cor(telco_chk$charge_gap, telco_chk$tenure))
record("gap_pct_median_abs_flagged",
       median(abs(telco_chk$gap_pct[telco_chk$charge_gap < gap_fence$lower_fence |
                                      telco_chk$charge_gap > gap_fence$upper_fence])))
record("n_mc_internet_outside", sum(mc_by_internet$n_outside))
for (lvl in levels(telco$InternetService)) {
  key <- gsub("[^A-Za-z]", "", lvl)
  record(paste0("mc_internet_median_", key), mc_by_internet$median[mc_by_internet$InternetService == lvl])
  record(paste0("mc_internet_outside_", key), mc_by_internet$n_outside[mc_by_internet$InternetService == lvl])
}
record("n_dummy_columns", ncol(x_dummy))
record("design_cols", ncol(x_design))
record("design_rank", qr(x_design)$rank)
record("n_redundant_dummies", ncol(x_design) - qr(x_design)$rank)
record("n_factor_vars", length(cat_vars))
for (lvl in levels(telco$Contract)) {
  key <- gsub("[^A-Za-z]", "", lvl)
  record(paste0("churn_rate_contract_", key), get_rate("Contract", lvl))
  record(paste0("n_contract_", key), get_cnt("Contract", lvl))
}
for (lvl in levels(telco$InternetService)) {
  key <- gsub("[^A-Za-z]", "", lvl)
  record(paste0("churn_rate_internet_", key), get_rate("InternetService", lvl))
  record(paste0("n_internet_", key), get_cnt("InternetService", lvl))
}
for (lvl in levels(telco$PaymentMethod)) {
  key <- gsub("[^A-Za-z]", "", lvl)
  record(paste0("churn_rate_payment_", key), get_rate("PaymentMethod", lvl))
}
record("w1_share_senior", cat_freq$share[cat_freq$variable == "SeniorCitizen" & cat_freq$level == "Yes"])
record("w1_share_m2m", cat_freq$share[cat_freq$variable == "Contract" & cat_freq$level == "Month-to-month"])
record("w1_share_fibre", cat_freq$share[cat_freq$variable == "InternetService" & cat_freq$level == "Fiber optic"])
record("churn_rate_senior_Yes", get_rate("SeniorCitizen", "Yes"))
record("churn_rate_senior_No", get_rate("SeniorCitizen", "No"))
record("churn_rate_tenure_0_12", tenure_bands$churn_rate[1])
record("churn_rate_tenure_61_72", tenure_bands$churn_rate[6])
record("n_tenure_0_12", tenure_bands$customers[1])
cor_names <- colnames(cor_pearson)            # fixed column order, locale independent
for (i in seq_along(cor_names)) for (j in seq_along(cor_names)) if (i < j) {
  record(paste0("pearson_", cor_names[i], "_", cor_names[j]), cor_pearson[i, j])
  record(paste0("spearman_", cor_names[i], "_", cor_names[j]), cor_spearman[i, j])
}
for (i in seq_len(nrow(cramers_v))) record(paste0("cramers_v_", cramers_v$variable[i]), cramers_v$cramers_v[i])
record("cramers_v_top_var", cramers_v$variable[1])
write_metrics("03_eda")
