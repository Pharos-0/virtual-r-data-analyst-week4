# =============================================================================
# 02_data_cleaning.R
# Week 1 - Step 2: clean the raw data.
#   - whitespace and label consistency
#   - missing values (detection, pattern, treatment, before/after)
#   - duplicate treatment
#   - data type conversion (character -> numeric, 0/1 -> factor, text -> factor)
#   - validation of the cleaned table
# Output: data/processed/telco_clean.rds and telco_clean.csv
# =============================================================================
source("R/00_setup.R")
load_packages("janitor")

raw_file <- file.path(paths$processed, "telco_raw.rds")
if (!file.exists(raw_file)) source("R/01_data_import.R")
telco_raw <- readRDS(raw_file)
n_raw <- nrow(telco_raw)

# ---- 2.1 Whitespace and label consistency ----
chr_cols <- names(telco_raw)[vapply(telco_raw, is.character, logical(1))]
ws_issues <- vapply(telco_raw[chr_cols],
                    function(x) sum(x != str_trim(x) | str_detect(x, "\\s{2,}")),
                    integer(1))
ws_issues[ws_issues > 0]

telco <- telco_raw %>% mutate(across(all_of(chr_cols), str_trim))

# Distinct labels of every categorical field (checks spelling / case variants)
cat_cols <- setdiff(chr_cols, c("customerID", "TotalCharges"))
label_check <- tibble(
  variable = cat_cols,
  n_labels = vapply(telco[cat_cols], n_distinct, integer(1)),
  labels   = vapply(telco[cat_cols], function(x) paste(sort(unique(x)), collapse = " | "),
                    character(1))
)
print(label_check, n = Inf, width = 100)
save_table(label_check, "w1_label_check")

# ---- 2.2 Missing values before cleaning ----
# Empty strings are counted as missing together with NA values. (The blank
# TotalCharges entries were already converted to NA by read.csv.)
count_missing <- function(df) {
  tibble(variable  = names(df),
         n_missing = vapply(df, function(x) sum(is.na(x) | (is.character(x) & x == "")),
                            integer(1))) %>%
    mutate(pct_missing = round(100 * n_missing / nrow(df), 3))
}
missing_before <- count_missing(telco)
print(missing_before, n = Inf)
save_table(missing_before, "w1_missing_before")
record("n_missing_total_before", sum(missing_before$n_missing))
record("n_vars_with_missing_before", sum(missing_before$n_missing > 0))

# ---- 2.3 Data type conversion ----
# Numeric columns: confirm the classes R assigned on import are appropriate
sapply(telco[c("tenure", "MonthlyCharges", "TotalCharges")], class)

# SeniorCitizen is a Yes/No attribute stored as 0/1 -> labelled factor
table(telco$SeniorCitizen)
telco <- telco %>%
  mutate(SeniorCitizen = factor(SeniorCitizen, levels = c(0, 1), labels = c("No", "Yes")))
table(telco$SeniorCitizen)

# ---- 2.4 Missing-value pattern ----
colSums(is.na(telco))[colSums(is.na(telco)) > 0]
telco %>%
  filter(is.na(TotalCharges)) %>%
  select(customerID, tenure, MonthlyCharges, TotalCharges, Contract, Churn)

# Are the missing totals linked to tenure = 0 (customers not yet billed)?
table(tenure_is_zero = telco$tenure == 0, total_missing = is.na(telco$TotalCharges))

# For billed customers TotalCharges should track tenure x MonthlyCharges
cor_tc_expected <- with(telco, cor(TotalCharges, tenure * MonthlyCharges, use = "complete.obs"))
cor_tc_expected

# ---- 2.5 Missing-value treatment ----
# Options considered for the 11 missing TotalCharges values
obs_tc <- telco$TotalCharges[!is.na(telco$TotalCharges)]
treatment_options <- tibble(
  method        = c("Listwise deletion", "Mean imputation", "Median imputation",
                    "Set to 0 because tenure = 0 (chosen)"),
  rows_retained = c(sum(!is.na(telco$TotalCharges)), rep(nrow(telco), 3)),
  value_filled  = c(NA, round(mean(obs_tc), 2), round(median(obs_tc), 2), 0)
)
treatment_options
save_table(treatment_options, "w1_missing_treatment_options")

telco <- telco %>%
  mutate(TotalCharges = if_else(is.na(TotalCharges) & tenure == 0, 0, TotalCharges))

missing_after <- count_missing(telco)
missing_compare <- missing_before %>%
  select(variable, missing_before = n_missing) %>%
  left_join(missing_after %>% select(variable, missing_after = n_missing), by = "variable") %>%
  filter(missing_before > 0 | missing_after > 0)
missing_compare
sum(is.na(telco))
save_table(missing_compare, "w1_missing_before_after")

# ---- 2.6 Duplicate treatment ----
# No exact duplicate rows and no repeated IDs were found in 01_data_import.R.
# Records sharing a profile apart from customerID are different customers, so
# they are kept.
janitor::get_dupes(telco, -customerID) %>%
  select(dupe_count, customerID, tenure, Contract, InternetService, MonthlyCharges,
         TotalCharges) %>%
  head(8)
n_before_dedup <- nrow(telco)
telco <- distinct(telco)                 # would remove exact duplicate rows only
c(rows_before = n_before_dedup, rows_after = nrow(telco))

# ---- 2.7 Convert categorical variables to factors ----
yes_no_vars  <- c("Partner", "Dependents", "PhoneService", "PaperlessBilling", "Churn")
service_vars <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport",
                  "StreamingTV", "StreamingMovies")

telco <- telco %>%
  mutate(
    gender          = factor(gender, levels = c("Female", "Male")),
    across(all_of(yes_no_vars), ~ factor(.x, levels = c("No", "Yes"))),
    MultipleLines   = factor(MultipleLines, levels = c("No", "Yes", "No phone service")),
    InternetService = factor(InternetService, levels = c("DSL", "Fiber optic", "No")),
    across(all_of(service_vars), ~ factor(.x, levels = c("No", "Yes", "No internet service"))),
    Contract        = factor(Contract, levels = c("Month-to-month", "One year", "Two year")),
    PaymentMethod   = factor(PaymentMethod,
                             levels = c("Bank transfer (automatic)", "Credit card (automatic)",
                                        "Electronic check", "Mailed check"))
  )

# Converting to factors must not create NA values (would indicate unmatched labels)
sum(is.na(telco))
sapply(telco, class)

# ---- 2.8 Cleaned data: str() and summary() ----
str(telco)
summary(telco)

# ---- 2.9 Validation checks ----
stopifnot(
  nrow(telco) == n_raw,
  !anyNA(telco),
  n_distinct(telco$customerID) == nrow(telco),
  all(telco$tenure >= 0), all(telco$MonthlyCharges > 0), all(telco$TotalCharges >= 0)
)
cat("Validation passed:", nrow(telco), "rows x", ncol(telco), "columns, no missing values\n")

# ---- 2.10 Save the cleaned data ----
saveRDS(telco, file.path(paths$processed, "telco_clean.rds"))
write_csv(telco, file.path(paths$processed, "telco_clean.csv"))

record("n_ws_issue_values", sum(ws_issues))
record("n_ws_issue_vars", sum(ws_issues > 0))
record("n_missing_totalcharges", sum(missing_before$n_missing[missing_before$variable == "TotalCharges"]))
record("pct_missing_totalcharges", missing_before$pct_missing[missing_before$variable == "TotalCharges"])
record("tc_mean_observed", mean(obs_tc))
record("tc_median_observed", median(obs_tc))
record("cor_tc_expected", cor_tc_expected)
record("n_tenure_zero", sum(telco$tenure == 0))
record("n_missing_after", sum(is.na(telco)))
record("n_rows_clean", nrow(telco))
record("n_cols_clean", ncol(telco))
record("n_factor_cols", sum(vapply(telco, is.factor, logical(1))))
write_metrics("02_data_cleaning")
