# =============================================================================
# 01_data_import.R
# Week 1 - Step 1: obtain the IBM Telco Customer Churn data and inspect the raw
# file before anything is changed: dimensions, names, head/tail, str(),
# summary(), data types, the target variable and duplicate records.
# =============================================================================
source("R/00_setup.R")

# ---- 1.1 Download the raw data (only if it is not already present) ----
if (!file.exists(paths$raw_data)) {
  dir.create(dirname(paths$raw_data), recursive = TRUE, showWarnings = FALSE)
  download.file(DATA_URL, destfile = paths$raw_data, mode = "wb")
}
file.size(paths$raw_data)                     # bytes on disk
unname(tools::md5sum(paths$raw_data))         # checksum used in reproducibility.md

# ---- 1.2 Import the raw CSV ----
# read.csv() with default settings: text stays as character (no factors yet)
# and R guesses a type for each column from its values.
telco_raw <- read.csv(paths$raw_data, stringsAsFactors = FALSE)
dim(telco_raw)
names(telco_raw)

# ---- 1.3 First and last records ----
head(telco_raw, 5)
tail(telco_raw, 5)
# Compact view of eight key columns (easier to read than the 21-column output)
key_cols <- c("customerID", "gender", "tenure", "Contract", "InternetService",
              "MonthlyCharges", "TotalCharges", "Churn")
head(telco_raw[, key_cols], 5)
tail(telco_raw[, key_cols], 5)

# ---- 1.4 Structure of the raw data: str() ----
str(telco_raw)

# ---- 1.5 Summary of the raw data: summary() ----
summary(telco_raw)

# ---- 1.6 Data type check ----
type_check <- tibble(
  variable = names(telco_raw),
  r_class  = vapply(telco_raw, function(x) class(x)[1], character(1)),
  n_unique = vapply(telco_raw, n_distinct, integer(1)),
  example  = vapply(telco_raw, function(x) as.character(x[1]), character(1))
)
print(type_check, n = Inf)
save_table(type_check, "w1_raw_type_check")

# TotalCharges was read as numeric but contains NA values. Re-read the column
# as plain text to see what the source file actually holds in those rows.
colSums(is.na(telco_raw))[colSums(is.na(telco_raw)) > 0]
tc_text     <- read.csv(paths$raw_data, colClasses = "character")$TotalCharges
non_numeric <- tc_text[is.na(telco_raw$TotalCharges)]
length(non_numeric)
table(sprintf("'%s' (nchar = %d)", non_numeric, nchar(non_numeric)))
# Only these whitespace-only entries fail numeric conversion
sum(is.na(suppressWarnings(as.numeric(tc_text))))

# SeniorCitizen is stored as 0/1 while every other Yes/No field uses text labels
table(telco_raw$SeniorCitizen)

# ---- 1.7 Duplicate checks ----
n_dup_rows    <- sum(duplicated(telco_raw))                       # identical rows
n_dup_ids     <- sum(duplicated(telco_raw$customerID))            # repeated IDs
n_dup_profile <- sum(duplicated(telco_raw[, names(telco_raw) != "customerID"]))
c(duplicate_rows = n_dup_rows, duplicate_ids = n_dup_ids,
  duplicate_profiles_excl_id = n_dup_profile)

# Inspect records that share every attribute apart from customerID
dup_profiles <- telco_raw %>%
  group_by(across(-customerID)) %>%
  filter(n() > 1) %>%
  ungroup()
nrow(dup_profiles)
dup_profiles %>%
  count(tenure, Contract, InternetService, PhoneService, MonthlyCharges, name = "records") %>%
  arrange(desc(records)) %>%
  print(n = 10)
summary(dup_profiles$tenure)
save_table(dup_profiles, "w1_duplicate_profiles")

# ---- 1.8 Target variable ----
churn_counts <- table(telco_raw$Churn)
churn_counts
round(prop.table(churn_counts), 4)

# ---- 1.9 Data dictionary ----
data_dictionary <- tibble(
  variable = names(telco_raw),
  description = c(
    "Unique customer identifier",
    "Customer gender (Female, Male)",
    "Whether the customer is a senior citizen (1 = yes, 0 = no)",
    "Whether the customer has a partner",
    "Whether the customer has dependents",
    "Number of months the customer has been with the company",
    "Whether the customer has a phone service",
    "Whether the customer has multiple lines (Yes, No, No phone service)",
    "Internet service type (DSL, Fiber optic, No)",
    "Online security add-on (Yes, No, No internet service)",
    "Online backup add-on (Yes, No, No internet service)",
    "Device protection add-on (Yes, No, No internet service)",
    "Technical support add-on (Yes, No, No internet service)",
    "Streaming TV add-on (Yes, No, No internet service)",
    "Streaming movies add-on (Yes, No, No internet service)",
    "Contract term (Month-to-month, One year, Two year)",
    "Whether the customer uses paperless billing",
    "Payment method (four categories)",
    "Amount currently charged to the customer each month",
    "Total amount charged to the customer over their tenure",
    "Target: whether the customer left in the last month (Yes, No)"
  ),
  raw_type = type_check$r_class,
  n_unique = type_check$n_unique,
  role = c("Identifier", rep("Predictor", 19), "Target")
)
print(select(data_dictionary, variable, role, description), n = Inf, width = 100)
save_table(data_dictionary, "w1_data_dictionary")

# ---- 1.10 Save the raw snapshot for the cleaning step ----
saveRDS(telco_raw, file.path(paths$processed, "telco_raw.rds"))

# Numbers quoted in the report
record("r_version", paste(R.version$major, R.version$minor, sep = "."))
record("raw_file_bytes", file.size(paths$raw_data))
record("raw_md5", unname(tools::md5sum(paths$raw_data)))
record("n_rows", nrow(telco_raw))
record("n_cols", ncol(telco_raw))
record("n_character_cols", sum(type_check$r_class == "character"))
record("n_numeric_cols", sum(type_check$r_class %in% c("numeric", "integer")))
record("n_totalcharges_non_numeric", length(non_numeric))
record("n_dup_rows", n_dup_rows)
record("n_dup_ids", n_dup_ids)
record("n_dup_profiles", n_dup_profile)
record("n_dup_profile_records", nrow(dup_profiles))
record("dup_profile_tenure_max", max(dup_profiles$tenure))
record("dup_profile_tenure_median", median(dup_profiles$tenure))
record("dup_profile_n_nointernet", sum(dup_profiles$InternetService == "No"))
record("dup_profile_n_m2m", sum(dup_profiles$Contract == "Month-to-month"))
record("n_churn_yes", unname(churn_counts["Yes"]))
record("n_churn_no", unname(churn_counts["No"]))
record("churn_rate", unname(churn_counts["Yes"] / sum(churn_counts)))
write_metrics("01_data_import")
