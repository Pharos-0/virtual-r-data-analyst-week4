# =============================================================================
# 06_predictive_model.R
# Week 3 - Part B: churn classification models.
#   - modelling data, leakage review and redundant-level recoding
#   - stratified 80/20 train/test split (seed 42)
#   - multicollinearity check (VIF) on the training data
#   - 10-fold stratified cross-validation; identical folds for every model;
#     centring/scaling learned inside each training fold (caret preProcess)
#   - Model 1: logistic regression; Model 2: random forest (ranger)
#   - test-set evaluation: confusion matrix, accuracy, precision, recall,
#     F1, ROC-AUC; threshold analysis on out-of-fold predictions
#   - diagnostics: calibration, Hosmer-Lemeshow, binned residuals, influence
#   - interpretation: odds ratios, permutation importance, cumulative gains
# =============================================================================
source("R/00_setup.R")
load_packages(c("caret", "pROC", "ranger", "car", "ResourceSelection", "broom"))

telco <- as_tibble(readRDS(file.path(paths$processed, "telco_clean.rds")))

# ---- 6.1 Modelling data and leakage review ----
# customerID is an identifier with no predictive meaning, so it is removed.
# The remaining fields describe the account at the time of the snapshot; none
# is computed from the churn outcome. "No internet service" / "No phone service"
# repeat information held in InternetService / PhoneService (Week 1 rank check),
# so they are recoded to "No" to avoid perfectly collinear dummy columns.
redundant_vars <- c("MultipleLines", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                    "TechSupport", "StreamingTV", "StreamingMovies")
model_df <- telco %>%
  select(-customerID) %>%
  mutate(across(all_of(redundant_vars),
                ~ factor(if_else(as.character(.x) %in% c("No internet service", "No phone service"),
                                 "No", as.character(.x)), levels = c("No", "Yes"))),
         Churn = factor(Churn, levels = c("Yes", "No")))   # "Yes" first = positive class
dim(model_df)
str(model_df[, c("OnlineSecurity", "MultipleLines", "Churn")])

# ---- 6.2 Stratified train/test split ----
set.seed(SEED)
train_idx <- createDataPartition(model_df$Churn, p = 0.8, list = FALSE)
train <- model_df[train_idx, ]
test  <- model_df[-train_idx, ]
c(train = nrow(train), test = nrow(test))
rbind(train = prop.table(table(train$Churn)), test = prop.table(table(test$Churn)))

# ---- 6.3 Multicollinearity check on the training data ----
train_glm <- train %>% mutate(churn_yes = as.integer(Churn == "Yes")) %>% select(-Churn)
glm_all <- glm(churn_yes ~ ., data = train_glm, family = binomial)
vif_all <- car::vif(glm_all)
round(vif_all[order(-vif_all[, "GVIF"]), ], 2)

# MonthlyCharges is almost fully explained by the services a customer takes
mc_r2 <- summary(lm(MonthlyCharges ~ PhoneService + MultipleLines + InternetService +
                      OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport +
                      StreamingTV + StreamingMovies, data = train))$r.squared
mc_r2
cor(train$TotalCharges, train$tenure * train$MonthlyCharges)

# Interpretable logistic model: drop MonthlyCharges and TotalCharges
glm_lr <- glm(churn_yes ~ . - MonthlyCharges - TotalCharges, data = train_glm, family = binomial)
vif_lr <- car::vif(glm_lr)
round(vif_lr[order(-vif_lr[, "GVIF"]), ], 2)
max(vif_lr[, "GVIF^(1/(2*Df))"])

# ---- 6.4 Cross-validation set-up ----
set.seed(SEED)
cv_index <- createFolds(train$Churn, k = 10, returnTrain = TRUE)   # stratified folds
sapply(cv_index, function(i) round(mean(train$Churn[-i] == "Yes"), 3))  # churn rate per held-out fold
ctrl <- trainControl(method = "cv", number = 10, index = cv_index,
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     savePredictions = "final")

# ---- 6.5 Model 1: logistic regression (10-fold CV) ----
set.seed(SEED)
model_lr <- train(Churn ~ . - MonthlyCharges - TotalCharges, data = train,
                  method = "glm", family = binomial,
                  preProcess = c("center", "scale"),     # learned within each fold
                  metric = "ROC", trControl = ctrl)
model_lr

# Sensitivity check: the same model with all predictors (collinear version)
set.seed(SEED)
model_lr_all <- train(Churn ~ ., data = train, method = "glm", family = binomial,
                      preProcess = c("center", "scale"), metric = "ROC", trControl = ctrl)
model_lr_all$results[, c("ROC", "Sens", "Spec")]

# ---- 6.6 Model 1 summary and odds ratios ----
summary(glm_lr)
or_table <- broom::tidy(glm_lr, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate))
print(or_table %>% mutate(across(where(is.numeric), ~ signif(.x, 3))), n = Inf, width = 100)
save_table(or_table, "w3_logistic_odds_ratios")
c(null_deviance = glm_lr$null.deviance, residual_deviance = glm_lr$deviance, AIC = AIC(glm_lr))
mcfadden_r2 <- 1 - glm_lr$deviance / glm_lr$null.deviance
mcfadden_r2

# ---- 6.7 Model 2: random forest (ranger, 10-fold CV with tuning) ----
rf_grid <- expand.grid(mtry = c(2, 3, 4, 6), splitrule = "gini",
                       min.node.size = c(10, 25, 50, 100))
set.seed(SEED)
model_rf <- train(Churn ~ ., data = train, method = "ranger",
                  num.trees = 500, importance = "permutation",
                  tuneGrid = rf_grid, metric = "ROC", trControl = ctrl)
model_rf
model_rf$bestTune

p_tune <- ggplot(model_rf$results, aes(x = mtry, y = ROC, colour = factor(min.node.size))) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_colour_manual(values = c("#94B3D4", "#6690BD", "#3F6B9C", "#1F4E79"), name = "Minimum node size") +
  labs(title = "Random forest tuning: cross-validated ROC-AUC by mtry and node size",
       subtitle = "10-fold stratified CV on the training set, 500 trees per forest",
       x = "mtry (predictors tried at each split)", y = "Mean CV ROC-AUC")
save_plot(p_tune, "w3_rf_tuning", width = 6.8, height = 3.8)

# ---- 6.8 Cross-validated comparison ----
cv_results <- resamples(list(LogReg = model_lr, LogReg_all = model_lr_all, RandomForest = model_rf))
summary(cv_results)
cv_summary <- tibble(
  model = c("Logistic regression", "Logistic regression (all predictors)", "Random forest"),
  cv_roc_mean = c(mean(model_lr$resample$ROC), mean(model_lr_all$resample$ROC), mean(model_rf$resample$ROC)),
  cv_roc_sd   = c(sd(model_lr$resample$ROC), sd(model_lr_all$resample$ROC), sd(model_rf$resample$ROC)),
  cv_sens     = c(mean(model_lr$resample$Sens), mean(model_lr_all$resample$Sens), mean(model_rf$resample$Sens)),
  cv_spec     = c(mean(model_lr$resample$Spec), mean(model_lr_all$resample$Spec), mean(model_rf$resample$Spec))
)
cv_summary
save_table(cv_summary, "w3_cv_summary")

# ---- 6.9 Test-set evaluation at the default 0.5 threshold ----
prob_lr <- predict(model_lr, newdata = test, type = "prob")[, "Yes"]
prob_rf <- predict(model_rf, newdata = test, type = "prob")[, "Yes"]
to_class <- function(prob, threshold) factor(if_else(prob >= threshold, "Yes", "No"),
                                             levels = c("Yes", "No"))
cm_lr <- confusionMatrix(to_class(prob_lr, 0.5), test$Churn, positive = "Yes", mode = "everything")
cm_rf <- confusionMatrix(to_class(prob_rf, 0.5), test$Churn, positive = "Yes", mode = "everything")
cm_lr
cm_rf

roc_lr <- pROC::roc(test$Churn, prob_lr, levels = c("No", "Yes"), direction = "<")
roc_rf <- pROC::roc(test$Churn, prob_rf, levels = c("No", "Yes"), direction = "<")
pROC::ci.auc(roc_lr)
pROC::ci.auc(roc_rf)
auc_test <- pROC::roc.test(roc_lr, roc_rf, method = "delong", paired = TRUE)
auc_test

metrics_row <- function(model, prob, cm, roc, threshold) {
  tibble(model = model, threshold = threshold,
         accuracy = unname(cm$overall["Accuracy"]), kappa = unname(cm$overall["Kappa"]),
         precision = unname(cm$byClass["Precision"]), recall = unname(cm$byClass["Recall"]),
         specificity = unname(cm$byClass["Specificity"]), f1 = unname(cm$byClass["F1"]),
         balanced_accuracy = unname(cm$byClass["Balanced Accuracy"]),
         roc_auc = as.numeric(pROC::auc(roc)),
         brier = mean((prob - as.integer(test$Churn == "Yes"))^2),
         TP = cm$table["Yes", "Yes"], FP = cm$table["Yes", "No"],
         FN = cm$table["No", "Yes"], TN = cm$table["No", "No"])
}
test_metrics <- bind_rows(metrics_row("Logistic regression", prob_lr, cm_lr, roc_lr, 0.5),
                          metrics_row("Random forest", prob_rf, cm_rf, roc_rf, 0.5))
print(test_metrics %>% mutate(across(where(is.double), ~ round(.x, 4))), width = 110)

# ---- 6.10 ROC curves ----
roc_df <- bind_rows(
  tibble(model = sprintf("Logistic regression (AUC = %.3f)", pROC::auc(roc_lr)),
         fpr = 1 - roc_lr$specificities, tpr = roc_lr$sensitivities),
  tibble(model = sprintf("Random forest (AUC = %.3f)", pROC::auc(roc_rf)),
         fpr = 1 - roc_rf$specificities, tpr = roc_rf$sensitivities)) %>%
  arrange(model, fpr, tpr)
p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, colour = model)) +
  geom_abline(linetype = "dashed", colour = col_muted) +
  geom_path(linewidth = 0.9) +
  scale_colour_manual(values = c(col_accent, col_neutral), name = NULL) +
  coord_equal() +
  labs(title = "ROC curves on the held-out test set",
       subtitle = sprintf("n = %s customers; dashed line = random guessing (AUC = 0.5)",
                          comma(nrow(test))),
       x = "False positive rate (1 - specificity)", y = "True positive rate (recall)") +
  theme(legend.position = "inside", legend.position.inside = c(0.98, 0.03),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", colour = NA))
save_plot(p_roc, "w3_roc_curves", width = 5.8, height = 5.4)

# ---- 6.11 Confusion matrices ----
cm_df <- bind_rows(
  as_tibble(cm_lr$table) %>% mutate(model = "Logistic regression"),
  as_tibble(cm_rf$table) %>% mutate(model = "Random forest")) %>%
  mutate(Prediction = factor(Prediction, levels = c("Yes", "No")),
         Reference = factor(Reference, levels = c("Yes", "No")),
         cell = case_when(Prediction == "Yes" & Reference == "Yes" ~ "True positive",
                          Prediction == "Yes" & Reference == "No" ~ "False positive",
                          Prediction == "No" & Reference == "Yes" ~ "False negative",
                          TRUE ~ "True negative"),
         correct = Prediction == Reference)
cm_df
p_cm <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = correct)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(comma(n), "\n", cell)), size = 3.3, lineheight = 0.9,
            colour = ifelse(cm_df$correct, "white", col_text)) +
  facet_wrap(~ model) +
  scale_fill_manual(values = c(`TRUE` = col_accent, `FALSE` = "#DDE3EA"), guide = "none") +
  scale_y_discrete(limits = c("No", "Yes")) +
  labs(title = "Confusion matrices on the test set (threshold = 0.5)",
       subtitle = "Rows = predicted class, columns = actual class",
       x = "Actual churn", y = "Predicted churn") +
  theme(panel.grid = element_blank())
save_plot(p_cm, "w3_confusion_matrices", width = 7, height = 3.6)

# ---- 6.12 Threshold analysis on out-of-fold CV predictions ----
threshold_metrics <- function(prob, truth, thresholds = seq(0.05, 0.95, by = 0.01)) {
  y <- truth == "Yes"
  map_dfr(thresholds, function(t) {
    pred <- prob >= t
    tp <- sum(pred & y); fp <- sum(pred & !y); fn <- sum(!pred & y); tn <- sum(!pred & !y)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    recall <- tp / (tp + fn)
    tibble(threshold = t, precision = precision, recall = recall,
           specificity = tn / (tn + fp),
           f1 = if (!is.na(precision) && (precision + recall) > 0)
             2 * precision * recall / (precision + recall) else NA_real_,
           flagged_share = mean(pred))
  })
}
oof_lr <- model_lr$pred
oof_rf <- model_rf$pred %>%
  filter(mtry == model_rf$bestTune$mtry, min.node.size == model_rf$bestTune$min.node.size)
nrow(oof_lr); nrow(oof_rf)                        # one out-of-fold prediction per training row
thr_lr <- threshold_metrics(oof_lr$Yes, oof_lr$obs) %>% mutate(model = "Logistic regression")
thr_rf <- threshold_metrics(oof_rf$Yes, oof_rf$obs) %>% mutate(model = "Random forest")
best_thr <- bind_rows(thr_lr, thr_rf) %>% group_by(model) %>% slice_max(f1, n = 1, with_ties = FALSE)
best_thr
save_table(bind_rows(thr_lr, thr_rf), "w3_threshold_metrics_cv")

p_thr <- thr_lr %>%
  select(threshold, Precision = precision, Recall = recall, F1 = f1) %>%
  pivot_longer(-threshold, names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%          # precision is undefined when nothing is flagged
  mutate(metric = factor(metric, levels = c("Recall", "F1", "Precision"))) %>%
  ggplot(aes(x = threshold, y = value, colour = metric)) +
  geom_vline(xintercept = c(0.5, best_thr$threshold[best_thr$model == "Logistic regression"]),
             linetype = c("dashed", "solid"), colour = col_muted) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = pal_ordinal[c(3, 2, 1)], name = NULL) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(title = sprintf("Logistic regression: F1 peaks at a threshold of %.2f in cross-validation",
                       best_thr$threshold[best_thr$model == "Logistic regression"]),
       subtitle = "Out-of-fold predictions on the training set; dashed line = default 0.5",
       x = "Classification threshold", y = "Metric value")
save_plot(p_thr, "w3_threshold_analysis", width = 7, height = 4)

# Apply the CV-chosen thresholds once to the test set
thr_lr_best <- best_thr$threshold[best_thr$model == "Logistic regression"]
thr_rf_best <- best_thr$threshold[best_thr$model == "Random forest"]
cm_lr_t <- confusionMatrix(to_class(prob_lr, thr_lr_best), test$Churn, positive = "Yes", mode = "everything")
cm_rf_t <- confusionMatrix(to_class(prob_rf, thr_rf_best), test$Churn, positive = "Yes", mode = "everything")
cm_lr_t$table
cm_rf_t$table
test_metrics_all <- bind_rows(test_metrics,
                              metrics_row("Logistic regression", prob_lr, cm_lr_t, roc_lr, thr_lr_best),
                              metrics_row("Random forest", prob_rf, cm_rf_t, roc_rf, thr_rf_best))
print(test_metrics_all %>% select(model, threshold, accuracy, precision, recall, specificity, f1,
                                  roc_auc, TP, FP, FN, TN) %>%
        mutate(across(where(is.double), ~ round(.x, 3))), width = 110)
save_table(test_metrics_all, "w3_test_metrics")

# ---- 6.13 Logistic regression diagnostics ----
# Hosmer-Lemeshow goodness-of-fit test (training data, 10 groups)
hl <- ResourceSelection::hoslem.test(train_glm$churn_yes, fitted(glm_lr), g = 10)
hl

# Influential observations (Cook's distance)
cooks <- cooks.distance(glm_lr)
c(max_cooks_d = max(cooks), n_above_4_over_n = sum(cooks > 4 / nrow(train_glm)))

# Binned residuals: average (observed - fitted) within 40 bins of fitted probability
binned <- tibble(fitted = fitted(glm_lr), resid = train_glm$churn_yes - fitted(glm_lr)) %>%
  mutate(bin = ntile(fitted, 40)) %>%
  group_by(bin) %>%
  summarise(mean_fitted = mean(fitted), mean_resid = mean(resid), n = n(),
            se = 2 * sqrt(mean(fitted) * (1 - mean(fitted)) / n()))
sum(abs(binned$mean_resid) > binned$se)          # bins outside the +/- 2 SE band
p_binned <- ggplot(binned, aes(x = mean_fitted, y = mean_resid)) +
  geom_hline(yintercept = 0, colour = col_muted) +
  geom_line(aes(y = se), linetype = "dashed", colour = col_muted) +
  geom_line(aes(y = -se), linetype = "dashed", colour = col_muted) +
  geom_point(colour = col_accent, size = 2) +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "Binned residuals of the logistic regression (training data)",
       subtitle = "40 equal-size bins; dashed lines = approximate +/- 2 standard errors",
       x = "Mean fitted churn probability", y = "Mean residual (observed - fitted)")
save_plot(p_binned, "w3_binned_residuals", width = 6.8, height = 3.8)

# Calibration on the test set: predicted vs observed churn by decile of risk
calib <- bind_rows(
  tibble(model = "Logistic regression", prob = prob_lr, actual = test$Churn == "Yes"),
  tibble(model = "Random forest", prob = prob_rf, actual = test$Churn == "Yes")) %>%
  group_by(model) %>%
  mutate(decile = ntile(prob, 10)) %>%
  group_by(model, decile) %>%
  summarise(mean_predicted = mean(prob), observed_rate = mean(actual), n = n(), .groups = "drop")
print(calib %>% mutate(across(c(mean_predicted, observed_rate), ~ round(.x, 3))), n = Inf)
save_table(calib, "w3_calibration_deciles")
p_calib <- ggplot(calib, aes(x = mean_predicted, y = observed_rate, colour = model)) +
  geom_abline(linetype = "dashed", colour = col_muted) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_colour_manual(values = c(col_accent, col_neutral), name = NULL) +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  coord_equal() +
  labs(title = "Calibration on the test set by decile of predicted risk",
       subtitle = "Points on the dashed line = predicted probabilities match observed churn",
       x = "Mean predicted churn probability", y = "Observed churn rate")
save_plot(p_calib, "w3_calibration", width = 5.8, height = 5.4)

# ---- 6.14 Variable importance ----
# Odds ratios with 95% profile-likelihood confidence intervals
p_or <- or_table %>%
  mutate(term = fct_reorder(term, estimate),
         significant = if_else(conf.low > 1 | conf.high < 1, "95% CI excludes 1", "95% CI includes 1")) %>%
  ggplot(aes(x = estimate, y = term, colour = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = col_muted) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
  geom_point(size = 2) +
  scale_x_log10() +
  scale_colour_manual(values = c("95% CI excludes 1" = col_accent, "95% CI includes 1" = "#A9B3BE"),
                      name = NULL) +
  labs(title = "Logistic regression odds ratios for churn",
       subtitle = "Log scale; > 1 = higher odds of churn than the reference level, holding other terms fixed",
       x = "Odds ratio (95% CI)", y = NULL) +
  theme(axis.text.y = element_text(size = 8))
save_plot(p_or, "w3_odds_ratios", width = 7, height = 5.6)

rf_imp <- varImp(model_rf, scale = FALSE)$importance %>%
  as_tibble(rownames = "feature") %>%
  rename(importance = Overall) %>%
  arrange(desc(importance))
print(rf_imp, n = 15)
save_table(rf_imp, "w3_rf_permutation_importance")
p_imp <- rf_imp %>%
  slice_max(importance, n = 15) %>%
  ggplot(aes(x = importance, y = fct_reorder(feature, importance))) +
  geom_col(fill = col_accent, width = 0.7) +
  labs(title = "Random forest: permutation importance (top 15 features)",
       subtitle = "Mean decrease in accuracy when a feature's values are shuffled",
       x = "Permutation importance", y = NULL) +
  theme(axis.text.y = element_text(size = 8.5))
save_plot(p_imp, "w3_rf_importance", width = 7, height = 4.6)

# ---- 6.15 Cumulative gains on the test set ----
gains <- bind_rows(
  tibble(model = "Logistic regression", prob = prob_lr, actual = test$Churn == "Yes"),
  tibble(model = "Random forest", prob = prob_rf, actual = test$Churn == "Yes")) %>%
  group_by(model) %>%
  arrange(desc(prob), .by_group = TRUE) %>%
  mutate(share_contacted = row_number() / n(),
         share_churners_captured = cumsum(actual) / sum(actual)) %>%
  ungroup()
gains_summary <- gains %>%
  group_by(model) %>%
  summarise(top10 = share_churners_captured[which.min(abs(share_contacted - 0.1))],
            top20 = share_churners_captured[which.min(abs(share_contacted - 0.2))],
            top30 = share_churners_captured[which.min(abs(share_contacted - 0.3))])
gains_summary
save_table(gains_summary, "w3_gains_summary")
p_gains <- ggplot(gains, aes(x = share_contacted, y = share_churners_captured, colour = model)) +
  geom_abline(linetype = "dashed", colour = col_muted) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(col_accent, col_neutral), name = NULL) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = sprintf("Targeting the 20%% highest-risk customers would reach %s of churners",
                       pct(gains_summary$top20[gains_summary$model == "Logistic regression"], 0)),
       subtitle = "Cumulative gains on the test set (logistic regression figure in title); dashed = random",
       x = "Share of customers contacted (highest predicted risk first)",
       y = "Share of churners reached")
save_plot(p_gains, "w3_cumulative_gains", width = 6.8, height = 4.2)

# ---- 6.16 Save model objects and test-set predictions ----
saveRDS(model_lr, file.path(paths$models, "model_logistic.rds"))
saveRDS(model_rf, file.path(paths$models, "model_random_forest.rds"))
test_predictions <- tibble(row_id = seq_len(nrow(test)), actual = test$Churn,
                           prob_logistic = prob_lr, prob_random_forest = prob_rf)
write_csv(test_predictions, file.path(paths$tables, "w3_test_predictions.csv"))

# ---- 6.17 Metrics for the report ----
record("n_train", nrow(train)); record("n_test", nrow(test))
record("train_churn_rate", mean(train$Churn == "Yes")); record("test_churn_rate", mean(test$Churn == "Yes"))
record("n_test_churn", sum(test$Churn == "Yes")); record("n_test_nochurn", sum(test$Churn == "No"))
record("vif_max_all", max(vif_all[, "GVIF"])); record("vif_monthly_all", vif_all["MonthlyCharges", "GVIF"])
record("vif_total_all", vif_all["TotalCharges", "GVIF"]); record("vif_internet_all", vif_all["InternetService", "GVIF"])
record("vif_max_lr", max(vif_lr[, "GVIF"])); record("vif_adj_max_lr", max(vif_lr[, "GVIF^(1/(2*Df))"]))
record("mc_r2_services", mc_r2)
record("cv_roc_lr", cv_summary$cv_roc_mean[1]); record("cv_roc_sd_lr", cv_summary$cv_roc_sd[1])
record("cv_roc_lr_all", cv_summary$cv_roc_mean[2]); record("cv_roc_sd_lr_all", cv_summary$cv_roc_sd[2])
record("cv_roc_rf", cv_summary$cv_roc_mean[3]); record("cv_roc_sd_rf", cv_summary$cv_roc_sd[3])
record("cv_sens_lr", cv_summary$cv_sens[1]); record("cv_spec_lr", cv_summary$cv_spec[1])
record("cv_sens_rf", cv_summary$cv_sens[3]); record("cv_spec_rf", cv_summary$cv_spec[3])
record("rf_best_mtry", model_rf$bestTune$mtry); record("rf_best_nodesize", model_rf$bestTune$min.node.size)
record("rf_grid_roc_min", min(model_rf$results$ROC)); record("rf_grid_roc_max", max(model_rf$results$ROC))
record("rf_grid_n", nrow(rf_grid))
record("lr_aic", AIC(glm_lr)); record("lr_mcfadden_r2", mcfadden_r2)
record("lr_null_dev", glm_lr$null.deviance); record("lr_resid_dev", glm_lr$deviance)
record("lr_n_coef", length(coef(glm_lr)) - 1)
record("lr_n_sig", sum(summary(glm_lr)$coefficients[-1, 4] < 0.05))
for (m in c("lr", "rf")) {
  for (t in c("default", "tuned")) {
    row <- test_metrics_all[test_metrics_all$model == ifelse(m == "lr", "Logistic regression", "Random forest"), ][ifelse(t == "default", 1, 2), ]
    for (col in c("threshold", "accuracy", "kappa", "precision", "recall", "specificity", "f1",
                  "balanced_accuracy", "roc_auc", "brier", "TP", "FP", "FN", "TN")) {
      record(paste0("test_", m, "_", t, "_", col), row[[col]])
    }
  }
}
record("nir", unname(cm_lr$overall["AccuracyNull"]))
record("lr_acc_vs_nir_p", unname(cm_lr$overall["AccuracyPValue"]))
record("rf_acc_vs_nir_p", unname(cm_rf$overall["AccuracyPValue"]))
record("auc_lr_ci_low", as.numeric(pROC::ci.auc(roc_lr))[1]); record("auc_lr_ci_high", as.numeric(pROC::ci.auc(roc_lr))[3])
record("auc_rf_ci_low", as.numeric(pROC::ci.auc(roc_rf))[1]); record("auc_rf_ci_high", as.numeric(pROC::ci.auc(roc_rf))[3])
record("auc_diff_p", auc_test$p.value); record("auc_diff_z", unname(auc_test$statistic))
record("hl_chisq", unname(hl$statistic)); record("hl_p", hl$p.value)
record("cooks_max", max(cooks)); record("cooks_n_above", sum(cooks > 4 / nrow(train_glm)))
record("binned_outside", sum(abs(binned$mean_resid) > binned$se)); record("binned_n", nrow(binned))
for (i in seq_len(nrow(gains_summary))) {
  m <- ifelse(gains_summary$model[i] == "Logistic regression", "lr", "rf")
  record(paste0("gains_", m, "_top10"), gains_summary$top10[i])
  record(paste0("gains_", m, "_top20"), gains_summary$top20[i])
  record(paste0("gains_", m, "_top30"), gains_summary$top30[i])
}
for (i in seq_len(nrow(or_table))) {
  key <- gsub("[^A-Za-z0-9]", "", or_table$term[i])
  record(paste0("or_", key), or_table$estimate[i])
  record(paste0("or_low_", key), or_table$conf.low[i]); record(paste0("or_high_", key), or_table$conf.high[i])
  record(paste0("or_p_", key), or_table$p.value[i])
}
for (i in 1:10) record(paste0("rf_imp_", i, "_name"), rf_imp$feature[i])
for (i in 1:10) record(paste0("rf_imp_", i, "_value"), rf_imp$importance[i])
calib_lr <- filter(calib, model == "Logistic regression")
record("calib_lr_top_pred", calib_lr$mean_predicted[10]); record("calib_lr_top_obs", calib_lr$observed_rate[10])
record("calib_lr_bottom_pred", calib_lr$mean_predicted[1]); record("calib_lr_bottom_obs", calib_lr$observed_rate[1])
calib_rf <- filter(calib, model == "Random forest")
record("calib_rf_top_pred", calib_rf$mean_predicted[10]); record("calib_rf_top_obs", calib_rf$observed_rate[10])
write_metrics("06_predictive_model")
