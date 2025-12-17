library(data.table)
library(xgboost)

# -----------------------------
# 1. Prep data
# -----------------------------
setDT(combined_summary)
setDT(context)

# Play-by-play context
context_small <- context[, .(
  game_id,
  play_id,
  pass_result,
  pass_length
)]

# Merge commitment data with play context
df <- merge(
  combined_summary,
  context_small,
  by = c("game_id", "play_id"),
  all.x = TRUE
)

# Define disruption ONCE (play-level truth)
df[, disruption := as.integer(pass_result %in% c("I", "IN"))]

# -----------------------------
# 2. Play-level aggregation
# -----------------------------
play_level <- df[, .(
  max_commitment    = max(avg_commitment, na.rm = TRUE),
  median_commitment = median(avg_commitment, na.rm = TRUE),
  num_defenders     = .N,
  pass_length       = pass_length[1],
  disruption        = disruption[1]
), by = .(game_id, play_id)]

# Remove bad rows
play_level <- play_level[
  is.finite(max_commitment) &
    is.finite(median_commitment) &
    is.finite(pass_length)
]

# -----------------------------
# 3. Feature interactions
# -----------------------------
play_level[, `:=`(
  max_pass = max_commitment * pass_length,
  med_pass = median_commitment * pass_length,
  max_def  = max_commitment * num_defenders,
  med_def  = median_commitment * num_defenders,
  max_med  = max_commitment * median_commitment
)]

features <- c(
  "max_commitment",
  "median_commitment",
  "num_defenders",
  "pass_length",
  "max_pass",
  "med_pass",
  "max_def",
  "med_def",
  "max_med"
)

X <- as.matrix(play_level[, ..features])
y <- play_level$disruption

# -----------------------------
# 4. XGBoost (PR-AUC)
# -----------------------------
dtrain <- xgb.DMatrix(data = X, label = y)

scale_pos_weight <- sum(y == 0) / sum(y == 1)
cat("scale_pos_weight:", scale_pos_weight, "\n")

params <- list(
  objective = "binary:logistic",
  eval_metric = "aucpr",
  max_depth = 4,
  eta = 0.07,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos_weight
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  verbose = 0
)

model_importance <- xgb.importance(
  feature_names = features,
  model = model
)
print(model_importance)

# Confusion matrix
set.seed(42)
train_idx <- sample(
  1:nrow(X),
  size = floor(0.8 * nrow(X)),
  replace = FALSE
)
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]
dtest <- xgb.DMatrix(data = X_test, label = y_test)
pred_prob <- predict(model, dtest)

final_pred <- ifelse(pred_prob > 0.5, 1, 0)

final_cm <- confusionMatrix(
  factor(final_pred, levels = c(0,1)),
  factor(y_test, levels = c(0,1))
)
print(final_cm)


# -----------------------------
# 5. Expected disruption
# -----------------------------
play_level[, expected_disruption := predict(model, X) * sum(y == 0) / nrow(play_level)]



# -----------------------------
# 6. Attach expected values back to defenders
# -----------------------------
df <- merge(
  df,
  play_level[, .(
    game_id,
    play_id,
    expected_disruption,
    disruption
  )],
  by = c("game_id", "play_id"),
  all.x = TRUE
)

