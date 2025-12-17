library(data.table)
library(xgboost)
library(caret)

# play-level compression
play_level <- df[, .(
  max_commitment    = max(commitment, na.rm=TRUE),
  median_commitment = median(commitment, na.rm=TRUE),
  num_defenders     = .N,
  disruption        = fifelse(pass_result %in% c("I","IN"), 1, 0)
), by=.(game_id, play_id)]

# Features
X <- as.matrix(play_level[, .(
  max_commitment,
  median_commitment,
  num_defenders
)])
y <- play_level$disruption

# Train-test split
set.seed(123)
train_idx <- sample(seq_len(nrow(X)), size = 0.7*nrow(X))

X_train <- X[train_idx,]
X_test  <- X[-train_idx,]
y_train <- y[train_idx]
y_test  <- y[-train_idx]

dtrain <- xgb.DMatrix(data=X_train, label=y_train)
dtest  <- xgb.DMatrix(data=X_test,  label=y_test)

# Compute proper weight for imbalance
pos_wt <- sum(y_train == 0) / sum(y_train == 1)
cat("scale_pos_weight =", pos_wt, "\n")

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = pos_wt   # <<< key line
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300,
  watchlist = list(train=dtrain),
  verbose = 0
)

# Predict
pred_prob <- predict(model, X_test)
pred <- ifelse(pred_prob > 0.5, 1, 0)

# Confusion matrix
cm <- confusionMatrix(
  factor(pred, levels=c(0,1)),
  factor(y_test, levels=c(0,1))
)

print(cm)

# features
importance <- xgb.importance(
  feature_names = colnames(X_train),
  model = model
)
print(importance)
