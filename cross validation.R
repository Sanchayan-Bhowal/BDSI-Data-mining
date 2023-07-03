library(caret)
ctrl <- trainControl(method = "cv", number = 10)

#fit a regression model and use k-fold CV to evaluate performance
cv_model_path1 <- train(g(Y) ~ ., data = pathway_surv1, method = "lm", trControl = ctrl)
cv_model_path2 <- train(g(Y) ~ ., data = pathway_surv2, method = "lm", trControl = ctrl)

cv_model_pc1 <- train(g(Y) ~ ., data = pc_surv1, method = "lm", trControl = ctrl)
cv_model_pc2 <- train(g(Y) ~ ., data = pc_surv2, method = "lm", trControl = ctrl)
