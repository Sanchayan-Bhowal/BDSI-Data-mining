library(caret)
ctrl <- trainControl(method = "cv", number = 10)

#fit a regression model and use k-fold CV to evaluate performance
cv_model <- train(g(Y) ~ ., data = pathway_surv1, method = "lm", trControl = ctrl)
