library(caret)
ctrl <- trainControl(method = "cv", number = 10)

#fit a regression model and use k-fold CV to evaluate performance
cv_model_path1 <- train(P ~ ., data = pathway_surv1, method = "lm", trControl = ctrl)
cv_model_path2 <- train(P ~ ., data = pathway_surv2, method = "lm", trControl = ctrl)

##boruta
cv_model_path3 <- train(formula_pathway_b1, data = pathway_surv, 
                        method = 'glmnet', 
                        tuneGrid = expand.grid(alpha = 0, lambda = 1), trControl = ctrl)
cv_model_path4 <- train(formula_pathway_b2, data = pathway_surv, 
                        method = "lm", trControl = ctrl)
##lmfuncs
cv_model_path5 <- train(formula_pathway_lm, data = pathway_surv, 
                        method = "lm", trControl = ctrl)
##baggedTrees
cv_model_path6 <- train(formula_pathway_bt, data = pathway_surv, 
                        method = "lm", trControl = ctrl)

#imaging
cv_model_pc1 <- train(P ~ ., data = pc_surv1, method = "lm", trControl = ctrl)
cv_model_pc2 <- train(P ~ ., data = pc_surv2, method = "lm", trControl = ctrl)

##boruta
cv_model_pc3 <- train(formula_pc_b1, data = pc_surv, 
                      method = "lm", trControl = ctrl)
cv_model_pc4 <- train(formula_pc_b2, data = pc_surv, 
                      method = "lm", trControl = ctrl)

##lmfuncs
cv_model_pc5 <- train(formula_pc_lm, data = pc_surv, 
                        method = "lm", trControl = ctrl)
##baggedTrees
cv_model_pc6 <- train(formula_pc_bt, data = pc_surv, 
                      method = "lm", trControl = ctrl)

#without multicolinearity

#genetics
##naive method
cv_model_path1_cor <- train(P~.,data = pathway_surv_cor,
                        method="lm", trControl = ctrl)
##random forests
cv_model_path2_cor <- train(formula_pathway_rf_cor, data = pathway_surv_cor, 
                        method = "lm", trControl = ctrl)
##boruta
cv_model_path3_cor <- train(formula_pathway_b1_cor, data = pathway_surv_cor, 
                        method = "lm", trControl = ctrl)
cv_model_path4_cor <- train(formula_pathway_b2_cor, data = pathway_surv_cor, 
                        method = "lm", trControl = ctrl)
##lmfuncs
cv_model_path5_cor <- train(formula_pathway_lm_cor, data = pathway_surv_cor, 
                        method = "lm", trControl = ctrl)
##baggedTrees
cv_model_path6_cor <- train(formula_pathway_bt_cor, data = pathway_surv_cor, 
                        method = "lm", trControl = ctrl)

#imaging
##random forests
cv_model_pc2_cor <- train(formula_pc_rf_cor, data = pc_surv_cor, 
                            method = "lm", trControl = ctrl)
##boruta
cv_model_pc3_cor <- train(formula_pc_b1_cor, data = pc_surv_cor, 
                      method = "lm", trControl = ctrl)
cv_model_pc4_cor <- train(formula_pc_b2_cor, data = pc_surv_cor, 
                      method = "lm", trControl = ctrl)

##lmfuncs
cv_model_pc5_cor <- train(formula_pc_lm_cor, data = pc_surv_cor, 
                      method = "lm", trControl = ctrl)
##baggedTrees
cv_model_pc6_cor <- train(formula_pc_bt_cor, data = pc_surv_cor, 
                      method = "lm", trControl = ctrl)
save(cv_model_path1,
    cv_model_path2,
    cv_model_path3,
    cv_model_path4,
    cv_model_path5,
    cv_model_path6,
    cv_model_pc1,
    cv_model_pc2,
    cv_model_pc3,
    cv_model_pc4,
    cv_model_pc5,
    cv_model_pc6,
    file="models.rda")

save(cv_model_path2_cor,
    cv_model_path3_cor,
    cv_model_path4_cor,
    cv_model_path5_cor,
    cv_model_path6_cor,
    cv_model_pc2_cor,
    cv_model_pc3_cor,
    cv_model_pc4_cor,
    cv_model_pc5_cor,
    cv_model_pc6_cor,
    file="models_cor.rda")

#ridge regression

parameters <- c(seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 1))

#naive
path_naive <- train(P ~ ., data = pathway_surv1_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)

predictions_path_naive <- path_naive %>% predict(pathway_surv1_test)
path_naive_sum = postResample(predictions_path_naive, P_test)

#randomForests
path_rf<-train(formula_pathway_rf, data = pathway_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_path_rf <- path_rf %>% predict(pathway_surv_test)

path_rf_sum = postResample(predictions_path_rf, P_test)

#boruta-confirmed and tentative
path_bor_ct<-train(formula_pathway_b1, data = pathway_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_path_bor_ct <- path_bor_ct %>% predict(pathway_surv_test)

path_bor_ct_sum = postResample(predictions_path_bor_ct, P_test)

#boruta-confirmed
path_bor_c<-train(formula_pathway_b2, data = pathway_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_path_bor_c <- path_bor_c %>% predict(pathway_surv_test)

path_bor_c_sum = postResample(predictions_path_bor_c, P_test)

#lmfuncs
path_lm<-train(formula_pathway_lm, data = pathway_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_path_lm <- path_lm %>% predict(pathway_surv_test)

path_lm_sum = postResample(predictions_path_lm, P_test)
#baggedTrees
path_bt<-train(formula_pathway_bt, data = pathway_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_path_bt <- path_bt %>% predict(pathway_surv_test)

path_bt_sum = postResample(predictions_path_bt, P_test)

#imaging

#naive
pc_naive <- train(P ~ ., data = pc_surv1_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)

predictions_pc_naive <- pc_naive %>% predict(pc_surv1_test)
pc_naive_sum = postResample(predictions_pc_naive, P_test)

#randomForests
pc_rf<-train(formula_pc_rf, data = pc_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_pc_rf <- pc_rf %>% predict(pc_surv_test)

pc_rf_sum = postResample(predictions_pc_rf, P_test)

#boruta-confirmed and tentative
pc_bor_ct<-train(formula_pc_b1, data = pc_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_pc_bor_ct <- pc_bor_ct %>% predict(pc_surv_test)

pc_bor_ct_sum = postResample(predictions_pc_bor_ct, P_test)

#boruta-confirmed
pc_bor_c<-train(formula_pc_b2, data = pc_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_pc_bor_c <- pc_bor_c %>% predict(pc_surv_test)

pc_bor_c_sum = postResample(predictions_pc_bor_c, P_test)

#lmfuncs
pc_lm<-train(formula_pc_lm, data = pc_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_pc_lm <- pc_lm %>% predict(pc_surv_test)

pc_lm_sum = postResample(predictions_pc_lm, P_test)

#baggedTrees
pc_bt<-train(formula_pc_bt, data = pc_surv_train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = parameters),
             trControl = ctrl,
             metric = "RMSE"
)
predictions_pc_bt <- pc_bt %>% predict(pc_surv_test)

pc_bt_sum = postResample(predictions_pc_bt, P_test)

save(path_naive,
     path_rf,
     path_bor_ct,
     path_bor_c,
     path_lm,
     path_bt,
     pc_naive,
     pc_rf,
     pc_bor_ct,
     pc_bor_c,
     pc_lm,
     pc_bt,file = "ridge_models.rda")