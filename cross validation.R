library(caret)
ctrl <- trainControl(method = "cv", number = 10)

#fit a regression model and use k-fold CV to evaluate performance
cv_model_path1 <- train(P ~ ., data = pathway_surv1, method = "lm", trControl = ctrl)
cv_model_path2 <- train(P ~ ., data = pathway_surv2, method = "lm", trControl = ctrl)

##boruta
cv_model_path3 <- train(formula_pathway_b1, data = pathway_surv, 
                        method = "lm", trControl = ctrl)
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
