#Random Forests
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway_class <- rfe(pathway.scores_train_class, Y_class_train, sizes=c(1:60), rfeControl=control)
# summarize the results
print(results_pathway_class)
# list the chosen features
predictors(results_pathway_class)
# plot the results
plot(results_pathway_class, type=c("g", "o"),xlim=c(0,60))
# generate formula
formula_pathway_rf_class <- as.formula(paste("Y_class ~ ", 
                                           paste(predictors(results_pathway_class)[1:60], 
                                                 collapse = "+")))

results_pc_class <- rfe(pc_scores_train_class, Y_class_train, sizes=c(1:60), rfeControl=control)
# summarize the results
print(results_pc_class)
# list the chosen features
predictors(results_pc_class)
# plot the results
plot(results_pc_class, type=c("g", "o"),xlim=c(0,60))
# generate formula
formula_pc_rf_class <- as.formula(paste("Y_class ~ ", 
                                      paste(predictors(results_pc_class), 
                                            collapse = "+")))

#boruta
library(Boruta)
boruta_path_class <- Boruta(Y_class~.,data = pathway_surv_train_class)
boruta_pc_class <- Boruta(Y_class~.,data = pc_surv_train_class)

#confirmed and tentative
boruta_path1_class <- names(boruta_path_class$finalDecision[boruta_path_class$finalDecision %in% 
                                                          c("Confirmed", "Tentative")])

formula_pathway_b1_class <- as.formula(paste("Y_class ~ ", paste(boruta_path1_class, collapse = "+")))

boruta_pc1_class <- names(boruta_pc_class$finalDecision[boruta_pc_class$finalDecision %in% 
                                                      c("Confirmed", "Tentative")])

formula_pc_b1_class <- as.formula(paste("Y_class ~ ", paste(boruta_pc1_class, collapse = "+")))

#confirmed
boruta_path2_class <- names(boruta_path_class$finalDecision[boruta_path_class$finalDecision %in% 
                                                          c("Confirmed")])

formula_pathway_b2_class <- as.formula(paste("Y_class ~ ", paste(boruta_path2_class, collapse = "+")))

boruta_pc2_class <- names(boruta_pc_class$finalDecision[boruta_pc_class$finalDecision %in% 
                                                      c("Confirmed")])
formula_pc_b2_class <- as.formula(paste("Y_class ~ ", paste(boruta_pc2_class, collapse = "+")))


#lmfuncs
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway2_class <- rfe(pathway.scores_train_class, Y_class_train, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pathway2_class)
# list the chosen features
predictors(results_pathway2_class)
# plot the results
plot(results_pathway2_class, type=c("g", "o"),xlim=c(0,30))
# generate formula
formula_pathway_lm_class <- as.formula(paste("Y_class ~ ", 
                                           paste(predictors(results_pathway2_class), 
                                                 collapse = "+")))

results_pc2_class <- rfe(pc_scores_train_class, Y_class_train, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pc2_class)
# list the chosen features
predictors(results_pc2_class)
# plot the results
plot(results_pc2_class, type=c("g", "o"),xlim=c(0,30))
# generate formula
formula_pc_lm_class <- as.formula(paste("Y_class ~ ", 
                                      paste(predictors(results_pc2_class), 
                                            collapse = "+")))

#baggedTrees
control <- rfeControl(functions=treebagFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway3_class <- rfe(pathway.scores_train_class, Y_class_train, sizes=c(1:60), rfeControl=control)
# summarize the results
print(results_pathway3_class)
# list the chosen features
predictors(results_pathway3_class)
# plot the results
plot(results_pathway3_class, type=c("g", "o"),xlim=c(0,60))
# generate formula
formula_pathway_bt_class <- as.formula(paste("Y_class ~ ", 
                                           paste(predictors(results_pathway3_class), 
                                                 collapse = "+")))

results_pc3_class <- rfe(pc_scores_train_class, Y_class_train, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pc3_class)
# list the chosen features
predictors(results_pc3_class)
# plot the results
plot(results_pc3_class, type=c("g", "o"),xlim=c(0,30))
# generate formula
formula_pc_bt_class <- as.formula(paste("Y_class ~ ", 
                                      paste(predictors(results_pc3_class), 
                                            collapse = "+")))