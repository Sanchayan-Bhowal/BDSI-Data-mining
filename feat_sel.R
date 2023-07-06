set.seed(07272023)
# load the library
library(caret)
# load the data
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway <- rfe(pathway.scores, Y, sizes=c(1:60), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results_pathway, type=c("g", "o"),xlim=c(0,60))

features_pathway <- results[["optVariables"]][1:24]

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_pc <- rfe(pc_scores, Y, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pc)
# list the chosen features
predictors(results_pc)
# plot the results
plot(results_pc, type=c("g", "o"),xlim=c(0,30))

features_pc <- results_pc[["optVariables"]][1:13]

pc_surv2=pc_surv %>% 
  dplyr::select(FLAIR_NCR.NET.17,T2_NCR.NET.4,T2_ED.1,FLAIR_NCR.NET.5,
         T2_NCR.NET.7,T2_NCR.NET.9,T2_NCR.NET.16,T1_NCR.NET.3,
         T2_NCR.NET.10,T2_NCR.NET.13,FLAIR_ET.17,T2_NCR.NET.1,T1_ET.10,Y)

intercept_only <- lm(g(Y) ~ 1, data=pc_surv2)

#define model with all predictors
all <- lm(g(Y) ~ ., data=pc_surv2)

#perform forward stepwise regression
forward_pc <- step(intercept_only, direction='forward', scope=formula(all), trace=0)

#perform backward stepwise regression
backward_pc <- step(intercept_only, direction='backward', scope=formula(all), trace=0)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_pc <- rfe(pc_scores, Y, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pc)
# list the chosen features
predictors(results_pc)
# plot the results
plot(results_pc, type=c("g", "o"),xlim=c(0,30))

pathway_surv=as.data.frame(cbind(pathway.scores,Y))

pathway_surv1=pathway_surv %>% 
  select(KEGG_RIG_I_LIKE_RECEPTOR_SIGNALING_PATHWAY,MODULE_457
         ,MORF_RAD21,KEGG_RIBOFLAVIN_METABOLISM
         ,GNF2_TTN,MORF_SART1
         ,MYC_UP.V1_DN,MORF_TERF1
         ,MODULE_440,MODULE_159,Y)
intercept_pathway <- lm(g(Y) ~ 1, data=pathway_surv1)

#define model with all predictors
all_pathway <- lm(g(Y) ~ ., data=pathway_surv1)

#perform forward stepwise regression
forward_pathway <- step(intercept_pathway, direction='forward', 
                        scope=formula(all_pathway), trace=0)

#perform backward stepwise regression
backward_pathway <- step(intercept_pathway, direction='backward', 
                         scope=formula(all_pathway), trace=0)
#boruta
library(Boruta)
boruta_path <- Boruta(P~.,data = pathway_surv)
boruta_pc <- Boruta(P~.,data = pc_surv)

#confirmed and tentative
boruta_path1 <- names(boruta_path$finalDecision[boruta_path$finalDecision %in% 
                                                  c("Confirmed", "Tentative")])

formula_pathway_b1 <- as.formula(paste("P ~", paste(boruta_path1, collapse = "+")))

boruta_pc1 <- names(boruta_pc$finalDecision[boruta_pc$finalDecision %in% 
                                                  c("Confirmed", "Tentative")])

formula_pc_b1 <- as.formula(paste("P ~", paste(boruta_pc1, collapse = "+")))

#confirmed
boruta_path2 <- names(boruta_path$finalDecision[boruta_path$finalDecision %in% 
                                                  c("Confirmed")])

formula_pathway_b2 <- as.formula(paste("P ~", paste(boruta_path2, collapse = "+")))

boruta_pc2 <- names(boruta_pc$finalDecision[boruta_pc$finalDecision %in% 
                                                  c("Confirmed")])
formula_pc_b2 <- as.formula(paste("P ~", paste(boruta_pc2, collapse = "+")))


#lmfuncs
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway2 <- rfe(pathway.scores, P, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pathway2)
# list the chosen features
predictors(results_pathway2)
# plot the results
plot(results_pathway2, type=c("g", "o"),xlim=c(0,30))
# generate formula
formula_pathway_lm <- as.formula(paste("P ~", 
                                       paste(predictors(results_pathway2), 
                                             collapse = "+")))

results_pc2 <- rfe(pc_scores, P, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pc2)
# list the chosen features
predictors(results_pc2)
# plot the results
plot(results_pc2, type=c("g", "o"),xlim=c(0,30))
# generate formula
formula_pc_lm <- as.formula(paste("P ~", 
                                       paste(predictors(results_pc2), 
                                             collapse = "+")))

#baggedTrees
control <- rfeControl(functions=treebagFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway3 <- rfe(pathway.scores, P, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pathway3)
# list the chosen features
predictors(results_pathway3)
# plot the results
plot(results_pathway3, type=c("g", "o"),xlim=c(0,30))
# generate formula
formula_pathway_bt <- as.formula(paste("P ~", 
                                       paste(predictors(results_pathway3), 
                                             collapse = "+")))

results_pc3 <- rfe(pc_scores, P, sizes=c(1:30), rfeControl=control)
# summarize the results
print(results_pc3)
# list the chosen features
predictors(results_pc3)
# plot the results
plot(results_pc3, type=c("g", "o"),xlim=c(0,30))
# generate formula
formula_pc_bt <- as.formula(paste("P ~", 
                                  paste(predictors(results_pc3), 
                                        collapse = "+")))

#removing multicolinearity
correlationMatrix <- cor(pathway.scores)
highlyCorrelated <- findCorrelation(correlationMatrix,cutoff = 0.5)
pathway_surv_cor <- pathway_surv[,-highlyCorrelated]
pathway.scores_cor <- pathway.scores[,-highlyCorrelated]

correlationMatrix <- cor(pc_scores)
highlyCorrelated <- findCorrelation(correlationMatrix,cutoff = 0.5)
pc_surv_cor <- pc_surv[,-highlyCorrelated]
pc_scores_cor <- pc_scores[,-highlyCorrelated]

#Random Forests
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway_cor <- rfe(pathway.scores_cor, P, sizes=c(1:55), rfeControl=control)
# summarize the results
print(results_pathway_cor)
# list the chosen features
predictors(results_pathway_cor)
# plot the results
plot(results_pathway_cor, type=c("g", "o"),xlim=c(0,55))
# generate formula
formula_pathway_rf_cor <- as.formula(paste("P ~", 
                                           paste(predictors(results_pathway_cor), 
                                                 collapse = "+")))

results_pc_cor <- rfe(pc_scores_cor, P, sizes=c(1:55), rfeControl=control)
# summarize the results
print(results_pc_cor)
# list the chosen features
predictors(results_pc_cor)
# plot the results
plot(results_pc_cor, type=c("g", "o"),xlim=c(0,55))
# generate formula
formula_pc_rf_cor <- as.formula(paste("P ~", 
                                      paste(predictors(results_pc_cor), 
                                            collapse = "+")))

#boruta
library(Boruta)
boruta_path_cor <- Boruta(P~.,data = pathway_surv_cor)
boruta_pc_cor <- Boruta(P~.,data = pc_surv_cor)

#confirmed and tentative
boruta_path1_cor <- names(boruta_path_cor$finalDecision[boruta_path_cor$finalDecision %in% 
                                                  c("Confirmed", "Tentative")])

formula_pathway_b1_cor <- as.formula(paste("P ~", paste(boruta_path1_cor, collapse = "+")))

boruta_pc1_cor <- names(boruta_pc_cor$finalDecision[boruta_pc_cor$finalDecision %in% 
                                              c("Confirmed", "Tentative")])

formula_pc_b1_cor <- as.formula(paste("P ~", paste(boruta_pc1_cor, collapse = "+")))

#confirmed
boruta_path2_cor <- names(boruta_path_cor$finalDecision[boruta_path_cor$finalDecision %in% 
                                                  c("Confirmed")])

formula_pathway_b2_cor <- as.formula(paste("P ~", paste(boruta_path2_cor, collapse = "+")))

boruta_pc2_cor <- names(boruta_pc_cor$finalDecision[boruta_pc_cor$finalDecision %in% 
                                              c("Confirmed")])
formula_pc_b2_cor <- as.formula(paste("P ~", paste(boruta_pc2_cor, collapse = "+")))


#lmfuncs
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway2_cor <- rfe(pathway.scores_cor, P, sizes=c(1:55), rfeControl=control)
# summarize the results
print(results_pathway2_cor)
# list the chosen features
predictors(results_pathway2_cor)
# plot the results
plot(results_pathway2_cor, type=c("g", "o"),xlim=c(0,55))
# generate formula
formula_pathway_lm_cor <- as.formula(paste("P ~", 
                                       paste(predictors(results_pathway2_cor), 
                                             collapse = "+")))

results_pc2_cor <- rfe(pc_scores_cor, P, sizes=c(1:55), rfeControl=control)
# summarize the results
print(results_pc2_cor)
# list the chosen features
predictors(results_pc2_cor)
# plot the results
plot(results_pc2_cor, type=c("g", "o"),xlim=c(0,55))
# generate formula
formula_pc_lm_cor <- as.formula(paste("P ~", 
                                  paste(predictors(results_pc2_cor), 
                                        collapse = "+")))

#baggedTrees
control <- rfeControl(functions=treebagFuncs, method="cv", number=10)
# run the RFE algorithm
results_pathway3_cor <- rfe(pathway.scores_cor, P, sizes=c(1:55), rfeControl=control)
# summarize the results
print(results_pathway3_cor)
# list the chosen features
predictors(results_pathway3_cor)
# plot the results
plot(results_pathway3_cor, type=c("g", "o"),xlim=c(0,55))
# generate formula
formula_pathway_bt_cor <- as.formula(paste("P ~", 
                                       paste(predictors(results_pathway3_cor), 
                                             collapse = "+")))

results_pc3_cor <- rfe(pc_scores_cor, P, sizes=c(1:55), rfeControl=control)
# summarize the results
print(results_pc3_cor)
# list the chosen features
predictors(results_pc3_cor)
# plot the results
plot(results_pc3_cor, type=c("g", "o"),xlim=c(0,55))
# generate formula
formula_pc_bt_cor <- as.formula(paste("P ~", 
                                  paste(predictors(results_pc3_cor), 
                                        collapse = "+")))
