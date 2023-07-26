library(caret)
library(Boruta)
library(dplyr)

N <- 10
bor_ct_coefs_path <- c()
bor_c_coefs_path <- c()
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
for (i in 1:N) {
  set.seed(i)
  if(i %% 10==0){
    print(i/10)
  }
  bor_keep <- sample(1:49,39)
  P_bor <- P_train[bor_keep]
  pathway_surv_bor <- pathway_surv_train[bor_keep,]
  pathway.scores_bor <- pathway.scores_train[bor_keep,]
  
  set.seed(07272023)
  boruta_path <- Boruta(P~.,data = pathway_surv_bor)

  #confirmed and tentative
  boruta_path1 <- names(boruta_path$finalDecision[boruta_path$finalDecision %in% 
                                                  c("Confirmed", "Tentative")])
  
  boruta_path2 <- names(boruta_path$finalDecision[boruta_path$finalDecision %in% 
                                                  c("Confirmed")])
  # list the chosen features
  bor_ct_coefs_path <- c(bor_ct_coefs_path,boruta_path1)
  bor_c_coefs_path <- c(bor_c_coefs_path,boruta_path2)
}
coefs_bor_ct_path=as.data.frame(bor_ct_coefs_path)
ggplot(coefs_bor_ct_path,aes(x = bor_ct_coefs_path)) +
  geom_bar()
coefs_bor_ct_path <- coefs_bor_ct_path %>% count(bor_ct_coefs_path)
cutoff <- as.numeric(quantile(coefs_bor_ct_path$n,probs = 0.8)) #fraction wanted
coefs_bor_ct_path <- coefs_bor_ct_path %>% filter(n>cutoff,bor_ct_coefs_path!="(Intercept)")

formula_pathway_bor_ct <- as.formula(paste("P ~", 
                                          paste(coefs_bor_ct_path$bor_ct_coefs_path, 
                                                collapse = "+")))
path_bor_ct<-train(formula_pathway_bor_ct, data = pathway_surv_train,
                  method = 'glmnet', 
                  tuneLength = 10,
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_path_bor_ct <- path_bor_ct %>% predict(pathway_surv_test)

path_bor_ct_sum = postResample(predictions_path_bor_ct, P_test)

coefs_bor_c_path=as.data.frame(bor_c_coefs_path)
ggplot(coefs_bor_c_path,aes(x = bor_c_coefs_path)) +
  geom_bar()
coefs_bor_c_path <- coefs_bor_c_path %>% count(bor_c_coefs_path)
cutoff <- as.numeric(quantile(coefs_bor_c_path$n,probs = 0.8)) #fraction wanted
coefs_bor_c_path <- coefs_bor_c_path %>% filter(n>cutoff,bor_c_coefs_path!="(Intercept)")

formula_pathway_bor_c <- as.formula(paste("P ~", 
                                          paste(coefs_bor_c_path$bor_c_coefs_path, 
                                                collapse = "+")))
path_bor_c<-train(formula_pathway_bor_c, data = pathway_surv_train,
                  method = 'glmnet', 
                  tuneLength = 10,
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_path_bor_c <- path_bor_c %>% predict(pathway_surv_test)
train_predictions_path_bor_c <- path_bor_c %>% predict(pathway_surv_train)

path_bor_c_sum = postResample(predictions_path_bor_c, P_test)

N <- 10
bor_ct_coefs_pc <- c()
bor_c_coefs_pc <- c()
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
for (i in 1:N) {
  set.seed(i)
  if(i %% 10==0){
    print(i/10)
  }
  bor_keep <- sample(1:49,39)
  P_bor <- P_train[bor_keep]
  pc_surv_bor <- pc_surv_train[bor_keep,]
  pc_scores_bor <- pc_scores_train[bor_keep,]
  
  set.seed(07272023)
  boruta_pc <- Boruta(P~.,data = pc_surv_bor)

  #confirmed and tentative
  boruta_pc1 <- names(boruta_pc$finalDecision[boruta_pc$finalDecision %in% 
                                                  c("Confirmed", "Tentative")])
  
  boruta_pc2 <- names(boruta_pc$finalDecision[boruta_pc$finalDecision %in% 
                                                  c("Confirmed")])
  # list the chosen features
  bor_ct_coefs_pc <- c(bor_ct_coefs_pc,boruta_pc1)
  bor_c_coefs_pc <- c(bor_c_coefs_pc,boruta_pc2)
}
coefs_bor_ct_pc=as.data.frame(bor_ct_coefs_pc)
ggplot(coefs_bor_ct_pc,aes(x = bor_ct_coefs_pc)) +
  geom_bar()
coefs_bor_ct_pc <- coefs_bor_ct_pc %>% count(bor_ct_coefs_pc)
cutoff <- as.numeric(quantile(coefs_bor_ct_pc$n,probs = 0.8)) #fraction wanted
coefs_bor_ct_pc <- coefs_bor_ct_pc %>% filter(n>cutoff,bor_ct_coefs_pc!="(Intercept)")

formula_pc_bor_ct <- as.formula(paste("P ~", 
                                          paste(coefs_bor_ct_pc$bor_ct_coefs_pc, 
                                                collapse = "+")))
pc_bor_ct<-train(formula_pc_bor_ct, data = pc_surv_train,
                  method = 'glmnet', 
                  tuneLength = 10,
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_pc_bor_ct <- pc_bor_ct %>% predict(pc_surv_test)

pc_bor_ct_sum = postResample(predictions_pc_bor_ct, P_test)

coefs_bor_c_pc=as.data.frame(bor_c_coefs_pc)
ggplot(coefs_bor_c_pc,aes(x = bor_c_coefs_pc)) +
  geom_bar()
coefs_bor_c_pc <- coefs_bor_c_pc %>% count(bor_c_coefs_pc)
cutoff <- as.numeric(quantile(coefs_bor_c_pc$n,probs = 0.8)) #fraction wanted
coefs_bor_c_pc <- coefs_bor_c_pc %>% filter(n>cutoff,bor_c_coefs_pc!="(Intercept)")

formula_pc_bor_c <- as.formula(paste("P ~", 
                                          paste(coefs_bor_c_pc$bor_c_coefs_pc, 
                                                collapse = "+")))
pc_bor_c<-train(formula_pc_bor_c, data = pc_surv_train,
                  method = 'glmnet', 
                  tuneLength = 10,
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_pc_bor_c <- pc_bor_c %>% predict(pc_surv_test)
train_predictions_pc_bor_c <- pc_bor_c %>% predict(pc_surv_train)

pc_bor_c_sum = postResample(predictions_pc_bor_c, P_test)

boruta_ct_coefs=c(coefs_bor_ct_path$bor_ct_coefs_path,
                  coefs_bor_ct_pc$bor_ct_coefs_pc)

boruta_c_coefs=c(coefs_bor_c_path$bor_c_coefs_path,
                  coefs_bor_c_pc$bor_c_coefs_pc)

formula_boruta_ct <- as.formula(paste("P ~", 
                                  paste(boruta_ct_coefs, 
                                        collapse = "+")))

formula_boruta_c <- as.formula(paste("P ~", 
                                  paste(boruta_c_coefs, 
                                        collapse = "+")))

final_en_boruta_ct <- train(formula_boruta_ct, data = full_train,
                               method = 'glmnet', 
                               tuneLength=10,
                               trControl = ctrl,
                               metric = "RMSE"
)

predictions_bor_ct <- final_en_boruta_ct %>% predict(full_test)

bor_ct_sum = postResample(predictions_bor_ct, P_test)

final_en_boruta_c <- train(formula_boruta_c, data = full_train,
                                   method = 'glmnet', 
                                   tuneLength=10,
                                   trControl = ctrl,
                                   metric = "RMSE"
)

predictions_bor_c <- final_en_boruta_c %>% predict(full_test)

bor_c_sum = postResample(predictions_bor_c, P_test)
