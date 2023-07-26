library(caret)

N <- 40
bt_coefs_path <- c()
control <- rfeControl(functions=treebagFuncs, method="cv", number=10)
for (i in 1:N) {
  set.seed(i)
  if(i %% 1==0){
    print(i/1)
  }
  bt_keep <- sample(1:49,39)
  P_bt <- P_train[bt_keep]
  pathway_surv_bt <- pathway_surv_train[bt_keep,]
  pathway.scores_bt <- pathway.scores_train[bt_keep,]
  
  set.seed(07272023)
  #Random Forests
  # run the RFE algorithm
  results_pathway_bt <- rfe(pathway.scores_bt, P_bt, sizes=c(1:30), rfeControl=control)
  # list the chosen features
  
  bt_coefs_path <- c(bt_coefs_path,predictors(results_pathway_bt))
}

coefs_bt_path=as.data.frame(bt_coefs_path)
ggplot(coefs_bt_path,aes(x = bt_coefs_path)) +
  geom_bar()
coefs_bt_path <- coefs_bt_path %>% count(bt_coefs_path)
cutoff <- as.numeric(quantile(coefs_bt_path$n,probs=0.8)) #fraction wanted
coefs_bt_path <- coefs_bt_path %>% filter(n>cutoff,bt_coefs_path!="(Intercept)")

formula_pathway_bt <- as.formula(paste("P ~", 
                                          paste(coefs_bt_path$bt_coefs_path, 
                                                collapse = "+")))
path_bt<-train(formula_pathway_bt, data = pathway_surv_train,
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_path_bt <- path_bt %>% predict(pathway_surv_test)
train_predictions_path_bt <- path_bt %>% predict(pathway_surv_train)

path_bt_sum = postResample(predictions_path_bt, P_test)

N <- 40
bt_coefs_pc <- c()
for (i in 1:N) {
  set.seed(i)
  if(i %% 1==0){
    print(i/1)
  }
  bt_keep <- sample(1:49,39)
  P_bt <- P_train[bt_keep]
  pc_surv_bt <- pc_surv_train[bt_keep,]
  pc_scores_bt <- pc_scores_train[bt_keep,]
  
  set.seed(07272023)
  #Random Forests
  # run the RFE algorithm
  results_pc_bt <- rfe(pc_scores_bt, P_bt, sizes=c(1:30), rfeControl=control)
  # list the chosen features
  
  bt_coefs_pc <- c(bt_coefs_pc,predictors(results_pc_bt))
}
coefs_bt_pc=as.data.frame(bt_coefs_pc)
ggplot(coefs_bt_pc,aes(x = bt_coefs_pc)) +
  geom_bar()
coefs_bt_pc <- coefs_bt_pc %>% count(bt_coefs_pc)
cutoff <- as.numeric(quantile(coefs_bt_pc$n,probs = 0.8)) #fraction wanted
coefs_bt_pc <- coefs_bt_pc %>% filter(n>cutoff,bt_coefs_pc!="(Intercept)")

formula_pc_bt <- as.formula(paste("P ~", 
                                          paste(coefs_bt_pc$bt_coefs_pc, 
                                                collapse = "+")))
pc_bt<-train(formula_pc_bt, data = pc_surv_train,
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_pc_bt <- pc_bt %>% predict(pc_surv_test)
train_predictions_pc_bt <- pc_bt %>% predict(pc_surv_train)

pc_bt_sum = postResample(predictions_pc_bt, P_test)

bt_coefs=c(coefs_bt_path$bt_coefs_path,coefs_bt_pc$bt_coefs_pc)
formula_bt <- as.formula(paste("P ~", 
                                  paste(bt_coefs, 
                                        collapse = "+")))

final_en_bt <- caret::train(formula_bt, data = full_train,
                                   method = 'glmnet', 
                                   tuneLength=10,
                                   trControl = ctrl,
                                   metric = "RMSE"
)

predictions_bt <- final_en_bt %>% predict(full_test)

bt_sum = postResample(predictions_bt, P_test)
