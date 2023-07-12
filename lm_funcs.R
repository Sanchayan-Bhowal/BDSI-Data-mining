library(caret)

N <- 10
lm_coefs_path <- c()
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
for (i in 1:N) {
  set.seed(i)
  if(i %% 1==0){
    print(i/1)
  }
  lm_keep <- sample(1:49,39)
  P_lm <- P_train[lm_keep]
  pathway_surv_lm <- pathway_surv_train[lm_keep,]
  pathway.scores_lm <- pathway.scores_train[lm_keep,]
  
  set.seed(07272023)
  #Random Forests
  # run the RFE algorithm
  results_pathway_lm <- rfe(pathway.scores_lm, P_lm, sizes=c(1:30), rfeControl=control)
  # list the chosen features
  
  lm_coefs_path <- c(lm_coefs_path,predictors(results_pathway_lm))
}
coefs_lm_path=as.data.frame(lm_coefs_path)
ggplot(coefs_lm_path,aes(x = lm_coefs_path)) +
  geom_bar()
coefs_lm_path <- coefs_lm_path %>% count(lm_coefs_path)
frac <- 0.2 #fraction wanted
coefs_lm_path <- coefs_lm_path %>% filter(n>(N*frac),lm_coefs_path!="(Intercept)")

formula_pathway_lm <- as.formula(paste("P ~", 
                                          paste(coefs_lm_path$lm_coefs_path, 
                                                collapse = "+")))
path_lm<-train(formula_pathway_lm, data = pathway_surv_train,
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_path_lm <- path_lm %>% predict(pathway_surv_test)

path_lm_sum = postResample(predictions_path_lm, P_test)

N <- 10
lm_coefs_pc <- c()
control <- rfeControl(functions=lmFuncs, method="cv", number=10)
for (i in 1:N) {
  set.seed(i)
  if(i %% 1==0){
    print(i/1)
  }
  lm_keep <- sample(1:49,39)
  P_lm <- P_train[lm_keep]
  pc_surv_lm <- pc_surv_train[lm_keep,]
  pc_scores_lm <- pc_scores_train[lm_keep,]
  
  set.seed(07272023)
  #Random Forests
  # run the RFE algorithm
  results_pc_lm <- rfe(pc_scores_lm, P_lm, sizes=c(1:30), rfeControl=control)
  # list the chosen features
  
  lm_coefs_pc <- c(lm_coefs_pc,predictors(results_pc_lm))
}
coefs_lm_pc=as.data.frame(lm_coefs_pc)
ggplot(coefs_lm_pc,aes(x = lm_coefs_pc)) +
  geom_bar()
coefs_lm_pc <- coefs_lm_pc %>% count(lm_coefs_pc)
frac <- 0.2 #fraction wanted
coefs_lm_pc <- coefs_lm_pc %>% filter(n>(N*frac),lm_coefs_pc!="(Intercept)")

formula_pc_lm <- as.formula(paste("P ~", 
                                          paste(coefs_lm_pc$lm_coefs_pc, 
                                                collapse = "+")))
pc_lm<-train(formula_pc_lm, data = pc_surv_train,
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                  trControl = ctrl,
                  metric = "RMSE"
)
predictions_pc_lm <- pc_lm %>% predict(pc_surv_test)

pc_lm_sum = postResample(predictions_pc_lm, P_test)