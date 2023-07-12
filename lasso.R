set.seed(07272023)

ctrl <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(
  .fraction = c(seq(0, 1, by = 0.01))
)


lasso <- train(
  P ~ .,
  data = pathway_surv_train,
  method = 'lasso',
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tuneGrid
)

scores=data.frame()
for(i in 1:10){
#naive
path_naive <- train(P ~ ., data = pathway_surv1_train,
                    method = 'glmnet', 
                    tuneLength=10,
                    trControl = ctrl,
                    metric = "RMSE"
)

predictions_path_naive <- path_naive %>% predict(pathway_surv_test)
path_naive_sum = postResample(predictions_path_naive, P_test)
scores <- rbind(scores,path_naive_sum)
}

library(glmnet)

N <- 1000
all_coefs_path <- c()
for (j in 1:N) {
  set.seed(j)
  if(j %% 100==0){
    print(j/100)
  }
  lasso_keep <- sample(1:49,39)
  P_lasso <- P_train[lasso_keep]
  pathway_surv_lasso <- pathway_surv_train[lasso_keep,]
  pathway.scores_lasso <- pathway.scores_train[lasso_keep,]
  
  set.seed(07272023)
  lasso_path <- cv.glmnet(pathway.scores_lasso,P_lasso,alpha=1)
  best_lambda_path <- lasso_path$lambda.min
  
  lasso_path <- glmnet(pathway.scores_train,P_train,alpha = 1,lambda = best_lambda_path)
  coef_lasso_path <- coef(lasso_path)
  all_coefs_path <- c(all_coefs_path,row.names(coef_lasso_path)[coef_lasso_path@i])
}
coefs_lasso_path=as.data.frame(all_coefs_path)
ggplot(coefs_lasso_path,aes(x = all_coefs_path)) +
  geom_bar()
coefs_lasso_path <- coefs_lasso_path %>% count(all_coefs_path)
frac <- 0.2 #fraction wanted
coefs_lasso_path <- coefs_lasso_path %>% filter(n>(N*frac),all_coefs_path!="(Intercept)")

formula_pathway_lasso <- as.formula(paste("P ~", 
                 paste(coefs_lasso_path$all_coefs_path, 
                       collapse = "+")))
path_lasso<-train(formula_pathway_lasso, data = pathway_surv_train,
                   method = 'glmnet', 
                   tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                   trControl = ctrl,
                   metric = "RMSE"
)
predictions_path_lasso <- path_lasso %>% predict(pathway_surv_test)

path_lasso_sum = postResample(predictions_path_lasso, P_test)

N <- 1000
all_coefs_pc <- c()
for (j in 1:N) {
  set.seed(j)
  if(j %% 100==0){
    print(j/100)
  }
  lasso_keep <- sample(1:49,39)
  P_lasso <- P_train[lasso_keep]
  pc_surv_lasso <- pc_surv_train[lasso_keep,]
  pc_scores_lasso <- pc_scores_train[lasso_keep,]
  pc_scores_lasso <- as.matrix(pc_scores_lasso)
  
  set.seed(07272023)
  lasso_pc <- cv.glmnet(pc_scores_lasso,P_lasso,alpha=1)
  best_lambda_pc <- lasso_pc$lambda.min
  
  lasso_pc <- glmnet(pc_scores_train,P_train,alpha = 1,lambda = best_lambda_pc)
  coef_lasso_pc <- coef(lasso_pc)
  all_coefs_pc <- c(all_coefs_pc,row.names(coef_lasso_pc)[coef_lasso_pc@i])
}
coefs_lasso_pc=as.data.frame(all_coefs_pc)
ggplot(coefs_lasso_pc,aes(x = all_coefs_pc)) +
  geom_bar()
coefs_lasso_pc <- coefs_lasso_pc %>% count(all_coefs_pc)
frac <- 0.05 #fraction wanted
coefs_lasso_pc <- coefs_lasso_pc %>% filter(n>(N*frac),all_coefs_pc!="(Intercept)")

formula_pc_lasso <- as.formula(paste("P ~", 
                 paste(coefs_lasso_pc$all_coefs_pc, 
                       collapse = "+")))
path_lasso<-train(formula_pc_lasso, data = pc_surv_train,
                   method = 'glmnet', 
                   tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                   trControl = ctrl,
                   metric = "RMSE"
)
predictions_path_lasso <- path_lasso %>% predict(pc_surv_test)

path_lasso_sum = postResample(predictions_path_lasso, P_test)
