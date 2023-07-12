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
all_coefs <- c()
for (i in 1:1000) {
  set.seed(i)
  if(i %% 100==0){
    print(i/100)
  }
  
  lasso <- cv.glmnet(pathway.scores_train,P_train,alpha=1)
  best_lambda <- lasso$lambda.min
  
  lasso <- glmnet(pathway.scores_train,P_train,alpha = 1,lambda = best_lambda)
  coef_lasso <- coef(lasso)
  all_coefs <- c(all_coefs,row.names(coef_lasso)[coef_lasso@i])
}
coefs_lasso=as.data.frame(all_coefs)
ggplot(coefs_lasso,aes(x = all_coefs)) +
  geom_bar()
coefs_lasso <- coefs_lasso %>% count(all_coefs)
frac <- 0.1 #fraction wanted
coefs_lasso <- coefs_lasso %>% filter(n>(N*frac),all_coefs!="(Intercept)")

formula_pathway_lasso <- as.formula(paste("P ~", 
                 paste(coefs_lasso$all_coefs, 
                       collapse = "+")))
path_lasso<-train(formula_pathway_lasso, data = pathway_surv_train,
                   method = 'glmnet', 
                   tuneGrid = expand.grid(alpha = 0, lambda = parameters),
                   trControl = ctrl,
                   metric = "RMSE"
)
predictions_path_lasso <- path_lasso %>% predict(pathway_surv_test)

path_lasso_sum = postResample(predictions_path_lasso, P_test)
