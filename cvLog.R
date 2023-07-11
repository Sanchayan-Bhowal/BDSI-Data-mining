pathway_naive_class <- train(Y_class~., data = pathway_surv1_train_class,
                        method = 'glmnet', 
                        trControl = ctrl,
                        family = 'binomial' )

predictions_pathway_naive_class <- data.frame(obs = factor(Y_class_test),
  pred = factor(pathway_naive_class %>% predict(pathway_surv1_test_class)))
pathway_naive_sum_class = twoClassSummary(predictions_pathway_naive_class,lev = c("0","1"))

pathway_rf_class <- train(formula_pathway_rf_class, data = pathway_surv_train_class,
      method = 'glmnet', 
      trControl = ctrl,
      family = 'binomial' )

predictions_pathway_rf_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pathway_rf_class %>% predict(pathway_surv_test_class)))
pathway_rf_sum_class = twoClassSummary(predictions_pathway_rf_class,lev = c("0","1"))

pathway_b1_class <- train(formula_pathway_b1_class, data = pathway_surv_train_class,
                     method = 'glmnet', 
                     trControl = ctrl,
                     family = 'binomial' )

predictions_pathway_b1_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pathway_b1_class %>% predict(pathway_surv_test_class)))
pathway_b1_sum_class = twoClassSummary(predictions_pathway_b1_class,lev = c("0","1"))

pathway_b2_class <- train(formula_pathway_b2_class, data = pathway_surv_train_class,
                     method = 'glmnet', 
                     trControl = ctrl,
                     family = 'binomial' )

predictions_pathway_b2_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pathway_b2_class %>% predict(pathway_surv_test_class)))
pathway_b2_sum_class = twoClassSummary(predictions_pathway_b2_class,lev = c("0","1"))


pathway_lm_class <- train(formula_pathway_lm_class, data = pathway_surv_train_class,
      method = 'glmnet', 
      trControl = ctrl,
      family = 'binomial' )

predictions_pathway_lm_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pathway_lm_class %>% predict(pathway_surv_test_class)))
pathway_lm_sum_class = twoClassSummary(predictions_pathway_lm_class,lev = c("0","1"))


pathway_bt_class <- train(formula_pathway_bt_class, data = pathway_surv_train_class,
      method = 'glmnet', 
      trControl = ctrl,
      family = 'binomial' )

predictions_pathway_bt_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pathway_bt_class %>% predict(pathway_surv_test_class)))
pathway_bt_sum_class = twoClassSummary(predictions_pathway_bt_class,lev = c("0","1"))


pc_naive_class <- train(Y_class~., data = pc_surv1_train_class,
                        method = 'glmnet', 
                        trControl = ctrl,
                        family = 'binomial' )

predictions_pc_naive_class <- data.frame(obs = factor(Y_class_test),
  pred = factor(pc_naive_class %>% predict(pc_surv1_test_class)))
pc_naive_sum_class = twoClassSummary(predictions_pc_naive_class,lev = c("0","1"))

pc_rf_class <- train(formula_pc_rf_class, data = pc_surv_train_class,
      method = 'glmnet', 
      trControl = ctrl,
      family = 'binomial' )

predictions_pc_rf_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pc_rf_class %>% predict(pc_surv_test_class)))
pc_rf_sum_class = twoClassSummary(predictions_pc_rf_class,lev = c("0","1"))

pc_b1_class <- train(formula_pc_b1_class, data = pc_surv_train_class,
                     method = 'glmnet', 
                     trControl = ctrl,
                     family = 'binomial' )

predictions_pc_b1_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pc_b1_class %>% predict(pc_surv_test_class)))
pc_b1_sum_class = twoClassSummary(predictions_pc_b1_class,lev = c("0","1"))

pc_b2_class <- train(formula_pc_b2_class, data = pc_surv_train_class,
                     method = 'glmnet', 
                     trControl = ctrl,
                     family = 'binomial' )

predictions_pc_b2_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pc_b2_class %>% predict(pc_surv_test_class)))
pc_b2_sum_class = twoClassSummary(predictions_pc_b2_class,lev = c("0","1"))


pc_lm_class <- train(formula_pc_lm_class, data = pc_surv_train_class,
      method = 'glmnet', 
      trControl = ctrl,
      family = 'binomial' )

predictions_pc_lm_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pc_lm_class %>% predict(pc_surv_test_class)))
pc_lm_sum_class = twoClassSummary(predictions_pc_lm_class,lev = c("0","1"))


pc_bt_class <- train(formula_pc_bt_class, data = pc_surv_train_class,
      method = 'glmnet', 
      trControl = ctrl,
      family = 'binomial' )

predictions_pc_bt_class <- data.frame(obs = factor(Y_class_test),
                                         pred = factor(pc_bt_class %>% predict(pc_surv_test_class)))
pc_bt_sum_class = twoClassSummary(predictions_pc_bt_class,lev = c("0","1"))
