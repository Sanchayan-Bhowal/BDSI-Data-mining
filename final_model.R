#summary of models
model_summary_path<-rbind(cv_model_path1$results,
    cv_model_path2$results,
    cv_model_path3$results,
    cv_model_path4$results,
    cv_model_path5$results,
    cv_model_path6$results)

ridge_summary_path<-rbind(path_lasso_sum,
    path_rf_sum,
    path_bor_ct_sum,
    path_bor_c_sum,
    path_lm_sum,
    path_bt_sum)

model_summary_pc<- rbind(cv_model_pc1$results,
    cv_model_pc2$results,
    cv_model_pc3$results,
    cv_model_pc4$results,
    cv_model_pc5$results,
    cv_model_pc6$results)

ridge_summary_pc<-rbind(pc_lasso_sum,
    pc_rf_sum,
    pc_bor_ct_sum,
    pc_bor_c_sum,
    pc_lm_sum,
    pc_bt_sum)

en_summary <- rbind(lasso_sum,
                    bor_ct_sum,
                    bor_c_sum,
                    lm_sum,
                    bt_sum)
en_formulae <- rbind(formula_lasso,
                     formula_boruta_ct,
                     formula_boruta_c,
                     formula_lm,
                     formula_bt)

class_summary_path<-rbind(pathway_naive_sum_class,
                          pathway_rf_sum_class,
                          pathway_b1_sum_class,
                          pathway_b2_sum_class,
                          pathway_lm_sum_class,
                          pathway_bt_sum_class)

class_summary_pc<-rbind(pc_naive_sum_class,
                        pc_rf_sum_class,
                        pc_b1_sum_class,
                        pc_b2_sum_class,
                        pc_bt_sum_class)
save(ridge_summary_path,ridge_summary_pc,file = "ridge_summaries.rda")

#best model predictions
P_path_train=predict(path_bor_ct,pathway_surv_train)
P_pc_train=predict(pc_lasso,pc_surv_train)

P_path_test=predict(path_bor_ct,pathway_surv_test)
P_pc_test=predict(pc_lasso,pc_surv_test)

P_gi_test <- coef(final_model)["(Intercept)"]+coef(final_model)["P_path_train"]*P_path_test+
  coef(final_model)["P_pc_train"]*P_pc_test

#best model predictions
P_path_class=predict(pathway_lm_class,pathway_surv_class)
P_pc_class=predict(pc_naive_class,pc_surv_class)

#linear model
final_model=lm(P_train~P_path_train+P_pc_train)
#logit model
final_model_class=glm(Y_class~P_path_class+P_pc_class,family = "binomial")

#adjusting X for non-linearity
final_model=lm(P~P_path+I(asin(P_try)))
summary(final_model)

library(car)
#partial residue plots
crPlots(final_model)
library(ggResidpanel)
#diagnostic plots
resid_panel(final_model,smoother = T,qqbands = T)

#removing multicolinearity
#summary of models
model_summary_path_cor<-rbind(cv_model_path2_cor$results,
                          cv_model_path3_cor$results,
                          cv_model_path4_cor$results,
                          cv_model_path5_cor$results,
                          cv_model_path6_cor$results)
model_summary_pc_cor<- rbind(cv_model_pc2_cor$results,
                         cv_model_pc3_cor$results,
                         cv_model_pc4_cor$results,
                         cv_model_pc5_cor$results,
                         cv_model_pc6_cor$results)
#best model predictions
P_path_cor=predict(cv_model_path3_cor,pathway_surv_cor)
P_pc_cor=predict(cv_model_pc4_cor,pc_surv_cor)

#linear model
final_model_cor=lm(P~P_path_cor+P_pc_cor)
summary(final_model_cor)

library(car)
#partial residue plots
crPlots(final_model_cor)
library(ggResidpanel)
#diagnostic plots
resid_panel(final_model_cor,smoother = T,qqbands = T)

plot(as.numeric(Y_class)-1,col="red")
par(new=T)
plot(as.numeric(P_path_class)-1,col="blue")

ensemble_pc_model <- lm(P_train~train_predictions_pc_lasso+
                            train_predictions_pc_bor_c+
                            train_predictions_pc_bt+
                            train_predictions_pc_rf)

test_matrix_path <- as.matrix(cbind(predictions_path_lasso,
                                  predictions_path_bor_c,
                                  predictions_path_bt,
                                  predictions_path_rf))
calib_pred_path <- test_matrix_path-P_test
calib_pred_path[calib_pred_path>0] <- 1
calib_pred_path[calib_pred_path<0] <- 0
calib_pred_path <- t(calib_pred_path)
over_pred_path <- rowSums(calib_pred_path * t(test_matrix_path-P_test))

calib_pred_path <- test_matrix_path-P_test
calib_pred_path[calib_pred_path>0] <- 0
calib_pred_path[calib_pred_path<0] <- -1
calib_pred_path <- t(calib_pred_path)
under_pred_path <- rowSums(calib_pred_path * t(test_matrix_path-P_test))

test_matrix_pc <- as.matrix(cbind(predictions_pc_lasso,
                               predictions_pc_bor_c,
                               predictions_pc_bt,
                               predictions_pc_rf))
calib_pred_pc <- test_matrix_pc-P_test
calib_pred_pc[calib_pred_pc>0] <- 1
calib_pred_pc[calib_pred_pc<0] <- 0
calib_pred_pc <- t(calib_pred_pc)
over_pred_pc <- rowSums(calib_pred_pc * t(test_matrix_pc-P_test))

calib_pred_pc <- test_matrix_pc-P_test
calib_pred_pc[calib_pred_pc>0] <- 0
calib_pred_pc[calib_pred_pc<0] <- -1
calib_pred_pc <- t(calib_pred_pc)
under_pred_pc <- rowSums(calib_pred_pc * t(test_matrix_pc-P_test))

postResample((test_matrix %*% as.numeric(coef(ensemble_pc_model))[-1])+
               as.numeric(coef(ensemble_pc_model))[1],P_test)
