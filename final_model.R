#summary of models
model_summary_path<-rbind(cv_model_path1$results,
    cv_model_path2$results,
    cv_model_path3$results,
    cv_model_path4$results,
    cv_model_path5$results,
    cv_model_path6$results)

ridge_summary_path<-rbind(path_naive_sum,
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

ridge_summary_pc<-rbind(pc_naive_sum,
    pc_rf_sum,
    pc_bor_ct_sum,
    pc_bor_c_sum,
    pc_lm_sum,
    pc_bt_sum)

#best model predictions
P_path=predict(path_bt,pathway_surv)
P_pc=predict(pc_lm,pc_surv)

#linear model
final_model=lm(P~P_path*P_pc)
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

