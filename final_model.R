#summary of models
model_summary_path<-rbind(cv_model_path1$results,
    cv_model_path2$results,
    cv_model_path3$results,
    cv_model_path4$results,
    cv_model_path5$results,
    cv_model_path6$results)
model_summary_pc<- rbind(cv_model_pc1$results,
    cv_model_pc2$results,
    cv_model_pc3$results,
    cv_model_pc4$results,
    cv_model_pc5$results,
    cv_model_pc6$results)
#best model predictions
P_path=predict(cv_model_path4,pathway_surv)
P_pc=predict(cv_model_pc4,pc_surv)

#linear model
final_model=lm(P~P_path+P_pc)
#adjusting X for non-linearity
final_model=lm(P~I(P_path^2)+P_path+I(P_pc^2)+P_pc)
summary(final_model)

library(car)
#partial residue plots
crPlots(final_model)
library(ggResidpanel)
#diagnostic plots
resid_panel(final_model,smoother = T)
