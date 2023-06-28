library(ggResidpanel)

# comparisons
resid_compare(list(model_pc1,model_pc2),
              c("resid","qq"),qqbands = T,smoother = T)
resid_compare(list(model_pathway1,model_pathway2),
              c("resid","qq"),qqbands = T,smoother = T)
