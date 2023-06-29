library(MASS)
library(dplyr)
set.seed(7)
pc_surv=as.data.frame(cbind(pc_scores,Y))
pathway_surv <- as.data.frame(cbind(pathway.scores,Y))
pc_scores=as.data.frame(pc_scores)
pc_scores <- na.omit(pc_scores)
pathway.scores <- na.omit(pathway.scores)

#feature selection 1
##genetics and survival
names_pathway <- colnames(pathway.scores)
combos <- unique(gsub("\\_.*$", "", names_pathway))
combos <- unique(gsub("\\..*$", "", combos))

possi_feat_pathway=sapply(combos, function(combo) which(startsWith(names_pathway,combo)))
features_pathway1=c()

for (i in 1:length(possi_feat_pathway)) {
  if(length(possi_feat_pathway[[i]])>2)
  {
    features_pathway1=c(features_pathway1,sample(possi_feat_pathway[[i]],1)) 
  }
}

pathway_surv1=as.data.frame(cbind(pathway.scores[,features_pathway1],Y))

cox_model <- boxcox(Y~.,data=pathway_surv1)

lambda <- cox_model$x[which.max(cox_model$y)]

g <- function(x) (x^lambda-1)/lambda #box-cox transformation
intercept_only_pathway <- lm(g(Y) ~ 1, data=pathway_surv1)

#define model with all predictors
all_pathway <- lm(g(Y) ~ ., data=pathway_surv1)

#perform forward stepwise regression
model_pathway1 <- step(all_pathway, direction='backward', 
                  scope=formula(all_pathway), trace=0)

##imaging and survival
pc_surv1=pc_surv %>% 
  dplyr::select(T1_NCR.NET.1,T1_ED.1,T1_ET.1,
         T1Gd_NCR.NET.1,T1Gd_ED.1,T1Gd_ET.1,
         T2_NCR.NET.1,T2_ED.1,T2_ET.1,
         FLAIR_NCR.NET.1,FLAIR_ED.1,FLAIR_ET.1,T1_NCR.NET.2,T1_ED.2,T1_ET.2,
         T1Gd_NCR.NET.2,T1Gd_ED.2,T1Gd_ET.2,
         T2_NCR.NET.2,T2_ED.2,T2_ET.2,
         FLAIR_NCR.NET.2,FLAIR_ED.2,FLAIR_ET.2,T1_NCR.NET.3,T1_ED.3,T1_ET.3,
         T1Gd_NCR.NET.3,T1Gd_ED.3,T1Gd_ET.3,
         T2_NCR.NET.3,T2_ED.3,T2_ET.3,
         FLAIR_NCR.NET.3,FLAIR_ED.3,FLAIR_ET.3,T1_NCR.NET.4,T1_ED.4,T1_ET.4,
         T1Gd_NCR.NET.4,T1Gd_ED.4,T1Gd_ET.4,
         T2_NCR.NET.4,T2_ED.4,T2_ET.4,
         FLAIR_NCR.NET.4,FLAIR_ED.4,FLAIR_ET.4,Y)


cox_model <- boxcox(Y~.,data=pc_surv1)

lambda <- cox_model$x[which.max(cox_model$y)]

g <- function(x) (x^lambda-1)/lambda #box-cox transformation
intercept_only_pc <- lm(g(Y) ~ 1, data=pc_surv1)

#define model with all predictors
all_pc <- lm(g(Y) ~ ., data=pc_surv1)

#perform forward stepwise regression
model_pc1 <- step(all_pc, direction='backward', 
                scope=formula(all_pc), trace=0)

#feature selection 2

##genetics and survival
pathway_surv2 = pathway_surv %>% 
  dplyr::select(KEGG_RIG_I_LIKE_RECEPTOR_SIGNALING_PATHWAY,MODULE_457,
         MORF_RAD21,KEGG_RIBOFLAVIN_METABOLISM,GNF2_TTN,MORF_SART1,
         MYC_UP.V1_DN,MORF_TERF1,MODULE_440,MODULE_159,Y)

cox_model <- boxcox(Y~.,data=pathway_surv2)

lambda <- cox_model$x[which.max(cox_model$y)]

g <- function(x) (x^lambda-1)/lambda #box-cox transformation

#define model with all predictors
model_pathway2 <- lm(g(Y) ~ ., data=pathway_surv2)

##imaging and survival
pc_surv2 = pc_surv %>% 
  dplyr::select(FLAIR_NCR.NET.17, T2_NCR.NET.4, T2_ED.1, FLAIR_NCR.NET.5,Y)

cox_model <- boxcox(Y~.,data=pc_surv2)

lambda <- cox_model$x[which.max(cox_model$y)]

g <- function(x) (x^lambda-1)/lambda #box-cox transformation

#define model with all predictors
model_pc2 <- lm(g(Y) ~ ., data=pc_surv2)

save(model_pathway1,model_pathway2,model_pc1,model_pc2,file="models.rda")
