library(MASS)
library(dplyr)
set.seed(07272023)
pc_scores=as.data.frame(pc_scores)
pc_scores <- na.omit(pc_scores)
pathway.scores <- na.omit(pathway.scores)

#boxcox transformation
J<-as.data.frame(Y)
normalization <- preProcess(J,method="BoxCox")
P=predict(normalization,J)$Y
removed <- sample(1:61,12)
P_test=P[removed]
P_train <- P[-removed]

pc_surv=as.data.frame(cbind(pc_scores,P))
pc_surv_train <- pc_surv[-removed,]
pc_surv_test <- pc_surv[removed,]

pc_surv1_train <- pc_surv1[-removed,]
pc_surv1_test <- pc_surv1[removed,]

pathway_surv <- as.data.frame(cbind(pathway.scores,P))
pathway_surv_train <- pathway_surv[-removed,]
pathway_surv_test <- pathway_surv[removed,]

pathway_surv1_train <- pathway_surv1[-removed,]
pathway_surv1_test <- pathway_surv1[removed,]

pathway.scores_train <- pathway.scores[-removed,]
pathway.scores_test <- pathway.scores[removed,]

pc_scores_train <- pc_scores[-removed,]
pc_scores_test <- pc_scores[removed,]

save(P_train,P_test,
     pathway_surv,pc_surv,
     pathway_surv_train,pathway_surv_test,
     pc_surv_train,pc_surv_test,
     pathway.scores_train,pathway.scores_test,
     pc_scores_train,pc_scores_test,file = "trainTestData.rda")
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

pathway_surv1=as.data.frame(cbind(pathway.scores[,features_pathway1],P))


intercept_only_pathway <- lm(P ~ 1, data=pathway_surv1)

#define model with all predictors
all_pathway <- lm(P ~ ., data=pathway_surv1)

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
         FLAIR_NCR.NET.4,FLAIR_ED.4,FLAIR_ET.4,P)


intercept_only_pc <- lm(P ~ 1, data=pc_surv1)

#define model with all predictors
all_pc <- lm(P ~ ., data=pc_surv1)

#perform forward stepwise regression
model_pc1 <- step(all_pc, direction='backward', 
                scope=formula(all_pc), trace=0)

#feature selection 2

##genetics and survival
pathway_surv2 = pathway_surv %>% 
  dplyr::select(KEGG_RIG_I_LIKE_RECEPTOR_SIGNALING_PATHWAY,MODULE_457,
                MORF_RAD21,KEGG_RIBOFLAVIN_METABOLISM,GNF2_TTN,MORF_SART1,MYC_UP.V1_DN,MORF_TERF1,
                MODULE_440,MODULE_159,MORF_EIF3S6
                ,KEGG_VASOPRESSIN_REGULATED_WATER_REABSORPTION,KEGG_ENDOMETRIAL_CANCER,
                MODULE_382,HALLMARK_MYC_TARGETS_V1 ,MODULE_81,MORF_RAB11A ,MORF_DEK ,GNF2_EIF3S6 ,GLI1_UP.V1_UP  
                ,MODULE_183,KEGG_GLYOXYLATE_AND_DICARBOXYLATE_METABOLISM ,MORF_RPA1,GNF2_SPI1,P)



#define model with all predictors
model_pathway2 <- lm(P ~ ., data=pathway_surv2)

##imaging and survival
pc_surv2 = pc_surv %>% 
  dplyr::select(FLAIR_NCR.NET.17,T2_NCR.NET.4,T2_ED.1,
                  FLAIR_NCR.NET.5,T2_NCR.NET.7,T2_NCR.NET.9,P)


#define model with all predictors
model_pc2 <- lm(P ~ ., data=pc_surv2)

save(model_pathway1,model_pathway2,model_pc1,model_pc2,file="models.rda")

#feature selction 3

##genetics and survival
pathway_surv3 = pathway_surv %>% 
  dplyr::select(HALLMARK_MYC_TARGETS_V1,KEGG_RIBOFLAVIN_METABOLISM,
                KEGG_RIG_I_LIKE_RECEPTOR_SIGNALING_PATHWAY,KEGG_THYROID_CANCER,
                MORF_RAD21,MORF_RPA1,MORF_TERF1,
                MORF_PPP2CA,MORF_RAF1,MORF_SART1,MODULE_115,MODULE_159,P)



#define model with all predictors
model_pathway3 <- lm(P ~ ., data=pathway_surv3)

##imaging and survival
pc_surv3 = pc_surv %>% 
  dplyr::select(T2_NCR.NET.4,T2_ED.1,FLAIR_NCR.NET.5,FLAIR_NCR.NET.17,P)


#define model with all predictors
model_pc3 <- lm(P ~ ., data=pc_surv3)

#feature selction 4

##genetics and survival
pathway_surv4 = pathway_surv %>% 
  dplyr::select(HALLMARK_MYC_TARGETS_V1,KEGG_RIG_I_LIKE_RECEPTOR_SIGNALING_PATHWAY,
                MORF_RAD21,MORF_TERF1,MORF_SART1,MODULE_159,P)



#define model with all predictors
model_pathway4 <- lm(P ~ ., data=pathway_surv4)

##imaging and survival
pc_surv4 = pc_surv %>% 
  dplyr::select(T2_NCR.NET.4,T2_ED.1,FLAIR_NCR.NET.17,P)


#define model with all predictors
model_pc4 <- lm(P ~ ., data=pc_surv4)