g <- function(x) log(x) #link function
pc_surv=as.data.frame(cbind(pc_scores,Y))
pc_scores=as.data.frame(pc_scores)
pc_scores <- na.omit(pc_scores)
pathway.scores <- na.omit(pathway.scores)
pc_surv1=pc_surv %>% 
  select(T1_NCR.NET.1,T1_ED.1,T1_ET.1,
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

intercept_only <- lm(g(Y) ~ 1, data=pc_surv1)

#define model with all predictors
all <- lm(g(Y) ~ ., data=pc_surv1)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)

#perform backward stepwise regression
backward <- step(intercept_only, direction='backward', scope=formula(all), trace=0)

pc_surv1=pc_surv %>% 
  select(T1_NCR.NET.1,T1_ED.1,T1_ET.1,
         T1Gd_NCR.NET.1,T1Gd_ED.1,T1Gd_ET.1,
         T2_NCR.NET.1,T2_ED.1,T2_ET.1,Y)

importance=varImp(all)

importance %>% 
  arrange(desc(Overall))

importance %>% 
  ggplot() +
  aes(x=as.factor(names),y=Overall) +
  geom_bar(stat = "identity")

imp_model <- lm(g(Y)~T2_ED.4+T2_ED.1+T2_ET.4+T1_ED.3+T1_NCR.NET.3+
                  T1Gd_NCR.NET.3+FLAIR_ED.1+T1Gd_ED.2+
                  T2_ED.3+FLAIR_ED.3+T1Gd_ET.2+T1Gd_NCR.NET.2+T1_ET.1+
                  T2_ET.3+FLAIR_ET.1+T1Gd_ET.1,data=pc_surv1)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(imp_model), trace=0)

#perform backward stepwise regression
backward <- step(intercept_only, direction='backward', scope=formula(imp_model), trace=0)

library(MASS)

cox_model <- boxcox(Y~.,data=pc_surv1)

lambda <- cox_model$x[which.max(cox_model$y)]

g <- function(x) (x^lambda-1)/lambda #box-cox transformation
