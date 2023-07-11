which_rows=which((Y<17.48) | (Y>37.48))
Y_class=Y[which_rows]
Y_class[Y_class<17.48]=0
Y_class[Y_class>37.48]=1
Y_class

pc_surv_class=pc_surv[which_rows,]
pc_surv1_class=cbind(pc_surv1[which_rows,] %>% select(-P),Y_class)
pc_scores_class=pc_scores[which_rows,]

pathway_surv_class=pathway_surv[which_rows,]
pathway_surv1_class=cbind(pathway_surv1[which_rows,] %>% select(-P),Y_class)
pathway.scores_class=pathway.scores[which_rows,]


removed <- sample(1:41,8)
Y_class_test=Y_class[removed]
Y_class_train <- Y_class[-removed]

pc_surv_class=as.data.frame(cbind(pc_scores_class,Y_class))
pc_surv_class$Y_class=as.factor(pc_surv_class$Y_class)
pc_surv_train_class <- pc_surv_class[-removed,]
pc_surv_test_class <- pc_surv_class[removed,]

pc_surv1_class$Y_class=as.factor(pc_surv1_class$Y_class)
pc_surv1_train_class <- pc_surv1_class[-removed,]
pc_surv1_test_class <- pc_surv1_class[removed,]

pathway_surv_class=as.data.frame(cbind(pathway.scores_class,Y_class))
pathway_surv_class$Y_class=as.factor(pathway_surv_class$Y_class)
pathway_surv_train_class <- pathway_surv_class[-removed,]
pathway_surv_test_class <- pathway_surv_class[removed,]

pathway_surv1_class$Y_class=as.factor(pathway_surv1_class$Y_class)
pathway_surv1_train_class <- pathway_surv1_class[-removed,]
pathway_surv1_test_class <- pathway_surv1_class[removed,]

pathway.scores_train_class <- pathway.scores_class[-removed,]
pathway.scores_test_class <- pathway.scores_class[removed,]

pc_scores_train_class <- pc_scores_class[-removed,]
pc_scores_test_class <- pc_scores_class[removed,]