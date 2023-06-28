load("pathway_scores.RData")
pathway.scores <- as.data.frame(pathway.scores)
length(colnames(pathway.scores))
which.max(apply(pathway.scores, 2, var)) # most variation at GNF2_MKI67
which.min(apply(pathway.scores, 2, var)) # most variation at MODULE_136
kegg_cor <- function(v) {
    return(cor(pathway.scores$KEGG_DRUG_METABOLISM_CYTOCHROME_P450, v))
}
deleted_score <- pathway.scores[
    ,
    !names(pathway.scores) %in% ("KEGG_DRUG_METABOLISM_CYTOCHROME_P450")
]
which.max(apply(deleted_score, 2, kegg_cor))
# KEGG_METABOLISM_OF_XENOBIOTICS_BY_CYTOCHROME_P450 has most correlation
load("survival.RData")
subset_score <- pathway.scores[which(Y > 10), ]
surv_cor <- function(v) {
    return(cor(Y[Y > 10], v))
}
which.max(apply(subset_score, 2, surv_cor)) # MODULE_457

lower <- pathway.scores[which(Y < quantile(Y, probs = c(0.25))), 1:10]
upper <- pathway.scores[which(Y > quantile(Y, probs = c(0.75))), 1:10]

lower_mean <- apply(lower, 2, mean)
upper_mean <- apply(upper, 2, mean)