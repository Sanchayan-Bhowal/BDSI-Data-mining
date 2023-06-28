load("pc_scores.rda")
pc_scores <- as.data.frame(pc_scores)
mri_subregion=colnames(pc_scores)
length(mri_subregion[grepl("T1_NCR.NET",mri_subregion)])
length(mri_subregion[grepl("T2_NCR.NET",mri_subregion)])
length(mri_subregion[grepl("T1Gd_NCR.NET",mri_subregion)])
length(mri_subregion[grepl("T1_ED",mri_subregion)])
length(mri_subregion[grepl("T2_ED",mri_subregion)])
length(mri_subregion[grepl("T1Gd_ED",mri_subregion)])
length(mri_subregion[grepl("T1_ET",mri_subregion)])
length(mri_subregion[grepl("T2_ET",mri_subregion)])
length(mri_subregion[grepl("T1Gd_ET",mri_subregion)])

T2_ED <- pc_scores[,grepl("T2_ED",mri_subregion)]
t2_var = as.data.frame(apply(T2_ED,2,sd))
t2_var %>% 
  arrange(desc(apply(T2_ED, 2, sd)))

pc_score1=pc_scores %>% 
  select(T1_NCR.NET.1,T1_ED.1,T1_ET.1,
         T1Gd_NCR.NET.1,T1Gd_ED.1,T1Gd_ET.1,
         T2_NCR.NET.1,T2_ED.1,T2_ET.1,
         FLAIR_NCR.NET.1,FLAIR_ED.1,FLAIR_ET.1)
correlation_matrix=cor(pathway.scores,pc_score1)

most3.names=apply(correlation_matrix,2,
                  function(x){
                    return(names(sort(x,decreasing = T)[1:3]))})
most3.values=apply(correlation_matrix,2,
                   function(x){
                     return(sort(x,decreasing = T)[1:3])})

least3.names=apply(correlation_matrix,2,
                  function(x){
                    return(names(sort(x)[1:3]))})
least3.values=apply(correlation_matrix,2,
                   function(x){
                     return(sort(x)[1:3])})

gene_pathways=colnames(pathway.scores)

which_most_appear=Reduce(c,most3.names)
most_appear_time=sapply(which_most_appear, function(v) sum(grepl(v,which_most_appear)))

which_least_appear=Reduce(c,least3.names)
least_appear_time=sapply(which_least_appear, function(v) sum(grepl(v,which_least_appear)))

most_appear_time=as.data.frame(most_appear_time)
most_appear_time %>% 
  ggplot()+
  aes(which_most_appear)+
  geom_histogram()
  