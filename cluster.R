#elbow plot
fviz_nbclust(pathway.scores, kmeans, method = "wss")

gap_stat <- clusGap(pathway.scores,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
fviz_gap_stat(gap_stat)

set.seed(07272023)

km_pc <- kmeans(pc_scores, centers = 2, nstart = 25)

fviz_cluster(km_pc, data = pc_scores)

km_path <- kmeans(pathway.scores, centers = 2, nstart = 25)

fviz_cluster(km_path, data = pathway.scores)


dist_path <- dist(pathway.scores, method = 'euclidean')

# Fitting Hierarchical clustering Model
hcl_path <- hclust(dist_path, method = "average")


# Plotting dendrogram
plot(hcl_path)

dist_pc <- dist(pc_scores, method = 'euclidean')

# Fitting Hierarchical clustering Model
hcl_pc <- hclust(dist_pc, method = "average")


# Plotting dendrogram
plot(hcl_pc)
