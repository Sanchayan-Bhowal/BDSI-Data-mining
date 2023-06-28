load("survival.RData")
length(which((Y > 60))) # patients survival greater than 60
median(Y) # median of survival time
library(ggplot2)
windows()
p <- ggplot(data = as.data.frame(Y), aes(y = Y)) +
    geom_boxplot()
print(p) # boxplot with outliers
length(which(is.na(Y))) # find number of NAss