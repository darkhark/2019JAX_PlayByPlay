# Use clustering to determine how well the clustering
# matches the quality of the play or if there are other 
# clusters that are not obvious

offense = read.csv("../../data/runOffenseNormalized.csv")
defense = read.csv("../../data/runDefenseNormalized.csv")

offense = subset(offense, select = -c(X))
defense = subset(defense, select = -c(X))
######## Offense ###########
set.seed(32)
factoextra::fviz_nbclust(
  x = offense,
  FUNcluster = kmeans,
  method = "wss"
)

factoextra::fviz_nbclust(
  x = offense,
  FUNcluster = kmeans,
  method = "silhouette"
)
# 4 clusters is marked as the optimal cluster size.
# Check further

offense_clusGap_kmeans <- cluster::clusGap(
  x = offense,
  FUNcluster = kmeans,
  K.max = 40,
  iter.max = 50
)

# Plot those results
factoextra::fviz_gap_stat(
  gap_stat = offense_clusGap_kmeans
)

# 4 seems like a the consensus of clusters.
# 8 is another possibility

factoextra::fviz_nbclust(
  x = offense,
  FUNcluster = kmeans,
  method = "gap_stat"
)

# The above states 4 clusters would be enough,
# so I'll do 4 instead of 5. Maybe two of the
# qualities are similar.

############## offense k = 4 ###########
offense_kmeans4 = kmeans(
  x = offense,
  centers = 4
)

# basic view
offense_kmeans4

offense_PC = data.frame(
  prcomp(
    x = offense, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Cluster = as.character(offense_kmeans4$cluster),
  stringsAsFactors = FALSE
)

require(ggplot2)
require(ggforce)

useful::plot.kmeans(
  x = offense_kmeans4,
  data = offense
)

fpc::plotcluster(
  x=offense,
  clvecd = offense_kmeans4$cluster
)

offense_clara <- cluster::clara(
  x = offense,
  k = 4
)
plot(offense_clara)
print(offense_clara)

######## offense k = 8 ##############
offense_kmeans8 = kmeans(
  x = offense,
  centers = 8
)

# basic view
offense_kmeans8

offense_PC8 = data.frame(
  prcomp(
    x = offense, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Cluster = as.character(offense_kmeans8$cluster),
  stringsAsFactors = FALSE
)

useful::plot.kmeans(
  x = offense_kmeans8,
  data = offense
)

fpc::plotcluster(
  x=offense,
  clvecd = offense_kmeans8$cluster
)

offense_clara8 <- cluster::clara(
  x = offense,
  k = 8
)
plot(offense_clara8)
print(offense_clara8)

####### Defense #############
factoextra::fviz_nbclust(
  x = defense,
  FUNcluster = kmeans,
  method = "wss"
)

factoextra::fviz_nbclust(
  x = defense,
  FUNcluster = kmeans,
  method = "silhouette"
)
# 4 clusters is marked as the optimal cluster size.
# Check further

defense_clusGap_kmeans <- cluster::clusGap(
  x = defense,
  FUNcluster = kmeans,
  K.max = 40,
  iter.max = 50
)

# Plot those results
factoextra::fviz_gap_stat(
  gap_stat = defense_clusGap_kmeans
)

# 4 seems like a decent number of clusters

factoextra::fviz_nbclust(
  x = defense,
  FUNcluster = kmeans,
  method = "gap_stat",
  iter.max = 50
)

# The above states 6 clusters 

############## defense k = 4 ###########
defense_kmeans4 = kmeans(
  x = defense,
  centers = 4
)

# basic view
defense_kmeans4

defense_PC = data.frame(
  prcomp(
    x = defense, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Cluster = as.character(defense_kmeans4$cluster),
  stringsAsFactors = FALSE
)

useful::plot.kmeans(
  x = defense_kmeans4,
  data = defense
)

fpc::plotcluster(
  x=defense,
  clvecd = defense_kmeans4$cluster
)

defense_clara4 <- cluster::clara(
  x = defense,
  k = 4
)
plot(defense_clara4)
print(defense_clara4)

######## defense k = 6 ##############
defense_kmeans6 = kmeans(
  x = defense,
  centers = 6
)

# basic view
defense_kmeans6

defense_PC6 = data.frame(
  prcomp(
    x = defense, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Cluster = as.character(defense_kmeans6$cluster),
  stringsAsFactors = FALSE
)

useful::plot.kmeans(
  x = defense_kmeans6,
  data = defense
)

fpc::plotcluster(
  x=defense,
  clvecd = defense_kmeans6$cluster
)

defense_clara6 <- cluster::clara(
  x = defense,
  k = 6
)
plot(defense_clara6)
print(defense_clara6)
