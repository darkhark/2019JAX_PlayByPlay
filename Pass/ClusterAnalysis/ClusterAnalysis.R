# Use clustering to determine how well the clustering
# matches the quality of the play or if there are other 
# clusters that are not obvious

offense = read.csv("../../data/passOffenseNormalized.csv")
defense = read.csv("../../data/passDefenseNormalized.csv")

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
# 9 clusters is marked as the optimal cluster size.
# Check further

offense_clusGap_kmeans <- cluster::clusGap(
  x = offense,
  FUNcluster = kmeans,
  K.max = 40
)

# Plot those results
factoextra::fviz_gap_stat(
  gap_stat = offense_clusGap_kmeans
)

# 9 seems like a decent number of clusters.
# The y has 5 categories so I will also try that.

factoextra::fviz_nbclust(
  x = offense,
  FUNcluster = kmeans,
  method = "gap_stat"
)

# The above states 4 clusters would be enough,
# so I'll do 4 instead of 5. Maybe two of the
# qualities are similar

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

######## offense k = 9 ##############
offense_kmeans9 = kmeans(
  x = offense,
  centers = 9
)

# basic view
offense_kmeans9

offense_PC9 = data.frame(
  prcomp(
    x = offense, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Cluster = as.character(offense_kmeans9$cluster),
  stringsAsFactors = FALSE
)

useful::plot.kmeans(
  x = offense_kmeans9,
  data = offense
)

fpc::plotcluster(
  x=offense,
  clvecd = offense_kmeans9$cluster
)

offense_clara9 <- cluster::clara(
  x = offense,
  k = 9
)
plot(offense_clara9)
print(offense_clara9)

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
# 9 clusters is marked as the optimal cluster size.
# Check further

defense_clusGap_kmeans <- cluster::clusGap(
  x = defense,
  FUNcluster = kmeans,
  K.max = 20
)

# Plot those results
factoextra::fviz_gap_stat(
  gap_stat = defense_clusGap_kmeans
)

# 9 seems like a decent number of clusters.
# The y has 5 categories so I will also try that.

factoextra::fviz_nbclust(
  x = defense,
  FUNcluster = kmeans,
  method = "gap_stat"
)

# The above states 4 clusters would be enough,
# so I'll do 4 instead of 5. Maybe two of the
# qualities are similar

############## offense k = 4 ###########
offense_kmeans4 = kmeans(
  x = defense,
  centers = 4
)

# basic view
offense_kmeans4

offense_PC = data.frame(
  prcomp(
    x = defense, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Cluster = as.character(defense_kmeans4$cluster),
  stringsAsFactors = FALSE
)

require(ggplot2)
require(ggforce)

useful::plot.kmeans(
  x = defense_kmeans4,
  data = defense
)

fpc::plotcluster(
  x=defense,
  clvecd = offense_kmeans4$cluster
)

offense_clara <- cluster::clara(
  x = defense,
  k = 4
)
plot(defense_clara)
print(defense_clara)

######## offense k = 9 ##############
defense_kmeans9 = kmeans(
  x = defense,
  centers = 9
)

# basic view
defense_kmeans9

defense_PC9 = data.frame(
  prcomp(
    x = defense, 
    center = FALSE,
    scale. = FALSE
  )$x[, 1:2],
  Cluster = as.character(defense_kmeans9$cluster),
  stringsAsFactors = FALSE
)

useful::plot.kmeans(
  x = defense_kmeans9,
  data = defense
)

fpc::plotcluster(
  x=defense,
  clvecd = defense_kmeans9$cluster
)

offense_clara9 <- cluster::clara(
  x = defense,
  k = 9
)
plot(defense_clara9)
print(defense_clara9)
