# Use dimension reduction first to eliminate future computation as much as possible and 
# distinguish which variables are actually helpful for determining the quality of the play.

set.seed(32)

jaxRunOffense_All = read.csv("../../data/jaxRunOffense.csv")
jaxRunDefense_All = read.csv("../../data/jaxRunDefense.csv")

jaxRunOffense_All = jaxRunOffense_All[order(jaxRunOffense_All$Quality, decreasing = FALSE),]
jaxRunDefense_All = jaxRunDefense_All[order(jaxRunDefense_All$Quality, decreasing = TRUE),]

yOffense = jaxRunOffense_All$Quality
yDefense = jaxRunDefense_All$Quality

# Discovered through analysis -
# Safeties are too rare to provide any information. Need to be removed
# Fourth downs are too rare for defense to provide any information. Need to be removed
# jaxRunDefense_All = jaxRunDefense_All[!(jaxRunDefense_All$down == 4),]
jaxRunOffense = subset(jaxRunOffense_All, select = -c(X, Quality, safety))
jaxRunDefense = subset(jaxRunDefense_All, select = -c(X, Quality, safety))

categoricalColumns = c("down", "run_location", "run_gap", "td_team")

dummyCategoricals = function(df) {
  df = fastDummies::dummy_cols(
    df,
    remove_first_dummy = TRUE,
    select_columns = categoricalColumns
  )
  df = subset(df, select = -c(down, run_location, run_gap, td_team))
  return(df)
}

normalizeColumn = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalizeData = function(df) {
  for(col in 1:ncol(df)) {
    df[col] = normalizeColumn(df[col])
  }
  return(df)
}

offense = dummyCategoricals(jaxRunOffense)
defense = dummyCategoricals(jaxRunDefense)

offense_normalized = normalizeData(offense)
defense_normalized = normalizeData(defense)

# factorColumnsDefense = c("home_team", "goal_to_go", "shotgun", "no_huddle", "first_down_rush", 
#                          "incomplete_pass", "interception",
#                          "down_2", "down_3", "pass_length_short",
#                          "pass_location_middle", "pass_location_right",
#                          "td_team_1", "td_team_2")
# factorColumnsOffense = c("home_team", "goal_to_go", "shotgun", "no_huddle", "first_down_rush", 
#                          "incomplete_pass", "interception",
#                          "down_2", "down_3", "down_4", "pass_length_short",
#                          "pass_location_middle", "pass_location_right",
#                          "td_team_1")
# numericColumns = c("yardline_100", "game_seconds_remaining", "yards_gained", 
#                    "score_differential")
# 
# convertToFactorOrNumeric = function(df) {
#   if ("td_team_2" %in% colnames(df)) {
#     df[, factorColumnsDefense] = lapply(df[, factorColumnsDefense], as.factor)
#   } else {
#     df[, factorColumnsOffense] = lapply(df[, factorColumnsOffense], as.factor)
#   }
#   
#   df[, numericColumns] = lapply(df[, numericColumns], as.numeric)
#   return(df)
# }
# 
# offense_normalized = convertToFactorOrNumeric(offense)
# defense_normalized = convertToFactorOrNumeric(defense)
# 
# offense_discretized = lapply(
#   X = c("interval"),
#   FUN = function(method) discretize(
#     data = offense_normalized,
#     method = method,
#     breaks = 4,
#     ordered = TRUE
#   )
# )
# 
# defense_discretized = lapply(
#   X = c("interval"),
#   FUN = function(method) discretize(
#     data = defense_normalized,
#     method = method,
#     breaks = 4,
#     ordered = TRUE
#   )
# )
# 
# offense_discretized = offense_discretized[[1]]
# defense_discretized = defense_discretized[[1]]
# offense_discretized = sapply(offense_discretized, as.numeric)
# defense_discretized = sapply(defense_discretized, as.numeric)

###### Finally to the dimension reduction!!! ###########

offense_PC = prcomp(offense_normalized)
defense_PC = prcomp(defense_normalized)

summary(offense_PC)
summary(defense_PC)

# head(offense_PC)
# head(defense_PC)

require("ggfortify")
autoplot(
  object = offense_PC,
  data = jaxRunOffense_All,
  colour = "Quality"
)

autoplot(
  object = defense_PC,
  data = jaxRunDefense_All,
  colour = "Quality"
)

factoextra::fviz_pca_var(
  X = offense_PC,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

# For offense, first four dimensions are above double digits in variance. It take 11 PCs
# to reach 90% cumulative variance.
factoextra::get_eigenvalue(offense_PC)

offense_get_vars = factoextra::get_pca_var(offense_PC)
offense_get_vars$contrib

factoextra::fviz_pca_var(
  X = defense_PC,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

# For defense, first three dimensions are above double digits in variance. It take 11 PCs
# to reach 90% cumulative variance.
factoextra::get_eigenvalue(defense_PC)

defense_get_vars = factoextra::get_pca_var(defense_PC)
defense_get_vars$contrib

##### t-sne ########
require("Rtsne")
offense_tsne = Rtsne::Rtsne(offense_normalized)

offense_PC2 = prcomp(
  x = offense_normalized,
  scale. = TRUE,
  center = TRUE,
  rank = 2
)

plot(
  offense_PC2$x[,1:2],
  col = yOffense + 1,
  pch = as.character(yOffense),
  main = "Scatter Plot of Jax Run Offense PCA Two Dimensions"
)

offense_tsne2 = Rtsne::Rtsne(
  X = offense_normalized,
  dims = 2, 
  PCA = FALSE,
  max_iter = 2000,
  perplexity = 60
)

plot(
  offense_tsne2$Y,
  col = yOffense + 1,
  pch = as.character(yOffense),
  main = "Scatter Plot of Jax Run Offense T-SNE Two Dimensions"
)

defense_tsne = Rtsne::Rtsne(defense_normalized)

defense_PC2 = prcomp(
  x = defense_normalized,
  scale. = TRUE,
  center = TRUE,
  rank = 2
)

plot(
  defense_PC2$x[,1:2],
  col = yDefense + 1,
  pch = as.character(yDefense),
  main = "Scatter Plot of Jax Run Defense PCA Two Dimensions"
)

defense_tsne2 = Rtsne::Rtsne(
  X = defense_normalized,
  dims = 2, 
  PCA = FALSE,
  max_iter = 2000,
  perplexity = 60
)

plot(
  defense_tsne2$Y,
  col = yDefense + 1,
  pch = as.character(yDefense),
  main = "Scatter Plot of Jax Run Defense T-SNE Two Dimensions"
)

###### non-negative matrix ########
options(scipen = 1, digits = 2)

offense_nmf = NMF::nmf(
  x = offense_normalized,
  rank = 2,
  seed = 32
)

offense_basis = NMF::basis(offense_nmf)
offense_coef = NMF::coef(offense_nmf)

plot(
  x = offense_basis,
  col = yOffense + 1,
  pch = as.character(yOffense)
)

defense_nmf = NMF::nmf(
  x = defense_normalized,
  rank = 2,
  seed = 32
)

defense_basis = NMF::basis(defense_nmf)
offense_coef = NMF::coef(defense_nmf)

plot(
  x = defense_basis,
  col = yDefense + 1,
  pch = as.character(yDefense)
)

write.csv(offense_normalized, "../../data/runOffenseNormalized.csv")
write.csv(defense_normalized, "../../data/runDefenseNormalized.csv")
