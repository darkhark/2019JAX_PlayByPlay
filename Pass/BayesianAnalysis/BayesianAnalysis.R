# Bayesian models for Passing

# Use dimension reduction first to eliminate future computation as much as possible and 
# distinguish which variables are actually helpful for determining the quality of the play.
set.seed(32)

jaxPassOffense_All = read.csv("../../data/jaxPassOffense.csv")
jaxPassDefense_All = read.csv("../../data/jaxPassDefense.csv")

jaxPassOffense_All = jaxPassOffense_All[order(jaxPassOffense_All$Quality, decreasing = FALSE),]
jaxPassDefense_All = jaxPassDefense_All[order(jaxPassDefense_All$Quality, decreasing = TRUE),]

# Discovered through analysis -
# Safeties are too rare to provide any information. Need to be removed
# Fourth downs are too rare for defense to provide any information. Need to be removed
jaxPassDefense_All = jaxPassDefense_All[!(jaxPassDefense_All$down == 4),]
jaxPassOffense = subset(jaxPassOffense_All, select = -c(X, safety))
jaxPassDefense = subset(jaxPassDefense_All, select = -c(X, safety))


categoricalColumns = c("down", "pass_length", "pass_location", "td_team", "Quality")

dummyCategoricals = function(df) {
  df = fastDummies::dummy_cols(
    df,
    remove_first_dummy = TRUE,
    select_columns = categoricalColumns
  )
  df = subset(df, select = -c(down, pass_length, pass_location, td_team, Quality))
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

offense = dummyCategoricals(jaxPassOffense)
defense = dummyCategoricals(jaxPassDefense)

offense_normalized = normalizeData(offense)
defense_normalized = normalizeData(defense)

factorColumnsDefense = c("home_team", "goal_to_go", "shotgun", "no_huddle", "first_down_pass",
                         "incomplete_pass", "interception",
                         "down_2", "down_3", "pass_length_short",
                         "pass_location_middle", "pass_location_right",
                         "td_team_1", "td_team_2", "Quality_1", "Quality_2",
                         "Quality_3", "Quality_4")
factorColumnsOffense = c("home_team", "goal_to_go", "shotgun", "no_huddle", "first_down_pass",
                         "incomplete_pass", "interception",
                         "down_2", "down_3", "down_4", "pass_length_short",
                         "pass_location_middle", "pass_location_right",
                         "td_team_1", "Quality_1", "Quality_2",
                         "Quality_3", "Quality_4")
numericColumns = c("yardline_100", "game_seconds_remaining", "yards_gained",
                   "score_differential")

convertToFactorOrNumeric = function(df) {
  if ("td_team_2" %in% colnames(df)) {
    df[, factorColumnsDefense] = lapply(df[, factorColumnsDefense], as.factor)
  } else {
    df[, factorColumnsOffense] = lapply(df[, factorColumnsOffense], as.factor)
  }

  df[, numericColumns] = lapply(df[, numericColumns], as.numeric)
  return(df)
}

offense_normalized = convertToFactorOrNumeric(offense)
defense_normalized = convertToFactorOrNumeric(defense)

offense_discretized = lapply(
  X = c("interval", "quantile"),
  FUN = function(method) discretize(
    data = offense_normalized,
    method = method,
    breaks = 4,
    ordered = TRUE
  )
)

defense_discretized = lapply(
  X = c("interval", "quantile"),
  FUN = function(method) discretize(
    data = defense_normalized,
    method = method,
    breaks = 4,
    ordered = TRUE
  )
)

names(offense_discretized) = c("interval", "quantile")
lapply(X = offense_discretized, FUN = summary)

names(defense_discretized) = c("interval", "quantile")
lapply(X = defense_discretized, FUN = summary)

require("Rgraphviz")

#### Offense ####
all4Algorithms = c("hc", "iamb.fdr", "h2pc", "aracne")
offbnlearnList = list()

for(j in all4Algorithms) for(k in names(offense_discretized)) try({
  offbnlearnList[[j]][[k]] <- do.call(
    what = j,
    args = list(x = offense_discretized[[k]])
  )
  M_arcs <- arcs(offbnlearnList[[j]][[k]])
  for(l in 1:nrow(M_arcs)){
    offbnlearnList[[j]][[k]] <- set.arc(
      x = offbnlearnList[[j]][[k]],
      from = M_arcs[l,1],
      to = M_arcs[l,2],
      check.cycles = FALSE,
      check.illegal = FALSE
    )
    offbnlearnList[[j]][[k]] <- choose.direction(
      x = offbnlearnList[[j]][[k]],
      arc = M_arcs[l,],
      data = offense_discretized[[k]]
    )
  }
},silent = TRUE)

networkScores <- matrix(
  data = NA,
  nrow = length(all4Algorithms),
  ncol = length(offense_discretized),
)
rownames(networkScores) <- all4Algorithms
colnames(networkScores) <- names(offense_discretized)

for(j in all4Algorithms) for(k in names(offense_discretized)) try({
  networkScores[j,k] <- bnlearn::score(
    x = offbnlearnList[[j]][[k]],
    data = offense_discretized[[k]],
    type = "aic"
  )
})

networkScores
networkScoresDF = data.frame(networkScores)
networkScoresDF = networkScoresDF[order(networkScoresDF$interval, decreasing = TRUE), , drop = FALSE]
networkScoresDF

g1 = graphviz.plot(
  offbnlearnList[[1]][[1]]
)
graph::nodeRenderInfo(g1) <- list(fill="lightgreen", fontsize=40)
Rgraphviz::renderGraph(g1)

jaxOffDF = data.frame(offense_discretized[1])
colnames(jaxOffDF) = colnames(offense)
jaxOffDF

offense_scored_arcStrength = arc.strength(
  x = offbnlearnList[[1]][[1]],
  data = data.frame(jaxOffDF)
)

offenseStrengthplot = strength.plot(
  x = offbnlearnList[[1]][[1]],
  strength = offense_scored_arcStrength,
)

graph::nodeRenderInfo(offenseStrengthplot) <- list(fill="lightgreen", fontsize=40)
Rgraphviz::renderGraph(offenseStrengthplot)
