# Bayesian models for Passing

# Use dimension reduction first to eliminate future computation as much as possible and 
# distinguish which variables are actually helpful for determining the quality of the play.
set.seed(32)

jaxPassOffense_All = read.csv("../../data/jaxRunOffense.csv")
jaxPassDefense_All = read.csv("../../data/jaxRunDefense.csv")

jaxRunOffense_All = jaxRunOffense_All[order(jaxRunOffense_All$Quality, decreasing = FALSE),]
jaxRunDefense_All = jaxRunDefense_All[order(jaxRunDefense_All$Quality, decreasing = TRUE),]

# Discovered through analysis -
# Safeties are too rare to provide any information. Need to be removed
# Fourth downs are too rare for defense to provide any information. Need to be removed
jaxRunDefense_All = jaxRunDefense_All[!(jaxRunDefense_All$down == 4),]
jaxRunOffense = subset(jaxRunOffense_All, select = -c(X, safety, first_down_rush, yards_gained))
jaxPassDefense = subset(jaxPassDefense_All, select = -c(X, safety, first_down_pass, yards_gained))


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

factorColumnsDefense = c("home_team", "goal_to_go", "shotgun", "no_huddle",
                         "incomplete_pass", "interception",
                         "down_2", "down_3", "pass_length_short",
                         "pass_location_middle", "pass_location_right",
                         "td_team_1", "td_team_2", "Quality_1", "Quality_2",
                         "Quality_3", "Quality_4")
factorColumnsOffense = c("home_team", "goal_to_go", "shotgun", "no_huddle",
                         "incomplete_pass", "interception",
                         "down_2", "down_3", "down_4", "pass_length_short",
                         "pass_location_middle", "pass_location_right",
                         "td_team_1", "Quality_1", "Quality_2",
                         "Quality_3", "Quality_4")
numericColumns = c("yardline_100", "game_seconds_remaining",
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

all4Algorithms = c("hc", "iamb.fdr", "h2pc", "aracne")


findBestModel = function(blacklist, discretized) {
  bnlearnList = list()
  for(j in all4Algorithms) for(k in names(discretized)) try({
  bnlearnList[[j]][[k]] <- do.call(
      what = j,
      args = if(is.null(blacklist)) {
        list(x = discretized[[k]])
      } else {
        list(x = discretized[[k]], blacklist = blacklist)
      }
    )
    M_arcs <- arcs(bnlearnList[[j]][[k]])
    for(l in 1:nrow(M_arcs)){
      bnlearnList[[j]][[k]] <- set.arc(
        x = bnlearnList[[j]][[k]],
        from = M_arcs[l,1],
        to = M_arcs[l,2],
        check.cycles = FALSE,
        check.illegal = FALSE
      )
      bnlearnList[[j]][[k]] <- choose.direction(
        x = bnlearnList[[j]][[k]],
        arc = M_arcs[l,],
        data = discretized[[k]]
      )
    }
  },silent = TRUE)
  return(bnlearnList)
}

printNetworkScores = function(bnlearnList, discretized, model = NULL) {
  networkScores <- matrix(
  data = NA,
  nrow = length(all4Algorithms),
  ncol = length(discretized),
  )
  rownames(networkScores) <- all4Algorithms
  colnames(networkScores) <- names(discretized)
  
  for(j in all4Algorithms) for(k in names(discretized)) try({
    networkScores[j,k] <- bnlearn::score(
      x = bnlearnList[[j]][[k]],
      data = discretized[[k]],
      type = "aic"
    )
  })
  
  networkScores
  networkScoresDF = data.frame(networkScores)
  networkScoresDF = networkScoresDF[order(networkScoresDF$interval, decreasing = TRUE), , drop = FALSE]
  networkScoresDF
}

createStrengthPlotBayesian = function(bnlearnList, discretized, model = NULL, def = FALSE) {
  if (is.null(model)) {
    model = bnlearnList[[1]][[1]]
  }
  
  g1 = graphviz.plot(
    model
  )
  graph::nodeRenderInfo(g1) <- list(fill="lightgreen", fontsize=40, shape = "ellipse")
  Rgraphviz::renderGraph(g1)
  
  jaxDF = data.frame(discretized[1])
  colnames(jaxDF) = if(def) {
    colnames(defense)
  } else {
    colnames(offense)
  }
  
  scored_arcStrength = arc.strength(
    x = model,
    data = data.frame(jaxDF)
  )
  
  strengthplot = strength.plot(
    x = model,
    strength = scored_arcStrength,
  )
  
  graph::nodeRenderInfo(strengthplot) <- list(
    fill="lightgreen",
    fontsize=35,
    shape = "ellipse"
  )
  Rgraphviz::renderGraph(strengthplot)
}

#### Offense ####
bnlearnList = findBestModel(NULL, offense_discretized)
printNetworkScores(bnlearnList, offense_discretized)
createStrengthPlotBayesian(bnlearnList, offense_discretized)

# I don't want any links between the qualities of play.
# Also no link between pass_locations
offBlacklist = data.frame(
  from = c("Quality_1","Quality_1","Quality_1",
           "Quality_2","Quality_2","Quality_2",
           "Quality_3","Quality_3","Quality_3",
           "Quality_4","Quality_4","Quality_4",
           "pass_location_middle", "pass_location_right",
           "down_2", "down_2",
           "down_3", "down_3", 
           "down_4", "down_4"),
  to =   c("Quality_2","Quality_3","Quality_4",
           "Quality_1","Quality_3","Quality_4",
           "Quality_1","Quality_2","Quality_4",
           "Quality_1","Quality_2","Quality_3", 
           "pass_location_right", "pass_location_middle",
           "down_3", "down_4", 
           "down_2", "down_4", 
           "down_2", "down_3")
)

offbnlearnList_2 = findBestModel(offBlacklist, offense_discretized)
printNetworkScores(offbnlearnList_2, offense_discretized)
createStrengthPlotBayesian(offbnlearnList_2, offense_discretized)

# There's a few directions that don't make since.
# The quality of play should never point to anything since it 
# is the result of the other variables.
offModel_3 = reverse.arc(
  x = offbnlearnList_2[[1]][[1]],
  from = "pass_location_middle",
  to =   "Quality_3",
  check.cycles = FALSE
)

offModel_3 = reverse.arc(
  x = offModel_3,
  from = "Quality_3",
  to =   "pass_length_short",
  check.cycles = FALSE
)

offModel_3 = reverse.arc(
  x = offModel_3,
  from = "Quality_1",
  to =   "pass_length_short",
  check.cycles = FALSE
)

offScore3 <- bnlearn::score(
  x = offModel_3,
  data = offense_discretized[[1]],
  type = "aic"
)
offScore3
createStrengthPlotBayesian(list(), offense_discretized, model = offModel_3)

###### Defense ########
defBlacklist = data.frame(
  from = c("Quality_1","Quality_1","Quality_1",
           "Quality_2","Quality_2","Quality_2",
           "Quality_3","Quality_3","Quality_3",
           "Quality_4","Quality_4","Quality_4",
           "pass_location_middle", "pass_location_right",
           "down_2","down_3"),
  to =   c("Quality_2","Quality_3","Quality_4",
           "Quality_1","Quality_3","Quality_4",
           "Quality_1","Quality_2","Quality_4",
           "Quality_1","Quality_2","Quality_3", 
           "pass_location_right", "pass_location_middle",
           "down_3", "down_2")
)
defbnlearnList = findBestModel(defBlacklist, defense_discretized)
printNetworkScores(defbnlearnList, defense_discretized)
createStrengthPlotBayesian(defbnlearnList, defense_discretized, def = TRUE)

defModel_2 = reverse.arc(
  x = defbnlearnList[[1]][[1]],
  from = "Quality_1",
  to =   "pass_length_short",
  check.cycles = FALSE
)

defModel_2 = reverse.arc(
  x = defModel_2,
  from = "Quality_2",
  to =   "pass_length_short",
  check.cycles = FALSE
)

defModel_2 = reverse.arc(
  x = defModel_2,
  from = "Quality_4",
  to =   "td_team_2",
  check.cycles = FALSE
)

defModel_2 = reverse.arc(
  x = defModel_2,
  from = "Quality_4",
  to =   "pass_location_middle",
  check.cycles = FALSE
)

defModel_2 = reverse.arc(
  x = defModel_2,
  from = "Quality_4",
  to =   "interception",
  check.cycles = FALSE
)

defScore_2 <- bnlearn::score(
  x = defModel_2,
  data = defense_discretized[[1]],
  type = "aic"
)

defScore_2
createStrengthPlotBayesian(list(), model = defModel_2,
                           defense_discretized, def = TRUE)
