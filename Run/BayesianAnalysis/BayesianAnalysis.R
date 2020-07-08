# Bayesian models for Rushing

# Use dimension reduction first to eliminate future computation as much as possible and 
# distinguish which variables are actually helpful for determining the quality of the play.
set.seed(32)

jaxRunOffense_All = read.csv("../../data/jaxRunOffense.csv")
jaxRunDefense_All = read.csv("../../data/jaxRunDefense.csv")

jaxRunOffense_All = jaxRunOffense_All[order(jaxRunOffense_All$Quality, decreasing = FALSE),]
jaxRunDefense_All = jaxRunDefense_All[order(jaxRunDefense_All$Quality, decreasing = TRUE),]

# Discovered through analysis -
# Safeties are too rare to provide any information. Need to be removed
# Run location is kind of duplicated by run gap but with less detail
jaxRunOffense = subset(jaxRunOffense_All, select = -c(X, safety, first_down_rush, yards_gained))
jaxRunDefense = subset(jaxRunDefense_All, select = -c(X, safety, first_down_rush, yards_gained))

categoricalColumns = c("down", "run_gap", "td_team", "run_location", "Quality")

dummyCategoricals = function(df) {
  df = fastDummies::dummy_cols(
    df,
    remove_first_dummy = FALSE,
    select_columns = categoricalColumns
  )
  df = subset(df, select = -c(down, run_gap, td_team, run_location, Quality))
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

factorColumnsDefense = c("home_team", "goal_to_go", "shotgun", "no_huddle",
                         "qb_dropback", "qb_scramble",
                         "down_1", "down_2", "down_3", "down_4",
                         "run_gap_center", "run_gap_end", "run_gap_guard","run_gap_tackle",
                         "run_location_left", "run_location_middle", "run_location_right",
                         "td_team_0","td_team_1", "td_team_2",
                         "Quality_0", "Quality_1", "Quality_2", "Quality_3", "Quality_4")
factorColumnsOffense = c("home_team", "goal_to_go", "shotgun", "no_huddle",
                         "qb_dropback", "qb_scramble",
                         "down_1", "down_2", "down_3", "down_4",
                         "run_gap_center", "run_gap_end", "run_gap_guard","run_gap_tackle",
                         "run_location_left", "run_location_middle", "run_location_right",
                         "td_team_0","td_team_1",
                         "Quality_0", "Quality_1", "Quality_2", "Quality_3", "Quality_4")
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

sampleOff = sample.split(offense_normalized$Quality_1, SplitRatio = .80)
offense_training = subset(offense_normalized, sampleOff == TRUE)
offense_test = subset(offense_normalized, sampleOff == FALSE)

sampleDef = sample.split(defense_normalized$Quality_1, SplitRatio = .80)
defense_training = subset(defense_normalized, sampleDef == TRUE)
defense_test = subset(defense_normalized, sampleDef == FALSE)

offense_discretized = lapply(
  X = c("interval", "quantile"),
  FUN = function(method) discretize(
    data = offense_training,
    method = method,
    breaks = 4,
    ordered = TRUE
  )
)

defense_discretized = lapply(
  X = c("interval", "quantile"),
  FUN = function(method) discretize(
    data = defense_training,
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
  return(model)
}

#### Offense ####
bnlearnList = findBestModel(NULL, offense_discretized)
printNetworkScores(bnlearnList, offense_discretized)
createStrengthPlotBayesian(bnlearnList, offense_discretized)

# I don't want any links between the qualities of play.
# Also no link between gaps
offBlacklist = data.frame(
  from = c("Quality_0","Quality_0","Quality_0","Quality_0",
           "Quality_1","Quality_1","Quality_1","Quality_1",
           "Quality_2","Quality_2","Quality_2","Quality_2",
           "Quality_3","Quality_3","Quality_3","Quality_3",
           "Quality_4","Quality_4","Quality_4","Quality_4",
           "run_gap_center", "run_gap_center","run_gap_center",
           "run_gap_guard", "run_gap_guard", "run_gap_guard", 
           "run_gap_tackle", "run_gap_tackle","run_gap_tackle",
           "run_gap_end", "run_gap_end", "run_gap_end",
           "run_location_left", "run_location_left",
           "run_location_middle", "run_location_middle",
           "run_location_right", "run_location_right",
           "down_1", "down_1", "down_1", 
           "down_2", "down_2","down_2", 
           "down_3", "down_3", "down_3", 
           "down_4", "down_4", "down_4"),
  to =   c("Quality_1","Quality_2","Quality_3","Quality_4",
           "Quality_0","Quality_2","Quality_3","Quality_4",
           "Quality_0","Quality_1","Quality_3","Quality_4",
           "Quality_0","Quality_1","Quality_2","Quality_4", 
           "Quality_0","Quality_1","Quality_2","Quality_3",
           "run_gap_guard", "run_gap_tackle", "run_gap_end", 
           "run_gap_center", "run_gap_tackle", "run_gap_end", 
           "run_gap_center", "run_gap_guard", "run_gap_end",
           "run_gap_center", "run_gap_guard", "run_gap_tackle",
           "run_location_middle","run_location_right", 
           "run_location_left","run_location_right",
           "run_location_left","run_location_middle",
           "down_2", "down_3", "down_4", 
           "down_1", "down_3", "down_4", 
           "down_1", "down_2", "down_4", 
           "down_1", "down_1", "down_3")
)

offbnlearnList_2 = findBestModel(offBlacklist, offense_discretized)
printNetworkScores(offbnlearnList_2, offense_discretized)
offModel = createStrengthPlotBayesian(offbnlearnList_2, offense_discretized)

qualityOffFit = bn.fit(
  x = offModel,
  data = data.frame(offense_discretized$interval)
)

qualityOffFit
trainingOffPredict = predict(qualityOffFit, node = "run_location_left", data = offense_training)
trainingOffPredict
require(caret)
caret::confusionMatrix(trainingOffPredict, offense_training$run_location_left)
testPredict_Off = predict(qualityOffFit, node = "run_location_left", data = offense_test)
caret::confusionMatrix(testPredict_Off, offense_test$run_location_left)

# All directions make sense

###### Defense ########
defBlacklist = data.frame(
  from = c("Quality_0","Quality_0","Quality_0","Quality_0",
           "Quality_1","Quality_1","Quality_1","Quality_1",
           "Quality_2","Quality_2","Quality_2","Quality_2",
           "Quality_3","Quality_3","Quality_3","Quality_3",
           "Quality_4","Quality_4","Quality_4","Quality_4",
           "run_gap_center", "run_gap_center","run_gap_center",
           "run_gap_guard", "run_gap_guard", "run_gap_guard", 
           "run_gap_tackle", "run_gap_tackle","run_gap_tackle",
           "run_gap_end", "run_gap_end", "run_gap_end",
           "run_location_left", "run_location_left",
           "run_location_middle", "run_location_middle",
           "run_location_right", "run_location_right",
           "down_1", "down_1", "down_1", 
           "down_2", "down_2","down_2", 
           "down_3", "down_3", "down_3", 
           "down_4", "down_4", "down_4"),
  to =   c("Quality_1","Quality_2","Quality_3","Quality_4",
           "Quality_0","Quality_2","Quality_3","Quality_4",
           "Quality_0","Quality_1","Quality_3","Quality_4",
           "Quality_0","Quality_1","Quality_2","Quality_4", 
           "Quality_0","Quality_1","Quality_2","Quality_3",
           "run_gap_guard", "run_gap_tackle", "run_gap_end", 
           "run_gap_center", "run_gap_tackle", "run_gap_end", 
           "run_gap_center", "run_gap_guard", "run_gap_end",
           "run_gap_center", "run_gap_guard", "run_gap_tackle",
           "run_location_middle","run_location_right", 
           "run_location_left","run_location_right",
           "run_location_left","run_location_middle",
           "down_2", "down_3", "down_4", 
           "down_1", "down_3", "down_4", 
           "down_1", "down_2", "down_4", 
           "down_1", "down_1", "down_3")
)

defbnlearnList = findBestModel(defBlacklist, defense_discretized)
printNetworkScores(defbnlearnList, defense_discretized)
createStrengthPlotBayesian(defbnlearnList, defense_discretized, def = TRUE)

defModel_2 = reverse.arc(
  x = defbnlearnList[[1]][[1]],
  from = "Quality_0",
  to =   "run_gap_center",
  check.cycles = FALSE
)

defModel_2 = reverse.arc(
  x = defModel_2,
  from = "Quality_4",
  to =   "shotgun",
  check.cycles = FALSE
)

defModel_2 = reverse.arc(
  x = defModel_2,
  from = "Quality_4",
  to =   "down_2",
  check.cycles = FALSE
)

defScore_2 <- bnlearn::score(
  x = defModel_2,
  data = defense_discretized[[1]],
  type = "aic"
)

defScore_2
defModel = createStrengthPlotBayesian(list(), model = defModel_2,
                           defense_discretized, def = TRUE)

qualityDefFit = bn.fit(
  x = defModel,
  data = data.frame(defense_discretized$interval)
)

qualityDefFit
trainingDefPredict = predict(qualityDefFit, node = "run_location_right", data = defense_training)
trainingDefPredict
require(caret)
caret::confusionMatrix(trainingDefPredict, defense_training$run_location_right)
testPredict_Def = predict(qualityDefFit, node = "run_location_right", data = defense_test)
caret::confusionMatrix(testPredict_Def, defense_test$run_location_right)
