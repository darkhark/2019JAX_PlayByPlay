# Use dimension reduciton first to eliminate future computation as much as possible and 
# distinguish which variables are actually helpful for determining the quality of the play.
set.seed(32)

jaxPassOffense = read.csv("../data/jaxPassOffense.csv")
jaxPassDefense = read.csv("../data/jaxPassDefense.csv")

jaxPassOffense = jaxPassOffense[order(jaxPassOffense$Quality, decreasing = FALSE),]
jaxPassDefense = jaxPassDefense[order(jaxPassDefense$Quality, decreasing = TRUE),]

yOffense = jaxPassOffense$Quality
yDefense = jaxPassDefense$Quality

jaxPassOffense = subset(jaxPassOffense, select = -c(X, Quality))
jaxPassDefense = subset(jaxPassDefense, select = -c(X, Quality))

categoricalColumns = c("down", "pass_length", "pass_location", "td_team")

dummyCategoricals = function(df) {
  df = fastDummies::dummy_cols(
    df,
    remove_first_dummy = TRUE,
    select_columns = categoricalColumns
  )
  df = subset(df, select = -c(down, pass_length, pass_location, td_team))
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
                         "incomplete_pass", "interception", "safety",
                         "down_2", "down_3", "down_4", "pass_length_short",
                         "pass_location_middle", "pass_location_right",
                         "td_team_1", "td_team_2")
factorColumnsOffense = c("home_team", "goal_to_go", "shotgun", "no_huddle", "first_down_pass", 
                         "incomplete_pass", "interception", "safety",
                         "down_2", "down_3", "down_4", "pass_length_short",
                         "pass_location_middle", "pass_location_right",
                         "td_team_1")
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
  X = c("interval"),
  FUN = function(method) discretize(
    data = offense_normalized,
    method = method,
    breaks = 4,
    ordered = TRUE
  )
)

defense_discretized = lapply(
  X = c("interval"),
  FUN = function(method) discretize(
    data = defense_normalized,
    method = method,
    breaks = 4,
    ordered = TRUE
  )
)

###### Finally to the dimension reduction!!! ###########