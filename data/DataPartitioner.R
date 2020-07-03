# Converts data into subsections - RunOffense, PassOffense, RunDefense, PassDefense
# This data is then further broken down to each divisional opponent - TEN, IND, HOU
# setwd("data")

require(dplyr)
# Load in the initial play by play data for the 2019 season
initial2019DF = read.csv("reg_pbp_2019.csv", stringsAsFactors = FALSE)

# Pick groupings that may be useful (ex. probabilities won't be)
partition_1 = initial2019DF %>% dplyr::select(3:58, first_down_pass, first_down_rush,
                                              incomplete_pass, interception, safety)

# Remove columns useless to all data sets first. This reduces the amount amount data 
# passed at each step. Also remove data that is directly related (qtr, drive, half_seconds_remaining, etc.)
partition_2 = subset(partition_1, 
                     select = -c(desc, two_point_conv_result, timeout_team, game_date,
                                 quarter_end, sp, qb_spike, qb_kneel, timeout,
                                 posteam_timeouts_remaining, defteam_timeouts_remaining,
                                 posteam_score, defteam_score, posteam_score_post,
                                 defteam_score_post, score_differential_post, yrdln,
                                 game_date, time, drive, qtr, side_of_field,
                                 game_half, half_seconds_remaining, home_timeouts_remaining,
                                 away_timeouts_remaining, field_goal_result, kick_distance,
                                 extra_point_result, ydstogo, quarter_seconds_remaining, ydsnet,
                                 air_yards, yards_after_catch, total_home_score,
                                 total_away_score, defteam, posteam_type))


# Only save plays Jax was involved in. 
# The team is searched for first because it is more unique.
jaxData_all = filter(partition_2,
  (away_team == "JAX" | home_team == "JAX")
  & (play_type == "run" | play_type == "pass"))

# Select only games with rival teams. We do this after selecting Jax to 
# ensure the only games that exist are the ones played against the Jags.
# jaxData_divisional = filter(jaxData_all,
#                     away_team == "TEN" | home_team == "TEN"
#                     | away_team == "HOU" | home_team == "HOU"
#                     | away_team == "IND" | home_team == "IND")

# Must be done after divisional split. Don't forget to remove away team after
# if (df[row, "home_team"] == "JAX") {
#   df[row, "home_team"] = 1
# } else {
#   df[row, "home_team"] = 0
# }
# Make everything a represent Jacksonville
# Change columns to always be in the perspective of JAX.
changeColumnsToRepresentJAXOrOPP = function(df) {
  for(row in 1:nrow(df)) {
    if (df[row, "home_team"] == "JAX") {
     df[row, "home_team"] = 1
    } else {
     df[row, "home_team"] = 0
    }
    
    # No team scored = 0, JAX scored = 1, OPP scored = 2
    # Handles NA values when dummied
    if (is.na(df[row, "td_team"])) {
      df[row, "td_team"] = 0
    } else if (df[row, "td_team"] == "JAX") {
      df[row, "td_team"] = 1
    } else {
      df[row, "td_team"] = 2
    }
    
    # Offense(1) or defense(0) for Jax
    if (df[row, "posteam"] == "JAX") {
      df[row, "offensive_play"] = 1
    } else {
      df[row, "offensive_play"] = 0
    }
  }
  
  df = subset(df, select = -c(posteam, away_team))
  return(df)
}

jaxData_Rep = changeColumnsToRepresentJAXOrOPP(jaxData_all)

# Separate run or pass
jaxData_Run = subset(
  jaxData_Rep,
  play_type == "run" & !is.na(run_location),
  select = -c(pass_length, pass_location, play_type, first_down_pass, interception)
)

jaxData_Pass = subset(
  jaxData_Rep,
  play_type == "pass",
  select = -c(run_location, run_gap, play_type, first_down_rush, qb_scramble, qb_dropback)
)

######### Modify the Run ###################
addCenterAsGap = function(df) {
  for (row in 1:nrow(df)) {
    if (df[row, "run_location"] == "middle" & is.na(df[row, "run_gap"])) {
      df[row, "run_gap"] = "center"
    }
  }
  return(df)
}

jaxData_Run = addCenterAsGap(jaxData_Run)

runUnder10YardsNotGoalLine = function(df, row) {
  if (df[row, "first_down_rush"] == 1) {
    if (df[row, "yards_gained"] < 3) {
      df[row, "Quality"] = 2
    } else {
      df[row, "Quality"] = 1
    }
  } else {
    if (df[row, "yards_gained"] < 1) {
      df[row, "Quality"] = 4
    } else if (df[row, "yards_gained"] < 3) {
      df[row, "Quality"] = 3
    } else if (df[row, "yards_gained"] < 5) {
      df[row, "Quality"] = 2
    } else {
      df[row, "Quality"] = 1
    }
  }
  return(df[row, "Quality"])
}

under10YardGain10ToGoal = function(df, row) {
  if (df[row, "td_team"] == 2) {
    df[row, "Quality"] = 1
  } else if (df[row, "yards_gained"] < 1) {
    df[row, "Quality"] = 4
  } else if (df[row, "yards_gained"] < 3) {
    df[row, "Quality"] = 3
  } else if (df[row, "yards_gained"] < 6){
    df[row, "Quality"] = 2
  } else {
    df[row, "Quality"] = 1
  }
  return(df[row, "Quality"])
}

# Create a y variable with 5 categories - Very Poor, Poor, Fair, Good, Excellent
# Represented as 0, 1, 2, 3, 4 respectfully for defense, opposite for offense
createYVariableRun = function(df) {
  for(row in 1:nrow(df)) {
    turnover = df[row, "td_team"] == 1 | df[row, "safety"] == 1
    if (df[row, "offensive_play"] == 0 & turnover) {
      df[row, "Quality"] = 4
    } else if (df[row, "yards_gained"] >= 10) {
      df[row, "Quality"] = 0
    } else {
      if (df[row, "yardline_100"] > "10") {
        df[row, "Quality"] = runUnder10YardsNotGoalLine(df, row)
      } else {
        df[row, "Quality"] = under10YardGain10ToGoal(df, row)
      }
    }
  }
  return(df)
}

######## Modify the Pass ####################
# Sack plays don't help us evaluate the pass on either offense or defense.
jaxData_Pass = subset(jaxData_Pass, !is.na(pass_length))

passOver10Yards = function(df, row) {
  if(df[row, "td_team"] == 2 || df[row, "yards_gained"] >= 20) {
    df[row, "Quality"] = 0
  } else {
    df[row, "Quality"] = 1
  }
  return(df[row, "Quality"])
}

passUnder10YardsNotGoalLine = function(df, row) {
  if (df[row, "first_down_pass"] == 1) {
    if (df[row, "yards_gained"] < 3) {
      df[row, "Quality"] = 3
    } else if (df[row, "yards_gained"] < 7) {
      df[row, "Quality"] = 2
    } else {
      df[row, "Quality"] = 1
    }
  } else {
    if (df[row, "yards_gained"] < 1) {
      df[row, "Quality"] = 4
    } else if (df[row, "yards_gained"] < 4) {
      df[row, "Quality"] = 3
    } else {
      df[row, "Quality"] = 2
    }
  }
  return(df[row, "Quality"])
}

# Create a y variable with 5 categories - Very Poor, Poor, Fair, Good, Excellent
# Represented as 0, 1, 2, 3, 4 respectfully for defense, opposite for offense
createYVariablePass = function(df) {
  for(row in 1:nrow(df)) {
    turnover = df[row, "td_team"] == 1 | 
      df[row, "interception"] == 1 |
      df[row, "safety"] == 1
    if (df[row, "offensive_play"] == 0 & turnover) {
        df[row, "Quality"] = 4
    } else if (df[row, "yards_gained"] >= 10) {
      df[row, "Quality"] = passOver10Yards(df, row)
    } else {
      if (df[row, "yardline_100"] > "10") {
        df[row, "Quality"] = passUnder10YardsNotGoalLine(df, row)
      } else {
        df[row, "Quality"] = under10YardGain10ToGoal(df, row)
      }
    }
  }
  return(df)
}

jaxData_Run = createYVariableRun(jaxData_Run)
jaxData_Pass = createYVariablePass(jaxData_Pass)

jaxRunDefense = subset(jaxData_Run, offensive_play == 0, select = -c(offensive_play))
jaxRunOffense = subset(jaxData_Run, offensive_play == 1, select = -c(offensive_play))
jaxPassDefense = subset(jaxData_Pass, offensive_play == 0, select = -c(offensive_play))
jaxPassOffense = subset(jaxData_Pass, offensive_play == 1, select = -c(offensive_play))

write.csv(jaxRunOffense, "jaxRunOffense.csv")
write.csv(jaxRunDefense, "jaxRunDefense.csv")
write.csv(jaxPassOffense, "jaxPassOffense.csv")
write.csv(jaxPassDefense, "jaxPassDefense.csv")
