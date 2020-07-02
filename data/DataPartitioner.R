# Converts data into subsections - RunOffense, PassOffense, RunDefense, PassDefense
# This data is then further broken down to each divisional opponent - TEN, IND, HOU
# setwd("data")

library(dplyr)
# Load in the initial play by play data for the 2019 season
initial2019DF = read.csv("reg_pbp_2019.csv", stringsAsFactors = FALSE)

# Pick groupings that may be useful (ex. probabilities won't be)
partition_1 = initial2019DF %>% dplyr::select(3:58, first_down_pass,
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
                                 game_half, half_seconds_remaining))


# Only save plays Jax was involved in. 
# The team is searched for first because it is more unique.
jaxData_all = filter(partition_2,
  (away_team == "JAX" | home_team == "JAX")
  & (play_type == "run" | play_type == "pass"))

# Select only games with rival teams. We do this after selecting Jax to 
# ensure the only games that exist are the ones played against the Jags.
jaxData_divisional = filter(jaxData_all,
                     away_team == "TEN" | home_team == "TEN"
                     | away_team == "HOU" | home_team == "HOU"
                     | away_team == "IND" | home_team == "IND")

