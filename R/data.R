#' Dataset for all the Shots in the 2016-17 season of the NBA
#'
#' A dataset containing a dataframe
#'
#' @format Data frame with the following columns
#' \describe{
#'   \item{GRID_TYPE}{A stack with eight time slices for species A}
#'   \item{GAME_ID}{A stack with eight time slices for species b}
#'   \item{GAME_EVENT_ID}{Id of the play when the shot happened}
#'   \item{PLAYER_ID}{Numeric code of the player whot took the shot}
#'   \item{PLAYER_NAME}{Name of the player who took the shot}
#'   \item{TEAM_ID}{Numeric code of the team}
#'   \item{TEAM_NAME}{Name of the team of the player who took the shot}
#'   \item{PERIOD}{Quarter when the shot was taken 1 to 4, if there are overtimes it keeps
#'   adding, that is period 6 is the second overtime}
#'   \item{MINUTES_REMAINING}{minutes remaining}
#'   \item{SECONDS_REMAINING}{seconds remaining in the minute}
#'   \item{EVENT_TYPE}{Weather the shot was made or not}
#'   \item{ACTION_TYPE}{What kind of shot was taken, this has 52 options ranging from hook
#'   bank shot to reverse dunk shot}
#'   \item{SHOT_TYPE}{Weather the shot was 2 point or 3 point shot}
#'   \item{SHOT_ZONE_BASIC}{Factor, one of "Above the Break 3", "Backcourt", "In The Paint (Non-RA)",
#'   "Left Corner 3", "Mid-Range", "Restricted Area", "Right Corner 3"}
#'   \item{SHOT_ZONE_AREA}{One of "Back Court(BC)", "Center(C)", "Left Side Center(LC)"
#'   , "Left Side(L)", "Right Side Center(RC)", "Right Side(R)"}
#'   \item{SHOT_ZONE_RANGE}{Distance range were the shot was attempted}
#'   \item{SHOT_DISTANCE}{Shot distance in feet}
#'   \item{LOC_X}{x coordinate of the player when the shot was attempted}
#'   \item{LOC_Y}{y coordinate of the player when the shot was attempted}
#'   \item{SHOT_ATTEMPTED_FLAG}{value 1 which means that there was an attempted shot}
#'   \item{SHOT_MADE_FLAG}{interger, 1 if the shot was made, 0 if it was missed}
#'   \item{GAME_DATE}{Date when the game was played}
#'   \item{HTM}{Name of the home team}
#'   \item{VTM}{Name of the visiting team}
#' }
#'
"season2017"
