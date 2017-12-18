#' Generates a shot chart for a given player
#'
#' Creates a shot chart for a player on a given season
#' @param Seasondata a data frame with the details of the season
#' @param player the name of the player that you want to make a graph of
#' @param quant the quantile of shots to be graphed, defaults to 0.4
#' @param type either "PPS" for points per shot or "PCT" for percentage
#' @return a shot chart graph
#' @examples
#' data("season2017")
#' #Examples with several players
#' ShotSeasonGraphPlayer(season2017, player = "Stephen Curry")
#' ShotSeasonGraphPlayer(season2017, player = "DeMar DeRozan")
#'
#'  #Examples with percentage instead of points per shot
#' ShotSeasonGraphPlayer(season2017, player = "Stephen Curry", type = "PCT")
#' @importFrom dplyr filter
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'

ShotSeasonGraphPlayer <- function(Seasondata, player, quant = 0.4, type = "PPS") {
  LOC_Y <- PLAYER_NAME <- ST <- NULL
  Seasondata <- filter(Seasondata, LOC_Y < 280)
  Seasondata <- filter(Seasondata, PLAYER_NAME == player)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the players

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(name = "PPS", midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(name = "PCT",midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1), breaks =c(0,0.5,1), labels =c("0%","50%","100%")) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280))+ xlim(c(-250, 250)) + theme(legend.position="bottom")
  }
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", player, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", player, sep =" ")
  )}


  return(GRAPH)
}

#' Generates a point based shot chart for a given player
#'
#' Creates a shot chart for a player on a given season creating a point
#' for each taken shot separating by colors mades and misses, also as you
#' can add a kernel of the frequency of usage of areas
#' @param Seasondata a data frame with the details of the season
#' @param player the name of the player that you want to make a graph of
#' @param Type either "Both" (default), for plotting every point, "Made"
#' to plot only the
#' made shots or "Missed" to plot only the missed shots.
#' @param kernel Logical, weather to plot or not the kernel of shots
#' @return a shot chart graph
#' @examples
#' data("season2017")
#' #Examples with several players
#' PointShotSeasonGraphPlayer(season2017, player = "James Harden")
#' PointShotSeasonGraphPlayer(season2017, player = "DeMar DeRozan")
#'
#' PointShotSeasonGraphPlayer(season2017, player = "Stephen Curry", kernel = FALSE)
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 stat_density_2d
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom RColorBrewer brewer.pal
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'

PointShotSeasonGraphPlayer <- function(Seasondata, player, Type = "Both", kernel = TRUE) {
  LOC_Y <- LOC_X <- PLAYER_NAME <- SHOT_MADE_FLAG <- NULL
  Seasondata <- filter(Seasondata, LOC_Y < 280 & LOC_Y > -40)
  Seasondata <- filter(Seasondata, LOC_X < 250 & LOC_X > -250)
  Seasondata <- filter(Seasondata, PLAYER_NAME == player)
  Seasondata$SHOT_MADE_FLAG <- ifelse(Seasondata$SHOT_MADE_FLAG == "1", "Made", "Missed")
  Seasondata$SHOT_MADE_FLAG <- as.factor(Seasondata$SHOT_MADE_FLAG)
  if (Type == "Made"){
    Seasondata2 <-filter(Seasondata, SHOT_MADE_FLAG == "Made")
  }

  if (Type == "Missed"){
    Seasondata2 <- dplyr::filter(Seasondata, SHOT_MADE_FLAG == "Missed")
  }

  if (Type == "Both"){
    Seasondata2 <- Seasondata
  }

  #Make Graph
  if (kernel == TRUE){
  GRAPH <- ggplot(Seasondata2, aes_string(x="LOC_X", y = "LOC_Y"))+ annotation_custom(court, -250, 250, -52, 418)  +
    stat_density_2d(aes_string(fill = "..level.."), geom = "polygon", show.legend = FALSE, alpha = 0.3) + geom_point(aes_string(color = "SHOT_MADE_FLAG"), alpha = 0.8) + scale_fill_gradientn(
      colours = rev(brewer.pal( 7, "Spectral"))
    ) +
    coord_fixed()  +theme(line = element_blank(),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                          legend.title = element_blank(),
                          plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }
  if (kernel == FALSE){
    GRAPH <- ggplot(Seasondata2, aes_string(x="LOC_X", y = "LOC_Y"))+ annotation_custom(court, -250, 250, -52, 418) +
      geom_point(aes_string(color = "SHOT_MADE_FLAG"), alpha = 0.8) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }
  return(GRAPH)
}


#' Generates an offensive shot chart for a given team
#'
#' creates an offensive Shot Chart for the desired team on a given season
#' @param Seasondata a data frame with the details of the season
#' @param team the name of the team that you want to make a graph of
#' @param quant the quantile of shots to be graphed, defaults to 0.4
#' @param type either "PPS" for points per shot or "PCT" for percentage
#' @return a shot chart graph
#' @examples
#' data("season2017")
#' #Examples with several teams
#' OffShotSeasonGraphTeam(season2017, team = "GSW")
#' OffShotSeasonGraphTeam(season2017, team = "Hou")
#' #Examples with shooting percentage instead of Points per Shot
#' OffShotSeasonGraphTeam(season2017, team = "ORL", type = "PCT")
#' @importFrom dplyr filter
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'

OffShotSeasonGraphTeam<- function(Seasondata, team, quant = 0.4, type = "PPS") {
  LOC_Y <- TEAM_NAME <- ST <- NULL
  Seasondata <- filter(Seasondata, LOC_Y < 280)
  Seasondata <- filter(Seasondata, TEAM_NAME == team)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph

  #data("court")

  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(name = "PPS", midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")

  }else{
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(name= "PCT",midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1), breaks =c(0,0.5,1), labels =c("0%","50%","100%")) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")}
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Points per Shot of", team, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Shooting percentage", team, sep =" ")
  )}


  return(GRAPH)
}


#' Generates an defensive shot chart for a given team
#'
#' Creates a defensive Shot Chart for the desired team on a given season, that
#' is a shot chart of the shots the team recieves during the year
#' @param Seasondata a data frame with the details of the season
#' @param team the name of the team that you want to make a graph of
#' @param quant the quantile of shots to be graphed, defaults to 0.4
#' @param type either "PPS" for points per shot or "PCT" for percentage
#' @return a shot chart graph
#' @examples
#' data("season2017")
#' #Examples with several teams
#' DefShotSeasonGraphTeam(season2017, team = "Sas")
#' DefShotSeasonGraphTeam(season2017, team = "Cle")
#' #Examples with shooting percentage instead of Points per Shot
#' DefShotSeasonGraphTeam(season2017, team = "Cle", type = "PCT")
#' @importFrom dplyr filter
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export
#'

DefShotSeasonGraphTeam<- function(Seasondata, team, quant = 0.4, type = "PPS") {
  LOC_Y <- TEAM_NAME <- ST <- HTM <- VTM <- NULL
  Seasondata <- filter(Seasondata, LOC_Y < 280)
  Seasondata <- filter(Seasondata, HTM == team | VTM == team & TEAM_NAME != team)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph

  #data("court")

  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(name = "PPS", midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")

  }else{
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(name ="PCT", midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1), breaks =c(0,0.5,1), labels =c("0%","50%","100%")) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))+ ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")}
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle(paste("Defensive shot chart", team, sep =" "))
  }  else {GRAPH <- GRAPH +  ggtitle(paste("Defensive shot chart", team, sep =" ")
  )}


  return(GRAPH)
}

#' plot the shot chart of a whole NBA Season
#'
#' This function takes an NBA season object and makes a shot chart of all the
#' shots takes through that regular season.
#' You can choose to either plot the results based on Points per Shot or on
#' Shooting Percentage
#' @param Seasondata The information of shots, it can be downloaded with function
#' read_season
#' @param quant A number between 0 and 1, it determines quantile of shots used
#' to plot the shot chart, (default is 0.4)
#' @param type A character to specify if the shot chart is based on Points per
#' Shot ("PPS") or percentage ("PCT")
#' @return a ggplot object plotting the shot chart of a given NBA season
#' @examples
#' data("season2017")
#' ShotSeasonGraph(season2017, quant = 0.4)
#' ShotSeasonGraph(season2017, quant = 0.4, type = "PCT")
#' @seealso \code{\link[SpatialBall]{DefShotSeasonGraphTeam}}
#' @seealso \code{\link[SpatialBall]{OffShotSeasonGraphTeam}}
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotation_custom
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom hexbin hcell2xy
#' @importFrom hexbin hexbin
#' @importFrom stats quantile
#' @author Derek Corcoran <derek.corcoran.barrios@gmail.com>
#' @export


ShotSeasonGraph <- function(Seasondata, quant = 0.4, type = "PPS") {
  LOC_Y <- ST <- NULL
  Seasondata <- filter(Seasondata, LOC_Y < 280)
  #Get the maximum and minumum values for x and y
  xbnds <- range(Seasondata$LOC_X)
  ybnds <- range(Seasondata$LOC_Y)
  #Make hexbin dataframes out of the teams

  if (type == "PPS"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG))*ifelse(tolower(df$SHOT_TYPE) == "3pt field goal", 3, 2), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }

  if (type == "PCT"){
    makeHexData <- function(df) {
      h <- hexbin(df$LOC_X, df$LOC_Y, 25, xbnds = xbnds, ybnds = ybnds, IDs = TRUE)
      data.frame(hcell2xy(h),
                 PPS = tapply(as.numeric(as.character(df$SHOT_MADE_FLAG)), h@cID, FUN = function(z) sum(z)/length(z)),
                 ST = tapply(df$SHOT_MADE_FLAG, h@cID, FUN = function(z) length(z)),
                 cid = h@cell)
    }
  }


  ##Total NBA data
  Totalhex <- makeHexData(Seasondata)
  Totalhex <- filter(Totalhex, ST > quantile(Totalhex$ST, probs = quant))
  Totalhex$ST <- ifelse(Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2], 0.06,
                        ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[2] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] , 0.12 ,
                               ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[3] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] , 0.25 ,
                                      ifelse(Totalhex$ST > quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[4] & Totalhex$ST <= quantile(Totalhex$ST, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))[5] , 0.5 ,
                                             1))))


  #Function to transform hexbins into polygons
  hex_coord_df <- function(x, y, width, height, size = 1) {
    # like hex_coord but returns a dataframe of vertices grouped by an id variable
    dx <- size * width / 6
    dy <- size * height / 2 / sqrt(3)

    hex_y <- rbind(y - 2 * dy, y - dy, y + dy, y + 2 * dy, y + dy, y - dy)
    hex_x <- rbind(x, x + dx, x + dx, x, x - dx, x - dx)
    id    <- rep(1:length(x), each=6)

    data.frame(cbind(x=as.vector(hex_x), y=as.vector(hex_y), id))
  }

  #Transform Hexbins into polygons

  Total <- hex_coord_df(Totalhex$x, Totalhex$y, 35*Totalhex$ST, 12*Totalhex$ST, size =1)
  Total$PPS <- rep(Totalhex$PPS, each = 6)

  #Make Graph
  if(type == "PPS"){
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(midpoint = 1, low = "blue", high = "red", limits=c(0, 3)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250)) + theme(legend.position="bottom")
  }else{
    GRAPH <- ggplot(Total, aes_string(x="x", y = "y"))+ annotation_custom(court, -250, 250, -52, 418) + geom_polygon(aes_string(group = "id", fill = "PPS")) + scale_fill_gradient2(midpoint = 0.5, low = "blue", high = "red", limits=c(0, 1)) +
      coord_fixed()  +theme(line = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.x = element_blank(),
                            axis.text.y = element_blank(),
                            legend.title = element_blank(),
                            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold")) + ylim(c(-40, 280)) + xlim(c(-250, 250))+ theme(legend.position="bottom")
  }
  if(type == "PPS"){
    GRAPH <- GRAPH +  ggtitle("Points per Shot")
  }  else {GRAPH <- GRAPH +  ggtitle("Shooting percentage")}


  return(GRAPH)
}
