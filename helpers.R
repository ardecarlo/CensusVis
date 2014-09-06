# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
percent_map <- function(var, color, legend.title, min = 0, max = 100, state="ALL") {

  if (state=="ALL"){
    state <- ""
  }
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # set NAs to 0
  #var[is.na(var)] <- 0
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- round(100*(var-min)/(max-min))
  percents[percents==0] <- 1
  fills <- shades[percents]

  # plot choropleth map
  map("county", state, fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", state, col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}

count_map <- function(var, color, legend.title, min = 0, max = 7, state="ALL") {
  
  if (state=="ALL"){
    state <- ""
  }
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # set NAs to 0
  #var[is.na(var)] <- 0
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- round(100*(var-min)/(max-min))
  percents[percents==0] <- 1
  fills <- shades[percents]
  
  # plot choropleth map
  map("county", state, fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", state, col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(round(10^min), " or less"),
                   round(10^(min + inc)),
                   round(10^(min + 2 * inc)), 
                   round(10^(min + 3 * inc)),
                   paste0(round(10^max), " or more"))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title)
}

makeTable <- function(counties, input){
  idx <- rep(TRUE,length(counties$state))
  if (input$state!="ALL"){
    idx <- counties$state==tolower(input$state)
  }
  arg <- switch(input$var,
                "White" = counties$white[idx],
                "Black" = counties$black[idx],
                "Hispanic" = counties$hispanic[idx], 
                "Asian" = counties$asian[idx])
  max <- input$range[2]
  min <- input$range[1]
  if (input$pctOrTot == "Total"){
    arg <- log10(arg * counties$total.pop[idx] / 100)
    counties$white <- round(counties$white * counties$total.pop[idx] / 100)
    counties$black <- round(counties$black * counties$total.pop[idx] / 100)
    counties$hispanic <- round(counties$hispanic * counties$total.pop[idx] / 100)
    counties$asian <- round(counties$asian * counties$total.pop[idx] / 100)
    max <- max * 7/100
    min <- min * 7/100
  }
  idx2 <- (arg >= min) & (arg <= max)
  countyTable <- counties[idx,1:6]
  countyTable <- countyTable[idx2,]
  return(countyTable)
}