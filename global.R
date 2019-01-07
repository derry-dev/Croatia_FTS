source("data.R", local = T)

choices.kpa <- c("Throughput","Efficiency","Safety","Sector Capacity")
choices.throughputs <- c("Total Throughput","Hourly Throughput","Rolling Hourly Throughput")
choices.efficiency <- c("Track Miles","Fuel Burn")
choices.efficiencygrouping <- c("Airport","Airport (Combined)", "Airport (Arrivals Only)", "Airport (Departures Only)", "Routing")
choices.safety <- c("Conflict Map", "Conflict Statistics")
choices.conflictsgrouping <- c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)")
choices.sectorcapacity <- c("Sector Entry Count","Sector Occupancy Count","Controller Workload")

plotlyTotalThroughput <- function(data = c("Current","PBN"), airport = "All", arrange="Vertical"){
  if (airport == "All") {
    d <- subset(table.TotalThroughputs, Airport %in% list.airports & Category %in% c("Descending","GroundAccelerating"))
    barmode <- "stack"
  } else {
    d <- subset(table.TotalThroughputs, Airport %in% airport & Category %in% c("Descending","GroundAccelerating"))
    barmode <- "dodge"
  }
  d$Category[d$Category == "Descending" & d$Scenario == "Current"] <- "Current Arrivals"
  d$Category[d$Category == "GroundAccelerating" & d$Scenario == "Current"] <- "Current Departures"
  d$Category[d$Category == "Descending" & d$Scenario == "PBN"] <- "PBN Arrivals"
  d$Category[d$Category == "GroundAccelerating" & d$Scenario == "PBN"] <- "PBN Departures"
  
  g <- d %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~Category, colors=c("darkgreen","darkblue","green","blue"), type="bar") %>%
    layout(hovermode="compare", title="Total Throughputs", legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~Category, colors=c("darkgreen","darkblue"), type="bar") %>%
    layout(hovermode="compare", barmode=barmode, title="Current Total Throughputs", legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~Category, colors=c("green","blue"), type="bar") %>%
    layout(hovermode="compare", barmode=barmode, title="PBN Total Throughputs", legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Group") {
      return(g %>% layout(barmode="group"))
    } else if (arrange == "Stack") {
      return(g %>% layout(barmode="stack"))
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotlyHourlyThroughput <- function(data = c("Current","PBN"), airport="All", arrange="Vertical"){
  if (airport == "All") {
    d <- aggregate(data=subset(table.HourlyThroughputs, Airport %in% list.airports), Count~Category+Hour+Scenario, "sum")
  } else {
    d <- subset(table.HourlyThroughputs, Airport %in% airport)
  }
  d$Category[d$Category == "Descending" & d$Scenario == "Current"] <- "Current Arrivals"
  d$Category[d$Category == "GroundAccelerating" & d$Scenario == "Current"] <- "Current Departures"
  d$Category[d$Category == "Descending" & d$Scenario == "PBN"] <- "PBN Arrivals"
  d$Category[d$Category == "GroundAccelerating" & d$Scenario == "PBN"] <- "PBN Departures"
  
  g <- d %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~Category, colors=c("darkgreen","darkblue","green","blue"), type="bar") %>%
    layout(hovermode="compare", title=paste("Hourly Throughputs",airport), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d, Scenario %in% "Current") %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~Category, colors=c("darkgreen","darkblue"), type="bar") %>%
    layout(hovermode="compare", barmode="stack", title=paste("Current Hourly Throughputs",airport), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~Category, colors=c("green","blue"), type="bar") %>%
    layout(hovermode="compare", barmode="stack", title=paste("PBN Hourly Throughputs",airport), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(barmode="stack",title=paste("Hourly Throughputs",airport)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(barmode="stack",title=paste("Hourly Throughputs",airport)))
    } else if (arrange == "Group") {
      return(g %>% layout(barmode="group"))
    } else if (arrange == "Stack") {
      return(g %>% layout(barmode="stack"))
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotlyRollingHourlyThroughput <- function(data = c("Current","PBN"), airport="All", arrange="Vertical"){
  temp <- gather(table.RollingHourlyThroughputs,"Category","Count",c(RollingThroughputCount,RollingLiftOffCount,RollingTouchDownCount))
  if (airport == "All") {
    d <- aggregate(data = subset(temp, Airport %in% list.airports), Count~Time+Category+Scenario, "sum")
  } else {
    d <- subset(temp, Airport %in% airport)
  }
  d$Category[d$Category == "RollingTouchDownCount" & d$Scenario == "Current"] <- "Current Arrivals"
  d$Category[d$Category == "RollingLiftOffCount" & d$Scenario == "Current"] <- "Current Departures"
  d$Category[d$Category == "RollingThroughputCount" & d$Scenario == "Current"] <- "Current Total"
  d$Category[d$Category == "RollingTouchDownCount" & d$Scenario == "PBN"] <- "PBN Arrivals"
  d$Category[d$Category == "RollingLiftOffCount" & d$Scenario == "PBN"] <- "PBN Departures"
  d$Category[d$Category == "RollingThroughputCount" & d$Scenario == "PBN"] <- "PBN Total"
  
  g <- d %>% group_by(Category) %>% 
    plot_ly(x=~Time, y=~Count, color=~Category, colors=c("darkgreen","darkblue","darkred","green","blue","red"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=paste("Rolling Hourly Throughputs", airport), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d, Scenario %in% "Current") %>% group_by(Category) %>%
    plot_ly(x=~Time, y=~Count, color=~Category, colors=c("darkgreen","darkblue","darkred"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=paste("Current Rolling Hourly Throughputs", airport), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Category) %>%
    plot_ly(x=~Time, y=~Count, color=~Category, colors=c("green","blue","red"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=paste("PBN Rolling Hourly Throughputs", airport), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)

  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Rolling Hourly Throughputs",airport)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Rolling Hourly Throughputs",airport)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotlyFuelBurn <- function(data = c("Current","PBN"), group="Airport", airport="All",arrange="Vertical") {
  if (airport == "All") {
    d <- subset(table.FuelBurn, Airport %in% list.airports)
    palette.current <- brewer.pal(n = 8, name = "Spectral")
    palette.PBN <- rainbow(12)[1:8]
  } else if (airport %in% list.airports) {
    d <- subset(table.FuelBurn, Airport %in% airport)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  if (group == "Airport") {
    
    g <- subset(d,!(RoutingType %in% "Both") & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,RoutingType), colors=c("darkgreen","darkblue","green","blue"), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & !(RoutingType %in% "Both") & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,RoutingType), colors=c("darkgreen","darkblue"), type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & !(RoutingType %in% "Both") & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,RoutingType), colors=c("green","blue"), type="bar")
    
  } else if (group == "Airport (Combined)") {

    g <- subset(d, RoutingType %in% "Both" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Both" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Both" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar")

  } else if (group == "Airport (Arrivals Only)") {
    
    g <- subset(d, RoutingType %in% "Arrival" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Arrival" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Arrival" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar")
    
  } else if (group == "Airport (Departures Only)") {
    
    g <- subset(d, RoutingType %in% "Departure" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Departure" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Departure" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar")
    
  } else if (group == "Routing") {
    
    g <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both")) %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=c(palette.current,palette.PBN), type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g1 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=palette.current, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g2 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=palette.PBN, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))

  }
  
  g <- g %>% layout(hovermode="compare", title=paste("Total Fuel Burn by", group), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto",fixedrange=T),
                    yaxis=list(title="Total Fuel Burn (kg)",tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", title=paste("Current Total Fuel Burn by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(title="Total Fuel Burn (kg)",tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", boxmode="group", title=paste("PBN Total Fuel Burn by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(title="Total Fuel Burn (kg)",tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Total Fuel Burn by", group)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Total Fuel Burn by", group)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotlyTrackMiles <- function(data = c("Current","PBN"), group="Airport", airport="All",arrange="Vertical") {
  if (airport == "All") {
    d <- subset(table.TrackMiles, Airport %in% list.airports)
    palette.current <- brewer.pal(n = 8, name = "Spectral")
    palette.PBN <- rainbow(12)[1:8]
  } else if (airport %in% list.airports) {
    d <- subset(table.TrackMiles, Airport %in% airport)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  if (group == "Airport") {
    
    g <- subset(d,!(RoutingType %in% "Both") & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,RoutingType), colors=c("darkgreen","darkblue","green","blue"), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & !(RoutingType %in% "Both") & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,RoutingType), colors=c("darkgreen","darkblue"), type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & !(RoutingType %in% "Both") & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,RoutingType), colors=c("green","blue"), type="bar")
    
  } else if (group == "Airport (Combined)") {
    
    g <- subset(d, RoutingType %in% "Both" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Both" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Both" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar")
    
  } else if (group == "Airport (Arrivals Only)") {
    
    g <- subset(d, RoutingType %in% "Arrival" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Arrival" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Arrival" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar")
    
  } else if (group == "Airport (Departures Only)") {
    
    g <- subset(d, RoutingType %in% "Departure" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Departure" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="bar")
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Departure" & Waypoint %in% "All") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar")
    
  } else if (group == "Routing") {
    
    g <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both")) %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=c(palette.current,palette.PBN), type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g1 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.current, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g2 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.PBN, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
  }
  
  g <- g %>% layout(hovermode="compare", title=paste("Total Track Miles by", group), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto",fixedrange=T),
                    yaxis=list(title="Total Track Miles (NM)",tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", title=paste("Current Total Track Miles by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(title="Total Track Miles (NM)",tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", boxmode="group", title=paste("PBN Total Track Miles by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(title="Total Track Miles (NM)",tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Total Track Miles by", group)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Total Track Miles by", group)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotConflictMap <- function(){
  table.current.Conflicts <- subset(table.Conflicts, Scenario %in% "Current")
  table.PBN.Conflicts <- subset(table.Conflicts, Scenario %in% "PBN")

  airport.location <- data.frame(
    airport <- c("LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"),
    lng <- c(16.29800033569336,18.268199920654297,16.0687999725,13.922200202941895,15.346699714660645,14.3930997849,14.570300102233887,16.67970085144043,18.810199737548828),
    lat <- c(43.53889846801758,42.5614013671875,45.7429008484,44.89350128173828,44.108299255371094,44.5657997131,45.21689987182617,43.285701751708984,45.46269989013672)
  )
  names(airport.location) <- c("airport","lng","lat")
  
  TMA <- c("TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB")
  table.current.Conflicts.NonTMA <- subset(table.current.Conflicts, !(Sector %in% TMA))
  table.current.Conflicts.TMA <- subset(table.current.Conflicts, Sector %in% TMA)
  table.PBN.Conflicts.NonTMA <- subset(table.PBN.Conflicts, !(Sector %in% TMA))
  table.PBN.Conflicts.TMA <- subset(table.PBN.Conflicts, Sector %in% TMA)
  labels.current.NonTMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>Severity: %s<br/>Vertical Severity: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.current.Conflicts.NonTMA$ID,
    table.current.Conflicts.NonTMA$Sector,
    table.current.Conflicts.NonTMA$Closest_Time,
    table.current.Conflicts.NonTMA$ConflictType,
    table.current.Conflicts.NonTMA$Severity,
    table.current.Conflicts.NonTMA$VerticalSeverity,
    table.current.Conflicts.NonTMA$FlightPlanPhase1,
    table.current.Conflicts.NonTMA$FlightPlanPhase2,
    table.current.Conflicts.NonTMA$LateralSeparation,
    table.current.Conflicts.NonTMA$ReqLateralSeparation,
    table.current.Conflicts.NonTMA$VerticalSeparation,
    table.current.Conflicts.NonTMA$ReqVerticalSeparation,
    table.current.Conflicts.NonTMA$Altitude_ft) %>% lapply(htmltools::HTML)
  labels.current.TMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>Severity: %s<br/>Vertical Severity: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.current.Conflicts.TMA$ID,
    table.current.Conflicts.TMA$Sector,
    table.current.Conflicts.TMA$Closest_Time,
    table.current.Conflicts.TMA$ConflictType,
    table.current.Conflicts.TMA$Severity,
    table.current.Conflicts.TMA$VerticalSeverity,
    table.current.Conflicts.TMA$FlightPlanPhase1,
    table.current.Conflicts.TMA$FlightPlanPhase2,
    table.current.Conflicts.TMA$LateralSeparation,
    table.current.Conflicts.TMA$ReqLateralSeparation,
    table.current.Conflicts.TMA$VerticalSeparation,
    table.current.Conflicts.TMA$ReqVerticalSeparation,
    table.current.Conflicts.TMA$Altitude_ft) %>% lapply(htmltools::HTML)
  labels.PBN.NonTMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>Severity: %s<br/>Vertical Severity: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.PBN.Conflicts.NonTMA$ID,
    table.PBN.Conflicts.NonTMA$Sector,
    table.PBN.Conflicts.NonTMA$Closest_Time,
    table.PBN.Conflicts.NonTMA$ConflictType,
    table.PBN.Conflicts.NonTMA$Severity,
    table.PBN.Conflicts.NonTMA$VerticalSeverity,
    table.PBN.Conflicts.NonTMA$FlightPlanPhase1,
    table.PBN.Conflicts.NonTMA$FlightPlanPhase2,
    table.PBN.Conflicts.NonTMA$LateralSeparation,
    table.PBN.Conflicts.NonTMA$ReqLateralSeparation,
    table.PBN.Conflicts.NonTMA$VerticalSeparation,
    table.PBN.Conflicts.NonTMA$ReqVerticalSeparation,
    table.PBN.Conflicts.NonTMA$Altitude_ft) %>% lapply(htmltools::HTML)
  labels.PBN.TMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>Severity: %s<br/>Vertical Severity: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.PBN.Conflicts.TMA$ID,
    table.PBN.Conflicts.TMA$Sector,
    table.PBN.Conflicts.TMA$Closest_Time,
    table.PBN.Conflicts.TMA$ConflictType,
    table.PBN.Conflicts.TMA$Severity,
    table.PBN.Conflicts.TMA$VerticalSeverity,
    table.PBN.Conflicts.TMA$FlightPlanPhase1,
    table.PBN.Conflicts.TMA$FlightPlanPhase2,
    table.PBN.Conflicts.TMA$LateralSeparation,
    table.PBN.Conflicts.TMA$ReqLateralSeparation,
    table.PBN.Conflicts.TMA$VerticalSeparation,
    table.PBN.Conflicts.TMA$ReqVerticalSeparation,
    table.PBN.Conflicts.TMA$Altitude_ft) %>% lapply(htmltools::HTML)
  
  pal <- colorNumeric(
    palette = brewer.pal(9,"YlOrRd"),
    domain = table.Conflicts$Severity)
  
  leaflet() %>% 
    setView(lng = 16.8, lat = 44.2, zoom = 7) %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group="Default") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Greyscale") %>% 
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
    addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_DUBROVNIK",select=c(Longitude,Latitude))),
                group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Dubrovnik",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_SPLIT",select=c(Longitude,Latitude))),
                group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Split",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_ZADAR",select=c(Longitude,Latitude))),
                group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Zadar",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_PULA",select=c(Longitude,Latitude))),
                group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Pula",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_ZAGREB",select=c(Longitude,Latitude))),
                group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Zagreb",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = Polygon(subset(table.SectorPolygons,Sector %in% "TMA_OSIJEK",select=c(Longitude,Latitude))),
                group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Osijek",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addCircleMarkers(data = airport.location,
                     lng=~lng,
                     lat=~lat,
                     label=~airport,
                     labelOptions=labelOptions(noHide=T, textsize="8px", direction="bottom", opacity = 0.8),
                     radius = 0, stroke = F, fillOpacity = 0, group="Airports") %>%
    addCircleMarkers(data = table.current.Conflicts.NonTMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="orange",
                     fillColor=~pal(Severity),
                     label=labels.current.NonTMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="Current Non-TMA Conflicts") %>%
    addCircleMarkers(data = table.current.Conflicts.TMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="red",
                     fillColor=~pal(Severity),
                     label=labels.current.TMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="Current TMA Conflicts") %>%
    addCircleMarkers(data = table.PBN.Conflicts.NonTMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="purple",
                     fillColor=~pal(Severity),
                     label=labels.PBN.NonTMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="PBN Non-TMA Conflicts") %>%
    addCircleMarkers(data = table.PBN.Conflicts.TMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="blue",
                     fillColor=~pal(Severity),
                     label=labels.PBN.TMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="PBN TMA Conflicts") %>%
    addLegend("bottomleft",
              colors=c("red","orange","blue","lightblue"),
              labels = c("Current TMA Conflicts","Current Non-TMA Conflicts","PBN TMA Conflicts","PBN Non-TMA Conflicts"),
              title="Conflict Type") %>%
    addLayersControl(overlayGroups = c("Airports","TMA Sectors", "Current TMA Conflicts","Current Non-TMA Conflicts","PBN TMA Conflicts","PBN Non-TMA Conflicts"),
                     baseGroups = c("Default","Greyscale","Satellite"),
                     options = layersControlOptions(collapsed = F)) %>%
    hideGroup(c("PBN TMA Conflicts","PBN Non-TMA Conflicts"))
}

plotlyConflictCount <- function(data = c("Current","PBN"), group="Conflict Type", arrange="Vertical"){
  palette.current <- brewer.pal(n = 8, name = "Spectral")
  palette.PBN <- rainbow(12)[1:8]
  if (group == "Conflict Type") {
    
    d <- subset(table.ConflictType, Sector %in% list.sectors)
    d$ConflictType <- paste(d$Scenario, d$ConflictType)
    
    g <- d %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~ConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~ConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~ConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Conflict Type (Lateral)") {
    
    d <- subset(table.LateralConflictType, Sector %in% list.sectors)
    d$LateralConflictType <- paste(d$Scenario, d$LateralConflictType)
    
    g <- d %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~LateralConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~LateralConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~LateralConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Conflict Type (Vertical)") {
    
    d <- subset(table.VerticalConflictType, Sector %in% list.sectors)
    d$VerticalConflictType <- paste(d$Scenario, d$VerticalConflictType)
    
    g <- d %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~VerticalConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~VerticalConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~freq, color=~VerticalConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Flight Phase") {
    
    d <- subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors)
    d$FlightPlanPhases <- paste(d$Scenario, d$FlightPlanPhases)
    
    g <- d %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=palette.PBN, type="bar")
    
  } else if (group == "Severity") {
    
    d <- subset(table.Severity, Sector %in% list.sectors)
    d$Severity <- paste(d$Scenario, d$Severity)
    
    g <- d %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~freq, color=~Severity, colors=c(brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)],brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)]), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~freq, color=~Severity, colors=brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)], type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~freq, color=~Severity, colors=brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)], type="bar")
    
  } else if (group == "Severity (Vertical)") {
    
    d <- subset(table.VerticalSeverity, Sector %in% list.sectors)
    d$VerticalSeverity <- paste(d$Scenario, d$VerticalSeverity)
    
    g <- d %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~freq, color=~VerticalSeverity, colors=c(brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)],brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)]), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~freq, color=~VerticalSeverity, colors=brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)], type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~freq, color=~VerticalSeverity, colors=brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)], type="bar")
    
  }
  
  g <- g %>% layout(hovermode="compare", dragmode="zoom", barmode="group", title=paste("Conflicts per Sector by", group), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto", fixedrange=T),
                    yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", dragmode="zoom", barmode="stack", title=paste("Current Conflicts per Sector by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", dragmode="zoom", barmode="stack", title=paste("PBN Conflicts per Sector by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Conflicts per Sector by", group)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Conflicts per Sector by", group)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotlySectorEntry <- function(data = c("Current","PBN"), sector="All", arrange="Vertical"){
  if (sector == "All") {
    d <- subset(table.SectorEntry, Sector %in% list.sectors | Sector %in% "All TMA")
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(table.SectorEntry, Sector %in% sector)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  d$Sector <- paste(d$Scenario,d$Sector)
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~Sector, colors=c(palette.current,palette.PBN), type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Rolling Hourly Sector Entries"), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of entries")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~Sector, colors=palette.current, type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Rolling Hourly Sector Entries"), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of entries")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~Sector, colors=palette.PBN, type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("PBN Rolling Hourly Sector Entries"), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of entries")) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title="Rolling Hourly Sector Entries"))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title="Rolling Hourly Sector Entries"))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotlySectorOccupancy <- function(data = c("Current","PBN"), sector="All", arrange="Vertical"){
  if (sector == "All") {
    d <- subset(table.SectorOccupancy, Sector %in% list.sectors | Sector %in% "All TMA")
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(table.SectorOccupancy, Sector %in% sector)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  d$Sector <- paste(d$Scenario,d$Sector)
  #d <- subset(d, substr(second(ms(d$Time)),2,2) %in% "0")
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~Sector, colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Sector Occupancy Count per Minute"), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)

  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~Sector, colors=palette.current, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Sector Occupancy Count per Minute"), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~Sector, colors=palette.PBN, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("PBN Sector Occupancy Count per Minute"), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title="Sector Occupancy Count per Minute"))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title="Sector Occupancy Count per Minute"))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}

plotlyControllerWorkload <- function(data = c("Current","PBN"), sector="All", arrange="Vertical"){
  if (sector == "All") {
    d <- subset(table.Workload, Sector %in% list.sectors | Sector %in% "All TMA")
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(table.Workload, Sector %in% sector)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  d$Sector <- paste(d$Scenario,d$Sector)
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentHourlyWorkload, color=~Sector, colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Rolling Hourly Percentage Radar Controller Workload", sector), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload (Hourly %)")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentHourlyWorkload, color=~Sector, colors=palette.current, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Rolling Hourly Percentage Radar Controller Workload", sector), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload (Hourly %)")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentHourlyWorkload, color=~Sector, colors=palette.PBN, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("PBN Rolling Hourly Percentage Radar Controller Workload", sector), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload (Hourly %)")) %>% config(collaborate=F)
  
  if ("Current" %in% data & "PBN" %in% data) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Rolling Hourly Percentage Radar Controller Workload", sector)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Rolling Hourly Percentage Radar Controller Workload", sector)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% data & !("PBN" %in% data)) {
    return(g1)
  } else if (!("Current" %in% data) & "PBN" %in% data) {
    return(g2)
  }
}