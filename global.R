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
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g1 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g2 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="box") %>%
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
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g1 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g2 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="box") %>%
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
  
  polygon.DUBROVNIK <- Polygon(cbind(
    c(16.16194444444444,16.35388888888889,17.61638888888889,17.75861111111111,18.19444444444445,18.38111111111111,18.66361111111111,18.45027777777778,17.95305555555555,17.78277777777778,17.68944444444444,17.43083333333334,17.27722222222222,17.22777777777778,17.19944444444445,17.125,16.84694444444444,16.61666666666667,16.2763888888888900,15.93611111111111,16.16194444444444),
    c(42.43361111111111,42.34722222222222,41.75333333333333,41.91861111111111,41.93277777777777,41.90027777777777,41.94388888888889,42.63666666666666,42.89333333333333,42.98083333333334,43.02861111111111,43.18027777777778,43.35166666666667,43.43944444444444,43.49583333333334,43.51694444444444,43.30166666666666,43.12194444444445,42.8518055555555600,42.58166666666667,42.43361111111111)))
  polygon.OSIJEK <- Polygon(cbind(
    c(17.865000000000002,17.981666666666666,18.099166666666665,18.21611111111111,18.329166666666666,18.413055555555555,18.519166666666667,18.611944444444447,18.636388888888888,18.772777777777776,18.849444444444444,19.011111111111113,19.012777777777778,19.39638888888889,19.395555555555553,19.0775,19.078055555555554,19.095555555555553,19.035833333333336,18.727777777777778,18.381666666666668,18.002777777777776,17.861666666666668,17.86416666666667,17.865000000000002),
    c(45.770833333333336,45.79194444444444,45.76694444444444,45.78055555555555,45.754444444444445,45.74583333333334,45.784166666666664,45.83638888888889,45.87861111111111,45.88805555555555,45.876666666666665,45.405833333333334,45.400555555555556,45.22416666666667,45.21222222222222,45.126666666666665,45.1225,45.00416666666667,44.90833333333333,45.003055555555555,45.10194444444445,45.06083333333333,45.04472222222222,45.740833333333335,45.770833333333336)))
  polygon.PULA <- Polygon(cbind(
    c(12.99555555555556,13.32888888888889,13.49805555555555,13.75972222222222,14.24638888888889,14.39722222222222,14.65916666666667,14.74444444444444,14.98222222222222,15.15,14.94111111111111,14.8675,14.73916666666667,14.6025,14.44861111111111,14.22444444444444,14.19833333333333,14.13388888888889,13.97194444444445,13.77333333333333,13.66666666666667,13.58472222222222,13.53666666666667,13.38722222222222,12.99555555555556,12.99555555555556,12.99555555555556),
    c(45.16638888888889,44.53305555555556,44.38638888888889,44.15722222222222,44.34722222222222,44.40527777777778,44.53305555555556,44.595,44.76611111111111,44.88555555555556,45.48166666666666,45.46583333333334,45.52333333333333,45.49833333333333,45.46916666666667,45.50138888888888,45.47083333333334,45.47555555555557,45.44777777777777,45.46555555555556,45.44749999999999,45.47583333333333,45.49805555555556,45.55805555555556,45.29972222222222,45.2625,45.16638888888889)))
  polygon.SPLIT <- Polygon(cbind(
    c(14.42472222222222,14.49555555555556,14.63027777777778,15.93611111111111,17.07527777777778,17.43083333333334,17.27722222222222,17.22777777777778,17.19944444444445,17.125,17.125,17.02083333333333,16.86416666666667,16.72944444444444,16.72944444444444,16.71611111111111,16.67111111111111,16.27444444444445,15.91388888888889,15.90277777777778,15.77583333333334,15.70638888888889,15.64027777777778,15.62027777777778,15.36416666666667,15.20388888888889,14.32888888888889,14.42472222222222),
    c(43.56416666666667,43.5,43.41777777777777,42.58166666666667,42.68,43.18027777777778,43.35166666666667,43.43944444444444,43.49583333333334,43.51694444444444,43.51694444444444,43.56861111111112,43.70972222222223,43.79222222222222,43.79222222222222,43.83611111111112,43.87611111111112,44.1675,44.13916666666667,44.10416666666667,44.14111111111111,44.12222222222222,44.09638888888889,44.11555555555556,44.09527777777778,44.0275,43.65055555555556,43.56416666666667)))
  polygon.ZADAR <- Polygon(cbind(
    c(14.63583333333333,14.25916666666667,13.49805555555555,13.75972222222222,14.32888888888889,15.41861111111111,15.6425,15.77861111111111,15.90277777777778,15.91388888888889,15.36555555555556,15.15,14.63583333333333),
    c(44.82694444444445,44.68305555555556,44.38638888888889,44.15722222222222,43.65055555555556,43.62361111111111,43.61666666666667,43.75833333333333,44.10416666666667,44.13916666666667,44.89277777777778,44.88555555555556,44.82694444444445)))
  polygon.ZAGREB <- Polygon(cbind(
    c(15.4275,15.35333333333333,15.35,15.34944444444444,15.29722222222222,15.29722222222222,15.09027777777778,15.18333333333334,15.75805555555555,15.76972222222222,15.77777777777778,15.77777777777778,16.90861111111111,16.95416666666667,17,17.05944444444445,17.16388888888889,17.38944444444444,17.29805555555556,17.28916666666667,17.18111111111111,17.05111111111111,16.96611111111111,16.83722222222222,16.72055555555555,16.58861111111111,16.5225,16.46972222222222,16.38138888888889,16.27305555555556,16.26166666666667,16.27416666666667,16.14694444444444,16.05333333333334,16.06694444444445,15.97611111111111,15.90194444444444,15.86666666666667,15.82611111111111,15.67361111111111,15.625,15.70305555555555,15.655,15.49111111111111,15.4275),
    c(45.79444444444443,45.77305555555555,45.66333333333332,45.63583333333333,45.57361111111111,45.57361111111111,45.51666666666667,45.42416666666667,45.07055555555556,45.07888888888888,45.17527777777778,45.17527777777778,44.56888888888889,44.69888888888889,44.83333333333334,44.96472222222223,45.15333333333333,45.93166666666667,45.99027777777778,46.02861111111112,46.12083333333334,46.20333333333333,46.23972222222222,46.37416666666667,46.39138888888889,46.44805555555556,46.47833333333334,46.50416666666667,46.5325,46.5175,46.45500000000001,46.37833333333333,46.40333333333332,46.38805555555555,46.34083333333334,46.30972222222222,46.28666666666666,46.27166666666666,46.26611111111112,46.22638888888889,46.17222222222222,45.97277777777778,45.84027777777778,45.8075,45.79444444444443)))
  
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
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.current.Conflicts.NonTMA$ID,
    table.current.Conflicts.NonTMA$Sector,
    table.current.Conflicts.NonTMA$Closest_Time,
    table.current.Conflicts.NonTMA$ConflictType,
    table.current.Conflicts.NonTMA$FlightPlanPhase1,
    table.current.Conflicts.NonTMA$FlightPlanPhase2,
    table.current.Conflicts.NonTMA$LateralSeparation,
    table.current.Conflicts.NonTMA$ReqLateralSeparation,
    table.current.Conflicts.NonTMA$VerticalSeparation,
    table.current.Conflicts.NonTMA$ReqVerticalSeparation,
    table.current.Conflicts.NonTMA$Altitude_ft) %>% lapply(htmltools::HTML)
  labels.current.TMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.current.Conflicts.TMA$ID,
    table.current.Conflicts.TMA$Sector,
    table.current.Conflicts.TMA$Closest_Time,
    table.current.Conflicts.TMA$ConflictType,
    table.current.Conflicts.TMA$FlightPlanPhase1,
    table.current.Conflicts.TMA$FlightPlanPhase2,
    table.current.Conflicts.TMA$LateralSeparation,
    table.current.Conflicts.TMA$ReqLateralSeparation,
    table.current.Conflicts.TMA$VerticalSeparation,
    table.current.Conflicts.TMA$ReqVerticalSeparation,
    table.current.Conflicts.TMA$Altitude_ft) %>% lapply(htmltools::HTML)
  labels.PBN.NonTMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.PBN.Conflicts.NonTMA$ID,
    table.PBN.Conflicts.NonTMA$Sector,
    table.PBN.Conflicts.NonTMA$Closest_Time,
    table.PBN.Conflicts.NonTMA$ConflictType,
    table.PBN.Conflicts.NonTMA$FlightPlanPhase1,
    table.PBN.Conflicts.NonTMA$FlightPlanPhase2,
    table.PBN.Conflicts.NonTMA$LateralSeparation,
    table.PBN.Conflicts.NonTMA$ReqLateralSeparation,
    table.PBN.Conflicts.NonTMA$VerticalSeparation,
    table.PBN.Conflicts.NonTMA$ReqVerticalSeparation,
    table.PBN.Conflicts.NonTMA$Altitude_ft) %>% lapply(htmltools::HTML)
  labels.PBN.TMA <- sprintf(
    "ID: %s<br/>Sector: %s<br/>Time: %s<br/>Type: %s<br/>FP Phase: %s %s<br/>Lateral Sep. (NM): %s (Req. %s)<br/>Vertical Sep. (ft): %s (Req. %s)<br/>Altitude (ft): %s",
    table.PBN.Conflicts.TMA$ID,
    table.PBN.Conflicts.TMA$Sector,
    table.PBN.Conflicts.TMA$Closest_Time,
    table.PBN.Conflicts.TMA$ConflictType,
    table.PBN.Conflicts.TMA$FlightPlanPhase1,
    table.PBN.Conflicts.TMA$FlightPlanPhase2,
    table.PBN.Conflicts.TMA$LateralSeparation,
    table.PBN.Conflicts.TMA$ReqLateralSeparation,
    table.PBN.Conflicts.TMA$VerticalSeparation,
    table.PBN.Conflicts.TMA$ReqVerticalSeparation,
    table.PBN.Conflicts.TMA$Altitude_ft) %>% lapply(htmltools::HTML)
  leaflet() %>% 
    setView(lng = 16.8, lat = 44.2, zoom = 7) %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group="Default") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Greyscale") %>% 
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
    addPolygons(data = polygon.DUBROVNIK, group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Dubrovnik",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = polygon.SPLIT, group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Split",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = polygon.ZADAR, group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Zadar",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = polygon.PULA, group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Pula",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = polygon.ZAGREB, group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Zagreb",
                labelOptions=labelOptions(style = list("font-weight" = "bold"), opacity = 1, textsize="12px", direction = "auto"),
                highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.35, bringToFront = F)) %>%
    addPolygons(data = polygon.OSIJEK, group="TMA Sectors", weight = 5, opacity = 0.5, color = "gray", dashArray = "18", label = "TMA Osijek",
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
                     color="black",
                     fillColor="orange",
                     label=labels.current.NonTMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="Current Non-TMA Conflicts") %>%
    addCircleMarkers(data = table.current.Conflicts.TMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="black",
                     fillColor="red",
                     label=labels.current.TMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="Current TMA Conflicts") %>%
    addCircleMarkers(data = table.PBN.Conflicts.NonTMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="black",
                     fillColor="lightblue",
                     label=labels.PBN.NonTMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="PBN Non-TMA Conflicts") %>%
    addCircleMarkers(data = table.PBN.Conflicts.TMA,
                     lng=~Longitude,
                     lat=~Latitude,
                     color="black",
                     fillColor="blue",
                     label=labels.PBN.TMA,
                     labelOptions=labelOptions(textsize="13px", direction="auto"),
                     radius = 5, stroke = TRUE, fillOpacity = 0.8, group="PBN TMA Conflicts") %>%
    addLegend("bottomleft",
              colors=c("red","orange","blue","lightblue"),
              labels = c("Current TMA Conflicts","Current Non-TMA Conflicts","PBN TMA Conflicts","PBN Non-TMA Conflicts"),
              title="Conflict Type") %>%
    addLayersControl(overlayGroups = c("Airports","TMA Sectors", "Current TMA Conflicts","Current Non-TMA Conflicts","PBN TMA Conflicts","PBN Non-TMA Conflicts"),
                     baseGroups = c("Default","Greyscale","Satellite"),
                     options = layersControlOptions(collapsed = F))
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
    plot_ly(x=~Time, y=~Workload, color=~Sector, colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Rolling Hourly Percentage Radar Controller Workload", sector), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload (Hourly %)")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Workload, color=~Sector, colors=palette.current, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Rolling Hourly Percentage Radar Controller Workload", sector), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload (Hourly %)")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Workload, color=~Sector, colors=palette.PBN, type="scatter", mode="line") %>%
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