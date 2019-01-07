source("data.R", local = T)

choices.operations <- c("Current Runway 1","Current Runway 2","PBN Runway 1", "PBN Runway 2")
choices.kpa <- c("Throughput","Efficiency","Safety","Sector Capacity")
choices.throughputs <- c("Total Throughput","Hourly Throughput","Rolling Hourly Throughput")
choices.efficiency <- c("Track Miles","Fuel Burn")
choices.efficiencygrouping <- c("Airport","Airport (Combined)", "Airport (Arrivals Only)", "Airport (Departures Only)", "Routing", "Routing (Combined)")
choices.safety <- c("Conflict Map", "Conflict Statistics")
choices.conflictsgrouping <- c("Conflict Type","Conflict Type (Lateral)","Conflict Type (Vertical)","Flight Phase","Severity","Severity (Vertical)")
choices.sectorcapacity <- c("Sector Entry Count","Sector Occupancy Count","Controller Workload")

plotlyTotalThroughput <- function(data = c(choices.operations), airport = "All", arrange="Vertical"){
  
  if (airport == "All") {
    d <- subset(table.TotalThroughputs, Airport %in% list.airports)
    barmode <- "stack"
  } else {
    d <- subset(table.TotalThroughputs, Airport %in% airport)
    barmode <- "dodge"
  }
  
  g <- subset(d, Scenario %in% data)
  g1 <- subset(d,Scenario %in% data[1])
  g2 <- subset(d,Scenario %in% data[2])
  g3 <- subset(d,Scenario %in% data[3])
  g4 <- subset(d,Scenario %in% data[4]) 

  g <- g1 <- g2 <- g3 <- g4 %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~Category, type="bar") %>%
    layout(hovermode="compare", barmode=barmode, title=~paste(paste(Scenario,collapse=', '), "Total Throughputs"), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  if (length(data) == 1) {
    return(g)
  } else if (length(data) == 2) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Group") {
      return(g %>% layout(barmode="group"))
    } else if (arrange == "Stack") {
      return(g %>% layout(barmode="stack"))
    }
  } else if (length(data) == 3) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,g3,shareY=T,shareX=T,nrows=1) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,g3,shareY=T,shareX=T,nrows=3) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Group") {
      return(g %>% layout(barmode="group"))
    } else if (arrange == "Stack") {
      return(g %>% layout(barmode="stack"))
    }
  } else if (length(data) == 4) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,g3,g4,shareY=T,shareX=T,nrows=2) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g3,g2,g4,shareY=T,shareX=T,nrows=2) %>% layout(barmode=barmode,title="Total Throughputs"))
    } else if (arrange == "Group") {
      return(g %>% layout(barmode="group"))
    } else if (arrange == "Stack") {
      return(g %>% layout(barmode="stack"))
    }
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

  } else if (group == "Routing (Combined)") {
    
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
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=c(palette.current,palette.PBN), type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g1 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.current, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
    g2 <- subset(d, !(Waypoint %in% list.airports) & !(RoutingType %in% "Both") & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.PBN, type="box") %>%
      layout(boxmode="group", xaxis=list(title="Route"))
    
  } else if (group == "Routing (Combined)") {
    
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