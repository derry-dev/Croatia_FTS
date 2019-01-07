source("data.R", local = T)

# Change these lists if you want to include other airports/sectors
list.airports <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE)
list.sectors <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

# Functions
'%!in%' <- function(x,y){!('%in%'(x,y))}

plotlyTotalThroughput <- function(operation=c("Current","PBN"), runway="1", airport="All", arrange="Vertical", run="1"){
  d <- subset(table.TotalThroughputs, Runway %in% runway & Run %in% run & Category %in% c("Arrivals","Departures"))
  if (airport == "All") {
    d <- subset(d, Airport %in% list.airports)
    barmode <- "stack"
    title1 <- paste("Total Throughputs for All Airports","<br>","RD",runway,"Run",run)
    title2 <- paste("Total Throughputs for All Airports","<br>",operation,"RD",runway,"Run",run)
  } else {
    d <- subset(d, Airport %in% airport)
    barmode <- "dodge"
    title1 <- paste("Total Throughputs for",airport,"<br>","RD",runway,"Run",run)
    title2 <- paste("Total Throughputs for",airport,"<br>",operation,"RD",runway,"Run",run)
  }
  
  g <- d %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue","green","blue"), type="bar") %>%
    layout(hovermode="compare", title=title1, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue"), type="bar") %>%
    layout(hovermode="compare", barmode=barmode, title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~paste(Scenario,Category), colors=c("green","blue"), type="bar") %>%
    layout(hovermode="compare", barmode=barmode, title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(barmode=barmode,title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(barmode=barmode,title=title1))
    } else if (arrange == "Group") {
      return(g %>% layout(barmode="group"))
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyHourlyThroughput <- function(operation=c("Current","PBN"), runway="1", airport="All", arrange="Vertical", run="1"){
  d <- subset(table.HourlyThroughputs, Runway %in% runway & Run %in% run & Category %in% c("Arrivals","Departures"))
  if (airport == "All") {
    d <- subset(d, Airport %in% list.airports)
    title1 <- paste("Hourly Throughputs for All Airports","<br>","RD",runway,"Run",run)
    title2 <- paste("Hourly Throughputs for All Airports","<br>",operation,"RD",runway,"Run:",run)
  } else {
    d <- subset(d, Airport %in% airport)
    title1 <- paste("Hourly Throughputs for",airport,"<br>","RD",runway,"Run",run)
    title2 <- paste("Hourly Throughputs for",airport,"<br>",operation,"RD",runway,"Run:",run)
  }
  
  g <- d %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue","green","blue"), type="bar") %>%
    layout(hovermode="compare", title=title1, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d, Scenario %in% "Current") %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue"), type="bar") %>%
    layout(hovermode="compare", barmode="stack", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~paste(Scenario,Category), colors=c("green","blue"), type="bar") %>%
    layout(hovermode="compare", barmode="stack", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(barmode="stack",title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(barmode="stack",title=title1))
    } else if (arrange == "Group") {
      return(g %>% layout(barmode="group"))
    } else if (arrange == "Stack") {
      return(g %>% layout(barmode="stack"))
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyRollingHourlyThroughput <- function(operation=c("Current","PBN"), runway="1", airport="All", arrange="Vertical", run="1"){
  d <- subset(table.RollingHourlyThroughputs, Runway %in% runway & Run %in% run) 
  if (airport == "All") {
    d <- subset(d, Airport %in% "All Major Airports")
    title1 <- paste("Rolling Hourly Throughputs for All Airports","<br>","RD",runway,"Run",run)
    title2 <- paste("Rolling Hourly Throughputs for All Airports","<br>",operation,"RD",runway,"Run",run)
  } else {
    d <- subset(d, Airport %in% airport)
    title1 <- paste("Rolling Hourly Throughputs for",airport,"<br>","RD",runway,"Run",run)
    title2 <- paste("Rolling Hourly Throughputs for",airport,"<br>",operation,"RD",runway,"Run",run)
  }
  
  g <- d %>% group_by(Category) %>% 
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkred","darkblue","green","red","blue"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=title1, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d, Scenario %in% "Current") %>% group_by(Category) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkred","darkblue"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Category) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Category), colors=c("green","red","blue"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)

  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyFuelBurn <- function(operation=c("Current","PBN"), runway="1", group="Airport", airport="All",arrange="Vertical", run="1") {
  if (run == "All Runs") {
    titletemp2 <- run
  } else {
    titletemp2 <- paste("Run",run)
  }
  if (airport == "All") {
    titletemp1 <- "All Airports"
    palette.current <- brewer.pal(n = 8, name = "Spectral")
    palette.PBN <- rainbow(12)[1:8]
  } else {
    titletemp1 <- airport
    palette.current <- "Blue"
    palette.PBN <- "Red"
  }
  
  title1 <- paste("Fuel Burn for",titletemp1,"by",group,"<br>","RD",runway,titletemp2)
  title2 <- paste("Fuel Burn for",titletemp1,"by",group,"<br>",operation,"RD",runway,titletemp2)
  
  if (group == "Airport (Combined)") {
    
    if (run == "All Runs") {
      d <- table.FuelBurn
      agg <- aggregate(data = table.FuelBurn,FuelBurn~Airport+RoutingType+Waypoint+Scenario+Runway,"sum")
      agg$Run <- "All Runs"
      d <- subset(rbind(d,agg), Runway %in% runway & Run %in% "All Runs")
    } else {
      d <- subset(table.FuelBurn, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d <- subset(d, Airport %in% list.airports)
    } else {
      d <- subset(d, Airport %in% airport)
    }
    
    g <- subset(d, RoutingType %!in% "Arrivals and Departures" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,RoutingType), colors=c(palette.current,palette.PBN), type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %!in% "Arrivals and Departures" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,RoutingType), colors=palette.current, type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %!in% "Arrivals and Departures" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,RoutingType), colors=palette.PBN, type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))

  } else if (group == "Airport (Arrivals Only)") {
    
    if (run == "All Runs") {
      d <- table.FuelBurn
      agg <- aggregate(data = table.FuelBurn,FuelBurn~Airport+RoutingType+Waypoint+Scenario+Runway,"sum")
      agg$Run <- "All Runs"
      d <- subset(rbind(d,agg), Runway %in% runway & Run %in% "All Runs")
    } else {
      d <- subset(table.FuelBurn, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d <- subset(d, Airport %in% list.airports)
    } else {
      d <- subset(d, Airport %in% airport)
    }
    
    g <- subset(d, RoutingType %in% "Arrival" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario), colors=c(palette.current,palette.PBN), type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Arrival" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Arrival" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
  } else if (group == "Airport (Departures Only)") {
    
    if (run == "All Runs") {
      d <- table.FuelBurn
      agg <- aggregate(data = table.FuelBurn,FuelBurn~Airport+RoutingType+Waypoint+Scenario+Runway,"sum")
      agg$Run <- "All Runs"
      d <- subset(rbind(d,agg), Runway %in% runway & Run %in% "All Runs")
    } else {
      d <- subset(table.FuelBurn, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d <- subset(d, Airport %in% list.airports)
    } else {
      d <- subset(d, Airport %in% airport)
    }
    
    g <- subset(d, RoutingType %in% "Departure" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario), colors=c(palette.current,palette.PBN), type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Departure" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Departure" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar") %>%
      layout(yaxis=list(title="Total Fuel Burn (kg)"))
    
  } else if (group == "Routing (Combined)") {
    
    if (run == "All Runs") {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway)
    } else {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d1 <- subset(d1, Airport %in% list.airports)
    } else {
      d1 <- subset(d1, Airport %in% airport)
    }
    
    g <- subset(d1, RoutingType %!in% "Arrivals and Departures") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g1 <- subset(d1, RoutingType %!in% "Arrivals and Departures" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g2 <- subset(d1, RoutingType %!in% "Arrivals and Departures" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))

  } else if (group == "Routing (Arrivals Only)") {
    
    if (run == "All Runs") {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway)
    } else {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d1 <- subset(d1, Airport %in% list.airports)
    } else {
      d1 <- subset(d1, Airport %in% airport)
    }
    
    g <- subset(d1, RoutingType %in% "Arrival") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g1 <- subset(d1, RoutingType %in% "Arrival" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g2 <- subset(d1, RoutingType %in% "Arrival" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
  } else if (group == "Routing (Departures Only)") {
    
    if (run == "All Runs") {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway)
    } else {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d1 <- subset(d1, Airport %in% list.airports)
    } else {
      d1 <- subset(d1, Airport %in% airport)
    }
    
    g <- subset(d1, RoutingType %in% "Departure") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g1 <- subset(d1, RoutingType %in% "Departure" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g2 <- subset(d1, RoutingType %in% "Departure" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
  }
  
  g <- g %>% layout(hovermode="compare", title=title1, legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto",fixedrange=T),
                    yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", title=title2, legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", title=title2, legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyTrackMiles <- function(operation=c("Current","PBN"), runway="1", group="Airport", airport="All",arrange="Vertical", run="1") {
  if (run == "All Runs") {
    titletemp2 <- run
  } else {
    titletemp2 <- paste("Run",run)
  }
  if (airport == "All") {
    titletemp1 <- "All Airports"
    palette.current <- brewer.pal(n = 8, name = "Spectral")
    palette.PBN <- rainbow(12)[1:8]
  } else {
    titletemp1 <- airport
    palette.current <- "Blue"
    palette.PBN <- "Red"
  }
  
  title1 <- paste("Track Miles for",titletemp1,"by",group,"<br>","RD",runway,titletemp2)
  title2 <- paste("Track Miles for",titletemp1,"by",group,"<br>",operation,"RD",runway,titletemp2)
  
  if (group == "Airport (Combined)") {
    
    if (run == "All Runs") {
      d <- table.TrackMiles
      agg <- aggregate(data = table.TrackMiles,TrackMiles~Airport+RoutingType+Waypoint+Scenario+Runway,"sum")
      agg$Run <- "All Runs"
      d <- subset(rbind(d,agg), Runway %in% runway & Run %in% "All Runs")
    } else {
      d <- subset(table.TrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d <- subset(d, Airport %in% list.airports)
    } else {
      d <- subset(d, Airport %in% airport)
    }
    
    g <- subset(d, RoutingType %!in% "Arrivals and Departures" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,RoutingType), colors=c(palette.current,palette.PBN), type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %!in% "Arrivals and Departures" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,RoutingType), colors=palette.current, type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %!in% "Arrivals and Departures" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,RoutingType), colors=palette.PBN, type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
  } else if (group == "Airport (Arrivals Only)") {
    
    if (run == "All Runs") {
      d <- table.TrackMiles
      agg <- aggregate(data = table.TrackMiles,TrackMiles~Airport+RoutingType+Waypoint+Scenario+Runway,"sum")
      agg$Run <- "All Runs"
      d <- subset(rbind(d,agg), Runway %in% runway & Run %in% "All Runs")
    } else {
      d <- subset(table.TrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d <- subset(d, Airport %in% list.airports)
    } else {
      d <- subset(d, Airport %in% airport)
    }
    
    g <- subset(d, RoutingType %in% "Arrival" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario), colors=c(palette.current,palette.PBN), type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Arrival" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Arrival" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
  } else if (group == "Airport (Departures Only)") {
    
    if (run == "All Runs") {
      d <- table.TrackMiles
      agg <- aggregate(data = table.TrackMiles,TrackMiles~Airport+RoutingType+Waypoint+Scenario+Runway,"sum")
      agg$Run <- "All Runs"
      d <- subset(rbind(d,agg), Runway %in% runway & Run %in% "All Runs")
    } else {
      d <- subset(table.TrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d <- subset(d, Airport %in% list.airports)
    } else {
      d <- subset(d, Airport %in% airport)
    }
    
    g <- subset(d, RoutingType %in% "Departure" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario), colors=c(palette.current,palette.PBN), type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
    g1 <- subset(d,Scenario %in% "Current" & RoutingType %in% "Departure" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
    g2 <- subset(d,Scenario %in% "PBN" & RoutingType %in% "Departure" & Waypoint %in% "All Routes") %>%
      plot_ly(x=~Airport, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="bar") %>%
      layout(yaxis=list(title="Total Track Miles (NM)"))
    
  } else if (group == "Routing (Combined)") {
    
    if (run == "All Runs") {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway)
    } else {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d1 <- subset(d1, Airport %in% list.airports)
    } else {
      d1 <- subset(d1, Airport %in% airport)
    }
    
    g <- subset(d1, RoutingType %!in% "Arrivals and Departures") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g1 <- subset(d1, RoutingType %!in% "Arrivals and Departures" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g2 <- subset(d1, RoutingType %!in% "Arrivals and Departures" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
  } else if (group == "Routing (Arrivals Only)") {
    
    if (run == "All Runs") {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway)
    } else {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d1 <- subset(d1, Airport %in% list.airports)
    } else {
      d1 <- subset(d1, Airport %in% airport)
    }
    
    g <- subset(d1, RoutingType %in% "Arrival") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g1 <- subset(d1, RoutingType %in% "Arrival" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g2 <- subset(d1, RoutingType %in% "Arrival" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
  } else if (group == "Routing (Departures Only)") {
    
    if (run == "All Runs") {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway)
    } else {
      d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway & Run %in% run)
    }
    
    if (airport == "All") {
      d1 <- subset(d1, Airport %in% list.airports)
    } else {
      d1 <- subset(d1, Airport %in% airport)
    }
    
    g <- subset(d1, RoutingType %in% "Departure") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g1 <- subset(d1, RoutingType %in% "Departure" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g2 <- subset(d1, RoutingType %in% "Departure" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
  }
  
  g <- g %>% layout(hovermode="compare", title=title1, legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto",fixedrange=T),
                    yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", title=title2, legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", title=title2, legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyConflictSummary <- function(runway="1", sector="All", run="1") {
  
  if (sector == "All") {
    d <- subset(table.FlightType, Sector %in% "All TMA" & Runway %in% runway & Run %in% run)
    title <- paste("Conflicts in All TMA Sectors","<br>","RD",runway,"Run",run)
  } else {
    d <- subset(table.FlightType, Sector %in% sector & Runway %in% runway & Run %in% run)
    title <- paste("Conflicts in",sector,"<br>","RD",runway,"Run",run)
  }
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~FlightTypes, y=~Count, color=~Scenario, colors=c("Blue","Red"), type="bar", text=~Scenario) %>%
    layout(hovermode="compare", dragmode="zoom", title=title, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T, title="Conflicting Flight Types"),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of Conflicts"))
  return(g)
  
}

plotlyConflictCount <- function(operation=c("Current","PBN"), runway="1", group="Conflict Type", arrange="Vertical", run="1"){
  if (run == "All Runs") {
    title1 <- paste("Conflicts per Sector by",group,"<br>","RD",runway,run)
    title2 <- paste("Conflicts per Sector by",group,"<br>","Current","RD",runway,run)
  } else {
    title1 <- paste("Conflicts per Sector by",group,"<br>","RD",runway,"Run",run)
    title2 <- paste("Conflicts per Sector by",group,"<br>","Current","RD",runway,"Run",run)
  }
  palette.current <- brewer.pal(n = 8, name = "Spectral")
  palette.PBN <- rainbow(12)[1:8]
  if (group == "Conflict Type") {
  
    if (run == "All Runs") {
      d <- subset(table.ConflictType, Sector %in% list.sectors & Runway %in% runway)
    } else {
      d <- subset(table.ConflictType, Sector %in% list.sectors & Runway %in% runway & Run %in% run)
    }
    d$ConflictType <- paste(d$Scenario, d$ConflictType)
    
    g <- d %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~ConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~ConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~ConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Conflict Type (Lateral)") {
    
    if (run == "All Runs") {
      d <- subset(table.LateralConflictType, Sector %in% list.sectors & Runway %in% runway)
    } else {
      d <- subset(table.LateralConflictType, Sector %in% list.sectors & Runway %in% runway & Run %in% run)
    }
    d$LateralConflictType <- paste(d$Scenario, d$LateralConflictType)
    
    g <- d %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~LateralConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~LateralConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~LateralConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Conflict Type (Vertical)") {
    
    if (run == "All Runs") {
      d <- subset(table.VerticalConflictType, Sector %in% list.sectors & Runway %in% runway)
    } else {
      d <- subset(table.VerticalConflictType, Sector %in% list.sectors & Runway %in% runway & Run %in% run)
    }
    d$VerticalConflictType <- paste(d$Scenario, d$VerticalConflictType)
    
    g <- d %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Flight Phase") {
    
    if (run == "All Runs") {
      d <- subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors & Runway %in% runway)
    } else {
      d <- subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors & Runway %in% runway & Run %in% run)
    }
    d$FlightPlanPhases <- paste(d$Scenario, d$FlightPlanPhases)
    
    g <- d %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=palette.PBN, type="bar")
    
  } else if (group == "Severity") {
    
    if (run == "All Runs") {
      d <- subset(table.Severity, Sector %in% list.sectors & Runway %in% runway)
    } else {
      d <- subset(table.Severity, Sector %in% list.sectors & Runway %in% runway & Run %in% run)
    }
    d$Severity <- paste(d$Scenario, d$Severity)
    
    g <- d %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~Count, color=~Severity, colors=c(brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)],brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)]), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~Count, color=~Severity, colors=brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)], type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~Count, color=~Severity, colors=brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)], type="bar")
    
  } else if (group == "Severity (Vertical)") {
    
    if (run == "All Runs") {
      d <- subset(table.VerticalSeverity, Sector %in% list.sectors & Runway %in% runway)
    } else {
      d <- subset(table.VerticalSeverity, Sector %in% list.sectors & Runway %in% runway & Run %in% run)
    }
    d$VerticalSeverity <- paste(d$Scenario, d$VerticalSeverity)
    
    g <- d %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalSeverity, colors=c(brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)],brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)]), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalSeverity, colors=brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)], type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalSeverity, colors=brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)], type="bar")
    
  }
  
  g <- g %>% layout(hovermode="compare", dragmode="zoom", barmode="group", title=title1, legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto", fixedrange=T),
                    yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", dragmode="zoom", barmode="stack", title=title2, legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", dragmode="zoom", barmode="stack", title=title2, legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlySectorEntry <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical", run="1"){
  d <- subset(table.SectorEntry, Runway %in% runway & Run %in% run)
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors | Sector %in% "All TMA")
    title1 <- paste("Rolling Hourly Sector Entries for All TMA Sectors","<br>","RD",runway,"Run",run)
    title2 <- paste("Rolling Hourly Sector Entries for All TMA Sectors","<br>",operation,"RD",runway,"Run",run)
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    title1 <- paste("Rolling Hourly Sector Entries for",sector,"<br>","RD",runway,"Run",run)
    title2 <- paste("Rolling Hourly Sector Entries for",sector,"<br>",operation,"RD",runway,"Run",run)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=title1, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlySectorOccupancy <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical", run="1"){
  d <- subset(table.SectorOccupancy, Runway %in% runway & Run %in% run)
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors | Sector %in% "All TMA")
    title1 <- paste("Sector Occupancy Count per Minute for All TMA Sectors","<br>","RD",runway,"Run",run)
    title2 <- paste("Sector Occupancy Count per Minute for All TMA Sectors","<br>",operation,"RD",runway,"Run",run)
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    title1 <- paste("Sector Occupancy Count per Minute for",sector,"<br>","RD",runway,"Run",run)
    title2 <- paste("Sector Occupancy Count per Minute for",sector,"<br>",operation,"RD",runway,"Run",run)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=title1, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)

  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyControllerWorkload <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical", run="1"){
  d <- subset(table.Workload, Runway %in% runway & Run %in% run)
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors)
    title1 <- paste("Rolling Hourly Percentage Radar Controller Workload for All TMA sectors","<br>","RD",runway,"Run",run)
    title2 <- paste("Rolling Hourly Percentage Radar Controller Workload for All TMA sectors","<br>",operation,"RD",runway,"Run",run)
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    title1 <- paste("Rolling Hourly Percentage Radar Controller Workload for",sector,"<br>","RD",runway,"Run",run)
    title2 <- paste("Rolling Hourly Percentage Radar Controller Workload for",sector,"<br>",operation,"RD",runway,"Run",run)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    add_lines(y=~RollingHourlyEntries, name=~paste(Scenario,Sector,"Entries"), line=list(color=c(palette.current,palette.PBN), width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=title1, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload % / Sector Entry Count")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="line") %>%
    add_lines(y=~RollingHourlyEntries, name=~paste(Scenario,Sector,"Entries"), line=list(color=palette.current, width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload % / Sector Entry Count")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="line") %>%
    add_lines(y=~RollingHourlyEntries, name=~paste(Scenario,Sector,"Entries"), line=list(color=palette.PBN, width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload % / Sector Entry Count")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyWorkloadEntries <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical", run="1"){
  
  if (run == "All Runs") {
    d <- subset(table.Workload, Runway %in% runway)
    titletemp2 <- run
  } else {
    d <- subset(table.Workload, Runway %in% runway & Run %in% run)
    titletemp2 <- paste("Run",run)
  }
  
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors)
    titletemp1 <- "All TMA Sectors"
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    titletemp1 <- sector
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  title1 <- paste("(Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for",titletemp1,"<br>","RD",runway,titletemp2)
  title2 <- paste("(Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for",titletemp1,"<br>",operation,"RD",runway,titletemp2)
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~RollingHourlyEntries, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d$Scenario,d$Sector)))) {
    g <- g %>% add_lines(data=subset(d, paste(Scenario,Sector) %in% unique(paste(d$Scenario,d$Sector))[i]),
                         y=~fitted(lm(PercentRollingHourlyWorkload~RollingHourlyEntries)), colors=c(palette.current,palette.PBN)[i])
  }
  g <- g %>% layout(hovermode="closest", dragmode="zoom", title=title1, legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
                    yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  d1 <- subset(d,Scenario %in% "Current")
  g1 <- d1 %>% group_by(Sector) %>%
    plot_ly(x=~RollingHourlyEntries, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d1$Scenario,d1$Sector)))) {
    g1 <- g1 %>% add_lines(data=subset(d1, paste(Scenario,Sector) %in% unique(paste(d1$Scenario,d1$Sector))[i]),
                           y=~fitted(lm(PercentRollingHourlyWorkload~RollingHourlyEntries)), colors=c(palette.current)[i])
  }
  g1 <- g1 %>% layout(hovermode="closest", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  d2 <- subset(d,Scenario %in% "PBN")
  g2 <- d2 %>% group_by(Sector) %>%
    plot_ly(x=~RollingHourlyEntries, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d2$Scenario,d2$Sector)))) {
    g2 <- g2 %>% add_lines(data=subset(d2, paste(Scenario,Sector) %in% unique(paste(d2$Scenario,d2$Sector))[i]),
                           y=~fitted(lm(PercentRollingHourlyWorkload~RollingHourlyEntries)), colors=c(palette.PBN)[i])
  }
  g2 <- g2 %>% layout(hovermode="closest", dragmode="zoom", title=title2, legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=title1))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyCapacityModel <- function(operation=c("Current","PBN"), runway="1", sector="TMA_DUBROVNIK", model="IFR Only", run="1"){
  
  if (run == "All Runs") {
    t1 <- subset(table.Workload, Scenario %in% "Current" & Runway %in% runway & Sector %in% sector)
    t2 <- subset(table.Workload, Scenario %in% "PBN" & Runway %in% runway & Sector %in% sector)
    titletemp <- run
  } else {
    t1 <- subset(table.Workload, Scenario %in% "Current" & Runway %in% runway & Run %in% run & Sector %in% sector)
    t2 <- subset(table.Workload, Scenario %in% "PBN" & Runway %in% runway & Run %in% run & Sector %in% sector)
    titletemp <- paste("Run",run)
  }
  t1$PRHWexp <- exp(t1$PercentRollingHourlyWorkload)
  t1$RHEexp <- exp(t1$RollingHourlyEntries)
  t1$RHIFRexp <- exp(t1$RollingHourlyIFR)
  t1$RHVFRexp <- exp(t1$RollingHourlyVFR)
  t1$RHMILexp <- exp(t1$RollingHourlyMIL)
  t1$PRHWsq <- (t1$PercentRollingHourlyWorkload)^2
  t1$RHEsq <- (t1$RollingHourlyEntries)^2
  t1$RHIFRsq <- (t1$RollingHourlyIFR)^2
  t1$RHVFRsq <- (t1$RollingHourlyVFR)^2
  t1$RHMILsq <- (t1$RollingHourlyMIL)^2
  t2$PRHWexp <- exp(t2$PercentRollingHourlyWorkload)
  t2$RHEexp <- exp(t2$RollingHourlyEntries)
  t2$RHIFRexp <- exp(t2$RollingHourlyIFR)
  t2$RHVFRexp <- exp(t2$RollingHourlyVFR)
  t2$RHMILexp <- exp(t2$RollingHourlyMIL)
  t2$PRHWsq <- (t2$PercentRollingHourlyWorkload)^2
  t2$RHEsq <- (t2$RollingHourlyEntries)^2
  t2$RHIFRsq <- (t2$RollingHourlyIFR)^2
  t2$RHVFRsq <- (t2$RollingHourlyVFR)^2
  t2$RHMILsq <- (t2$RollingHourlyMIL)^2
  
  title1 <- paste("Acceptable Rolling Hourly Workload for",sector,"<br>","Model:",model,"RD",runway,titletemp)
  title2 <- paste("Acceptable Rolling Hourly Workload for",sector,"<br>","Model:",model,"Current","RD",runway,titletemp)
  title3 <- paste("Acceptable Rolling Hourly Workload for",sector,"<br>","Model:",model,"PBN","RD",runway,titletemp)
  
  if (model == "All (Combined)") {
    
    if (sector %in% c("TMA_OSIJEK")) {
      
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries,data=t2)
      y1 <- seq(0,70,1)
      x1 <- (seq(0,70,1)-lm1$coefficients[1])/lm1$coefficients[2]
      y2 <- seq(0,70,1)
      x2 <- (seq(0,70,1)-lm2$coefficients[1])/lm2$coefficients[2]
      
    } else if (sector %in% c("TMA_DUBROVNIK","TMA_PULA","TMA_SPLIT","TMA_ZADAR") ) {
      
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t2)
      y1 <- seq(0,70,1)
      x1 <- (-lm1$coefficient[2]+sqrt(lm1$coefficient[2]^2-4*lm1$coefficient[3]*(lm1$coefficient[1]-seq(0,70,1))))/(2*lm1$coefficient[3])
      y2 <- seq(0,70,1)
      x2 <- (-lm2$coefficient[2]+sqrt(lm2$coefficient[2]^2-4*lm2$coefficient[3]*(lm2$coefficient[1]-seq(0,70,1))))/(2*lm2$coefficient[3])
      
    } else if (sector %in% c("TMA_ZAGREB")) {
      
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEexp,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEexp,data=t2)
      new1 <- data.frame(RollingHourlyEntries=seq(0,max(t1$RollingHourlyEntries),1),RHEexp=0); new1$RHEexp <- exp(new1$RollingHourlyEntries)
      while (max(predict(lm1,new1)) < 70) {
        new1 <- rbind(new1,data.frame(RollingHourlyEntries=max(new1$RollingHourlyEntries)+0.01, RHEexp=exp(max(new1$RollingHourlyEntries))+0.01))
      }
      new2 <- data.frame(RollingHourlyEntries=seq(0,max(t2$RollingHourlyEntries),1),RHEexp=0); new2$RHEexp <- exp(new2$RollingHourlyEntries)
      while (max(predict(lm2,new2)) < 70) {
        new2 <- rbind(new2,data.frame(RollingHourlyEntries=max(new2$RollingHourlyEntries)+0.01, RHEexp=exp(max(new2$RollingHourlyEntries))+0.01))
      }
      y1 <- predict(lm1,new1)
      x1 <- new1$RollingHourlyEntries
      y2 <- predict(lm2,new2)
      x2 <- new2$RollingHourlyEntries
      
    }
    
    g1 <- t1 %>% plot_ly(x=~RollingHourlyEntries) %>%
      add_lines(y=y1, x=x1, name="Current All (Combined)", line=list(color="blue"), fillcolor="rgba(50,50,255,.5)", fill="tozeroy", hoveron="points+fills") %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="Current All (Combined)", marker=list(color="blue")) %>%
      layout(title=title2, legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly Entries"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
    g2 <- t2 %>% plot_ly(x=~RollingHourlyEntries) %>%
      add_lines(y=y2, x=x2, name="PBN All (Combined)", line=list(color="red"), fillcolor="rgba(255,50,50,.5)", fill="tozeroy", hoveron="points+fills") %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="PBN All (Combined)", marker=list(color="red")) %>%
      layout(title=title3, legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly Entries"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
  } else if (model == "All (Separate)") {
    
    if (all(t1$RollingHourlyMIL == 0)) {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t1)
      g1 <- plot_ly(r=c((70-lm1$coefficient[1])/lm1$coefficient[2],(70-lm1$coefficient[1])/lm1$coefficient[3],0),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="Current All (Separate)", marker=list(color="blue"), fillcolor="rgba(50,50,255,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=title2, legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    } else {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR+RollingHourlyMIL,data=t1)
      g1 <- plot_ly(r=c((70-lm1$coefficient[1])/lm1$coefficient[2],(70-lm1$coefficient[1])/lm1$coefficient[3],(70-lm1$coefficient[1])/lm1$coefficient[4]),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="Current All (Separate)", marker=list(color="blue"), fillcolor="rgba(50,50,255,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=title3, legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    }
    
    if (all(t2$RollingHourlyMIL == 0)) {
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t2)
      g2 <- plot_ly(r=c((70-lm2$coefficient[1])/lm2$coefficient[2],(70-lm2$coefficient[1])/lm2$coefficient[3],0),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="PBN All (Separate)", marker=list(color="red"), fillcolor="rgba(255,50,50,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=title2, legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    } else {
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR+RollingHourlyMIL,data=t2)
      g2 <- plot_ly(r=c((70-lm2$coefficient[1])/lm2$coefficient[2],(70-lm2$coefficient[1])/lm2$coefficient[3],(70-lm2$coefficient[1])/lm2$coefficient[4]),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="PBN All (Separate)", marker=list(color="red"), fillcolor="rgba(255,50,50,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=title3, legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    }
    
  } else if (model == "IFR and VFR") {
    
    lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t1)
    lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t2)
    
    g1 <- plot_ly(x=c(0,(70-lm1$coefficient[1])/lm1$coefficient[2]), y=c((70-lm1$coefficient[1])/lm1$coefficient[3],0),
            name="Current IFR and VFR", marker=list(color="blue"), line=list(color="blue"), fillcolor="rgba(50,50,255,0.5)", type="scatter", mode="markers+lines", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=title2, legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly VFR")) %>% config(collaborate=F)
    
    g2 <- plot_ly(x=c(0,(70-lm2$coefficient[1])/lm2$coefficient[2]), y=c((70-lm2$coefficient[1])/lm2$coefficient[3],0),
                 name="PBN IFR and VFR", marker=list(color="red"), line=list(color="red"), fillcolor="rgba(255,50,50,0.5)", type="scatter", mode="markers+lines", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=title3, legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly VFR")) %>% config(collaborate=F)
    
  } else if (model == "IFR Only") {
    
    lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR,data=t1)
    lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR,data=t2)
    
    g1 <- t1 %>% plot_ly(x=~RollingHourlyIFR) %>%
      add_lines(y=seq(0,70,1), x=(seq(0,70,1)-lm1$coefficients[1])/lm1$coefficients[2], name="Current IFR Only", line=list(color="blue"), fillcolor="rgba(50,50,255,.5)", fill="tozeroy", hoveron="points+fills") %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="Current All (Combined)", marker=list(color="blue")) %>%
      layout(title=title2, legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
    g2 <- t2 %>% plot_ly(x=~RollingHourlyIFR) %>%
      add_lines(y=seq(0,70,1), x=(seq(0,70,1)-lm2$coefficients[1])/lm2$coefficients[2], name="PBN IFR Only", line=list(color="red"), fillcolor="rgba(255,50,50,.5)", fill="tozeroy", hoveron="points+fills") %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="PBN All (Combined)", marker=list(color="red")) %>%
      layout(title=title3, legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
  }
  if ("Current" %in% operation & "PBN" %in% operation) {
    return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=title1))
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}