source("data.R", local = T)

# Change these lists if you want to include other airports/sectors
list.airports <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE)
list.sectors <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

# Functions
'%!in%' <- function(x,y)!('%in%'(x,y))

plotlyTotalThroughput <- function(operation=c("Current","PBN"), runway="1", airport="All", arrange="Vertical"){
  d <- subset(table.TotalThroughputs, Runway %in% runway & Category %in% c("Arrivals","Departures"))
  if (airport == "All") {
    d <- subset(d, Airport %in% list.airports)
    barmode <- "stack"
  } else {
    d <- subset(d, Airport %in% airport)
    barmode <- "dodge"
  }
  
  g <- d %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue","green","blue"), type="bar") %>%
    layout(hovermode="compare", title=paste("Total Throughputs Runway", runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue"), type="bar") %>%
    layout(hovermode="compare", barmode=barmode, title=paste(operation, "Total Throughputs Runway", runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Category) %>% arrange(Airport) %>%
    plot_ly(x=~Airport, y=~Count, color=~paste(Scenario,Category), colors=c("green","blue"), type="bar") %>%
    layout(hovermode="compare", barmode=barmode, title=paste(operation, "Total Throughputs Runway", runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(barmode=barmode,title=paste("Total Throughputs for Runway", runway)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(barmode=barmode,title=paste("Total Throughputs for Runway", runway)))
    } else if (arrange == "Group") {
      return(subplot(g1,g2,shareY=T,shareX=T) %>% layout(barmode=barmode,title=paste("Total Throughputs for Runway", runway)))
      #return(g %>% layout(barmode="group"))
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyHourlyThroughput <- function(operation=c("Current","PBN"), runway="1", airport="All", arrange="Vertical"){
  d <- subset(table.HourlyThroughputs, Runway %in% runway & Category %in% c("Arrivals","Departures")) 
  if (airport == "All") {
    d <- subset(d, Airport %in% list.airports)
  } else {
    d <- subset(d, Airport %in% airport)
  }
  
  g <- d %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue","green","blue"), type="bar") %>%
    layout(hovermode="compare", title=paste("Hourly Throughputs for",airport,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d, Scenario %in% "Current") %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkblue"), type="bar") %>%
    layout(hovermode="compare", barmode="stack", title=paste("Current Hourly Throughputs for",airport,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Category) %>%
    plot_ly(x=~Hour, y=~Count, color=~paste(Scenario,Category), colors=c("green","blue"), type="bar") %>%
    layout(hovermode="compare", barmode="stack", title=paste("PBN Hourly Throughputs for",airport,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(barmode="stack",title=paste("Hourly Throughputs for",airport,"Runway",runway)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(barmode="stack",title=paste("Hourly Throughputs for",airport,"Runway",runway)))
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

plotlyRollingHourlyThroughput <- function(operation=c("Current","PBN"), runway="1", airport="All", arrange="Vertical"){
  d <- subset(table.RollingHourlyThroughputs, Runway %in% runway) 
  if (airport == "All") {
    d <- subset(d, Airport %in% "All Airports")
  } else {
    d <- subset(d, Airport %in% airport)
  }
  
  g <- d %>% group_by(Category) %>% 
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkred","darkblue","green","red","blue"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=paste("Rolling Hourly Throughputs for",airport,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g1 <- subset(d, Scenario %in% "Current") %>% group_by(Category) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Category), colors=c("darkgreen","darkred","darkblue"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=paste("Current Rolling Hourly Throughputs for",airport,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)
  
  g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Category) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Category), colors=c("green","red","blue"), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", barmode = "stack", title=paste("PBN Rolling Hourly Throughputs for",airport,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of movements")) %>% config(collaborate=F)

  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Rolling Hourly Throughputs for",airport,"Runway",runway)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Rolling Hourly Throughputs for",airport,"Runway",runway)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyFuelBurn <- function(operation=c("Current","PBN"), runway="1", group="Airport", airport="All",arrange="Vertical") {
  d <- subset(table.FuelBurn, Runway %in% runway) 
  d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway) 
  if (airport == "All") {
    d <- subset(d, Airport %in% list.airports)
    d1 <- subset(d1, Airport %in% list.airports)
  } else {
    d <- subset(d, Airport %in% airport)
    d1 <- subset(d1, Airport %in% airport)
  }
  
  palette.current <- brewer.pal(n = 8, name = "Spectral")
  palette.PBN <- rainbow(12)[1:8]
  
  if (group == "Airport (Combined)") {

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
    
    g <- subset(d1, Waypoint %!in% list.airports & RoutingType %!in% "Arrivals and Departures") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g1 <- subset(d1, Waypoint %!in% list.airports & RoutingType %!in% "Arrivals and Departures" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g2 <- subset(d1, Waypoint %!in% list.airports & RoutingType %!in% "Arrivals and Departures" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport,RoutingType), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))

  } else if (group == "Routing (Arrivals Only)") {
    
    g <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Arrival") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g1 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Arrival" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g2 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Arrival" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
  } else if (group == "Routing (Departures Only)") {
    
    g <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Departure") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g1 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Departure" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
    g2 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Departure" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~FuelBurn, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Fuel Burn (kg)"))
    
  }
  
  g <- g %>% layout(hovermode="compare", title=paste("Fuel Burn for Runway",runway,"by", group), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto",fixedrange=T),
                    yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", title=paste("Current Fuel Burn for Runway",runway,"by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", title=paste("PBN Fuel Burn for Runway",runway,"by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Fuel Burn for Runway",runway,"by", group)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Fuel Burn for Runway",runway,"by", group)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyTrackMiles <- function(operation=c("Current","PBN"), runway="1", group="Airport", airport="All",arrange="Vertical") {
  d <- subset(table.TrackMiles, Runway %in% runway)
  d1 <- subset(table.FuelBurnTrackMiles, Runway %in% runway)
  if (airport == "All") {
    d <- subset(d, Airport %in% list.airports)
    d1 <- subset(d1, Airport %in% list.airports)
  } else {
    d <- subset(d, Airport %in% airport)
    d1 <- subset(d1, Airport %in% airport)
  }
  
  palette.current <- brewer.pal(n = 8, name = "Spectral")
  palette.PBN <- rainbow(12)[1:8]
  
  if (group == "Airport (Combined)") {
    
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
    
    g <- subset(d1, Waypoint %!in% list.airports & RoutingType %!in% "Arrivals and Departures") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g1 <- subset(d1, Waypoint %!in% list.airports & RoutingType %!in% "Arrivals and Departures" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g2 <- subset(d1, Waypoint %!in% list.airports & RoutingType %!in% "Arrivals and Departures" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport,RoutingType), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
  } else if (group == "Routing (Arrivals Only)") {
    
    g <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Arrival") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g1 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Arrival" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g2 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Arrival" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
  } else if (group == "Routing (Departures Only)") {
    
    g <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Departure") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=c(palette.current,palette.PBN), type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g1 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Departure" & Scenario %in% "Current") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.current, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
    g2 <- subset(d1, Waypoint %!in% list.airports & RoutingType %in% "Departure" & Scenario %in% "PBN") %>%
      plot_ly(x=~Waypoint, y=~TrackMiles, color=~paste(Scenario,Airport), colors=palette.PBN, type="box", boxmean=T) %>%
      layout(boxmode="group", xaxis=list(title="Route"), yaxis=list(title="Track Miles (NM)"))
    
  }
  
  g <- g %>% layout(hovermode="compare", title=paste("Track Miles for Runway",runway,"by", group), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto",fixedrange=T),
                    yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", title=paste("Current Track Miles for Runway",runway,"by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", boxmode="group", title=paste("PBN Track Miles for Runway",runway,"by", group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto",fixedrange=T),
                      yaxis=list(tickmode="auto",fixedrange=T)) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Track Miles for Runway",runway,"by", group)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Track Miles for Runway",runway,"by", group)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyConflictCount <- function(operation=c("Current","PBN"), runway="1", group="Conflict Type", arrange="Vertical"){
  palette.current <- brewer.pal(n = 8, name = "Spectral")
  palette.PBN <- rainbow(12)[1:8]
  if (group == "Conflict Type") {
    
    d <- subset(table.ConflictType, Sector %in% list.sectors & Runway %in% runway)
    d$ConflictType <- paste(d$Scenario, d$ConflictType)
    
    g <- d %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~ConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~ConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(ConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~ConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Conflict Type (Lateral)") {
    
    d <- subset(table.LateralConflictType, Sector %in% list.sectors & Runway %in% runway)
    d$LateralConflictType <- paste(d$Scenario, d$LateralConflictType)
    
    g <- d %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~LateralConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~LateralConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(LateralConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~LateralConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Conflict Type (Vertical)") {
    
    d <- subset(table.VerticalConflictType, Sector %in% list.sectors & Runway %in% runway)
    d$VerticalConflictType <- paste(d$Scenario, d$VerticalConflictType)
    
    g <- d %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalConflictType, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalConflictType, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(VerticalConflictType) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalConflictType, colors=palette.PBN, type="bar")
    
  } else if (group == "Flight Phase") {
    
    d <- subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors & Runway %in% runway)
    d$FlightPlanPhases <- paste(d$Scenario, d$FlightPlanPhases)
    
    g <- d %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=c(palette.current,palette.PBN), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=palette.current, type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(FlightPlanPhases) %>%
      plot_ly(x=~Sector, y=~Count, color=~FlightPlanPhases, colors=palette.PBN, type="bar")
    
  } else if (group == "Severity") {
    
    d <- subset(table.Severity, Sector %in% list.sectors & Runway %in% runway)
    d$Severity <- paste(d$Scenario, d$Severity)
    
    g <- d %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~Count, color=~Severity, colors=c(brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)],brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)]), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~Count, color=~Severity, colors=brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)], type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(Severity) %>%
      plot_ly(x=~Sector, y=~Count, color=~Severity, colors=brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)], type="bar")
    
  } else if (group == "Severity (Vertical)") {
    
    d <- subset(table.VerticalSeverity, Sector %in% list.sectors & Runway %in% runway)
    d$VerticalSeverity <- paste(d$Scenario, d$VerticalSeverity)
    
    g <- d %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalSeverity, colors=c(brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)],brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)]), type="bar")
    
    g1 <- subset(d, Scenario %in% "Current") %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalSeverity, colors=brewer.pal(7, "YlOrRd")[c(2,3,4,5,6,7)], type="bar")
    
    g2 <- subset(d, Scenario %in% "PBN") %>% group_by(VerticalSeverity) %>%
      plot_ly(x=~Sector, y=~Count, color=~VerticalSeverity, colors=brewer.pal(7, "PuBuGn")[c(2,3,4,5,6,7)], type="bar")
    
  }
  
  g <- g %>% layout(hovermode="compare", dragmode="zoom", barmode="group", title=paste("Conflicts per Sector for Runway",runway,"by",group), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto", fixedrange=T),
                    yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  g1 <- g1 %>% layout(hovermode="compare", dragmode="zoom", barmode="stack", title=paste("Current Conflicts per Sector for Runway",runway,"by",group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  g2 <- g2 %>% layout(hovermode="compare", dragmode="zoom", barmode="stack", title=paste("PBN Conflicts per Sector for Runway",runway,"by",group), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Number of conflicts")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Conflicts per Sector for Runway",runway,"by",group)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Conflicts per Sector for Runway",runway,"by",group)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlySectorEntry <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical"){
  d <- subset(table.SectorEntry, Runway %in% runway)
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors | Sector %in% "All TMA")
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Rolling Hourly Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Rolling Hourly Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Entries, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="lines") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("PBN Rolling Hourly Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Rolling Hourly Sector Entries for Sector",sector,"Runway",runway)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Rolling Hourly Sector Entries for Sector",sector,"Runway",runway)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlySectorOccupancy <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical"){
  d <- subset(table.SectorOccupancy, Runway %in% runway)
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors | Sector %in% "All TMA")
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Sector Occupancy Count per Minute for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)

  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Sector Occupancy Count per Minute for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~Count, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="line") %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("PBN Sector Occupancy Count per Minute for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Number of aircraft")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Sector Occupancy Count per Minute for Sector",sector,"Runway",runway)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Sector Occupancy Count per Minute for Sector",sector,"Runway",runway)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyControllerWorkload <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical"){
  d <- subset(table.Workload, Runway %in% runway)
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors)
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentHourlyWorkload, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    add_lines(y=~Entries, name=~paste(Scenario,Sector,"Entries"), line=list(color=c(palette.current,palette.PBN), width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Rolling Hourly Percentage Radar Controller Workload for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="line") %>%
    add_lines(y=~Entries, name=~paste(Scenario,Sector,"Entries"), line=list(color=palette.current, width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Rolling Hourly Percentage Radar Controller Workload for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="line") %>%
    add_lines(y=~Entries, name=~paste(Scenario,Sector,"Entries"), line=list(color=palette.PBN, width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("PBN Rolling Hourly Percentage Radar Controller Workload for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Rolling Hourly Percentage Radar Controller Workload for Sector",sector,"Runway",runway)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("Rolling Hourly Percentage Radar Controller Workload for Sector",sector,"Runway",runway)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}

plotlyWorkloadEntries <- function(operation=c("Current","PBN"), runway="1", sector="All", arrange="Vertical"){
  d <- subset(table.Workload, Runway %in% runway)
  if (sector == "All") {
    d <- subset(d, Sector %in% list.sectors)
    palette.current <- brewer.pal(n = 10, name = "Spectral")
    palette.PBN <- rainbow(15)[1:10]
  } else {
    d <- subset(d, Sector %in% sector)
    palette.current <- "blue"
    palette.PBN <- "red"
  }
  
  g <- d %>% group_by(Sector) %>%
    plot_ly(x=~Entries, y=~PercentHourlyWorkload,# text=~paste(Scenario,Sector,"<br>Predicted max capacity:",PredictedCapacity), name=~Sector,
            color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d$Scenario,d$Sector)))) {
    g <- g %>% add_lines(data=subset(d, paste(Scenario,Sector) %in% unique(paste(d$Scenario,d$Sector))[i]),
                         y=~fitted(lm(PercentHourlyWorkload~Entries)), colors=c(palette.current,palette.PBN)[i])
  }
  g <- g %>% layout(hovermode="closest", dragmode="zoom", title=paste("(Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
                    yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  d1 <- subset(d,Scenario %in% "Current")
  g1 <- d1 %>% group_by(Sector) %>%
    plot_ly(x=~Entries, y=~PercentHourlyWorkload,# text=~paste(Scenario,Sector,"<br>Predicted max capacity:",PredictedCapacity), name=~Sector,
            color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d1$Scenario,d1$Sector)))) {
    g1 <- g1 %>% add_lines(data=subset(d1, paste(Scenario,Sector) %in% unique(paste(d1$Scenario,d1$Sector))[i]),
                           y=~fitted(lm(PercentHourlyWorkload~Entries)), colors=c(palette.current)[i])
  }
  g1 <- g1 %>% layout(hovermode="closest", dragmode="zoom", title=paste("Current (Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  d2 <- subset(d,Scenario %in% "PBN")
  g2 <- d2 %>% group_by(Sector) %>%
    plot_ly(x=~Entries, y=~PercentHourlyWorkload,# text=~paste(Scenario,Sector,"<br>Predicted max capacity:",PredictedCapacity), name=~Sector,
            color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d2$Scenario,d2$Sector)))) {
    g2 <- g2 %>% add_lines(data=subset(d2, paste(Scenario,Sector) %in% unique(paste(d2$Scenario,d2$Sector))[i]),
                           y=~fitted(lm(PercentHourlyWorkload~Entries)), colors=c(palette.PBN)[i])
  }
  g2 <- g2 %>% layout(hovermode="closest", dragmode="zoom", title=paste("PBN (Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  if ("Current" %in% operation & "PBN" %in% operation) {
    if (arrange == "Horizontal") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("(Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for Sector",sector,"Runway",runway)))
    } else if (arrange == "Vertical") {
      return(subplot(g1,g2,shareY=T,shareX=T,nrows=2) %>% layout(title=paste("(Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for Sector",sector,"Runway",runway)))
    } else if (arrange == "Group") {
      return(g)
    }
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}
