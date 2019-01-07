source("data.R", local = T)

# Change these lists if you want to include other airports/sectors
list.airports <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE)
list.sectors <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

# Functions
'%!in%' <- function(x,y){!('%in%'(x,y))}

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
    plot_ly(x=~Time, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="line") %>%
    add_lines(y=~RollingHourlyEntries, name=~paste(Scenario,Sector,"Entries"), line=list(color=c(palette.current,palette.PBN), width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Rolling Hourly Percentage Radar Controller Workload for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  g1 <- subset(d,Scenario %in% "Current") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="line") %>%
    add_lines(y=~RollingHourlyEntries, name=~paste(Scenario,Sector,"Entries"), line=list(color=palette.current, width=1, dash="dot")) %>%
    layout(hovermode="compare", dragmode="zoom", title=paste("Current Rolling Hourly Percentage Radar Controller Workload for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
           xaxis=list(tickmode="auto", rangeslider=list(type="time")),
           yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  g2 <- subset(d,Scenario %in% "PBN") %>% group_by(Sector) %>%
    plot_ly(x=~Time, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="line") %>%
    add_lines(y=~RollingHourlyEntries, name=~paste(Scenario,Sector,"Entries"), line=list(color=palette.PBN, width=1, dash="dot")) %>%
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
    plot_ly(x=~RollingHourlyEntries, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=c(palette.current,palette.PBN), type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d$Scenario,d$Sector)))) {
    g <- g %>% add_lines(data=subset(d, paste(Scenario,Sector) %in% unique(paste(d$Scenario,d$Sector))[i]),
                         y=~fitted(lm(PercentRollingHourlyWorkload~RollingHourlyEntries)), colors=c(palette.current,palette.PBN)[i])
  }
  g <- g %>% layout(hovermode="closest", dragmode="zoom", title=paste("(Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
                    xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
                    yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  d1 <- subset(d,Scenario %in% "Current")
  g1 <- d1 %>% group_by(Sector) %>%
    plot_ly(x=~RollingHourlyEntries, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.current, type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d1$Scenario,d1$Sector)))) {
    g1 <- g1 %>% add_lines(data=subset(d1, paste(Scenario,Sector) %in% unique(paste(d1$Scenario,d1$Sector))[i]),
                           y=~fitted(lm(PercentRollingHourlyWorkload~RollingHourlyEntries)), colors=c(palette.current)[i])
  }
  g1 <- g1 %>% layout(hovermode="closest", dragmode="zoom", title=paste("Current (Rolling Hourly) Percentage Radar Controller Workload vs Sector Entries for Sector",sector,"Runway",runway), legend=list(x=100, y=0.5),
                      xaxis=list(tickmode="auto", fixedrange=T, title="Sector Entry Count"),
                      yaxis=list(tickmode="auto", fixedrange=T, title="Workload %")) %>% config(collaborate=F)
  
  d2 <- subset(d,Scenario %in% "PBN")
  g2 <- d2 %>% group_by(Sector) %>%
    plot_ly(x=~RollingHourlyEntries, y=~PercentRollingHourlyWorkload, color=~paste(Scenario,Sector), colors=palette.PBN, type="scatter", mode="markers")
  for (i in 1:length(unique(paste(d2$Scenario,d2$Sector)))) {
    g2 <- g2 %>% add_lines(data=subset(d2, paste(Scenario,Sector) %in% unique(paste(d2$Scenario,d2$Sector))[i]),
                           y=~fitted(lm(PercentRollingHourlyWorkload~RollingHourlyEntries)), colors=c(palette.PBN)[i])
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

plotlyCapacityModel <- function(operation=c("Current","PBN"), runway="1", sector="TMA_DUBROVNIK", model="IFR Only"){
  
  t1 <- subset(table.Workload, Scenario %in% "Current" & Runway %in% runway & Sector %in% sector)
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
  
  t2 <- subset(table.Workload, Scenario %in% "PBN" & Runway %in% runway & Sector %in% sector)
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
  
  if (model == "All (Combined)") {
    
    if (sector == "TMA_DUBROVNIK") {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t2)
    } else if (sector == "TMA_OSIJEK") {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries,data=t2)
    } else if (sector == "TMA_PULA") {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t2)
    } else if (sector == "TMA_SPLIT") {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEsq,data=t2)
    } else if (sector == "TMA_ZADAR") {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEexp,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEexp,data=t2)
    } else if (sector == "TMA_ZAGREB") {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEexp,data=t1)
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyEntries+RHEexp,data=t2)
    }
    
    g1 <- t1 %>% plot_ly(x=~RollingHourlyEntries) %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="Current All (Combined)", marker=list(color="blue")) %>%
      add_lines(y=fitted(lm1), name="Current All (Combined)", line=list(color="blue"), fillcolor="rgba(50,50,255,.5)", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=paste("Current Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly Entries"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
    g2 <- t2 %>% plot_ly(x=~RollingHourlyEntries) %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="PBN All (Combined)", marker=list(color="red")) %>%
      add_lines(y=fitted(lm2), name="PBN All (Combined)", line=list(color="red"), fillcolor="rgba(255,50,50,.5)", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=paste("PBN Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly Entries"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
  } else if (model == "All (Separate)") {
    
    if (all(t1$RollingHourlyMIL == 0)) {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t1)
      g1 <- plot_ly(r=c((70-lm1$coefficient[1])/lm1$coefficient[2],(70-lm1$coefficient[1])/lm1$coefficient[3],0),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="Current All (Separate)", marker=list(color="blue"), fillcolor="rgba(50,50,255,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=paste("Current Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    } else {
      lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR+RollingHourlyMIL,data=t1)
      g1 <- plot_ly(r=c((70-lm1$coefficient[1])/lm1$coefficient[2],(70-lm1$coefficient[1])/lm1$coefficient[3],(70-lm1$coefficient[1])/lm1$coefficient[4]),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="Current All (Separate)", marker=list(color="blue"), fillcolor="rgba(50,50,255,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=paste("Current Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    }
    
    if (all(t2$RollingHourlyMIL == 0)) {
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t2)
      g2 <- plot_ly(r=c((70-lm2$coefficient[1])/lm2$coefficient[2],(70-lm2$coefficient[1])/lm2$coefficient[3],0),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="PBN All (Separate)", marker=list(color="red"), fillcolor="rgba(255,50,50,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=paste("PBN Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    } else {
      lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR+RollingHourlyMIL,data=t2)
      g2 <- plot_ly(r=c((70-lm2$coefficient[1])/lm2$coefficient[2],(70-lm2$coefficient[1])/lm2$coefficient[3],(70-lm2$coefficient[1])/lm2$coefficient[4]),
                    theta=c("RollingHourlyIFR","RollingHourlyVFR","RollingHourlyMIL"),
                    name="PBN All (Separate)", marker=list(color="red"), fillcolor="rgba(255,50,50,0.5)", type="scatterpolar",mode="markers", fill = "toself", hoveron="points+fills") %>%
        layout(title=paste("PBN Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5)) %>% config(collaborate=F)
    }
    
  } else if (model == "IFR and VFR") {
    
    lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t1)
    lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR+RollingHourlyVFR,data=t2)
    
    g1 <- plot_ly(x=c(0,(70-lm1$coefficient[1])/lm1$coefficient[2]), y=c((70-lm1$coefficient[1])/lm1$coefficient[3],0),
            name="Current IFR and VFR", marker=list(color="blue"), line=list(color="blue"), fillcolor="rgba(50,50,255,0.5)", type="scatter", mode="markers+lines", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=paste("Current Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly VFR")) %>% config(collaborate=F)
    
    g2 <- plot_ly(x=c(0,(70-lm2$coefficient[1])/lm2$coefficient[2]), y=c((70-lm2$coefficient[1])/lm2$coefficient[3],0),
                 name="PBN IFR and VFR", marker=list(color="red"), line=list(color="red"), fillcolor="rgba(255,50,50,0.5)", type="scatter", mode="markers+lines", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=paste("PBN Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly VFR")) %>% config(collaborate=F)
    
  } else if (model == "IFR Only") {
    
    lm1 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR,data=t1)
    lm2 <- lm(PercentRollingHourlyWorkload~RollingHourlyIFR,data=t2)
    
    g1 <- t1 %>% plot_ly(x=~RollingHourlyIFR) %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="Current All (Combined)", marker=list(color="blue")) %>%
      add_lines(y=fitted(lm1), name="Current All (Combined)", line=list(color="blue"), fillcolor="rgba(50,50,255,.5)", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=paste("Current Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
    g2 <- t2 %>% plot_ly(x=~RollingHourlyIFR) %>%
      add_markers(y=~PercentRollingHourlyWorkload, name="PBN All (Combined)", marker=list(color="red")) %>%
      add_lines(y=fitted(lm2), name="PBN All (Combined)", line=list(color="red"), fillcolor="rgba(255,50,50,.5)", fill="tozeroy", hoveron="points+fills") %>%
      layout(title=paste("PBN Acceptable Rolling Hourly Workload",model,sector,"Runway",runway), legend=list(x=100, y=0.5),
             xaxis=list(title="Rolling Hourly IFR"), yaxis=list(title="Rolling Hourly Workload")) %>% config(collaborate=F)
    
  }
  if ("Current" %in% operation & "PBN" %in% operation) {
    return(subplot(g1,g2,shareY=T,shareX=T,nrows=1) %>% layout(title=paste("Acceptable Rolling Hourly Workload",model,sector,"Runway",runway)))
  } else if ("Current" %in% operation & "PBN" %!in% operation) {
    return(g1)
  } else if ("Current" %!in% operation & "PBN" %in% operation) {
    return(g2)
  }
}
