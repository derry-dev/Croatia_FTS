library(RPostgreSQL)
library(plyr)
library(tidyr)

# Change these lists if you want to include other airports/sectors
list.airports <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE)
list.sectors <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

# Change these to match schema names (base name if multiple runs) for each scenario (assuming rest of query remain the same)
schema.current1 <- "Croatia"
schema.current2 <- "Croatia"
schema.PBN1 <- "Croatia_PBN"
schema.PBN2 <- "Croatia_PBN"

con <- dbConnect(dbDriver("PostgreSQL"),dbname="airtopdb",host="192.168.1.157",port=5432,user="think",password="think")

# Total Throughput --------------------------------------------------------
query.TotalThroughputs <-
  'SELECT
COUNT("Callsign") AS "Count", 
"New DepartureOrArrivalAirport" AS "Airport",
"AircraftState" AS "Category"
FROM "ENTER SCHEMA NAME"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
WHERE "Time_day" = 2
GROUP BY "New DepartureOrArrivalAirport", "AircraftState"
ORDER BY "New DepartureOrArrivalAirport", "AircraftState"'

table.current1.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.TotalThroughputs))
table.current2.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.TotalThroughputs))
table.PBN1.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.TotalThroughputs))
table.PBN2.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.TotalThroughputs))

t1 <- table.current1.TotalThroughputs; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.TotalThroughputs; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.TotalThroughputs; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.TotalThroughputs; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.TotalThroughputs <- rbind(t1,t2,t3,t4)

table.TotalThroughputs <- table.TotalThroughputs[!(table.TotalThroughputs$Category == "Climbing"),]
agg1 <- aggregate(data = table.TotalThroughputs,Count~Category+Scenario+Runway,"sum")
agg1$Airport <- "All Airports"
agg2 <- aggregate(data = subset(table.TotalThroughputs, Airport %in% list.airports),Count~Category+Scenario+Runway,"sum")
agg2$Airport <- "All Major Airports"
agg3 <- aggregate(data = table.TotalThroughputs,Count~Airport+Scenario+Runway,"sum")
agg3$Category <- "Arrival and Departures"
table.TotalThroughputs <- rbind(table.TotalThroughputs, agg1, agg2, agg3)
table.TotalThroughputs$Category[table.TotalThroughputs$Category == "Descending"] <- "Arrivals"
table.TotalThroughputs$Category[table.TotalThroughputs$Category == "GroundAccelerating"] <- "Departures"

write.csv(table.TotalThroughputs, file="~/Croatia v3.2/data/TotalThroughputs.csv", row.names=F)

# Hourly Throughput -------------------------------------------------------
query.HourlyThroughputs <-
  'SELECT
EXTRACT(HOUR FROM "Time_time") AS "Hour",
COUNT("Callsign") AS "Count", 
"New DepartureOrArrivalAirport" AS "Airport",
"AircraftState" AS "Category"
FROM "ENTER SCHEMA NAME"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
WHERE "Time_day" = 2
GROUP BY EXTRACT(HOUR FROM "Time_time"),"New DepartureOrArrivalAirport", "AircraftState"
ORDER BY "New DepartureOrArrivalAirport", "AircraftState",EXTRACT(HOUR FROM "Time_time")'

table.current1.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.HourlyThroughputs))
table.current2.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.HourlyThroughputs))
table.PBN1.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.HourlyThroughputs))
table.PBN2.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.HourlyThroughputs))

t1 <- table.current1.HourlyThroughputs; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.HourlyThroughputs; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.HourlyThroughputs; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.HourlyThroughputs; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.HourlyThroughputs <- rbind(t1,t2,t3,t4)

table.HourlyThroughputs <- table.HourlyThroughputs[!(table.HourlyThroughputs$Category == "Climbing"),]
agg1 <- aggregate(data = table.HourlyThroughputs,Count~Category+Scenario+Runway+Hour,"sum")
agg1$Airport <- "All Airports"
agg2 <- aggregate(data = subset(table.HourlyThroughputs, Airport %in% list.airports),Count~Category+Scenario+Runway+Hour,"sum")
agg2$Airport <- "All Major Airports"
agg3 <- aggregate(data = table.HourlyThroughputs,Count~Airport+Scenario+Runway+Hour,"sum")
agg3$Category <- "Arrivals and Departures"
table.HourlyThroughputs <- rbind(table.HourlyThroughputs, agg1, agg2, agg3)
table.HourlyThroughputs$Category[table.HourlyThroughputs$Category == "Descending"] <- "Arrivals"
table.HourlyThroughputs$Category[table.HourlyThroughputs$Category == "GroundAccelerating"] <- "Departures"

write.csv(table.HourlyThroughputs, file="~/Croatia v3.2/data/HourlyThroughputs.csv", row.names=F)

# Rolling Hourly Throughput -----------------------------------------------
query.RollingHourlyThroughputs <-
  'SELECT
to_char("Time_time", \'HH24:MI\') AS "Time",
"AirportStatus" AS "Airport",
"LiftOffCountInPeriod" AS "RollingLiftOffCount",
"TouchDownCountInPeriod" AS "RollingTouchDownCount",
"ThroughputCountInPeriod" AS "RollingThroughputCount"
FROM "ENTER SCHEMA NAME"."RS_RWYTHROUGHPUT"
WHERE "Time_day" = 2
ORDER BY "AirportStatus", "Time_time"'

table.current1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.RollingHourlyThroughputs))
table.current2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.RollingHourlyThroughputs))
table.PBN1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.RollingHourlyThroughputs))
table.PBN2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.RollingHourlyThroughputs))

t1 <- table.current1.RollingHourlyThroughputs; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.RollingHourlyThroughputs; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.RollingHourlyThroughputs; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.RollingHourlyThroughputs; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.RollingHourlyThroughputs <- rbind(t1,t2,t3,t4)

colnames(table.RollingHourlyThroughputs) <- c("Time","Airport","Departures","Arrivals","Arrivals and Departures","Scenario","Runway")
table.RollingHourlyThroughputs <- gather(table.RollingHourlyThroughputs,"Category","Count",c(Departures,Arrivals,"Arrivals and Departures"))
agg1 <- aggregate(data = table.RollingHourlyThroughputs, Count~Time+Category+Scenario+Runway, "sum")
agg1$Airport <- "All Airports"
agg2 <- aggregate(data = subset(table.RollingHourlyThroughputs, Airport %in% list.airports), Count~Time+Category+Scenario+Runway, "sum")
agg2$Airport <- "All Major Airports"
table.RollingHourlyThroughputs <- rbind(table.RollingHourlyThroughputs, agg1, agg2)
table.RollingHourlyThroughputs <- table.RollingHourlyThroughputs[,c(1,6,2,5,3,4)]

write.csv(table.RollingHourlyThroughputs, file="~/Croatia v3.2/data/RollingHourlyThroughputs.csv", row.names=F)

# Fuel Burn Track Miles ---------------------------------------------------
query.FuelBurnTrackMiles <-
  'SELECT
"Time_time" AS "Time",
"Callsign",
"OriginInfo" AS "Origin",
"DestinationInfo" AS "Destination",
ROUND("TotalFuelBurned"::numeric, 5) AS "FuelBurn",
ROUND("TotalDistanceFlown"::numeric/1852, 5) AS "TrackMiles",
"AircraftType",
"FlightType",
"Routing",
CASE
WHEN "OriginInfo" LIKE \'LD%\' AND "DestinationInfo" NOT LIKE \'LD%\' THEN \'Departure\'
WHEN "OriginInfo" NOT LIKE \'LD%\' AND "DestinationInfo"  LIKE \'LD%\' THEN \'Arrival\'
WHEN "OriginInfo" LIKE \'LD%\' AND "DestinationInfo" LIKE \'LD%\' THEN \'Domestic\'
WHEN "OriginInfo" NOT LIKE \'LD%\' AND "DestinationInfo" NOT LIKE \'LD%\' THEN \'Overflight\'
END AS "RoutingType"
FROM "ENTER SCHEMA NAME"."RS.FB_TM"
WHERE "Time_day" = 2'

table.current1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.FuelBurnTrackMiles))
table.current2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.FuelBurnTrackMiles))
table.PBN1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.FuelBurnTrackMiles))
table.PBN2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.FuelBurnTrackMiles))

t1 <- table.current1.FuelBurnTrackMiles; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.FuelBurnTrackMiles; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.FuelBurnTrackMiles; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.FuelBurnTrackMiles; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.FuelBurnTrackMiles <- rbind(t1,t2,t3,t4)

table.FuelBurnTrackMiles <- table.FuelBurnTrackMiles %>%
  mutate(Waypoint = ifelse(RoutingType == 'Departure', sub(".*_", "", Routing),
                           ifelse(RoutingType == 'Arrival',  sub("_.*","",gsub('FUT.','',Routing)),'')))
table.FuelBurnTrackMiles$FlightType[table.FuelBurnTrackMiles$FlightType == "AirCarrier"] <- "IFR"
table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, FlightType %in% "IFR" ,select=-c(Time,FlightType,Routing))
temp <- table.FuelBurnTrackMiles[0,]
for (i in 1:nrow(table.FuelBurnTrackMiles)) {
  if (table.FuelBurnTrackMiles[i,]$RoutingType == "Domestic" & is.na(table.FuelBurnTrackMiles[i,]$RoutingType) == F) {
    t1 <- table.FuelBurnTrackMiles[i,]
    t1$RoutingType <- "Arrival"
    t1$Waypoint <- t1$Origin
    t2 <- table.FuelBurnTrackMiles[i,]
    t2$RoutingType <- "Departure"
    t2$Waypoint <- t2$Destination
    temp <- rbind(temp,t1,t2)
  }
}
table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, !(RoutingType %in% c("Domestic","Overflight")))
table.FuelBurnTrackMiles <- rbind(table.FuelBurnTrackMiles,temp)
table.FuelBurnTrackMiles$Airport <- ifelse(table.FuelBurnTrackMiles$RoutingType == "Departure",table.FuelBurnTrackMiles$Origin,NA)
table.FuelBurnTrackMiles$Airport <- ifelse(table.FuelBurnTrackMiles$RoutingType == "Arrival",table.FuelBurnTrackMiles$Destination,table.FuelBurnTrackMiles$Airport)
table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, substr(Airport,1,4) %in% list.airports)

table.FuelBurn <- aggregate(data=table.FuelBurnTrackMiles,FuelBurn~Airport+Scenario+Runway+RoutingType+Waypoint,"sum")
temp1 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Runway+Waypoint,"sum")
temp1$RoutingType <- "Arrivals and Departures"
temp2 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Runway+RoutingType,"sum")
temp2$Waypoint <- "All Routes"
temp3 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Runway,"sum")
temp3$RoutingType <- "Arrivals and Departures"
temp3$Waypoint <- "All Routes"
temp4 <- aggregate(data = table.FuelBurn, FuelBurn~Scenario+Runway,"sum")
temp4$RoutingType <- "Arrivals and Departures"
temp4$Waypoint <- "All Routes"
temp4$Airport <- "All Airports"
table.FuelBurn <- rbind(table.FuelBurn,temp1,temp2,temp3,temp4)
table.FuelBurn <- table.FuelBurn[,c(1,6,4,5,2,3)]

table.TrackMiles <- aggregate(data=table.FuelBurnTrackMiles,TrackMiles~Airport+Scenario+Runway+RoutingType+Waypoint,"sum")
temp1 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+Runway+Waypoint,"sum")
temp1$RoutingType <- "Arrivals and Departures"
temp2 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+Runway+RoutingType,"sum")
temp2$Waypoint <- "All Routes"
temp3 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Runway+Scenario,"sum")
temp3$RoutingType <- "Arrivals and Departures"
temp3$Waypoint <- "All Routes"
temp4 <- aggregate(data = table.TrackMiles, TrackMiles~Scenario+Runway,"sum")
temp4$RoutingType <- "Arrivals and Departures"
temp4$Waypoint <- "All Routes"
temp4$Airport <- "All Airports"
table.TrackMiles <- rbind(table.TrackMiles,temp1,temp2,temp3,temp4)
table.TrackMiles <- table.TrackMiles[,c(1,6,4,5,2,3)]

write.csv(table.FuelBurnTrackMiles, file="~/Croatia v3.2/data/FuelBurnTrackMiles.csv", row.names=F)
write.csv(table.FuelBurn, file="~/Croatia v3.2/data/FuelBurn.csv", row.names=F)
write.csv(table.TrackMiles, file="~/Croatia v3.2/data/TrackMiles.csv", row.names=F)

# Conflict ----------------------------------------------------------------
query.Conflicts <-
  'SELECT 
"ID",
"ATCSector" AS "Sector",
"StartTime_time" AS "Start_Time",
"ClosestApproachTime_time" AS "Closest_Time",
"EndTime_time" AS "End_Time",
"ConflictType",
"LateralConflictType",
"VerticalConflictType",
"Severity"::text,
"VerticalSeverity"::text,
ROUND("VerticalSeparation_m"::numeric/0.3048, 5) AS "VerticalSeparation",
ROUND("ReqVerticalSeparation_m"::numeric/0.3048, 5) AS "ReqVerticalSeparation",
ROUND("LateralSeparation_m"::numeric/1852, 5) AS "LateralSeparation",
ROUND("ReqLateralSeparation_m"::numeric/1852, 5) AS "ReqLateralSeparation",
"Altitude_ft",
"FlightPlanPhase1",
"FlightPlanPhase2",
t1."FlightType" AS "FlightType1",
t2."FlightType" AS "FlightType2",
t1."Routing" AS "Routing1",
t2."Routing" AS "Routing2",
SPLIT_PART("2DLocation",\' \',6)::numeric + SPLIT_PART("2DLocation",\' \',7)::numeric/60 + SPLIT_PART("2DLocation",\' \',8)::numeric/3600 AS "Longitude",
SPLIT_PART("2DLocation",\' \',2)::numeric + SPLIT_PART("2DLocation",\' \',3)::numeric/60 + SPLIT_PART("2DLocation",\' \',4)::numeric/3600 AS "Latitude"
FROM "ENTER SCHEMA NAME"."Conflict"
LEFT JOIN LATERAL (
SELECT "Callsign", "FlightType", "Routing"
FROM "ENTER SCHEMA NAME"."RS.FB_TM"
WHERE "Time_day" = 2
AND REPLACE("Callsign",\'F_\',\'\') = SPLIT_PART(REPLACE(REPLACE(REPLACE("ID",\'_F_\',\'_\'),\'_2_\',\'_\'),\'_1_\',\'\'),\'_\',2)
) AS t1 ON TRUE
LEFT JOIN LATERAL (
SELECT "Callsign", "FlightType", "Routing"
FROM "ENTER SCHEMA NAME"."RS.FB_TM"
WHERE "Time_day" = 2
AND REPLACE("Callsign",\'F_\',\'\') = SPLIT_PART(REPLACE(REPLACE(REPLACE("ID",\'_F_\',\'_\'),\'_2_\',\'_\'),\'_1_\',\'\'),\'_\',3)
) AS t2 ON TRUE
WHERE "StartTime_day" = 2'

table.current1.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.Conflicts))
table.current2.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.Conflicts))
table.PBN1.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.Conflicts))
table.PBN2.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.Conflicts))

t1 <- table.current1.Conflicts; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.Conflicts; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.Conflicts; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.Conflicts; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.Conflicts <- rbind(t1,t2,t3,t4)

for(i in 1:ncol(table.Conflicts)) table.Conflicts[[i]][is.na(table.Conflicts[[i]])] = "NULL"

temp1 <- as.data.frame(gsub("Approach","App",t(apply(table.Conflicts[,c(16,17)], 1, sort))))
for (i in 1:nrow(temp1)) {
  if (temp1[i,1] == temp1[i,2]) {
    temp1[i,3] <- paste("Both",temp1[i,1])
  } else {
    temp1[i,3] <- paste(temp1[i,1],"&",temp1[i,2],sep="")
  }
}
temp2 <- as.data.frame(gsub("AirCarrier","IFR",gsub("GeneralAviation","VFR",t(apply(table.Conflicts[,c(18,19)], 1, sort)))))
for (i in 1:nrow(temp2)) {
  if (temp2[i,1] == temp2[i,2]) {
    temp2[i,3] <- paste("Both",temp2[i,1])
  } else {
    temp2[i,3] <- paste(temp2[i,1],"&",temp2[i,2])
  }
}
temp3 <- as.data.frame(t(apply(table.Conflicts[,c(20,21)], 1, sort)))
for (i in 1:nrow(temp3)) {
  if (temp3[i,1] == temp3[i,2]) {
    temp3[i,3] <- paste("Both",temp3[i,1])
  } else {
    temp3[i,3] <- paste(temp3[i,1],temp3[i,2])
  }
}
table.Conflicts <- cbind(table.Conflicts, temp1[,3], temp2[,3], temp3[,3])
names(table.Conflicts)[c(26,27,28)] <- c("FlightPlanPhases","FlightTypes","Routings")
table.Conflicts <- table.Conflicts[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,26,18,19,27,20,21,28,22,23,24,25)]

table.ConflictType <- count(table.Conflicts, vars = c("Sector","ConflictType","Scenario","Runway"))
temp1 <- aggregate(data = table.ConflictType,freq~ConflictType+Scenario+Runway,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.ConflictType, Sector %in% list.sectors),freq~ConflictType+Scenario+Runway,"sum")
temp2$Sector <- "All TMA"
table.ConflictType <- rbind(table.ConflictType, temp1, temp2)
table.ConflictType$ConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.ConflictType$ConflictType)
names(table.ConflictType) <- c("Sector","ConflictType","Scenario","Runway","Count")
table.ConflictType <- table.ConflictType[,c(1,5,2,3,4)]

table.LateralConflictType <- count(table.Conflicts, vars = c("Sector","LateralConflictType","Scenario","Runway"))
temp1 <- aggregate(data = table.LateralConflictType,freq~LateralConflictType+Scenario+Runway,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.LateralConflictType, Sector %in% list.sectors),freq~LateralConflictType+Scenario+Runway,"sum")
temp2$Sector <- "All TMA"
table.LateralConflictType <- rbind(table.LateralConflictType, temp1, temp2)
table.LateralConflictType$LateralConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.LateralConflictType$LateralConflictType)
names(table.LateralConflictType) <- c("Sector","LateralConflictType","Scenario","Runway","Count")
table.LateralConflictType <- table.LateralConflictType[,c(1,5,2,3,4)]

table.VerticalConflictType <- count(table.Conflicts, vars = c("Sector","VerticalConflictType","Scenario","Runway"))
temp1 <- aggregate(data = table.VerticalConflictType,freq~VerticalConflictType+Scenario+Runway,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.VerticalConflictType, Sector %in% list.sectors),freq~VerticalConflictType+Scenario+Runway,"sum")
temp2$Sector <- "All TMA"
table.VerticalConflictType <- rbind(table.VerticalConflictType, temp1, temp2)
table.VerticalConflictType$VerticalConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.VerticalConflictType$VerticalConflictType)
names(table.VerticalConflictType) <- c("Sector","VerticalConflictType","Scenario","Runway","Count")
table.VerticalConflictType <- table.VerticalConflictType[,c(1,5,2,3,4)]

table.Severity <- count(table.Conflicts, vars = c("Sector","Severity","Scenario","Runway"))
temp1 <- aggregate(data = table.Severity,freq~Severity+Scenario+Runway,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.Severity, Sector %in% list.sectors),freq~Severity+Scenario+Runway,"sum")
temp2$Sector <- "All TMA"
table.Severity <- rbind(table.Severity, temp1, temp2)
names(table.Severity) <- c("Sector","Severity","Scenario","Runway","Count")
table.Severity <- table.Severity[,c(1,5,2,3,4)]

table.VerticalSeverity <- count(table.Conflicts, vars = c("Sector","VerticalSeverity","Scenario","Runway"))
temp1 <- aggregate(data = table.VerticalSeverity,freq~VerticalSeverity+Scenario+Runway,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.VerticalSeverity, Sector %in% list.sectors),freq~VerticalSeverity+Scenario+Runway,"sum")
temp2$Sector <- "All TMA"
table.VerticalSeverity <- rbind(table.VerticalSeverity, temp1, temp2)
names(table.VerticalSeverity) <- c("Sector","VerticalSeverity","Scenario","Runway","Count")
table.VerticalSeverity <- table.VerticalSeverity[,c(1,5,2,3,4)]

write.csv(table.Conflicts, file="~/Croatia v3.2/data/Conflicts.csv", row.names=F)
write.csv(table.ConflictType, file="~/Croatia v3.2/data/ConflictType.csv", row.names=F)
write.csv(table.LateralConflictType, file="~/Croatia v3.2/data/LateralConflictType.csv", row.names=F)
write.csv(table.VerticalConflictType, file="~/Croatia v3.2/data/VerticalConflictType.csv", row.names=F)
write.csv(table.Severity, file="~/Croatia v3.2/data/Severity.csv", row.names=F)
write.csv(table.VerticalSeverity, file="~/Croatia v3.2/data/VerticalSeverity.csv", row.names=F)

# Conflict Flight Plan Phase ---------------------------------------------
query.ConflictsFlightPlanPhase <-
  'SELECT
"ATCSector" AS "Sector",
CASE
WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2")
THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2"
ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1"
END AS "FlightPlanPhases",
COUNT(*) AS "Count"
FROM "ENTER SCHEMA NAME"."Conflict"
GROUP BY "ATCSector", (CASE WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2") THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2" ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1" END)'

table.current1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.ConflictsFlightPlanPhase))
table.current2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.ConflictsFlightPlanPhase))
table.PBN1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.ConflictsFlightPlanPhase))
table.PBN2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.ConflictsFlightPlanPhase))

t1 <- table.current1.ConflictsFlightPlanPhase; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.ConflictsFlightPlanPhase; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.ConflictsFlightPlanPhase; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.ConflictsFlightPlanPhase; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.ConflictsFlightPlanPhase <- rbind(t1,t2,t3,t4)

agg1 <- aggregate(data = table.ConflictsFlightPlanPhase,Count~FlightPlanPhases+Scenario+Runway,"sum")
agg1$Sector <- "All Sectors"
agg2 <- aggregate(data = subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors),Count~FlightPlanPhases+Scenario+Runway,"sum")
agg2$Sector <- "All TMA"
table.ConflictsFlightPlanPhase <- rbind(table.ConflictsFlightPlanPhase, agg1, agg2)

temp <- strsplit(table.ConflictsFlightPlanPhase$FlightPlanPhases, " ")
for (i in 1:length(temp)) {
  if (unlist(temp[i])[1] == unlist(temp[i])[2]) {
    temp[i] <- paste("Both",gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp[i])[1]))
  } else {
    temp[i] <- paste(gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp[i])[1]),"and",gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp[i])[2]))
  }
}
table.ConflictsFlightPlanPhase$FlightPlanPhases <- unlist(temp)
table.ConflictsFlightPlanPhase <- table.ConflictsFlightPlanPhase[,c(1,3,2,4,5)]

write.csv(table.ConflictsFlightPlanPhase, file="~/Croatia v3.2/data/ConflictsFlightPlanPhase.csv", row.names=F)

# Sector Occupancy --------------------------------------------------------
query.SectorOccupancy <-
  'SELECT
to_char("Time_time", \'HH24:MI\') AS "Time",
"RadarController" AS "Sector",
"AircraftList" AS "Count"
FROM "ENTER SCHEMA NAME"."RS_SECOCC"
WHERE "Time_day" = 2'

table.current1.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.SectorOccupancy))
table.current2.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.SectorOccupancy))
table.PBN1.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.SectorOccupancy))
table.PBN2.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.SectorOccupancy))

t1 <- table.current1.SectorOccupancy; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.SectorOccupancy; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.SectorOccupancy; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.SectorOccupancy; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.SectorOccupancy <- rbind(t1,t2,t3,t4)

agg1 <- aggregate(data = table.SectorOccupancy,Count~Time+Scenario+Runway,"sum")
agg1$Sector <- "All Sectors"
agg2 <- aggregate(data = subset(table.SectorOccupancy, Sector %in% list.sectors),Count~Time+Scenario+Runway,"sum")
agg2$Sector <- "All TMA"
table.SectorOccupancy <- rbind(table.SectorOccupancy,agg1,agg2)

write.csv(table.SectorOccupancy, file="~/Croatia v3.2/data/SectorOccupancy.csv", row.names=F)

# Sector Entry ------------------------------------------------------------
query.SectorEntry <-
  'SELECT
to_char("Time_time", \'HH24:MI\') AS "Time", 
"RadarController" AS "Sector",
"LastPeriodReceivedAircraftCount" AS "Entries"
FROM "ENTER SCHEMA NAME"."RS_SECENTRY"
WHERE "Time_day" = 2'

table.current1.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.SectorEntry))
table.current2.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.SectorEntry))
table.PBN1.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.SectorEntry))
table.PBN2.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.SectorEntry))

t1 <- table.current1.SectorEntry; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.SectorEntry; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.SectorEntry; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.SectorEntry; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.SectorEntry <- rbind(t1,t2,t3,t4)

agg1 <- aggregate(data = table.SectorEntry,Entries~Time+Scenario+Runway,"sum")
agg1$Sector <- "All Sectors"
agg2 <- aggregate(data = subset(table.SectorEntry, Sector %in% list.sectors),Entries~Time+Scenario+Runway,"sum")
agg2$Sector <- "All TMA"
table.SectorEntry <- rbind(table.SectorEntry,agg1,agg2)

write.csv(table.SectorEntry, file="~/Croatia v3.2/data/SectorEntry.csv", row.names=F)

# Workload ----------------------------------------------------------------
query.Workload <- 
  'SELECT
  "Time",
"Sector",
"RollingHourlyWorkload",
"PercentRollingHourlyWorkload",
"RollingHourlyIFR" + "RollingHourlyVFR" + "RollingHourlyMIL" AS "RollingHourlyEntries",
"RollingHourlyIFR",
"RollingHourlyVFR",
"RollingHourlyMIL"
FROM (
SELECT
"Time",
"Sector",
"Workload",
"IFR" + "VFR" + "MIL" AS "Entries",
"IFR",
"VFR",
"MIL",
SUM("Workload") OVER (PARTITION BY "Sector" ROWS BETWEEN 3599 PRECEDING AND CURRENT ROW) AS "RollingHourlyWorkload",
ROUND((SUM("Workload") OVER (PARTITION BY "Sector" ROWS BETWEEN 3599 PRECEDING AND CURRENT ROW))/36) AS "PercentRollingHourlyWorkload",
SUM("IFR") OVER (PARTITION BY "Sector" ROWS BETWEEN 3599 PRECEDING AND CURRENT ROW) AS "RollingHourlyIFR",
SUM("VFR") OVER (PARTITION BY "Sector" ROWS BETWEEN 3599 PRECEDING AND CURRENT ROW) AS "RollingHourlyVFR",
SUM("MIL") OVER (PARTITION BY "Sector" ROWS BETWEEN 3599 PRECEDING AND CURRENT ROW) AS "RollingHourlyMIL"
FROM (
SELECT
t1."Time",
t1."Sector",
CASE WHEN t3."Workload" IS NULL THEN 0 ELSE t3."Workload" END,
CASE WHEN t2."IFR" IS NULL THEN 0 ELSE t2."IFR" END,
CASE WHEN t2."VFR" IS NULL THEN 0 ELSE t2."VFR" END,
CASE WHEN t2."MIL" IS NULL THEN 0 ELSE t2."MIL" END
FROM (
SELECT "Time", "Sector" FROM time_template
CROSS JOIN (
(SELECT \'TMA_ZAGREB\' AS "Sector" UNION SELECT \'TMA_ZADAR\' UNION SELECT \'TMA_SPLIT\' UNION SELECT \'TMA_PULA\' UNION SELECT \'TMA_OSIJEK\' UNION SELECT \'TMA_DUBROVNIK\')
) AS verysql
ORDER BY "Sector", "Time"
) AS t1
LEFT JOIN (
SELECT
"Time",
"Sector",
SUM("IFR") AS "IFR",
SUM("VFR") AS "VFR",
SUM("MIL") AS "MIL"
FROM (
SELECT
"Time_time" AS "Time",
"New CurrentRadarController" AS "Sector",
CASE WHEN "FlightType" = \'AirCarrier\' THEN 1 ELSE 0 END AS "IFR",
CASE WHEN "FlightType" = \'GeneralAviation\' THEN 1 ELSE 0 END AS "VFR",
CASE WHEN "FlightType" = \'Military\' THEN 1 ELSE 0 END AS "MIL"
FROM "ENTER SCHEMA NAME"."RS_ATC_SECTOR_ENTRY_EXIT_AND_CONTROLLED"
WHERE "New CurrentRadarController" LIKE \'TMA%\' AND "Time_day" = 2
) AS suchsql
GROUP BY "Sector", "Time"
) AS t2 ON t1."Time" = t2."Time" AND t1."Sector" = t2."Sector"
LEFT JOIN (
SELECT "TimeAdjusted", "Sector", SUM("Task Duration") AS "Workload"
FROM "ENTER SCHEMA NAME"."Workload"
GROUP BY "Sector", "TimeAdjusted"
) AS t3 ON t1."Time" = t3."TimeAdjusted" AND t1."Sector" = t3."Sector"
) AS extremelysql
) AS definitelysql
WHERE EXTRACT(second from "Time") = 0
AND EXTRACT(minute from "Time") IN (0,10,20,30,40,50)'

table.current1.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current1,query.Workload))
table.current2.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current2,query.Workload))
table.PBN1.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN1,query.Workload))
table.PBN2.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN2,query.Workload))

t1 <- table.current1.Workload; t1$Scenario <- "Current"; t1$Runway <- "1"
t2 <- table.current2.Workload; t2$Scenario <- "Current"; t2$Runway <- "2"
t3 <- table.PBN1.Workload; t3$Scenario <- "PBN"; t3$Runway <- "1"
t4 <- table.PBN2.Workload; t4$Scenario <- "PBN"; t4$Runway <- "2"
table.Workload <- rbind(t1,t2,t3,t4)

write.csv(table.Workload, file="~/Croatia v3.2/data/Workload.csv", row.names=F)

# Sector Polygons ---------------------------------------------------------
table.SectorPolygons <-
  dbGetQuery(con,'
             SELECT
             "Sector",
             --ST_SetSRID(ST_UNION("Polygon"::geometry), 4326) AS "Geometry",
             REPLACE(REPLACE(ST_AsText(ST_UNION("Polygon"::geometry)),\'POLYGON((\',\'\'),\'))\',\'\') AS "SectorPolygon"
             FROM (
             SELECT
             t1."Sector",
             t1."Block",
             t2."PolygonID",
             t3."Polygon"
             FROM (
             SELECT x."ID" AS "Sector",
             y."SectorBlockList" AS "Block"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',1) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',2) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',3) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',4) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',5) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',6) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',7) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',8) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',9) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',10) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',11) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',12) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',13) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',14) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',15) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',16) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',17) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',18) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',19) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',20) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',21) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',22) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',23) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',24) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',25) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',26) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',27) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',28) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',29) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',30) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',31) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',32) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',33) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',34) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',35) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',36) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',37) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',38) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',39) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',40) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',41) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',42) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',43) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',44) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',45) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',46) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',47) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',48) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',49) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',50) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',51) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',52) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',53) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',54) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',55) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',56) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',57) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',58) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',59) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',60) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',61) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',62) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',63) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',64) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',65) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',66) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',67) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',68) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',69) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             UNION
             SELECT x."ID",
             y."SectorBlockList"
             FROM "Croatia"."ATCSector" AS x
             LEFT JOIN LATERAL (
             SELECT "ID", SPLIT_PART(REPLACE("SectorBlockList",\',In,\',\'\'),\',\',70) AS "SectorBlockList"
             FROM "Croatia"."ATCSector"
             WHERE x."ID" = "ID"
             ) AS y ON TRUE
             ) AS t1
             JOIN (
             SELECT "ID" AS "Block", "Polygon" AS "PolygonID"
             FROM "Croatia"."SectorBlock"
             ) AS t2 ON t1."Block" = t2."Block"
             JOIN (
             SELECT "PolygonID", "Polygon"
             FROM "Croatia"."Polygon2"
             ) AS t3 ON t3."PolygonID" = t2."PolygonID"
             WHERE t1."Block" NOT LIKE \'\'
             ORDER BY "Sector"
             ) AS t1
             GROUP BY "Sector"')

table.SectorPolygons <- separate(data=table.SectorPolygons, col=SectorPolygon, into=paste("V", 1:120, sep=""), sep=",")
table.SectorPolygons <- subset(gather(data=table.SectorPolygons, "V", "Position", -c(Sector)),is.na(Position)==FALSE)
table.SectorPolygons <- separate(data=table.SectorPolygons, col=Position, into=c("Longitude","Latitude"), sep=" ")
table.SectorPolygons$Longitude <- as.numeric(table.SectorPolygons$Longitude); table.SectorPolygons$Latitude <- as.numeric(table.SectorPolygons$Latitude)  

write.csv(table.SectorPolygons, file="~/Croatia v3.2/data/SectorPolygons.csv", row.names=F)

dbDisconnect(con)