library(RPostgreSQL)
library(plyr)
library(tidyr)


con <- dbConnect(dbDriver("PostgreSQL"),dbname="airtopdb",host="192.168.1.157",port=5432,user="think",password="think")

# Change these lists if you want to include other airports/sectors --------------------------------------------------------
list.airports <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE)
list.sectors <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)

# Change these to match schema names for each scenario (assuming rest of query remain the same) --------------------------------------------------------
schema.current_RD1_1 <- "Croatia_REF_RD1_1"
schema.current_RD1_2 <- "Croatia_REF_RD1_2"
schema.current_RD1_3 <- "Croatia_REF_RD1_3"
schema.current_RD1_4 <- "Croatia_REF_RD1_4"
schema.current_RD1_5 <- "Croatia_REF_RD1_5"
schema.current_RD2_1 <- "Croatia_REF_RD2_1"
schema.current_RD2_2 <- "Croatia_REF_RD2_2"
schema.current_RD2_3 <- "Croatia_REF_RD2_3"
schema.current_RD2_4 <- "Croatia_REF_RD2_4"
schema.current_RD2_5 <- "Croatia_REF_RD2_5"
schema.PBN_RD1_1 <- "Croatia_PBN_RD1_1"
schema.PBN_RD1_2 <- "Croatia_PBN_RD1_2"
schema.PBN_RD1_3 <- "Croatia_PBN_RD1_3"
schema.PBN_RD1_4 <- "Croatia_PBN_RD1_4"
schema.PBN_RD1_5 <- "Croatia_PBN_RD1_5"
schema.PBN_RD2_1 <- "Croatia_PBN_RD2_1"
schema.PBN_RD2_2 <- "Croatia_PBN_RD2_2"
schema.PBN_RD2_3 <- "Croatia_PBN_RD2_3"
schema.PBN_RD2_4 <- "Croatia_PBN_RD2_4"
schema.PBN_RD2_5 <- "Croatia_PBN_RD2_5"

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

table.current_RD1_1.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.TotalThroughputs))
table.current_RD1_2.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.TotalThroughputs))
table.current_RD1_3.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.TotalThroughputs))
table.current_RD1_4.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.TotalThroughputs))
table.current_RD1_5.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.TotalThroughputs))
table.current_RD2_1.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.TotalThroughputs))
table.current_RD2_2.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.TotalThroughputs))
table.current_RD2_3.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.TotalThroughputs))
table.current_RD2_4.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.TotalThroughputs))
table.current_RD2_5.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.TotalThroughputs))
table.PBN_RD1_1.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.TotalThroughputs))
table.PBN_RD1_2.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.TotalThroughputs))
table.PBN_RD1_3.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.TotalThroughputs))
table.PBN_RD1_4.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.TotalThroughputs))
table.PBN_RD1_5.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.TotalThroughputs))
table.PBN_RD2_1.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.TotalThroughputs))
table.PBN_RD2_2.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.TotalThroughputs))
table.PBN_RD2_3.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.TotalThroughputs))
table.PBN_RD2_4.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.TotalThroughputs))
table.PBN_RD2_5.TotalThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.TotalThroughputs))

table.current_RD1_1.TotalThroughputs$Scenario <- "Current"; table.current_RD1_1.TotalThroughputs$Runway <- "1"; table.current_RD1_1.TotalThroughputs$Run <- "1"
table.current_RD1_2.TotalThroughputs$Scenario <- "Current"; table.current_RD1_2.TotalThroughputs$Runway <- "1"; table.current_RD1_2.TotalThroughputs$Run <- "2"
table.current_RD1_3.TotalThroughputs$Scenario <- "Current"; table.current_RD1_3.TotalThroughputs$Runway <- "1"; table.current_RD1_3.TotalThroughputs$Run <- "3"
table.current_RD1_4.TotalThroughputs$Scenario <- "Current"; table.current_RD1_4.TotalThroughputs$Runway <- "1"; table.current_RD1_4.TotalThroughputs$Run <- "4"
table.current_RD1_5.TotalThroughputs$Scenario <- "Current"; table.current_RD1_5.TotalThroughputs$Runway <- "1"; table.current_RD1_5.TotalThroughputs$Run <- "5"
table.current_RD2_1.TotalThroughputs$Scenario <- "Current"; table.current_RD2_1.TotalThroughputs$Runway <- "2"; table.current_RD2_1.TotalThroughputs$Run <- "1"
table.current_RD2_2.TotalThroughputs$Scenario <- "Current"; table.current_RD2_2.TotalThroughputs$Runway <- "2"; table.current_RD2_2.TotalThroughputs$Run <- "2"
table.current_RD2_3.TotalThroughputs$Scenario <- "Current"; table.current_RD2_3.TotalThroughputs$Runway <- "2"; table.current_RD2_3.TotalThroughputs$Run <- "3"
table.current_RD2_4.TotalThroughputs$Scenario <- "Current"; table.current_RD2_4.TotalThroughputs$Runway <- "2"; table.current_RD2_4.TotalThroughputs$Run <- "4"
table.current_RD2_5.TotalThroughputs$Scenario <- "Current"; table.current_RD2_5.TotalThroughputs$Runway <- "2"; table.current_RD2_5.TotalThroughputs$Run <- "5"
table.PBN_RD1_1.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD1_1.TotalThroughputs$Runway <- "1"; table.PBN_RD1_1.TotalThroughputs$Run <- "1"
table.PBN_RD1_2.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD1_2.TotalThroughputs$Runway <- "1"; table.PBN_RD1_2.TotalThroughputs$Run <- "2"
table.PBN_RD1_3.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD1_3.TotalThroughputs$Runway <- "1"; table.PBN_RD1_3.TotalThroughputs$Run <- "3"
table.PBN_RD1_4.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD1_4.TotalThroughputs$Runway <- "1"; table.PBN_RD1_4.TotalThroughputs$Run <- "4"
table.PBN_RD1_5.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD1_5.TotalThroughputs$Runway <- "1"; table.PBN_RD1_5.TotalThroughputs$Run <- "5"
table.PBN_RD2_1.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD2_1.TotalThroughputs$Runway <- "2"; table.PBN_RD2_1.TotalThroughputs$Run <- "1"
table.PBN_RD2_2.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD2_2.TotalThroughputs$Runway <- "2"; table.PBN_RD2_2.TotalThroughputs$Run <- "2"
table.PBN_RD2_3.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD2_3.TotalThroughputs$Runway <- "2"; table.PBN_RD2_3.TotalThroughputs$Run <- "3"
table.PBN_RD2_4.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD2_4.TotalThroughputs$Runway <- "2"; table.PBN_RD2_4.TotalThroughputs$Run <- "4"
table.PBN_RD2_5.TotalThroughputs$Scenario <- "PBN"; table.PBN_RD2_5.TotalThroughputs$Runway <- "2"; table.PBN_RD2_5.TotalThroughputs$Run <- "5"

table.TotalThroughputs <- rbind(table.current_RD1_1.TotalThroughputs,table.current_RD1_2.TotalThroughputs,table.current_RD1_3.TotalThroughputs,
                                table.current_RD1_4.TotalThroughputs,table.current_RD1_5.TotalThroughputs,
                                table.current_RD2_1.TotalThroughputs,table.current_RD2_2.TotalThroughputs,table.current_RD2_3.TotalThroughputs,
                                table.current_RD2_4.TotalThroughputs,table.current_RD2_5.TotalThroughputs,
                                table.PBN_RD1_1.TotalThroughputs,table.PBN_RD1_2.TotalThroughputs,table.PBN_RD1_3.TotalThroughputs,
                                table.PBN_RD1_4.TotalThroughputs,table.PBN_RD1_5.TotalThroughputs,
                                table.PBN_RD2_1.TotalThroughputs,table.PBN_RD2_2.TotalThroughputs,table.PBN_RD2_3.TotalThroughputs,
                                table.PBN_RD2_4.TotalThroughputs,table.PBN_RD2_5.TotalThroughputs)

table.TotalThroughputs <- table.TotalThroughputs[!(table.TotalThroughputs$Category == "Climbing"),]
agg1 <- aggregate(data = table.TotalThroughputs,Count~Category+Scenario+Runway+Run,"sum")
agg1$Airport <- "All Airports"
agg2 <- aggregate(data = subset(table.TotalThroughputs, Airport %in% list.airports),Count~Category+Scenario+Runway+Run,"sum")
agg2$Airport <- "All Major Airports"
agg3 <- aggregate(data = table.TotalThroughputs,Count~Airport+Scenario+Runway+Run,"sum")
agg3$Category <- "Arrival and Departures"
table.TotalThroughputs <- rbind(table.TotalThroughputs, agg1, agg2, agg3)
table.TotalThroughputs$Category[table.TotalThroughputs$Category == "Descending"] <- "Arrivals"
table.TotalThroughputs$Category[table.TotalThroughputs$Category == "GroundAccelerating"] <- "Departures"

write.csv(table.TotalThroughputs, file="~/Croatia v3.3/data/TotalThroughputs.csv", row.names=F)

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

table.current_RD1_1.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.HourlyThroughputs))
table.current_RD1_2.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.HourlyThroughputs))
table.current_RD1_3.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.HourlyThroughputs))
table.current_RD1_4.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.HourlyThroughputs))
table.current_RD1_5.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.HourlyThroughputs))
table.current_RD2_1.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.HourlyThroughputs))
table.current_RD2_2.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.HourlyThroughputs))
table.current_RD2_3.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.HourlyThroughputs))
table.current_RD2_4.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.HourlyThroughputs))
table.current_RD2_5.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.HourlyThroughputs))
table.PBN_RD1_1.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.HourlyThroughputs))
table.PBN_RD1_2.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.HourlyThroughputs))
table.PBN_RD1_3.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.HourlyThroughputs))
table.PBN_RD1_4.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.HourlyThroughputs))
table.PBN_RD1_5.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.HourlyThroughputs))
table.PBN_RD2_1.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.HourlyThroughputs))
table.PBN_RD2_2.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.HourlyThroughputs))
table.PBN_RD2_3.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.HourlyThroughputs))
table.PBN_RD2_4.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.HourlyThroughputs))
table.PBN_RD2_5.HourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.HourlyThroughputs))

table.current_RD1_1.HourlyThroughputs$Scenario <- "Current"; table.current_RD1_1.HourlyThroughputs$Runway <- "1"; table.current_RD1_1.HourlyThroughputs$Run <- "1"
table.current_RD1_2.HourlyThroughputs$Scenario <- "Current"; table.current_RD1_2.HourlyThroughputs$Runway <- "1"; table.current_RD1_2.HourlyThroughputs$Run <- "2"
table.current_RD1_3.HourlyThroughputs$Scenario <- "Current"; table.current_RD1_3.HourlyThroughputs$Runway <- "1"; table.current_RD1_3.HourlyThroughputs$Run <- "3"
table.current_RD1_4.HourlyThroughputs$Scenario <- "Current"; table.current_RD1_4.HourlyThroughputs$Runway <- "1"; table.current_RD1_4.HourlyThroughputs$Run <- "4"
table.current_RD1_5.HourlyThroughputs$Scenario <- "Current"; table.current_RD1_5.HourlyThroughputs$Runway <- "1"; table.current_RD1_5.HourlyThroughputs$Run <- "5"
table.current_RD2_1.HourlyThroughputs$Scenario <- "Current"; table.current_RD2_1.HourlyThroughputs$Runway <- "2"; table.current_RD2_1.HourlyThroughputs$Run <- "1"
table.current_RD2_2.HourlyThroughputs$Scenario <- "Current"; table.current_RD2_2.HourlyThroughputs$Runway <- "2"; table.current_RD2_2.HourlyThroughputs$Run <- "2"
table.current_RD2_3.HourlyThroughputs$Scenario <- "Current"; table.current_RD2_3.HourlyThroughputs$Runway <- "2"; table.current_RD2_3.HourlyThroughputs$Run <- "3"
table.current_RD2_4.HourlyThroughputs$Scenario <- "Current"; table.current_RD2_4.HourlyThroughputs$Runway <- "2"; table.current_RD2_4.HourlyThroughputs$Run <- "4"
table.current_RD2_5.HourlyThroughputs$Scenario <- "Current"; table.current_RD2_5.HourlyThroughputs$Runway <- "2"; table.current_RD2_5.HourlyThroughputs$Run <- "5"
table.PBN_RD1_1.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_1.HourlyThroughputs$Runway <- "1"; table.PBN_RD1_1.HourlyThroughputs$Run <- "1"
table.PBN_RD1_2.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_2.HourlyThroughputs$Runway <- "1"; table.PBN_RD1_2.HourlyThroughputs$Run <- "2"
table.PBN_RD1_3.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_3.HourlyThroughputs$Runway <- "1"; table.PBN_RD1_3.HourlyThroughputs$Run <- "3"
table.PBN_RD1_4.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_4.HourlyThroughputs$Runway <- "1"; table.PBN_RD1_4.HourlyThroughputs$Run <- "4"
table.PBN_RD1_5.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_5.HourlyThroughputs$Runway <- "1"; table.PBN_RD1_5.HourlyThroughputs$Run <- "5"
table.PBN_RD2_1.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_1.HourlyThroughputs$Runway <- "2"; table.PBN_RD2_1.HourlyThroughputs$Run <- "1"
table.PBN_RD2_2.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_2.HourlyThroughputs$Runway <- "2"; table.PBN_RD2_2.HourlyThroughputs$Run <- "2"
table.PBN_RD2_3.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_3.HourlyThroughputs$Runway <- "2"; table.PBN_RD2_3.HourlyThroughputs$Run <- "3"
table.PBN_RD2_4.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_4.HourlyThroughputs$Runway <- "2"; table.PBN_RD2_4.HourlyThroughputs$Run <- "4"
table.PBN_RD2_5.HourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_5.HourlyThroughputs$Runway <- "2"; table.PBN_RD2_5.HourlyThroughputs$Run <- "5"

table.HourlyThroughputs <- rbind(table.current_RD1_1.HourlyThroughputs,table.current_RD1_2.HourlyThroughputs,table.current_RD1_3.HourlyThroughputs,
                                table.current_RD1_4.HourlyThroughputs,table.current_RD1_5.HourlyThroughputs,
                                table.current_RD2_1.HourlyThroughputs,table.current_RD2_2.HourlyThroughputs,table.current_RD2_3.HourlyThroughputs,
                                table.current_RD2_4.HourlyThroughputs,table.current_RD2_5.HourlyThroughputs,
                                table.PBN_RD1_1.HourlyThroughputs,table.PBN_RD1_2.HourlyThroughputs,table.PBN_RD1_3.HourlyThroughputs,
                                table.PBN_RD1_4.HourlyThroughputs,table.PBN_RD1_5.HourlyThroughputs,
                                table.PBN_RD2_1.HourlyThroughputs,table.PBN_RD2_2.HourlyThroughputs,table.PBN_RD2_3.HourlyThroughputs,
                                table.PBN_RD2_4.HourlyThroughputs,table.PBN_RD2_5.HourlyThroughputs)

table.HourlyThroughputs <- table.HourlyThroughputs[!(table.HourlyThroughputs$Category == "Climbing"),]
agg1 <- aggregate(data = table.HourlyThroughputs,Count~Category+Scenario+Runway+Run+Hour,"sum")
agg1$Airport <- "All Airports"
agg2 <- aggregate(data = subset(table.HourlyThroughputs, Airport %in% list.airports),Count~Category+Scenario+Runway+Run+Hour,"sum")
agg2$Airport <- "All Major Airports"
agg3 <- aggregate(data = table.HourlyThroughputs,Count~Airport+Scenario+Runway+Run+Hour,"sum")
agg3$Category <- "Arrivals and Departures"
table.HourlyThroughputs <- rbind(table.HourlyThroughputs, agg1, agg2, agg3)
table.HourlyThroughputs$Category[table.HourlyThroughputs$Category == "Descending"] <- "Arrivals"
table.HourlyThroughputs$Category[table.HourlyThroughputs$Category == "GroundAccelerating"] <- "Departures"

write.csv(table.HourlyThroughputs, file="~/Croatia v3.3/data/HourlyThroughputs.csv", row.names=F)

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

table.current_RD1_1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.RollingHourlyThroughputs))
table.current_RD1_2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.RollingHourlyThroughputs))
table.current_RD1_3.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.RollingHourlyThroughputs))
table.current_RD1_4.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.RollingHourlyThroughputs))
table.current_RD1_5.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.RollingHourlyThroughputs))
table.current_RD2_1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.RollingHourlyThroughputs))
table.current_RD2_2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.RollingHourlyThroughputs))
table.current_RD2_3.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.RollingHourlyThroughputs))
table.current_RD2_4.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.RollingHourlyThroughputs))
table.current_RD2_5.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.RollingHourlyThroughputs))
table.PBN_RD1_1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.RollingHourlyThroughputs))
table.PBN_RD1_2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.RollingHourlyThroughputs))
table.PBN_RD1_3.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.RollingHourlyThroughputs))
table.PBN_RD1_4.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.RollingHourlyThroughputs))
table.PBN_RD1_5.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.RollingHourlyThroughputs))
table.PBN_RD2_1.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.RollingHourlyThroughputs))
table.PBN_RD2_2.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.RollingHourlyThroughputs))
table.PBN_RD2_3.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.RollingHourlyThroughputs))
table.PBN_RD2_4.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.RollingHourlyThroughputs))
table.PBN_RD2_5.RollingHourlyThroughputs <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.RollingHourlyThroughputs))

table.current_RD1_1.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD1_1.RollingHourlyThroughputs$Runway <- "1"; table.current_RD1_1.RollingHourlyThroughputs$Run <- "1"
table.current_RD1_2.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD1_2.RollingHourlyThroughputs$Runway <- "1"; table.current_RD1_2.RollingHourlyThroughputs$Run <- "2"
table.current_RD1_3.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD1_3.RollingHourlyThroughputs$Runway <- "1"; table.current_RD1_3.RollingHourlyThroughputs$Run <- "3"
table.current_RD1_4.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD1_4.RollingHourlyThroughputs$Runway <- "1"; table.current_RD1_4.RollingHourlyThroughputs$Run <- "4"
table.current_RD1_5.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD1_5.RollingHourlyThroughputs$Runway <- "1"; table.current_RD1_5.RollingHourlyThroughputs$Run <- "5"
table.current_RD2_1.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD2_1.RollingHourlyThroughputs$Runway <- "2"; table.current_RD2_1.RollingHourlyThroughputs$Run <- "1"
table.current_RD2_2.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD2_2.RollingHourlyThroughputs$Runway <- "2"; table.current_RD2_2.RollingHourlyThroughputs$Run <- "2"
table.current_RD2_3.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD2_3.RollingHourlyThroughputs$Runway <- "2"; table.current_RD2_3.RollingHourlyThroughputs$Run <- "3"
table.current_RD2_4.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD2_4.RollingHourlyThroughputs$Runway <- "2"; table.current_RD2_4.RollingHourlyThroughputs$Run <- "4"
table.current_RD2_5.RollingHourlyThroughputs$Scenario <- "Current"; table.current_RD2_5.RollingHourlyThroughputs$Runway <- "2"; table.current_RD2_5.RollingHourlyThroughputs$Run <- "5"
table.PBN_RD1_1.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_1.RollingHourlyThroughputs$Runway <- "1"; table.PBN_RD1_1.RollingHourlyThroughputs$Run <- "1"
table.PBN_RD1_2.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_2.RollingHourlyThroughputs$Runway <- "1"; table.PBN_RD1_2.RollingHourlyThroughputs$Run <- "2"
table.PBN_RD1_3.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_3.RollingHourlyThroughputs$Runway <- "1"; table.PBN_RD1_3.RollingHourlyThroughputs$Run <- "3"
table.PBN_RD1_4.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_4.RollingHourlyThroughputs$Runway <- "1"; table.PBN_RD1_4.RollingHourlyThroughputs$Run <- "4"
table.PBN_RD1_5.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD1_5.RollingHourlyThroughputs$Runway <- "1"; table.PBN_RD1_5.RollingHourlyThroughputs$Run <- "5"
table.PBN_RD2_1.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_1.RollingHourlyThroughputs$Runway <- "2"; table.PBN_RD2_1.RollingHourlyThroughputs$Run <- "1"
table.PBN_RD2_2.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_2.RollingHourlyThroughputs$Runway <- "2"; table.PBN_RD2_2.RollingHourlyThroughputs$Run <- "2"
table.PBN_RD2_3.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_3.RollingHourlyThroughputs$Runway <- "2"; table.PBN_RD2_3.RollingHourlyThroughputs$Run <- "3"
table.PBN_RD2_4.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_4.RollingHourlyThroughputs$Runway <- "2"; table.PBN_RD2_4.RollingHourlyThroughputs$Run <- "4"
table.PBN_RD2_5.RollingHourlyThroughputs$Scenario <- "PBN"; table.PBN_RD2_5.RollingHourlyThroughputs$Runway <- "2"; table.PBN_RD2_5.RollingHourlyThroughputs$Run <- "5"

table.RollingHourlyThroughputs <- rbind(table.current_RD1_1.RollingHourlyThroughputs,table.current_RD1_2.RollingHourlyThroughputs,table.current_RD1_3.RollingHourlyThroughputs,
                                 table.current_RD1_4.RollingHourlyThroughputs,table.current_RD1_5.RollingHourlyThroughputs,
                                 table.current_RD2_1.RollingHourlyThroughputs,table.current_RD2_2.RollingHourlyThroughputs,table.current_RD2_3.RollingHourlyThroughputs,
                                 table.current_RD2_4.RollingHourlyThroughputs,table.current_RD2_5.RollingHourlyThroughputs,
                                 table.PBN_RD1_1.RollingHourlyThroughputs,table.PBN_RD1_2.RollingHourlyThroughputs,table.PBN_RD1_3.RollingHourlyThroughputs,
                                 table.PBN_RD1_4.RollingHourlyThroughputs,table.PBN_RD1_5.RollingHourlyThroughputs,
                                 table.PBN_RD2_1.RollingHourlyThroughputs,table.PBN_RD2_2.RollingHourlyThroughputs,table.PBN_RD2_3.RollingHourlyThroughputs,
                                 table.PBN_RD2_4.RollingHourlyThroughputs,table.PBN_RD2_5.RollingHourlyThroughputs)

colnames(table.RollingHourlyThroughputs) <- c("Time","Airport","Departures","Arrivals","Arrivals and Departures","Scenario","Runway","Run")
table.RollingHourlyThroughputs <- gather(table.RollingHourlyThroughputs,"Category","Count",c(Departures,Arrivals,"Arrivals and Departures"))
agg1 <- aggregate(data = table.RollingHourlyThroughputs, Count~Time+Category+Scenario+Runway+Run, "sum")
agg1$Airport <- "All Airports"
agg2 <- aggregate(data = subset(table.RollingHourlyThroughputs, Airport %in% list.airports), Count~Time+Category+Scenario+Runway+Run, "sum")
agg2$Airport <- "All Major Airports"
table.RollingHourlyThroughputs <- rbind(table.RollingHourlyThroughputs, agg1, agg2)
table.RollingHourlyThroughputs <- table.RollingHourlyThroughputs[,c(1,7,2,6,3,4,5)]

write.csv(table.RollingHourlyThroughputs, file="~/Croatia v3.3/data/RollingHourlyThroughputs.csv", row.names=F)

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

table.current_RD1_1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.FuelBurnTrackMiles))
table.current_RD1_2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.FuelBurnTrackMiles))
table.current_RD1_3.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.FuelBurnTrackMiles))
table.current_RD1_4.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.FuelBurnTrackMiles))
table.current_RD1_5.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.FuelBurnTrackMiles))
table.current_RD2_1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.FuelBurnTrackMiles))
table.current_RD2_2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.FuelBurnTrackMiles))
table.current_RD2_3.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.FuelBurnTrackMiles))
table.current_RD2_4.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.FuelBurnTrackMiles))
table.current_RD2_5.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.FuelBurnTrackMiles))
table.PBN_RD1_1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.FuelBurnTrackMiles))
table.PBN_RD1_2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.FuelBurnTrackMiles))
table.PBN_RD1_3.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.FuelBurnTrackMiles))
table.PBN_RD1_4.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.FuelBurnTrackMiles))
table.PBN_RD1_5.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.FuelBurnTrackMiles))
table.PBN_RD2_1.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.FuelBurnTrackMiles))
table.PBN_RD2_2.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.FuelBurnTrackMiles))
table.PBN_RD2_3.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.FuelBurnTrackMiles))
table.PBN_RD2_4.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.FuelBurnTrackMiles))
table.PBN_RD2_5.FuelBurnTrackMiles <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.FuelBurnTrackMiles))

table.current_RD1_1.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD1_1.FuelBurnTrackMiles$Runway <- "1"; table.current_RD1_1.FuelBurnTrackMiles$Run <- "1"
table.current_RD1_2.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD1_2.FuelBurnTrackMiles$Runway <- "1"; table.current_RD1_2.FuelBurnTrackMiles$Run <- "2"
table.current_RD1_3.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD1_3.FuelBurnTrackMiles$Runway <- "1"; table.current_RD1_3.FuelBurnTrackMiles$Run <- "3"
table.current_RD1_4.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD1_4.FuelBurnTrackMiles$Runway <- "1"; table.current_RD1_4.FuelBurnTrackMiles$Run <- "4"
table.current_RD1_5.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD1_5.FuelBurnTrackMiles$Runway <- "1"; table.current_RD1_5.FuelBurnTrackMiles$Run <- "5"
table.current_RD2_1.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD2_1.FuelBurnTrackMiles$Runway <- "2"; table.current_RD2_1.FuelBurnTrackMiles$Run <- "1"
table.current_RD2_2.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD2_2.FuelBurnTrackMiles$Runway <- "2"; table.current_RD2_2.FuelBurnTrackMiles$Run <- "2"
table.current_RD2_3.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD2_3.FuelBurnTrackMiles$Runway <- "2"; table.current_RD2_3.FuelBurnTrackMiles$Run <- "3"
table.current_RD2_4.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD2_4.FuelBurnTrackMiles$Runway <- "2"; table.current_RD2_4.FuelBurnTrackMiles$Run <- "4"
table.current_RD2_5.FuelBurnTrackMiles$Scenario <- "Current"; table.current_RD2_5.FuelBurnTrackMiles$Runway <- "2"; table.current_RD2_5.FuelBurnTrackMiles$Run <- "5"
table.PBN_RD1_1.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD1_1.FuelBurnTrackMiles$Runway <- "1"; table.PBN_RD1_1.FuelBurnTrackMiles$Run <- "1"
table.PBN_RD1_2.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD1_2.FuelBurnTrackMiles$Runway <- "1"; table.PBN_RD1_2.FuelBurnTrackMiles$Run <- "2"
table.PBN_RD1_3.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD1_3.FuelBurnTrackMiles$Runway <- "1"; table.PBN_RD1_3.FuelBurnTrackMiles$Run <- "3"
table.PBN_RD1_4.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD1_4.FuelBurnTrackMiles$Runway <- "1"; table.PBN_RD1_4.FuelBurnTrackMiles$Run <- "4"
table.PBN_RD1_5.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD1_5.FuelBurnTrackMiles$Runway <- "1"; table.PBN_RD1_5.FuelBurnTrackMiles$Run <- "5"
table.PBN_RD2_1.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD2_1.FuelBurnTrackMiles$Runway <- "2"; table.PBN_RD2_1.FuelBurnTrackMiles$Run <- "1"
table.PBN_RD2_2.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD2_2.FuelBurnTrackMiles$Runway <- "2"; table.PBN_RD2_2.FuelBurnTrackMiles$Run <- "2"
table.PBN_RD2_3.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD2_3.FuelBurnTrackMiles$Runway <- "2"; table.PBN_RD2_3.FuelBurnTrackMiles$Run <- "3"
table.PBN_RD2_4.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD2_4.FuelBurnTrackMiles$Runway <- "2"; table.PBN_RD2_4.FuelBurnTrackMiles$Run <- "4"
table.PBN_RD2_5.FuelBurnTrackMiles$Scenario <- "PBN"; table.PBN_RD2_5.FuelBurnTrackMiles$Runway <- "2"; table.PBN_RD2_5.FuelBurnTrackMiles$Run <- "5"

table.FuelBurnTrackMiles <- rbind(table.current_RD1_1.FuelBurnTrackMiles,table.current_RD1_2.FuelBurnTrackMiles,table.current_RD1_3.FuelBurnTrackMiles,
                                        table.current_RD1_4.FuelBurnTrackMiles,table.current_RD1_5.FuelBurnTrackMiles,
                                        table.current_RD2_1.FuelBurnTrackMiles,table.current_RD2_2.FuelBurnTrackMiles,table.current_RD2_3.FuelBurnTrackMiles,
                                        table.current_RD2_4.FuelBurnTrackMiles,table.current_RD2_5.FuelBurnTrackMiles,
                                        table.PBN_RD1_1.FuelBurnTrackMiles,table.PBN_RD1_2.FuelBurnTrackMiles,table.PBN_RD1_3.FuelBurnTrackMiles,
                                        table.PBN_RD1_4.FuelBurnTrackMiles,table.PBN_RD1_5.FuelBurnTrackMiles,
                                        table.PBN_RD2_1.FuelBurnTrackMiles,table.PBN_RD2_2.FuelBurnTrackMiles,table.PBN_RD2_3.FuelBurnTrackMiles,
                                        table.PBN_RD2_4.FuelBurnTrackMiles,table.PBN_RD2_5.FuelBurnTrackMiles)

#Filter to remove incorrect flights
table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles,
                   !(grepl('LDZD_LDSB',Routing)) &
                   !(grepl('SHARK',Callsign)) & 
                   !(grepl('ARMIX_LDRI',Routing) & Runway == '1' & TrackMiles >= 100) &
                   !(grepl('ARMIX_LDRI',Routing) & Runway == '2' & TrackMiles >= 130) &
                   !(grepl('LDZA_LDOS',Routing)  & TrackMiles >= 140) &
                   !(grepl('PODET_LDOS',Routing) & TrackMiles >= 190) &
                   !(grepl('LDSP_LDOS',Routing) & TrackMiles >= 210) &
                   !(grepl('LDPL_LDOS',Routing) & TrackMiles >= 240) &
                   !(grepl('BAREB_LDOS',Routing) & TrackMiles >= 100) &
                   !(grepl('ARGOM_LDZA',Routing) & Runway == '1' & TrackMiles >= 130) &
                   !(grepl('LDLO_LDZA',Routing) & Runway == '1' & TrackMiles >= 130) &
                   !(grepl('LDZD_LDZA',Routing) & Runway == '1' & TrackMiles >= 150) &
                   !(grepl('KOPRY_LDZA',Routing) & Runway == '1' & TrackMiles >= 150) &
                   !(grepl('LABIN_LDZA',Routing) & Runway == '1' & TrackMiles >= 250) &
                   !(grepl('PERAN_LDZA',Routing) & Runway == '1' & TrackMiles >= 300) &
                   !(grepl('PEROT_LDZA',Routing) & Runway == '1' & TrackMiles >= 250) &
                   !(grepl('PETOV_LDZA',Routing) & Runway == '1' & TrackMiles >= 150) &
                   !(grepl('ARGOM_LDZA',Routing) & Runway == '2' & TrackMiles >= 130) &
                   !(grepl('LDLO_LDZA',Routing) & Runway == '2' & TrackMiles >= 150) &
                   !(grepl('KOPRY_LDZA',Routing) & Runway == '2' & TrackMiles >= 110) &
                   !(grepl('PEROT_LDZA',Routing) & Runway == '2' & TrackMiles >= 200) &
                   !(grepl('PETOV_LDZA',Routing) & Runway == '2' & TrackMiles >= 110))

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
    t1$Waypoint <- paste("From",t1$Origin)
    t2 <- table.FuelBurnTrackMiles[i,]
    t2$RoutingType <- "Departure"
    t2$Waypoint <- paste("To",t2$Destination)
    temp <- rbind(temp,t1,t2)
  }
}
table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, !(RoutingType %in% c("Domestic","Overflight")))
table.FuelBurnTrackMiles <- rbind(table.FuelBurnTrackMiles,temp)
table.FuelBurnTrackMiles$Airport <- ifelse(table.FuelBurnTrackMiles$RoutingType == "Departure",table.FuelBurnTrackMiles$Origin,NA)
table.FuelBurnTrackMiles$Airport <- ifelse(table.FuelBurnTrackMiles$RoutingType == "Arrival",table.FuelBurnTrackMiles$Destination,table.FuelBurnTrackMiles$Airport)
table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, substr(Airport,1,4) %in% list.airports)

table.FuelBurn <- aggregate(data=table.FuelBurnTrackMiles,FuelBurn~Airport+Scenario+Runway+Run+RoutingType+Waypoint,"sum")
temp1 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Runway+Run+Waypoint,"sum")
temp1$RoutingType <- "Arrivals and Departures"
temp2 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Runway+Run+RoutingType,"sum")
temp2$Waypoint <- "All Routes"
temp3 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Runway+Run,"sum")
temp3$RoutingType <- "Arrivals and Departures"
temp3$Waypoint <- "All Routes"
temp4 <- aggregate(data = table.FuelBurn, FuelBurn~Scenario+Runway+Run,"sum")
temp4$RoutingType <- "Arrivals and Departures"
temp4$Waypoint <- "All Routes"
temp4$Airport <- "All Airports"
table.FuelBurn <- rbind(table.FuelBurn,temp1,temp2,temp3,temp4)
table.FuelBurn <- table.FuelBurn[,c(1,7,5,6,2,3,4)]

table.TrackMiles <- aggregate(data=table.FuelBurnTrackMiles,TrackMiles~Airport+Scenario+Runway+Run+RoutingType+Waypoint,"sum")
temp1 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+Runway+Run+Waypoint,"sum")
temp1$RoutingType <- "Arrivals and Departures"
temp2 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+Runway+Run+RoutingType,"sum")
temp2$Waypoint <- "All Routes"
temp3 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Runway+Run+Scenario,"sum")
temp3$RoutingType <- "Arrivals and Departures"
temp3$Waypoint <- "All Routes"
temp4 <- aggregate(data = table.TrackMiles, TrackMiles~Scenario+Runway+Run,"sum")
temp4$RoutingType <- "Arrivals and Departures"
temp4$Waypoint <- "All Routes"
temp4$Airport <- "All Airports"
table.TrackMiles <- rbind(table.TrackMiles,temp1,temp2,temp3,temp4)
table.TrackMiles <- table.TrackMiles[,c(1,7,5,6,2,3,4)]

write.csv(table.FuelBurnTrackMiles, file="~/Croatia v3.3/data/FuelBurnTrackMiles.csv", row.names=F)
write.csv(table.FuelBurn, file="~/Croatia v3.3/data/FuelBurn.csv", row.names=F)
write.csv(table.TrackMiles, file="~/Croatia v3.3/data/TrackMiles.csv", row.names=F)

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
t1."OriginInfo" AS "Origin1",
t2."OriginInfo" AS "Origin2",
t1."DestinationInfo" AS "Destination1",
t2."DestinationInfo" AS "Destination2",
SPLIT_PART("2DLocation",\' \',6)::numeric + SPLIT_PART("2DLocation",\' \',7)::numeric/60 + SPLIT_PART("2DLocation",\' \',8)::numeric/3600 AS "Longitude",
SPLIT_PART("2DLocation",\' \',2)::numeric + SPLIT_PART("2DLocation",\' \',3)::numeric/60 + SPLIT_PART("2DLocation",\' \',4)::numeric/3600 AS "Latitude"
FROM "ENTER SCHEMA NAME"."Conflict"
LEFT JOIN LATERAL (
SELECT "Callsign", "FlightType", "Routing", "OriginInfo", "DestinationInfo"
FROM "ENTER SCHEMA NAME"."RS.FB_TM"
WHERE "Time_day" = 2
AND "Callsign" = "FlightPlan1"
) AS t1 ON TRUE
LEFT JOIN LATERAL (
SELECT "Callsign", "FlightType", "Routing", "OriginInfo", "DestinationInfo"
FROM "ENTER SCHEMA NAME"."RS.FB_TM"
WHERE "Time_day" = 2
AND "Callsign" = "FlightPlan2"
) AS t2 ON TRUE
WHERE "StartTime_day" = 2'

table.current_RD1_1.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.Conflicts))
table.current_RD1_2.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.Conflicts))
table.current_RD1_3.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.Conflicts))
table.current_RD1_4.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.Conflicts))
table.current_RD1_5.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.Conflicts))
table.current_RD2_1.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.Conflicts))
table.current_RD2_2.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.Conflicts))
table.current_RD2_3.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.Conflicts))
table.current_RD2_4.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.Conflicts))
table.current_RD2_5.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.Conflicts))
table.PBN_RD1_1.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.Conflicts))
table.PBN_RD1_2.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.Conflicts))
table.PBN_RD1_3.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.Conflicts))
table.PBN_RD1_4.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.Conflicts))
table.PBN_RD1_5.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.Conflicts))
table.PBN_RD2_1.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.Conflicts))
table.PBN_RD2_2.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.Conflicts))
table.PBN_RD2_3.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.Conflicts))
table.PBN_RD2_4.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.Conflicts))
table.PBN_RD2_5.Conflicts <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.Conflicts))

table.current_RD1_1.Conflicts$Scenario <- "Current"; table.current_RD1_1.Conflicts$Runway <- "1"; table.current_RD1_1.Conflicts$Run <- "1"
table.current_RD1_2.Conflicts$Scenario <- "Current"; table.current_RD1_2.Conflicts$Runway <- "1"; table.current_RD1_2.Conflicts$Run <- "2"
table.current_RD1_3.Conflicts$Scenario <- "Current"; table.current_RD1_3.Conflicts$Runway <- "1"; table.current_RD1_3.Conflicts$Run <- "3"
table.current_RD1_4.Conflicts$Scenario <- "Current"; table.current_RD1_4.Conflicts$Runway <- "1"; table.current_RD1_4.Conflicts$Run <- "4"
table.current_RD1_5.Conflicts$Scenario <- "Current"; table.current_RD1_5.Conflicts$Runway <- "1"; table.current_RD1_5.Conflicts$Run <- "5"
table.current_RD2_1.Conflicts$Scenario <- "Current"; table.current_RD2_1.Conflicts$Runway <- "2"; table.current_RD2_1.Conflicts$Run <- "1"
table.current_RD2_2.Conflicts$Scenario <- "Current"; table.current_RD2_2.Conflicts$Runway <- "2"; table.current_RD2_2.Conflicts$Run <- "2"
table.current_RD2_3.Conflicts$Scenario <- "Current"; table.current_RD2_3.Conflicts$Runway <- "2"; table.current_RD2_3.Conflicts$Run <- "3"
table.current_RD2_4.Conflicts$Scenario <- "Current"; table.current_RD2_4.Conflicts$Runway <- "2"; table.current_RD2_4.Conflicts$Run <- "4"
table.current_RD2_5.Conflicts$Scenario <- "Current"; table.current_RD2_5.Conflicts$Runway <- "2"; table.current_RD2_5.Conflicts$Run <- "5"
table.PBN_RD1_1.Conflicts$Scenario <- "PBN"; table.PBN_RD1_1.Conflicts$Runway <- "1"; table.PBN_RD1_1.Conflicts$Run <- "1"
table.PBN_RD1_2.Conflicts$Scenario <- "PBN"; table.PBN_RD1_2.Conflicts$Runway <- "1"; table.PBN_RD1_2.Conflicts$Run <- "2"
table.PBN_RD1_3.Conflicts$Scenario <- "PBN"; table.PBN_RD1_3.Conflicts$Runway <- "1"; table.PBN_RD1_3.Conflicts$Run <- "3"
table.PBN_RD1_4.Conflicts$Scenario <- "PBN"; table.PBN_RD1_4.Conflicts$Runway <- "1"; table.PBN_RD1_4.Conflicts$Run <- "4"
table.PBN_RD1_5.Conflicts$Scenario <- "PBN"; table.PBN_RD1_5.Conflicts$Runway <- "1"; table.PBN_RD1_5.Conflicts$Run <- "5"
table.PBN_RD2_1.Conflicts$Scenario <- "PBN"; table.PBN_RD2_1.Conflicts$Runway <- "2"; table.PBN_RD2_1.Conflicts$Run <- "1"
table.PBN_RD2_2.Conflicts$Scenario <- "PBN"; table.PBN_RD2_2.Conflicts$Runway <- "2"; table.PBN_RD2_2.Conflicts$Run <- "2"
table.PBN_RD2_3.Conflicts$Scenario <- "PBN"; table.PBN_RD2_3.Conflicts$Runway <- "2"; table.PBN_RD2_3.Conflicts$Run <- "3"
table.PBN_RD2_4.Conflicts$Scenario <- "PBN"; table.PBN_RD2_4.Conflicts$Runway <- "2"; table.PBN_RD2_4.Conflicts$Run <- "4"
table.PBN_RD2_5.Conflicts$Scenario <- "PBN"; table.PBN_RD2_5.Conflicts$Runway <- "2"; table.PBN_RD2_5.Conflicts$Run <- "5"

table.Conflicts <- rbind(table.current_RD1_1.Conflicts,table.current_RD1_2.Conflicts,table.current_RD1_3.Conflicts,
                                  table.current_RD1_4.Conflicts,table.current_RD1_5.Conflicts,
                                  table.current_RD2_1.Conflicts,table.current_RD2_2.Conflicts,table.current_RD2_3.Conflicts,
                                  table.current_RD2_4.Conflicts,table.current_RD2_5.Conflicts,
                                  table.PBN_RD1_1.Conflicts,table.PBN_RD1_2.Conflicts,table.PBN_RD1_3.Conflicts,
                                  table.PBN_RD1_4.Conflicts,table.PBN_RD1_5.Conflicts,
                                  table.PBN_RD2_1.Conflicts,table.PBN_RD2_2.Conflicts,table.PBN_RD2_3.Conflicts,
                                  table.PBN_RD2_4.Conflicts,table.PBN_RD2_5.Conflicts)

for(i in 1:ncol(table.Conflicts)) table.Conflicts[[i]][is.na(table.Conflicts[[i]])] = "NULL"

robfilters <- function(){
    d <- subset(table.Conflicts, (FlightType1 %in% "GeneralAviation" & FlightType2 %in% "GeneralAviation" & Severity >= 4)
                | (((FlightType1 %in% "AirCarrier" & FlightType2 %in% "GeneralAviation") | (FlightType1 %in% "GeneralAviation" & FlightType2 %in% "AirCarrier")) & Severity >= 2)
                | (FlightType1 %in% "AirCarrier" & FlightType2 %in% "AirCarrier" & Severity >= 1)
                | ((FlightType1 %in% "Military" | FlightType2 %in% "Military") & Severity >= 1))
    swag <- d[0,]
    dest1 <- d$Destination1
    dest2 <- d$Destination2
    orig1 <- d$Origin1
    orig2 <- d$Origin2
    ft1 <- d$FlightType1
    ft2 <- d$FlightType2
    sev <- d$Severity
    alt <- as.numeric(d$Altitude_ft)
    for (i in 1:nrow(d)) {
        if (
            !(dest1[i] == dest2[i]
              & dest1[i] %in% list.airports
              & dest2[i] %in% list.airports
            )
            & !(ft1[i] == "AirCarrier" & ft2[i] == "AirCarrier"
                & orig1[i] == "LDZA" & orig2[i] == "LDZA"
                & alt[i] <= 5000
                & sev[i] %in% c("1","2")
            )
            & !(((orig1[i] == "LDSP" & dest2[i] == "LDSP")|(dest1[i] == "LDSP" & orig2[i] == "LDSP"))
                & alt[i] <= 6500
                & sev[i] %in% c("1","2")
            )
            & !(ft1[i] == "AirCarrier" & ft2[i] == "AirCarrier"
                & ((orig1[i] == "LDDU" & dest2[i] == "LDDU")|(dest1[i] == "LDDU" & orig2[i] == "LDDU"))
                & alt[i] <= 10000
                & sev[i] %in% c("1","2")
            )
            & !(ft1[i] == "Military" & ft2[i] == "Military"
            )
            & !((ft1[i] == "Military" | ft2[i] == "Military")
                & dest1[i] == "LDZA" & dest2[i] == "LDZA"
            )
            & !((ft1[i] == "Military" | ft2[i] == "Military")
                & dest1[i] == "LDZD" & dest2[i] == "LDZD"
            )
        ) {
            swag <- rbind(swag, d[i,])
        }
    }
    row.names(swag) <- 1:nrow(swag)
    return(swag)
}

table.Conflicts <- robfilters()

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
  if (temp2[i,1] == temp2[i,2] & temp2[i,1] %in% c("IFR","VFR") & temp2[i,2] %in% c("IFR","VFR")) {
    temp2[i,3] <- paste("Both",temp2[i,1])
  } else if (temp2[i,1] != temp2[i,2] & temp2[i,1] %in% c("IFR","VFR") & temp2[i,2] %in% c("IFR","VFR")) {
    temp2[i,3] <- paste(temp2[i,1],"&",temp2[i,2])
  } else {
    temp2[i,3] <- "Military"
  }
}
table.Conflicts <- cbind(table.Conflicts, temp1[,3], temp2[,3])
names(table.Conflicts)[c(31,32)] <- c("FlightPlanPhases","FlightTypes")
table.Conflicts <- table.Conflicts[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,31,18,19,32,20,21,22,23,24,25,26,27,28,29,30)]

table.ConflictType <- plyr::count(table.Conflicts, vars = c("Sector","ConflictType","Scenario","Runway","Run"))
temp1 <- aggregate(data = table.ConflictType,freq~ConflictType+Scenario+Runway+Run,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.ConflictType, Sector %in% list.sectors),freq~ConflictType+Scenario+Runway+Run,"sum")
temp2$Sector <- "All TMA"
table.ConflictType <- rbind(table.ConflictType, temp1, temp2)
table.ConflictType$ConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.ConflictType$ConflictType)
names(table.ConflictType) <- c("Sector","ConflictType","Scenario","Runway","Run","Count")
table.ConflictType <- table.ConflictType[,c(1,6,2,3,4,5)]

table.LateralConflictType <- plyr::count(table.Conflicts, vars = c("Sector","LateralConflictType","Scenario","Runway","Run"))
temp1 <- aggregate(data = table.LateralConflictType,freq~LateralConflictType+Scenario+Runway+Run,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.LateralConflictType, Sector %in% list.sectors),freq~LateralConflictType+Scenario+Runway+Run,"sum")
temp2$Sector <- "All TMA"
table.LateralConflictType <- rbind(table.LateralConflictType, temp1, temp2)
table.LateralConflictType$LateralConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.LateralConflictType$LateralConflictType)
names(table.LateralConflictType) <- c("Sector","LateralConflictType","Scenario","Runway","Run","Count")
table.LateralConflictType <- table.LateralConflictType[,c(1,6,2,3,4,5)]

table.VerticalConflictType <- plyr::count(table.Conflicts, vars = c("Sector","VerticalConflictType","Scenario","Runway","Run"))
temp1 <- aggregate(data = table.VerticalConflictType,freq~VerticalConflictType+Scenario+Runway+Run,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.VerticalConflictType, Sector %in% list.sectors),freq~VerticalConflictType+Scenario+Runway+Run,"sum")
temp2$Sector <- "All TMA"
table.VerticalConflictType <- rbind(table.VerticalConflictType, temp1, temp2)
table.VerticalConflictType$VerticalConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.VerticalConflictType$VerticalConflictType)
names(table.VerticalConflictType) <- c("Sector","VerticalConflictType","Scenario","Runway","Run","Count")
table.VerticalConflictType <- table.VerticalConflictType[,c(1,6,2,3,4,5)]

table.Severity <- plyr::count(table.Conflicts, vars = c("Sector","Severity","Scenario","Runway","Run"))
temp1 <- aggregate(data = table.Severity,freq~Severity+Scenario+Runway+Run,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.Severity, Sector %in% list.sectors),freq~Severity+Scenario+Runway+Run,"sum")
temp2$Sector <- "All TMA"
table.Severity <- rbind(table.Severity, temp1, temp2)
names(table.Severity) <- c("Sector","Severity","Scenario","Runway","Run","Count")
table.Severity <- table.Severity[,c(1,6,2,3,4,5)]

table.VerticalSeverity <- plyr::count(table.Conflicts, vars = c("Sector","VerticalSeverity","Scenario","Runway","Run"))
temp1 <- aggregate(data = table.VerticalSeverity,freq~VerticalSeverity+Scenario+Runway+Run,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.VerticalSeverity, Sector %in% list.sectors),freq~VerticalSeverity+Scenario+Runway+Run,"sum")
temp2$Sector <- "All TMA"
table.VerticalSeverity <- rbind(table.VerticalSeverity, temp1, temp2)
names(table.VerticalSeverity) <- c("Sector","VerticalSeverity","Scenario","Runway","Run","Count")
table.VerticalSeverity <- table.VerticalSeverity[,c(1,6,2,3,4,5)]

table.FlightType <- plyr::count(table.Conflicts, vars = c("Sector","FlightTypes","Scenario","Runway","Run"))
temp1 <- aggregate(data = table.FlightType,freq~FlightTypes+Scenario+Runway+Run,"sum")
temp1$Sector <- "All Sectors"
temp2 <- aggregate(data = subset(table.FlightType, Sector %in% list.sectors),freq~FlightTypes+Scenario+Runway+Run,"sum")
temp2$Sector <- "All TMA"
table.FlightType <- rbind(table.FlightType, temp1, temp2)
names(table.FlightType) <- c("Sector","FlightTypes","Scenario","Runway","Run","Count")
table.FlightType <- table.FlightType[,c(1,6,2,3,4,5)]

write.csv(table.Conflicts, file="~/Croatia v3.3/data/Conflicts.csv", row.names=F)
write.csv(table.ConflictType, file="~/Croatia v3.3/data/ConflictType.csv", row.names=F)
write.csv(table.LateralConflictType, file="~/Croatia v3.3/data/LateralConflictType.csv", row.names=F)
write.csv(table.VerticalConflictType, file="~/Croatia v3.3/data/VerticalConflictType.csv", row.names=F)
write.csv(table.Severity, file="~/Croatia v3.3/data/Severity.csv", row.names=F)
write.csv(table.VerticalSeverity, file="~/Croatia v3.3/data/VerticalSeverity.csv", row.names=F)
write.csv(table.FlightType, file="~/Croatia v3.3/data/FlightType.csv", row.names=F)

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

table.current_RD1_1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.ConflictsFlightPlanPhase))
table.current_RD1_2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.ConflictsFlightPlanPhase))
table.current_RD1_3.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.ConflictsFlightPlanPhase))
table.current_RD1_4.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.ConflictsFlightPlanPhase))
table.current_RD1_5.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.ConflictsFlightPlanPhase))
table.current_RD2_1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.ConflictsFlightPlanPhase))
table.current_RD2_2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.ConflictsFlightPlanPhase))
table.current_RD2_3.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.ConflictsFlightPlanPhase))
table.current_RD2_4.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.ConflictsFlightPlanPhase))
table.current_RD2_5.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.ConflictsFlightPlanPhase))
table.PBN_RD1_1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.ConflictsFlightPlanPhase))
table.PBN_RD1_2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.ConflictsFlightPlanPhase))
table.PBN_RD1_3.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.ConflictsFlightPlanPhase))
table.PBN_RD1_4.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.ConflictsFlightPlanPhase))
table.PBN_RD1_5.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.ConflictsFlightPlanPhase))
table.PBN_RD2_1.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.ConflictsFlightPlanPhase))
table.PBN_RD2_2.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.ConflictsFlightPlanPhase))
table.PBN_RD2_3.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.ConflictsFlightPlanPhase))
table.PBN_RD2_4.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.ConflictsFlightPlanPhase))
table.PBN_RD2_5.ConflictsFlightPlanPhase <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.ConflictsFlightPlanPhase))

table.current_RD1_1.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD1_1.ConflictsFlightPlanPhase$Runway <- "1"; table.current_RD1_1.ConflictsFlightPlanPhase$Run <- "1"
table.current_RD1_2.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD1_2.ConflictsFlightPlanPhase$Runway <- "1"; table.current_RD1_2.ConflictsFlightPlanPhase$Run <- "2"
table.current_RD1_3.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD1_3.ConflictsFlightPlanPhase$Runway <- "1"; table.current_RD1_3.ConflictsFlightPlanPhase$Run <- "3"
table.current_RD1_4.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD1_4.ConflictsFlightPlanPhase$Runway <- "1"; table.current_RD1_4.ConflictsFlightPlanPhase$Run <- "4"
table.current_RD1_5.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD1_5.ConflictsFlightPlanPhase$Runway <- "1"; table.current_RD1_5.ConflictsFlightPlanPhase$Run <- "5"
table.current_RD2_1.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD2_1.ConflictsFlightPlanPhase$Runway <- "2"; table.current_RD2_1.ConflictsFlightPlanPhase$Run <- "1"
table.current_RD2_2.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD2_2.ConflictsFlightPlanPhase$Runway <- "2"; table.current_RD2_2.ConflictsFlightPlanPhase$Run <- "2"
table.current_RD2_3.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD2_3.ConflictsFlightPlanPhase$Runway <- "2"; table.current_RD2_3.ConflictsFlightPlanPhase$Run <- "3"
table.current_RD2_4.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD2_4.ConflictsFlightPlanPhase$Runway <- "2"; table.current_RD2_4.ConflictsFlightPlanPhase$Run <- "4"
table.current_RD2_5.ConflictsFlightPlanPhase$Scenario <- "Current"; table.current_RD2_5.ConflictsFlightPlanPhase$Runway <- "2"; table.current_RD2_5.ConflictsFlightPlanPhase$Run <- "5"
table.PBN_RD1_1.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD1_1.ConflictsFlightPlanPhase$Runway <- "1"; table.PBN_RD1_1.ConflictsFlightPlanPhase$Run <- "1"
table.PBN_RD1_2.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD1_2.ConflictsFlightPlanPhase$Runway <- "1"; table.PBN_RD1_2.ConflictsFlightPlanPhase$Run <- "2"
table.PBN_RD1_3.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD1_3.ConflictsFlightPlanPhase$Runway <- "1"; table.PBN_RD1_3.ConflictsFlightPlanPhase$Run <- "3"
table.PBN_RD1_4.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD1_4.ConflictsFlightPlanPhase$Runway <- "1"; table.PBN_RD1_4.ConflictsFlightPlanPhase$Run <- "4"
table.PBN_RD1_5.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD1_5.ConflictsFlightPlanPhase$Runway <- "1"; table.PBN_RD1_5.ConflictsFlightPlanPhase$Run <- "5"
table.PBN_RD2_1.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD2_1.ConflictsFlightPlanPhase$Runway <- "2"; table.PBN_RD2_1.ConflictsFlightPlanPhase$Run <- "1"
table.PBN_RD2_2.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD2_2.ConflictsFlightPlanPhase$Runway <- "2"; table.PBN_RD2_2.ConflictsFlightPlanPhase$Run <- "2"
table.PBN_RD2_3.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD2_3.ConflictsFlightPlanPhase$Runway <- "2"; table.PBN_RD2_3.ConflictsFlightPlanPhase$Run <- "3"
table.PBN_RD2_4.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD2_4.ConflictsFlightPlanPhase$Runway <- "2"; table.PBN_RD2_4.ConflictsFlightPlanPhase$Run <- "4"
table.PBN_RD2_5.ConflictsFlightPlanPhase$Scenario <- "PBN"; table.PBN_RD2_5.ConflictsFlightPlanPhase$Runway <- "2"; table.PBN_RD2_5.ConflictsFlightPlanPhase$Run <- "5"

table.ConflictsFlightPlanPhase <- rbind(table.current_RD1_1.ConflictsFlightPlanPhase,table.current_RD1_2.ConflictsFlightPlanPhase,table.current_RD1_3.ConflictsFlightPlanPhase,
                         table.current_RD1_4.ConflictsFlightPlanPhase,table.current_RD1_5.ConflictsFlightPlanPhase,
                         table.current_RD2_1.ConflictsFlightPlanPhase,table.current_RD2_2.ConflictsFlightPlanPhase,table.current_RD2_3.ConflictsFlightPlanPhase,
                         table.current_RD2_4.ConflictsFlightPlanPhase,table.current_RD2_5.ConflictsFlightPlanPhase,
                         table.PBN_RD1_1.ConflictsFlightPlanPhase,table.PBN_RD1_2.ConflictsFlightPlanPhase,table.PBN_RD1_3.ConflictsFlightPlanPhase,
                         table.PBN_RD1_4.ConflictsFlightPlanPhase,table.PBN_RD1_5.ConflictsFlightPlanPhase,
                         table.PBN_RD2_1.ConflictsFlightPlanPhase,table.PBN_RD2_2.ConflictsFlightPlanPhase,table.PBN_RD2_3.ConflictsFlightPlanPhase,
                         table.PBN_RD2_4.ConflictsFlightPlanPhase,table.PBN_RD2_5.ConflictsFlightPlanPhase)

agg1 <- aggregate(data = table.ConflictsFlightPlanPhase,Count~FlightPlanPhases+Scenario+Runway+Run,"sum")
agg1$Sector <- "All Sectors"
agg2 <- aggregate(data = subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors),Count~FlightPlanPhases+Scenario+Runway+Run,"sum")
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
table.ConflictsFlightPlanPhase <- table.ConflictsFlightPlanPhase[,c(1,3,2,4,5,6)]

write.csv(table.ConflictsFlightPlanPhase, file="~/Croatia v3.3/data/ConflictsFlightPlanPhase.csv", row.names=F)

# Sector Occupancy --------------------------------------------------------
query.SectorOccupancy <-
  'SELECT
to_char("Time_time", \'HH24:MI\') AS "Time",
"RadarController" AS "Sector",
"AircraftList" AS "Count"
FROM "ENTER SCHEMA NAME"."RS_SECOCC"
WHERE "Time_day" = 2'

table.current_RD1_1.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.SectorOccupancy))
table.current_RD1_2.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.SectorOccupancy))
table.current_RD1_3.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.SectorOccupancy))
table.current_RD1_4.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.SectorOccupancy))
table.current_RD1_5.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.SectorOccupancy))
table.current_RD2_1.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.SectorOccupancy))
table.current_RD2_2.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.SectorOccupancy))
table.current_RD2_3.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.SectorOccupancy))
table.current_RD2_4.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.SectorOccupancy))
table.current_RD2_5.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.SectorOccupancy))
table.PBN_RD1_1.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.SectorOccupancy))
table.PBN_RD1_2.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.SectorOccupancy))
table.PBN_RD1_3.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.SectorOccupancy))
table.PBN_RD1_4.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.SectorOccupancy))
table.PBN_RD1_5.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.SectorOccupancy))
table.PBN_RD2_1.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.SectorOccupancy))
table.PBN_RD2_2.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.SectorOccupancy))
table.PBN_RD2_3.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.SectorOccupancy))
table.PBN_RD2_4.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.SectorOccupancy))
table.PBN_RD2_5.SectorOccupancy <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.SectorOccupancy))

table.current_RD1_1.SectorOccupancy$Scenario <- "Current"; table.current_RD1_1.SectorOccupancy$Runway <- "1"; table.current_RD1_1.SectorOccupancy$Run <- "1"
table.current_RD1_2.SectorOccupancy$Scenario <- "Current"; table.current_RD1_2.SectorOccupancy$Runway <- "1"; table.current_RD1_2.SectorOccupancy$Run <- "2"
table.current_RD1_3.SectorOccupancy$Scenario <- "Current"; table.current_RD1_3.SectorOccupancy$Runway <- "1"; table.current_RD1_3.SectorOccupancy$Run <- "3"
table.current_RD1_4.SectorOccupancy$Scenario <- "Current"; table.current_RD1_4.SectorOccupancy$Runway <- "1"; table.current_RD1_4.SectorOccupancy$Run <- "4"
table.current_RD1_5.SectorOccupancy$Scenario <- "Current"; table.current_RD1_5.SectorOccupancy$Runway <- "1"; table.current_RD1_5.SectorOccupancy$Run <- "5"
table.current_RD2_1.SectorOccupancy$Scenario <- "Current"; table.current_RD2_1.SectorOccupancy$Runway <- "2"; table.current_RD2_1.SectorOccupancy$Run <- "1"
table.current_RD2_2.SectorOccupancy$Scenario <- "Current"; table.current_RD2_2.SectorOccupancy$Runway <- "2"; table.current_RD2_2.SectorOccupancy$Run <- "2"
table.current_RD2_3.SectorOccupancy$Scenario <- "Current"; table.current_RD2_3.SectorOccupancy$Runway <- "2"; table.current_RD2_3.SectorOccupancy$Run <- "3"
table.current_RD2_4.SectorOccupancy$Scenario <- "Current"; table.current_RD2_4.SectorOccupancy$Runway <- "2"; table.current_RD2_4.SectorOccupancy$Run <- "4"
table.current_RD2_5.SectorOccupancy$Scenario <- "Current"; table.current_RD2_5.SectorOccupancy$Runway <- "2"; table.current_RD2_5.SectorOccupancy$Run <- "5"
table.PBN_RD1_1.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD1_1.SectorOccupancy$Runway <- "1"; table.PBN_RD1_1.SectorOccupancy$Run <- "1"
table.PBN_RD1_2.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD1_2.SectorOccupancy$Runway <- "1"; table.PBN_RD1_2.SectorOccupancy$Run <- "2"
table.PBN_RD1_3.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD1_3.SectorOccupancy$Runway <- "1"; table.PBN_RD1_3.SectorOccupancy$Run <- "3"
table.PBN_RD1_4.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD1_4.SectorOccupancy$Runway <- "1"; table.PBN_RD1_4.SectorOccupancy$Run <- "4"
table.PBN_RD1_5.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD1_5.SectorOccupancy$Runway <- "1"; table.PBN_RD1_5.SectorOccupancy$Run <- "5"
table.PBN_RD2_1.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD2_1.SectorOccupancy$Runway <- "2"; table.PBN_RD2_1.SectorOccupancy$Run <- "1"
table.PBN_RD2_2.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD2_2.SectorOccupancy$Runway <- "2"; table.PBN_RD2_2.SectorOccupancy$Run <- "2"
table.PBN_RD2_3.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD2_3.SectorOccupancy$Runway <- "2"; table.PBN_RD2_3.SectorOccupancy$Run <- "3"
table.PBN_RD2_4.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD2_4.SectorOccupancy$Runway <- "2"; table.PBN_RD2_4.SectorOccupancy$Run <- "4"
table.PBN_RD2_5.SectorOccupancy$Scenario <- "PBN"; table.PBN_RD2_5.SectorOccupancy$Runway <- "2"; table.PBN_RD2_5.SectorOccupancy$Run <- "5"

table.SectorOccupancy <- rbind(table.current_RD1_1.SectorOccupancy,table.current_RD1_2.SectorOccupancy,table.current_RD1_3.SectorOccupancy,
                                        table.current_RD1_4.SectorOccupancy,table.current_RD1_5.SectorOccupancy,
                                        table.current_RD2_1.SectorOccupancy,table.current_RD2_2.SectorOccupancy,table.current_RD2_3.SectorOccupancy,
                                        table.current_RD2_4.SectorOccupancy,table.current_RD2_5.SectorOccupancy,
                                        table.PBN_RD1_1.SectorOccupancy,table.PBN_RD1_2.SectorOccupancy,table.PBN_RD1_3.SectorOccupancy,
                                        table.PBN_RD1_4.SectorOccupancy,table.PBN_RD1_5.SectorOccupancy,
                                        table.PBN_RD2_1.SectorOccupancy,table.PBN_RD2_2.SectorOccupancy,table.PBN_RD2_3.SectorOccupancy,
                                        table.PBN_RD2_4.SectorOccupancy,table.PBN_RD2_5.SectorOccupancy)

agg1 <- aggregate(data = table.SectorOccupancy,Count~Time+Scenario+Runway+Run,"sum")
agg1$Sector <- "All Sectors"
agg2 <- aggregate(data = subset(table.SectorOccupancy, Sector %in% list.sectors),Count~Time+Scenario+Runway+Run,"sum")
agg2$Sector <- "All TMA"
table.SectorOccupancy <- rbind(table.SectorOccupancy,agg1,agg2)

write.csv(table.SectorOccupancy, file="~/Croatia v3.3/data/SectorOccupancy.csv", row.names=F)

# Sector Entry ------------------------------------------------------------
query.SectorEntry <-
  'SELECT
to_char("Time_time", \'HH24:MI\') AS "Time", 
"RadarController" AS "Sector",
"LastPeriodReceivedAircraftCount" AS "Entries"
FROM "ENTER SCHEMA NAME"."RS_SECENTRY"
WHERE "Time_day" = 2'

table.current_RD1_1.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.SectorEntry))
table.current_RD1_2.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.SectorEntry))
table.current_RD1_3.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.SectorEntry))
table.current_RD1_4.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.SectorEntry))
table.current_RD1_5.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.SectorEntry))
table.current_RD2_1.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.SectorEntry))
table.current_RD2_2.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.SectorEntry))
table.current_RD2_3.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.SectorEntry))
table.current_RD2_4.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.SectorEntry))
table.current_RD2_5.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.SectorEntry))
table.PBN_RD1_1.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.SectorEntry))
table.PBN_RD1_2.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.SectorEntry))
table.PBN_RD1_3.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.SectorEntry))
table.PBN_RD1_4.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.SectorEntry))
table.PBN_RD1_5.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.SectorEntry))
table.PBN_RD2_1.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.SectorEntry))
table.PBN_RD2_2.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.SectorEntry))
table.PBN_RD2_3.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.SectorEntry))
table.PBN_RD2_4.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.SectorEntry))
table.PBN_RD2_5.SectorEntry <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.SectorEntry))

table.current_RD1_1.SectorEntry$Scenario <- "Current"; table.current_RD1_1.SectorEntry$Runway <- "1"; table.current_RD1_1.SectorEntry$Run <- "1"
table.current_RD1_2.SectorEntry$Scenario <- "Current"; table.current_RD1_2.SectorEntry$Runway <- "1"; table.current_RD1_2.SectorEntry$Run <- "2"
table.current_RD1_3.SectorEntry$Scenario <- "Current"; table.current_RD1_3.SectorEntry$Runway <- "1"; table.current_RD1_3.SectorEntry$Run <- "3"
table.current_RD1_4.SectorEntry$Scenario <- "Current"; table.current_RD1_4.SectorEntry$Runway <- "1"; table.current_RD1_4.SectorEntry$Run <- "4"
table.current_RD1_5.SectorEntry$Scenario <- "Current"; table.current_RD1_5.SectorEntry$Runway <- "1"; table.current_RD1_5.SectorEntry$Run <- "5"
table.current_RD2_1.SectorEntry$Scenario <- "Current"; table.current_RD2_1.SectorEntry$Runway <- "2"; table.current_RD2_1.SectorEntry$Run <- "1"
table.current_RD2_2.SectorEntry$Scenario <- "Current"; table.current_RD2_2.SectorEntry$Runway <- "2"; table.current_RD2_2.SectorEntry$Run <- "2"
table.current_RD2_3.SectorEntry$Scenario <- "Current"; table.current_RD2_3.SectorEntry$Runway <- "2"; table.current_RD2_3.SectorEntry$Run <- "3"
table.current_RD2_4.SectorEntry$Scenario <- "Current"; table.current_RD2_4.SectorEntry$Runway <- "2"; table.current_RD2_4.SectorEntry$Run <- "4"
table.current_RD2_5.SectorEntry$Scenario <- "Current"; table.current_RD2_5.SectorEntry$Runway <- "2"; table.current_RD2_5.SectorEntry$Run <- "5"
table.PBN_RD1_1.SectorEntry$Scenario <- "PBN"; table.PBN_RD1_1.SectorEntry$Runway <- "1"; table.PBN_RD1_1.SectorEntry$Run <- "1"
table.PBN_RD1_2.SectorEntry$Scenario <- "PBN"; table.PBN_RD1_2.SectorEntry$Runway <- "1"; table.PBN_RD1_2.SectorEntry$Run <- "2"
table.PBN_RD1_3.SectorEntry$Scenario <- "PBN"; table.PBN_RD1_3.SectorEntry$Runway <- "1"; table.PBN_RD1_3.SectorEntry$Run <- "3"
table.PBN_RD1_4.SectorEntry$Scenario <- "PBN"; table.PBN_RD1_4.SectorEntry$Runway <- "1"; table.PBN_RD1_4.SectorEntry$Run <- "4"
table.PBN_RD1_5.SectorEntry$Scenario <- "PBN"; table.PBN_RD1_5.SectorEntry$Runway <- "1"; table.PBN_RD1_5.SectorEntry$Run <- "5"
table.PBN_RD2_1.SectorEntry$Scenario <- "PBN"; table.PBN_RD2_1.SectorEntry$Runway <- "2"; table.PBN_RD2_1.SectorEntry$Run <- "1"
table.PBN_RD2_2.SectorEntry$Scenario <- "PBN"; table.PBN_RD2_2.SectorEntry$Runway <- "2"; table.PBN_RD2_2.SectorEntry$Run <- "2"
table.PBN_RD2_3.SectorEntry$Scenario <- "PBN"; table.PBN_RD2_3.SectorEntry$Runway <- "2"; table.PBN_RD2_3.SectorEntry$Run <- "3"
table.PBN_RD2_4.SectorEntry$Scenario <- "PBN"; table.PBN_RD2_4.SectorEntry$Runway <- "2"; table.PBN_RD2_4.SectorEntry$Run <- "4"
table.PBN_RD2_5.SectorEntry$Scenario <- "PBN"; table.PBN_RD2_5.SectorEntry$Runway <- "2"; table.PBN_RD2_5.SectorEntry$Run <- "5"

table.SectorEntry <- rbind(table.current_RD1_1.SectorEntry,table.current_RD1_2.SectorEntry,table.current_RD1_3.SectorEntry,
                               table.current_RD1_4.SectorEntry,table.current_RD1_5.SectorEntry,
                               table.current_RD2_1.SectorEntry,table.current_RD2_2.SectorEntry,table.current_RD2_3.SectorEntry,
                               table.current_RD2_4.SectorEntry,table.current_RD2_5.SectorEntry,
                               table.PBN_RD1_1.SectorEntry,table.PBN_RD1_2.SectorEntry,table.PBN_RD1_3.SectorEntry,
                               table.PBN_RD1_4.SectorEntry,table.PBN_RD1_5.SectorEntry,
                               table.PBN_RD2_1.SectorEntry,table.PBN_RD2_2.SectorEntry,table.PBN_RD2_3.SectorEntry,
                               table.PBN_RD2_4.SectorEntry,table.PBN_RD2_5.SectorEntry)

agg1 <- aggregate(data = table.SectorEntry,Entries~Time+Scenario+Runway+Run,"sum")
agg1$Sector <- "All Sectors"
agg2 <- aggregate(data = subset(table.SectorEntry, Sector %in% list.sectors),Entries~Time+Scenario+Runway+Run,"sum")
agg2$Sector <- "All TMA"
table.SectorEntry <- rbind(table.SectorEntry,agg1,agg2)

write.csv(table.SectorEntry, file="~/Croatia v3.3/data/SectorEntry.csv", row.names=F)

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

table.current_RD1_1.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_1,query.Workload))
table.current_RD1_2.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_2,query.Workload))
table.current_RD1_3.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_3,query.Workload))
table.current_RD1_4.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_4,query.Workload))
table.current_RD1_5.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD1_5,query.Workload))
table.current_RD2_1.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_1,query.Workload))
table.current_RD2_2.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_2,query.Workload))
table.current_RD2_3.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_3,query.Workload))
table.current_RD2_4.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_4,query.Workload))
table.current_RD2_5.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.current_RD2_5,query.Workload))
table.PBN_RD1_1.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_1,query.Workload))
table.PBN_RD1_2.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_2,query.Workload))
table.PBN_RD1_3.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_3,query.Workload))
table.PBN_RD1_4.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_4,query.Workload))
table.PBN_RD1_5.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD1_5,query.Workload))
table.PBN_RD2_1.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_1,query.Workload))
table.PBN_RD2_2.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_2,query.Workload))
table.PBN_RD2_3.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_3,query.Workload))
table.PBN_RD2_4.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_4,query.Workload))
table.PBN_RD2_5.Workload <- dbGetQuery(con,gsub("ENTER SCHEMA NAME",schema.PBN_RD2_5,query.Workload))

table.current_RD1_1.Workload$Scenario <- "Current"; table.current_RD1_1.Workload$Runway <- "1"; table.current_RD1_1.Workload$Run <- "1"
table.current_RD1_2.Workload$Scenario <- "Current"; table.current_RD1_2.Workload$Runway <- "1"; table.current_RD1_2.Workload$Run <- "2"
table.current_RD1_3.Workload$Scenario <- "Current"; table.current_RD1_3.Workload$Runway <- "1"; table.current_RD1_3.Workload$Run <- "3"
table.current_RD1_4.Workload$Scenario <- "Current"; table.current_RD1_4.Workload$Runway <- "1"; table.current_RD1_4.Workload$Run <- "4"
table.current_RD1_5.Workload$Scenario <- "Current"; table.current_RD1_5.Workload$Runway <- "1"; table.current_RD1_5.Workload$Run <- "5"
table.current_RD2_1.Workload$Scenario <- "Current"; table.current_RD2_1.Workload$Runway <- "2"; table.current_RD2_1.Workload$Run <- "1"
table.current_RD2_2.Workload$Scenario <- "Current"; table.current_RD2_2.Workload$Runway <- "2"; table.current_RD2_2.Workload$Run <- "2"
table.current_RD2_3.Workload$Scenario <- "Current"; table.current_RD2_3.Workload$Runway <- "2"; table.current_RD2_3.Workload$Run <- "3"
table.current_RD2_4.Workload$Scenario <- "Current"; table.current_RD2_4.Workload$Runway <- "2"; table.current_RD2_4.Workload$Run <- "4"
table.current_RD2_5.Workload$Scenario <- "Current"; table.current_RD2_5.Workload$Runway <- "2"; table.current_RD2_5.Workload$Run <- "5"
table.PBN_RD1_1.Workload$Scenario <- "PBN"; table.PBN_RD1_1.Workload$Runway <- "1"; table.PBN_RD1_1.Workload$Run <- "1"
table.PBN_RD1_2.Workload$Scenario <- "PBN"; table.PBN_RD1_2.Workload$Runway <- "1"; table.PBN_RD1_2.Workload$Run <- "2"
table.PBN_RD1_3.Workload$Scenario <- "PBN"; table.PBN_RD1_3.Workload$Runway <- "1"; table.PBN_RD1_3.Workload$Run <- "3"
table.PBN_RD1_4.Workload$Scenario <- "PBN"; table.PBN_RD1_4.Workload$Runway <- "1"; table.PBN_RD1_4.Workload$Run <- "4"
table.PBN_RD1_5.Workload$Scenario <- "PBN"; table.PBN_RD1_5.Workload$Runway <- "1"; table.PBN_RD1_5.Workload$Run <- "5"
table.PBN_RD2_1.Workload$Scenario <- "PBN"; table.PBN_RD2_1.Workload$Runway <- "2"; table.PBN_RD2_1.Workload$Run <- "1"
table.PBN_RD2_2.Workload$Scenario <- "PBN"; table.PBN_RD2_2.Workload$Runway <- "2"; table.PBN_RD2_2.Workload$Run <- "2"
table.PBN_RD2_3.Workload$Scenario <- "PBN"; table.PBN_RD2_3.Workload$Runway <- "2"; table.PBN_RD2_3.Workload$Run <- "3"
table.PBN_RD2_4.Workload$Scenario <- "PBN"; table.PBN_RD2_4.Workload$Runway <- "2"; table.PBN_RD2_4.Workload$Run <- "4"
table.PBN_RD2_5.Workload$Scenario <- "PBN"; table.PBN_RD2_5.Workload$Runway <- "2"; table.PBN_RD2_5.Workload$Run <- "5"

table.Workload <- rbind(table.current_RD1_1.Workload,table.current_RD1_2.Workload,table.current_RD1_3.Workload,
                           table.current_RD1_4.Workload,table.current_RD1_5.Workload,
                           table.current_RD2_1.Workload,table.current_RD2_2.Workload,table.current_RD2_3.Workload,
                           table.current_RD2_4.Workload,table.current_RD2_5.Workload,
                           table.PBN_RD1_1.Workload,table.PBN_RD1_2.Workload,table.PBN_RD1_3.Workload,
                           table.PBN_RD1_4.Workload,table.PBN_RD1_5.Workload,
                           table.PBN_RD2_1.Workload,table.PBN_RD2_2.Workload,table.PBN_RD2_3.Workload,
                           table.PBN_RD2_4.Workload,table.PBN_RD2_5.Workload)

write.csv(table.Workload, file="~/Croatia v3.3/data/Workload.csv", row.names=F)

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

write.csv(table.SectorPolygons, file="~/Croatia v3.3/data/SectorPolygons.csv", row.names=F)

dbDisconnect(con)
