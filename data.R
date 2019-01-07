library(RPostgreSQL)

# con <- dbConnect(dbDriver("PostgreSQL"), dbname="airtopdb",host="192.168.1.137",port=5432,user="think",password="think")
con <- dbConnect(dbDriver("PostgreSQL"), dbname="airtopdb",host="localhost",port=5432,user="rob",password="rob")

table.current.TotalThroughputs <- 
  dbGetQuery(con,'
             SELECT
             COUNT("Callsign") AS "Count", 
             "New DepartureOrArrivalAirport" AS "Airport",
             "AircraftState" AS "Category"
             FROM "Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
             WHERE "Time_day" = 2
             GROUP BY "New DepartureOrArrivalAirport", "AircraftState"
             ORDER BY "New DepartureOrArrivalAirport", "AircraftState"')
table.current.HourlyThroughputs <- 
  dbGetQuery(con,'
             SELECT
             EXTRACT(HOUR FROM "Time_time") AS "Hour",
             COUNT("Callsign") AS "Count", 
             "New DepartureOrArrivalAirport" AS "Airport",
             "AircraftState" AS "Category"
             FROM "Croatia"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
             WHERE "Time_day" = 2
             GROUP BY EXTRACT(HOUR FROM "Time_time"),"New DepartureOrArrivalAirport", "AircraftState"
             ORDER BY "New DepartureOrArrivalAirport", "AircraftState",EXTRACT(HOUR FROM "Time_time")')
table.current.RollingHourlyThroughputs <- 
  dbGetQuery(con,'
             SELECT
             to_char("Time_time", \'HH24:MI\') AS "Time",
             "AirportStatus" AS "Airport",
             "LiftOffCountInPeriod" AS "RollingLiftOffCount",
             "TouchDownCountInPeriod" AS "RollingTouchDownCount",
             "ThroughputCountInPeriod" AS "RollingThroughputCount"
             FROM "Croatia"."RS_RWYTHROUGHPUT"
             WHERE "Time_day" = 2
             ORDER BY "AirportStatus",  "Time_time"')
table.current.Conflicts <- 
  dbGetQuery(con,'
             SELECT 
             "ID",
             "ATCSector" AS "Sector",
             "StartTime_day" AS "Start_Day",
             "StartTime_time" AS "Start_Time",
             "ClosestApproachTime_day" AS "Closest_Day",
             "ClosestApproachTime_time" AS "Closest_Time",
             "EndTime_day" AS "End_Day",
             "EndTime_time" AS "End_Time",
             "ConflictType",
             "Severity",
             "VerticalSeverity",
             "LateralConflictType",
             "VerticalConflictType",
             ROUND("VerticalSeparation_m"::numeric/0.3048, 5) AS "VerticalSeparation",
             ROUND("ReqVerticalSeparation_m"::numeric/0.3048, 5) AS "ReqVerticalSeparation",
             ROUND("LateralSeparation_m"::numeric/1852, 5) AS "LateralSeparation",
             ROUND("ReqLateralSeparation_m"::numeric/1852, 5) AS "ReqLateralSeparation",
             "Altitude_ft",
             "FlightPlanPhase1",
             "FlightPlanPhase2",
             "2DLocation",
             SPLIT_PART("2DLocation",\' \',6)::numeric + SPLIT_PART("2DLocation",\' \',7)::numeric/60 + SPLIT_PART("2DLocation",\' \',8)::numeric/3600 AS "Longitude",
             SPLIT_PART("2DLocation",\' \',2)::numeric + SPLIT_PART("2DLocation",\' \',3)::numeric/60 + SPLIT_PART("2DLocation",\' \',4)::numeric/3600 AS "Latitude"
             FROM "Croatia"."Conflict"
             WHERE "StartTime_day" = 2')
table.current.ConflictsFlightPlanPhase <- 
  dbGetQuery(con,'
             SELECT
             "ATCSector" AS "Sector",
             CASE
             WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2")
             THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2"
             ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1"
             END AS "FlightPlanPhases",
             COUNT(*) AS "Count"
             FROM "Croatia"."Conflict"
             GROUP BY "ATCSector", (CASE WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2") THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2" ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1" END)')
table.current.SectorOccupancy <- 
  dbGetQuery(con,'
             SELECT
             to_char("Time_time", \'HH24:MI\') AS "Time",
             "ATCSector" AS "Sector",
             "AircraftLoad" AS "Count"
             FROM "Croatia"."RS_SECOCC"
             WHERE "Time_day" = 2')
table.current.SectorEntry <- 
  dbGetQuery(con,'
             SELECT
             to_char("Time_time", \'HH24:MI\') AS "Time", 
             "ATCSector" AS "Sector",
             "LastPeriodEntryCount" AS "Entries"
             FROM "Croatia"."RS_SECENTRY"
             WHERE "Time_day" = 2')
# table.PBN.TotalThroughputs <- dbGetQuery(con,'')
# table.PBN.HourlyThroughputs <- dbGetQuery(con,'')
# table.PBN.RollingHourlyThroughputs <- dbGetQuery(con,'')
# table.PBN.Conflicts <- dbGetQuery(con,'')
# table.PBN.ConflictsFlightPlanPhase <- dbGetQuery(con,'')
# table.PBN.SectorOccupancy <- dbGetQuery(con,'')
# table.PBN.SectorEntry <- dbGetQuery(con,'')

dbDisconnect(con)