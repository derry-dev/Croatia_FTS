library(plyr)
library(RPostgreSQL)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(sp)
library(plotly)
#library(lubridate)
library(DT)
library(shinyjs)
# setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path)))

# Change these lists if you want to include other airports/sectors
list.airports <- factor(c("All","LDSP","LDDU","LDZA","LDPL","LDZD","LDLO","LDRI","LDSB","LDOS"), ordered = TRUE) 
list.sectors <- factor(c("All","TMA_DUBROVNIK","TMA_OSIJEK","TMA_PULA","TMA_SPLIT","TMA_ZADAR","TMA_ZAGREB"), ordered = TRUE)


dbimport <- function() {
  
  #con <- dbConnect(dbDriver("PostgreSQL"), dbname="airtopdb",host="192.168.1.137",port=5432,user="think",password="think")
  con <- dbConnect(dbDriver("PostgreSQL"),dbname="airtopdb",host="192.168.1.157",port=5432,user="think",password="think")
  
  table.SectorPolygons <-
    dbGetQuery(con,'
               SELECT
               "Sector",
               --"Block",
               --"PolygonID",
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
  table.current.FuelBurnTrackMiles <- 
    dbGetQuery(con,'
               SELECT
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
               WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) NOT LIKE "DestinationInfo"||\'%\'
              	THEN REPLACE(SPLIT_PART("Routing",\'_\',2),\'.\',\' \')
              	WHEN SPLIT_PART("Routing",\'_\',1) NOT LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN REPLACE(SPLIT_PART("Routing",\'_\',1),\'.\',\' \')
              	WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN NULL
              END AS "Waypoint",
              CASE
              	WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) NOT LIKE "DestinationInfo"||\'%\'
              	THEN \'Departure\'
              	WHEN SPLIT_PART("Routing",\'_\',1) NOT LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN \'Arrival\'
              	WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN \'Domestic\'
              END AS "RoutingType"
              FROM "Croatia"."RS.FB_TM"
              WHERE "Time_day" = 2')
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
               "Severity"::text,
               "VerticalSeverity"::text,
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
  table.current.Workload <- 
    dbGetQuery(con,'
               SELECT "Time", "Sector", ROUND("HourlyWorkload"/36) AS "PercentHourlyWorkload"
               FROM "Croatia"."RollingHourlyWorkload"
               WHERE EXTRACT(second from "Time") = 0
               AND EXTRACT(minute from "Time") IN (0,10,20,30,40,50)
               AND "Sector" LIKE \'TMA_%\'
               ORDER BY "Sector"')
  
  table.PBN.TotalThroughputs <- 
    dbGetQuery(con,'
               SELECT
               COUNT("Callsign") AS "Count", 
               "New DepartureOrArrivalAirport" AS "Airport",
               "AircraftState" AS "Category"
               FROM "Croatia_PBN"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
               WHERE "Time_day" = 2
               GROUP BY "New DepartureOrArrivalAirport", "AircraftState"
               ORDER BY "New DepartureOrArrivalAirport", "AircraftState"')
  table.PBN.HourlyThroughputs <- 
    dbGetQuery(con,'
               SELECT
               EXTRACT(HOUR FROM "Time_time") AS "Hour",
               COUNT("Callsign") AS "Count", 
               "New DepartureOrArrivalAirport" AS "Airport",
               "AircraftState" AS "Category"
               FROM "Croatia_PBN"."FLIGHT.AIRPORT.DEPARTED_OR_REACHED"
               WHERE "Time_day" = 2
               GROUP BY EXTRACT(HOUR FROM "Time_time"),"New DepartureOrArrivalAirport", "AircraftState"
               ORDER BY "New DepartureOrArrivalAirport", "AircraftState",EXTRACT(HOUR FROM "Time_time")')
  table.PBN.RollingHourlyThroughputs <- 
    dbGetQuery(con,'
               SELECT
               to_char("Time_time", \'HH24:MI\') AS "Time",
               "AirportStatus" AS "Airport",
               "LiftOffCountInPeriod" AS "RollingLiftOffCount",
               "TouchDownCountInPeriod" AS "RollingTouchDownCount",
               "ThroughputCountInPeriod" AS "RollingThroughputCount"
               FROM "Croatia_PBN"."RS_RWYTHROUGHPUT"
               WHERE "Time_day" = 2
               ORDER BY "AirportStatus",  "Time_time"')
  table.PBN.FuelBurnTrackMiles <- 
    dbGetQuery(con,'
              SELECT
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
              	WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) NOT LIKE "DestinationInfo"||\'%\'
              	THEN REPLACE(SPLIT_PART("Routing",\'_\',2),\'.\',\' \')
              	WHEN SPLIT_PART("Routing",\'_\',1) NOT LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN REPLACE(SPLIT_PART("Routing",\'_\',1),\'.\',\' \')
              	WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN NULL
              END AS "Waypoint",
              CASE
              	WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) NOT LIKE "DestinationInfo"||\'%\'
              	THEN \'Departure\'
              	WHEN SPLIT_PART("Routing",\'_\',1) NOT LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN \'Arrival\'
              	WHEN SPLIT_PART("Routing",\'_\',1) LIKE "OriginInfo"||\'%\' AND SPLIT_PART("Routing",\'_\',2) LIKE "DestinationInfo"||\'%\'
              	THEN \'Domestic\'
              END AS "RoutingType"
              FROM "Croatia_PBN"."RS.FB_TM"
              WHERE "Time_day" = 2')
  table.PBN.Conflicts <- 
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
               "Severity"::text,
               "VerticalSeverity"::text,
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
               FROM "Croatia_PBN"."Conflict"
               WHERE "StartTime_day" = 2')
  table.PBN.ConflictsFlightPlanPhase <- 
    dbGetQuery(con,'
               SELECT
               "ATCSector" AS "Sector",
               CASE
               WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2")
               THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2"
               ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1"
               END AS "FlightPlanPhases",
               COUNT(*) AS "Count"
               FROM "Croatia_PBN"."Conflict"
               GROUP BY "ATCSector", (CASE WHEN LOWER("FlightPlanPhase1") < LOWER("FlightPlanPhase2") THEN "FlightPlanPhase1" || \' \' || "FlightPlanPhase2" ELSE "FlightPlanPhase2" || \' \' || "FlightPlanPhase1" END)')
  table.PBN.SectorOccupancy <- 
    dbGetQuery(con,'
               SELECT
               to_char("Time_time", \'HH24:MI\') AS "Time",
               "ATCSector" AS "Sector",
               "AircraftLoad" AS "Count"
               FROM "Croatia_PBN"."RS_SECOCC"
               WHERE "Time_day" = 2')
  table.PBN.SectorEntry <- 
    dbGetQuery(con,'
               SELECT
               to_char("Time_time", \'HH24:MI\') AS "Time", 
               "ATCSector" AS "Sector",
               "LastPeriodEntryCount" AS "Entries"
               FROM "Croatia_PBN"."RS_SECENTRY"
               WHERE "Time_day" = 2')
  table.PBN.Workload <- 
    dbGetQuery(con,'
              SELECT "Time", "Sector", ROUND("HourlyWorkload"/36) AS "PercentHourlyWorkload"
              FROM "Croatia_PBN"."RollingHourlyWorkload"
              WHERE EXTRACT(second from "Time") = 0
              AND EXTRACT(minute from "Time") IN (0,10,20,30,40,50)
              AND "Sector" LIKE \'TMA_%\'
              ORDER BY "Sector"')
  
  dbDisconnect(con)
  
  t1 <- table.current.TotalThroughputs
  t1$Scenario <- "Current"
  t2 <- table.PBN.TotalThroughputs
  t2$Scenario <- "PBN"
  table.TotalThroughputs <- rbind(t1,t2)
  temp1 <- aggregate(data = table.TotalThroughputs,Count~Category+Scenario,"sum")
  temp1$Airport <- "All Airports"
  temp2 <- aggregate(data = subset(table.TotalThroughputs, Airport %in% list.airports),Count~Category+Scenario,"sum")
  temp2$Airport <- "All Major Airports"
  temp3 <- aggregate(data = table.TotalThroughputs,Count~Airport+Scenario,"sum")
  temp3$Category <- "All Categories"
  table.TotalThroughputs <- rbind(table.TotalThroughputs, temp1, temp2, temp3)
  
  t1 <- table.current.HourlyThroughputs
  t1$Scenario <- "Current"
  t2 <- table.PBN.HourlyThroughputs
  t2$Scenario <- "PBN"
  table.HourlyThroughputs <- rbind(t1,t2)
  
  t1 <- table.current.RollingHourlyThroughputs
  t1$Scenario <- "Current"
  t2 <- table.PBN.RollingHourlyThroughputs
  t2$Scenario <- "PBN"
  table.RollingHourlyThroughputs <- rbind(t1,t2)
  
  t1 <- table.current.FuelBurnTrackMiles
  t1$Scenario <- "Current"
  t2 <- table.PBN.FuelBurnTrackMiles
  t2$Scenario <- "PBN"
  table.FuelBurnTrackMiles <- rbind(t1,t2)
  table.FuelBurnTrackMiles$FlightType[table.FuelBurnTrackMiles$FlightType == "AirCarrier"] <- "IFR"
  #table.FuelBurnTrackMiles$FlightType[table.FuelBurnTrackMiles$FlightType == "GeneralAviation"] <- "VFR"
  table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, FlightType %in% "IFR" ,select=-c(Time,FlightType,Routing))
  temp <- table.FuelBurnTrackMiles[0,]
  for (i in 1:nrow(table.FuelBurnTrackMiles)) {
    if (table.FuelBurnTrackMiles[i,]$RoutingType == "Domestic") {
      t1 <- table.FuelBurnTrackMiles[i,]
      t1$RoutingType <- "Arrival"
      t1$Waypoint <- t1$Origin
      t2 <- table.FuelBurnTrackMiles[i,]
      t2$RoutingType <- "Departure"
      t2$Waypoint <- t2$Destination
      temp <- rbind(temp,t1,t2)
    }
  }
  table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, !(RoutingType %in% "Domestic"))
  table.FuelBurnTrackMiles <- rbind(table.FuelBurnTrackMiles,temp)
  table.FuelBurnTrackMiles$Airport <- ifelse(table.FuelBurnTrackMiles$RoutingType == "Departure",table.FuelBurnTrackMiles$Origin,NA)
  table.FuelBurnTrackMiles$Airport <- ifelse(table.FuelBurnTrackMiles$RoutingType == "Arrival",table.FuelBurnTrackMiles$Destination,table.FuelBurnTrackMiles$Airport)
  table.FuelBurnTrackMiles <- subset(table.FuelBurnTrackMiles, substr(Airport,1,4) %in% list.airports)
  
  table.FuelBurn <- aggregate(data=table.FuelBurnTrackMiles,FuelBurn~Airport+Scenario+RoutingType+Waypoint,"sum")
  temp1 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+Waypoint,"sum")
  temp1$RoutingType <- "Both"
  temp2 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario+RoutingType,"sum")
  temp2$Waypoint <- "All"
  temp3 <- aggregate(data = table.FuelBurn, FuelBurn~Airport+Scenario,"sum")
  temp3$RoutingType <- "Both"
  temp3$Waypoint <- "All"
  temp4 <- aggregate(data = table.FuelBurn, FuelBurn~Scenario,"sum")
  temp4$RoutingType <- "Both"
  temp4$Waypoint <- "All"
  temp4$Airport <- "All"
  table.FuelBurn <- rbind(table.FuelBurn,temp1,temp2,temp3,temp4)
  
  table.TrackMiles <- aggregate(data=table.FuelBurnTrackMiles,TrackMiles~Airport+Scenario+RoutingType+Waypoint,"sum")
  temp1 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+Waypoint,"sum")
  temp1$RoutingType <- "Both"
  temp2 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario+RoutingType,"sum")
  temp2$Waypoint <- "All"
  temp3 <- aggregate(data = table.TrackMiles, TrackMiles~Airport+Scenario,"sum")
  temp3$RoutingType <- "Both"
  temp3$Waypoint <- "All"
  temp4 <- aggregate(data = table.TrackMiles, TrackMiles~Scenario,"sum")
  temp4$RoutingType <- "Both"
  temp4$Waypoint <- "All"
  temp4$Airport <- "All"
  table.TrackMiles <- rbind(table.TrackMiles,temp1,temp2,temp3,temp4)
  
  t1 <- table.current.Conflicts
  t1$Scenario <- "Current"
  t2 <- table.PBN.Conflicts
  t2$Scenario <- "PBN"
  table.Conflicts <- rbind(t1,t2)
  table.Conflicts <- table.Conflicts[!(names(table.Conflicts) %in% c("Start_Day","Closest_Day","End_Day","2DLocation"))]
  
  table.ConflictType <- count(table.Conflicts, vars = c("Sector","ConflictType","Scenario"))
  temp1 <- aggregate(data = table.ConflictType,freq~ConflictType+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.ConflictType, Sector %in% list.sectors),freq~ConflictType+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.ConflictType <- rbind(table.ConflictType, temp1, temp2)
  table.ConflictType$ConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.ConflictType$ConflictType)
  
  table.LateralConflictType <- count(table.Conflicts, vars = c("Sector","LateralConflictType","Scenario"))
  temp1 <- aggregate(data = table.LateralConflictType,freq~LateralConflictType+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.LateralConflictType, Sector %in% list.sectors),freq~LateralConflictType+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.LateralConflictType <- rbind(table.LateralConflictType, temp1, temp2)
  table.LateralConflictType$LateralConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.LateralConflictType$LateralConflictType)
  
  table.VerticalConflictType <- count(table.Conflicts, vars = c("Sector","VerticalConflictType","Scenario"))
  temp1 <- aggregate(data = table.VerticalConflictType,freq~VerticalConflictType+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.VerticalConflictType, Sector %in% list.sectors),freq~VerticalConflictType+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.VerticalConflictType <- rbind(table.VerticalConflictType, temp1, temp2)
  table.VerticalConflictType$VerticalConflictType <- gsub("([a-z])([A-Z])", "\\1 \\2", table.VerticalConflictType$VerticalConflictType)
  
  t1 <- table.current.ConflictsFlightPlanPhase
  t1$Scenario <- "Current"
  t2 <- table.PBN.ConflictsFlightPlanPhase
  t2$Scenario <- "PBN"
  table.ConflictsFlightPlanPhase <- rbind(t1,t2)
  temp1 <- aggregate(data = table.ConflictsFlightPlanPhase,Count~FlightPlanPhases+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.ConflictsFlightPlanPhase, Sector %in% list.sectors),Count~FlightPlanPhases+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.ConflictsFlightPlanPhase <- rbind(table.ConflictsFlightPlanPhase, temp1, temp2)
  temp3 <- strsplit(table.ConflictsFlightPlanPhase$FlightPlanPhases, " ")
  for (i in 1:length(temp3)) {
    if (unlist(temp3[i])[1] == unlist(temp3[i])[2]) {
      temp3[i] <- paste("Both",gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp3[i])[1]))
    } else {
      temp3[i] <- paste(gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp3[i])[1]),"and",gsub("([a-z])([A-Z])","\\1 \\2",unlist(temp3[i])[2]))
    }
  }
  table.ConflictsFlightPlanPhase$FlightPlanPhases <- unlist(temp3)
  
  table.Severity <- count(table.Conflicts, vars = c("Sector","Severity","Scenario"))
  temp1 <- aggregate(data = table.Severity,freq~Severity+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.Severity, Sector %in% list.sectors),freq~Severity+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.Severity <- rbind(table.Severity, temp1, temp2)
  
  table.VerticalSeverity <- count(table.Conflicts, vars = c("Sector","VerticalSeverity","Scenario"))
  temp1 <- aggregate(data = table.VerticalSeverity,freq~VerticalSeverity+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.VerticalSeverity, Sector %in% list.sectors),freq~VerticalSeverity+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.VerticalSeverity <- rbind(table.VerticalSeverity, temp1, temp2)
  
  t1 <- table.current.SectorOccupancy
  t1$Scenario <- "Current"
  t2 <- table.PBN.SectorOccupancy
  t2$Scenario <- "PBN"
  table.SectorOccupancy <- rbind(t1,t2)
  temp1 <- aggregate(data = table.SectorOccupancy,Count~Time+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.SectorOccupancy, Sector %in% list.sectors),Count~Time+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.SectorOccupancy <- rbind(table.SectorOccupancy,temp1,temp2)
  
  t1 <- table.current.SectorEntry
  t1$Scenario <- "Current"
  t2 <- table.PBN.SectorEntry
  t2$Scenario <- "PBN"
  table.SectorEntry <- rbind(t1,t2)
  temp1 <- aggregate(data = table.SectorEntry,Entries~Time+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.SectorEntry, Sector %in% list.sectors),Entries~Time+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.SectorEntry <- rbind(table.SectorEntry,temp1,temp2)
  
  t1 <- table.current.Workload
  t1$Scenario <- "Current"
  t2 <- table.PBN.Workload
  t2$Scenario <- "PBN"
  table.Workload <- rbind(t1,t2)
  temp1 <- aggregate(data = table.Workload,PercentHourlyWorkload~Time+Scenario,"sum")
  temp1$Sector <- "All Sectors"
  temp2 <- aggregate(data = subset(table.Workload, Sector %in% list.sectors),PercentHourlyWorkload~Time+Scenario,"sum")
  temp2$Sector <- "All TMA"
  table.Workload <- rbind(table.Workload,temp1,temp2)
  
  write.csv(table.SectorPolygons, file="~/Croatia v3.0/data/SectorPolygons.csv", row.names=F)
  write.csv(table.TotalThroughputs, file="~/Croatia v3.0/data/TotalThroughputs.csv", row.names=F)
  write.csv(table.HourlyThroughputs, file="~/Croatia v3.0/data/HourlyThroughputs.csv", row.names=F)
  write.csv(table.RollingHourlyThroughputs, file="~/Croatia v3.0/data/RollingHourlyThroughputs.csv", row.names=F)
  write.csv(table.FuelBurn, file="~/Croatia v3.0/data/FuelBurn.csv", row.names=F)
  write.csv(table.TrackMiles, file="~/Croatia v3.0/data/TrackMiles.csv", row.names=F)
  write.csv(table.Conflicts, file="~/Croatia v3.0/data/Conflicts.csv", row.names=F)
  write.csv(table.ConflictType, file="~/Croatia v3.0/data/ConflictType.csv", row.names=F)
  write.csv(table.LateralConflictType, file="~/Croatia v3.0/data/LateralConflictType.csv", row.names=F)
  write.csv(table.VerticalConflictType, file="~/Croatia v3.0/data/VerticalConflictType.csv", row.names=F)
  write.csv(table.ConflictsFlightPlanPhase, file="~/Croatia v3.0/data/ConflictsFlightPlanPhase.csv", row.names=F)
  write.csv(table.Severity, file="~/Croatia v3.0/data/Severity.csv", row.names=F)
  write.csv(table.VerticalSeverity, file="~/Croatia v3.0/data/VerticalSeverity.csv", row.names=F)
  write.csv(table.SectorOccupancy, file="~/Croatia v3.0/data/SectorOccupancy.csv", row.names=F)
  write.csv(table.SectorEntry, file="~/Croatia v3.0/data/SectorEntry.csv", row.names=F)
  write.csv(table.Workload, file="~/Croatia v3.0/data/Workload.csv", row.names=F)
}

table.SectorPolygons <- read.csv("~/Croatia v3.0/data/SectorPolygons.csv", stringsAsFactors = FALSE)
table.TotalThroughputs <- read.csv("~/Croatia v3.0/data/TotalThroughputs.csv", stringsAsFactors = FALSE)
table.HourlyThroughputs <- read.csv("~/Croatia v3.0/data/HourlyThroughputs.csv", stringsAsFactors = FALSE)
table.RollingHourlyThroughputs <- read.csv("~/Croatia v3.0/data/RollingHourlyThroughputs.csv", stringsAsFactors = FALSE)
table.FuelBurn <- read.csv("~/Croatia v3.0/data/FuelBurn.csv", stringsAsFactors = FALSE)
table.TrackMiles <- read.csv("~/Croatia v3.0/data/TrackMiles.csv", stringsAsFactors = FALSE)
table.Conflicts <- read.csv("~/Croatia v3.0/data/Conflicts.csv", stringsAsFactors = FALSE)
table.ConflictType <- read.csv("~/Croatia v3.0/data/ConflictType.csv", stringsAsFactors = FALSE)
table.LateralConflictType <- read.csv("~/Croatia v3.0/data/LateralConflictType.csv", stringsAsFactors = FALSE)
table.VerticalConflictType <- read.csv("~/Croatia v3.0/data/VerticalConflictType.csv", stringsAsFactors = FALSE)
table.ConflictsFlightPlanPhase <- read.csv("~/Croatia v3.0/data/ConflictsFlightPlanPhase.csv", stringsAsFactors = FALSE)
table.Severity <- read.csv("~/Croatia v3.0/data/Severity.csv", stringsAsFactors = FALSE)
table.VerticalSeverity <- read.csv("~/Croatia v3.0/data/VerticalSeverity.csv", stringsAsFactors = FALSE)
table.SectorOccupancy <- read.csv("~/Croatia v3.0/data/SectorOccupancy.csv", stringsAsFactors = FALSE)
table.SectorEntry <- read.csv("~/Croatia v3.0/data/SectorEntry.csv", stringsAsFactors = FALSE)
table.Workload <- read.csv("~/Croatia v3.0/data/Workload.csv", stringsAsFactors = FALSE)
