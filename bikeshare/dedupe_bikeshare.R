library(tidyverse)
library(RMySQL)
library(zoo)
library(lubridate)
library(stringr)

conn <- dbConnect(MySQL(), 
                  user = 'rcharlie',
                  password = 'charlie86', 
                  dbname = 'rcharlie',
                  host = "rcharlie.ch74fm7hgclb.us-west-2.rds.amazonaws.com")

dbSendQuery(conn, "DROP TABLE IF EXISTS bikeshare_station_lookup;")
dbSendQuery(conn, "
            CREATE TABLE bikeshare_station_lookup (
            station_id INT,
            station_name VARCHAR(50),
            lat FLOAT,
            long FLOAT,
            neighborhood VARCHAR,
            quadrant VARCHAR);")

dbSendQuery(conn, "
            INSERT INTO bikeshare_station_lookup
            SELECT 
            `terminalName`
            , `name`
            , `lat`
            , `long`
            , max(`latestUpdateTime`)
            , max(`lastCommWithServer`)
            FROM bikeshare_stations 
            GROUP BY 1,2,3,4;")

dbSendQuery(conn, "DROP TABLE IF EXISTS bikeshare_deduped;")
dbSendQuery(conn, "
            CREATE TABLE bikeshare_deduped (
            station_id INT
            , bikes INT
            , docks INT
            , last_updated TIMESTAMP);")
dbSendQuery(conn, "CREATE INDEX station_id ON bikeshare_deduped (station_id);")

dbSendQuery(conn, "
            INSERT INTO bikeshare_deduped 
            SELECT 
            DISTINCT `terminalName` AS station_id
            , `nbBikes` AS bikes
            , `nbEmptyDocks` AS docks
            , `latestUpdateTime` AS last_updated
            FROM bikeshare_stations;")