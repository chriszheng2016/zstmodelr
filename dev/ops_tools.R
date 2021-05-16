# When you need to update database and produce indicator periodically,
# here are some notes for your reference.

# Update stock database ----

## Prepare raw data from GTA(SQL_DATA/Data/Origin/)

# 1.Download raw data files from GTA into “Download”, please see
# "Download_Files_Notes(Current)" in "SQL_Data/Data/Source/Guide/Specification/
# 2.Backup old raw files in "SQL_DATA/Data/Source/Origin/" into "Backup", and remove
# all files except "gta_fieldname.csv".
# 3.Unzip new raw data file into "SQL_DATA/Data/Origin".

## Use "Microsoft SQL Server Management Stdio" to backup old database

# 1.Detach GTA_SQLDATA.
# 2.Compress "GTA_SQLDATA.mdf" and "GTA_SQLDATA_log.ldf" into a zip file.
# 3.Save zip file into "./../Data_Archive" like "FY2020Q1(2020-9-11)",in which
#  FY2020Q1 means last date of report date of data.

## Use data-manager.R to update/process/clean stock database.

# 1.Load data-manager.R.
source("tools/data-manager/data-manager.R")

# 2.Process data before importing into database
# Process all input_files for importing by default
data_manager(dsn = "GTA_SQLData", action = "process")
# Retry process input_files with logged errors
data_manager(dsn = "GTA_SQLData", action = "process", retry_error = TRUE)

# 3.Update data in database with proceed files.
# Update all tables by default
data_manager(dsn = "GTA_SQLData", action = "update")
# Retry to update tables when countering errors in updating
data_manager(dsn = "GTA_SQLData", action = "update", retry_error = TRUE)

# 4.Clear data in database if needed.
data_manager(dsn = "GTA_SQLData", action = "clear")

# Update indicators data ----

## Use indicator-producer.R to backup/clean/produce indicators

# 1. Load indicator-producer.R
source("tools/indicator-producer/indicator-producer.R")

# 2.Archive all indicators files by default
indicator_producer(action = "archive")

# 3.Clear all indicators files by default
indicator_producer(action = "clear")

# 4.Produce all indicators in parallel process(Production)
indicator_producer(action = "produce", parallel = TRUE)

