
# Load GTA Stock Database -------------------------------------------------

# open gta stock databse
stock_db <- stock_db(gta_db, "GTA_SQLData")
open_stock_db(stock_db)
init_stock_db(stock_db)
stock_cd_list <- c(600066,000550, 600031, 000157,000651, 000333)
(db_tables <- list_stock_tables(stock_db))


# load one table from stock db
fr_t1_debt.df <- get_table_dataset(stock_db, table_name = "FR_T1_偿债能力")

# load all tables from stock db
#result_table_list <- fetch_table_dataset(stock_db, table_list = db_tables[c(6,7)])

# stock_field_list
stock_field_list <- stock_field_list(stock_db)
(name2code(stock_field_list, "资产报酬率A"))
(code2name(stock_field_list, "f050101b"))

# stock_name_list
stock_name_list <- stock_name_list(stock_db)
(name2code(stock_name_list, "宇通客车"))
(code2name(stock_name_list, "600066"))

# translation between code and name
(name2code(stock_db, "资产报酬率A", type = "field"))
(code2name(stock_db, "f050101b", type = "field"))
(name2code(stock_db, "宇通客车", type = "stock"))
(code2name(stock_db, "600066", type = "stock"))
(stock_name_list <- code2name(stock_db, stock_cd_list, type = "stock"))
(stock_cd_list   <- name2code(stock_db, stock_name_list, type = "stock"))



# Explore dataset: ds_trd_mnth.df -----------------------------------------
# ds_trd_mnth.df <- get("ds_TRD_Mnth_月个股回报率.df")
ds_trd_mnth.df <- get_table_dataset(stock_db, table_name = "TRD_Mnth_月个股回报率")

ds_trd_dalyr.df <- get_stock_dataset(stock_db, table_name = "TRD_Dalyr_日个股回报率", stock_cd_list)


# Test get_stock_data
ds_stock_mretnd.fts <- get_stock_field_dataset(ds_source.df = ds_trd_mnth.df, stock_cd = 600066, target_field = "mretnd", date_field = "trdmnt")
ds_stock_dretwd.fts <- get_stock_field_dataset(ds_source.df = ds_trd_dalyr.df, stock_cd = 600066, target_field = "dretwd", date_field = "trddt")


# Test fetch_stocks_data
ds_stocks_mretnd.fts <- fetch_stock_field_dataset(ds_source.df = ds_trd_mnth.df, stock_cd_list = stock_cd_list, replaceNA = "zeros",target_field = "mretnd", date_field = "trdmnt")

ds_stocks_dretwd.fts <- fetch_stock_field_dataset(ds_source.df = ds_trd_dalyr.df, stock_cd_list = stock_cd_list, replaceNA = "zeros",target_field = "dretwd", date_field = "trddt")



# Plot data for return series
old_par <- par(mfrow = c(3,2))

# plots for stock_stock_mrewnd
fBasics::cumulatedPlot(ds_stocks_mretnd.fts)
fBasics::drawdownPlot(ds_stocks_mretnd.fts)

par(old_par)




# Explore dataset: ds_fr_t5_profitable.df -----------------------------------------
#ds_fr_t5_profitable.df <- get("ds_FR_T5_盈利能力.df")
ds_fr_t5_profitable.df <- get_table_dataset(stock_db, table_name = "FR_T5_盈利能力")

ds_stocks_f050101b.fts <- fetch_stock_dataset(ds_source.df = ds_fr_t5_profitable.df, stock_cd_list = stock_cd_list, target_field = "f050101b", date_field = "accper")


# close gta stock databse
close_stock_db(stock_db)
