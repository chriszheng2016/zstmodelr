
# Load stock from datasource ---------------------------------------------

# Open odbc connection with GTA database
con_gta <- RODBC::odbcConnect(dsn = "GTA_SQLData")
db_tables <- RODBC::sqlTables(con_gta, tableType = "TABLE")

# Fetech datasets from datatables
ds_trd_company.df <- RODBC::sqlFetch(con_gta, "TRD_Co_公司基本情况")
colnames(ds_trd_company.df) <- tolower(colnames(ds_trd_company.df))

ds_trd_mnth.df <- RODBC::sqlFetch(con_gta, "TRD_Mnth_月个股回报率")
colnames(ds_trd_mnth.df) <- tolower(colnames(ds_trd_mnth.df))

ds_fr_t1_debt.df <- RODBC::sqlFetch(con_gta, "FR_T1_偿债能力")
colnames(ds_fr_t1_debt.df) <- tolower(colnames(ds_fr_t1_debt.df))


#获取数据表的字段字典
gta_indicator.df <- read.csv("R/gta_相关指标.csv", stringsAsFactors = FALSE)
row.names(gta_indicator.df) <- tolower(gta_indicator.df[['参数字段']])

# Get individual stock monthly return -------------------------------------

#提取个股数据
ds_600066.df <- na.omit(ds_trd_mnth.df[ds_trd_mnth.df$stkcd == 600066,])
dim(ds_600066.df)
head(ds_600066.df)

#收益数据
#a. df --> timeSeries --> xts########
#timeSereis time Sereis
ds_600066_mretwd.fts <- timeSeries::timeSeries(ds_600066.df['mretwd'], zoo::as.Date(zoo::as.yearmon(ds_600066.df$trdmnt)), units = "600066")
head(ds_600066_mretwd.fts)

#xts time Series
ds_600066_mretwd.xts <- xts::as.xts(ds_600066_mretwd.fts,.RECLASS = T)
class(xts::Reclass(ds_600066_mretwd.xts))
head(ds_600066_mretwd.xts)

#b. df --> xts --> timeSeries########
#xts time Series
ds_600066_mretwd.xts <- xts::as.xts(ds_600066.df['mretwd'], zoo::as.yearmon(ds_600066.df$trdmnt))
head(ds_600066_mretwd.xts)

#timeSereis time Sereis
ds_600066_mretwd.fts <- timeSeries::as.timeSeries(xts::convertIndex(ds_600066_mretwd.xts, "timeDate"), units = "600066")
head(ds_600066_mretwd.fts)

# Test get_stock_data
ds_stock_mretnd.fts <- get_stock_dataset(ds_source.df = ds_trd_mnth.df, stock_cd = 600066, target_field = "mretnd", date_field = "trdmnt")

# Test fetch_stocks_data
stock_cd_list <- c(600066,000550, 600031, 000157,000651, 000333)
ds_stocks_mretnd.fts <- fetch_stock_dataset(ds_source.df = ds_trd_mnth.df, stock_cd_list = stock_cd_list, target_field = "mretnd", date_field = "trdmnt")


