library(tidyverse)
#library(data.table)
#library(modelr)
#library(purrr)
library(tidymodels)
#library(rsample)
library(bigrquery) # used for querying BigQuery
#library(ggplot2) # used for visualization
#library(dplyr) # used for data wrangling
#library(parsnip)
#library(randomForest)
#library(GGally)
#library(vtreat)
#library(rpart.plot)  # for visualizing a decision tree
library(vip)         # for variable importance plots
library(DALEXtra)
#library(party) 
library(qeML)
library(tibble)
#library(janitor)
library(devtools)
#library(visdat)
#library(reshape2)
#library(regtools)
#library(psych)
#library(keras)
library(corrplot)
#library(hexbin)
#library(umap)
#library(h20)
PROJECT_ID <-  "gcp-wow-finance-faa-dev"
billing <- "gcp-wow-finance-faa-dev"
con <- dbConnect(
  bigrquery::bigquery(),
  project = PROJECT_ID,
  dataset = "zHagenTest",
  billing = billing
)
con
gcp-wow-finance-de-lab-dev.360_smkt_incremental_sales.instore_incremental_sales

sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.360_smkt_incremental_sales.instore_incremental_sales`"
tb <- bq_project_query(billing, sql)
instore_incremental_sales <- bq_table_download(tb)

install_github('https://github.com/matloff/qeML')
options(na.action = na.warn)
tidymodels_prefer()

str(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_model)
sapply(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_model, function(x) length(unique(x)))
summary(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_model)
Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor <- Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_model %>% mutate_if(is.character, as.factor)
summary(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor)

#install.packages("janitor")
#install.packages("tibble")


Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor$Launch_Date <-   excel_numeric_to_date(as.numeric(as.character( Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor$RENEWAL_DATE)), date_system = "modern")

prior_summary <- Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor %>%
group_by(Site) %>%
mutate(time_since_launch = difftime(Week_Ending, Launch_Date, unit = 'weeks'))


str(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor)
ggplot(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor, aes(x = Incremental_Rate)) +
geom_histogram(bins = 50, col= "white")

ggplot(,aes(x=year, y= Incremental_sales, color = MULTICULTURAL_TAG)) +
geom_point(alpha=0.5)+ geom_line(size = 2) +
theme_ipsum() +
theme(legend.position="right") +
xlab("Year") + ggtitle("Renewal B&M program") +
ylab("incremental sales [%]")
set.seed(1801)


ggplot(historical_incremental_wide_clean_model_copy , aes(x= time_since_launch, y= incremental_rate, color = fiscal_year)) +
 geom_smooth() +
xlab("Year") + ggtitle("Renewal B&M program") +
ylab("incremental sales [%]")
set.seed(1801)




since_launch_incremental_sales <- prior_summary |> group_by(time_since_launch) |>
summarise(incremental_sales_weekly = mean(Incremental_Rate, na.rm = TRUE))

#esl_since_launch_incremental_rate  <- esl_summary |> filter( time_to_launch > 0) |> group_by(time_to_launch, ESL_launch) |>
#summarise(incremental_rate_weekly = mean(Incremental_Rate, na.rm = TRUE))
names(since_launch_incremental_sales)[1] <- "time"
#names(esl_since_launch_incremental_rate)[1] <- "time"
ggplot(since_launch_incremental_sales, aes(time, incremental_sales_weekly))+ geom_point() +  geom_smooth() + ggtitle( "FY 21/22/23")  + xlab('Weeks since launch')

#FY24/25 incremental rate %

`gcp-wow-finance-de-lab-dev.360_smkt_incremental_sales.instore_incremental_sales`
#FY 21/25 incremental sales $
`gcp-wow-ent-im-tbl-prod.adp_equipmentsupplychain_view.instore_incremental_sales_budget_v` 

sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.360_smkt_incremental_sales.instore_incremental_sales`"
tb <- bq_project_query(billing, sql)
instore_incremental_sales <-  bq_table_download(tb)

sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.360_smkt.smkt_incremental_sales_f24`"
tb <- bq_project_query(billing, sql)
instore_incremental_sales_f24 <-  bq_table_download(tb)

sapply(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_model, function(x) length(unique(x)))
#Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor <- Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate %>% mutate_if(is.character, as.factor)


sapply(instore_incremental_sales, function(x) length(unique(x)))
instore_incremental_sales_factor <- instore_incremental_sales %>% mutate_if(is.character, as.factor)
instore_incremental_sales_factor_renewals <- subset(instore_incremental_sales_factor, DType == "Renewal")

instore_incremental_sales_factor_f24 <- instore_incremental_sales_f24 %>% mutate_if(is.character, as.factor)
instore_incremental_sales_factor_renewals_f24 <- subset(instore_incremental_sales_factor_f24, Dev_Type == "Renewal")

ggplot(instore_incremental_sales_factor_renewals, aes(x = Incremental_Rate)) +
geom_histogram(bins = 50, col= "white")

instore_incremental_sales_factor_renewals_model <- instore_incremental_sales_factor_renewals |> select("Site", "Launch_Week", "Incremental_Rate", "Fiscal_Week", "Fiscal_Year")
instore_incremental_sales_factor_renewals_model_f24 <- instore_incremental_sales_factor_renewals_f24 |> select("Site", "Launch_Date", "incremental_sales_growth", "Fiscal_Week")

Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate

calendar_full$Week_Ending <- as.Date(calendar_full$Fiscal_End_Date, format = "%d/%m/%Y")

my_calendar <- calendar_full[c(3,9,13,14,17)]
my_calendar <- unique(my_calendar)

Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1 <- merge(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor, my_calendar, by.x = "Week_Ending", by.y = "Week_Ending", all.x = TRUE)
Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1_ <- Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1 |> select("Site",  "Incremental_Rate", "Fiscal_Week_Year", "Week_Ending", "Easter_Flag", "Xmas_Flag","Launch_Date", "Fiscal_Year")

my_calendar <- calendar_full[c(3,13,14,17)]
my_calendar <- unique(my_calendar)
instore_incremental_sales_factor_renewals_model_1 <- merge(instore_incremental_sales_factor_renewals_model,my_calendar, by.x = "Fiscal_Week", by.y = "Fiscal_Week_Year", all.x = TRUE)
instore_incremental_sales_factor_renewals_model_1_f24 <- merge(instore_incremental_sales_factor_renewals_model_f24,my_calendar, by.x = "Fiscal_Week", by.y = "Fiscal_Week_Year", all.x = TRUE)
names(instore_incremental_sales_factor_renewals_model_1_f24)[4] <- "incremental_rate"

instore_incremental_sales_factor_renewals_model_1_f24$time_since_launch <- as.numeric(instore_incremental_sales_factor_renewals_model_1_f24$time_since_launch)

#instore_incremental_sales_factor_renewals_model_1_ <- instore_incremental_sales_factor_renewals_model_1 |> select("Site",  "Incremental_Rate", "Fiscal_Week", "Week_Ending", "Easter_Flag", "Xmas_Flag", "Launch_Week", "Fiscal_Year")
#names(instore_incremental_sales_factor_renewals_model_1)[8] <- "Fiscal_Week_Ending"
my_calendar <- calendar_full[c(3,17)]
my_calendar <- unique(my_calendar)
instore_incremental_sales_factor_renewals_model_1_ <- merge(instore_incremental_sales_factor_renewals_model_1, my_calendar, by.x = "Launch_Week", by.y = "Fiscal_Week_Year", all.x = TRUE)
#instore_incremental_sales_factor_renewals_model_1_ <- instore_incremental_sales_factor_renewals_model_1_ |> select("Site",  "Incremental_Rate", "Fiscal_Week", "Week_Ending", "Easter_Flag", "Xmas_Flag", "Date", "Fiscal_Year")
names(instore_incremental_sales_factor_renewals_model_1_)[9] <- "Launch_Date"



write.csv(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1_, file = "Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate.csv")

write.csv(instore_incremental_sales_factor_renewals_model_1_f24, file = "instore_incremental_sales_f24.csv")

write.csv(instore_incremental_sales_factor_renewals_model_1_, file = "instore_incremental_sales.csv")

names(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1_)[3] <- "Fiscal_Week"

Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1_$Fiscal_Week <- as.character(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1_$Fiscal_Week)
#instore_incremental_sales_factor_renewals_model_1_ <-  na.omit(instore_incremental_sales_factor_renewals_model_1_)
#names(instore_incremental_sales_factor_renewals_model_1_)[7] <- "Launch_Date"
instore_incremental_sales_factor_renewals_model_1_$Launch_Week <- NULL

names(instore_incremental_sales_factor_renewals_model_1_)[7] <- "Week_Ending"


historical_incremental <- rbind(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_1_, instore_incremental_sales_factor_renewals_model_1_)

instore_incremental_sales_factor_renewals_model_1_f24$Fiscal_Year <- as.character(instore_incremental_sales_factor_renewals_model_1_f24$Fiscal_Year)
historical_incremental_no_fy24 <- subset(historical_incremental, Week_Ending <= '2023-07-01' | Week_Ending > '2024-07-01')
instore_incremental_sales_factor_renewals_model_1_f24 <- instore_incremental_sales_factor_renewals_model_1_f24 %>% mutate(time_since_launch = difftime(Week_Ending, Launch_Date, unit = 'weeks'))
instore_incremental_sales_factor_renewals_model_1_f24$time_since_launch <- as.numeric(instore_incremental_sales_factor_renewals_model_1_f24$time_since_launch)
instore_incremental_sales_factor_renewals_model_1_f24 <- instore_incremental_sales_factor_renewals_model_1_f24[!is.na(instore_incremental_sales_factor_renewals_model_1_f24$incremental_rate),]

historical_incremental_fy24 <- subset(historical_incremental, Week_Ending >= '2023-07-01' & Week_Ending < '2024-07-01')
historical_incremental_fy24 <- historical_incremental_fy24 %>% mutate(time_since_launch = difftime(Week_Ending, Launch_Date, unit = 'weeks'))

historical_incremental_fy24$time_since_launch <- as.numeric(historical_incremental_fy24$time_since_launch)

historical_incremental_fy24 <-  janitor::clean_names(historical_incremental_fy24)
instore_incremental_sales_factor_renewals_model_1_f24 <- janitor::clean_names(instore_incremental_sales_factor_renewals_model_1_f24)
setdiff(historical_incremental_fy24,instore_incremental_sales_factor_renewals_model_1_f24)

write.csv(historical_incremental, "historical_incremental.csv")

prior_summary <- instore_incremental_sales_factor_renewals_model_1_f24 %>%
group_by(Site, Fiscal_Year) %>%
mutate(time_since_launch = difftime(Week_Ending, Launch_Date, unit = 'weeks'))

since_launch_incremental_sales <- prior_summary |> group_by(time_since_launch, Fiscal_Year) |>
summarise(incremental_sales_weekly = mean(incremental_rate, na.rm = TRUE))




since_launch_incremental_sales <- prior_summary |> group_by(time_since_launch, Fiscal_Year) |>
summarise(incremental_sales_weekly = mean(Incremental_Rate, na.rm = TRUE))


names(since_launch_incremental_sales)[1] <- "time"
since_launch_incremental_sales$time <- base::as.numeric(since_launch_incremental_sales$time)
#names(esl_since_launch_incremental_rate)[1] <- "time"
ggplot(since_launch_incremental_sales, aes(time, incremental_sales_weekly))+ geom_point() +  geom_smooth() + ggtitle( "FY21-25")  + xlab('Weeks since launch')
ggplot(since_launch_incremental_sales, aes(time, incremental_sales_weekly, color = Fiscal_Year))+ geom_point() +  geom_smooth() + ggtitle( "FY21-25")  + xlab('Weeks since launch')


WOW_Store_SA1$int64_field_0 <- NULL

historical_incremental <- merge(historical_incremental, WOW_Store_SA1,  by.x = "Site", by.y = "Loc_No", all.x = TRUE)

#sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.061_FND.store_attributes_hagen`"


sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.360_smkt.smkt_store_attributes`"

tb <- bq_project_query(billing, sql)
smkt_store_attributes <-  bq_table_download(tb)


historical_incremental <- merge(historical_incremental, smkt_store_attributes,  by.x = "Site", by.y = "Location_No", all.x = TRUE)
#historical_incremental_new <- merge(historical_incremental_new, smkt_store_attributes,  by.x = "Site", by.y = "Location_No", all.x = TRUE)



sql <- "SELECT * FROM `gcp-wow-corp-smr-dev.dev_workarea.combined_data`"
tb <- bq_project_query(billing, sql)
combined_data <-  bq_table_download(tb)

#install.packages('devtools')


historical_incremental$SA1_2021 <- NULL

historical_incremental <- historical_incremental[c(1:24,26,39:78,89)]
historical_incremental$Centre_Type.y <- NULL
#historical_incremental$ESL_Status <- NULL


historical_incremental <- historical_incremental |> drop_na(Incremental_Rate)

sql <- "SELECT * FROM `gcp-wow-corp-smr-dev.dev_workarea.combined_data`"
tb <- bq_project_query(billing, sql)
combined_data <-  bq_table_download(tb)

combined_data$store_id_x
combined_data$store_id_y

combined_data_model <- combined_data[c(4,27,28, 32, 33,34, 38, 43:45,87:114, 119:142)]
sapply(combined_data_model, function(x) length(unique(x)))



historical_incremental_model <- historical_incremental[c(1:15,20:65)]
sapply(historical_incremental_model, function(x) length(unique(x)))

dups <- combined_data_model %>% group_by(store_id) %>% filter(n() > 1)

combined_data_model <- combined_data_model[!duplicated(combined_data_model$store_id),]

sum(is.na(historical_incremental_model))
sum(is.na(combined_data_model))
historical_incremental_wide <- merge(historical_incremental_model, combined_data_model,  by.x = "Site", by.y = "store_id", all.x = TRUE)
dim(historical_incremental_wide)
sum(is.na(historical_incremental_wide))


# we go down from 13.5k rows to 9.5k.
#cchistorical_incremental <- which(complete.cases(historical_incremental_wide))
#historical_incremental_wide <- historical_incremental_wide[cchistorical_incremental,]

#what about removing only rows with na and constant columns
cchistorical_incremental <- which(complete.cases(historical_incremental_wide_cc))
historical_incremental_wide_cc1 <- historical_incremental_wide_cc[cchistorical_incremental,]

historical_incremental_wide_cc <- historical_incremental_wide
historical_incremental_wide_cc$FULL_SHOP <- NULL
historical_incremental_wide_cc$secondary_access_road___intersection_to_carpark_type <- NULL
historical_incremental_wide_cc$secondary_access_road_type <- NULL
historical_incremental_wide_cc$main_access_road_____of_lanes_in <- NULL
historical_incremental_wide_cc$nrst_coles_trading_area <- NULL
historical_incremental_wide_cc$car_park_type <- NULL

historical_incremental_wide_cc <- historical_incremental_wide_cc |> drop_na(SuburbType)
historical_incremental_wide_cc <- historical_incremental_wide_cc |> drop_na(BUDGET)
historical_incremental_wide_cc <- historical_incremental_wide_cc |> drop_na(trading_area_sqm)
historical_incremental_wide_cc <- historical_incremental_wide_cc |> drop_na(car_park_levels)
historical_incremental_wide_cc <- historical_incremental_wide_cc |> drop_na(hours_of_free_parking)

write.csv(historical_incremental_wide_cc, "historical_incremental_wide_cc.csv")

historical_incremental_wide_cc_sample <- historical_incremental_wide_cc |> dplyr::slice_sample(n =1000)
vis_miss(historical_incremental_wide_cc_sample, cluster = TRUE)

#cchistorical_incremental <- which(complete.cases(historical_incremental_wide_cc))
#historical_incremental_wide_cc <- historical_incremental_wide_cc[cchistorical_incremental,]

historical_incremental_wide_clean_ecom_zircon_factor <-  historical_incremental_wide_clean_ecom_zircon %>% mutate_if(is.character, as.factor)

historical_incremental_wide_cc %>%
is.na() %>%
reshape2::melt() %>%
ggplot(aes(Var2, Var1, fill=value)) +
geom_raster() +
coord_flip() +
scale_y_continuous(NULL, expand = c(0, 0)) +
scale_fill_grey(name = "",
labels = c("Present",
"Missing")) +
xlab("Observation") +
theme(axis.text.y = element_text(size = 4))

historical_incremental_wide_clean <-  janitor::clean_names(historical_incremental_wide_cc)

summary(historical_incremental_wide_clean$age_since_being_built)

historical_incremental_wide_clean$difftime_age <- difftime(historical_incremental_wide_clean$date_opened, historical_incremental_wide_clean$launch_date, unit = "weeks")
historical_incremental_wide_clean$difftime_age <- as.numeric(historical_incremental_wide_clean$difftime_age)
historical_incremental_wide_clean$difftime_age <- historical_incremental_wide_clean$difftime_age/52
historical_incremental_wide_clean$age_since_being_built <- NULL
historical_incremental_wide_clean$time_since_launch <- as.numeric(historical_incremental_wide_clean$time_since_launch)

#sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.Formats.candice_store_attributes`"
#tb <- bq_project_query(billing, sql)
#candice_store_attributes <-  bq_table_download(tb)

#candice_store_attributes <- candice_store_attributes[c(1,2)]

#historical_incremental_wide_clean <- merge(historical_incremental_wide_clean, candice_store_attributes, by.x = "site", by.y = "store_num", all.x = TRUE) 
#historical_incremental_wide_clean_since <- historical_incremental_wide_clean %>%
#group_by(site) %>%
#mutate(time_since_launch = difftime(week_ending, launch_date, unit = 'weeks'))

#smkt_ecom_attributes_model <- subset(smkt_ecom_attributes, program == 'Renewal')

#smkt_ecom_attributes_model <- smkt_ecom_attributes_model[c(1, 80:84,98,99 )]
#smkt_ecom_attributes_model_sub <- smkt_ecom_attributes_model[!is.na(smkt_ecom_attributes_model$go_live_date),]

#historical_incremental_wide_clean_ecom <- merge(historical_incremental_wide_clean, smkt_ecom_attributes_model,  by.x = "site", by.y = "Location_No", all.x = TRUE)

# no ecom data, used for rfo
historical_incremental_wide_clean_noecom <- historical_incremental_wide_clean[c(1:122)]

write.csv(historical_incremental_wide_clean, "historical_incremental_wide_clean_since_model.csv")
historical_incremental_wide_clean$time_since_launch <- as.numeric(historical_incremental_wide_clean$time_since_launch)
bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.historical_incremental_wide_clean_model", historical_incremental_wide_clean_model,fields = as_bq_fields(historical_incremental_wide_clean)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')

bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.historical_incremental_wide_clean_model_copy", historical_incremental_wide_clean_model_copy,fields = as_bq_fields(historical_incremental_wide_clean_model_copy)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')


bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.instore_incremental_sales_factor_renewals_model_1_f24", instore_incremental_sales_factor_renewals_model_1_f24,fields = as_bq_fields(instore_incremental_sales_factor_renewals_model_1_f24)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')

bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model", WOW_Store_sample_model,fields = as_bq_fields(WOW_Store_sample_model)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')


sapply(historical_incremental_wide_clean, function(x) length(unique(x)))


zircon_insights_adjusted_dates_channel <- bquxjob_728e6391_1971a7f16de
zircon_insights_adjusted_dates_channel <- subset(zircon_insights_adjusted_dates_channel, DType == "Renewal" & Channel == "Online" & Renewal__Type == "Renewal - Standard Renewal" & DTB_Grading != "Pick Up - ISC")
zircon_model <- zircon_insights_adjusted_dates_channel[c(3,12,13)]
zircon_model <- zircon_model[!is.na(zircon_model$DTB_Pick_up_Offer),]

historical_incremental_wide_clean_ecom_zircon <- merge(historical_incremental_wide_clean, zircon_model,  by.x = "site", by.y = "Location_Number", all.x = TRUE)



#
#ecom data for 158 stores, can be used for 270 stores with incremental sales if dtb_offer equals 0 instead of Na??/
#cchistorical_incremental <- which(complete.cases(historical_incremental_wide_clean))
#historical_incremental_wide_clean <- historical_incremental_wide_clean[cchistorical_incremental,]
#dim(historical_incremental_wide_clean)
#[1] 9242  122

#can be done to get more accurate model and exclude covid
#historical_incremental_wide_clean_model <- historical_incremental_wide_clean[c(1:68,97:116 )]

#historical_incremental_wide_clean_model <- subset(historical_incremental_wide_clean, incremental_rate <= 0.90 & incremental_rate >= -0.70)

#historical_incremental_wide_clean[historical_incremental_wide_clean$ == , "incremental_rate"] <- 1
#historical_incremental_wide_clean_since[4736,2] <- 0.203
#covid flag to be implemented for high uplifts
Metro_COVID_Dashboard_Time_Frames_MasterList <- read_csv("~/Downloads/Metro COVID Dashboard Time Frames - MasterList.csv",
col_types = cols(Week = col_date(format = "%Y-%m-%d")))
covid_data <- subset(Metro_COVID_Dashboard_Time_Frames_MasterList, COVID_Period %in% c(  "Exclusion","OG Wave" ,   "OG Wave - 2 (VIC)" ,  "OG Wave - 3 (Sydney)" ,
"Delta Wave" ,   "Omicron Wave" ))
covid_data$State <- NULL
covid_data <- merge(covid_data, calendar_full, by.x = "Week", by.y = "Date", all.x = TRUE)
covid_data <- covid_data[c(1,2,18)]

historical_incremental_wide_clean_copy <- historical_incremental_wide_clean


historical_incremental_wide_clean <- historical_incremental_wide_clean_copy 

historical_incremental_wide_clean <- merge(historical_incremental_wide_clean,covid_data, by.x = "week_ending", by.y = "Week_Ending", all.x =  TRUE)

historical_incremental_wide_clean



historical_incremental_wide_clean_model <- historical_incremental_wide_clean





#outlier stores
historical_incremental_wide_clean_model <- historical_incremental_wide_clean_model |> filter(site != '3161')
historical_incremental_wide_clean_model <- historical_incremental_wide_clean_model |> filter(site != '3244')
#difftime doesn't work on this store???
historical_incremental_wide_clean_model <- historical_incremental_wide_clean_model |> filter(site != '7203')
historical_incremental_wide_clean_model <- historical_incremental_wide_clean_model |> filter(site != '3352')


historical_incremental_wide_clean_model["5337", "incremental_rate"] <- 0.04
historical_incremental_wide_clean_model["4715", "incremental_rate"] <- 0.19
historical_incremental_wide_clean_model["5618", "incremental_rate"] <- -0.03

#a <- historical_incremental_wide_clean_model |> filter(site == '2048') |> group_by(site) |> summarize(incr = mean(incremental_rate))
#a <- historical_incremental_wide_clean_model |> filter(site == '2541') |> group_by(site) |> summarize(incr = mean(incremental_rate))
#a <- historical_incremental_wide_clean_model |> filter(site == '2574') |> group_by(site) |> summarize(incr = mean(incremental_rate))
 historical_incremental_wide_clean_model |> filter(site == '2541') |> group_by(site) |> summarize(incr = mean(incremental_rate))
# A tibble: 1 × 2
#  site    incr
#  <chr>  <dbl>
#1 2541  0.0365
historical_incremental_wide_clean_model |> filter(site == '2048') |> group_by(site) |> summarize(incr = mean(incremental_rate))
# A tibble: 1 × 2
#  site   incr
#  <chr> <dbl>
#1 2048  0.189
historical_incremental_wide_clean_model |> filter(site == '2574') |> group_by(site) |> summarize(incr = mean(incremental_rate))
# A tibble: 1 × 2
#  site     incr
#  <chr>   <dbl>
#1 2574  -0.0279
# a = 0.014
#historical_incremental_wide_clean_since["5380","2"] <- 0.014

#historical_incremental_wide_clean_since <- subset(historical_incremental_wide_clean, incremental_rate <= 0.90 & incremental_rate >= -0.70)

#historical_incremental_wide_clean[historical_incremental_wide_clean$ == , "incremental_rate"] <- 1
#historical_incremental_wide_clean_since[4736,2] <- 0.203

#outlier stores
#historical_incremental_wide_clean_since <- historical_incremental_wide_clean_since |> filter(site != '3161')
#historical_incremental_wide_clean_since <- historical_incremental_wide_clean_since |> filter(site != '2574')
#historical_incremental_wide_clean_since$fiscal_year <- as.character(historical_incremental_wide_clean_since$fiscal_year)







historical_incremental_wide_clean_ecom_zircon <- merge(historical_incremental_wide_clean_model, zircon_model,  by.x = "site", by.y = "Location_Number", all.x = TRUE)
#historical_incremental_wide_clean_ecom_zircon_factor <-  historical_incremental_wide_clean_ecom_zircon %>% mutate_if(is.character, as.factor)

 
historical_incremental_wide_clean_ecom_zircon$ecom <-  ifelse(is.na(historical_incremental_wide_clean_ecom_zircon$DTB_Pick_up_Offer), 0, 1 ) 

historical_incremental_wide_clean_ecom_zircon[["DTB_Pick_up_Offer"]][is.na(historical_incremental_wide_clean_ecom_zircon[["DTB_Pick_up_Offer"]])] <- "none"
historical_incremental_wide_clean_ecom_zircon[["DTB_Grading"]][is.na(historical_incremental_wide_clean_ecom_zircon[["DTB_Grading"]])] <- "none"

ggplot(historical_incremental_wide_clean_model, aes(x = incremental_rate)) +
geom_histogram(bins = 50, col= "white")
#historical_incremental_wide_clean_ecom_zircon$DTB_Grading <- NULL
 #historical_incremental_wide_clean_ecom_zircon$DTB_Pick_up_Offer <- NULL
 
historical_incremental_wide_clean_model <-  historical_incremental_wide_clean_ecom_zircon
 
ggplot(historical_incremental_wide_clean_model, aes(time_since_launch, incremental_rate))+ geom_smooth() + ggtitle( "FY 21/22/23/24/25")  + xlab('Weeks since launch')

sapply(historical_incremental_wide_clean_model, function(x) length(unique(x)))
dim(historical_incremental_wide_clean_model)


historical_incremental_wide_clean_model$free_paid_parking <- NULL
historical_incremental_wide_clean_model$hours_of_free_parking <-NULL
historical_incremental_wide_clean_model$north_american_population_density <- NULL
historical_incremental_wide_clean_model$escalators <- NULL
historical_incremental_wide_clean_model$store_id_x <- NULL

historical_incremental_wide_clean_model_copy <- historical_incremental_wide_clean_model

historical_incremental_wide_clean_model <- merge(historical_incremental_wide_clean_model,covid_data, by.x = "week_ending", by.y = "Week_Ending", all.x =  TRUE)

historical_incremental_wide_clean_model$covid_flag <- ifelse(is.na(historical_incremental_wide_clean_model$COVID_Period), 0, 1)
historical_incremental_wide_clean_model$COVID_Period <- NULL
historical_incremental_wide_clean_model$Week <- NULL

write.csv(historical_incremental_wide_clean_model, "historical_incremental_wide_clean_model.csv")

historical_incremental_wide_clean_model_clean <- historical_incremental_wide_clean_model
historical_incremental_wide_clean_model_clean$week_ending <- NULL
historical_incremental_wide_clean_model_clean$launch_date <- NULL
historical_incremental_wide_clean_model_clean$name <- NULL
historical_incremental_wide_clean_model_clean$date_opened <- NULL

#historical_incremental_wide_clean_model_copy <- merge(historical_incremental_wide_clean_model_copy, instore_incremental_sales_factor_renewals_model_1_f24[c(1:2,4)],by.x  = c("fiscal_week", "site"),  by.y  = c("Fiscal_Week", "Site"), all = TRUE)
historical_incremental_wide_clean_model_fy24 <- subset(historical_incremental_wide_clean_model, week_ending >= '2023-07-01' & week_ending < '2024-07-01')
#all or all.x ???
historical_incremental_wide_clean_model_fy24 <- merge(historical_incremental_wide_clean_model_fy24, instore_incremental_sales_factor_renewals_model_1_f24[c(1:2,4)],by.x  = c("fiscal_week", "site"),  by.y  = c("fiscal_week", "site"), all  = TRUE)
historical_incremental_wide_clean_model_no_fy24 <- subset(historical_incremental_wide_clean_model, week_ending < '2023-07-01' | week_ending >= '2024-07-01')
historical_incremental_wide_clean_model_fy24 <- na.omit(historical_incremental_wide_clean_model_fy24)

#results <- setdiff(historical_incremental_wide_clean_model_copy, historical_incremental_wide_clean_model)
#results
#historical_incremental_wide_clean_model_fy24 <- cbind(historical_incremental_wide_clean_model_fy24[c(1,2,3,5:116)], 
#                                                              "incremental_rate" = with(historical_incremental_wide_clean_model_fy24, ifelse(is.na(incremental_rate.y), incremental_rate.x, incremental_rate.y)))

historical_incremental_wide_clean_model_fy24$incremental_rate.x <- NULL

names(historical_incremental_wide_clean_model_fy24)[115] <- "incremental_rate"


historical_incremental_wide_clean_model_fy24_subset <- subset(historical_incremental_wide_clean_model_fy24, site == '3101')

##incremental_rate.x in the last step would be better and preserve more rws, however featrue engineering is needed 
#historical_incremental_wide_clean_model_fy24  <- historical_incremental_wide_clean_model_fy24[!is.na(historical_incremental_wide_clean_model_fy24$incremental_rate),]

#tidymodels_prefer()
#historical_incremental_wide_clean_model_fy24 %>% step_impute_knn(all_predictors(), neighbors = 2)


historical_incremental_wide_clean_model_copy <- rbind(historical_incremental_wide_clean_model_no_fy24,historical_incremental_wide_clean_model_fy24)

write.csv(historical_incremental_wide_clean_model_copy, "historical_incremental_wide_clean_model_copy.csv")

earlier_renewals <- subset(historical_incremental_wide_clean_model, launch_date <= '2025-01-01')
later_renewals <- subset(historical_incremental_wide_clean_model, launch_date > '2025-01-01')

earlier_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')
later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')
    
             
set.seed(9999)
rfo3 <- qeRF(historical_incremental_wide_clean_model_copy,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_copy))))
rfo3$testAcc
rfo3$baseAcc
#rfo3$testAcc
#[1] 0.0281654
# rfo3$baseAcc
#[1] 0.061

saveRDS(historical_incremental_wide_clean_model, file = "historical_incremental_wide_clean_model.RData")
saveRDS(historical_incremental_wide_clean_model_copy, file = "historical_incremental_wide_clean_model_copy.RData")

varImpPlot(rfo3 , sort = TRUE , 10 , main = "Title" )

cmd<-"qeRF(historical_incremental_wide_clean_model,'incremental_rate',nTree=500,minNodeSize= 10,holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model))))$testAcc"
crossvalOutput<-replicMeans(10,cmd)

#11632
w <- subset(historical_incremental_wide_clean_model, site == "3101")
w$incremental_rate <- NULL
w[2] <- "202547"
w[3] <- "2025-05-25"



set.seed(9999)
rfo4 <- qeRF(earlier_renewals,'incremental_rate', nTree=500 , minNodeSize= 10, holdout = floor(min(1000,0.1*nrow(earlier_renewals))))
rfo4$testAcc
rfo4$baseAcc

# rfo4$testAcc
# [1] 0.03040636
# > rfo4$baseAcc
# [1] 0.0631191

varImpPlot(rfo4 , sort = TRUE , 20 , main = "Title" )

preds <- predict(rfo4, w)
w <- subset(historical_incremental_wide_clean_model, site == "3101")
a <- cbind (w ,preds)
TwoHistos <- ggplot(a) +
  labs(color="incremental_rate",x="Incremental_Rate",y="Count")+
  geom_histogram(aes(x=incremental_rate, fill= "incremental_rate"),  alpha = 0.2 ) + 
  geom_histogram(aes(x= a$preds, fill= "predicted_incremental_rate"), alpha = 0.2) + 
  scale_fill_manual(values = c("blue","red"))
TwoHistos

#w.ts <- a[,c(2,115)]
plot(a$incremental_rate, a$preds)

a$res <- a$`rfo3$predicted` - a$incremental_rate
plot(a$incremental_rate, a$res)

ggplot(a, aes(x = incremental_rate)) +
geom_histogram(bins = 50, col= "white")

ggplot(a, aes(x = a$`rfo3$predicted`)) +
geom_histogram(bins = 50, col= "white")

#rfo19 <- randomForest(incremental_rate, data = historical_incremental_wide_clean, importance = TRUE, na.action= na.roughfix)
#write.csv(historical_incremental_wide_clean_since, "historical_incremental_wide_clean_since.csv")




#dim(historical_incremental_wide_clean_since)
#length(rfo3$predicted)
a <- cbind(historical_incremental_wide_clean_model[1:11976,] ,rfo4$predicted)
TwoHistos <- ggplot(a) +
  labs(color="incremental_rate",x="Incremental_Rate",y="Count")+
  geom_histogram(aes(x=incremental_rate, fill= "incremental_rate"),  alpha = 0.2 ) + 
  geom_histogram(aes(x=rfo4$predicted, fill= "predicted_incremental_rate"), alpha = 0.2) + 
  scale_fill_manual(values = c("blue","red"))
TwoHistos


plot(a$incremental_rate, a$`rfo3$predicted`)

a$res <- a$`rfo4$predicted` - a$incremental_rate
plot(a$incremental_rate, a$res)

ggplot(a, aes(x = incremental_rate)) +
geom_histogram(bins = 50, col= "white")

ggplot(a, aes(x = a$`rfo3$predicted`)) +
geom_histogram(bins = 50, col= "white")




b <- rfo3$importance
mean(abs(rfo3$predicted))
hist(abs(rfo3$predicted))
hist((rfo3$predicted))
mean((rfo3$predicted))
summary(a$`rfo3$predicted`)
summary(a$incremental_rate)


mean(rfo3$rsq)
mean(rfo3$mse)

#MAPE
rfo3$trainAcc

##
set.seed(9999)
library(qeML)
set.seed(9999)

rfo13 <- qeRF(historical_incremental_wide_clean_ecom_zircon,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_ecom_zircon))))
rfo13$testAcc
rfo13$baseAcc

b <- rfo3$importance
set.seed(9999)
#historical_incremental_wide_clean_ecom_zircon_factor <- historical_incremental_wide_clean_ecom_zircon %>% mutate_if(is.character, as.factor)
cchistorical_incremental <- which(complete.cases(historical_incremental_wide_clean_ecom_zircon))
historical_incremental_wide_clean_ecom_zircon <- historical_incremental_wide_clean_ecom_zircon[cchistorical_incremental,]

set.seed(9999)

rfo5 <- qeRF(historical_incremental_wide_clean_ecom_zircon,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_ecom_zircon))))




rfo2 <- qeRF(historical_incremental_wide_clean_ecom,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental))))
#0.38
# 0.65



set.seed(9999)

rfo4 <- qeRF(historical_incremental_wide_clean,'incremental_rate',nTree=166,minNodeSize= 100,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean))))

set.seed(9999)
rfo6 <- qeRF(historical_incremental_wide_clean_since,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_since))))
rfo6$testAcc
rfo6$baseAcc
rfo6$testAcc
#[1] 0.02830788
#> rfo6$baseAcc
#[1] 0.06759892

#historical_incremental_wide_clean_since <- historical_incremental_wide_clean

#cchistorical_incremental <- which(complete.cases(historical_incremental_wide_clean_since))
#historical_incremental_wide_clean_since <- historical_incremental_wide_clean_since[cchistorical_incremental,]


#historical_incremental_wide_clean_since$fiscal_year <- as.character(historical_incremental_wide_clean_since$fiscal_year)
#historical_incremental_wide_clean_since_rfo7 <- historical_incremental_wide_clean_since[c(2,3,5,6,8,10:22, 24:124)]

#historical_incremental_wide_clean_since <- unique(historical_incremental_wide_clean_since)
#historical_incremental_wide_clean_since$store_id_x <- NULL


set.seed(9999)
rfo8 <- qeRF(historical_incremental_wide_clean_since,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_since))))
rfo8$testAcc
rfo8$baseAcc

set.seed(9999)
rfo9 <- qeRF(historical_incremental_wide_clean_since,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_since))))
rfo9$testAcc
rfo9$baseAcc


# rfo8$testAcc
#[1] 0.02786728
#> rfo8$baseAcc
#[1] 0.05479403
 
a <- cbind(historical_incremental_wide_clean_since[1:21273,] ,rfo8$predicted)

plot(a$incremental_rate, a$...124)


dim(historical_incremental_wide_clean_since)

a <- cbind(historical_incremental_wide_clean_since[1:21273,] ,rfo8$predicted)
plot(a$incremental_rate, a$...124)
b <- rfo8$importance
mean(abs(rfo8$predicted))
hist(abs(rfo8$predicted))
hist((rfo8$predicted))
mean((rfo8$predicted))
summary(a$...124)
summary(a$incremental_rate)


mean(rfo8$rsq)
mean(rfo8$mse)
rfo8$trainAcc

#[1] 0.02227413




historical_incremental_wide_clean_since_rfo7 <- historical_incremental_wide_clean_since[c(2,3,5,6,8,10:22, 24:124)]

cchistorical_incremental <- which(complete.cases(historical_incremental_wide_clean_since_rfo7))
historical_incremental_wide_clean_since_rfo7 <- historical_incremental_wide_clean_since_rfo7[cchistorical_incremental,]

set.seed(9999)
rfo7 <- qeRF(historical_incremental_wide_clean_since_rfo7,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_since_rfo7))))
rfo7$testAcc
rfo7$baseAcc




set.seed(9999) 
dtout<-qeDT(historical_incremental_wide_clean_model_lasso_na,'incremental_rate')
# dtout$baseAcc
# [1] 0.06166544
# > dtout$testAcc
# [1] 0.05062501





## prepare for xgboost

historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_model_copy %>% mutate_if(is.character, as.factor)
#historical_incremental_wide_clean_factor_scaled <- mmscale(historical_incremental_wide_clean_factor)
historical_incremental_wide_clean_factor$other_external_store_signage_visible <- ifelse(historical_incremental_wide_clean_factor$other_external_store_signage_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$pylon_signage_with_store_logo_is_visible <- ifelse(historical_incremental_wide_clean_factor$pylon_signage_with_store_logo_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$centre_is_visible <- ifelse(historical_incremental_wide_clean_factor$centre_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$store_itself_is_visible <- ifelse(historical_incremental_wide_clean_factor$store_itself_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$xmas_flag <- ifelse(historical_incremental_wide_clean_factor$xmas_flag== "TRUE", 1,0 )
historical_incremental_wide_clean_factor$easter_flag <-  ifelse(historical_incremental_wide_clean_factor$easter_flag== "TRUE", 1,0 )
#historical_incremental_wide_clean_factor <- subset(historical_incremental_wide_clean_factor, week_ending < '2025-01-01')
historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_factor[,c(-1,-7,-19)]
historical_incremental_wide_clean_factor$name <- NULL

historical_incremental_wide_clean_model_lasso <- factorsToDummies(historical_incremental_wide_clean_factor)
historical_incremental_wide_clean_model_lasso_na <- as.data.frame(historical_incremental_wide_clean_model_lasso)
write.csv(historical_incremental_wide_clean_model_lasso_na, "historical_incremental_wide_clean_model_lasso_na.csv")

which(colnames(historical_incremental_wide_clean_model_lasso_na)=="incremental_rate" )
historical_incremental_wide_clean_model_lasso
#historical_incremental_wide_clean_factor$site <- as.numeric(historical_incremental_wide_clean_factor$site)

historical_incremental_wide_clean_model_lasso_na_scaled <- mmscale(historical_incremental_wide_clean_model_lasso_na)


#it doesnt like site because it thinks it is a grouped variable?
set.seed(9999)
gbboost <- qeGBoost(historical_incremental_wide_clean_model_lasso_na,'incremental_rate',nTree=490,minNodeSize= 15,learnRate = 0.05,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))
gbboost$testAcc
gbboost$baseAcc
#gbboost$testAcc
#1] 0.04620181
# gbboost$baseAcc
#[1] 0.06166544
a <- cbind(historical_incremental_wide_clean_model_lasso_na[12183:13182,] ,gbboost$holdoutPreds)
a$res <- a$`gbboost$holdoutPreds` - a$incremental_rate

which(colnames(historical_incremental_wide_clean_model_lasso_na)=="incremental_rate" )
preds <- predict(gbboost, historical_incremental_wide_clean_model_lasso_na[-c(278)])
a <- cbind(historical_incremental_wide_clean_model_lasso_na , preds)
a$res <- a$preds - a$incremental_rate
plot(a$incremental_rate, a$res)
a$
gbout <- gbboost
gbm::gbm.perf(gbout$gbmOuts)
mean(abs(preds - historical_incremental_wide_clean_model_lasso_na$incremental_rate))
#0.047
#this runs fine
set.seed(9999)
ftout3 <- qeFT(data=historical_incremental_wide_clean_model_lasso_na,yName='incremental_rate',qeftn='qeGBoost',
pars=list(nTree = 490, minNodeSize= 5:15, learnRate = 0.02:0.2),nCombs = 10,nTst = 1000, nXval = 5)


##this doesn't'
#set.seed(9999)
#ftout4 <- qeFT(data=historical_incremental_wide_clean_model_copy ,yName='incremental_rate',qeftn='qeRF',
#pars=list(minNodeSize= 5:15, nTree = 200:500),nCombs = 10, nTst = 1000, nXval = 5)






#set.seed(9999)
#gbboost1 <- gbm::gbm.fit(historical_incremental_wide_clean_factor,'incremental_rate',nTree=500,minNodeSize= 10,learnRate = 0.1,n.cores = 4,
#holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_factor))))



set.seed(9999)
gbboost <- qeGBoost(historical_incremental_wide_clean_model_lasso_na,'incremental_rate',nTree=500,minNodeSize= 10,learnRate = 0.1,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))

gbout$testAcc
gbout$baseAcc


gbm::gbm.perf(gbboost$gbmOuts)


head(preds)
mean(abs(preds - historical_incremental_wide_clean_model_lasso_na$incremental_rate))

mean(gbout$gbmOuts$train.error)
mean(gbout$gbmOuts$test.error)



set.seed(9999)
xgboost <- qeXGBoost(historical_incremental_wide_clean_model_lasso_na,'incremental_rate',nRounds=500,
    params=list(eta=0.3,max_depth= 6 ,alpha= 0),
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))

# xgboost10 <- qeXGBoost(earlier_renewals,'incremental_rate',nRounds=250,
# params=list(eta=0.3,max_depth= 6 ,alpha= 0),
# holdout=floor(min(1000,0.1*nrow(earlier_renewals))))
# 

w <- historical_incremental_wide_clean_model_lasso_na  ##, site == '3101')
w$incremental_rate <- NULL

preds <- predict(xgboost, w)
w <- historical_incremental_wide_clean_model_lasso_na##, site == '3101')
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])

xgboost$testAcc
xgboost$baseAcc
# xgboost2$testAcc
# [1] 1.000125
# > 
# > xgboost2$baseAcc
# [1] 1.001303
# > 
mean(abs(preds - historical_incremental_wide_clean_model_lasso_na$incremental_rate))
# 0.01

which(colnames(historical_incremental_wide_clean_model_lasso_na)=="incremental_rate" )
which(colnames(earlier_renewals)=="incremental_rate" )

preds <- predict(xgboost, earlier_renewals[-c(3)])
a <- cbind(earlier_renewals , preds)
plot(a$incremental_rate, a$preds)

set.seed(9999)
xgboost2 <- qeXGBoost(historical_incremental_wide_clean_model_copy,'incremental_rate',nRounds=500,
   params=list(eta=0.3,max_depth= 6 ,alpha= 0),
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_copy))))

# xgboost10 <- qeXGBoost(earlier_renewals,'incremental_rate',nRounds=500,
# params=list(eta=0.3,max_depth= 6 ,alpha= 0),
# holdout=floor(min(1000,0.1*nrow(earlier_renewals))))


w <- historical_incremental_wide_clean_model_copy  ##, site == '3101')
w$incremental_rate <- NULL

preds <- predict(xgboost2, w)
w <- historical_incremental_wide_clean_model_copy##, site == '3101')
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])

xgboost2$testAcc
xgboost2$baseAcc
# xgboost2$testAcc
# [1] 1.000125
# > 
# > xgboost2$baseAcc
# [1] 1.001303
# > 
mean(abs(preds - historical_incremental_wide_clean_model_copy$incremental_rate))
#[1] 0.009002682


a <- cbind(historical_incremental_wide_clean_model_copy , preds)
a$res <- a$preds - a$incremental_rate
plot( a$res,a$incremental_rate)
gbout <- gbboost
gbm::gbm.perf(xgboost2)
library(dygraphs)
dygraph(a, main = "Sales (Promo/Control/Last Year) vs Actual") %>% dySeries("incremental_rate", label = "Sales_Actual") %>% #dySeries("preds", label = "predictions") %>%
dyOptions(stackedGraph = FALSE) %>%
dyRangeSelector(height = 20) %>%
dyOptions(colors = RColorBrewer::brewer.pal(6, "Set1"))

summary_fy25 <- a |> group_by(week_ending) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds
a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+  geom_smooth(method = "loess", span = 0.7, family = "symmetric") +  ggtitle("Renewals") + xlab("Date") + ylab("Incremental Rate [%]")



plot(a$week_ending, a$incremental_rate )

xgboost2$testAcc

xgboost4 <- qeXGBoost(earlier_renewals_lasso,'incremental_rate',nRounds=500,
   params=list(eta=0.3,max_depth= 6 ,alpha= 0),
holdout=floor(min(1000,0.1*nrow(earlier_renewals_lasso))))

which(colnames(earlier_renewals_lasso)=="incremental_rate" )

preds <- predict(xgboost4, earlier_renewals_lasso[-c(279)])
a <- cbind(earlier_renewals_lasso , preds)
plot(a$incremental_rate, a$preds)


TwoHistos <- ggplot(a) +
  labs(color="incremental_rate",x="Incremental_Rate",y="Count")+
  geom_histogram(aes(x=incremental_rate, fill= "incremental_rate"),  alpha = 0.2 ) + 
  geom_histogram(aes(x=preds, fill= "predicted_incremental_rate"), alpha = 0.2) + 
  scale_fill_manual(values = c("blue","red"))
TwoHistos


w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
w$incremental_rate <- NULL

preds <- predict(xgboost, w)
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])


###############
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
w$incremental_rate <- NULL

preds <- predict(xgboost4, w)
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])


#this doesnt work?????
set.seed(9999)
ftout13 <- qeFT(data=historical_incremental_wide_clean_model_lasso_na, yName='incremental_rate', qeftn='qeXGBoost',
      pars=list(max_depth = 3:6, eta=0.3:0.8), nCombs = 20, nTst = 1000, nXval =5)

save.image("~/Library/CloudStorage/GoogleDrive-hschulteindenbaeumen@woolworths.com.au/My Drive/backup/channel_shift/incremental_longer/incremental_longer.RData")
#historical_incremental_wide_clean_factor$north_american_population_density <-NULL
#set.seed(9999)
#ftout <- qeFT(data=historical_incremental_wide_clean_factor,yName='incremental_rate',qeftn='qeGBoost',
 #  pars=list(learnRate = 0.05:0.2), nCombs = 10, nTst = 1000, nXval = 5)
ftout12 <- qeFT(data=historical_incremental_wide_clean_model,yName='incremental_rate',qeftn='qeRF',
+    pars=list(nTree=100:500,minNodeSize= 5:15),nCombs = 12,nTst = 1000, nXval = 7)
# 404  10    0.02903854 
# 196  13    0.02978742 
# 316  15    0.02875761 
# 168  9    0.02897264 
# 495  11    0.02880475 
# 289  7    0.028492 
# 190  13    0.02870198 
# 366  14    0.02958367 
# 389  12    0.0281971 
# 359  7    0.0287501 
# 384  12    0.02916216 
# 111  8    0.02956203 




# Hyperparameter tuning
set.seed(9999)
ftout12 <- qeFT(data=historical_incremental_wide_clean_model_copy, yName = 'incremental_rate',qeftn='qeRF',
   pars=list(nTree=300:700,minNodeSize= 5:15),nCombs = 12,nTst = 1000, nXval = 7)

# 404  10    0.02634038 
# 217  8    0.02667767 
# 230  5    0.02598748 
# 216  10    0.02645939 
# 208  10    0.02662488 
# 473  5    0.02693903 
# 479  5    0.02729201 
# 168  9    0.0261525 
# 289  7    0.02710394 
# 404  7    0.02654302 


set.seed(9999)
rfo17 <- qeRF(historical_incremental_wide_clean_model,'incremental_rate',nTree=230 ,minNodeSize= 5,
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model))))
rfo17$testAcc
rfo17$baseAcc

sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.360_Reporting.zircon_insights_adjusted_dates_channel`"
tb <- bq_project_query(billing, sql)
zircon_insights_adjusted_dates_channel <-  bq_table_download(tb)

a <- cbind(historical_incremental_wide_clean_factor_lasso[1:12476,] ,rfo8$predicted)
plot(a$incremental_rate, a$...124)
b <- rfo8$importance
mean(abs(rfo8$predicted))
hist(abs(rfo8$predicted))
hist((rfo8$predicted))
mean((rfo8$predicted))
summary(a$...124)
summary(a$incremental_rate)


mean(rfo8$rsq)
mean(rfo8$mse)
rfo8$trainAcc
#Doesn't work'
#set.seed(9999)
#xgboost <- qeXGBoost(historical_incremental_wide_clean_factor,'incremental_rate',nTree=166,minNodeSize= 10,learnRate  = 0.1,
#holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_factor))))

qelout <- qeLin(historical_incremental_wide_clean_model_lasso_na, 'incremental_rate', holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))

qelout$baseAcc
#[1] 0.05839005
qelout$testAcc
# 0.03692294
 
library(regtools)

#historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_model %>% mutate_if(is.character, as.factor)
 
#historical_incremental_wide_clean_factor_lasso <- historical_incremental_wide_clean_factor
 
 
qeLASSO()
historical_incremental_wide_clean_factor$name <- NULL
historical_incremental_wide_clean_model_lasso <- factorsToDummies(historical_incremental_wide_clean_factor)
historical_incremental_wide_clean_model_lasso_na <- as.data.frame(historical_incremental_wide_clean_model_lasso)
lassout <- qeLASSO(historical_incremental_wide_clean_model_lasso_na, 'incremental_rate', alpha=1, holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))

lassout$testAcc
lassout$baseAcc
#  lassout$testAcc
# [1] 0.03949938
# > lassout$baseAcc
# [1] 0.06476187
preds <- as.data.frame(lassout$holdoutPreds)

a <- cbind(historical_incremental_wide_clean_model_lasso_na[12183:13182,] , preds)
a$res <- a$preds - a$incremental_rate
plot(a$incremental_rate, a$res)

qepout <- qePolyLin(historical_incremental_wide_clean_model_lasso_na, 'incremental_rate', deg = 2, holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))

length(lassout$coefs)
sum(lassout$coefs != 0)

plot(lassout)
lassout$nzero
head(lassout$whenEntered,20)


summary(qelout)


a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam") + facet_wrap(. ~ site) + ggtitle("Renewals FY25 with low rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")

compareout <- qeCompare(historical_incremental_wide_clean_model_lasso_na,'incremental_rate', c( 'qeLASSO' , 'qeRF', 'qeGBoost' ), 100)

#EDA

library(psych)
pairs.panels(historical_incremental_wide_clean_model[c("ratio_of_trading_area_to_carpark_space", "essential", "seifa_index",
"incremental_rate")], pch = ".")

correlation_matrix <- cor(historical_incremental_wide_clean_model_clean[, sapply(historical_incremental_wide_clean_model_clean, is.numeric)])
#install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method = "circle") # or other methods like "number", "shade", "color", "pie", etc.

highly_correlated <- which(abs(correlation_matrix) > 0.7 & upper.tri(correlation_matrix), arr.ind = TRUE)

highly_correlated_pairs <- data.frame(Variable1 = rownames(correlation_matrix)[highly_correlated[, 1]],
                      Variable2 = colnames(correlation_matrix)[highly_correlated[, 2]],   Correlation = correlation_matrix[highly_correlated])

highly_correlated <- which(abs(correlation_matrix) > 0.7 & upper.tri(correlation_matrix), arr.ind = TRUE)


print(highly_correlated_pairs)
heatmap(correlation_matrix)
       
# blueprint <- recipe(incremental_rate ~ ., data = historical_incremental_wide_clean_model) %>%
# step_center(all_numeric()) %>%
# step_scale(all_numeric()) %>%
# step_pca(all_numeric(), threshold = .95)       
# prepare <- prep(blueprint, training = historical_incremental_wide_clean_model)
# library(h2o)
       
#ratio_of_trading_area_to_carpark_space
mod2 <- lm(incremental_rate ~ ratio_of_trading_area_to_carpark_space, data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid(ratio_of_trading_area_to_carpark_space) %>%
add_predictions(mod2)

ggplot(historical_incremental_wide_clean_model, aes(ratio_of_trading_area_to_carpark_space)) +
geom_smooth(aes(y = incremental_rate))  +
geom_point(
data = grid,
aes(y = pred),
color = "red",
size = 2
)
mod2 <- lm(incremental_rate ~ seifa_index, data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid(seifa_index) %>%
add_predictions(mod2)

ggplot(historical_incremental_wide_clean_model, aes(seifa_index)) +
geom_smooth(aes(y = incremental_rate))  +
geom_point(
data = grid,
aes(y = pred),
color = "red",
size = 2
)
mod2 <- lm(incremental_rate ~ voc_score_before, data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid(voc_score_before) %>%
add_predictions(mod2)

ggplot(historical_incremental_wide_clean_model, aes(voc_score_before)) +
geom_smooth(aes(y = incremental_rate))  +
geom_point(
data = grid,
aes(y = pred),
color = "red",
size = 2
)

mod2 <- lm(incremental_rate ~ population_per_sqkm_10mins, data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid(population_per_sqkm_10mins) %>%
add_predictions(mod2)

ggplot(historical_incremental_wide_clean_model, aes(population_per_sqkm_10mins)) +
geom_smooth(aes(y = incremental_rate))  +
geom_point(
data = grid,
aes(y = pred),
color = "red",
size = 2
)

mod2 <- lm(incremental_rate ~ pop_growth_pre_vs_post_0_10_mins, data = historical_incremental_wide_clean_model)
mod2 <- randomForest(incremental_rate ~ pop_growth_pre_vs_post_0_10_mins, data = historical_incremental_wide_clean_model)
mod2 <- randomForest(incremental_rate ~ main_access_road_of_lanes_out, data = historical_incremental_wide_clean_factor)

grid <- historical_incremental_wide_clean_model %>%
data_grid(pop_growth_pre_vs_post_0_10_mins) %>%
add_predictions(mod2)

ggplot(historical_incremental_wide_clean_model, aes(pop_growth_pre_vs_post_0_10_mins)) +
geom_smooth(aes(y = incremental_rate))  +
geom_point(
data = grid,
aes(y = pred),
color = "red",
size = 2
)
grid <- historical_incremental_wide_clean_factor %>%
data_grid(main_access_road_of_lanes_out) %>%
add_predictions(mod2)

ggplot(since_launch_incremental_sales, aes(time, incremental_sales_weekly))+ geom_point() +  geom_smooth(data=subset(since_launch_incremental_sales,
time < 52, time > 0, method = "lm")) + ggtitle( "FY21-25")  + xlab('Weeks since launch')


ggplot(historical_incremental_wide_clean_factor, aes(main_access_road_of_lanes_out)) +
geom_smooth(data = subset(historical_incremental_wide_clean_factor, incremental_rate < 0.8), aes(y = incremental_rate), method = "lm")  +
geom_smooth(
data = grid,
aes(y = pred),
color = "red",
size = 2
)

ggplot(historical_incremental_wide_clean_factor, aes(main_access_road_of_lanes_out)) +
geom_point(aes(y = incremental_rate))  +
geom_line(
data = grid,
aes(y = pred),
color = "red",
size = 2
)





mod1 <- lm(incremental_rate ~  seifa_index * grouped_vcu , data = historical_incremental_wide_clean_model)
mod2 <- lm(incremental_rate ~  seifa_index + grouped_vcu, data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid( seifa_index , grouped_vcu) %>%
gather_predictions(mod1, mod2)



ggplot(historical_incremental_wide_clean_model, aes(seifa_index, incremental_rate, color = grouped_vcu)) +
geom_smooth() +
geom_line(data = grid, aes(y = pred)) +
facet_wrap( ~ model)

mod1 <- lm(incremental_rate ~  time_since_launch + fiscal_year , data = historical_incremental_wide_clean_model)
mod2 <- lm(incremental_rate ~  time_since_launch + fiscal_year, data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid( fiscal_year ,time_since_launch, grouped_vcu) %>%
gather_predictions(mod1)


ggplot(historical_incremental_wide_clean_model, aes(time_since_launch, incremental_rate, color = fiscal_year)) +
geom_smooth() +
geom_line(data = grid, aes(y = pred)) 



mod1 <- lm(incremental_rate ~  week_ending + essential   , data = historical_incremental_wide_clean_model)
mod2 <- lm(incremental_rate ~  week_ending * essential  , data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid( week_ending , essential ) %>%
gather_predictions(mod1, mod2)

ggplot(historical_incremental_wide_clean_model, aes( essential , incremental_rate, color =  week_ending )) +
geom_smooth() +
geom_line(data = grid, aes(y = pred)) +
facet_wrap( ~ model )

historical_incremental_wide_clean_model_essential <- subset(historical_incremental_wide_clean_model, dist_to_cluster_2_family <= 1)
mod1 <- randomForest(incremental_rate ~  dist_to_cluster_0_family   , data = historical_incremental_wide_clean_model_clean)
mod1 <- lm(incremental_rate ~  dist_to_cluster_0_family   , data = historical_incremental_wide_clean_model_clean)


mod2 <- lm(incremental_rate ~   essential   , data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model_clean %>%
data_grid(
x1 = seq_range(dist_to_cluster_2_family, 5),
x2 = seq_range(dist_to_cluster_0_ancestry, 5)
) %>%
gather_predictions(mod1, mod2)

grid <- historical_incremental_wide_clean_model_clean %>%
data_grid( dist_to_cluster_0_family ) %>%
add_predictions(mod1)

grid <- historical_incremental_wide_clean_model %>%
data_grid( essential ) %>%
gather_predictions(mod2)

ggplot(historical_incremental_wide_clean_model, aes(x =  dist_to_cluster_0_family )) +  geom_point(aes(y = incremental_rate)) + geom_smooth(aes(y = incremental_rate)) +
geom_point(data = grid, aes(y = pred) ,color = "red",
size = 1) 

facet_wrap( ~ mod2 )



ggplot(historical_incremental_wide_clean_model, aes(  essential , incremental_rate )) + geom_point()+ 
geom_smooth() +
geom_line(data = grid, aes(y = pred)) +
facet_wrap( ~model  )

write.csv(historical_incremental_wide_clean_factor, "historical_incremental_wide_clean_factor_lasso.csv")
z3 <-qeLASSO(historical_incremental_wide_clean_model, 'incremental_rate', holdout=NULL)
#historical_incremental_wide_clean_model_copy

#historical_incremental_wide_clean_factor
z3$testAcc
z3$baseAcc
z3$coefs

plot(z3)
#historical_incremental_wide_clean_na_sub <- historical_incremental_wide_clean_na[,c(-2)]

sum(z3$coefs!=0)


zz <- predict(z3, historical_incremental_wide_clean_factor)

z3$nzero
which(colnames(historical_incremental_wide_clean_model_copy)=="incremental_rate" )

z <- historical_incremental_wide_clean_factor[,c(-3)]

nnout <- qeNeural(z, 'incremental_rate', holdout = NULL)

nnout <- qeNeural(historical_incremental_wide_clean_model, 'incremental_rate', holdout = NULL)

#historical_incremental_wide_clean_factor <- historical_incremental_wide_clean %>% mutate_if(is.character, as.factor)

#historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_factor[,c(-4,-7,-23)]


#  dim(historical_incremental_wide_clean_factor)
# [1] 13172   110
# > dim(historical_incremental_wide_clean_model)
# [1] 13100   114
# > dim(historical_incremental_wide_clean_model_clean)
# [1] 13172   110





#library(keras)
# set.seed(9999)
# nnout<-qeNeural(historical_incremental_wide_clean_model_copy,'incremental_rate',holdout=NULL)

# PCA
#the next step wont't work any longer since the file changed due to removing four columns or so


historical_numeric <- historical_incremental_wide_clean_model_copy[,sapply(historical_incremental_wide_clean_model_copy, is.numeric) ]

historical_numeric <- historical_incremental_wide_clean_factor[,c(2,20:38,43:69,71:73,75:88,92:95,97:108,111)]

historical_numeric$free_paid_parking <- NULL
historical_numeric$hours_of_free_parking <-NULL
historical_numeric$north_american_population_density <- NULL
historical_numeric$escalators <- NULL
historical_numeric$floor_store_is_on <- NULL
historical_numeric$multicultural_mix <- NULL
historical_numeric$expected_multicultural_increase <- NULL         
historical_numeric$ethnicity_l1 <- NULL

historical_numeric$carpark_access_circulation_type <- NULL
historical_numeric$main_access_road_type <- NULL
historical_numeric$free_paid_parking <- NULL
historical_numeric$hours_of_free_parking <-NULL
historical_numeric$north_american_population_density <- NULL
historical_numeric$escalators <- NULL

which(colnames(historical_numeric)=='incremental_rate' )

set.seed(9999)
pcout <-prcomp(historical_numeric[,c(-1)])
rt <- pcout$rotation
dim(rt)

pcout$rotation <-rt[,1:20]
pcX<-predict(pcout,historical_numeric[,c(-1)])
dim(pcX)
pcsds <- pcout$sdev^2
cumsum(pcsds)/sum(pcsds)

historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_factor[,c(-1,-7,-19)]
historical_incremental_wide_clean_factor$name <- NULL

historical_incremental_wide_clean_model_lasso <- factorsToDummies(historical_incremental_wide_clean_factor)
historical_incremental_wide_clean_model_lasso_na <- as.data.frame(historical_incremental_wide_clean_model_lasso)
write.csv(historical_incremental_wide_clean_model_lasso_na, "historical_incremental_wide_clean_model_lasso_na.csv")

earlier_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending <= '2024-08-01')

later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending > '2024-08-01')

### dimension reduction 

z<-qePCA(historical_numeric,'incremental_rate','qeRF', holdout=floor(min(1000,0.1*nrow(historical_numeric))), opts = list(nTree=500, minNodeSize= 10), 0.99)
z$testAcc
z$baseAcc
# [1] 0.061
# > z$testAcc

# [1] 0.02159




summary(historical_numeric$incremental_rate)
Newx = later_renewals
pcNewx = predict(pcout,later_renewals)

zz <- predict(z, historical_numeric)

head(zz)
summary(z$qeOut$predicted)
mean(abs(zz - historical_numeric$incremental_rate))

#later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
#later_renewals <- subset(later_renewals, site %in% c('3196','4372', '1785', '1327', '1121')  )
w <- later_renewals
w$incremental_rate <- NULL

preds <- predict(rfo10, w)
w <- later_renewals
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
summary_fy25 <- a |> group_by(time_since_launch) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")+ facet_wrap(. ~ site) + ggtitle("Renewals FY25 with low rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")




a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = time_since_launch, y = Val, color = Var))+ geom_smooth(method = "gam") + ggtitle("Renewals FY25 with low rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")

summary_fy25 <- a |> group_by(week_ending) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds
a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+  geom_smooth(method = "loess", span = 0.7, family = "symmetric") +  ggtitle("Renewals") + xlab("Date") + ylab("Incremental Rate [%]")



mean(z$qeOut$mse)
mean(z$qeOut$rsq)

# mean(z$qeOut$mse)
# [1] 0.001559772
# > mean(z$qeOut$rsq)
# [1] 0.8386581

#b <- z$qeOut$importance


a <- cbind(historical_numeric ,zz)

TwoHistos <- ggplot(a) +
  labs(color="incremental_rate",x="Incremental_Rate",y="Count")+
  geom_histogram(aes(x=incremental_rate, fill= "incremental_rate"),  alpha = 0.2 ) + 
  geom_histogram(aes(x=z$qeOut$predicted, fill= "predicted_incremental_rate"), alpha = 0.2) + 
  scale_fill_manual(values = c("blue","red"))

TwoHistos
#plot(a$incremental_rate, a$`z$qeOut$predicted`)
plot(a$incremental_rate, a$zz)



b <- rfo3$importance
mean(abs(rfo3$predicted))
hist(abs(rfo3$predicted))
hist((rfo3$predicted))
mean((rfo3$predicted))
summary(a$`rfo3$predicted`)
summary(a$incremental_rate)


# runs for 20 minutes or so FOCI
ze <- qeFOCI(historical_incremental_wide_clean_model, "incremental_rate", numCores=2, parPlat='locThreads')

ze <- qeFOCI(historical_numeric, "incremental_rate", numCores=2, parPlat='locThreads')



z5<- qeUMAP(historical_numeric,'incremental_rate','qeRF', holdout=floor(min(1000,0.1*nrow(historical_numeric))), opts = list(nTree=250), 0.99)
z5$testAcc
z5$baseAcc
# z5$testAcc
# [1] 0.02569541
# > z5$baseAcc
# [1] 0.06809843
z5$qeOut$importance

a <- cbind(historical_numeric[1:12277,] ,z5$qeOut$predicted)
TwoHistos <- ggplot(a) +
  labs(color="incremental_rate",x="Incremental_Rate",y="Count")+
  geom_histogram(aes(x=incremental_rate, fill= "incremental_rate"),  alpha = 0.2 ) + 
  geom_histogram(aes(x=z$qeOut$predicted, fill= "predicted_incremental_rate"), alpha = 0.2) + 
  scale_fill_manual(values = c("blue","red"))

TwoHistos
plot(a$incremental_rate, a$`z$qeOut$predicted`)



summary(historical_numeric$incremental_rate)



z6 <- predict(z5, historical_numeric)
head(z6)
summary(z5$qeOut$predicted)
mean(abs(z5$qeOut$predicted - historical_numeric$incremental_rate))


z5$baseAcc
mean(z5$qeOut$mse)
mean(z5$qeOut$rsq)
b <- z5$qeOut$importance



historical_eastgardens <- historical_incremental_wide_clean_model |> filter(site == '1412' )
historical_morrabin_vic <- historical_incremental_wide_clean |> filter(site == '3161' )

mod1 <- lm(incremental_rate ~   fiscal_year + dist_to_cluster_2_family  , data = historical_incremental_wide_clean_model_clean)

#mod1 <- lm(incremental_rate ~  week_ending + dist_to_cluster_2_family + essential + seifa_index +   , data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model_clean %>%
data_grid( dist_to_cluster_2_family , .model = mod1 ) %>%
add_predictions(mod1) 

ggplot(grid, aes(dist_to_cluster_2_family, pred)) +
geom_point()



#grid <- historical_incremental_wide_clean_model %>%
#data_grid( seifa_index , grouped_vcu) %>%
#gather_predictions(mod1, mod2)

ggplot(historical_incremental_wide_clean_model, aes(incremental_rate,dist_to_cluster_2_family)) +
geom_hex(bins = 50) +
geom_line(data = grid, color = "red", linewidth = 0.1)

ggplot(historical_incremental_wide_clean_model_clean, aes( incremental_rate, dist_to_cluster_2_family , color = fiscal_year)) +
geom_smooth() +
geom_line(data = grid, aes(y = pred)) +
facet_wrap( ~ model)




ggplot(historical_incremental_wide_clean_model1, aes(incremental_rate, lresid)) +
geom_hex(bins = 50)

historical_incremental_wide_clean_model2 <- historical_incremental_wide_clean_model %>%
add_residuals(mod1, "lresid")
ggplot(historical_incremental_wide_clean_model2, aes(incremental_rate, lresid)) +
geom_hex(bins = 50)


# would benefit from hyper parameter tuning
dtout<-qeDT(historical_incremental_wide_clean_model_lasso_na,'incremental_rate',alpha=0.05, minsplit=20,minbucket=7,
maxdepth=0,mtry=10,holdout=floor(min(1000,0.1 * nrow(historical_incremental_wide_clean_model_lasso_na))))
plot(dtout)
dtout
dtout$nNodes
dtout$nTermNodes
dtout$baseAcc
dtout$trainAcc
# dtout$baseAcc
# [1] 0.06648462
# > dtout$trainAcc
# [1] 0.05011301


#qeFT(data,yName,qeftn,pars,nCombs=NULL,nTst,nXval,showProgress=TRUE)”

pcout <- prcomp(historical_incremental_wide_clean_factor[,-2])

mod2 <- randomForest(incremental_rate ~ pop_growth_pre_vs_post_0_10_mins, data = historical_incremental_wide_clean_model)
grid <- historical_incremental_wide_clean_model %>%
data_grid(pop_growth_pre_vs_post_0_10_mins) %>%
add_predictions(mod2)
ggplot(historical_incremental_wide_clean_model, aes(pop_growth_pre_vs_post_0_10_mins)) +
geom_smooth(aes(y = incremental_rate))  +
geom_point(
data = grid,
aes(y = pred),
color = "red",
size = 2
)


mod1 <- lm(incremental_rate ~  trading_density_sales_per_sqm
  , data = historical_incremental_wide_clean_model)

grid <- historical_incremental_wide_clean_model %>%
data_grid(trading_density_sales_per_sqm
 ) %>%
add_predictions(mod1)


ggplot(historical_incremental_wide_clean_model, aes(trading_density_sales_per_sqm)) +
geom_smooth(aes(y = incremental_rate))  +
geom_point(
data = grid,
aes(y = pred),
color = "red",
size = 2
)

mod1 <- lm(incremental_rate ~  population_per_sqkm_10mins + fiscal_year , data = historical_incremental_wide_clean_model_clean)
grid <- historical_incremental_wide_clean_model_clean %>%
data_grid(population_per_sqkm_10mins, fiscal_year) %>%
gather_predictions(mod1)

ggplot(historical_incremental_wide_clean_model, aes( population_per_sqkm_10mins   , incremental_rate, color =  fiscal_year))  + geom_smooth() +
geom_line(data = grid, aes(y = pred)) +
facet_wrap( ~ mod1 )

mod1 <- lm(incremental_rate ~  voc_score_before + fiscal_year , data = historical_incremental_wide_clean_model_clean)
grid <- historical_incremental_wide_clean_model_clean %>%
data_grid(voc_score_before, fiscal_year) %>%
gather_predictions(mod1)

ggplot(historical_incremental_wide_clean_model, aes( voc_score_before   , incremental_rate, color =  fiscal_year))  + geom_smooth() +
geom_line(data = grid, aes(y = pred)) +
facet_wrap( ~ model )

library(ranger)
set.seed(9999)
 rangerout <- qeRFranger(historical_incremental_wide_clean_model_copy ,"incremental_rate" ,nTree=500,minNodeSize=10,
    mtry=floor(sqrt(ncol(historical_incremental_wide_clean_model_copy)))+1, deweightPars=NULL,
    holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_copy))), yesYVal=NULL) 

rangerout$testAcc
rangerout$baseACC
# rangerout$testAcc
# [1] 0.02764893
# > rangerout$baseAcc
# [1] 0.06166544
earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')

earlier_renewals <- earlier_renewals
later_renewals <- 
later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')
library(ranger)
set.seed(9999)
 rangerout1 <- qeRFranger(earlier_renewals ,"incremental_rate" ,nTree=500,minNodeSize=10,
    mtry=floor(sqrt(ncol(earlier_renewals)))+1, deweightPars=NULL,
    holdout=floor(min(1000,0.1*nrow(earlier_renewals))), yesYVal=NULL) 


rangerout1$testAcc
# [1] 0.0302514
rangerout1$baseAcc
# [1] 0.06366287
later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
#later_renewals <- subset(later_renewals, site %in% c('3196','4372', '1785', '1327', '1121')  )
w <- later_renewals
w$incremental_rate <- NULL

preds <- predict(rangerout1$forest, w)
w <- later_renewals
a <- cbind (w , preds$predictions)

plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
summary_fy25 <- a |> group_by(site) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$`preds$predictions`

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")+ facet_wrap(. ~ site) + ggtitle("Renewals FY25 with positive rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")




 
set.seed(9999)
adaout4 <-qeRFgrf(historical_incremental_wide_clean_model_lasso_na, "incremental_rate",nTree=2000,minNodeSize=5,mtry=floor(sqrt(ncol(historical_incremental_wide_clean_model_lasso_na)))+1,
   ll=FALSE,lambda=0.1,splitCutoff=sqrt(nrow(historical_incremental_wide_clean_model_lasso_na)),quantls=NULL,
   holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))
adaout4$testAcc
#[1] 0.03515963
adaout4$baseAcc
#[1] 0.06166544
mean(adaout4$predictions)


library(lightgbm)
set.seed(9999)
#historical_incremental_wide_clean_model
adaout <- qeLightGBoost(historical_incremental_wide_clean_model_copy, "incremental_rate",nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_copy))))
which(colnames(historical_incremental_wide_clean_model_copy)=="incremental_rate" )

 adaout$baseAcc

 adaout$testAcc
# > adaout$baseAcc
# [1] 0.06165822
# >
# >  adaout$testAcc
# [1] 0.02780559
earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')
later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')
set.seed(9999)
#historical_incremental_wide_clean_model
adaout1 <- qeLightGBoost(earlier_renewals, "incremental_rate",nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(earlier_renewals))))
which(colnames(earlier_renewals)=="incremental_rate" )

 adaout1$baseAcc

 adaout1$testAcc 
#  adaout1$baseAcc
# [1] 0.06376411
# > 
# >  adaout1$testAcc 
# [1] 0.02872791
#  
 
#later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
later_renewals <- subset(later_renewals, site %in% c('3196','4372', '1785', '1327', '1121')  )
w <- later_renewals
w$incremental_rate <- NULL
w <- as.matrix(w)
preds <- predict(adaout1$lgbout, w)
w <- later_renewals

a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
summary_fy25 <- a |> group_by(site) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")+ facet_wrap(. ~ site) + ggtitle("Renewals FY25 with positive rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")
 
 
 
adaout$trnx
newNTree=NULL
preds <- adaout$lgbout$predict(adaout, historical_incremental_wide_clean_model_copy)

,
  start_iteration = NULL,
  num_iteration = NULL,
  header = FALSE, params = list())

a <- cbind(historical_incremental_wide_clean_model_lasso_na , preds)
plot(a$incremental_rate, a$preds)
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
w$incremental_rate <- NULL

preds <- predict(adaout, w, newNTree=NULL)
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])



# adaout2 <-qeNeural(historical_incremental_wide_clean_model, "incremental_rate", hidden=c(100,100),nEpoch=30,
#    acts=rep("relu",length(hidden)),learnRate=0.001,
#    conv=NULL,xShape=NULL,
#    holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model))))


preds <- predict(adaout4, historical_incremental_wide_clean_model_lasso_na[-c(278)])

a <- cbind(historical_incremental_wide_clean_model_lasso_na , preds)
plot(a$incremental_rate, a$preds)

####
historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_model_copy %>% mutate_if(is.character, as.factor)
#historical_incremental_wide_clean_factor_scaled <- mmscale(historical_incremental_wide_clean_factor)
historical_incremental_wide_clean_factor$other_external_store_signage_visible <- ifelse(historical_incremental_wide_clean_factor$other_external_store_signage_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$pylon_signage_with_store_logo_is_visible <- ifelse(historical_incremental_wide_clean_factor$pylon_signage_with_store_logo_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$centre_is_visible <- ifelse(historical_incremental_wide_clean_factor$centre_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$store_itself_is_visible <- ifelse(historical_incremental_wide_clean_factor$store_itself_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$xmas_flag <- ifelse(historical_incremental_wide_clean_factor$xmas_flag== "TRUE", 1,0 )
historical_incremental_wide_clean_factor$easter_flag <-  ifelse(historical_incremental_wide_clean_factor$easter_flag== "TRUE", 1,0 )
earlier_renewals_lasso <- subset(historical_incremental_wide_clean_factor, week_ending < '2024-08-01')
earlier_renewals_lasso <- earlier_renewals_lasso[,c(-1,-7,-19)]
earlier_renewals_lasso$name <- NULL

earlier_renewals_lasso <- factorsToDummies(earlier_renewals_lasso)
earlier_renewals_lasso <- as.data.frame(earlier_renewals_lasso)

which(colnames(earlier_renewals_lasso)=="incremental_rate" )
#278

later_renewals_lasso <- subset(historical_incremental_wide_clean_factor, week_ending >= '2024-08-01')
later_renewals_lasso <- later_renewals_lasso[,c(-1,-7,-19)]
later_renewals_lasso$name <- NULL

later_renewals_lasso <- factorsToDummies(later_renewals_lasso)
later_renewals_lasso <- as.data.frame(later_renewals_lasso)

which(colnames(later_renewals_lasso)=="incremental_rate" )

#279
ggplot(historical_incremental_wide_clean_model_copy , aes(x= time_since_launch, y= incremental_rate, color = fiscal_year)) +
geom_smooth() +
xlab("Year") + ggtitle("Renewal B&M program") +
ylab("incremental sales [%]")

earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')
later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')



### 
xgboost10 <- qeXGBoost(earlier_renewals,'incremental_rate',nRounds=500,
params=list(eta=0.3,max_depth = 6 ,alpha= 0),
holdout=NULL)
w <- later_renewals  ##, site == '3101')
w$incremental_rate <- NULL

preds <- predict(xgboost10, w)
w <- later_renewals##, site == '3101')
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])

xgboost10$testAcc
xgboost10$baseAcc
# xgboost2$testAcc
# [1] 1.000125
# > 
# > xgboost2$baseAcc
# [1] 1.001303
# > 
mean(abs(preds - later_renewals$incremental_rate))
#[1] 0.0058

# w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
# w$incremental_rate <- NULL
# 
# preds <- predict(xgboost10, w)
# w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
# a <- cbind (w ,preds)
# 
# plot.ts(a[c("incremental_rate", "preds")])

# w <- later_renewals_lasso
# w$incremental_rate <- NULL
# 
# preds <- predict(xgboost10, w)
# w <- later_renewals_lasso
# a <- cbind (w ,preds)
# 
# plot.ts(a[c("incremental_rate", "preds")])
# plot(a$incremental_rate, a$preds)
# mean(abs(a$preds - a$incremental_rate))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds
summary_fy25 <- test_set_actual_vs_predicted_per_store |> group_by(store) |> summarise(actual_incremental_rate = mean(actual_incremental_rate), predicted_incremental_rate = mean(predicted_incremental_rate))

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+  geom_smooth(method = "loess", span = 0.7, family = "symmetric") +  ggtitle("Renewals") + xlab("Date") + ylab("Incremental Rate [%]")

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = time_since_launch, y = Val, color = Var))+ geom_smooth(method = "gam") + ggtitle("Renewals FY25 with low rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")


a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_col(method = "gam") + facet_wrap(. ~ site) + ggtitle("Renewals FY25 with low rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")

p <- summary_fy25 %>%
pivot_longer(., cols = c(actual_incremental_rate, predicted_incremental_rate),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = store, y = Val, fill = Var)) +   geom_col()  + ggtitle("Renewals FY25 LTD") + ylab("Incremental Rate [%]")
# here only plot stores with FY25 launches in year 2025 maybe

# Source - https://stackoverflow.com/a
# Posted by Jonathan Chang, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-14, License - CC BY-SA 4.0

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




Data_for_Hagen_F25_Renewals_LTD_Results <- read_csv("~/Downloads/Data for Hagen - F25 Renewals - LTD Results.csv")

Data_for_Hagen_F25_Renewals_LTD_Results$`LTD Incremental Rate` <- data.frame(sapply(Data_for_Hagen_F25_Renewals_LTD_Results$`LTD Incremental Rate`, (function(x) as.numeric(gsub("%", "", x)))))


FY26_National_Balance_Program_FY26_National_Balance_Program_Live

fy25_test_data <- Data_for_Hagen_F25_Renewals_LTD_Results[c(2,45)]

#later_renewals_lasso<- later_renewals_lasso |> select(incremental_rate, week_ending, site.1287,site.1495, site.1685, site.3325, site.5945)
later_renewals_lasso <- later_renewals_lasso |> filter(site.1287 ==1 | site.1495 ==1 | site.1685==1 | site.3325==1 | site.5945==1 )

w <- later_renewals_lasso
w$incremental_rate <- NULL

preds <- predict(xgboost10, w)
w <- later_renewals_lasso
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))

a1 <- a |> select(incremental_rate, week_ending, site.1287,site.1495, site.1685, site.3325, site.5945, preds)

a1_subset <- a1 |> filter(site.1287 == 1 )


long <- a1 %>% 
  pivot_longer(
    cols = c(site.1287,site.1495, site.1685, site.3325, site.5945), 
    names_to = "site",
    values_to = "incremental_rate"
)

summary(a1_subset)
aggregate(site.1287 ~ incremental_rate, data = a1_subset, mean)


plot.ts(a1_subset[c("incremental_rate", "preds")])


###xgboost10
earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2023-06-01')
later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2023-08-01')

later_renewals <- subset(later_renewals,site %in% fy25_test_data$Site)

later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
later_renewals <- subset(later_renewals, site %in% c('3196','4372', '1785', '1327', '1121')  )
w <- later_renewals
w$incremental_rate <- NULL

preds <- predict(xgboost10, w)
w <- later_renewals
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
summary_fy25 <- a |> group_by(site) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")+ facet_wrap(. ~ site) + ggtitle("Renewals FY25 with positive rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")



ggplot(a,aes(x=week_ending, y= Incremental_sales, color = Geo_Class)) +
  geom_point(alpha=0.5)+ geom_line(size = 2) +
  theme_ipsum() +
  theme(legend.position="right") +
  xlab("Year") + ggtitle("Renewal B&M program") +
  ylab("incremental sales [%]") 

##
set.seed(9999)
rfo10 <- qeRF(earlier_renewals,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=NULL)
rfo10$testAcc
rfo10$baseAcc
# rfo10$testAcc
# [1] 0.03099235
# > rfo10$baseAcc
# [1] 0.06366287
later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
later_renewals <- subset(later_renewals, site %in% c('3196','4372', '1785', '1327', '1121')  )
w <- later_renewals
w$incremental_rate <- NULL

preds <- predict(rfo10, w)
w <- later_renewals
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
summary_fy25 <- a |> group_by(site) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")#+ facet_wrap(. ~ site) + ggtitle("Renewals FY25 with low rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")

TwoHistos <- ggplot(a) +
labs(color="incremental_rate",x="Incremental_Rate",y="Count")+
geom_histogram(aes(x=incremental_rate, fill= "incremental_rate"),  alpha = 0.2 ) +
geom_histogram(aes(x= a$preds, fill= "predicted_incremental_rate"), alpha = 0.2) +
scale_fill_manual(values = c("blue","red"))
TwoHistos



###############
historical_incremental_wide_clean_copy_clean <- historical_incremental_wide_clean_copy


earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')
later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')



# try different xgboost models
#overfitting, data leaking
 <- qeXGBoost(historical_incremental_wide_clean_model_lasso_na,'incremental_rate',nRounds=250,
params=list(eta=0.3,max_depth= 6 ,alpha= 0),
holdout=floor(min(1000,0.1*nrow(historical_incremental_wide_clean_model_lasso_na))))

w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
w$incremental_rate <- NULL

preds <- predict(xgboost2, w)
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])

# set.seed(9999)
# ftout4 <- qeFT(data=historical_incremental_wide_clean_model_lasso_na,yName='incremental_rate',qeftn='qeXGBoost',
# pars=list(max_depth = 4:8), nCombs = 2,nTst = 1000, nXval = 5)


##### linear models

z10 <- qeLASSO(earlier_renewals, "incremental_rate", holdout = NULL)
z10$nzero


# try different xgboost models
xgboost3 <- qeXGBoost(earlier_renewals_lasso,'incremental_rate',nRounds=500,
params=list(eta=0.3,max_depth= 3 ,alpha= 0),
holdout=NULL)

w <- later_renewals_lasso
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
w$incremental_rate <- NULL

preds <- predict(xgboost3, w)
w <- subset(historical_incremental_wide_clean_model_lasso_na, site.3101 == 1)
a <- cbind (later_renewals_lasso ,preds)

plot.ts(a[c("incremental_rate", "preds")])


weekly_incremental_sales <- instore_incremental_sales_factor_renewals_model_1_f24 |> group_by(week_ending) |>  summarise(incremental_sales = mean(incremental_rate)*100)
weekly_incremental_sales <- historical_incremental_wide_clean_model_fy24 |> group_by(week_ending) |>  summarise(incremental_sales = mean(incremental_rate)*100)
weekly_incremental_sales <- historical_incremental_wide_clean_model_copy |> group_by(week_ending) |>  summarise(incremental_sales = mean(incremental_rate)*100)


plot(weekly_incremental_sales$week_ending, weekly_incremental_sales$incremental_sales)

ggplot(historical_incremental_wide_clean_model,
aes(x = week_ending,
y = incremental_rate)) +
geom_point() +
geom_smooth(method = loess, se = FALSE) +
scale_color_viridis_d(option = "plasma", end = .7)

library(hrbrthemes)
ggplot(weekly_incremental_sales, aes(x =week_ending , y=incremental_sales)) +
  geom_point(alpha=0.5)+ geom_line() + geom_smooth() + scale_size(range = c(24,.1)) + scale_color_viridis_d(option = "plasma", end = .7) +
  scale_fill_viridis_d(option = "plasma", end = .7) +
  theme_ipsum() +
  theme(legend.position="right") +
  xlab("Date") + ggtitle("Renewal B&M program") +
  ylab("incremental sales [%]")  

ggplot(weekly_incremental_sales, aes(x =week_ending , y=incremental_sales)) +
   geom_smooth() + scale_size(range = c(24,.1)) + scale_color_viridis_d(option = "plasma", end = .7) +
  scale_fill_viridis_d(option = "plasma", end = .7) +
  theme_ipsum() +
  theme(legend.position="right") +
  xlab("Date") + ggtitle("Renewal B&M program") +
  ylab("incremental sales [%]")  

tidymodels_prefer()
set.seed(501)

# Save the split information for an 80/20 split of the data
historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_model_copy %>% mutate_if(is.character, as.factor)
#historical_incremental_wide_clean_factor_scaled <- mmscale(historical_incremental_wide_clean_factor)
historical_incremental_wide_clean_factor$other_external_store_signage_visible <- ifelse(historical_incremental_wide_clean_factor$other_external_store_signage_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$pylon_signage_with_store_logo_is_visible <- ifelse(historical_incremental_wide_clean_factor$pylon_signage_with_store_logo_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$centre_is_visible <- ifelse(historical_incremental_wide_clean_factor$centre_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$store_itself_is_visible <- ifelse(historical_incremental_wide_clean_factor$store_itself_is_visible == "TRUE", 1,0 )
historical_incremental_wide_clean_factor$xmas_flag <- ifelse(historical_incremental_wide_clean_factor$xmas_flag== "TRUE", 1,0 )
historical_incremental_wide_clean_factor$easter_flag <-  ifelse(historical_incremental_wide_clean_factor$easter_flag== "TRUE", 1,0 )

historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_factor[,c(-1,-4,-7,-19)]
historical_incremental_wide_clean_factor$name <- NULL

ames_split <- initial_split(historical_incremental_wide_clean_factor, prop = 0.80)
ames_split
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

rf_defaults <- rand_forest(mode = "regression")
rf_defaults

library(GGally)
historical_incremental_wide_clean_model_copy %>% select(incremental_rate ,voc_score_before, seifa_index, trading_area_sqm, difftime_age) %>% GGally::ggpairs()
set.seed(501)

library(skimr)
skimr::skim(ames_train)

set.seed(501)

rf_ranger <- rand_forest(mtry = 35, trees = 2000) %>% set_engine("ranger", importance = "impurity") %>%
   set_mode("regression") %>% fit(incremental_rate ~  . , data = ames_train)

#ames_test <- na.omit(ames_test)
predict <- predict(rf_ranger, new_data = ames_test)

rf_ranger %>% extract_fit_engine() ## %>% vcov()


model_res <- rf_ranger %>% extract_fit_engine()  %>%   summary()


# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
#> [1] "matrix" "array"
param_est
#>             Estimate Std. Error t value  Pr(>|t|)
#> (Intercept) -302.974    14.3983  -21.04 3.640e-90
#> Longitude     -2.075     0.1286  -16.13 1.395e-55
#> Latitude       2.710     0.1804   15.02 9.289e-49
rf_training_pred <- 
  predict(rf_ranger,  ames_train) %>% 
   # Add the true outcome data back in
  bind_cols(ames_train %>% 
              select(incremental_rate))

rf_training_pred %>%                # training set predictions
  rsq(.pred,incremental_rate)


set.seed(345)
folds <- vfold_cv(ames_train, v = 10)
folds

#rf_wf <- 
#  workflow() %>%
#  add_model(rf_ranger) %>%
#  add_formula(incremental_rate ~ . ) 
#set.seed(456)
#rf_fit_rs <- 
#  rf_wf %>% 
#  fit_resamples(folds)

collect_metrics(rf_wf)
# A tibble: 2 × 6
#.metric .estimator   mean     n std_err .config             
#<chr>   <chr>       <dbl> <int>   <dbl> <chr>               
#  1 rmse    standard   0.0421    10 0.00245 Preprocessor1_Model1
#   2 rsq     standard   0.916     10 0.00801 Preprocessor1_Model1

#rf_wf <- rf_wf %>% remove_formula() %>% add_variables(outcome = bm_increm_perc, predictors = everything())

#rf_fit <- fit(rf_wf, ames_train)
#rf_fit


rf_testing_pred <- 
  predict(rf_ranger,  ames_test) %>% 
   # Add the true outcome data back in
  bind_cols(ames_test %>% 
              select(incremental_rate))

rf_testing_pred %>% yardstick::rsq(truth =  incremental_rate, .pred)

vector <- colnames(ames_train)

vip_features <- vector
vip_train <-
  ames_train %>%
  select(all_of(vip_features))

# model function to explain feature importance
explainer_rf <-
  explain_tidymodels(
    rf_ranger,
    data = vip_train,
    y = ames_train$incremental_rate,
    label = "random forest",
    verbose = FALSE
  )


set.seed(1804)
vip_rf <- model_parts(explainer_rf, loss_function =
                        loss_root_mean_square)


plot(vip_rf)


####xgboost
historical_incremental_wide_clean_factor <- historical_incremental_wide_clean_factor[,c(-1, -2,-4,-7,-19)]
#historical_incremental_wide_clean_factor$site <- NULL
ames_split <- initial_split(historical_incremental_wide_clean_factor, prop = 0.80)
ames_split
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)
  
ames_train <- subset(historical_incremental_wide_clean_factor,  week_ending < '2024-08-01' )
ames_test <-  subset(historical_incremental_wide_clean_factor, week_ending >= '2024-08-01')
# ames_split <- initial_split(historical_incremental_wide_clean_factor, prop = 0.80)
# ames_split
# ames_train <- training(ames_split)
# ames_test  <-  testing(ames_split)



# boost_tree(
#   mode = "regression",
#   engine = "xgboost",
#   mtry = 35,
#   trees = 100,
#   min_n = 5,
#   tree_depth = NULL,
#   learn_rate = NULL,
#   loss_reduction = NULL,
#   sample_size = NULL,
#   stop_iter = NULL
# )

##starting value for mtry = p/3; needs hyperparametertuning
set.seed(501)
rf_boost <- boost_tree( mtry = 38,trees = 450) %>% set_engine("xgboost") %>%
   set_mode("regression") %>% fit(incremental_rate ~  . , data = ames_train)

set.seed(501)
rf_boost1 <- boost_tree( mtry = 38,trees = 450,tree_depth = 3 ) %>% set_engine("xgboost") %>%
   set_mode("regression") %>% fit(incremental_rate ~  . , data = ames_train)


#ames_test <- na.omit(ames_test)
predict <- predict(rf_boost1, new_data = ames_test)

rf_boost1 %>% extract_fit_engine() ## %>% vcov()


model_res <- rf_boost1 %>% extract_fit_engine()  %>%   summary()

rf_training_pred <- 
  predict(rf_boost1,  ames_train) %>% 
   # Add the true outcome data back in
  bind_cols(ames_train %>% 
              select(incremental_rate))

rf_training_pred %>%                # training set predictions
  yardstick::mae(.pred,incremental_rate)


set.seed(345)
folds <- vfold_cv(ames_train, v = 10)
folds

rf_wf <- 
  workflow() %>%
  add_model(rf_boost1) %>%
  add_formula(incremental_rate ~ . ) 
set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

collect_metrics(rf_wf)
# A tibble: 2 × 6
#.metric .estimator   mean     n std_err .config             
#<chr>   <chr>       <dbl> <int>   <dbl> <chr>               
#  1 rmse    standard   0.0421    10 0.00245 Preprocessor1_Model1
#   2 rsq     standard   0.916     10 0.00801 Preprocessor1_Model1

#rf_wf <- rf_wf %>% remove_formula() %>% add_variables(outcome = bm_increm_perc, predictors = everything())

#rf_fit <- fit(rf_wf, ames_train)
#rf_fit


rf_testing_pred <- 
  predict(rf_boost1,  ames_test) %>% 
   # Add the true outcome data back in
  bind_cols(ames_test %>% 
              select(incremental_rate))

rf_testing_pred %>% yardstick::mae(truth =  incremental_rate, .pred)
#later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
later_renewals <- subset(ames_test, site %in% c('3196','4372', '1785', '1327', '1121')  )
w <-later_renewals
w$incremental_rate <- NULL

preds <- predict(rf_boost1, w)
w <- later_renewals
a <- cbind (w ,preds)

plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$.pred - a$incremental_rate))
summary_fy25 <- a |> group_by(site) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$.pred

a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")+ facet_wrap(. ~ site) + ggtitle("Renewals FY25 with positive rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")

vector <- colnames(ames_train)
#vip_features <- c("trading_area" , "Simplified_VCU" , "suburb" ,"lifestage" , "Fiscal_Week", "pre_Instore_VOC",
#                  "centre" , "MULTICULTURAL_TAG" ,"cluster", "Age_Since_Being_Built", "Fiscal_Week", "pre_Instore_VOC",
# 
 #                                  "Competitor_COLES" , "Competitor_ALDI",  "site_sales" , "state_sales" ,"Geo_Class" , "Geo_Group" )
#ecom

vip_features <- vector
vip_train <-
  ames_train %>%
  select(all_of(vip_features))
which(colnames(vip_train)=="time_since_launch" )
vip_train_1685 <- subset(vip_train, site == '1685')
vip_train_3196 <- subset(vip_train, site == '3196')


a <- cbind(ames_train ,rf_training_pred)
a <- cbind(ames_test ,rf_testing_pred)

#plot.ts(a[c("incremental_rate", ".pred")])

a %>%
pivot_longer(., cols = c(incremental_rate, .pred),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = time_since_launch, y = Val, color = Var))+ geom_smooth(method = "gam")+  ggtitle("Renewals FY25 with positive rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")

a %>%
pivot_longer(., cols = c(incremental_rate, .pred),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")+  ggtitle("Renewals FY25 with positive rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")



# model function to explain feature importance
explainer_rf <-
  explain_tidymodels(
    rf_ranger,
    data = vip_train,
    y = ames_train$preds,
    label = "random forest",
    verbose = FALSE
  )


set.seed(1804)
vip_rf <- model_parts(explainer_rf, loss_function =
                        loss_root_mean_square)


plot(vip_rf)
ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name,
                      "after permutations\n(higher indicates more
important)")
  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")
  perm_vals <- full_vip %>%
    filter(variable == "_full_model_") %>%
    group_by(label) %>%
    summarise(dropout_loss = mean(dropout_loss))
  p <- full_vip %>%
    filter(variable != "_full_model_") %>%
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable))
  if(length(obj) > 1) {
    p <- p +
      facet_wrap(vars(label)) +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss,
                                       color = label),
                 size = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p +
      geom_vline(data = perm_vals, aes(xintercept =
                                         dropout_loss),
                 size = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
  }
  p +
    theme(legend.position = "none") +
    labs(x = metric_lab,
         y = NULL, fill = NULL, color = NULL)
}

ggplot_imp(vip_rf)

duplex <- vip_train[vip_train$site == '1685' & vip_train$time_since_launch == '33' ,]
duplex <- vip_train[vip_train$site == '3196' & vip_train$time_since_launch == '25' ,]

rf_breakdown <- predict_parts(explainer = explainer_rf,
new_observation = duplex)

plot(rf_breakdown)

set.seed(1801)
shap_duplex <-
  predict_parts(
    explainer = explainer_rf,
    new_observation = duplex,
    type = "shap",
    B = 20
  )
plot(shap_duplex)

#EDA
library(broom)
corr_res <- map(historical_numeric %>% select(-incremental_rate), cor.test, y = historical_numeric$incremental_rate)
corr_res[[1]]
corr_res %>% 
  # Convert each to a tidy format; `map_dfr()` stacks the data frames 
  map_dfr(tidy, .id = "predictor") %>% 
  ggplot(aes(x = fct_reorder(predictor, estimate))) + 
  geom_point(aes(y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  labs(x = NULL, y = "Correlation with incremental_rate") + scale_x_discrete(guide = guide_axis(angle = 90))


##############################
rf_boost1 <- boost_tree( mtry = 35,trees = 450) %>% set_engine("xgboost") %>%
   set_mode("regression") %>% fit(incremental_rate ~  . , data = ames_train)


rf_mod <- boost_tree( mtry = 35,trees = 450) %>% set_engine("xgboost") %>%
   set_mode("regression")

rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(incremental_rate ~  .   )

set.seed(345)
folds <- vfold_cv(ames_train, v = 10)
folds
set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

collect_metrics(rf_fit_rs)
# A tibble: 2 × 6
#.metric .estimator   mean     n std_err .config             
#<chr>   <chr>       <dbl> <int>   <dbl> <chr>               
#  1 rmse    standard   0.0421    10 0.00245 Preprocessor1_Model1
#   2 rsq     standard   0.916     10 0.00801 Preprocessor1_Model1

#rf_wf <- rf_wf %>% remove_formula() %>% add_variables(outcome = bm_increm_perc, predictors = everything())

rf_fit <- fit(rf_wf, ames_train)
rf_fit
final_rf_res <- last_fit(rf_wf,consumption_split)


instore_incremental_sales
instore_incremental_sales_summary <- instore_incremental_sales %>%
group_by(Site) %>%
mutate(time_since_launch = difftime(Week_Ending, Launch_Relaunch, unit = 'weeks'))

instore_incremental_sales
since_launch_incremental_sales <- instore_incremental_sales  |> group_by(Fiscal_Year, Weeks_Post_Launch) |>
summarise(incremental_sales_weekly = mean(Incremental_Rate, na.rm = TRUE))

#esl_since_launch_incremental_rate  <- esl_summary |> filter( time_to_launch > 0) |> group_by(time_to_launch, ESL_launch) |>
#summarise(incremental_rate_weekly = mean(Incremental_Rate, na.rm = TRUE))
names(since_launch_incremental_sales)[2] <- "time"
#names(esl_since_launch_incremental_rate)[1] <- "time"
ggplot(since_launch_incremental_sales, aes(time, incremental_sales_weekly, color = Fiscal_Year))+ geom_point() +  geom_smooth() + ggtitle( "FY 25/26")  + xlab('Weeks since launch')


str(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor)
ggplot(Prior_Years_Incremental_Sales_Extracts_Weekly_Incremental_Rate_factor, aes(x = Incremental_Rate)) +
geom_histogram(bins = 50, col= "white")

ggplot(instore_incremental_sales, aes(x=year, y= Incremental_sales, color = MULTICULTURAL_TAG)) +
geom_point(alpha=0.5)+ geom_line(size = 2) +
theme_ipsum() +
theme(legend.position="right") +
xlab("Year") + ggtitle("Renewal B&M program") +
ylab("incremental sales [%]")
set.seed(1801)


write.csv(historical_incremental_wide_clean_factor, file = "historical_incremental_wide_clean_factor.csv")
aldi_dist <- historical_incremental_wide_clean_model_copy |> group_by(competitor_aldi, fiscal_year, site) |> summarise(incremental_sales = mean(incremental_rate), nbr_stores = n_distinct(site))

View(aldi_dist)
aldi_dist$competitor_aldi <- factor(aldi_dist$competitor_aldi , levels = c("SAMECENTRE", "<1 KM" , "1-3 KM" ,"3-5 KM" , ">5 KM" ))
plot(aldi_dist$competitor_aldi, aldi_dist$incremental_sales
     )
write.csv(historical_incremental_wide_clean_model_copy, file = "historical_incremental_wide_clean_model_copy")

aldi_dist$incremental_sales <- aldi_dist$incremental_sales * 100
library(hrbrthemes)
ggplot(aldi_dist, aes(x=competitor_aldi, y= incremental_sales, color = nbr_stores, fill = nbr_stores)) +
geom_col() + 
theme_ipsum() +
theme(legend.position="right") +
xlab("Distance to ALDI") + ggtitle("Renewal B&M program") +
ylab("incremental sales [%]") + facet_wrap(~ fiscal_year)

p <- renewals_historical_mean_sum  %>% filter(Channel_Description == "Brick & Mortar") %>%
pivot_longer(., cols = c(basket),  names_to = "Var", values_to = "Val") %>%

ggplot(aes(x=competitor_aldi, y= incremental_sales, color = Lifestage_Group, fill = Price_Segment)) +  geom_col(position= "dodge") +  facet_wrap(~Var)

p + theme_bw() +theme(legend.position = "top")+ ggtitle("Driver tree renewals FY21/22")





