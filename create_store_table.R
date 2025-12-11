
WOW_Store_SA1$int64_field_0 <- NULL

WOW_Store_SA1,  by.x = "Site", by.y = "Loc_No", all.x = TRUE)


sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.360_smkt.smkt_store_attributes`"

tb <- bq_project_query(billing, sql)
smkt_store_attributes <-  bq_table_download(tb)


WOW_Store<- merge(WOW_Store_SA1, smkt_store_attributes,  by.x = "Loc_No", by.y = "Location_No", all.x  = TRUE)


sql <- "SELECT * FROM `gcp-wow-corp-smr-dev.dev_workarea.combined_data`"
tb <- bq_project_query(billing, sql)
combined_data <-  bq_table_download(tb)

#install.packages('devtools')


WOW_Store$SA1_2021 <- NULL

WOW_Store <- WOW_Store[c(1:71)]

WOW_Store$Centre_Type.y <- NULL

combined_data$store_id_x
combined_data$store_id_y

combined_data_model <- combined_data[c(4,27,28, 32, 33,34, 38, 43:45,87:114, 119:142)]
sapply(combined_data_model, function(x) length(unique(x)))

combined_data_model <- combined_data_model[!duplicated(combined_data_model$store_id),]

dups <- combined_data_model %>% group_by(store_id) %>% filter(n() > 1)

combined_data_model <- combined_data_model[!duplicated(combined_data_model$store_id),]

sum(is.na(combined_data_model))
which(colnames(WOW_Store)=="Loc_No" )

names(WOW_Store)[1] <- "Site"


WOW_Store <- merge(WOW_Store, combined_data_model,  by.x = "Site", by.y = "store_id", all.x = TRUE)
dim(WOW_Store)
sum(is.na(WOW_Store_sample_model))

WOW_Store_sample <- WOW_Store 

vis_miss(WOW_Store, cluster = TRUE)

WOW_Store_sample$FULL_SHOP <- NULL
WOW_Store_sample$secondary_access_road___intersection_to_carpark_type <- NULL
WOW_Store_sample$secondary_access_road_type <- NULL
WOW_Store_sample$main_access_road_____of_lanes_in <- NULL
WOW_Store_sample$nrst_coles_trading_area <- NULL
WOW_Store_sample$car_park_type <- NULL



WOW_Store_sample$Date_Closed <- NULL
WOW_Store_sample$Next_Dev_Type <- NULL
WOW_Store_sample$Latest_Dev_Type <- NULL
WOW_Store_sample$Next_Dev_In_Store_Start_Date <- NULL
WOW_Store_sample$Next_Dev_Launch_Date <- NULL
WOW_Store_sample$Next_Dev_Project_No <- NULL
WOW_Store_sample$Opening_Week <- NULL



WOW_Store_sample_model <- WOW_Store_sample |> drop_na(suburb_type)
WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(budget)
##WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(trading_area_sqm)
WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(latest_dev_date)
WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(latest_dev_launch_week)
WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(age_since_last_dev)

WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(number_of_retail_levels_in_centre)
WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(free_paid_parking)


write.csv(WOW_Store_sample, "Wow_store_sample.csv")

WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(car_park_levels)
WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(hours_of_free_parking)
WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(floor_store_is_on)

vis_miss(WOW_Store_sample_model, cluster = TRUE)
vis_miss(historical_incremental_wide_clean_model_copy, cluster = TRUE)
write.csv(WOW_Store_sample_model, "Wow_store_sample_model_18072025.csv")

WOW_Store_sample_model %>%
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
sum(is.na(WOW_Store_sample_model$floor_store_is_on))
historical_incremental_wide_cc_sample <- historical_incremental_wide_clean_model_copy |> dplyr::slice_sample(n =5000)
vis_miss(historical_incremental_wide_cc_sample, cluster = TRUE)

historical_incremental_wide_clean_model_copy %>%
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
sum(is.na(WOW_Store_sample_model$floor_store_is_on))

#WOW_Store_sample_model <- na.omit(WOW_Store_sample_model)



#WOW_Store_sample_model <-  janitor::clean_names(WOW_Store_sample_model)
#WOW_Store_sample <-  janitor::clean_names(WOW_Store_sample)
#1000 rows
bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample", WOW_Store_sample,fields = as_bq_fields(WOW_Store_sample)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')
#800 rows no na values#
bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model", WOW_Store_sample_model,fields = as_bq_fields(WOW_Store_sample_model)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')



# WOW_Store_sample_model$difftime_age <- difftime(WOW_Store_sample_model$date_opened, WOW_Store_sample_model$launch_date, unit = "weeks")
# WOW_Store_sample_model$difftime_age <- as.numeric(WOW_Store_sample_model$difftime_age)
# WOW_Store_sample_model$difftime_age <- WOW_Store_sample_model$difftime_age/52
# WOW_Store_sample_model$time_since_launch <- as.numeric(WOW_Store_sample_model$time_since_launch)
# WOW_Store_sample_model$time_since_launch <- as.numeric(WOW_Store_sample_model$time_since_launch)
# 

zircon_insights_adjusted_dates_channel <- bquxjob_728e6391_1971a7f16de
zircon_insights_adjusted_dates_channel <- subset(zircon_insights_adjusted_dates_channel, DType == "Renewal" & Channel == "Online" & Renewal__Type == "Renewal - Standard Renewal" & DTB_Grading != "Pick Up - ISC")
zircon_model <- zircon_insights_adjusted_dates_channel[c(3,12,13)]
zircon_model <- zircon_model[!is.na(zircon_model$DTB_Pick_up_Offer),]

WOW_Store_sample_model <- merge(WOW_Store_sample_model, zircon_model,  by.x = "site", by.y = "Location_Number", all.x = TRUE)


#WOW_Store_sample_model_factor <-  WOW_Store_sample_model %>% mutate_if(is.character, as.factor)

 
WOW_Store_sample_model$ecom <-  ifelse(is.na(WOW_Store_sample_model$DTB_Pick_up_Offer), 0, 1 ) 

WOW_Store_sample_model[["DTB_Pick_up_Offer"]][is.na(WOW_Store_sample_model[["DTB_Pick_up_Offer"]])] <- "none"
WOW_Store_sample_model[["DTB_Grading"]][is.na(WOW_Store_sample_model[["DTB_Grading"]])] <- "none"
WOW_Store_sample_model <-  janitor::clean_names(WOW_Store_sample_model)

#null variance columns
WOW_Store_sample_model$free_paid_parking <- NULL
WOW_Store_sample_model$hours_of_free_parking <- NULL
WOW_Store_sample_model$north_american_population_density <- NULL
WOW_Store_sample_model$escalators <- NULL
WOW_Store_sample_model$store_id_x <- NULL
WOW_Store_sample_model$covid_flag <- 0
Data_for_Hagen_F25_Renewals_LTD_Results <- read_csv("~/Downloads/Data for Hagen - F25 Renewals - LTD Results.csv")
FY26_National_Balance_Program_FY26_National_Balance_Program_Live <- read_csv("~/Downloads/FY26 National Balance Program - FY26 National Balance Program Live.csv")
FY26_National_Balance_Program_FY26_National_Balance_Program_Live <- FY26_National_Balance_Program_FY26_National_Balance_Program_Live[1:75,]
WOW_Store_sample_model$unique_store <- NULL
WOW_Store_sample_model$trading_store <-  NULL

my_calendar <- calendar_full[c(3,13,14,17)]
my_calendar <- unique(my_calendar)

my_calendar_future <- subset(my_calendar, Week_Ending >= '2026-07-01' & Week_Ending < '2027-07-01')
my_calendar_future <- my_calendar_future[!duplicated(my_calendar_future$Week_Ending),]
###subset wow_store_sample_model to stores which havent been touched since ??? 2020 i.e. which are not in historical_incremental_rate_model_copy
#remove columns age sinced being build, age since development because they are outdated, recalculate them?
#check and maybe remove latest touch date or development date later when predicting on data set because they are not in model data
#expand_grid with my_calendar_future
# subset to exlude FY26 list as well to to have store list to predict uplift for FY27 

dim(WOW_Store_sample_model)
#[1] 817 115

#covid flag
WOW_Store_sample_model_fy27 <- WOW_Store_sample_model[!WOW_Store_sample_model$site %in% historical_incremental_wide_clean_model_copy$site, ]
#includes high odo, neighbourhood and renewals
WOW_Store_sample_model_fy27 <- WOW_Store_sample_model_fy27[!WOW_Store_sample_model_fy27$site %in% FY26_National_Balance_Program_FY26_National_Balance_Program_Live$`Store No`, ]

WOW_Store_sample_model_subset <- WOW_Store_sample_model |> select(site, latest_dev_date, latest_dev_launch_week, age_since_being_built, age_since_last_dev)
historical_incremental_wide_clean_model_copy <- merge(historical_incremental_wide_clean_model_copy, WOW_Store_sample_model_subset, by.x = ("site"), by.y = ("site"), all.x = TRUE)

summary(WOW_Store_sample_model$latest_dev_date)

write.csv(WOW_Store_sample_model, "Wow_store_sample_model.csv")

WOW_Store_sample_model$age_since_being_built <- drop_na()
WOW_Store_sample_model$age_since_last_dev <-  drop_na()
WOW_Store_sample_model$latest_dev_date <-  drop_na()
WOW_Store_sample_model$latest_dev_launch_week <-  drop_na()
WOW_Store_sample_model$store_status <-  NULL

bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model", WOW_Store_sample_model,fields = as_bq_fields(WOW_Store_sample_model)
,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')

my_calendar_future <- subset(my_calendar, Week_Ending >= '2026-07-01' & Week_Ending < '2027-07-01')

WOW_Store_sample_model_FY_27 <- WOW_Store_sample |> filter(site %in% c('1117','1165','1175','1317','2190','2519','2538','2575','2585','2601','2853','2883','3174','3316','4170','4359','4383','5618','5668','7058','7216')) 

WOW_Store_sample_model_FY_27 <- WOW_Store|> filter(Site %in% c('1117','1165','1175','1317','2190','2519','2538','2575','2585','2601','2853','2883','3174','3316','4170','4359','4383','5618','5668','7058','7216')) 

### add columnns launch_date, fiscal_week, time_since_launch, week_ending, fiscal_year

WOW_Store_sample_model_expand <- expand_grid(WOW_Store_sample_model_FY_27, my_calendar_future[c(5)])
WOW_Store_sample_model_expand <- merge(WOW_Store_sample_model_expand, my_calendar_future, by.x = "Week_Ending", by.y = "Week_Ending", all.x = TRUE)

write.csv(WOW_Store_sample_model_expand, "Wow_store_sample_model_expand.csv")

WOW_Store_sample_model_expand$launch_date <- as.Date("2026-07-01")

WOW_Store_sample_model_expand <- WOW_Store_sample_model_expand %>% mutate(time_since_launch = difftime(week_ending, launch_date, unit = 'weeks'))

WOW_Store_sample_model_expand$time_since_launch <- as.numeric(WOW_Store_sample_model_expand$time_since_launch)

WOW_Store_sample_model_expand$difftime_age <- difftime(WOW_Store_sample_model_expand$date_opened, WOW_Store_sample_model_expand$launch_date, unit = "weeks")
WOW_Store_sample_model_expand$difftime_age <- as.numeric(WOW_Store_sample_model_expand$difftime_age)
WOW_Store_sample_model_expand$difftime_age <- WOW_Store_sample_model_expand$difftime_age/52

library(waldo)

compare(names(WOW_Store_sample_model_fy27), names(historical_incremental_wide_clean_model_copy))
compare(names(earlier_renewals), names(later_renewals))


WOW_Store_sample_model_expand$division <- NULL
WOW_Store_sample_model_expand$banner <- NULL
WOW_Store_sample_model_expand$store_name <- NULL
WOW_Store_sample_model_expand$states <- NULL
#which(colnames(WOW_Store_sample_model_expand)=="Easter_Flag" )

names(WOW_Store_sample_model_expand)[1] <- "week_ending"
names(WOW_Store_sample_model_expand)[113] <- "fiscal_week"
names(WOW_Store_sample_model_expand)[114] <- "fiscal_year"
names(WOW_Store_sample_model_expand)[115] <- "easter_flag"
names(WOW_Store_sample_model_expand)[116] <- "xmas_flag"

earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')
later_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')

earlier_renewals <- historical_incremental_wide_clean_model_copy
constCols(earlier_renewals)
set.seed(9999)
rfo13 <- qeRF(earlier_renewals,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=NULL)
rfo10$testAcc
rfo10$baseAcc
# rfo10$testAcc
# [1] 0.03099235
# > rfo10$baseAcc
# [1] 0.06366287
#later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
#later_renewals <- subset(later_renewals, site %in% c('3196','4372', '1785', '1327', '1121')  )

historical_incremental_wide_clean_model_copy_notartget <- historical_incremental_wide_clean_model_copy
historical_incremental_wide_clean_model_copy_notartget$incremental_rate <- NULL


WOW_Store_sample_model_expand$DTB_Grading <- WOW_Store_sample_model_expand$dtb_grading
WOW_Store_sample_model_expand$dtb_grading <- NULL

WOW_Store_sample_model_expand$DTB_Pick_up_Offer <- WOW_Store_sample_model_expand$dtb_pick_up_offer
WOW_Store_sample_model_expand$dtb_pick_up_offer <- NULL

WOW_Store_sample_model_expand$age_since_being_built <- NULL
WOW_Store_sample_model_expand$latest_dev_date <- NULL
WOW_Store_sample_model_expand$latest_dev_launch_week <- NULL
WOW_Store_sample_model_expand$age_since_last_dev <- NULL
WOW_Store_sample_model_expand$store_status <- NULL


saveRDS(WOW_Store_sample_model_expand, file = "Wow_store_sample_model_expand_FY26_Julia.RData")
saveRDS(WOW_Store_sample_model_expand_fy26, file = "Wow_store_sample_model_expand_FY26_Hagen_morestores.RData")


later_renewals <- WOW_Store_sample_model_expand

bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model_expand_fy27_nerworkplannning", WOW_Store_sample_model_expand,fields = as_bq_fields(WOW_Store_sample_model_expand)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')

target <- Reduce(intersect, list(colnames(earlier_renewals), colnames(WOW_Store_sample_model_expand)))

# Source - https://stackoverflow.com/a
# Posted by simranpal kohli, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-17, License - CC BY-SA 4.0

col_name=colnames(earlier_renewals[which(!(colnames(earlier_renewals) %in% colnames(WOW_Store_sample_model_expand)))])




w <- later_renewals
w$incremental_rate <- NULL


preds <- predict(rfo13, w)
#w <- later_renewals
a <- cbind (w ,preds)
a <- rbind(a, predictions_fy26_2585)
mape_rf10 <- mean(abs(a$incremental_rate- a$preds)/ a$incremental_rate)
  
  
plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds
summary_fy27 <- a |> group_by(site, state, centre_type_x, grouped_vcu, suburb_type) |> summarise(predicted_incremental_rate = mean(preds))

summary_fy27_1 <- a |> group_by(time_since_launch) |> summarise(predicted_incremental_rate = mean(predicted))
# summary_fy27 <- a |> group_by(site, name, state, centre_type_x, grouped_vcu, suburb_type,latest_dev_date, latest_dev_launch_week, launch_date) |> summarise(predicted_incremental_rate = mean(predicted))
# summary_fy27$difftime_touched <- difftime(summary_fy27$latest_dev_date,summary_fy27$launch_date, unit = "weeks")
# summary_fy27$difftime_touched <- as.numeric(summary_fy27$difftime_touched)
# summary_fy27$difftime_touched <- summary_fy27$difftime_touched/52
a %>%
pivot_longer(., cols = c(observed, predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam"). ##+ facet_wrap(. ~ site) + ggtitle("Renewals FY25 with low rates LTD") + xlab("Date") + ylab("Incremental Rate [%]")

write.csv(summary_fy27, "renewal_stores_uplift_fy27.csv")

ggplot(summary_fy27, aes(x = predicted_incremental_rate)) +
geom_histogram(bins = 50, col= "white") + xlab("predicted_incremental_rate [%]") + ylab("number of stores")+ ggtitle( "FY 27")


#esl_since_launch_incremental_rate  <- esl_summary |> filter( time_to_launch > 0) |> group_by(time_to_launch, ESL_launch) |>
#summarise(incremental_rate_weekly = mean(Incremental_Rate, na.rm = TRUE))
names(summary_fy27_1)[1] <- "time"
#names(esl_since_launch_incremental_rate)[1] <- "time"
ggplot(summary_fy27_1, aes(time, predicted_incremental_rate))+ geom_point() +  geom_smooth() + ggtitle( "FY 27")  + xlab('Weeks since launch')

PROJECT_ID <-  "gcp-wow-finance-360-prod"
billing <- "gcp-wow-finance-360-prod"
con <- dbConnect(
  bigrquery::bigquery(),
  project = PROJECT_ID,
  dataset = "historical_incremental_sales",
  billing = billing
)
con

sql <- "SELECT * FROM `gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model_fy27`"
tb <- bq_project_query(billing, sql)
WOW_Store_sample_model_fy27_new <- bq_table_download(tb)

#############.  FY26 before was FY27
# sql <- "SELECT * FROM `gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample`"
# tb <- bq_project_query(billing, sql)
# WOW_Store_sample <-  bq_table_download(tb)
# 
# WOW_Store_sample_model <- WOW_Store_sample |> drop_na(SuburbType)
# WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(BUDGET)
# WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(trading_area_sqm)
# WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(Latest_Dev_Date)
# WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(Latest_Dev_Launch_Week)
# WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(Age_Since_Last_Dev)
# 
# WOW_Store_sample_model <- WOW_Store_sample_model |> drop_na(number_of_retail_levels_in_centre)
# WOW_Store_sample_model <- WOW_Store_sample |> drop_na(free_paid_parking)
# 
# # WOW_Store_sample_model$difftime_age <- difftime(WOW_Store_sample_model$date_opened, WOW_Store_sample_model$launch_date, unit = "weeks")
# # WOW_Store_sample_model$difftime_age <- as.numeric(WOW_Store_sample_model$difftime_age)
# # WOW_Store_sample_model$difftime_age <- WOW_Store_sample_model$difftime_age/52
# # WOW_Store_sample_model$time_since_launch <- as.numeric(WOW_Store_sample_model$time_since_launch)
# # WOW_Store_sample_model$time_since_launch <- as.numeric(WOW_Store_sample_model$time_since_launch)
# # 
# 
# zircon_insights_adjusted_dates_channel <- bquxjob_728e6391_1971a7f16de
# zircon_insights_adjusted_dates_channel <- subset(zircon_insights_adjusted_dates_channel, DType == "Renewal" & Channel == "Online" & Renewal__Type == "Renewal - Standard Renewal" & DTB_Grading != "Pick Up - ISC")
# zircon_model <- zircon_insights_adjusted_dates_channel[c(3,12,13)]
# zircon_model <- zircon_model[!is.na(zircon_model$DTB_Pick_up_Offer),]
# 
# WOW_Store_sample_model <- merge(WOW_Store_sample_model, zircon_model,  by.x = "site", by.y = "Location_Number", all.x = TRUE)
# 
# 
# 
# #WOW_Store_sample_model_factor <-  WOW_Store_sample_model %>% mutate_if(is.character, as.factor)
# 
#  
# WOW_Store_sample_model$ecom <-  ifelse(is.na(WOW_Store_sample_model$DTB_Pick_up_Offer), 0, 1 ) 
# 
# WOW_Store_sample_model[["DTB_Pick_up_Offer"]][is.na(WOW_Store_sample_model[["DTB_Pick_up_Offer"]])] <- "none"
# WOW_Store_sample_model[["DTB_Grading"]][is.na(WOW_Store_sample_model[["DTB_Grading"]])] <- "none"
# 
# #null variance columns
# WOW_Store_sample_model$free_paid_parking <- NULL
# WOW_Store_sample_model$hours_of_free_parking <- NULL
# WOW_Store_sample_model$north_american_population_density <- NULL
# WOW_Store_sample_model$escalators <- NULL
# WOW_Store_sample_model$store_id_x <- NULL
# WOW_Store_sample_model$covid_flag <- 0
# WOW_Store_sample_model$unique_store <- NULL
# WOW_Store_sample_model$trading_store <-  NULL

#my_calendar <- calendar_full[c(3,13,14,17)]
#my_calendar <- unique(my_calendar)

#FY26
my_calendar_future <- subset(my_calendar, Week_Ending >= '2025-07-01' & Week_Ending < '2026-07-01')
my_calendar_future <- my_calendar_future[!duplicated(my_calendar_future$Week_Ending),]
###subset wow_store_sample_model to stores which havent been touched since ??? 2020 i.e. which are not in historical_incremental_rate_model
#remove columns age sinced being build, age since development because they are outdated, recalculate them?
#check and maybe remove latest touch date or development date later when predicting on data set because they are not in model data
#expand_grid with my_calendar_future
# subset to exlude FY26 list as well to to have store list to predict uplift for FY27 

#covid flag
#WOW_Store_sample_model <- WOW_Store_sample_model[!WOW_Store_sample_model$site %in% historical_incremental_wide_clean_model$site, ]
#includes high odo, neighbourhood and renewals
WOW_Store_sample_model_fy26 <- WOW_Store_sample_model[WOW_Store_sample_model$site %in% FY26_National_Balance_Program_FY26_National_Balance_Program_Live$`Store No`, ]

summary(WOW_Store_sample_model$latest_dev_date)

write.csv(WOW_Store_sample_model_fy26, "Wow_store_sample_model_fy26.csv")
write.csv(WOW_Store_sample_model_fy27, "Wow_store_sample_model_fy27.csv")

bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model_fy26", WOW_Store_sample_model_fy26,fields = as_bq_fields(WOW_Store_sample_model_fy26)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')


bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model_fy27",WOW_Store_sample_model_fy27,fields = as_bq_fields(WOW_Store_sample_model_fy27)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')

bq_table_upload("gcp-wow-finance-360-prod.historical_incremental_sales.WOW_Store_sample_model",WOW_Store_sample_model,fields = as_bq_fields(WOW_Store_sample_model)
               ,create_disposition='CREATE_IF_NEEDED', write_disposition='WRITE_TRUNCATE')

write_csv(a, "predictions_fy26.csv")

 

### add columnns launch_date, fiscal_week, time_since_launch, week_ending, fiscal_year

WOW_Store_sample_model_expand_fy26 <- expand_grid(WOW_Store_sample_model_fy26, my_calendar_future[c(5)])
WOW_Store_sample_model_expand_fy26 <- merge( WOW_Store_sample_model_expand_fy26, my_calendar_future, by.x = "Week_Ending", by.y = "Week_Ending", all.x = TRUE)


write.csv( WOW_Store_sample_model_expand_fy26, " WOW_Store_sample_model_expand_fy26.csv")

WOW_Store_sample_model_expand_fy26$launch_date <- as.Date("2025-07-01")
WOW_Store_sample_model_expand_fy26 <- janitor::clean_names( WOW_Store_sample_model_expand_fy26)

WOW_Store_sample_model_expand_fy26 <-  WOW_Store_sample_model_expand_fy26 %>% mutate(time_since_launch = difftime(week_ending, launch_date, unit = 'weeks'))
WOW_Store_sample_model_expand_fy26$time_since_launch <- as.numeric( WOW_Store_sample_model_expand_fy26$time_since_launch)

WOW_Store_sample_model_expand_fy26$difftime_age <- difftime( WOW_Store_sample_model_expand_fy26$date_opened,  WOW_Store_sample_model_expand_fy26$launch_date, unit = "weeks")
WOW_Store_sample_model_expand_fy26$difftime_age <- as.numeric( WOW_Store_sample_model_expand_fy26$difftime_age)
WOW_Store_sample_model_expand_fy26$difftime_age <-  WOW_Store_sample_model_expand_fy26$difftime_age/52

library(waldo)
compare(names(historical_incremental_wide_clean_model_copy), names(WOW_Store_sample_model_expand_fy26))

WOW_Store_sample_model_expand_fy26$division <- NULL
WOW_Store_sample_model_expand_fy26$banner <- NULL
WOW_Store_sample_model_expand_fy26$store_name <- NULL
WOW_Store_sample_model_expand_fy26$states <- NULL
WOW_Store_sample_model_expand_fy26$store_status <- NULL
which(colnames( WOW_Store_sample_model_expand_fy26)=="fiscal_week_year" )
#112
#WOW_Store_sample_model_expand <- WOW_Store_sample_model_expand[,-c(112)]
names(WOW_Store_sample_model_expand_fy26)[112] <- "fiscal_week"
write.csv(WOW_Store_sample_model_expand_fy26, "WOW_Store_sample_model_expand_FY26.csv")
write.csv(WOW_Store_sample_model_expand, "WOW_Store_sample_model_expand_FY27.csv")
later_renewals <- WOW_Store_sample_model_expand_fy26
historical_incremental_wide_clean_model_copy <- janitor::clean_names(historical_incremental_wide_clean_model_copy)

historical_incremental_wide_clean_model_copy <- historical_incremental_wide_clean_model_copy |> drop_na(age_since_being_built)
historical_incremental_wide_clean_model_copy <- historical_incremental_wide_clean_model_copy |> drop_na(age_since_last_dev)
historical_incremental_wide_clean_model_copy <- historical_incremental_wide_clean_model_copy |> drop_na(latest_dev_date)
historical_incremental_wide_clean_model_copy <- historical_incremental_wide_clean_model_copy |> drop_na(latest_dev_launch_week)



earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2027-07-01')


compare(names(earlier_renewals), names(later_renewals))
compare(names(WOW_Store_sample_model_expand), names(historical_incremental_wide_clean_model_copy))

#names(WOW_Store_sample_model_expand)[1] <- "week_ending"
names(WOW_Store_sample_model_expand_fy26)[113] <- "fiscal_week"
#names(WOW_Store_sample_model_expand)[112] <- "fiscal_year"
#names(WOW_Store_sample_model_expand)[110] <- "easter_flag"
#names(WOW_Store_sample_model_expand)[111] <- "xmas_flag"
set.seed(9999)
rfo12 <- qeRF(earlier_renewals,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=NULL)
rfo12$testAcc
rfo12$baseAcc
# rfo10$testAcc
# [1] 0.03099235
# > rfo10$baseAcc
# [1] 0.06366287
#later_renewals <- subset(later_renewals, site %in% c('1287','1495', '1685', '3325', '5945')  )
#later_renewals <- subset(later_renewals, site %in% c('3196','4372', '1785', '1327', '1121')  )

cmd <- "qeRF(earlier_renewals,'incremental_rate',nTree=500,minNodeSize= 10)$testAcc"
crossvalOutput<-replicMeans(10,cmd)

w <- later_renewals
#w$incremental_rate <- NULL


preds <- predict(rfo13, w)
#w <- later_renewals
a <- cbind (w ,preds)


a$predicted <- 100* a$preds

summary_fy26 <- a |> group_by(site, name, state, centre_type_x, grouped_vcu, suburb_type,latest_dev_date, latest_dev_launch_week, launch_date,trading_density_sales_per_sqm, trading_area_sqm, ethnicity_l2) |> summarise(predicted_incremental_rate = mean(predicted))
summary_fy26$difftime_touched <- difftime(summary_fy26$latest_dev_date,summary_fy26$launch_date, unit = "weeks")
summary_fy26$difftime_touched <- as.numeric(summary_fy26$difftime_touched)
summary_fy26$difftime_touched <- summary_fy26$difftime_touched/52


summary_fy26 
summary_fy26_1 <- a |> group_by(time_since_launch) |> summarise(predicted_incremental_rate = mean(predicted))

a %>%
pivot_longer(., cols = c( predicted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = week_ending, y = Val, color = Var))+ geom_smooth(method = "gam")+ facet_wrap(. ~ site) + ggtitle("Renewals FY26") + xlab("Date") + ylab("Incremental Rate [%]")

write.csv(WOW_Store_sample_model_expand, "WOW_Store_sample_model_expand_fy26.csv")

write.csv(summary_fy26, "renewal_stores_uplift_fy26.csv")
write.csv(a, "renewal_stores_uplift_fy26_julia.csv")
write_csv(a, "predictions_fy26.csv")

ggplot(summary_fy26, aes(x = predicted_incremental_rate)) +
geom_histogram(bins = 50, col= "white") + xlab("predicted_incremental_rate [%]") + ylab("number of stores")+ ggtitle( "FY 26")

predictions_fy26 <- read_csv("predictions_fy26.csv",
col_types = cols(week_ending = col_date(format = "%Y-%m-%d"),
site = col_character(), date_opened = col_date(format = "%Y-%m-%d"),
latest_dev_date = col_date(format = "%Y-%m-%d"),
launch_date = col_date(format = "%Y-%m-%d")))

predictions_fy26_2585 <- predictions_fy26 |> filter(site == '2585')
predicted_store_uplift_FY26 <- read_csv("predicted_store_uplift_FY26.csv")
predicted_store_uplift_FY26_2585 <- predicted_store_uplift_FY26 |> filter(site == '2585')
predicted_store_uplift_FY26_2585$...1 <- NULL
predicted_store_uplift_FY26_2585$site <- as.character(predicted_store_uplift_FY26_2585$site)
#esl_since_launch_incremental_rate  <- esl_summary |> filter( time_to_launch > 0) |> group_by(time_to_launch, ESL_launch) |>
#summarise(incremental_rate_weekly = mean(Incremental_Rate, na.rm = TRUE))
names(summary_fy26_1)[1] <- "time"
#names(esl_since_launch_incremental_rate)[1] <- "time"
ggplot(summary_fy26_1, aes(time, predicted_incremental_rate))+ geom_point() +  geom_smooth() + ggtitle( "FY 26")  + xlab('Weeks since launch')

FY26_National_info <- FY26_National_Balance_Program_FY26_National_Balance_Program_Live |> select(`Store No`, `Brick Uplift %`)
names(FY26_National_info)[1] <- "site"

summary_fy26 <- merge(summary_fy26, FY26_National_info, by.x = "site", by.y = "site", all.x = TRUE)

summary_fy26$`Brick Uplift %` <- as.numeric(gsub("\\%", "",summary_fy26$`Brick Uplift %` ))
pcm_model_26 <- subset(pcm_model, Balance_Program_Year == 'F26')
summary_fy26 <- merge(summary_fy26, pcm_model_26, by.x = "site", by.y = "Site", all.x = TRUE)
# Install and load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")
if (!require("tidyr")) install.packages("tidyr")

library(ggplot2)
library(readr)
library(tidyr)
summary_fy26 <- summary_fy26 %>% mutate_if(is.character, as.factor)

# Read the dataset
df <- summary_fy26

# Rename columns for easier use
colnames(df)[colnames(df) == 'predicted_incremental_rate'] <- 'Predicted_Incremental_Rate'
colnames(df)[colnames(df) == 'f0_'] <- 'PCM_Uplift_Percent'
colnames(df)[colnames(df) == 'name'] <- 'Store_Name'
df$PCM_Uplift_Percent <- 100 * df$PCM_Uplift_Percent

# Reshape the data from wide to long format for ggplot
df_long <- pivot_longer(df, 
                        cols = c(Predicted_Incremental_Rate, PCM_Uplift_Percent), 
                        names_to = "Metric", 
                        values_to = "Value")

# Create the plot
ggplot(df_long, aes(x = Store_Name, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of model Predicted Incremental Rate and PCM Uplift % by Store",
    x = "Store",
    y = "Value",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Save the plot
ggsave("store_comparison_plot.png")
#earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')


#xgboost11
#earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')
#later_renewals <- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')
FY26_National_Balance_Program_FY26_National_Balance_Program_Live$site
WOW_Store_sample_model <- WOW_Store_sample_model[!WOW_Store_sample_model$site %in% FY26_National_Balance_Program_FY26_National_Balance_Program_Live$`Store No`, ]

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
summary_fy26 <- a |> group_by(site) |> summarise(actual_incremental_rate = mean(incremental_rate), predicted_incremental_rate = mean(preds))
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

sql <- "SELECT * FROM `gcp-wow-finance-de-lab-dev.360_smkt.smkt_store_attributes`"

tb <- bq_project_query(billing, sql)
smkt_store_attributes <-  bq_table_download(tb)


####### continued from rfo10 earlier FY 27 predicitons in a
a$predicted <- 100* a$preds


a$site <- as.character(a$site)
summary_fy27_filtered <- a |> filter(site %in% c('1117','1165','1175','1317','2190','2519','2538','2575','2585','2601','2853','2883','3174','3316','4170','4359','4383','5618','5668','7058','7216')) 


summary_fy27_annual <- a |> group_by(site) |> summarise(predicted_incremental_rate = mean(predicted))

summary_fy27_filtered <- summary_fy27 |> filter(site %in% c('1117','1165','1175','1317','2190','2519','2538','2575','2585','2601','2853','2883','3174','3316','4170','4359','4383','5618','5668','7058','7216')) 

summary_fy27_annual <- rbind(summary_fy27_annual, predicted_store_uplift_FY26_2585[,c(1,7)])

p <- summary_fy27_annual %>%
pivot_longer(., cols = c(predicted_incremental_rate),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = site, y = Val, fill = Var)) +   geom_col()  + ggtitle("Renewals FY27") + ylab("Incremental Rate [%]")

hagen_uplift_merged$Site <- as.character(hagen_uplift_merged$site)
hagen_uplift_merged$W360 <- round(hagen_uplift_merged$predicted_uplift,2)
hagen_uplift_merged$Network_Planning <- round(hagen_uplift_merged$np_uplift,2)

p <- hagen_uplift_merged %>%
pivot_longer(., cols = c(W360, Network_Planning),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = reorder(Site, Val), y = Val, fill = Var)) +   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + geom_text(aes(label = Val), position = position_dodge(width = 0.9),  vjust = -0.5) + 
                                                                                                          ggtitle("Renewals FY27") + ylab("Bricks & Mortar Sales Uplift [%]") + xlab("Site")
network_adjusted_planning <- read_excel("network_adjusted_planning.xlsx")
network_adjusted_planning_filter <- network_adjusted_planning |> select(Store, `network adjusted`)
hagen_uplift_new <- merge(hagen_uplift_merged, network_adjusted_planning_filter, by.x = "Site", by.y = 'Store', all.x = TRUE)
hagen_uplift_new$Network_Planning_new <- 100*hagen_uplift_new$`network adjusted`
hagen_uplift_new$W360<- round(hagen_uplift_new$W360,2)
# here only plot stores with FY25 launches in year 2025 maybe

# Source - https://stackoverflow.com/a
# Posted by Jonathan Chang, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-14, License - CC BY-SA 4.0

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p <- hagen_uplift_new %>%
pivot_longer(., cols = c(W360, network_planning_adjusted),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = reorder(Site, Val), y = Val, fill = Var)) +   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + geom_text(aes(label = Val), position = position_dodge(width = 0.9),  vjust = -0.5) + 
                                                                                                          ggtitle("Renewals FY27") + ylab("Bricks & Mortar Sales Uplift [%]") + xlab("Site")

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
       
p <- hagen_uplift_new %>%
pivot_longer(., cols = c(W360, Network_Planning_new),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = reorder(site, Val), y = Val, fill = Var)) +   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + geom_text(aes(label = Val), position = position_dodge(width = 0.9),  vjust = -0.5) + 
                                                                                                          ggtitle("Renewals FY27") + ylab("Bricks & Mortar Sales Uplift [%]") + xlab("Site")


p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# try model explanations


ames_split <- initial_split(a, prop = 0.99)
ames_split
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)


set.seed(501)

rf_ranger <- rand_forest(mtry = 35, trees = 2000) %>% set_engine("ranger", importance = "impurity") %>%
   set_mode("regression") %>% fit(preds ~  . , data = ames_train)



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
    y = ames_train$preds,
    label = "random forest",
    verbose = FALSE
  )


set.seed(1804)
vip_rf <- model_parts(explainer_rf, loss_function =
                        loss_root_mean_square)


plot(vip_rf)

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
vip_train_5618 <- vip_train |> filter(site == 5618 &  time_since_launch == 32)
duplex <- vip_train[vip_train$site =='5618' ,] # & vip_train$time_since_launch == '32'
duplex <- duplex[duplex$week_ending == '2027-02-21' ,]

duplex <- vip_train[vip_train$site =='4383' ,] # & vip_train$time_since_launch == '32'
duplex <- duplex[duplex$week_ending == '2027-02-21' ,]



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

###rfo13

earlier_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending < '2024-08-01')
later_renewals<- subset(historical_incremental_wide_clean_model_copy, week_ending >= '2024-08-01')


set.seed(9999)
rfo13 <- qeRF(earlier_renewals,'incremental_rate',nTree=500,minNodeSize= 10,
holdout=NULL)
rfo13$testAcc
rfo13$baseAcc
w <-later_renewals
w$incremental_rate <- NULL


preds <- predict(rfo13, w)
#w <- later_renewals
a <- cbind (w ,preds)
#a <- rbind(a, predictions_fy26_2585)
mape_rf10 <- mean(abs(a$incremental_rate- a$preds)/ a$incremental_rate)
  
  
plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds

a <- a |> group_by(site) |> summarise(predicted_incremental_rate = mean(predicted), actual_incremental_rate = mean(observed))

p <- a %>%
pivot_longer(., cols = c(predicted_incremental_rate, actual_incremental_rate),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = reorder(site, Val), y = Val, fill = Var)) +   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) 
                                                                                                          ggtitle("Renewals FY25") + ylab("Bricks & Mortar Sales Uplift [%]") + xlab("Site")



# here only plot stores with FY25 launches in year 2025 maybe

# Source - https://stackoverflow.com/a
# Posted by Jonathan Chang, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-14, License - CC BY-SA 4.0

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

library(xgboost)
set.seed(9999)
xgboost13 <- qeXGBoost(earlier_renewals,'incremental_rate',nRounds=500,
    params=list(eta=0.3,max_depth= 6 ,alpha= 0),
holdout=NULL)

w <-later_renewals
w$incremental_rate <- NULL


preds <- predict(xgboost13, w)
#w <- later_renewals
a <- cbind (w ,preds)
#a <- rbind(a, predictions_fy26_2585)
mape_rf10 <- mean(abs(a$incremental_rate- a$preds)/ a$incremental_rate)
  
  
plot.ts(a[c("incremental_rate", "preds")])
plot(a$incremental_rate, a$preds)
mean(abs(a$preds - a$incremental_rate))
a$observed <- 100 *a$incremental_rate
a$predicted <- 100* a$preds

a <- a |> group_by(site) |> summarise(predicted_incremental_rate = mean(predicted), actual_incremental_rate = mean(observed))

p <- a %>%
pivot_longer(., cols = c(predicted_incremental_rate, actual_incremental_rate),  names_to = "Var", values_to = "Val") %>%
ggplot(aes(x = reorder(site, Val), y = Val, fill = Var)) +   geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
     ggtitle("Renewals FY25") + ylab("Bricks & Mortar Sales Uplift [%]") + xlab("Site")

p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))