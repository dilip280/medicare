library(car)
library(VIM)
library(tidyverse)

medicare_dataset = read.csv(
  "C:\\Users\\dilip\\Downloads\\MSBA\\QuantitativeAnalysis\\GroupProject\\Medicare_Home_Health_Agency__HHA__Provider_Aggregate_Report__CY_2016.csv"
)

colnames(medicare_dataset)



names(medicare_dataset)[1] = "provider_id"
names(medicare_dataset)[2] = "agency_name"
names(medicare_dataset)[3] = "street_address"
names(medicare_dataset)[4] = "city"
names(medicare_dataset)[5] = "state"
names(medicare_dataset)[6] = "zip_code"
names(medicare_dataset)[7] = "total_episodes"
names(medicare_dataset)[8] = "distinct_beneficiaries"
names(medicare_dataset)[9] = "total_visits_per_episode"
names(medicare_dataset)[10] = "skilled_nursing_per_episode"
names(medicare_dataset)[11] = "pt_per_episode"
names(medicare_dataset)[12] = "ot_per_episode"
names(medicare_dataset)[13] = "st_per_episode"
names(medicare_dataset)[14] = "hha_per_episode"
names(medicare_dataset)[15] = "msv_per_episode"
names(medicare_dataset)[16] = "total_charge"
names(medicare_dataset)[17] = "medicare_payment"
names(medicare_dataset)[18] = "expected_payment"
names(medicare_dataset)[19] = "outlier_payment"
names(medicare_dataset)[20] = "lupa_episodes"
names(medicare_dataset)[21] = "lupa_payment"
names(medicare_dataset)[22] = "age"
names(medicare_dataset)[23] = "num_male"
names(medicare_dataset)[24] = "num_female"
names(medicare_dataset)[25] = "non_dual"
names(medicare_dataset)[26] = "dual"
names(medicare_dataset)[27] = "white"
names(medicare_dataset)[28] = "black"
names(medicare_dataset)[29] = "asian"
names(medicare_dataset)[30] = "hispanic"
names(medicare_dataset)[31] = "alaskan"
names(medicare_dataset)[32] = "unknown"
names(medicare_dataset)[33] = "hcc_score"
names(medicare_dataset)[34] = "atrial_fib"
names(medicare_dataset)[35] = "alzheimer"
names(medicare_dataset)[36] = "asthma"
names(medicare_dataset)[37] = "cancer"
names(medicare_dataset)[38] = "chf"
names(medicare_dataset)[39] = "kidney_disease"
names(medicare_dataset)[40] = "copd"
names(medicare_dataset)[41] = "depression"
names(medicare_dataset)[42] = "diabetes"
names(medicare_dataset)[43] = "hyperlipidemia"
names(medicare_dataset)[44] = "hypertension"
names(medicare_dataset)[45] = "ihd"
names(medicare_dataset)[46] = "osteoporosis"
names(medicare_dataset)[47] = "ra_oa"
names(medicare_dataset)[48] = "schizophrenia"
names(medicare_dataset)[49] = "stroke"

medicare_dataset = medicare_dataset[order(medicare_dataset[,7]),]

unused_columns = names(medicare_dataset) %in% c(
 'provider_id',
  'agency_name',
  'street_address',
  'city',
  'state',
  'zip_code',
  'total_visits_per_episode',
  'age',
  'num_male',
  'num_female',
  'total_charge',
  'medicare_payment',
  'outlier_payment',
  'lupa_episodes',
  'lupa_payment',
  'non_dual',
  'dual',
  'white',
  'black',
  'asian',
  'hispanic',
  'alaskan',
  'unknown',
  'ra_oa',
  'hyperlipidemia',
  'hypertension',
  'ihd'
)

filtered_medicare_data = medicare_dataset[!unused_columns]
names(filtered_medicare_data)

filtered_medicare_data$total_episodes = as.numeric(gsub(",", "", filtered_medicare_data$total_episodes, fixed = TRUE))
filtered_medicare_data$distinct_beneficiaries = as.numeric(gsub(
  ",",
  "",
  filtered_medicare_data$distinct_beneficiaries,
  fixed = TRUE
))
filtered_medicare_data$expected_payment = as.numeric(gsub(",", "", filtered_medicare_data$expected_payment, fixed = TRUE))
# imputed_data <-
#   mice(
#     filtered_medicare_data,
#     m = 1,
#     maxit = 50,
#     method = 'cart',
#     seed = 500
#   )
# complete_data <- complete(imputed_data, 1)


complete_data = na.omit(filtered_medicare_data)

initial_model = lm(expected_payment ~ . , data = complete_data)
summary(initial_model)


finalmodel = lm(
  log(expected_payment) ~ log(total_episodes) + skilled_nursing_per_episode + pt_per_episode + 
    ot_per_episode + st_per_episode + hha_per_episode + hcc_score,
  data = complete_data
)

summary(finalmodel)

finalmodel_with_degree2= lm(
  log(expected_payment) ~ log(total_episodes) + poly(skilled_nursing_per_episode, 2) + 
    poly(pt_per_episode,2) + 
    ot_per_episode + st_per_episode + hha_per_episode + hcc_score,
  data = complete_data
)

summary(finalmodel_with_degree2)
summary(finalmodel)

anova (finalmodel, finalmodel_with_degree2)
qqnorm(resid(finalmodel_with_degree2))
plot(fitted(finalmodel_with_degree2), resid(finalmodel_with_degree2))
crPlots(finalmodel_with_degree2)
influencePlot(finalmodel_with_degree2)
dwt(finalmodel_with_degree2)
