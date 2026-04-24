#this script takes the eu agreed endpoints of the substances monitored in 2024 and toxicity data collection by EL

#empty environment
rm(list = ls())

#load libraries
library(tidyverse)
library(data.table)
library(openxlsx)

tox_eu <- read.csv('./eu_agreed_endpoints.csv', header = TRUE, sep = ",")

#record the number of unique substances here for later comparison
tox_subs <- distinct(tox_eu, substance, cas) %>% rename(as_name = substance)

####create a table for the lowest toxicity endpoint per trophic level and substance####
hc5_endpoints <- c("hc5(er50)", "hc5(ic25)") #for the HC5 relevance

#collapse ONLY the ">=" qualifier rows, keep the highest value within those groups
grp_cols_ge <- c("substance","norm_cc_unit","organism","endpoint",
                 "exposure_type","norm_cc_qualifier")

tox_ge_collapsed <- tox_eu %>%
  filter(norm_cc_qualifier == ">=", !is.na(norm_cc_as)) %>%
  group_by(across(all_of(grp_cols_ge))) %>%
  arrange(desc(norm_cc_as)) %>%
  slice(1) %>%
  ungroup()

tox_not_ge <- tox_eu %>%
  filter(is.na(norm_cc_qualifier) | norm_cc_qualifier != ">=")

for_low_tox <- bind_rows(tox_not_ge, tox_ge_collapsed)

rm(tox_not_ge, tox_ge_collapsed)

low_tox <- for_low_tox %>%
  # encode rules as quality tiers
  mutate(
    tier = case_when(
      # tier 1: equal-to value, HC 5 endpoint
      !is.na(norm_cc_as) &
        norm_cc_qualifier == "=" &
        endpoint %in% hc5_endpoints ~ 1L,
      
      # tier 2: equal-to value and chronic
      !is.na(norm_cc_as) &
        exposure_type == "chronic" &
        norm_cc_qualifier == "=" &
        !(endpoint %in% hc5_endpoints) ~ 2L,
      
      # tier 3: equal-to value and acute
      !is.na(norm_cc_as) &
        exposure_type == "acute" &
        norm_cc_qualifier == "=" &
        !(endpoint %in% hc5_endpoints) ~ 3L,
      
      # tier 4: unbound value and chronic
      !is.na(norm_cc_as) &
        exposure_type == "chronic" &
        norm_cc_qualifier != "=" &
        !(endpoint %in% hc5_endpoints) ~ 4L,
      
      # tier 5: unbound value and acute
      !is.na(norm_cc_as) &
        exposure_type == "acute" &
        norm_cc_qualifier != "=" &
        !(endpoint %in% hc5_endpoints) ~ 5L,
      
      is.na(effect_cc_as) & is.na(endpoint) ~ 
        NA_integer_,
      
      .default = 6L
    )
  ) %>%#until here all tox data are still contained in low_tox (useful to check for which entries fell out in the tiering)
  filter(!is.na(tier)) %>%  
  group_by(substance) %>%
  #within the allowed set, pick best tier + lowest norm_cc_as
  mutate(best_tier = min(tier, na.rm = TRUE)) %>%
  #to cover the case when a tier 1 is not the overall lowest norm_cc_as but a tier 2 from a different trophic level
  #this implies that the tier 1 within the same trophic levels is still preferred
  #even if it is higher than a tier 2 in the same trophic level
  mutate(
    # lowest tier-1 value in group
    tier1_min_cc = if (any(tier == 1L)) {
      min(norm_cc_as[tier == 1L], na.rm = TRUE)
    } else {
      NA_real_
    },
    # trophic level of that lowest tier-1 value
    tier1_min_tl = if (any(tier == 1L)) {
      trophic_level[which.min(if_else(tier == 1L, norm_cc_as, Inf))]
    } else {
      NA_character_
    },
    # switch from tier 1 to tier 2 only if a lower tier-2 exists
    # in a different trophic level
    best_tier = case_when(
      best_tier == 1L &
        any(
          tier == 2L &
            trophic_level != tier1_min_tl &
            norm_cc_as < tier1_min_cc,
          na.rm = TRUE
        ) ~ 2L,
      TRUE ~ best_tier
    )
  ) %>%
  filter(tier == best_tier) %>%
  # prefer equal-to values within best tier if any exist
  mutate(has_equal = any(norm_cc_qualifier == "=", na.rm = TRUE)) %>%
  filter(!has_equal | norm_cc_qualifier == "=") %>%  # if "=" exists, drop the others
  arrange(norm_cc_as, .by_group = TRUE) %>%
  slice(1) %>% #only keep the lowest high quality tier value (should be at top due to arrange())
  ungroup() %>%
  select(editor, cas, substance, norm_cc_qualifier, lowest_norm_cc = norm_cc_as, norm_cc_unit,
         endpoint, exposure_type, organism, tier) %>%
  #flagging for susbtances with lowest tox data being an unbound value
  mutate(
    lowest_tox_flag = case_when(
      is.na(lowest_norm_cc) ~ "data gap",
      tier %in% c(1L,2L,3L)   ~ "lowest tox is equal-to value",
      tier %in% c(4L, 5L) ~ "data gap",
      TRUE                 ~ "data gap"
    )
  )

ec_vals_med <- paste0("ec", 20:50)
ec_vals_high <- paste0("ec", 51:100)

#add the multiplication factor mf
#consider special case of <=  or < values as lowest tox (adding of a multiplication factor to increase AF):
low_tox <- low_tox %>%
  mutate(mf = case_when(
    norm_cc_qualifier == "<=" & #if norm_cc_qualifier is "<=" and endpoint specifies (<=EC25)
      #add an multiplication factor of 1 to the final AF
      endpoint == "ec20" ~ 1,
    
    norm_cc_qualifier == "<=" &  #if norm_cc_qualifier is "<=" and endpoint specifies (EC25) up to (EC50)
      #add an multiplication factor of 10 to the final AF
      endpoint %in% ec_vals_med ~ 10, 
    
    norm_cc_qualifier == "<=" & #if norm_cc_qualifier is "<=" and endpoint specifies (>= EC55)
      #add an multiplication factor of 20 to the final AF
      endpoint %in% ec_vals_high ~ 20,
    
    .default = 1
  )
  )

#effect_concentration normalisation considering ionisation of the substance
#normalised concentration were corrected to a soil
#organic matter (OM) content of 3.4% (or the TG specified max OM content) of a 100% neutral substance
#theoretically this needs correction in case the AS is negatively charged,
#which increases the bioavailable exposure concentration
#however, the positively charged AS fractions is assumed to sorb to other soil constituents 
#like the neutral AS fraction sorbs to soil OM
#with the availability on the substance pka, basicity/acidity the soil ph and OM content in the tox study, 
#this correction is feasible but not meaningful if the exposure data from 
#the soil field samples is not corrected the same way for acids

#AF choice
#per substance and trophic level count the number of chronic/acute data points
#When only acute endpoints are available, the AF of 1000 is applied to the lowest value
#When an acute endpoint provides the lowest equal-to effect concentration and 
#further chronic effect concentration/s is/are available, the acute value is used 
#for the ad hoc SGV with an AF of 100. This can be the case, when only ER50/EC50 
#values are available for terrestrial plants along with other chronic value(s)
#In case of one chronic value, the AF of 100 is used
#In case of two chronic values at two trophic levels (the higher can 
#be a greater-than/equal-to value), the AF is 50.
#In case of three (or more) effect concentrations at three (or four)
#trophic levels, the AF of 10 is applied to the lowest value

#for the AF selection it does not matter whether the EPs of the trophic levels are R3 or R4!
#in the present case no such evaluation even exists!
tox_af_prep <- tox_eu %>% 
  filter(!is.na(norm_cc_as)) %>%  # include all non-NA values
  group_by(substance) %>%
  summarise(
    n_acute = sum(endpoint %in% c("lc50", "er50", "ec50") & exposure_type == "acute"),               # include all acute tests
    n_chronic = sum(exposure_type == "chronic"),            # include all chronic tests
    chronic_trophic_lvl = n_distinct(trophic_level[exposure_type == "chronic"]), # distinct chronic trophic levels
    .groups = "drop"
  )

####derive the AF and ad hoc sgv values####
ah_sgv <- left_join(low_tox, tox_af_prep, by = "substance") %>%
  mutate(
    af = case_when(
      endpoint == "hc5(noer)" ~ 5,
      endpoint == "hc5(ic25)" ~ 10,
      endpoint == "hc5(er50)" ~ 50,
      n_acute == 0 & n_chronic == 0 ~ NA_real_,   # If no valid value
      n_acute == 1 & n_chronic == 0 & exposure_type == "acute" ~ 1000, #endpoint refers to the lowest value for sgv derivation
      n_acute == 1 & n_chronic > 0 & exposure_type == "acute" ~ 100,
      n_chronic == 1 &
        !endpoint %in% hc5_endpoints ~ 100,
      n_chronic > 1 & chronic_trophic_lvl == 1  &
        !endpoint %in% hc5_endpoints ~ 100,
      n_chronic == 2 & chronic_trophic_lvl == 1 &
        !endpoint %in% hc5_endpoints ~ 100,
      n_chronic >= 3 & chronic_trophic_lvl >= 1 &
        norm_cc_qualifier %in% c("<", "<=") &
        !endpoint %in% hc5_endpoints ~ 100,
      n_chronic >= 3 & chronic_trophic_lvl >= 1 &
        norm_cc_qualifier %in% c(">=", ">") &
        !endpoint %in% hc5_endpoints ~ 50,
      chronic_trophic_lvl == 2 & norm_cc_qualifier == "=" &
        !endpoint %in% hc5_endpoints ~ 50,#norm_cc_qualifier refers to the lowest value for sgv derivation
      chronic_trophic_lvl >= 3 &
        !endpoint %in% hc5_endpoints ~ 10,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(sgv_mg_kg = lowest_norm_cc/(mf*af)) %>% 
  rename(`acute/chronic` = "exposure_type",
         scientific_name = "organism")

# final_sgv_table now has:
# editor, cas, substance, sgv_mg_kg, scientific_name, endpoint,
# reliability, relevance, norm_cc_qualifier, lowest_norm_cc,
# norm_cc_unit, af and mf

rm(list = setdiff(ls() , c("ah_sgv", "tox_eu", "tox_subs")))

ah_sgv <- ah_sgv %>% rename("as_name" = "substance")

#check if substances got lost
tox_subs_sgv <- distinct(ah_sgv, as_name, cas) 

message("During the ad hoc derivation, these substances got lost:")
setdiff(tox_subs, tox_subs_sgv)
message("Number of unique substances with eu endpoint ad hoc sgv and w/o data gap is ", 
        length(unique(ah_sgv$as_name[ah_sgv$lowest_tox_flag != "data gap"])))
#to find out which substances with identified data gap are actually needed for NABO check in nabo_sgv_prep.R

#create output
wb <- createWorkbook("wb")
addWorksheet(wb, sheetName = "screened_ad_hoc_sgv")
writeData(wb, sheet = "screened_ad_hoc_sgv", x = ah_sgv)
saveWorkbook(wb, file = "./src/data_package/screened_ad_hoc_sgv.xlsx", overwrite = TRUE)