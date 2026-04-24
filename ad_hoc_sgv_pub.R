#this script derives the ad hoc sgv based on the collected and evaluated soil organism-toxicity data in soil_toxicity_endpoints

#empty environment
rm(list = ls())

#load libraries
library(tidyverse)
library(data.table)
library(openxlsx)

#read in the toxicity data collection and structure
tox <- read.csv2(file = "./soil_toxicity_endpoints.csv",
                 sep = ",", dec = ".")

#transfer to the norm_cc_unit
tox$Normalised.effect.cc.unit <- tox$Effect.concentration.unit

#housekeeping
names(tox) <- tolower(names(tox))
names(tox) <- gsub("[\\.(]","_", names(tox))
names(tox) <- gsub(")","", names(tox))
names(tox) <- gsub("__","_", names(tox))
names(tox) <- gsub(" ","", names(tox))
names(tox) <- gsub("__","_", names(tox))

tox <- tox %>% mutate(across(
  .cols = -c("trophic_level",
             "taxonomic_group",
             "scientific_name",
             "effect_group",
             "endpoint",
             "measured_effect",
             "rationale_for_reliability_relevance",
             "other_comments",
             "study_test_guideline",
             "evaluation_test_guideline_if_different",
             "dossier_name"
  ),
  .fns  = ~ gsub(" ", "", .) #remove spaces except in the above chr columns
)) %>%
  mutate(across(
    .cols = c("rationale_for_reliability_relevance"
    ),
    .fns  = ~ gsub("  ", " ", .) #remove spaces except in the above chr columns
  )) %>%
  mutate(across(where(is.character), tolower))


tox$soil_ph <- as.numeric(tox$soil_ph)
tox$test_duration_endpoint_specific <- as.numeric(tox$test_duration_endpoint_specific)
tox$norm_cc_as <- as.numeric(tox$norm_cc_as)
tox$effect_cc_as <- as.numeric(tox$effect_cc_as)
tox$pka <- as.numeric(tox$pka)
tox$log_kow <- as.numeric(tox$log_kow)
tox$om_perc <- as.numeric(tox$om_perc)

#remove unwanted characters from important identifying substances info
tox$cas <-  str_squish(tox$cas)
tox$as_name <-  str_squish(tox$as_name)

#remove random tabs from tox
tox$cas <- gsub("\n", "", tox$cas)

#replace om_perc ranges with the higher om_perc value
tox$om_perc[tox$om_perc == "1.8-2.2"] <- 2.2

tox$om_perc <- as.numeric(tox$om_perc)
tox$oc_perc <- as.numeric(tox$oc_perc)

#do some renaming of the trophic levels
tox$trophic_level[tox$trophic_level == "decomposers (litter transformers/primary consumers)"] <- "PC"
tox$trophic_level[tox$trophic_level == "decomposers (nutrient transformers)"] <- "NT"
tox$trophic_level[tox$trophic_level == "decomposers (nutrient tranformers)"] <- "NT"
tox$trophic_level[tox$trophic_level == "secondary consumers"] <- "SC"
tox$trophic_level[tox$trophic_level == "secondary consumer"] <- "SC"
tox$trophic_level[tox$trophic_level == "primary producers"] <- "PP"
tox$trophic_level[tox$trophic_level == "primary producers (terrestrial plants)"] <- "PP"

#adapt units to uniform format
#check the units
table(tox$norm_cc_unit, useNA = "always")

tox$norm_cc_unit[tox$norm_cc_unit == "mg/kgsoil"] <- "mg/kg"
tox$norm_cc_unit[tox$norm_cc_unit == "mg/kgsoildryweight"] <- "mg/kg"
tox$norm_cc_unit[tox$norm_cc_unit == "mg/kgdryweightsoil"] <- "mg/kg"

#change endpoint naming
tox$endpoint[tox$endpoint =="< 25% effect"] <- "<EC25"

#check the qualifier types
table(tox$norm_cc_qualifier, useNA = "always")

#unify symbol qualifiers
tox$norm_cc_qualifier <- gsub(" ", "", tox$norm_cc_qualifier)
tox <- tox %>% mutate(norm_cc_qualifier= str_squish(norm_cc_qualifier))
tox$norm_cc_qualifier[tox$norm_cc_qualifier == "≤"] <- "=<"
tox$norm_cc_qualifier[tox$norm_cc_qualifier == "≥"] <- ">="
tox$norm_cc_qualifier[tox$norm_cc_qualifier %in% c("n.a.", "n.r.")] <- NA

tox <- tox %>% mutate(norm_cc_qualifier =
                        case_when(is.na(norm_cc_qualifier) & !is.na(effect_cc_qualifier) ~
                                    effect_cc_qualifier,
                                  .default = norm_cc_qualifier)
)

####adapt the TG details where needed####
table(tox$study_test_guideline, useNA = "always")

TG_check <- distinct(tox, study_test_guideline, evaluation_test_guideline_if_different) %>% 
  filter(evaluation_test_guideline_if_different != "n.a.")

tox$study_test_guideline <- tolower(tox$study_test_guideline)

tox$evaluation_test_guideline_if_different <- tolower(tox$evaluation_test_guideline_if_different)

tox$study_test_guideline[tox$study_test_guideline == "epa 850.4100" &
                           tox$evaluation_test_guideline_if_different == "oecd 208"] <- "oecd 208"

tox$study_test_guideline[tox$study_test_guideline == "iso 11267 (1999)" &
                           tox$evaluation_test_guideline_if_different == "oecd 232"] <- "oecd 232"

tox$study_test_guideline[tox$study_test_guideline == "used soil defined according to oecd guidelines for testing of chemicals (revised draft document, august_x000d_1999), proposal for a new guideline 216,"
] <- "oecd 216"

tox$study_test_guideline[tox$study_test_guideline == "iso 11268-2 (1998)" &
                           tox$evaluation_test_guideline_if_different == "oecd 222"] <- "oecd 222"

tox$study_test_guideline[tox$study_test_guideline == "iso 11268-1" &
                           tox$evaluation_test_guideline_if_different == "n.a."] <- "oecd 207"

tox$study_test_guideline[tox$study_test_guideline == "oecd guideline 208"] <- "oecd 208"

tox$study_test_guideline[tox$evaluation_test_guideline_if_different == "oecd 216 (2000)"] <- "oecd 216"

tox$study_test_guideline[tox$study_test_guideline == "iso 11268-2 (1998) and bba vi 2-2 (1994)" &
                           tox$evaluation_test_guideline_if_different == "oecd 222"] <- "oecd 222"

tox$study_test_guideline[tox$study_test_guideline == "draft oecd" &
                           tox$evaluation_test_guideline_if_different == "oecd 216"] <- "oecd 216"

tox$study_test_guideline[tox$study_test_guideline == "draft oecd" &
                           tox$evaluation_test_guideline_if_different == "oecd 217"] <- "oecd 217"

tox$study_test_guideline[tox$study_test_guideline == "draft oecd" &
                           tox$evaluation_test_guideline_if_different == "oecd 222"] <- "oecd 222"

tox$study_test_guideline[tox$study_test_guideline == "eppo bulletin 24, 1-16 (1994)" &
                           tox$evaluation_test_guideline_if_different == "oecd 216"] <- "oecd 216"

tox$study_test_guideline[tox$study_test_guideline == "eppo bulletin 24, 1-16 (1994)" &
                           tox$evaluation_test_guideline_if_different == "oecd 217"] <- "oecd 217"

tox$study_test_guideline[tox$study_test_guideline == "iso 11268-2 (1998) and bba vi 2-2 (1994)" &
                           tox$evaluation_test_guideline_if_different == "oecd 222"] <- "oecd 222"

tox$study_test_guideline[tox$study_test_guideline == "iso 11267 (1999)" &
                           tox$evaluation_test_guideline_if_different == "oecd 232"] <- "oecd 232"

tox$study_test_guideline[tox$study_test_guideline == "oecd 208 draft (2000)"] <- "oecd 208"

tox$study_test_guideline[tox$study_test_guideline == "iso 11267" &
                           tox$evaluation_test_guideline_if_different == "oecd 232"] <- "oecd 232"

tox$study_test_guideline[tox$study_test_guideline == "in-house protocol" &
                           tox$evaluation_test_guideline_if_different == "oced 216"] <- "oecd 216"

tox$study_test_guideline[tox$study_test_guideline == "in-house protocol" &
                           tox$evaluation_test_guideline_if_different == "oced 217"] <- "oecd 217"

tox$study_test_guideline[tox$study_test_guideline == "bba-guideline vi, 2-2 (1994) and iso-draft (iso/dis 11268-2)" &
                           tox$evaluation_test_guideline_if_different == "oecd 222"] <- "oecd 222"

tox$study_test_guideline[tox$study_test_guideline == "eppo, chapter 7, 1994" &
                           tox$evaluation_test_guideline_if_different == "oecd 216"] <- "oecd 216"

tox$study_test_guideline[tox$study_test_guideline == "eppo, chapter 7, 1994" &
                           tox$evaluation_test_guideline_if_different == "oecd 217"] <- "oecd 217"

tox$study_test_guideline[tox$study_test_guideline == "iso and bba" &
                           tox$evaluation_test_guideline_if_different == "oecd 222"] <- "oecd 222"

tox$study_test_guideline[tox$study_test_guideline == "bba vi 1-1 (1990)" &
                           tox$evaluation_test_guideline_if_different %in% c("oecd 216", "oecd 217")] <- "oecd 216"

tox$study_test_guideline[tox$study_test_guideline == "bba vi 1-1 (1990)" &
                           tox$evaluation_test_guideline_if_different %in% c("oecd 217")] <- "oecd 217"

tox$study_test_guideline[tox$study_test_guideline == "bba vi 2-2" &
                           tox$evaluation_test_guideline_if_different == "oecd 222"] <- "oecd 222"

#####
#if a qualifier value is not reported (n.r.) assume it being an equal-to
tox$norm_cc_qualifier[tox$norm_cc_qualifier == "n.r."] <- "="

#rename a weird substance names
tox$as_name[tox$as_name == "phenmediphamtechnical(aeb038584)"] <- "phenmedipham"
tox$as_name[tox$as_name == "fludioxonil(cga173506)"] <- "fludioxonil"

#delete the product with two active substances
tox <- tox %>% filter(as_name != "napropamide&quinmerac")

#record the number of unique substances here for later comparison
tox_subs <- distinct(tox, as_name, cas)

#consideration of OM normalisation when TG validity criteria were fulfilled 
#but OM content not reported and thus the norm_cc is missing
#make sure that all documented om_perc are transformed to their om_perc equivalent
#take the TG max recommended OM
#(for estimation of Koc: Koc = Kow * 0.41 if needed)
tox <- tox %>% mutate(om_perc = 
                        case_when(
                          is.na(om_perc) & !is.na(oc_perc) ~
                            oc_perc*1.7 ,
                          .default = om_perc)
) %>%
  mutate(norm_cc_as = 
           case_when(
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline %in% c("oecd 216", "oecd 217") ~ 
               effect_cc_as * 3.4/2.55, #OECD216 /217
             
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline %in% 
               c("oecd 220", "oecd 207", "oecd 222") ~ 
               effect_cc_as * 3.4/10, #OECD220/207/222
             
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline %in% 
               c("oecd 232", "oecd 226") ~ 
               effect_cc_as * 3.4/5, #OECD226/232
             
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline == "oecd 208" ~ 
               effect_cc_as * 3.4/3, #OECD208
             
             .default = norm_cc_as
           )
  ) %>% #add the OM content to om_perc too (same criteria)
  mutate(om_perc =
           case_when(
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline %in% c("oecd 216", "oecd 217") ~ 
               2.55, #OECD216 /217
             
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline %in% 
               c("oecd 220", "oecd 207", "oecd 222") ~ 
               10, #OECD220/207/222
             
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline %in% 
               c("oecd 232", "oecd 226") ~ 
               5, #OECD226/232
             
             is.na(om_perc) &
               validity_met == "y" & 
               study_test_guideline == "oecd 208" ~ 
               3, #OECD208
             
             .default = om_perc
           )
  ) %>% # add another mutate(case_when()) that changes r3 r4 to r2 if above criteria are met but
  #rating was caused by missing OC/OM reporting:
  mutate(reliability =
           case_when(
             validity_met == "y" & 
               study_test_guideline %in% c("oecd 216", "oecd 217",
                                           "oecd 220", "oecd 207", "oecd 222",
                                           "oecd 232", "oecd 226", "oecd 208") &
               grepl("om/oc is not summarised|no test details and oc/om content|
                       organic matter content is not reported|oc/om is not reported|
                       oc/om not summarized|om is not summarised|om is not summarized|no soil info", 
                     `rationale_for_reliability_relevance`) ~ "r2",
             .default = reliability
           )
  )

#where applicable, calculate geometric means for endpoints 
#For geom mean calculation:
#if there are several comparable endpoints (equal-to values/same species/effect/duration/type of effect conc.) 
#if they are within factor 10 derive a geom mean
tox <- tox %>%
  group_by(as_name, norm_cc_qualifier, norm_cc_unit, scientific_name,
           test_duration_endpoint_specific, duration_unit, endpoint,
           effect_group, measured_effect) %>%
  mutate(
    # only "=" groups are ever eligible for geomean logic
    n_vals = if_else(
      norm_cc_qualifier == "=",
      sum(!is.na(norm_cc_as)),
      NA_integer_
    ),
    
    range_ratio = if_else(
      norm_cc_qualifier == "=" & n_vals > 1,
      max(norm_cc_as, na.rm = TRUE) / min(norm_cc_as, na.rm = TRUE),
      NA_real_
    ),
    
    geomean_factor = case_when(
      norm_cc_qualifier != "="        ~ "no geomean needed",
      n_vals <= 1                     ~ "no geomean needed",
      is.na(range_ratio)              ~ "no geomean needed",
      range_ratio < 10                ~ "within 10",
      range_ratio >= 10               ~ "greater 10"
    )
  ) %>%
  ungroup()

tox_nogeo <- tox %>% #filter out data entries for which geomean is derivable in line with the if_else statement
  filter(
    (geomean_factor %in% c("no geomean needed", "greater 10"))
  )
tox_geo <- tox %>%
  filter(norm_cc_qualifier == "=", n_vals > 1, geomean_factor == "within 10") %>%
  group_by(as_name, norm_cc_qualifier, norm_cc_unit, scientific_name,
           test_duration_endpoint_specific, duration_unit, endpoint,
           effect_group, measured_effect
  ) %>%
  mutate(
    norm_cc_as = exp(mean(log(norm_cc_as)))
  ) %>%
  ungroup() %>%
  distinct(as_name, scientific_name, norm_cc_as, .keep_all = TRUE)

tox <- rbind(tox_nogeo, tox_geo) %>% select(-n_vals, -range_ratio, -geomean_factor) #the entries needed for 
#geomean calculation are removed from the original df and only the ready geomean added again

rm("tox_geo", "tox_nogeo", "TG_check")

#ad hoc sgv derivation rules (bases on the AF method):
#find the lowest equal-to or unbound (>=/<=) effect concentration per trophic level and substance

#filter criteria: OM/OC content should be reported for the lowest value
#is the lowest tox endpoint an equal-to value? If not equal-to value (>=/<=)
#the next highest equal-to value should be set as the lowest tox endpoint and a data gap is flagged

####create a table for the lowest toxicity endpoint per trophic level and substance####
hc5_endpoints <- c("HC5 NOEC (NOER)", "HC5 EC25 (ER25)", "HC5 EC50 (ER50)") #for the HC5 relevance

#collapse ONLY the ">=" qualifier groups, keep the highest value
grp_cols_ge <- c(
  "as_name", "norm_cc_unit", "scientific_name",
  "test_duration_endpoint_specific", "duration_unit", "endpoint",
  "effect_group", "measured_effect", "norm_cc_qualifier"
)

tox_ge_collapsed <- tox %>%#collapse to highest >= tox value for >= groups and take best reliability
  filter(norm_cc_qualifier == ">=", !is.na(norm_cc_as)) %>%
  mutate(
    rel_rank = match(reliability, c("r1","r2","r3","r4")),
    rel_rank = if_else(is.na(rel_rank), 99L, rel_rank)
  ) %>%
  group_by(across(all_of(grp_cols_ge))) %>%
  arrange(desc(norm_cc_as), rel_rank) %>%
  slice(1) %>%
  ungroup() %>%
  select(-rel_rank)

tox_not_ge <- tox %>%
  filter(is.na(norm_cc_qualifier) | norm_cc_qualifier != ">=")

for_low_tox <- bind_rows(tox_not_ge, tox_ge_collapsed)

rm("tox_not_ge", "tox_ge_collapsed")

low_tox <- for_low_tox %>%
  # encode rules as quality tiers
  mutate(
    tier = case_when(
      # tier 1: relevance c1/c2, reliabilty r1/r2, OM_perc reported, equal-to value, HC 5 endpoint
      !is.na(norm_cc_as) &
        relevance %in% c("c1", "c2") &
        reliability %in% c("r1", "r2") &
        !is.na(OM_perc) &
        norm_cc_qualifier == "=" &
        endpoint %in% hc5_endpoints ~ 1L,
      
      # tier 2: relevance c1/c2, reliabilty r1/r2, OM_perc reported, equal-to value
      !is.na(norm_cc_as) &
        relevance %in% c("c1", "c2") &
        reliability %in% c("r1", "r2") &
        !is.na(OM_perc) &
        norm_cc_qualifier == "=" &
        !(endpoint %in% hc5_endpoints) ~ 2L,
      
      # tier 3: drop equal_to requirement, still c1/c2, r1/r2, OM_perc reported
      !is.na(norm_cc_as) &
        relevance %in% c("c1", "c2") &
        reliability %in% c("r1", "r2") &
        !is.na(OM_perc) &
        norm_cc_qualifier != "=" &
        !(endpoint %in% hc5_endpoints) ~ 3L,
      
      # tier 4: allow r3/r4 but keep "equal_to" if possible
      !is.na(norm_cc_as) &
        relevance %in% c("c1", "c2", "c3", "c4") &
        reliability %in% c("r3", "r4") &
        !is.na(OM_perc) &
        norm_cc_qualifier == "=" &
        !(endpoint %in% hc5_endpoints) ~ 4L,
      
      # tier 5: r3/r4 and qualifier != "="
      !is.na(norm_cc_as) &
        relevance %in% c("c1", "c2", "c3", "c4") &
        reliability %in% c("r3", "r4") &
        !is.na(OM_perc) &
        norm_cc_qualifier != "=" &
        !(endpoint %in% hc5_endpoints) ~ 5L,
      
      TRUE ~ 6L
    )
  ) %>%#until here all tox data are still contained in low_tox (useful to check for which entries fell out in the tiering)
  filter(!is.na(tier)) %>%      
  group_by(as_name) %>%
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
  select(
    editor, cas, as_name, norm_cc_qualifier, lowest_norm_cc = norm_cc_as, norm_cc_unit,
    endpoint, `acute/chronic`, scientific_name, validity_met, reliability, relevance,
    tier, `rationale_for_reliability/relevance`
  ) %>%
  #flagging
  mutate(
    lowest_tox_flag = case_when(
      is.na(lowest_norm_cc) ~ "data gap",
      tier %in% c(1L,2L)   ~ "guidance compliant (r1/r2, OM reported, equal-to)",
      TRUE                 ~ "data gap"
    )
  )

#make numeric vectors to classify the effect size for selection of the additional mulitplication factor
ec_vals_med <- paste0("EC", 20:50)
ec_vals_high <- paste0("EC", 51:100)

#add the multiplication factor mf
#consider special case of <=  or < values as lowest tox (adding of a multiplication factor to increase AF):
low_tox <- low_tox %>%
  mutate(mf = case_when(
    norm_cc_qualifier == "<=" & #if norm_cc_qualifier is "<=" and endpoint specifies (<=EC20)
      #add an multiplication factor of 1 to the final AF
      endpoint == "EC20" &
      reliability %in% c("r1","r2") ~ 1,
    
    norm_cc_qualifier == "<=" &  #if norm_cc_qualifier is "<=" and endpoint specifies (EC25) up to (EC50)
      #add an multiplication factor of 10 to the final AF
      endpoint %in% ec_vals_med &
      reliability%in% c("r1","r2") ~ 10, 
    
    norm_cc_qualifier == "<=" & #if norm_cc_qualifier is "<=" and endpoint specifies (>= EC51)
      #add an multiplication factor of 20 to the final AF
      endpoint %in% ec_vals_high &
      reliability%in% c("r1","r2") ~ 20,
    
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
tox_af_prep <- tox %>% 
  filter(!is.na(norm_cc_as)) %>%  # include all non-NA values
  group_by(as_name) %>%
  summarise(
    n_acute = sum(endpoint %in% c("lc50", "er50", "ec50") & trophic_level != "PP"),               # include all acute tests
    n_chronic = sum(!endpoint %in% c("lc50", "er50", "ec50") | (endpoint %in% c("lc50", "er50", "ec50") & trophic_level == "PP")),            # include all chronic tests
    chronic_trophic_lvl = n_distinct(trophic_level[!endpoint %in% c("lc50", "er50", "ec50")]), # distinct chronic trophic levels
    .groups = "drop"
  )

####derive the AF and ad hoc sgv values####
ah_sgv <- left_join(low_tox, tox_af_prep, by = "as_name") %>%
  mutate(
    af = case_when(
      endpoint == "HC5 NOEC (NOER)" ~ 5,
      endpoint == "HC5 EC25 (ER25)" ~ 10,
      endpoint == "HC5 EC50 (ER50)" ~ 50,
      n_acute == 0 & n_chronic == 0 ~ NA_real_,   # If no valid value
      n_acute == 1 & n_chronic == 0 & `acute_chronic` == "acute" ~ 1000, #endpoint refers to the lowest value for sgv derivation
      n_acute == 1 & n_chronic > 0 & `acute_chronic` == "acute" ~ 100,
      n_acute == 1 & n_chronic > 0 & `acute_chronic` == "chronic" &
        norm_cc_qualifier != "=" ~ 100,
      n_chronic == 1 &
        !endpoint %in% hc5_endpoints ~ 100,
      n_chronic > 1 & chronic_trophic_lvl == 1  &
        !endpoint %in% hc5_endpoints ~ 100,
      chronic_trophic_lvl == 2 &
        !endpoint %in% hc5_endpoints ~ 50,#norm_cc_qualifier refers to the lowest value for sgv derivation
      chronic_trophic_lvl >= 3 &
        !endpoint %in% hc5_endpoints ~ 10,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(sgv_mg_kg = lowest_norm_cc/(mf*af)) %>%
  select(-tier)

# final_sgv_table now has:
# editor, cas, as_name, sgv_mg_kg, scientific_name, endpoint,
# reliability, relevance, norm_cc_qualifier, lowest_norm_cc,
# norm_cc_unit, af and mf

rm(list = setdiff(ls() , c("ah_sgv", "tox", "tox_subs")))

#check if substances got lost
tox_subs_sgv <- distinct(ah_sgv, as_name, cas)

message("During the ad hoc derivation, these substances got lost:")
setdiff(tox_subs, tox_subs_sgv)
message("Number of unique substances with ad hoc sgv is and no data gap ", 
        length(unique(ah_sgv$as_name[ah_sgv$lowest_tox_flag != "data gap"]))
)

#create output
wb <- createWorkbook("wb")
addWorksheet(wb, sheetName = "ad_hoc_sgv")
writeData(wb, sheet = "ad_hoc_sgv", x = ah_sgv)
saveWorkbook(wb, file = "./ad_hoc_sgv.xlsx", overwrite = TRUE)