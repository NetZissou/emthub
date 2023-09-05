## code to prepare `DISEASE_DATA` dataset goes here

DISEASE_DATA <- get_ct_level_data()
DISEASE_OUTCOMES <-
  c(
    "Arthritis","Asthma","Cancer (excluding skin Cancer)","Chr Kidney Diease","Chronic obstructive pulmonary","Coronary Heart Disease","Depression","Diabetes","High BP","High Cholesterol","Obesity","Stroke"
  )

DISEASE_DATA_VARS <- c(
  "rank_Stroke","rank_Obesity","rank_Arthritis","rank_Depression","rank_Diabetes","rank_High.BP","rank_Asthma","rank_Chr.Kidney.Diease","rank_Coronary.Heart.Disease","rank_High.Cholesterol","rank_Cancer..excluding.skin.Cancer.","rank_Chronic.obstructive.pulmonary"
)

usethis::use_data(DISEASE_DATA_VARS, overwrite = TRUE)
usethis::use_data(DISEASE_OUTCOMES, overwrite = TRUE)

