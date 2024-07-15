#' Select PC variables
#' 
#' @param x data.frame PC data
#' @export
select_pc_vars_18 <- function(x) {
  ID <- c("MRN", "NewMRN")
  time_vars <- c("CompletedDate", "AdmDate", "CurrentLOS")
  SH_items <- c("sh_count_lifetime", "sh_previousvisit", "sh_currentvisittotal")
  SH_resp_xtra <- c("SelfHarm3Day", "SelfHarmID", "SelfHarmDate") # drop these
  
  # Mental Health questionnaires:
  HoNOS_items <- grep("HON_", names(x), value = TRUE)
  IPQ_items <- grep("IPQA", names(x), value = TRUE)
  MHQ_items <- grep("MHQ", names(x), value = TRUE)
  Audit_item <- "Audit_tot"
  QOL_item <- "QOL_A_QOLTotal"
  # keep only the original WB10 items, drop those introduced in 2017
  WB10_adm_items<- c("WB10_A_ACT", "WB10_A_ANX", "WB10_A_CHR", "WB10_A_CLM", "WB10_A_DEP",
                     "WB10_A_FRR", "WB10_A_INT", "WB10_A_NCP", "WB10_A_SQ", "WB10_A_WOR")
  psyc_items <- c(HoNOS_items, IPQ_items, MHQ_items, Audit_item, QOL_item, "Wellbeing", "Distress", WB10_adm_items)
  
  wards <- grep("Ward", names(pc_data), value = TRUE)   # include or not????
  # demographic items:
  age <- "AgeAtAdmDate"
  marital_status <- c("Marital_Divorced", "Marital_Married.Defacto", "Marital_Separated",
                      "Marital_Single", "Marital_Widow.Widower")
  sex <- c("Sex_0", "Sex_F", "Sex_M")
  demog_items <- c(age, marital_status, sex)
  
  wellbeing_items <- c("WB10_CHR", "WB10_CLM", "WB10_ACT", "WB10_FRR", "WB10_INT")
  distress_items <- c("WB10_ANX", "WB10_DEP", "WB10_WOR", "WB10_SQ", "WB10_NCP")
  WB10_items <- c(wellbeing_items, distress_items)
  
  return(x[ , c("SelfHarmEvent", ID, SH_items, Audit_item, QOL_item, WB10_items)])
}
