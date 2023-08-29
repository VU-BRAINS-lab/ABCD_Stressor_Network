## Wrote an R function for FDR Corrections because there are many to run. First it reads in the mplus out file. 
## Then it pulls the pvalues for the various networks for that metric and performs an FDR Correction on them
## Then it saves them into an excel based on the four dimensions. 
## NOTE This script labels the networks in this order: SC, Motor, MF, FP, DM, V2, V1, VA - so if your MPlus script labels the networks in this order, then it will be consistent, if not then you will have to change the MPlus script or this script to reflect the order of your networks 

## Run this script to save the function. When it runs, FDR_correct_shen(output) will be the function name/ syntax.
## 'output' should be replaced with the MPlus out file name without the .out (ex. if the MPlus script is called  Mid10_ASP.out, output should be set to "Mid10_ASP" WITH quotation marks)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(MplusAutomation)
#FDR_correct_shen <- function(output) {
  
  output="Rest30_SWO_SecondOrder_Subfactors_fixed"
  model = paste(output,".out",sep="")
  model
  modelResults <- readModels(model)
  
  ######### read standardized results ############
  standardizedResults <- modelResults[["parameters"]][["stdyx.standardized"]]
  
  # read the index of regression results (can find in "paramHeader" column ending in ".ON")
  RegI=grep(".ON", standardizedResults$paramHeader)
  
  # read the index of regression results with "GENERAL" factor (can find in "param" column)
  # GenI=grep("GENERAL", standardizedResults$param)
  # read the index of regression results with "FAMHX" factor
  FamI=grep("FAMHX", standardizedResults$param)
  # read the index of regression results with "CAREGVR" factor
  CvgI=grep("CAREGVR", standardizedResults$param)
  # read the index of regression results with "SES" factor
  SesI=grep("SES", standardizedResults$param)
  # read the index of regression results with "ENVIRON" factor
  EnvI=grep("ENVIRON", standardizedResults$param)
  # read the index of regression results with "pfactor" factor
  
  # Find the index of regression results for each factor
  # RegGen=intersect(RegI, GenI)
  RegFam=intersect(RegI, FamI)
  RegCvg=intersect(RegI, CvgI)
  RegSes=intersect(RegI, SesI)
  RegEnv=intersect(RegI, EnvI)
  
  # Find the p value of regression results for each factor
  # Gen_p=standardizedResults$pval[RegGen]
  Fam_p=standardizedResults$pval[RegFam]
  Cvg_p=standardizedResults$pval[RegCvg]
  Ses_p=standardizedResults$pval[RegSes]
  Env_p=standardizedResults$pval[RegEnv]
  
  # Adjust length if needed by adding -# at the end if you want to cut off some numbers
  # Gen_p <- Gen_p[1:(length(Gen_p))]
  Fam_p <- Fam_p[1:(length(Fam_p))]
  Cvg_p <- Cvg_p[1:(length(Cvg_p))]
  Ses_p <- Ses_p[1:(length(Ses_p))]
  Env_p <- Env_p[1:(length(Env_p))]
  
  # Perform fdr
  # Gen_p_fdr <- p.adjust(Gen_p, method="fdr")
  Fam_p_fdr <- p.adjust(Fam_p, method="fdr")
  Cvg_p_fdr <- p.adjust(Cvg_p, method="fdr")
  Ses_p_fdr <- p.adjust(Ses_p, method="fdr")
  Env_p_fdr <- p.adjust(Env_p, method="fdr")
  
  #Convert to data frame
  # Gen_p_fdr <- as.data.frame(Gen_p_fdr)
  Fam_p_fdr <- as.data.frame(Fam_p_fdr)
  Cvg_p_fdr <- as.data.frame(Cvg_p_fdr)
  Ses_p_fdr <- as.data.frame(Ses_p_fdr)
  Env_p_fdr <- as.data.frame(Env_p_fdr)
  
  #Print fdr-corrected p-values to three decimal places
  # Gen_p_fdr_round <- round(Gen_p_fdr,3)
  Fam_p_fdr_round <- round(Fam_p_fdr,3)
  Cvg_p_fdr_round <- round(Cvg_p_fdr,3)
  Ses_p_fdr_round <- round(Ses_p_fdr,3)
  Env_p_fdr_round <- round(Env_p_fdr,3)
  
  #create dataframe to store Gen data
  # Gen <- data.frame(
  #   Factor = "General",
  #   est = standardizedResults$est[RegGen],
  #   se = standardizedResults$se[RegGen],
  #   est_se = standardizedResults$est_se[RegGen],
  #   pval = standardizedResults$pval[RegGen],
  #   FDR_pval = Gen_p_fdr_round,
  #   stringsAsFactors = FALSE
  # )
  # Gen[1,1] = "10%"
  # Gen[2,1] = "17%"
  # Gen[3,1] = "23%"
  # Gen[4,1] = "30%"
  
  
  #create dataframe to store FamHX data
  Fam <- data.frame(
    Factor = "FamHX",
    est = standardizedResults$est[RegFam],
    se = standardizedResults$se[RegFam],
    est_se = standardizedResults$est_se[RegFam],
    pval = standardizedResults$pval[RegFam],
    FDR_pval = Fam_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Fam[1,1] = "SC"
  Fam[2,1] = "Motor"
  Fam[3,1] = "MF"
  Fam[4,1] = "FP"
  Fam[5,1] = "DM"
  Fam[6,1] = "V2"
  Fam[7,1] = "V1"
  Fam[8,1] = "VA"
  
  
  #create dataframe to store CAREGVR data
  Cvg <- data.frame(
    Factor = "CAREGVR",
    est = standardizedResults$est[RegCvg],
    se = standardizedResults$se[RegCvg],
    est_se = standardizedResults$est_se[RegCvg],
    pval = standardizedResults$pval[RegCvg],
    FDR_pval = Cvg_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Cvg[1,1] = "SC"
  Cvg[2,1] = "Motor"
  Cvg[3,1] = "MF"
  Cvg[4,1] = "FP"
  Cvg[5,1] = "DM"
  Cvg[6,1] = "V2"
  Cvg[7,1] = "V1"
  Cvg[8,1] = "VA"

  
  #create dataframe to store SES data
  Ses <- data.frame(
    Factor = "SES",
    est = standardizedResults$est[RegSes],
    se = standardizedResults$se[RegSes],
    est_se = standardizedResults$est_se[RegSes],
    pval = standardizedResults$pval[RegSes],
    FDR_pval = Ses_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Ses[1,1] = "SC"
  Ses[2,1] = "Motor"
  Ses[3,1] = "MF"
  Ses[4,1] = "FP"
  Ses[5,1] = "DM"
  Ses[6,1] = "V2"
  Ses[7,1] = "V1"
  Ses[8,1] = "VA"
  
  
  #create dataframe to store ENVIRON data
  Env <- data.frame(
    Factor = "ENVIRON",
    est = standardizedResults$est[RegEnv],
    se = standardizedResults$se[RegEnv],
    est_se = standardizedResults$est_se[RegEnv],
    pval = standardizedResults$pval[RegEnv],
    FDR_pval = Env_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Env[1,1] = "SC"
  Env[2,1] = "Motor"
  Env[3,1] = "MF"
  Env[4,1] = "FP"
  Env[5,1] = "DM"
  Env[6,1] = "V2"
  Env[7,1] = "V1"
  Env[8,1] = "VA"
  
  
  ######## Saving Files #########
  
  # GeneralOutput = paste("General_",output,"_FDR.csv",sep = "")
  # GeneralOutput
  FamilyDynamicOutput = paste("FamilyDynamic_",output,"_FDR.csv",sep = "")
  FamilyDynamicOutput  
  InterpersonalSupportOutput = paste("InterpersonalSupport_",output,"_FDR.csv",sep = "")
  InterpersonalSupportOutput
  NeighborhoodSESOutput = paste("NeighborhoodSES_",output,"_FDR.csv",sep = "")
  NeighborhoodSESOutput
  UrbanicityOutput = paste("Urbanicity_",output,"_FDR.csv",sep = "")
  UrbanicityOutput
  
  
  # write.table(Gen, GeneralOutput, quote = FALSE, sep = ",", row.names=FALSE)
  write.table(Fam, FamilyDynamicOutput, quote = FALSE, sep = ",", row.names=FALSE)
  write.table(Cvg, InterpersonalSupportOutput, quote = FALSE, sep = ",", row.names=FALSE)
  write.table(Ses, NeighborhoodSESOutput, quote = FALSE, sep = ",", row.names=FALSE)
  write.table(Env, UrbanicityOutput, quote = FALSE, sep = ",", row.names=FALSE)

  remove(list = ls())
  
#}

  