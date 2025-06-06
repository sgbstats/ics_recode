
library(plumber)
library(survival)
library(tidyverse)

#* @apiTitle Submodel Prediction API

#* @apiDescription API for making predictions using submodels.


source("helper_scripts/submodels.R", local = T)
source("helper_scripts/predict_submodels.R", local = T)
source("helper_scripts/print_submodels.R", local = T)
source("helper_scripts/model_breakdown.R", local = T)
source("helper_scripts/unpack.R", local = T)
source("helper_scripts/missingness_pattern.R", local = T)
source("helper_scripts/remove_missing_vars.R", local = T)
source("helper_scripts/targetrow.R", local = T)
load("submodels_sev.rda")
load("submodels_modsev.rda")
load("submodels_pneum.rda")
load("test_list.RDa")
# load("coefs.RDa")
coef_modsev=submodel.print(submodels.object_modsev)
coef_sev=submodel.print(submodels.object_sev)
coef_pneum=submodel.print(submodels.object_pneum)
interpretation=data.frame("var"=c("(Intercept)","age_imp","arm_ipdICS", "arm_ipdICS:eos_bl","eos_bl","exac_bl","fev1_bl","sexM","smoking_blCurrent"),
                          "interpretation"=c("Intercept", "Age", "ICS-Yes", "Eos Interaction", "Eos/10^9", "Per Exac", "FEV1/L", "Sex-Male", "Smoking-Current"))

test_list=build_nested_list(test_list_df, value_cols = 9:14)
#* Log the incoming request
#* @filter logger
function(req) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s %s\n",
              timestamp,
              req$REQUEST_METHOD,
              req$PATH_INFO))
  plumber::forward()
}

#* Make predictions using predict.submodels
#* @param newdata JSON string containing the new data for prediction
#* @post /predict

function(newdata, res) {
  tryCatch({
    
    newdata <- as.data.frame(newdata)
    exac_modsev=predict.submodels(newdata, submodels.object_modsev, type="response")
    exac_sev=predict.submodels(newdata, submodels.object_sev, type="response")
    exac_pneum=predict.submodels(newdata, submodels.object_pneum, type="response")
    
    unpack(missingness_pattern(newdata))
    coefs=coef_modsev[["coefficients"]] %>% rename("modsev"="coef") %>%
      merge(coef_sev[["coefficients"]] %>% rename("sev"="coef") ) %>%
      merge(coef_pneum[["coefficients"]] %>% rename("pneum"="coef") ) %>%
      mutate(pattern=as.character(pattern)) %>%
      filter(pattern==as.character(tmp.pattern[1])) %>%
      merge(interpretation) %>%
      dplyr::select(interpretation, modsev, sev, pneum) %>%
      mutate(modsev=sprintf("%.3f", modsev),
             sev=sprintf("%.3f", sev),
             pneum=sprintf("%.3f", pneum))
    
    # #
    return(list("modsev"=exac_modsev,
                "sev"=exac_sev,
                "pneum"=exac_pneum,
                "coefs"=coefs))
    # return(exac_modsev)
  }, error = function(e) {
    res$status <- 400
    print(paste("Error:", e$message))
    return(list(error = "Something went wrong", details = e$message))
  })
}


#* Make predictions using predict.submodels
#* @param newdata JSON string containing the new data for prediction
#* @post /predict2

function(newdata, res) {
  tryCatch({
    
    newdata <- as.data.frame(newdata) 
    matched_test=match_elements(original_list = original_list,
                                newdata %>% dplyr::select(names(original_list)) %>% 
                                  mutate(sex=as.character(sex),
                                         arm_ipd=as.character(arm_ipd),
                                         smoking_bl=as.character(smoking_bl),
                                         eos_bl=as.numeric(eos_bl),
                                         exac_bl=as.numeric(exac_bl),
                                         age_imp=as.numeric(age_imp)) %>% 
                                  mutate(trt_dur=1) %>% 
                                  as.list())
    
    predictions=get_data_from_nested_list(matched_test, test_list)
    
    unpack(missingness_pattern(newdata))
    coefs=coef_modsev[["coefficients"]] %>% rename("modsev"="coef") %>%
      merge(coef_sev[["coefficients"]] %>% rename("sev"="coef") ) %>%
      merge(coef_pneum[["coefficients"]] %>% rename("pneum"="coef") ) %>%
      mutate(pattern=as.character(pattern)) %>%
      filter(pattern==as.character(tmp.pattern[1])) %>%
      merge(interpretation) %>%
      dplyr::select(interpretation, modsev, sev, pneum) %>%
      mutate(modsev=sprintf("%.3f", modsev),
             sev=sprintf("%.3f", sev),
             pneum=sprintf("%.3f", pneum))
    
  
    predictions <- predictions %>%
      mutate(across(contains("fit"), ~ . * newdata$trt_dur))
    
    
    
    # #
    return(list("modsev"=predictions %>% dplyr::select(names(original_list), "fit"=ms_fit, "se.fit"=ms_se.fit), 
                "sev"=predictions %>% dplyr::select(names(original_list), "fit"=s_fit, "se.fit"=s_se.fit), 
                "pneum"=predictions %>% dplyr::select(names(original_list), "fit"=p_fit, "se.fit"=p_se.fit), 
                "coefs"=coefs))
    # return(exac_modsev)
  }, error = function(e) {
    res$status <- 400
    print(paste("Error:", e$message))
    return(list(error = "Something went wrong", details = e$message))
  })
}


