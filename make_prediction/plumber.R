
library(plumber)
library(survival)

#* @apiTitle Submodel Prediction API

#* @apiDescription API for making predictions using submodels.


  source("helper_scripts/submodels.R", local = T)
  source("helper_scripts/predict_submodels.R", local = T)
  source("helper_scripts/print_submodels.R", local = T)
  source("helper_scripts/model_breakdown.R", local = T)
  source("helper_scripts/unpack.R", local = T)
  source("helper_scripts/missingness_pattern.R", local = T)
  source("helper_scripts/remove_missing_vars.R", local = T)
  load("submodels_sev.rda")
  load("submodels_modsev.rda")
  load("submodels_pneum.rda")


#* Make predictions using predict.submodels
#* @param newdata JSON string containing the new data for prediction
#* @post /predict

function(newdata, res) {
  tryCatch({

    newdata <- as.data.frame(newdata)
    exac_modsev=predict.submodels(newdata, submodels.object_modsev, type="response")
    exac_sev=predict.submodels(newdata, submodels.object_sev, type="response")
    exac_pneum=predict.submodels(newdata, submodels.object_pneum, type="response")
    # #
    return(list("modsev"=exac_modsev,
    "sev"=exac_sev,
    "pneum"=exac_pneum))
    # return(exac_modsev)
  }, error = function(e) {
    res$status <- 400
    print(paste("Error:", e$message))
    return(list(error = "Something went wrong", details = e$message))
  })
}




