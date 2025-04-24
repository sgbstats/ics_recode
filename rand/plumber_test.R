
library(httr)
library(tidyverse)
library(jsonlite)

api_url <- "http://127.0.0.1:8000/predict"




test_data=tribble(~"arm_ipd", ~"eos_bl",~"age_imp",~"exac_bl",~"sex",~"smoking_bl",~"trt_dur",~"fev1_bl",
                   "ICS", NA_integer_, 65, 1, "M", "Former",  1,0.8,
                   "Control", NA_integer_, 65, 1, "M", "Former",  1, 0.8)

# Make the API request


response <- httr::POST(api_url, body = list(newdata=as.list(test_data)), 
                       encode = "json")


# Check the response status
if (http_status(response)$category == "Success") {
  cat("API request successful!\n")
  
  # Print the content
  content <- content(response, "text")
  cat("Response content:\n")
  cat(content)
} else {
  cat("API request failed!\n")
  cat("Status code:", status_code(response), "\n")
  cat("Error message:", content(response, "text"), "\n")
}

fromJSON(content)

# Test data
test_data1=tribble(~"arm_ipd", ~"eos_bl",~"age_imp",~"exac_bl",~"sex",~"smoking_bl",~"trt_dur",~"fev1_bl",
                   "Control", 0.2, 65, 1, "M", "Former",  1, 0.8)

test_data2=tribble(~"arm_ipd", ~"eos_bl",~"age_imp",~"exac_bl",~"sex",~"smoking_bl",~"trt_dur",~"fev1_bl",
                   
                   "Control", 0.2, 65, 1, "M", "Former",  1, 0.8)
newdata=jsonlite::toJSON(list(arm_ipd = test_data$arm_ipd, 
                              eos_bl = test_data$eos_bl,
                              age_imp = test_data$age_imp,
                              exac_bl = test_data$exac_bl,
                              sex = test_data$sex,
                              smoking_bl = test_data$smoking_bl,
                              trt_dur = test_data$trt_dur,
                              fev1_bl = test_data$fev1_bl))
writeLines(newdata, "rand/json")
