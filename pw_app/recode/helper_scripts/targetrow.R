
library(tidyr)
library(dplyr)

match_elements <- function(original_list, supplied_list) {
  
  if(class(supplied_list)=="data.frame"){
    supplied_list=as.list(supplied_list)
  }else if(class(supplied_list)!="list"){
    stop("supply list as list or dataframe")
  }
  # Check if both lists have the same names
  if (!all(names(original_list) %in% names(supplied_list)) || 
      !all(names(supplied_list) %in% names(original_list))) {
    warning("Lists don't have identical names")
  }
  
  result_list <- list()
  
  # Process each element in the original list
  for (name in names(original_list)) {
    original_vec <- original_list[[name]]
    supplied_vec <- supplied_list[[name]]
    
    # Skip if the name doesn't exist in supplied list
    if (is.null(supplied_vec)) {
      result_list[[name]] <- rep(NA, length(original_vec))
      next
    }
    
    # Check if vector types match
    if (class(original_vec) != class(supplied_vec)) {
      warning(paste("Type mismatch for element:", name))
      result_list[[name]] <- rep(NA, length(original_vec))
      next
    }
    
    # Handle numeric vectors
    if (is.numeric(original_vec)) {
      result <- sapply(supplied_vec, function(val) {
        if (is.na(val)) {
          return(NA)
        } else {
          # Find closest value
          closest_idx <- which.min(abs(original_vec - val))
          return(original_vec[closest_idx])
        }
      })
      result_list[[name]] <- result
    }
    # Handle character vectors
    else if (is.character(original_vec)) {
      result <- sapply(supplied_vec, function(val) {
        if (val %in% original_vec) {
          return(val)
        } else {
          return(NA)
        }
      })
      result_list[[name]] <- result
    }
    # Any other type, just return NA
    else {
      result_list[[name]] <- rep(NA, length(supplied_vec))
    }
  }
  
  return(result_list)
}

library(tidyverse)

# Function to build nested lists efficiently
build_nested_list <- function(df, col_start = 1, col_end = 8, value_cols = 9:10) {
  # Base case: if we've gone through all hierarchy columns
  if (col_start > col_end) {
    return(df[, value_cols, drop = FALSE])
  }
  
  # Split the data by current column
  grouped <- split(df, df[[col_start]])
  
  # Recursively process each group
  result <- map(grouped, ~ build_nested_list(.x, col_start + 1, col_end, value_cols))
  
  
  result[["vars"]] <- names(df)[col_start:col_end]
  result[["values"]] <- names(df)[value_cols] 
  return(result)
}


get_data_from_nested_list=function(new, list, return_input=T){
  new2=new %>%  as.data.frame() %>% 
    dplyr::select(list$vars)%>% 
    mutate(across(1:length(list$vars), ~ifelse(is.na(.), "NA", as.character(.))))
  out=matrix(ncol=length(list$values), nrow=0) %>% as.data.frame()
  names(out)=list$values
  for(i in 1:nrow(new2)){
    x=list
    for(j in 1:length(list$vars)){
      x=x[[new2[i,j]]]
    }
    out=out %>% rbind.data.frame(x)
  }
  if(return_input){
    return(cbind(new, out))
  }else{
    return(out)
  }
}


