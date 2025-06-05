library(tidyverse)
predict_glm_nb <- function(object, newdata, type = c("link", "response")) {
  type <- match.arg(type, c("link", "response"))
  
  # Check required components
  if (is.null(object$coefficients) || is.null(object$terms)) {
    stop("Object must contain 'coefficients' and 'terms' components")
  }
  
  # Get model terms and create model matrix
  terms_obj <- object$terms
  
  # Check for response variable and add dummy if missing
  response_var <- all.vars(terms_obj)[attr(terms_obj, "response")]
  if (length(response_var) > 0 && !response_var %in% names(newdata)) {
    # Add dummy response variable with NA values
    newdata[[response_var]] <- NA
  }
  
  # Handle factor levels if xlevels is available
  if (!is.null(object$xlevels)) {
    # Ensure factor levels in newdata match the original model
    for (var in names(object$xlevels)) {
      if (var %in% names(newdata)) {
        # Convert to factor with correct levels regardless of current type
        newdata[[var]] <- factor(newdata[[var]], levels = object$xlevels[[var]])
      }
    }
  }
  
  
  # Create model matrix
  # Use delete.response() to remove response from terms for prediction
  pred_terms <- delete.response(terms_obj)
  
  # Try to create model matrix with error handling
  tryCatch({
    X <- model.matrix(pred_terms, data = newdata)
  }, error = function(e) {
    # If that fails, try with original terms but ignore response
    tryCatch({
      X <<- model.matrix(terms_obj, data = newdata)
    }, error = function(e2) {
      stop("Failed to create model matrix. Error: ", e2$message, 
           "\nCheck that all required variables are in newdata and have correct types.")
    })
  })
  
  # Check if model matrix is empty
  if (nrow(X) == 0) {
    stop("Model matrix has no rows. Check that newdata is not empty and contains required variables.")
  }
  if (ncol(X) == 0) {
    stop("Model matrix has no columns. Check that model terms are properly specified.")
  }
  
  # Check coefficient alignment
  coef_names <- names(object$coefficients)
  X_names <- colnames(X)
  
  # Match coefficients to model matrix columns
  if (!all(coef_names %in% X_names)) {
    missing_coefs <- coef_names[!coef_names %in% X_names]
    stop("Missing variables in newdata for coefficients: ", paste(missing_coefs, collapse = ", "))
  }
  
  # Align model matrix with coefficients
  X <- X[, coef_names, drop = FALSE]
  
  # Calculate linear predictor
  eta <- as.vector(X %*% object$coefficients)
  
  # Add offset if present
  if (!is.null(object$offset)) {
    # If offset is stored in the object
    if (length(object$offset) == nrow(X)) {
      eta <- eta + object$offset
    } else {
      warning("Stored offset length doesn't match prediction data")
    }
  } else {
    # Check if offset is specified in terms and present in newdata
    offset_var <- attr(terms_obj, "offset")
    if (!is.null(offset_var)) {
      offset_name <- all.vars(terms_obj)[offset_var]
      if (offset_name %in% names(newdata)) {
        eta <- eta + newdata[[offset_name]]
      } else {
        # Check if offset() function was used in formula
        offset_terms <- attr(terms_obj, "variables")
        offset_calls <- grep("offset\\(", offset_terms, value = TRUE)
        if (length(offset_calls) > 0) {
          # Extract variable name from offset() call
          offset_var_name <- gsub(".*offset\\(([^)]+)\\).*", "\\1", offset_calls[1])
          if (offset_var_name %in% names(newdata)) {
            eta <- eta + newdata[[offset_var_name]]
          } else {
            warning("Offset variable '", offset_var_name, "' not found in newdata")
          }
        }
      }
    }
  }
  
  # Return based on type
  if (type == "link") {
    return(eta)
  } else {
    # For negative binomial, response is exp(eta)
    return(exp(eta))
  }
}


predict.glm.nb.submodel=function(newdata, submodels.object.smol, type=c("response", "link"), retain_lhs=FALSE,include_mp=FALSE, ...){
  type=match.arg(type, c("response", "link"))
  unpack(model_breakdown(submodels.object.smol[["meta"]][["model"]]))
  
  #keeping the correct bits of data.
  mod.data<-newdata %>% dplyr::select(any_of(c(mod.lhs, submodels.object.smol[["meta"]][["vars"]]))) %>% 
    tidyr::drop_na(all_of(submodels.object.smol[["meta"]][["force_variables"]]))
  
  sdata<- mod.data
  
  #adding missing cols
  missing_vars=submodels.object.smol[["meta"]][["vars"]][!submodels.object.smol[["meta"]][["vars"]] %in% names(sdata)]
  
  for(i in missing_vars){
    sdata[,i] <- NA
  }
  
  # getting the missingness patterns and setting up the data ready.
  unpack(missingness_pattern(sdata %>% dplyr::select(-any_of(mod.lhs))))
  mp.info=submodels.object.smol[["meta"]][["mp.info"]]# %>% filter(mp %in% mp.info$mp)
  
  pred.out=sdata[0,]
  
  pred.out$fit=numeric(0)
  for(i in 1:nrow(mp.info)){
    newdata_mp=sdata[tmp.info[[mp.info$mp[i]]],]
    if(nrow(newdata_mp)==0){next}
    
    pred=predict_glm_nb(submodels.object.smol[[i]][["mod"]],newdata_mp, type=type)
    #     
    # predict(submodels.object_modsev[[i]][["mod"]],newdata_mp, type="response")
    newdata_mp$fit=pred
    if(include_mp){
      newdata_mp$mp=mp.info$mp[i]
    }
    pred.out=pred.out %>% rbind(newdata_mp)
  }
  if(!retain_lhs){
    pred.out=pred.out %>% dplyr::select(-any_of(mod.lhs))
  }
  
  return(pred.out)
  
  
}
