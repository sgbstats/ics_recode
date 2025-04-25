#' @prediction.data the data used for testing
#' @submodels.object from submodels
#' @se.fit if the SE of prediction is to be used.
#' @retain_lhs if the true outcome variable is to be be kept in the output.
#' @include_mp if the mssingness pattern is to be included in the output data frame. 
#' @... arguments passed as necessary

predict.submodels <- function(newdata, submodels.object,se.fit=TRUE,retain_lhs=FALSE,include_mp=FALSE, ...){
  
  #break down the model as passsed to the first argument.
  unpack(model_breakdown(submodels.object[["meta"]][["model"]]))
  
  #keeping the correct bits of data.
  mod.data<-newdata %>% dplyr::select(any_of(c(mod.lhs, submodels.object[["meta"]][["vars"]]))) %>% 
    tidyr::drop_na(all_of(submodels.object[["meta"]][["force_variables"]]))
  
  #its useful to have a copy
  sdata<- mod.data
  
  #adding missing cols
  missing_vars=submodels.object[["meta"]][["vars"]][!submodels.object[["meta"]][["vars"]] %in% names(sdata)]
  
  for(i in missing_vars){
    sdata[,i] <- NA
  }
  
  # getting the missingness patterns and setting up the data ready.
  unpack(missingness_pattern(sdata %>% dplyr::select(-any_of(mod.lhs))))
  mp.info=submodels.object[["meta"]][["mp.info"]]# %>% filter(mp %in% mp.info$mp)
  
  pred.out=sdata[0,]
  pred.out$fit=numeric(0)
  if(se.fit){
    pred.out$se.fit=numeric(0)
  }
  if(include_mp){
    pred.out$mp=character(0)
  }
  
  # predicting for each missing pattern
  for(i in 1:nrow(mp.info)){
    newdata_mp=sdata[tmp.info[[mp.info$mp[i]]],]
    if(nrow(newdata_mp)==0){next}
    pred=predict(submodels.object[[i]][["mod"]], newdata=newdata_mp, se.fit=se.fit,...)
    if(se.fit){
      newdata_mp$fit=pred$fit 
      newdata_mp$se.fit=pred$se.fit
    }else{
      newdata_mp$fit=pred.out
    }
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
