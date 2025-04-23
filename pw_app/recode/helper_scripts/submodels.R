#' @data the training data as a data frame
#' @model the formula of the model as a formula or coercible to formula
#' @model_fn the function of the model, currently, we know that lm glm, amd MASS::glm.nb work
#' @submodel_type a choice between psm and ccsm
#' @force_variables a vector of strings with varaibles that have to be in the model and drops any obs that have a missing force_variable
#' @remove_model_data vector of things that need to be removed from the model data because of vivli
#' @threshold_multiplier sets the threshold for how small the sample can be, default is 2 the total threshold is multiplier times number of variables
#' @... arguments passed as necessary

submodels<- function(data, model, model_fn,submodel_type=c("psm", "ccsm"), force_variables=c(),threshold_multiplier=2,remove_model_data=c("model", "y", "fitted"), ...){
  
  #house keeping
  submodel_type=submodel_type[1] #picks the first of submodels
  if(!submodel_type %in% c("psm", "ccsm")){
    stop("psm or ccsm required")
  }
  
  # breakdown the model see helper functions
  unpack(model_breakdown(model))
  
  model=as.formula(model) #coerces the model formula into a formula class object
  mod.data<- get_all_vars(model, data=data) %>%
    drop_na(all_of(mod.lhs)) %>%
    drop_na(all_of(force_variables))
  
  model_names=names(mod.data)
  
  sdata <- mod.data[,-1] #remove the outcome 
  var_names=names(sdata) 
  unpack(missingness_pattern(sdata)) #gets the missingness patterns
  
  all.patterns0=expand.grid(rep(list(0:1),ncol(sdata))) 
  
  names(all.patterns0)=var_names #makes every combination of missingness
  
  #stops the computing of models that would have dropped the force variables.
  remove=intersect(var_names, force_variables)
  all.patterns1=all.patterns0[rowSums(as.matrix(all.patterns0[,remove]==1))==0,]
  all.patterns <- factor(apply(all.patterns1,1,function(z) paste(z,collapse="")))
  obs.patterns <- unique(tmp.pattern)
  
  # add the empty patterns into the list of things to do
  if(length(setdiff(all.patterns,obs.patterns)) == 0){
    empty.patterns = NULL
  } else {
    empty.patterns <- data.frame(mp = factor(setdiff(all.patterns,obs.patterns)), n=0)
  }	
  mp.info <- rbind(mp.info,empty.patterns)
  
  
  
  # sets the threshold
  threshold <- (length(mod.rhs)+length(mod.rhs.interaction))*threshold_multiplier
  
  # sets whether the pattern uses a ccs or pms
  mp.info$use.psm <- (as.numeric(mp.info$n)>=threshold)*1
  
  if(mp.info$use.psm[1]==0){
    stop("Not enough data for the full model")
  }
  
  reg.out <- vector('list', length(all.patterns))
  names(reg.out) <- mp.info$mp.info
  
  
  for(i in seq(nrow(mp.info))) {
    col.keep  <- which(strsplit(mp.info$mp[i],'')[[1]]=='0')
    if(length(col.keep)==0){
      new.mod <- paste(mod.lhs,1,sep='~')
    } else {
      cols=names(mod.data)[names(mod.data)!=mod.lhs]
      vars_to_keep=cols[col.keep]
      
      new.mod=remove_missing_vars(model, cols, vars_to_keep)
    }
    
    # makes sure that the right model is used so it will always use the complete case data if ccsm is chosen and then use ccsm if there is insufficient data points.
    if(mp.info$use.psm[i]==1&submodel_type=="psm"){
      
      reg.out[[i]] <- list(pattern=vars_to_keep,
                         mp=mp.info$mp[i],
                         mod=model_fn(formula=new.mod,data=mod.data[tmp.info[[i]],]))
    }else{
      
      reg.out[[i]] <- list(pattern = vars_to_keep,
                         mp=mp.info$mp[i],
                         mod=model_fn(formula=new.mod,data=mod.data))
      
    }
    #this is a fix for vivli
    for(i in remove_model_data){
      reg.out[i][["mod"]][[i]]=NULL
    }
  }
  reg.out[["meta"]][["model"]]=model
  reg.out[["meta"]][["vars"]]=var_names
  reg.out[["meta"]][["force_variables"]]=force_variables
  reg.out[["meta"]][["mp.info"]]=mp.info
  reg.out[["meta"]][["all.patterns"]]=all.patterns
  reg.out[["meta"]][["submodel_type"]]=submodel_type
  reg.out[["meta"]][["threshold"]]=threshold
  
  return(reg.out)
}
