remove_missing_vars=function(model,cols, vars_to_keep){
  
  model=as.formula(model) #coerces the model formula into a formula class object
  unpack(model_breakdown(model))
  
  filter_partial_matches=function(vec1, vec2){
    vec2[sapply(vec2, function(x) any(grepl(paste(vec1, collapse = "|"), x)))]
  }
  
  new.mod0=filter_partial_matches(vars_to_keep, mod.rhs)
  new.mod=paste(mod.lhs,paste(new.mod0,collapse='+'),
        sep='~')
  if(length(mod.rhs.interaction)>0){
    for(i in 1:length(mod.rhs.interaction))
      new.mod=gsub(gsub("\\*","\\\\+",mod.rhs.interaction[i]),mod.rhs.interaction[i],  new.mod)
  }
  return(as.formula(new.mod))
  
}