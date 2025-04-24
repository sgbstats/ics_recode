
model_breakdown=function(model){
  model=as.character(as.formula(model))
  mod.lhs <- model[2]
  mod.rhs0 <- model[3]
  mod.rhs.interaction0 <- strsplit(mod.rhs0,'\\+')[[1]]
  mod.rhs <- strsplit(mod.rhs0,'\\+|\\*|\\:')[[1]] %>% stringr::str_squish()
  mod.rhs <- stringr::str_squish(mod.rhs[!(mod.rhs%in%c('','+','~'))])
  
  #getting the interaction terms
  mod.rhs.interaction0=mod.rhs.interaction0[which(grepl("\\*|\\:", mod.rhs.interaction0))]%>% stringr::str_squish()
  mod.rhs.interaction2=stringr::str_split(mod.rhs.interaction0, "\\*|\\:")
  mod.rhs.interaction=c()
  if(length(mod.rhs.interaction2)>0){
    for(j in 1:length(mod.rhs.interaction2)){
      for(i in length(mod.rhs.interaction2[[j]]):2){
        mod.rhs.interaction3=t(combn(mod.rhs.interaction2[[j]],i)) %>% as.data.frame()
        mod.rhs.interaction4 <- apply( mod.rhs.interaction3 , 1 , paste , collapse = "*" )
        mod.rhs.interaction=c(mod.rhs.interaction, mod.rhs.interaction4)
        
      }
    }
  }
  return(list("mod.rhs"=mod.rhs,
              "mod.lhs"=mod.lhs,
              "mod.rhs.interaction"=stringr::str_replace_all(mod.rhs.interaction, pattern = " ", repl=""),
              "mod.rhs.raw"=mod.rhs0))
}

