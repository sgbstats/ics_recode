missingness_pattern=function(SDATA){
  tmp.dat      <- as.data.frame(is.na(SDATA)*1)
  tmp.pattern  <- factor(apply(tmp.dat,1,function(z) paste(z,collapse="")))
  tmp.info     <- split(seq(nrow(SDATA)), tmp.pattern)
  mp.levels    <- levels(tmp.pattern)
  mp.pattern   <- do.call(rbind, lapply(as.list(mp.levels),function(ZZ) strsplit(ZZ,'')[[1]])) 		
  mp.info     <- data.frame(cbind(names(tmp.info), unlist(lapply(tmp.info, length))),
                            stringsAsFactors= FALSE)
  rownames(mp.info) <- seq(nrow(mp.info))
  colnames(mp.info) <- c('mp','n')
  
  return(list("tmp.dat"=tmp.dat, "tmp.pattern"=tmp.pattern, "tmp.info"=tmp.info, "mp.levels"=mp.levels, "mp.pattern"=mp.pattern, "mp.info"=mp.info))
}
