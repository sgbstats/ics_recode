if(!require("httpuv")){
  install.packages("httpuv")
}

httpuv::runStaticServer("docs")