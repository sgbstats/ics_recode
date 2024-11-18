library(googledrive)

drive_deauth()
temp <- tempfile(fileext = ".zip")
dl <- drive_download(
  "https://drive.google.com/file/d/134g7LCeV8rs75ibU4lSVYtuRJQOqfs4y/view?usp=sharing", path = paste0(getwd(),"\\Data\\data.zip"), overwrite = TRUE)
out <- unzip(paste0(getwd(),"\\Data\\data.zip"), exdir=paste0(getwd(),"\\Data"))
load(out[1])
load(out[2])
load(out[3])


dl=drive_download("https://drive.google.com/file/d/1S3S06P1oMjd9DpLaotRazH5awEvFwdLG/view?usp=drive_link", path = "Data\\imp_aggregated_results.RDa", overwrite = T)
dl2=drive_download("https://drive.google.com/file/d/1J7e6skxB7KzMzigEdwDdGI5iOLjKhAh3/view?usp=sharing", path = "Data\\vars.RDa", overwrite = T)
