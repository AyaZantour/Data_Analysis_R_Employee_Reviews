#library(future)
library(future.apply)
plan(multisession, workers=16)

index <- dir(pattern=".*[.]qmd")
qmd <- dir(path="qmd", pattern=".*[.]qmd")


ret0 %<-% system(paste0("quarto render ", index), intern = TRUE)

compile <- function(file) {
  list(file = file,
       ret = system(paste0("quarto render ", file), wait = TRUE)
  )
}


system.time(ret <- future_sapply(paste0("qmd/", qmd), compile))
