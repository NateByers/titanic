library(dplyr)

titanic <- readxl::read_excel("data/titanic3.xls")

process_dashboard_data <- function(dat, survival) {
  # dat <- titanic; survival <- "Yes"
  if(survival == "Yes") {
    survival <- 1
  } else if(survival == "No") {
    survival <- 0
  } else {
    survival <- 0:1
  }
  
  dat <- dat %>%
    dplyr::filter(survived %in% survival)
}