suppressWarnings({
  
pkgs <- c("tidyverse", "lubridate", "readxl", "highcharter", "knitr", "tidyquant", "timetk", "tibbletime", "quantmod", "PerformanceAnalytics", "scales")

sapply(pkgs, function(pkg) if(!require(pkg, character.only = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org/"))

sapply(pkgs, library, character.only = TRUE)

filenames <- list.files(pattern = "*.csv")

prices <- filenames %>%
  map(~read_csv(.,
                col_types =
                  cols(Date =
                         col_date(format = "%Y/%m/%d")),
                na = c("", "#N/A", "NA"))) %>%
  bind_cols() %>%
  rename(date = `Date...1`) %>%
  select(date, everything(),-contains("...")) %>%
  tk_xts(date_var = date)

symbols <- substr(filenames, 1, 3)

})
