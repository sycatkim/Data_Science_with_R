# this is very slightly edited code from library(blscrapeR). 
#   there is a big problem in the dates...
library(lubridate)
get_bls_county = function (date_mth = NULL, stateName = NULL, ...) 
{
  countyemp = contyemp = fips_state = V1 = V2 = V3 = V4 = V5 = V6 = V7 = V8 = V9 = period = i = unemployed = employed = labor_force = NULL
  state_fips <- blscrapeR::state_fips
  target <- blscrapeR::urlExists("https://www.bls.gov/web/metro/laucntycur14.txt")
  if (!isTRUE(target)) {
    message("Woops, looks like 'https://www.bls.gov/web/metro/laucntycur14.txt' is unavailable right now!")
  }
  else {
    temp <- tempfile()
    download.file("https://www.bls.gov/web/metro/laucntycur14.txt", 
                  temp)
  }
  countyemp <- read.csv(temp, fill = T, header = F, sep = "|", 
                        skip = 6, stringsAsFactors = F, strip.white = T) %>% 
    dplyr::rename(area_code = V1, fips_state = V2, fips_county = V3, 
                  area_title = V4, period = V5, labor_force = V6, 
                  employed = V7, unemployed = V8, unemployed_rate = V9) %>% 
    na.omit() %>% dplyr::mutate(period = as_date(paste("01-", 
                                                       period, sep = "")))
  countyemp$fips_county <- formatC(countyemp$fips_county, 
                                   width = 3, format = "d", flag = "0")
  countyemp$fips_state <- formatC(countyemp$fips_state, width = 2, 
                                  format = "d", flag = "0")
  countyemp$fips <- paste(countyemp$fips_state, countyemp$fips_county, 
                          sep = "")
  unlink(temp)
  if (!is.null(stateName)) {
    state_check <- purrr::map_lgl(stateName, function(x) any(grepl(x, 
                                                                   state_fips$state)))
    if (any(state_check == FALSE)) {
      stop(message("Please make sure you state names are spelled correctly using full state names."))
    }
    state_rows <- purrr::map_int(stateName, function(x) grep(x, 
                                                             state_fips$state))
    state_selection <- state_fips$fips_state[state_rows]
    statelist <- purrr::map(state_selection, function(s) {
      state_vals <- subset(countyemp, fips_state == s)
    })
    countyemp <- do.call(rbind, statelist)
  }
  if (!is.null(date_mth)) {
    date_mth <- as_date(paste("01", date_mth, sep = ""))
    dt_exist <- sapply(date_mth, function(x) any(grepl(x, 
                                                       countyemp$period)))
    if (any(dt_exist == FALSE)) {
      message("Are you sure your date(s) is published? Please check the BLS release schedule.")
      if (i > Sys.Date() - 54) {
        stop(message("County-wide statistics are usually published on the third Friday of each month for the previous month."))
      }
      if (i < Sys.Date() - 360) {
        stop(message("This data set only goes back one year. Make sure your date(s) is correct."))
      }
    }
  }
  if (is.null(date_mth)) {
    date_mth <- max(countyemp$period)
    date_mth <- as_date(date_mth)
  }
  datalist <- purrr::map(date_mth, function(i) {
    mth_vals <- subset(countyemp, period == i)
  })
  df <- do.call(rbind, datalist)
  df %<>% dplyr::mutate(unemployed = as.numeric(gsub(",", 
                                                     "", as.character(unemployed))), employed = as.numeric(gsub(",", 
                                                                                                                "", as.character(employed))), labor_force = as.numeric(gsub(",", 
                                                                                                                                                                            "", as.character(labor_force)))) %>% tibble::as_tibble()
  return(df)
}