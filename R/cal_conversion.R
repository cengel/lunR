#' Lunar Calendar conversion
#'
#' Convert Lunar date to Gregorian date
#' @param lunar_date a string with the lunar date in the following format "YYYY-MM-DD "or "YYYY-MML-DD", if it is a leap month add "L" after the month MM
#' @return The Gregorian date as a string
#' @examples 
#' lunar_to_gregorian("1888-12-24");
#' lunar_to_gregorian(c("1888-12-24", "1966-03L-12"));
#' @export lunar_to_gregorian
#' @importFrom stringr str_detect 
#' @importFrom stringr str_split_fixed 
#' @importFrom stringr str_ends 
#' @importFrom stringr str_sub 
#' @importFrom dplyr %>% 
#' @importFrom dplyr filter 
#' @importFrom dplyr select 
#' @importFrom dplyr bind_cols
#' @importFrom tidyselect ends_with
#' @importFrom lubridate ymd
#' @importFrom purrr pmap_dbl
#' @importFrom tibble tibble
#' 

lunar_to_gregorian <- function(lunar_date){
  
  # check for object type
  if (!is.character(lunar_date)) {
    stop("lunar date needs to be a string in YYYY-MM-DD or YYYY-MML-DD format")
  }
  
  # check for string format
  # using sledgehammer here - be more detailed in error msg using which
  if (!any(str_detect(lunar_date, "^\\d{4}-\\d{2}L?-\\d{2}$"), na.rm = T)) {
    stop("lunar date needs to be in YYYY-MM-DD or YYYY-MML-DD format")
  }
  
  # get the components
  yr <- str_split_fixed(lunar_date, "-", 3)[,1] 
  mo_l <- str_split_fixed(lunar_date, "-", 3)[,2] # note this can be 2 or 3 characters
  leap_month_flag <- str_ends(mo_l, "L")
  mo <- as.numeric(str_sub(mo_l, 1, 2))
  dy <- as.numeric(str_split_fixed(lunar_date, "-", 3)[,3])
  
  # check for 'reasonable' values - can be more detailed in error msg using which
  if(any(mo > 12 | mo < 1, na.rm = T)) {
    stop("month needs to be between 1 and 12")
  }
  if(any(dy > 30 | dy < 1, na.rm = T)) {
    stop("day needs to be between 1 and 30")
  }
  
  # get first day of month for each value
  tib <- tibble(yr = yr, mo = mo, dy=dy, leap_month_flag = leap_month_flag)
  first_day_of_month <- pmap_dbl(tib, get_first_day_of_month)

  # start with gregorian new year
  gregorian_ny <- ymd(paste0(yr, "-01-01")) - 1 # we need to start counting before Jan 1st
  
  # add the days until the relevant month and the remaining days
  gregorian_date <- gregorian_ny + first_day_of_month  + dy - 1 # we need to start counting before the first of month

  # check if the date is before Gregorian Calendar was implemented
  # use which to be more specific
  if (any(gregorian_date < ymd("1552-10-15"), na.rm = T)){
    warning("Gregorian Calendar was implemented 15 October 1582")
  }
  return (as.character(gregorian_date))
}


get_first_day_of_month <- function(yr, mo, dy, leap_month_flag){
  
  if (is.na(yr) | yr == "") {return(NA)}
  
  ## BEGIN MAKE CALENDAR
  # extract the row for year and all months
  calendar <- filter(conversion_table, year == yr) 
  months_only <- select(calendar, first_day_month1:first_day_month12)
  
  # check if year has a leap month and if so, insert into the sequence
  if (calendar$leap_month > 0) {
    
    # insert leap month by extracting all months and sorting (by number of first days)
    months_only <- calendar %>% 
      select(-c(year, leap_month, days_in_year)) %>% 
      sort() 
    
    # rename and insert leap month name
    names(months_only) <- paste0("first_day_month", c(1:calendar$leap_month, paste0(calendar$leap_month,"L"), (calendar$leap_month+1):12))
    
    # add other values back in
    calendar <- bind_cols(year = calendar$year, 
                          months_only, 
                          leap_month = calendar$leap_month, 
                          days_in_year = calendar$days_in_year) 
  }
  
  # more error checks: 
  # how many days in a month for a particular year. e.g. is there a Feb 30th or not?
  # (calculate days for last month: days in year + first day month1  - first day month12)
  max_days <- c(diff(unlist(months_only)), 
                calendar$days_in_year + calendar$first_day_month1 - calendar$first_day_month12)
  
  if(dy > max_days[mo]){
    stop(paste(month.name[mo], yr, "has only", max_days[mo], "days in lunar calendar")) 
  }
  
  # does the year in fact have the leap month as requested in the query?
  m <- paste0("first_day_month", mo, "L")
  if(leap_month_flag & !(m %in% names(months_only))){
    stop(paste("no leap month", mo_l, "in", yr)) 
  }
  
  # extract the fist day of the month from lunar
  first_day_of_month <- 
    calendar %>% 
    select(ends_with(paste0("month", mo))) %>%  
    as.numeric()
  
  return(first_day_of_month)
  
}
