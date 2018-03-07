#Calculates the annual daily mean o3 concentration for a specified county in CA ozone/location dataset
# x = o3.filenames
# y = County Name

annual_daily_mean <- function (x,y){
  annual_daily_mean <- x %>%
    inner_join(filter(loc, grepl(paste(c(y)), loc$'County Name')), by = "site") %>% 
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>% 
    summarize(mean = mean(o3, na.rm = TRUE))
  annual_daily_mean 
}