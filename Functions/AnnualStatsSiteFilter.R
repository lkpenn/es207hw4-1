#Calculates the annual mean, median, max and min of all sites that have “San” or “Santa” in their name in CA ozone/location dataset
# x = o3.filenames
# y = Filter 1 text ("San")
# z = Filter 2 text ("Santa")

annual_stats <- function(x,y,z){
  yearly.site <- x %>%
    rbindlist() %>%
    group_by(site = as.factor(site))%>%
    inner_join(filter(loc, grepl(paste(c(y,z), collapse="|"), loc$`Site Name`)), by = "site")%>%
    mutate(year = format(date, "%Y")) %>%
    group_by(site,year) %>%
    summarize(mean = mean(obs, na.rm = TRUE), median = median(obs, na.rm = TRUE),max = max(obs, na.rm = TRUE),min = min(obs, na.rm = TRUE) )
  yearly.site
}