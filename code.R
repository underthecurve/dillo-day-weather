library('tidyverse')
library('rvest')

# Dillo Day dates from 2007-17 (see notes)

dates <- c("2007/5/27", "2008/5/31", 
           "2009/5/30", "2010/5/29", 
           "2011/5/28", "2012/5/26", 
           "2013/6/1", "2014/5/31", 
           "2015/5/30", "2016/5/21",
           "2017/5/20")

# Get historical data from wunderground.com

urls <- sprintf("https://www.wunderground.com/history/airport/KPWK/%s/DailyHistory.html?req_city=Evanston&req_state=IL&req_statename=Illinois&reqdb.zip=60208&reqdb.magic=1&reqdb.wmo=99999", dates)

weather_data <- function(url) { 
  
  page <- url %>%
    read_html() 
  
  tbls_ls <- page %>%
    html_nodes("table") %>%
    .[5] %>%
    html_table(fill = TRUE)
  
  hourly <- tbls_ls[[1]]
  
  colnames(hourly) <- tolower(colnames(hourly))
  
  hourly <- hourly %>% 
    rename(time = `time (cdt)`, temp = temp., dewpoint = `dew point`, windspeed = `wind speed`) %>% 
    mutate(temp = as.numeric(gsub("°F", "", temp)), 
           dewpoint = as.numeric(gsub("°F", "", dewpoint)), 
           windspeed = as.numeric(gsub("mph", "", windspeed)),
           precip = as.numeric(gsub("in", "", precip)),
           date = substring(url, 51, 59)) %>%
    separate(date, c("year", "month", "day"), sep = "/") %>%
    select(time, temp, dewpoint, windspeed, precip, events, conditions, year, month, day)
}

years <- lapply(urls, weather_data)
years.df <- data.frame(Reduce(rbind, years))

# Get hourly forecast for June 2
forecast_url <- 'https://www.wunderground.com/hourly/us/il/evanston/date/2018-06-02'
page <- forecast_url %>%
  read_html() 

tbls_ls <- page %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

hourly <- tbls_ls[[1]]
colnames(hourly) <- tolower(colnames(hourly))

hourly <- hourly %>% select(-precip) %>%
  rename(temp = temp., dewpoint = `dew point`, windspeed = wind, precip = amount) %>% 
  mutate(temp = as.numeric(gsub("°F", "", temp)), 
         dewpoint = as.numeric(gsub("°F", "", dewpoint)), 
         windspeed = as.numeric(str_extract_all(windspeed, "[0-9]+")),
         precip = as.numeric(gsub("in", "", precip)),
         year = 2018, 
         month = 6,
         day = 2,
         events = "") %>%
  select(time, temp, dewpoint, windspeed, precip, events, conditions, year, month, day)
         
# Let's merge the forecast with weather history and include only approximately noon-11pm as that is when Dillo Day's main events take place (https://www.facebook.com/DilloDay/photos/pb.157897221002390.-2207520000.1527903995./1598195516972546/?type=3&theater)

years.df <- rbind(years.df, hourly)

years.df$time.format <- as.POSIXct(strptime(years.df$time, format = "%I:%M %p"))
years.df$hour <- format(years.df$time.format, "%H") # hour in 24 hour time
years.df$year <- as.numeric(years.df$year)
years.df <- years.df %>% filter(hour >= 11 & hour <= 22)

# Box plots
# code inspired by http://biochemres.com/beautiful-minimalist-boxplots-with-r-and-ggplot2

ggplot(years.df, aes(x = year, y = temp, group = year)) + 
  geom_boxplot(fill = '#4E2A84', color = '#4E2A84') + 
  # geom_jitter(shape = 16, position = position_jitter(0.2), color = '#4E2A84') +
  labs(x = '', y = '') +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) + 
  scale_y_continuous(limits = c(45, 85), breaks = seq(45, 85, 10)) +
  scale_x_continuous(breaks = seq(2007, 2018, 1), 
                     labels = c("'07", "'08", "'09", "'10", "'11", "'12", "'13", 
                                "'14", "'15", "'16", "'17", "'18*")) +
  theme_minimal() + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(),
        axis.line.x = element_line(size = 0.5, colour = "black"), 
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"), 
        text=element_text(size = 16),
        axis.text.x=element_text(colour="black", size = 14),
        axis.text.y=element_text(colour="black", size = 14), 
        plot.caption = element_text(hjust = -.01, color = 'gray47', size = 9)) +
  ggtitle("A Decade of Dillo Days") + 
  labs(caption = 'Source: Weather Underground\n*2018 forecast', 
subtitle = "temperatures (°F) 10AM–11PM on Dillo Day each year") 
  
ggsave('plot.png', width = 8, height = 6)







