# by Daniel Xia, dansummer94@gmail.com

library(xts)
# library(PerformanceAnalytics)
# library(lubridate)

to_monthly = function(daily, index_last=TRUE) {
  monthly = apply.monthly(daily, PerformanceAnalytics::Return.cumulative)
  # using last calendar day of the month for index instead of the last available date in data
  if (index_last) {
    end_of_month = function(date) {
      lubridate::day(date) = lubridate::days_in_month(date)
      date
    }
    index(monthly) = do.call(c, lapply(index(monthly), end_of_month))
  }
  monthly
}

to_weekly = function(daily, week_ending_date = 5, days_in_week = 5, index_last=TRUE) {
  # weekdays starts in Monday = 1
  week_index = xts::endpoints(daily, "weeks")
  week_index[2:length(week_index)] = week_index[2:length(week_index)] - (days_in_week - week_ending_date)
  weekly = period.apply(daily, week_index, PerformanceAnalytics::Return.cumulative)
  # using last calendar day of the designated week for index instead of the last available date in data
  if (index_last) {
    end_of_week = function(date) {
      lubridate::day(date) = lubridate::day(date) - lubridate::wday(date, week_start=getOption("lubridate.week.start", 1)) + week_ending_date
      date
    }
    index(weekly) = do.call(c, lapply(index(weekly), end_of_week))
  }
  weekly
}

rf_dly = readxl::read_excel("../FactorAnalyticsData/risk-free 202010 output for Dec 1992 - Copy.xlsx")
rf_dly = xts(rf_dly[2:4], order.by = rf_dly$CALDT)

rf_mly = to_monthly(rf_dly)
head(rf_mly)
rf_mly = to_monthly(rf_dly, index_last=FALSE)
head(rf_mly)

rf_wly = to_weekly(rf_dly)
head(rf_wly)
rf_wly = to_weekly(rf_dly, index_last=FALSE)
head(rf_wly)
rf_wly = to_weekly(rf_dly, week_ending_date=3)
head(rf_wly)
