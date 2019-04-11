library(dplyr)
remove_outliers <- function(data) {
  outliers <- data %>%
    group_by(Cow) %>%
    mutate(c = pmin(abs(dailyyield - lag(dailyyield, order_by = afidate, n = 4)),
                    abs(dailyyield - lag(dailyyield, order_by = afidate, n = 3)),
                    abs(dailyyield - lag(dailyyield, order_by = afidate, n = 2)),
                    abs(dailyyield - lag(dailyyield, order_by = afidate)))) %>%
    mutate(dailyyield = ifelse(c > 0.4,  (lag(dailyyield, order_by = afidate, n = 1)+
                                            lag(dailyyield, order_by = afidate, n = 2)+
                                            lag(dailyyield, order_by = afidate, n = 3)+
                                            lag(dailyyield, order_by = afidate, n = 4))/4,dailyyield)) %>%
    mutate(d = pmin(abs(Cond - lag(Cond, order_by = afidate, n = 4)),
                    abs(Cond - lag(Cond, order_by = afidate, n = 3)),
                    abs(Cond - lag(Cond, order_by = afidate, n = 2)),
                    abs(Cond - lag(Cond, order_by = afidate)))) %>%
    mutate(Cond = ifelse(d > 0.4,  (lag(Cond, order_by = afidate, n = 1)+
                                      lag(Cond, order_by = afidate, n = 2)+
                                      lag(Cond, order_by = afidate, n = 3)+
                                      lag(Cond, order_by = afidate, n = 4))/4,Cond)) %>%
    mutate(e = pmin(abs(lactose - lag(lactose, order_by = afidate, n = 4)),
                    abs(lactose - lag(lactose, order_by = afidate, n = 3)),
                    abs(lactose - lag(lactose, order_by = afidate, n = 2)),
                    abs(lactose - lag(lactose, order_by = afidate)))) %>%
    mutate(lactose = ifelse(e > 1, (lag(lactose, order_by = afidate, n = 1)+
                                      lag(lactose, order_by = afidate, n = 2)+
                                      lag(lactose, order_by = afidate, n = 3)+
                                      lag(lactose, order_by = afidate, n = 4))/4,lactose)) %>%
    mutate(f = pmin(abs(DailyRestBout - lag(DailyRestBout, order_by = afidate, n = 4)),
                    abs(DailyRestBout - lag(DailyRestBout, order_by = afidate, n = 3)),
                    abs(DailyRestBout - lag(DailyRestBout, order_by = afidate, n = 2)),
                    abs(DailyRestBout - lag(DailyRestBout, order_by = afidate)))) %>%
    mutate(DailyRestBout = ifelse(f > 1, (lag(DailyRestBout, order_by = afidate, n = 1)+
                                            lag(DailyRestBout, order_by = afidate, n = 2)+
                                            lag(DailyRestBout, order_by = afidate, n = 3)+
                                            lag(DailyRestBout, order_by = afidate, n = 4))/4,DailyRestBout)) %>%
    mutate(g = pmin(abs(Activity - lag(Activity, order_by = afidate, n = 4)),
                    abs(Activity - lag(Activity, order_by = afidate, n = 3)),
                    abs(Activity - lag(Activity, order_by = afidate, n = 2)),
                    abs(Activity - lag(Activity, order_by = afidate)))) %>%
    mutate(Activity = ifelse(g > 0.25, (lag(Activity, order_by = afidate, n = 1)+
                                          lag(Activity, order_by = afidate, n = 2)+
                                          lag(Activity, order_by = afidate, n = 3)+
                                          lag(Activity, order_by = afidate, n = 4))/4,Activity)) %>%
    mutate(h = pmin(abs(RestDur - lag(RestDur, order_by = afidate, n = 4)),
                    abs(RestDur - lag(RestDur, order_by = afidate, n = 3)),
                    abs(RestDur - lag(RestDur, order_by = afidate, n = 2)),
                    abs(RestDur - lag(RestDur, order_by = afidate)))) %>%
    mutate(RestDur = ifelse(h > 0.4, (lag(RestDur, order_by = afidate, n = 1)+
                                        lag(RestDur, order_by = afidate, n = 2)+
                                        lag(RestDur, order_by = afidate, n = 3)+
                                        lag(RestDur, order_by = afidate, n = 4))/4,RestDur)) %>%
    mutate(j = pmin(abs(TotalRestTime - lag(TotalRestTime, order_by = afidate, n = 4)),
                    abs(TotalRestTime - lag(TotalRestTime, order_by = afidate, n = 3)),
                    abs(TotalRestTime - lag(TotalRestTime, order_by = afidate, n = 2)),
                    abs(TotalRestTime - lag(TotalRestTime, order_by = afidate)))) %>%
    mutate(TotalRestTime = ifelse(j > 1, (lag(TotalRestTime, order_by = afidate, n = 1)+
                                            lag(TotalRestTime, order_by = afidate, n = 2)+
                                            lag(TotalRestTime, order_by = afidate, n = 3)+
                                            lag(TotalRestTime, order_by = afidate, n = 4))/4,TotalRestTime))
  return(data.frame(outliers))
}          
