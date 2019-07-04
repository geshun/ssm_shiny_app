#----
require(tidyverse)
require(janitor)
require(lubridate)

#----
df <- read_csv('ssm_data.csv') %>%
  clean_names() 

df <- df %>%
  mutate(call_day = mday(call_arrived_ts)
         , call_hour = hour(call_arrived_ts)
         , dispatch_day = mday(unit_dispatch_ts)
         , dispatch_hour = hour(unit_dispatch_ts)
         , clear_day = mday(unit_clear_ts)
         , clear_hour = hour(unit_clear_ts)) %>%
  mutate(all_day = if_else(!is.na(dispatch_day), dispatch_day, call_day)
         , all_hour = if_else(!is.na(dispatch_hour), dispatch_hour, call_hour)) %>%
  mutate(unit_dispatch_itv = unit_dispatch_ts - call_arrived_ts
         , unit_chute_itv = unit_enroute_ts - unit_dispatch_ts
         , unit_travel_itv = unit_arrive_ts - unit_enroute_ts
         , unit_response_itv = unit_arrive_ts - call_arrived_ts
         , unit_scene_itv = unit_transport_ts - unit_arrive_ts
         , unit_transport_itv = unit_transport_arrive_ts - unit_transport_ts
         , unit_hospital_itv = unit_clear_ts - unit_transport_arrive_ts
         , event_itv = unit_clear_ts - call_arrived_ts) %>%
  mutate(unit_pretravel_itv = unit_dispatch_itv + unit_chute_itv) %>%
  mutate(event_dispatch_distance = sqrt((x_event - x_dispatch) ^ 2
                                        + (y_event - y_dispatch) ^ 2))

#----
df_ops_events <- df %>%
  select(event_no
         , event_type
         , event_category
         , determinant
         , call_arrived_ts
         , call_hour
         , call_day
         , x_event
         , y_event
         , aa_event
         , ward_event) %>%
  distinct(event_no, .keep_all = TRUE) 

df_ops_responses <- df %>%
  select(event_no
         , determinant
         , event_category
         , unit
         , call_arrived_ts
         , unit_dispatch_ts
         , all_hour
         , all_day
         , x_dispatch
         , y_dispatch
         , aa_dispatch)

#----
df_responses <- df %>%
  filter(determinant %in% c("D", "E") 
         & event_category == "L&S Emerg" 
         & first_on_scene == "Y") %>%
  filter(!is.na(unit_response_itv) & unit_response_itv > 0 
         & !is.na(unit_dispatch_itv) & unit_dispatch_itv > 0
         & !is.na(unit_chute_itv) & unit_chute_itv > 0
         & !is.na(unit_travel_itv) & unit_travel_itv > 0) %>%  
  select(event_no
         , call_hour
         , call_day
         , ward_event
         , station_dispatch
         , unit_response_itv
         , unit_pretravel_itv
         , unit_dispatch_itv
         , unit_chute_itv
         , unit_travel_itv
         , event_dispatch_distance) %>%
  mutate(unit_loc = if_else(is.na(station_dispatch), 'out station', 'in station'))

df_travel_predict <- df_responses %>%
  select(unit_loc
         , call_hour
         , event_dispatch_distance
         , unit_travel_itv) %>%
  filter(!((as.numeric(unit_travel_itv) / 60 < 0.8
            & event_dispatch_distance / 100000 > 17)
           | (as.numeric(unit_travel_itv) / 60 > 59
              & event_dispatch_distance / 100000 < 1))) 

response_percentiles <- df_responses %>%
  group_by(call_day) %>%
  summarise(volume = n() 
            , pretravelP50 = quantile(unit_pretravel_itv, .5) 
            , pretravelP90 = quantile(unit_pretravel_itv, .9) 
            , travelP50 = quantile(unit_travel_itv, .5) 
            , travelP90 = quantile(unit_travel_itv, .9)
            , responseP50 = quantile(unit_response_itv, .5) 
            , responseP90 = quantile(unit_response_itv, .9)) %>%
  ungroup() %>%
  gather(stats, value, volume:responseP90) 

#----
#---- unit destination interval for transported events
df_destinations <- df %>%
  filter(!is.na(unit_hospital_itv) 
         & unit_hospital_itv > 0) %>%
  select(event_no
         , event_transport
         , unit_dispatch_ts
         , dispatch_day
         , unit_hospital_itv
         , aa_destination) 
  
destination_percentile <- df_destinations %>%
  group_by(dispatch_day) %>%
  summarise(volume = n()
            , total = sum(unit_hospital_itv)
            , P50 = quantile(unit_hospital_itv, .5) 
            , P90 = quantile(unit_hospital_itv, .9)) %>%
  ungroup() %>%
  gather(stats, value, volume:P90)

#----
#---- event location for gis
df_event_location <- df_ops_events %>%
  group_by(x_event, y_event) %>%
  summarise(count = n(), 
            ward = min(ward_event), 
            firstTime = min(call_arrived_ts), 
            lastTime = max(call_arrived_ts)) %>%
  ungroup() %>%
  arrange(desc(count))

#----
unit_clear_percentile <- df %>%
  filter(!is.na(event_itv)
         & event_itv != 0 
         & event_transport == "Yes") %>%
  select(event_no
         , unit
         , unit_clear_ts
         , clear_day
         , event_itv) %>%
  group_by(clear_day) %>%
  summarise(volume = n() 
            , total = sum(event_itv)
            , average = mean(event_itv)
            , P50 = quantile(event_itv, .5) 
            , P90 = quantile(event_itv, .9)) %>%
  ungroup() %>%
  gather(stats, value, volume:P90)

#----
get_selected_items <- function(param, compare_with, to_return) {
  if(length(param) == 1 && param == compare_with) {
    return(to_return)
  }
  return(param[param != compare_with])
}

generic_axis <- list(title = "", 
                     titlefont = list(size = 13, color = 'rgb(107, 107, 107)'), 
                     tickfont = list(size = 11, color = 'rgb(107, 107, 107)')
                     )
generic_legend <- list(x = 0, y = 1, orientation = 'h')