library(shiny)

source("helper.R")

shinyServer(function(input, output) {
  
  df_loc <- reactive({
    req(input$event_ward)
    req(input$call_hour)
    
    location_latlong %>%
      filter(ward_event %in% get_selected_items(input$event_ward, 0, 1:12)) %>%
      filter(call_hour %in% get_selected_items(input$call_hour, -1, 0:23))
  })
  
  df_dist <- reactive({
    #req(input$travel)
    df_travel_predict %>%
      filter(unit_loc %in% get_selected_items(input$station, 'any', c('in station', 'out station'))) %>%
      mutate(dist = event_dispatch_distance / 100000
           , itv = as.numeric(unit_travel_itv) / 60) %>%
      mutate(cutoff = if_else(itv <= as.numeric(input$travel), 'Target', 'Off-target'))  
  })
  
  output$map_event_location <- renderLeaflet({
    colored <- ifelse(length(unique(df_loc()$ward_event)) <= 8, 'Dark2', 'Dark2')
    pal <- colorFactor(palette = colored, domain = unique(df_loc()$ward_event))
    
    if(nrow(df_loc()) <= 2000) {
      df_loc() %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(radius = 2, 
                         weight = 2,
                         lng = ~Longitude, lat = ~Latitude, color = ~pal(ward_event), 
                         label = paste("Ward ", df_loc()$ward_event)
        )  %>%
        addLegend("bottomleft", pal = pal, values = ~ward_event, title = "Ward", opacity = 1)
    } else {
      df_loc() %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(radius = ~log(counting * 10000), 
                         clusterOptions = markerClusterOptions(), 
                         lng = ~Longitude, lat = ~Latitude, 
                         weight = 3, color = "#333333", 
                         label = paste("Ward ", df_loc()$ward_event)
                         )  
      }
  })
  
  output$bar_event_location <- renderPlotly({
    req(input$event_ward)
    req(input$call_hour)
    
    df_ops_events %>%
      filter(ward_event %in% get_selected_items(input$event_ward, 0, 1:12)) %>%
      filter(call_hour %in% get_selected_items(input$call_hour, -1, 0:23)) %>%
      group_by(ward_event) %>%
      summarise(volume = n()) %>%
      ungroup() %>%
      plot_ly(x = ~ward_event, y = ~volume, type = "bar") %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(xaxis = list(title = "Ward"), yaxis = list(title = "# of Events")) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 0.00, y = 1, 
                      text = 'Operational Event by Ward', 
                      showarrow = FALSE
                      )
    })
  
  output$heatmap_event_location <- renderPlotly({
    req(input$event_ward)
    req(input$call_hour)
    
    df_demand_w <- df_ops_events %>%
      filter(ward_event %in% get_selected_items(input$event_ward, 0, 1:12)) %>%
      filter(call_hour %in% get_selected_items(input$call_hour, -1, 0:23)) %>%
      group_by(call_hour, ward_event) %>%
      summarise(volume = n()) %>%
      ungroup() %>%
      spread(ward_event, volume) %>%
      column_to_rownames(var='call_hour') %>%
      as.matrix()

    df_demand_w <- apply(df_demand_w, 1, function(x){x/mean(x)})
    plot_ly(y = rownames(df_demand_w),
            x = colnames(df_demand_w),
            z = df_demand_w, type = 'heatmap'
            ) %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(xaxis = list(title = "Call Hour"), yaxis = list(title = "Ward")) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 0, y = 1.05, 
                      text = 'Normalized Hourly Calls by Ward', 
                      showarrow = FALSE
                      )
    })
  
  output$response_percentile <- renderPlotly({
    response_percentiles %>%
      filter(grepl(input$percentile_type, stats)) %>%
      plot_ly(x = ~call_day, y = ~value / 60, type = 'scatter', mode = 'lines+markers', 
              color = ~stats, colors = "Set1"
              ) %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(legend = generic_legend,
             xaxis = list(title = "Day"), yaxis = list(title = "Time Interval (minutes)")
             ) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 1, y = 1, 
                      text = '', 
                      showarrow = FALSE
                      )
    })
  
  output$coverage_time <- renderDataTable({
    df_responses %>%
      filter(unit_loc %in% get_selected_items(input$station , 'any', c('in station', 'out station'))) %>%
      filter(ward_event %in% get_selected_items(input$event_ward, 0, 1:12)) %>%
      group_by(station_dispatch) %>%
      summarise(volume = n()
                , avg_travel_time = round(as.numeric(mean(unit_travel_itv)) / 60, 2)
                , std_travel_time = round(as.numeric(sd(unit_travel_itv)) / 60, 2)) %>%
      datatable(rownames = FALSE
                , colnames = c("Station Dispatch"
                               , "Volume (First on Scene)"
                               , "Average Travel Time (minutes)"
                               , "Standard Deviation Time (minutes)")
                , options = list(pageLength = 5)
      )
  })
  
  output$station_boxplot <- renderPlotly({
    df_travel_predict %>%
      plot_ly(y = ~unit_travel_itv / 60, 
              color = ~unit_loc,
              colors = "Set1",
              opacity = .8,
              type = 'box') %>%
      layout(yaxis = list(title = "Travel Interval (minutes)", 
                          titlefont = list(size = 13, color = 'rgb(107, 107, 107)'), 
                          tickfont = list(size = 11, color = 'rgb(107, 107, 107)')
                          )
             ) %>%
      layout(showlegend = FALSE) 
  })
  
  output$travel_distribution <- renderPlotly({
    targets <- filter(df_dist(), cutoff == 'Target')
    dens_target <- density(targets$dist)
    offtargets <- filter(df_dist(), cutoff == 'Off-target')
    dens_offtarget <- density(offtargets$dist)  
    
    plot_ly(x = ~dens_target$x, y = ~dens_target$y, type = 'scatter', mode = 'lines', 
            name = 'Target', color = 'firebrick', fill = 'tozeroy'
            , fillcolor = 'rgba(178, 34, 34, 0.8)'
            , line = list(width = 0.5, color = 'firebrick')
            ) %>%
      add_trace(x = ~dens_offtarget$x, y = ~dens_offtarget$y, 
                name = 'Off-target', color = 'dimgrey', fill = 'tozeroy'
                , fillcolor = 'rgba(105, 105, 105, 0.8)'
                , line = list(width = 0.5, color = 'dimgrey')
                ) %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(legend = list(x = 0.7, y = 0.9, orientation = 'h'),
             xaxis = list(title = "Travel Distance (km)"), 
             yaxis = list(title = "Density", zeroline = FALSE)
            ) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 0.4, y = 0.95, 
                      text = 'Distribution of Euclidean Travel Distance', 
                      showarrow = FALSE
                      )
    })
  
  output$dist_distribution <- renderPlotly({
    df_dist() %>%
    plot_ly(y = ~itv, x = ~dist, type = 'scatter', mode = 'markers', color = ~cutoff, 
            colors = c('dimgrey', 'firebrick'), size = ~dist, marker = list(opacity = .7)
            ) %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(legend = list(x = 0.7, y = 0.9, orientation = 'h'),
             xaxis = list(title = "Euclidean Travel Distance (km)"),
             yaxis = list(title = "Travel Interval (minutes)")
             ) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 0.4, y = 0.95, 
                      text = 'Travel Time vs. Travel Distance', 
                      showarrow = FALSE
                      )
    })
  
  output$destination_percentile <- renderPlotly({
    destination_percentile %>%
      filter(grepl(input$percentile_type, stats)) %>%
      plot_ly(x = ~dispatch_day, y = ~value / 60, type = 'scatter',
              mode = 'lines+markers', color = ~stats, colors = "Set1"
              ) %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(xaxis = list(title = "Dispatch Day"),
             yaxis = list(title = "Wait Time Percentile (minutes)")
            ) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 0.8, y = 0.95, 
                      text = "High percentiles in hospital wait times 
                      at early days are probably due to relatively 
                      high transport volumes during those days", 
                      showarrow = FALSE
                      )
    })
  
  output$clear_percenetile <- renderPlotly({
    unit_clear_percentile %>%
      filter(grepl(input$percentile_type, stats)) %>%
      plot_ly(x = ~clear_day, y = ~value / 360, type = 'scatter',
              mode = 'lines+markers', color = ~stats, colors = "Set1"
      ) %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(xaxis = list(title = "Clear Day"),
             yaxis = list(title = "Time on Task (hours)")
      ) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 0.8, y = 0.95, 
                      text = "High percentiles in time on task 
                      at early days are due to high hospital wait times 
                      during those days", 
                      showarrow = FALSE
      )
  })
  
  output$transport_volume <- renderPlotly({
    df_destinations %>%
      mutate(cutoff = if_else((unit_hospital_itv / 60) <= as.numeric(input$destination), 
                              'Target', 'Off-target'
                              )
             ) %>%
      group_by(cutoff, dispatch_day) %>%
      summarise(volume = n()) %>%
      ungroup() %>%
      plot_ly(x = ~dispatch_day, y = ~volume, type = 'bar', 
              color = ~cutoff, colors = c('dimgrey', 'firebrick')
              ) %>%
      layout(xaxis = generic_axis, yaxis = generic_axis) %>%
      layout(legend = list(x = 0.8, y = 1, orientation = 'h'),
             xaxis = list(title = "Dispatch Day"), yaxis = list(title = "Transport Volume"),
             barmode = 'stack', bargap = 0.15, bargroupgap = 0.1) %>%
      add_annotations(xref = 'paper', yref = 'paper', x = 0.4, y = 0.95, 
                      text = '', 
                      showarrow = FALSE
                      )
    })
  })