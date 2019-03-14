create_fluid_eq_map <- function(fluid_data, fluid_lat_col, fluid_long_col, fluid_vol_col,
                                eq_data, eq_lat_col, eq_long_col, eq_mag_col) {
  df_fluidinjection <- read_csv(fluid_data) %>% 
    drop_na() %>% 
    select(long = !!fluid_long_col, lat = !!fluid_lat_col, vol = !!fluid_vol_col)
  df_eq <- read_csv(eq_data) %>% 
    drop_na() %>% 
    select(long = !!eq_long_col, lat = !!eq_lat_col, mag = !!eq_mag_col)
  color_palette <- colorNumeric(palette = 'Reds', domain = df_fluidinjection$vol, reverse = F)
  leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(df_fluidinjection$long, df_fluidinjection$lat, radius = 1,
                     color = color_palette(df_fluidinjection$vol)) %>%
    addCircleMarkers(df_eq$long, df_eq$lat, radius = df_eq$mag/10, color = 'grey')
}