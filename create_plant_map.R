create_plant_map <- function(data, lat_col, long_col, energy_col, rad_col) {
  df <- read_csv(data) %>%
    drop_na() %>% 
    select(long = !!long_col, lat = !!lat_col, energy = !!energy_col, rad_level = !!rad_col)
  color_palette <- colorNumeric(palette = 'viridis', domain = df$energy, reverse = F)
  df %>% 
    leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(~long, ~lat, radius = ~rad_level, color = ~color_palette(energy))
}