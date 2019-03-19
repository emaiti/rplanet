library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tools)
library(leaflet)

shinyApp(
  shinyUI(
    pageWithSidebar(
      headerPanel(h1("",
                     style = "font-family: 'Comic Sans MS'; color: #29A5E8;")),
      sidebarPanel(
        img(src = 'rplanet_icon.png', align = "center", width = "100%", height = "100%"),
        helpText('Welcome to your rPlanet dashboard!', style = 'font-weight: bold', align = 'center'),
        helpText('We are excited to help you monitor your energy production to best benefit the Earth,
                 as well as minimize your cost.', align = 'left'),
        helpText('To begin, please select a tab and input your data files of choice.')
        # fileInput("filename", "Upload Dataset (csv):", accept = c('.csv'))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Carbon Dioxide",
                   dashboardBody(
                     box(title = 'Your Carbon Footprint',
                         fileInput('footprintnum_data', 'Upload data file (csv):', accept = c('.csv')),
                         # something else,
                         selectInput('footprintnum_type', 'Energy type:', c('Natural Gas', 'Distilled Oil', 'Electricity'))
                     ),
                     box(title = 'Total Carbon Dioxide Production'
                         # numericInput()
                     )
                   )
          ),
          tabPanel("Electricity",
                   dashboardBody(
                     box(title = 'Contribution to Energy Production for Electricity',
                         numericInput('propensourcesELEC_coalpetrol', 'Coal & petrol production:', 0),
                         numericInput('propensourcesELEC_natgas', 'Natural gas production:', 0),
                         numericInput('propensourcesELEC_wind', 'Wind production:', 0),
                         numericInput('propensourcesELEC_hydro', 'Hydro production:', 0),
                         numericInput('propensourcesELEC_total', 'Total production:', 0),
                         numericInput('propensourcesELEC_tohydro', 'Hydro production goal:', 0),
                         numericInput('propensourcesELEC_towind', 'Wind production goal:', 0)
                     ),
                     box(title = 'Plotting Energy Production',
                         fileInput('productionplot_data', 'Upload data file (csv):', accept = c('.csv')),
                         selectInput('productionplot_type', 'Energy type:', c('Natural Gas', 'Electricity', 'Distilled Oil')),
                         textInput('productionplot_units', 'Units:', 'ex: Megawatt Hours (MwH)')
                     )
                   )
          ),
          tabPanel("Radiation"),
          tabPanel("Fluid Injection")
        )
      )
    )
  ),
  
  shinyServer(
    function(input, output) {
      # INSTRUCTIONS TAB
      output$instructions <- renderText({ "Please input your data file(s) of choice." })
      
      
      
      # FILE UPLOADS
      co2Input <- reactive({
        validate(
          need(input$filename != 0, "test")
        )
        inFile <- input$filename
        if (is.null(inFile)) return(NULL)
        read_csv(inFile$datapath)
      })
      
      electricityInput <- reactive({
        validate(
          need(input$filename != 0, "test")
        )
        inFile <- input$filename
        if (is.null(inFile)) return(NULL)
        read_csv(inFile$datapath)
      })
      
      radiationInput <- reactive({
        validate(
          need(input$filename != 0, "test")
        )
        inFile <- input$filename
        if (is.null(inFile)) return(NULL)
        read_csv(inFile$datapath)
      })
      
      fluidInjInput <- reactive({
        validate(
          need(input$filename != 0, "test")
        )
        inFile <- input$filename
        if (is.null(inFile)) return(NULL)
        read_csv(inFile$datapath)
      })
      
      
      
      # OUTPUTS
      output$test <- renderPlot({
        iris %>% 
          ggplot(aes(Sepal.Length, Sepal.Width)) +
          geom_point()
      })
      
      
      
      # FUNCTIONS
      unit_co2_oil <- function(oil_out, units = c("barrels", "gallons"), 
                               emissions_yr, preds_co2, preds_oil) {
        if(units == "barrels") {
          tons_oil_month <- (oil_out * 42 * 365)/(264.3405 * 12)
        } else {
          tons_oil_month <- (oil_out * 365)/12
        }
        preds_co2 <- data.frame(rnorm(12, emissions_yr/12, 10000))
        preds_oil <- data.frame(rnorm(12, tons_oil_month, 325000))
        co2_prod_per_ton <- (preds_co2/preds_oil)
        return(mean(co2_prod_per_ton[,1]))
      }
      
      prop_en_sources_ELEC <- function(coal_petrol, nat_gas, wind, hydro, total, to_hydro, to_wind) {
        other <- total - coal_petrol - nat_gas - wind - hydro
        props <- data.frame(type = c("Coal and Petrol", "Natural Gas", "Wind", "Hydroelectric", "Other"), 
                            proportion = c(coal_petrol/total, nat_gas/total, wind/total, 
                                           hydro/total, other/total), id = rep("Before", 5)) 
        to_allocate <- (0.4*coal_petrol) + (0.4*nat_gas)
        allo_hydro <- to_hydro*to_allocate
        allo_wind <- to_wind*to_allocate
        props_chg <- data.frame(type = c("Coal and Petrol", "Natural Gas", "Wind", "Hydroelectric", "Other"), 
                                proportion = c(.6*coal_petrol/total, .6*nat_gas/total, (wind + allo_wind) /total, 
                                               (hydro+ allo_hydro)/total, other/total), id = rep("After", 5) ) 
        all <- data.frame(rbind(props, props_chg))
        all$type <- factor(all$type, levels = c("Other", "Wind", "Hydroelectric", "Natural Gas", "Coal and Petrol"))
        all$id <- factor(all$id, levels = c("Before", "After"))
        plot <- ggplot(all, aes(x=type, y=proportion, fill= id)) + 
          geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + 
          labs(title="Proportion of Annual MwH Produced\nby Energy Source", x="", 
               y="Fraction of Annual Energy Produced") + theme(legend.position = "bottom") + 
          scale_fill_discrete(name = "EPA Reduction\nin Effect") + coord_flip()
        print(plot)
        return(all)
      }
      
      co2_averted <- function(bef_af_tab, total) {
        bef_af_tab %>% filter(., type == "Wind" | type == "Hydroelectric") %>% 
          mutate(., prop_tot_oil = proportion*total*1.0174, prop_tot_gas = proportion*total*0.4516) -> av
        plot_oil <- ggplot(av, aes(x=type, y=prop_tot_oil, fill= id)) + 
          geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + 
          labs(title="Emissions Averted by Replacing Coal and\nPetrol in Electricity Production", x="", 
               y="CO2 Emissions Averted\n(metric tonnes)") + theme(legend.position = "bottom") + 
          scale_fill_discrete(name = "EPA Reduction\nin Effect") 
        print(plot_oil)
        plot_gas <- ggplot(av, aes(x=type, y=prop_tot_gas, fill= id)) + 
          geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + 
          labs(title="Emissions Averted by Replacing Natural\nGas in Electricity Production", x="", 
               y="CO2 Emissions Averted\n(metric tonnes)") + theme(legend.position = "bottom") + 
          scale_fill_discrete(name = "EPA Reduction\nin Effect") 
        print(plot_gas)
      }
      
      production_plot <- function(data, type, units) {
        df <- data.frame(prod = data, month = 1:length(data))
        names(df) <- c(eval(type), "month")
        ggplot(df, aes(x= month, y = df[,1])) + geom_line() + 
          labs(title = paste0("Monthly Production of ", type), x = "Month", 
               y = eval(units) ) + 
          scale_x_continuous(breaks= seq(1,length(data),2), limits = c(1,length(data))) + 
          theme(axis.text.y = element_text(angle = 45, hjust = 1)) 
      }
      
      footprint_Num <- function(monthly_data, table, type = c("Natural Gas", "Distilled Oil", "Electricity")) {
        if(eval(type) == "Electricity"){
          table %>% filter(., id == "Before") %>% select(., proportion) %>% 
            data.matrix(., rownames.force = NA) -> before_EPA
          table %>% filter(., id == "After") %>% select(., proportion) %>% 
            data.matrix(., rownames.force = NA) -> after_EPA
          cf <- data.frame(emissions =  matrix(monthly_data, nrow = length(entry)) %*% t(before_EPA) 
                           %*% matrix(c(2.0174, 0.4516, 0, 0, 0.2348), nrow = 5), month = 1:length(monthly_data))
          print(sum(cf$emissions))
        } else if(eval(type) == "Distilled Oil") {
          cf <- data.frame(emissions =  matrix(monthly_data, nrow = length(entry)) *2.0174)
          print(sum(cf$emissions))
        } else {
          cf <- data.frame(emissions =  matrix(monthly_data, nrow = length(entry)) *0.4516)
          print(sum(cf$emissions))
        }
      }
      
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
      
    }
  )
)
