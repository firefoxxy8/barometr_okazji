library(shiny)
library(cluster)
library(dplyr)

function(input, output) {
  
  simillar_offers <- reactive({
    # load dataset and district dictionary
    load('offers_final.Rdata')
    load('districts_mapping.Rdata')
    
    # get announcement data
    source('scripts/scraping.R')
    new_offer <- scrap_offer(input$url, offers_final, districts_mapping)
    
    
    # calculate disimilarities
    offers_final$url <- as.character(offers_final$url)
    offers_to_matrix <- rbind(
      offers_final,
      data.frame(
        id = 0, 
        url = input$url, 
        price = new_offer$price, 
        price_per_meter = new_offer$price_per_meter, 
        construction_year = new_offer$construction_year, 
        building_type = new_offer$building_type, 
        district_group = new_offer$district_group,
        rooms = new_offer$rooms,
        area = new_offer$area,
        floors_in_building = new_offer$floors_in_building,
        cctv_or_security_agency = new_offer$cctv_or_security_agency,
        elevator = new_offer$elevator, 
        windows_type = new_offer$windows_type, 
        balcony = new_offer$balcony,
        construction_year_init = new_offer$construction_year_init,
        area_init = new_offer$area_init,
        floors_in_building_init = new_offer$floors_in_building_init,
        district_init = new_offer$district
      )
    )
    
    row.names(offers_to_matrix) <- offers_to_matrix$id
    offers_to_matrix <- offers_to_matrix[,c(5:14)]
    
    dissim <- as.matrix(daisy(offers_to_matrix))
    
    single <- dissim[rownames(dissim) == '0',]
    sim_ids <- data.frame(
      id = names(head(single[order(single, decreasing = F)], input$obs+1)),
      distance = head(single[order(single, decreasing = F)], input$obs+1)
    )
    
    sims <- merge(offers_final, sim_ids)
    sims$link <- paste("<a href='", sims$url, "' target='_blank'>link</a>", sep='')
    sims
  })
    
  
  
  
  # Table with simillar offers
  output$sims <- renderDataTable({
    s <- simillar_offers() %>% 
      select(price, price_per_meter, district_init, area_init, rooms, construction_year_init, link) %>%
      mutate(
        price_per_meter = round(price_per_meter,0),
        district_init = gsub(' - Zobacz na mapie', '', district_init)
      )
    colnames(s) <- c(
      'Cena (zl)',
      'Cena/m2 (zl)',
      'Dzielnica',
      'Powierzchnia (m2)',
      'Pokoje',
      'Rok budowy',
      'Link'
    )
    s
  }, escape = FALSE)
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
}