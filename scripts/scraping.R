# scraping sales announcement from otodom.pl

# 0. Libraries
#install.packages('rvest')
library(rvest)
library(xlsx)


# 1. Exact function
scrap_offer <- function(url, offers_final, districts_mapping){
  offer <- read_html(url)
  
  # Price
  price <- html_node(offer, '.param_price strong') %>% 
    html_text() 
  
  price <- as.numeric(gsub('[^0-9]', '', price))
  
  
  # Area 
  area <- html_node(offer, '.param_m strong') %>%
    html_text()
  area <- as.numeric(gsub('[^0-9]', '', strsplit(area, ',')[[1]][1]))
  
  area_init <- area
  area <- (area - 16)/574
  
  # Price per meter
  price_per_meter <- price/area_init
  
  
  # Get sublist (there are key-value pairs)
  sub_list <- html_nodes(offer, '.sub-list li') %>% html_text()
  
  # Construction year
  if (sum(grepl('.*rok budowy:.*', sub_list))>0){
    construction_year <- strsplit(grep('.*rok budowy:.*', sub_list, value=T),': ')[[1]][2]
    construction_year <- as.numeric(gsub('[^0-9]', '', construction_year))
  } else construction_year <- 1995
  
  construction_year_init <- construction_year
  construction_year <- (construction_year - 1849)/169
    
  
  # Building type
  if (sum(grep('.*rodzaj zabudowy:.*', sub_list))>0){
    building_type <- strsplit(grep('.*rodzaj zabudowy:.*', sub_list, value=T),': ')[[1]][2]
    #translating to original dict values
    building_type <- switch(
      building_type, 
      'blok' = 1, 
      'kamienica' = 2, 
      'dom wolnostojący' = 3,
      'plomba' = 4,
      'szeregowiec' = 5,
      'apartamentowiec' = 6,
      'loft' =7, 
      8
    )
  } else building_type <- NA
  
  building_type <- factor(building_type, levels(offers_final$building_type))
  building_type <- addNA(building_type)
  
  
  # Floors in building
  floors_in_building <- html_node(offer, '.param_floor_no span') %>% 
    html_text() 
  
  if (sum(grepl('.*z.*', floors_in_building)) > 0){
    floors_in_building <- strsplit(floors_in_building, 'z')[[1]][2]
    floors_in_building <- as.numeric(gsub('[^0-9]', '', floors_in_building))
  }else floors_in_building <- 5
  
  floors_in_building_init <- floors_in_building
  floors_in_building <- (floors_in_building-1)/30
  
  
  # Rooms
  rooms <- html_node(offer, '.main-list li:nth-child(3) strong') %>% 
    html_text() 
  
  rooms <- factor(rooms, levels(offers_final$rooms))
  rooms <- addNA(rooms)
  
  # District Group 
  district <- html_node(offer, '.address-links') %>% 
    html_text() 
  
  if(length(strsplit(district, ',')[[1]]) >=3){
    district <- strsplit(district, ', ')[[1]][3]
  } else district <- NA
  
  
  district <- gsub(' - Zobacz na mapie', '', district)
  
  district_group <- districts_mapping$district_group[districts_mapping$district == district]
  
  if(is.na(district) | district == '') {
    district_group <- 'Brak'
  } else if(!is.na(district) & is.na(district_group)) {district_group <- 'Grupa Inne'}
  
  district_group <- factor(district_group, levels(offers_final$district_group))
  district_group <- addNA(district_group)
  
  # Windows type
  if (sum(grep('.*okna:.*', sub_list))>0){
    windows_type <- strsplit(grep('.*okna:.*', sub_list, value=T),': ')[[1]][2]
    windows_type <- switch(
      windows_type, 
      'plastikowe' = 1, 
      'drewniane' = 2, 
      'aluminiowe' = 3, 
      4
    )
    
  } else windows_type <- NA
  
  windows_type <- factor(windows_type, levels(offers_final$windows_type))
  windows_type <- addNA(windows_type)
  
  # features
  features <- html_nodes(offer, '.dotted-list li') %>% html_text()
  
  # 'cctv_or_security_agency'
  if (sum(grep('.*monitoring / ochrona.*', features))>0){
    cctv_or_security_agency <- 't'
  } else {
    cctv_or_security_agency <- 'f'
  }
  
  cctv_or_security_agency <- factor(cctv_or_security_agency, levels(offers_final$cctv_or_security_agency))
  cctv_or_security_agency <- addNA(cctv_or_security_agency)
  
  
  # elevator
  
  if (sum(grep('.*winda.*', features))>0){
    elevator <- 't'
  } else {
    elevator <- 'f'
  }
  
  elevator <- factor(elevator, levels(offers_final$elevator))
  elevator <- addNA(elevator)
  
  # balcony
  
  if (sum(grep('.*balkon.*', features))>0){
    balcony <- 't'
  } else {
    balcony <- 'f'
  }
  
  balcony <- factor(balcony, levels(offers_final$balcony))
  balcony <- addNA(balcony)
  
  list(
    price = price,
    area = area,
    area_init = area_init,
    price_per_meter = price_per_meter, 
    construction_year = construction_year,
    construction_year_init = construction_year_init,
    building_type = building_type, 
    floors_in_building = floors_in_building,
    floors_in_building_init = floors_in_building_init,
    rooms = rooms, 
    district = district, 
    district_group = district_group, 
    windows_type = windows_type,
    cctv_or_security_agency = cctv_or_security_agency,
    elevator = elevator,
    balcony = balcony
  )
}

#out <- scrap_offer(sample_url, offers_final, districts_mapping)

