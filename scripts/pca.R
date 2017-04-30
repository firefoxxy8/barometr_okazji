# Multiple Corespondence Analysis

# 0. Read dataset and load libraries
setwd("~/Desktop/barometr_okazji/barometr_okazji")
offers <- read.csv('announcements.csv')

library(FactoMineR)
library(dplyr)


# 1. Clean the data

# 1.1 Remove columns we do not need
offers_2 <- offers[,-c(2:5, 10, 16, 27:29, 35, 54:57)]

# 1.2 Construct other variables
offers_2$is_last_floor <- as.factor(offers_2$floor == offers_2$floors_in_building)
offers_2 <- offers_2 %>% mutate(average_room_area = rooms / area)
offers_2$is_first_floor <- as.factor(offers_2$floor == 1)


# 1.3 Change type of variables
offers_3 <- offers_2

vars_factors <- c(
  'rooms', 
  'building_type', 
  'building_material', 
  'ownership_form', 
  'windows_type', 
  'finish_type', 
  'heating_type'
)


for(v in vars_factors){
  offers_3[,v] <- as.factor(offers_3[,v])
}



# 1.4 Replae NAs
offers_4 <- offers_3

for (name in colnames(offers_4)){
  v <- offers_4[,name]
  
  # in factor variables create another category for NA
  if(is.factor(v)){
    offers_4[,name] <- addNA(v)
  }
  # in numeric variabes replace na's by median  
  if(is.numeric(v)){
    med <- median(v, na.rm = TRUE)
    offers_4[is.na(v),name] <- med
  }  
}



# 2. Normalize numeric variables


MCA

MCA(X, ncp = 5, ind.sup = NULL, quanti.sup = NULL,
    quali.sup = NULL, excl=NULL, graph = TRUE,
    level.ventil = 0, axes = c(1,2), row.w = NULL,
    method="Indicator", na.method="NA", tab.disj=NULL)