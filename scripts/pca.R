# Multiple Corespondence Analysis

# 0. Read dataset and load libraries
setwd("~/Desktop/barometr_okazji/barometr_okazji")
offers <- read.csv('announcements.csv')

library(FactoMineR)
library(dplyr)


# 1. Clean the data

# 1.1 Remove columns we do not need and 
#     observation with wrong values 
offers_2 <- offers[, -c(2:5, 10, 16, 27:29, 35, 54:57)]

# wrong value in floors in building
offers_2 <- offers_2[id %in% c()]

# wrong values in construction year
offers_2 <- offers_2[offers_2$construction_year <= 2018 &
                       offers_2$construction_year > 1500,]

# wrong values in area field
offers_2 <- offers_2[offers_2$area <= 1000,]

# 1.2 Construct other variables
offers_2$is_last_floor <- as.factor(offers_2$floor == offers_2$floors_in_building)
offers_2 <- offers_2 %>% mutate(average_room_area = area / rooms)
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

# Check histograms

numerics <- sapply(offers_4,function(col) is.numeric(col))
numerics <- names(numerics[numerics==TRUE])
numerics <- numerics[-1]

for (v in names(numerics[numerics==TRUE])){
  hist(offers_4[,v], main=v)
}

# transform to <0,1> interval
normalize <- function(x){
  sm <- summary(x)
  x <- (x - sm[1]) / (sm[6]-sm[1])
}


offers_5 <- as.data.frame(sapply(offers_4, function(col){
    if(is.numeric(col)){
      normalize(col)
    } else{
      col
    }
  }
))

offers_5$id <- offers_4$id


MCA

MCA(X, ncp = 5, ind.sup = NULL, quanti.sup = NULL,
    quali.sup = NULL, excl=NULL, graph = TRUE,
    level.ventil = 0, axes = c(1,2), row.w = NULL,
    method="Indicator", na.method="NA", tab.disj=NULL)
