# Prepare dataset and reduce number of dimmensions

# 0. Read dataset and load libraries
library(randomForest)
library(xlsx)
library(dplyr)

setwd("~/Desktop/barometr_okazji/barometr_okazji")
offers <- read.csv('announcements.csv')
districts_mapping <- read.xlsx('districts_mapping.xlsx',1)
districts_mapping$district <- as.character(districts_mapping$district)
districts_mapping$district_group <- as.character(districts_mapping$district_group)


# 1. Clean the data

# 1.1 Remove columns we do not need and 
#     observation with wrong values 
offers_2 <- offers[, -c(2:5, 10, 16, 27:29, 35, 54:57)]

# wrong value in floors in building
offers_2 <- offers_2[offers_2$id != 1413, ]

# wrong values in construction year
offers_2 <- offers_2[(offers_2$construction_year <= 2018 &
                       offers_2$construction_year > 1500) | is.na(offers_2$construction_year),]

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

# 1.5 Handle variables with too many categories
sapply(offers_4, function(col) length(unique(col)))

offers_4$street_or_settlement <- NULL

offers_4$district <- gsub(' - Zobacz na mapie', '', as.character(offers_4$district))

offers_4 <- merge(offers_4, districts_mapping, by='district', all.x=T)

ind <- !is.na(offers_4$district) & is.na(offers_4$district_group)

offers_4$district_group[ind] <- 'Grupa Inne'

ind <- is.na(offers_4$district) | offers_4$district == ''

offers_4$district_group[ind] <- 'Brak'

offers_4$district <- NULL

offers_4$district_group <- as.factor(offers_4$district_group)


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

offers_5 <- offers_4

for(name in colnames(offers_5)){
  v <- offers_5[,name]
  if(is.numeric(v) & name != 'id'){
    offers_5[,name] <- normalize(v)
  }
}


# 3. Use RandomForest to choose top predictors

# add a target variable to dataset
target <- offers %>% select(id, price_per_meter)
offers_6 <- merge(offers_5, target, by='id', all.x=T)

# build a simple model


set.seed(2017)
offers_6$id <- NULL
fit <- randomForest( price_per_meter ~ .,
                           data=offers_6, 
                           importance=TRUE, 
                           ntree=500)
varImpPlot(fit)

selected_vars <- c(
  'construction_year',
  'building_type', 
  'district_group', 
  'rooms', 
  'area',
  'floors_in_building', 
  'cctv_or_security_agency',
  'elevator', 
  'windows_type',
  'balcony'
)

other_vars <- c(
  'id', 
  'url',
  'price',
  'price_per_meter'
)



offers_final <- merge(offers_5, offers[,other_vars])[,c(other_vars, selected_vars)]


# add values before normalization to display them in app
init_vars <- c(
  'construction_year',
  'area',
  'floors_in_building',
  'district'
)

init <- offers[,c('id',init_vars)]
colnames(init) <- c('id', paste(init_vars, 'init', sep='_'))

offers_final <- merge(offers_final, init)

save(offers_final, file='offers_final.Rdata')
