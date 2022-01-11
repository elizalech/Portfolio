library(tidyverse)
library(magrittr)
library(R.utils)
library(readr)
library(sf)
library(tmap)
library(fixest)

####
##Census data code
####
setwd("~/ECON_184_Econometrics/Final/safegraph_open_census_data_2018/data")

## list of variables needed:
# income
# age
# race
# education

#### initialize
chunk0 = read_csv("cbg_b00.csv")

#### age 
chunk1 = read_csv("cbg_b01.csv")
temp_census = chunk0 %>% left_join(chunk1,by="census_block_group")

census_age = temp_census %>% 
  select(census_block_group,contains('e',ignore.case=F)) %>% summarize(
    census_bg = census_block_group,
    total_population = B01003e1,
    pop_male_0_21 = rowSums(select(.,B01001e3:B01001e9)),
    pop_male_22_39 = rowSums(select(.,B01001e10:B01001e13)),
    pop_male_40_59 = rowSums(select(.,B01001e14:B01001e17)),
    pop_male_60_100 = rowSums(select(.,B01001e18:B01001e25)),
    pop_female_0_21 = rowSums(select(.,B01001e27:B01001e33)),
    pop_female_22_39 = rowSums(select(.,B01001e34:B01001e37)),
    pop_female_40_59 = rowSums(select(.,B01001e38:B01001e41)),
    pop_female_60_100 = rowSums(select(.,B01001e42:B01001e49)),
    pop_0_21 = (pop_male_0_21 + pop_female_0_21) / total_population,
    pop_22_39 = (pop_male_22_39 + pop_female_22_39) / total_population,
    pop_40_59 = (pop_male_40_59 + pop_female_40_59) / total_population,
    pop_60_100 = (pop_male_60_100 + pop_female_60_100) / total_population
  )

census_age %<>% select(-contains("male"))
census_store = census_age

### race
chunk2 = read_csv("cbg_b02.csv")
temp_census = chunk0 %>% left_join(chunk2,by="census_block_group")
census_race = temp_census %>% 
  select(census_block_group,contains('e',ignore.case=F)) %>% summarize(
    census_bg = census_block_group,
    total_population = B02001e1,
    white = B02001e2 / total_population,
    black = B02001e3 / total_population,
    american_indian = B02001e4 / total_population,
    asian = B02001e5 / total_population
  )  

census_race %<>% select(-total_population)
census_store %<>% left_join(census_race,by="census_bg")

## hispanics
chunk3 = read_csv("cbg_b03.csv")
temp_census = chunk0 %>% left_join(chunk3,by="census_block_group")
census_hispanic = temp_census %>% 
  select(census_block_group,contains('e',ignore.case=F)) %>% summarize(
    census_bg = census_block_group,
    total_population = B03002e1,
    hispanic = B03002e12 / total_population
  ) 
census_hispanic %<>% select(-total_population)
census_store %<>% left_join(census_hispanic,by="census_bg")                        

## education
chunk15 = read_csv("cbg_b15.csv")
temp_census = chunk0 %>% left_join(chunk15,by="census_block_group")
census_education = temp_census %>% 
  select(census_block_group,contains('e',ignore.case=F)) %>% summarize(
    census_bg = census_block_group,
    total_population = B15003e1,
    below_high_school = rowSums(select(.,B15003e2:B15003e13)) / total_population,
    high_school = rowSums(select(.,B15003e14:B15003e18)) / total_population,
    some_college_associate = rowSums(select(.,B15003e19:B15003e21))/ total_population,
    college = B15003e22 / total_population,
    graduate_school = rowSums(select(.,B15003e23:B15003e25))/ total_population
  ) 
census_education %<>% select(-total_population)
census_store %<>% left_join(census_education,by="census_bg")   

## income
chunk19 = read_csv("cbg_b19.csv")
temp_census = chunk0 %>% left_join(chunk19,by="census_block_group")
census_income = temp_census %>% 
  select(census_block_group,contains('e',ignore.case=F)) %>% summarize(
    census_bg = census_block_group,
    median_income = B19049e1
  ) 
census_store %<>% left_join(census_income,by="census_bg") 

## and census done
census_store = write_csv(census_store,"../../census_bg_demographics.csv")

####
#SPATIAL ANALYSIS CODE
####

#loading data 
load_flag = TRUE

if(load_flag){
  data = read.csv("https://raw.githubusercontent.com/f1kidd/Econ184/main/Final%20Project/Housing_Transaction.csv")
  census = read.csv("census_bg_demographics.csv")
  save(data, census, file = "Housing_Transaction.Rdata")
} else {
  load("~/ECON_184_Econometrics/Final/Housing_Transaction.Rdata")
}

#transforming data
boros = filter(data, fips %in% c('36005','36081','36047','36085'), distressed_sale == FALSE, real_estate_owned != "YES:LENDER SELLING")

#Binding data
boros = boros%>% left_join(census,by="census_bg")

# The input file geodatabase
floodgdb <- "C:\\Users\\19085\\OneDrive\\Documents\\ECON_184_Econometrics\\Final\\NYCFutureHighTideWithSLR.gdb"
# output files
data_out <- "boros.RData"
# Load in data and determine projection data, setting coordinate system
gdb_layers <- st_layers(dsn = floodgdb)

# Layer names in tide data:
gdb_layers$name

j = 1
layer_name = gdb_layers$name[j]

tide_sf <- st_read(dsn = floodgdb, 
                   layer=layer_name)

# SLR data is in nad83 / new york long island projection
tide_crs <- st_crs(tide_sf)

# Load csv data and transform to NAD83 projection
# convert data frame into sf object
boros_sf <-  st_as_sf(boros,
                      agr = "identity",
                      coords = c("longitude", "latitude")
)

# Check for CRS metadata
st_is_longlat(boros_sf)

#A CRS can be added to sf 
boros_geo = st_set_crs(boros_sf, "EPSG:4326")
st_is_longlat(boros_geo)

# Project data onto tide data projection nad83
boros_proj = st_transform(boros_geo, tide_crs)
# check projection
st_is_longlat(boros_proj)
st_crs(boros_proj)

# # # # #  
# Loop through list of layers, Check intersection of points with each layer
# Add new column to df_proj where 0=FALSE, 1=TRUE intersection with tide map
# New column is named after tide layer
# # # # #  
for (j in 1:length(gdb_layers$name)){
  
  layer_name = gdb_layers$name[j]
  
  tide_sf <- st_read(dsn = floodgdb, 
                     layer=layer_name)
  
  # Find intersection of each of the polygons with each point in df_proj
  intersect_all = st_intersects(boros_proj, tide_sf, sparse = FALSE)
  
  # If the df_proj data coordinate is in any of the polygons, then intersection is true
  any_column_true = apply(intersect_all, 1, any)
  true_index      = which(any_column_true)
  
  # Get list of column names
  col_names = colnames(boros_proj)
  
  # Add new column of 0's and 1's representing intersection with tide layer
  boros_proj <- boros_proj %>% mutate(new_column_name = 0)
  boros_proj$new_column_name[true_index] = 1
  
  # Convert to factor
  boros_proj$new_column_name <- as.factor(boros_proj$new_column_name)
  
  # Rename the new column to match the tide layer
  col_names_new = c(col_names, layer_name)
  colnames(boros_proj) = col_names_new
  
}

# Save dataframe to RData output file
save(boros_proj, file = data_out)

#regression analysis    
fixed = feols(log(sale_price)~total_area_sq_ft+year_built+as.numeric(stories)+white+black+asian+hispanic+below_high_school+high_school+some_college_associate+college+median_income+DCP_WOS_SLR2100s15in+DCP_WOS_SLR2100s22in+DCP_WOS_SLR2100s36in+DCP_WOS_SLR2100s50in+DCP_WOS_SLR2100s75in|saleyear+as.factor(fips), data=boros_proj)
summary(fixed, cluster=~saleyear)
