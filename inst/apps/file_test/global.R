#' Load objects into the global environment
#' 
#' @description The global.R file allows us to load libraries, 
#'              functions, data sets etc. into the global 
#'              environment in which the app is created .
#'              
#'              All objects created in this file can be used 
#'              throughout the app
#'
#' @details 

# Second: We load data from https://github.com/treypujats/COVID19/tree/master/covid19/data ,  usafacts.org , and covid19.healthdata.org
#         The github data has static information on on all air force bases, US counties, and US hospitals. 
#         The usafacts data has dynamic information that is updated daily reporting the number of cases and number of deaths daily.
#         The IHME provides projection data of the pandemic
#         After loading the data, we format headers, establish data tables for printing, and do any static changes to the dataset for the app.
# Third:  Functions are used to execute the tasks in the server. Functions in the global are not dynamic, but they take in dynamic inputs
#         Global functions are used to calculate statistics, make data tables, plot graphs, and create visuals.
##############################################################################################################################################       

# Step 1: load R packages containing functions used throughout the app
  source("www/1_load_pkgs/app_pkgs.R")

# Step 2: load local data sets that do not change daily
  load("www/2_ingest_data/local_data/cimd.rda")
  load("www/2_ingest_data/local_data/himd.rda")
  load("www/2_ingest_data/local_data/AFBaseLocations.rda")
  load("www/2_ingest_data/local_data/CountyHospRate.rda")
  load("www/2_ingest_data/local_data/CountyInfo.rda")
  load("www/2_ingest_data/local_data/HospitalInfo.rda")
  load("www/2_ingest_data/local_data/HospUtlzCounty.rda")

# Step 3: load non-local data sets from URL


# Step 4: load user-defined R functions dev'd for CHAD app
  R_code_files <- list.files("www/3_load_functions", 
                             pattern = "\\.R$",
                             full.names = T)
  purrr::map(R_code_files, source)
