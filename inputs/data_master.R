# loading data


#**********************************************************************
#                           Packages                               ####
#**********************************************************************
source("libraries.R")

# Intro to quantmod
#    1. quantmod http://statmath.wu.ac.at/~hornik/QFS1/quantmod-vignette.pdf
#    2. https://www.quantinsti.com/blog/a-guide-on-r-quantmod-package-how-to-get-started/

# Quandl for R
# https://www.quandl.com/tools/r

# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf


#*******************************************************************************
#                     Global settings                              ####
#*******************************************************************************
# dir_data_raw <- "inputs/data_raw/"
# dir_data_out <- "inputs/data_proc/"
# Quandl.api_key("rsakY2-RD8pa1JNBk9sd") 
# 
# df_us_states <- tibble(state = c(state.name, "District of Columbia", "United States"), state_abb = c(state.abb, "DC", "US"))
# 


#*******************************************************************************
#                    Reading data files                                     ####
#*******************************************************************************


source("inputs/data_read_gsp.R")

source("inputs/data_read_tax.R")

source("inputs/data_read_financial.R")




#*******************************************************************************
#                    Reading data files                                     ####
#*******************************************************************************







