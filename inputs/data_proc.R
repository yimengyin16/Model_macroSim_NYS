#*******************************************************************************
#                          Notes                                            ####
#*******************************************************************************

# 1. Create a new variable LCap_TRI for total return index of large cap stocks (SP500)
  # 1926 - 2015-12: SBBI large cap total return index  
  # 2016-1 ~      : SP500TR from yahoo finance

# 2. Quarterly and annual data from monthly data. 
  # Quarterly data: use Mar, June, Sep, Dec series, Sherries and Zhang 2009, p20 
  # Annual data:    use June series, HSZ2016, p28


# Issue
  # merging issue with Shiller data. 



#*******************************************************************************
#                          Goals                                            ####
#*******************************************************************************

# Model 1: GDP and stock returns: quarter model
#  - national level, NY only, other state
#  - Data: quarterly national GDP growth and stock return 
#          quarterly state GDP growth and stock return
 

# Model 2: GDP and tax revenues, annual model
#  - National level, NY only, state panel
#  - Data: annual national GDP 
#          annual state GDP
#          annual national tax revenue by type
#          annual state tax revenue by type
           

# Model 3: GDP and tax revenues, quarterly model
#  - National level, NY only, state panel
#  - Data: quarterly national GDP 
#          quarterly state GDP
#          quarterly national tax revenue by type
#          quarterly state tax revenue by type


#' Roadmap

#' Quarterly data
#'  - GDP national: FRED quarterly GDP
#'  - GDP state   : BEA, adjusted for 1997 breaking point + state coincident index
#'  - Stock       : SBBI monthly and Yahoo monthly
#'  - bond return : FRED monthly, SBBI monthly
#'  - interestRate: FRED monthly
#'  - inflation   : FRED monthly, various 
#'  - State tax   : BEA quarterly tax data  

 
#' Annual data 
#'  - GDP national: FRED quarterly GDP
#'  - GDP state   : BEA, adjusted for 1997 breaking point
#'  - Stock       : SBBI monthly and Yahoo monthly
#'  - bond return : FRED monthly, SBBI monthly
#'  - interestRate: FRED monthly
#'  - inflation   : FRED monthly, various 
#'  - State tax   : BEA annual tax data from Urban  
 


#**********************************************************************
#                           Packages                               ####
#**********************************************************************


# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf


#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "data_raw/"
dir_data_out <- "data_out/"










