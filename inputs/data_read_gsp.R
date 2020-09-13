# Loading state and local government finance data and state GDP data


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# Goal:
    # 1. Load state GDP data 
    # 2.

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
dir_data_raw <- "inputs/data_raw/"
dir_data_out <- "inputs/data_proc/"
Quandl.api_key("rsakY2-RD8pa1JNBk9sd") 

df_us_states <- tibble(state = c(state.name, "District of Columbia", "United States"), state_abb = c(state.abb, "DC", "US"))


#**********************************************************************
#                   State GDP 1997-2016 from FRED                  ####
#**********************************************************************

# Nominal and real GSP from 1997 to 2016 for 50 states + DC
# Real GSP in 2009 dollar

# Example link:
 # https://fred.stlouisfed.org/series/NYRGSP


# varnames_NGSP <- paste0(setdiff(df_us_states$state_abb, "US"), "NGSP")
# varnames_RGSP <- paste0(setdiff(df_us_states$state_abb, "US"), "RGSP")
# 
# 
# # loading data through Quandl (2000 calls per 10 mins). 
# df_NGSP_raw <- Quandl(paste0("FRED/", varnames_NGSP), order = "asc")
# df_RGSP_raw <- Quandl(paste0("FRED/", varnames_RGSP), order = "asc")
# 
# 
# 
# df_NGSP <- 
# 	df_NGSP_raw %>% 
# 	gather(var, NGSP, -Date) %>% 
# 	mutate(var = str_extract(var, "\\w{2}NGSP"),
# 				 state_abb = str_replace(var, "NGSP", ""),
# 				 year = as.Date(Date) %>% year,
# 				 Date = NULL,
# 				 var  = NULL) %>% 
# 	select(state_abb, year, NGSP)
# df_NGSP 
#  
# df_RGSP <- 
# 	df_RGSP_raw %>% 
# 	gather(var, RGSP, -Date) %>% 
# 	mutate(var = str_extract(var, "\\w{2}RGSP"),
# 				 state_abb = str_replace(var, "RGSP", ""),
# 				 year = as.Date(Date) %>% year,
# 				 Date = NULL,
# 				 var  = NULL) %>% 
# 	select(state_abb, year, RGSP)
# df_RGSP
# 
# df_GSP_FRED <- left_join(df_NGSP, df_RGSP)
# df_GSP_FRED 	


#**********************************************************************
#               State GDP from BEA: befoe and after 1997           ####
#**********************************************************************

# BEA: 
  # https://www.bea.gov/itable/iTable.cfm?ReqID=70&step=1#reqid=70&step=4&isuri=1&7003=200&7001=1200&7002=1&7090=70

# GSP before 1997, SIC,  US+DC+50
   # Nominal:  1963-1997
   # Real:     1987-1997 in 1997 dollar, adjustment factors vary across states
  
# GSP after 1997: NAICS, US+DC+50
   # Nominal:  1987-1997
   # Real:     1987-1997 in 2009 dollar, adjustment factors vary across states


# GSP before 1997: SIC   See Standard Industrial Classification (SIC) For a complete list of regional statistics, see Regional Definitions.
	# 1/ Estimates for 1977-86 are for the 1972 Standard Industrial Classification (SIC) industries electric and electronic equipment and instruments and related products.
	# 2/ Estimates for 1977-86 are for the 1972 SIC industries banking and credit agencies other than banks.
	# 3/ Estimates for 1977-86 are for the 1972 SIC industries business services and miscellaneous professional services.
	# 4/ The combination of 1987 SIC industries electronic and other electric equipment and instruments and related products is the equivalent of 1972 SIC industries electric and electronic equipment and instruments and related products.
	# 5/ The combination of 1987 SIC industries depository institutions and nondepository institutions is the equivalent of 1972 SIC industries banking and credit agencies other than banks.
	# 6/ The combination of 1987 SIC industries business services and other services is the equivalent of 1972 SIC industries business services and miscellaneous professional services.
	# Note-- SIC Industry detail for the years 1987-97 is based on the 1987 Standard Industrial Classification (SIC). Industry detail for the years 1963-86 is based on the 1972 SIC.
	# Note-- Per capita real GDP statistics for 1987-1997 reflect Census Bureau midyear population estimates.
	# Last updated: November 24, 2010.
	

# GSP after 1997:  NAICS See North American Industrial Classification System (NAICS). For a complete list of regional statistics, see Regional Definitions.
	# Note-- NAICS Industry detail is based on the 2007 North American Industry Classification System (NAICS).
	# Note-- Per capita real GDP statistics for 2010-2016 reflect Census Bureau midyear population estimates available as of December 2016.
	# Last updated: November 21, 2017 -- revised statistics for 2014-2016.


# Difference between NAICS and SIC
	# https://www.naics.com/what-is-the-difference-between-naics-codes-and-sic-codes/

# Why are the 1997 estimates of GDP by state different for SIC and NAICS?
	# https://www.bea.gov/faq/index.cfm?faq_id=455


# How to treat a discontinuity of NAICS and SIC in U.S. GDP?
	# https://www.statalist.org/forums/forum/general-stata-discussion/general/1419580-how-to-treat-a-discontinuity-of-naics-and-sic-in-u-s-gdp

# BEA's caution against appending NAICS and SIC data to construct a single time series
	# https://www.bea.gov/regional/docs/product/

# Pew paper use data from 1996(?) - 2015

# post-1997 GSP from FRED/Quandl are the same as those from BEA


RGSP_BEA_before1997 <- read_excel(paste0(dir_data_raw, 'GSP_BEA/RGSP_BEA_before1997.xls'), skip = 5) %>% filter(!is.na(Area))
RGSP_BEA_after1997  <- read_excel(paste0(dir_data_raw, 'GSP_BEA/RGSP_BEA_after1997.xls'),  skip = 5) %>% filter(!is.na(Area))

NGSP_BEA_before1997 <- read_excel(paste0(dir_data_raw, 'GSP_BEA/NGSP_BEA_before1997.xls'), skip = 5) %>% filter(!is.na(Area))
NGSP_BEA_after1997  <- read_excel(paste0(dir_data_raw, 'GSP_BEA/NGSP_BEA_after1997.xls'),  skip = 5) %>% filter(!is.na(Area))


RGSP_BEA_before1997 %<>% 
	gather(year, RGSP_SIC, -Fips, -Area) %>% 
	mutate(year = as.numeric(year),
	       Fips = NULL,
				 RGSP_SIC = as.numeric(RGSP_SIC)) %>% 
	rename(state = Area)

RGSP_BEA_after1997 %<>% 
	gather(year, RGSP_NAICS, -Fips, -Area) %>% 
	mutate(year = as.numeric(year),
				 Fips = NULL) %>% 
	rename(state = Area)


NGSP_BEA_before1997 %<>% 
	gather(year, NGSP_SIC, -Fips, -Area) %>% 
	mutate(year = as.numeric(year),
				 Fips = NULL) %>% 
	rename(state = Area)

NGSP_BEA_after1997 %<>% 
	gather(year, NGSP_NAICS, -Fips, -Area) %>% 
	mutate(year = as.numeric(year),
				 Fips = NULL) %>% 
	rename(state = Area)





df_RGSP_BEA <- 
	full_join(RGSP_BEA_before1997,
						RGSP_BEA_after1997) %>% 
	left_join(df_us_states) %>% 
	select(state, state_abb, year, everything()) %>% 
	arrange(state, year) 
df_RGSP_BEA

df_NGSP_BEA <- 
	full_join(NGSP_BEA_before1997,
						NGSP_BEA_after1997) %>% 
	left_join(df_us_states) %>% 
	select(state, state_abb, year, everything()) %>% 
	arrange(state, year) 
df_NGSP_BEA





#**********************************************************************
#                   State coincident index                         ####
#**********************************************************************

df_coincIdx <- read_excel(paste0(dir_data_raw, 'coincident-revised_201909.xls'))

df_coincIdx %<>% 
	gather(state_abb, value, -Date) %>% 
	mutate(date = as.yearmon(Date),
				 year = lubridate::year(date),
				 month  = lubridate::month(date)) %>% 
	select(date, year, month, everything(), -Date)
	



#**********************************************************************
#                   State coincident index                         ####
#**********************************************************************

ls_GSP_BEA_raw <- 
	list(
		df_NGSP_BEA = df_NGSP_BEA,
		df_RGSP_BEA = df_RGSP_BEA,
		
		df_coincIdx = df_coincIdx
	)

save(ls_GSP_BEA_raw,
		 file = paste0(dir_data_out, "dataRaw_GSP_BEA.RData"))



