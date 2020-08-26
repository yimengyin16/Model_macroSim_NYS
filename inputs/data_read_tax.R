# Loading state and local government tax data 


#*******************************************************************************
#                          Notes                                   ####
#*******************************************************************************

# Goal:
		# 1. Load state and local government finance data
    # 2. Standardize data formats

#*******************************************************************************
#                           Packages                               ####
#*******************************************************************************

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



#*******************************************************************************
#     Census Bureau Annual Survey of State and Local Government Finances    ####
#*******************************************************************************

#' Source: Tax Policy Center, Urban Institute,  
#'         http://slfdqs.taxpolicycenter.org/pages.cfm


# TODO: update the data to 2020
#' State and local government revnue compiled by Urban Institue.
#'   - Data values are slightly different from the Census data but are generally consistent.
#'   - Many government types and revenue variables are available, data loaded here are from
#'     a data query that includes the folling variables:
#'   - 1977-2015
#'   - Real values are based on 2015 dollar:
#'       Finance data are adjusted to 2015 dollars using the Consumer Price Index (CPI) from the 
#'       Bureau of Labor Statistics. The parameters for the CPI used are Not Seasonally Adjusted, 
#'       All-Urban Consumers, All Cities Average, and All Items. Data are for the calendar year of the finance data.
#'   - The same adjustment factors are applied to all levels and all tax categories. 


#' selected series:
#' 	- (R01) Total revenue
#'  - (R02) Total Rev-Own Sources 
#'  - (R03) General Revenue
#'  - (R04) Gen Rev-Own Sources
#'  - (R05) Total Taxes
#'  - (R06) Property Taxes
#'  - (R08) Tot Sales & Gr Rec Tax
#'  - (R09) Total Gen Sales Tax (T09)
#'  - (R10) Total select Sales Tax
#'  - (R27) Individual Income Tax (T40)
#'  - (R28) Corp Net Income Tax 
#'  - (R36) Tot Chgs and Misc Rev
#' Type:
#' 	- State/local: total, nominal
#'  - Local: total, nominal 
#'  - State: total, nominal
#' 

#' Notes:  
#'  - state and local sum up to "state and local " only for 'own source revenue and tax'
#'
#'
#'
#' Notes from Census
#' https://www2.census.gov/govs/local/state-local_govt_fin_user_notes.pdf




# State and local
df_govRevA_SL_nom <- read_excel(paste0(dir_data_raw, 'taxCensusAnnual/Rev_urban_SL_tot_nom.xlsx'), skip = 3)
df_govRevA_S_nom  <- read_excel(paste0(dir_data_raw, 'taxCensusAnnual/Rev_urban_S_tot_nom.xlsx'),  skip = 3)
df_govRevA_L_nom  <- read_excel(paste0(dir_data_raw, 'taxCensusAnnual/Rev_urban_L_tot_nom.xlsx'),  skip = 3)

df_govRevA_SL_real <- read_excel(paste0(dir_data_raw, 'taxCensusAnnual/Rev_urban_SL_tot_real.xlsx'), skip = 3)
df_govRevA_S_real  <- read_excel(paste0(dir_data_raw, 'taxCensusAnnual/Rev_urban_S_tot_real.xlsx'),  skip = 3)
df_govRevA_L_real  <- read_excel(paste0(dir_data_raw, 'taxCensusAnnual/Rev_urban_L_tot_real.xlsx'),  skip = 3)


df_govRevA_SL_nom
df_govRevA_S_nom
df_govRevA_L_nom

df_govRevA_SL_real
df_govRevA_S_real
df_govRevA_L_real




fn <- function(df){	
	df %>% 
		filter(!is.na(Year)) %>% 
		gather(varcode, value, -State, -Year) %>% 
	  mutate(varcode = str_extract(varcode,  '\\(([^()]*)\\)'), # '\\(([^)]+)\\)')) # extract text inside (): http://www.rexegg.com/regex-cookbook.html#captureparen
	  			 varcode = str_replace(varcode, '\\(', ''),
	  			 varcode = str_replace(varcode, '\\)', ''),
	  			 value   = as.numeric(value),   # DC values for 2013-14 are 'N/A'.
	  			 value   = ifelse(is.na(value), 0, value)) %>% 
		rename(state = State)
}

df_govRevA_SL_nom %<>% fn %>% mutate(type = "SL") 
df_govRevA_L_nom  %<>% fn %>% mutate(type = "local") 
df_govRevA_S_nom  %<>% fn %>% mutate(type = "state") 

df_govRevA_SL_real %<>% fn %>% mutate(type = "SL") 
df_govRevA_L_real  %<>% fn %>% mutate(type = "local") 
df_govRevA_S_real  %<>% fn %>% mutate(type = "state") 


df_govRevA_nom <-  
	bind_rows(df_govRevA_SL_nom,
						df_govRevA_S_nom,
						df_govRevA_L_nom) %>% 
	mutate(nomReal = "nom")

df_govRevA_real <-  
	bind_rows(df_govRevA_SL_real,
						df_govRevA_S_real,
						df_govRevA_L_real) %>% 
	mutate(nomReal = "real")

df_govRevA_nom
df_govRevA_real

# Index data frame 1: Full state names and abbreviations 
df_us_states <- tibble(state = c(state.name, "District of Columbia", "United States"), state_abb = c(state.abb, "DC", "US"))
# x <- data.frame(df_govRevA_nom$State %>% unique %>% sort, us_states$state %>% sort)
# identical(x[[1]], x[[2]])


# Index data frame 2: urban variable index and short variable names 
df_vars <- 
		tribble(
			~varcode, ~varname, ~vardesc, 
		  'R01', 'rev_tot',          'Total revenue', 
			'R02', 'rev_tot_ownSrc',   'Total Rev-Own Sources',
			'R03', 'rev_gen',          'General Revenue',
			'R04', 'rev_gen_ownSrc',   'Gen Rev-Own Sources',
			'R05', 'tax_tot',          'Total Taxes',
			'R06', 'tax_property',     'Property Taxes',
			'R08', 'tax_sales_tot',    'Tot Sales & Gr Rec Tax',
			'R09', 'tax_sales_gen',    'Total Gen Sales Tax (T09)',
			'R10', 'tax_sales_sel',    'Total select Sales Tax',
		  'R27', 'tax_indivIncome',  'Individual Income Tax (T40)',
			'R28', 'tax_corpIncome',   'Corp Net Income Tax',
			'R36', 'chgs_Misc',        'Tot Chgs and Misc Rev'
		)


# comebine data and indices
df_govRevA_nom %<>% 
	left_join(df_us_states) %>% 
	left_join(df_vars) %>% 
	select(state,state_abb, type, nomReal, year = Year, varcode, varname, value, vardesc)
df_govRevA_nom

df_govRevA_real %<>% 
	left_join(df_us_states) %>% 
	left_join(df_vars) %>% 
	select(state,state_abb, type, nomReal, year = Year, varcode, varname, value, vardesc)
df_govRevA_real




#*******************************************************************************
#     Census Bureau Quarterly Summary of State and Local Tax Revenues    ####
#*******************************************************************************

# source: https://www.census.gov/programs-surveys/qtax.html
# non-digitized data before 1994: https://www.census.gov/programs-surveys/qtax/data/tables.All.html 




# Define data range:
df_govRevQ_category  <- read_excel(paste0(dir_data_raw, 'taxCensusQuarterly/QTAX-mf.xlsx'), range = c("a2:d5"))
df_govRevQ_types     <- read_excel(paste0(dir_data_raw, 'taxCensusQuarterly/QTAX-mf.xlsx'), range = c("a9:d47"))
df_govRevQ_geoLevels <- read_excel(paste0(dir_data_raw, 'taxCensusQuarterly/QTAX-mf.xlsx'), range = c("a51:c103"))
df_govRevQ_time      <- read_excel(paste0(dir_data_raw, 'taxCensusQuarterly/QTAX-mf.xlsx'), range = c("a107:b220"))
df_govRevQ_data      <- read_excel(paste0(dir_data_raw, 'taxCensusQuarterly/QTAX-mf.xlsx'), skip = 231)  # c("a232:f141753") 


##' Indices:

#' 1. Category: cat_idx = 3, Table 3 - Latest State Tax Collections by State and Type of Tax

#' 2. Index data frame 2: urban variable index and short variable names 

df_varsQ <- 
	tribble(
		  ~dt_idx, ~varname,
			37,    'tax_tot',   
			27,    'tax_indivIncome',
			29,    'tax_corpIncome',
			3 ,    'tax_sales_gen',
			15,    'tax_sales_sel_other',
			5 ,    'tax_sales_sel_alc',
			7 ,    'tax_sales_sel_amuse',
			9 ,    'tax_sales_sel_fuels',
			11,    'tax_sales_sel_pm',  # pari-mutuels
			12,    'tax_sales_sel_utilities',
			13,    'tax_sales_sel_tobacco',
			1,     'tax_property'
	)
			
#' 3. geo levels

df_govRevQ_geoLevels %<>% 
	left_join(df_us_states, by = c("geo_code" = "state_abb")) %>% 
	select(geo_idx, state_abb = geo_code, state )

#' 4. time periods	
df_govRevQ_time %<>% 
	mutate(year = as.numeric(str_sub(per_name, -4, -1)),
				 qtr  = as.numeric(str_sub(per_name, 2, 2)),
				 yearqtr = year + (qtr - 1)/4
				 ) %>% 
	select(-per_name)
	


##' Combine data and indices 

df_govRevQ <- 
  df_govRevQ_data %>% 
	filter(cat_idx == 3, dt_idx %in% df_varsQ$dt_idx) %>% 
	left_join(df_govRevQ_geoLevels, by = "geo_idx") %>% 
	left_join(df_govRevQ_time, by = "per_idx") %>% 
	left_join(df_varsQ, by = "dt_idx") %>% 
	select(state, state_abb, year, qtr, yearqtr, varname, value = val)
	

##' construct total selective sales tax and total sales tax

df_govRevQ <- 
	bind_rows(
		df_govRevQ,
	
	  df_govRevQ %>% 
			filter(str_detect(varname, "tax_sales_sel")) %>% 
			group_by(state, state_abb, year, qtr, yearqtr) %>% 
			summarise(value = sum(value)) %>% 
			mutate(varname = "tax_sales_sel")
	) %>% 
	filter(!str_detect(varname, "tax_sales_sel_")) %>% 
	mutate(nomReal = "nom") %>% 
	arrange(state_abb, year, qtr, varname)
	
# df_govRevQ %>% spread(varname, value)


ls_govRev <- list(
	df_govRevA_nom  = df_govRevA_nom,
	df_govRevA_real = df_govRevA_real,
	
	df_govRevQ = df_govRevQ,
	
	df_us_states = df_us_states
)


save(ls_govRev,
	#    df_govRevA_nom,
	#    df_govRevA_real,
	#    
	#    df_govRevQ,
	# 	 
	# 	 df_us_states,
		 
		 file = paste0(dir_data_out, "dataRaw_govRev.RData"))











