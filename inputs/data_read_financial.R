# loading data


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************

# Goal:
		# Load and organize data from each source into the same format that is easy for merge operations. 
		# The format:
		# 	- data frame
		# 	- Three date variables: year, month, date(in Date format)
		# 	- All variables, within and across data sources, have unique names.


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



# Note for Don: The api key is associated with my Quandl account. 
# You may want to register to obtain you own api key (Account settings -> API Key)


#**********************************************************************
#            Loading data from FRED                ####
#**********************************************************************

# http://rstudio-pubs-static.s3.amazonaws.com/24858_1f006c3965614b0099c963913100e9f0.html


# Major economic variables

FRED_vars <- c(
  "GDPC1",           # Quarterly, Seasonally adjusted GDP level, billion
  
  "A191RL1Q225SBEA", # Quarterly, seasonally adjusted GDP growth, annual rate
  "TB3MS",           # 3-Month Treasury-bill: secondary market rate, monthly
  "GS2",             # 2-Year  Treasury constant maturity rate
  "GS10",            # 10-Year Treasury constant maturity rate
  "GS20",            # 20-Year Treasury constant maturity rate
  "GS30",            # 30-Year Treasury constant maturity rate
  
  "CPIAUCSL",        # CPI-U, seasonally adjusted
  "CPIAUCNS",        # CPI_U, not seasonally adjusted
  "CPILFESL",        # core CPI: CPI-U less food and energy, seasonally adjusted 
  
  "GDPDEF",          # GDP implicit price deflator,Quarterly, seasonally adjusted, 2009 = 100
  "GDPCTPI",         # GDP Chain-type price index, Quarterly, seasonally adjusted, 2009 = 100
  
  #"B191RA3Q086SBEA", # GDP chain-type quantity index (discontinuted), quarterly, seasonally adjusted, 2009 = 100
  
  #"AAA",            # Moody's Seasoned Aaa Corporate Bond Yield, monthly not seasonally adjusted
  
  "UNRATE"           # civilian unemployent rate, seasonally adjusted
)

# loading data through Quandl (2000 calls per 10 mins). 
df_FRED <- Quandl(paste0("FRED/", FRED_vars), order = "asc")
names(df_FRED) <- str_replace_all(names(df_FRED), c("FRED." = "", " - Value" = ""))

df_FRED %<>% 
	mutate(year    = lubridate::year(Date),
				 month   = lubridate::month(Date),
				 yearMon = as.yearmon(Date)) %>% 
	select(year, month, yearMon, everything(), -Date) %>% 
	rename(
		     GDP = GDPC1, 
				 GDP_growth = A191RL1Q225SBEA,
				 TBill3m  = TB3MS,
				 TBond2y  = GS2,
				 Tbond10y = GS10,
				 Tbond20y = GS20,
				 Tbond30y = GS30,
				 CPIU_SA  = CPIAUCSL,
				 CPIU_NA  = CPIAUCNS,
				 CPIc_SA  = CPILFESL,
				 
				 GDPdeflator = GDPDEF,
				 GDPCTPI = GDPCTPI,
				 #GDPCTQI = B191RA3Q086SBEA,
				 
				 unrate_SA= UNRATE       
	) 
# %>% mutate(GDPdeflator_FRED = na.locf(GDPdeflator_FRED, na.rm = FALSE),  .
# 				 GDPCTPI_FRED     = na.locf(GDPCTPI_FRED, na.rm = FALSE),
				
names(df_FRED)	
df_FRED



# data through quantmod
macroData <- new.env()
getSymbols("AAA", src = "FRED", env = macroData )
df_AAA <- macroData$AAA %>% as.data.frame()
df_AAA %<>%  
	mutate(Date  = row.names(df_AAA),
         year    = lubridate::year(Date),
				 month   = lubridate::month(Date),
				 yearMon = as.yearmon(Date)) %>%
	rename(CBond_Yield_AAA = AAA) %>% 
	select(year, month, yearMon, everything(), -Date) 


# Merging data
df_FRED <- full_join(df_FRED, df_AAA, by = c("year", "month", "yearMon"))
head(df_FRED)
tail(df_FRED,100)



# Quarterly GDP and price indices are given in month 1, 4, 7, 10. 
# Fill other month with value in the first month of the quarter

df_FRED  %<>% group_by(year) %>% 
	mutate(
		GDP = case_when(
			month %in% 1:3   ~ as.numeric(GDP[ 1]),
			month %in% 4:6   ~ as.numeric(GDP[ 4]),
			month %in% 7:9   ~ as.numeric(GDP[ 7]),
			month %in% 10:12 ~ as.numeric(GDP[ 10])
	),
  	GDP_growth = case_when(
			month %in% 1:3   ~ GDP_growth[1],
			month %in% 4:6   ~ GDP_growth[4],
			month %in% 7:9   ~ GDP_growth[7],
			month %in% 10:12 ~ GDP_growth[10]
	),
	
	  GDPdeflator = case_when(
		month %in% 1:3   ~ GDPdeflator[1],
		month %in% 4:6   ~ GDPdeflator[4],
		month %in% 7:9   ~ GDPdeflator[7],
		month %in% 10:12 ~ GDPdeflator[10]
	),
	
	 GDPCTPI = case_when(
		month %in% 1:3   ~ GDPCTPI[1],
		month %in% 4:6   ~ GDPCTPI[4],
		month %in% 7:9   ~ GDPCTPI[7],
		month %in% 10:12 ~ GDPCTPI[10]
	)
)

df_FRED



#**********************************************************************
#                     Loading data from SBBI Yearbook              ####
#**********************************************************************

# Data from SBBI2016 yearbook appendix B: 1/1926~12/2015
SBBI_AppendB_vars <- c("LCapStock_TRI",
										 	"LCapStock_CAI",
										 	"SCapStock_TRI",
										 	"CBond_TRI",
										 	"LTGBond_TRI",
										 	"MTGBond_TRI",
										 	"TBills_TRI",
										 	"Inflation_Index")  

SBBI_AppendA_vars <- c("LTGBond_TR",
	                     "MTGBond_TR",
	                     "LTGBond_Yield",
											 "MTGBond_Yield",
											 "CBond_TR") 

SBBI_AppendB_file <- paste0(dir_data_raw, "SBBI2016_AppendixB.xlsx")
SBBI_AppendB_cell <- "A3:M94"

SBBI_AppendA_file <- paste0(dir_data_raw, "SBBI2016_AppendixA.xlsx")
SBBI_AppendA_cell <- "A3:M93"



fn <- function(fileName, varName, cells){
	  # a function to read a single sheet and convert it into long format
	  read_excel(fileName, varName, cells) %>% 
		gather(month, var, -Year) %>% 
		mutate(month   = match(month, substr(month.name, 1, 3)),
					 varName = varName) %>% 
		arrange(Year, month, var) %>% 
		rename(year = Year)
}


df_SBBI_AppendB <-  
		sapply(SBBI_AppendB_vars, fn, fileName = SBBI_AppendB_file, cells = SBBI_AppendB_cell, simplify = FALSE) %>% 
		bind_rows() %>%
	  mutate(varName = factor(varName, levels = SBBI_AppendB_vars)) %>% 
	  spread(varName, var) %>% 
	  mutate(yearMon = as.yearmon(paste(year,  month, sep =  "-"))) %>% 
	  select(year, month, yearMon, everything())

df_SBBI_AppendA <- 
	sapply(SBBI_AppendA_vars, fn, fileName = SBBI_AppendA_file, cells = SBBI_AppendA_cell, simplify = FALSE) %>% 
	bind_rows() %>%
	mutate(varName = factor(varName, levels = SBBI_AppendA_vars)) %>% 
	spread(varName, var) %>% 
	mutate(yearMon = as.yearmon(paste(year,  month, sep =  "-"))) %>% 
	select(year, month, yearMon, everything())


head(df_SBBI_AppendB)
tail(df_SBBI_AppendB)

head(df_SBBI_AppendA)
tail(df_SBBI_AppendA)



#**********************************************************************
#                     Loading R. Shiller data                      ####
#**********************************************************************
# df_Shiller <- read_xls(paste0(dir_data_raw,"RShiller_data.xls"), sheet = "Data", skip = 7) %>% 
# 	mutate(yearMon = str_replace(as.character(Date), "\\.", "-") %>% as.yearmon(),
# 				 year  = year(yearMon),
# 				 month = month(yearMon)) %>% 
# 	filter(!is.na(year)) %>% 
# 	select(year, month, yearMon, everything(), -Date)
# 	
# head(df_Shiller)
# tail(df_Shiller)



#**********************************************************************
#                     Loading yahoo finance data                    ####
#**********************************************************************

yahoo_vars <- c("^GSPC", #SP500 index (price only)
						 "^SP500TR"  #SP500 total return 
				   	 #"^W5000",
						 #"^RUA"    # Russell 3000 Index 
)

# use quantmod package
env_yahoo <- new.env()
getSymbols(yahoo_vars, src = "yahoo", env = env_yahoo, 
					 from = "1900-01-01", periodicity = "monthly")

SP500   <- env_yahoo$GSPC %>% Cl     # get closing prices
SP500TR <- env_yahoo$SP500TR %>% Cl  # get closing prices

df_yahoo <- cbind(SP500, SP500TR) %>% as.data.frame
df_yahoo %<>%
	mutate(Date    = row.names(df_yahoo),
				 year    = year(Date),
				 month   = month(Date),
				 yearMon = as.yearmon(Date)) %>%
	rename(SP500 = GSPC.Close,
				 SP500TR = SP500TR.Close) %>% 
	select(year, month, yearMon, everything(), -Date)
	
head(df_yahoo)
tail(df_yahoo)

# # get monthly returns
# GSPC.pctchange <- ClCl(GSPC)
# GSPC.pctchange %>% head()




#**********************************************************************
#               Saving data                ####
#**********************************************************************


ls_financial <- list(
	df_FRED = df_FRED, 
	df_SBBI_AppendA = df_SBBI_AppendA,
	df_SBBI_AppendB = df_SBBI_AppendB, 
	df_yahoo        = df_yahoo
)


save(ls_financial,
		 file = paste0(dir_data_out, "dataRaw_financial.RData"))


# save(df_FRED, 
# 		 df_SBBI_AppendA,
# 		 df_SBBI_AppendB, 
# 		 df_yahoo
# 		 file = paste0(dir_data_raw, "dataRaw.RData"))





