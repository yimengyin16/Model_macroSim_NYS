


# TODO:
#  1. Check details of coincident index: 
#       - seasonality, timing. 


# Goal:
#  1. Annual real GSP growth for all states: use BEA growth directly
#  2. quarterly GSP level index:
#       - Annual growth follows BEA RGSP annual growth 
#       - adjusting quarterly growth follows coincident index

#       - construct quarterly GSP




## Key questions about the validation of the interpolation method. 

# 1. How do US aggregate of BEA RGSP compare to the national GDP? This will determine
#   whether it is appropriate to use the RGSP as the trend in the construction of quarterly state GSP growth

# 2. If applying the interpolation method to the national data, can the BEA data 
#   reproduce (approximately) the offical quarterly GDP growth? 

# 3. Are the coincident index seasonal? Any seasonal adjustment needed?





#*******************************************************************************
#                     Global settings                              ####
#*******************************************************************************
source("libraries.R")

# dir_data <- "inputs/data_raw/"
dir_data <- "inputs/data_proc/"


load(paste0(dir_data, "dataProc_financial.RData"))  # ls_financial
load(paste0(dir_data, "dataRaw_GSP_BEA.RData"))     # ls_GSP_BEA_raw




#*******************************************************************************
#        1.1  Compare annual growth of official GDP and aggregate GSP        ####
#*******************************************************************************

## Official quarterly GDP and GDP growth

# calculate annual GDP growth using June value
# GDP_target_annual <- 
# 	ls_financial$df_financial_q %>% 
# 	filter(year >=1986, month == 6) %>% 
# 	select(year, month, yearMon, GDP) %>% 
# 	mutate(GDP_growth = GDP/lag(GDP) - 1 )
# GDP_target_annual

# calculate annual GDP growth using annual average value
GDP_target_annual_avg <- 
	ls_financial$df_financial_q %>% 
	filter(year >=1986) %>% 
	select(year, month, GDP) %>%
	group_by(year) %>% 
	summarise(GDP = mean(GDP)) %>% 
	mutate(GDP_growth = GDP/lag(GDP) - 1 )



## Annual aggregate GSP
GSP_agg <- 
ls_GSP_BEA_raw$df_RGSP_BEA %>% 
	filter(state_abb == "US", year >= 1987) %>% 
	mutate(RGSP_SIC_growth   =  RGSP_SIC/lag(RGSP_SIC) - 1,
				 RGSP_NAICS_growth =  RGSP_NAICS/lag(RGSP_NAICS) - 1,
				 RGSP_growth = ifelse(year <1998, RGSP_SIC_growth, RGSP_NAICS_growth)) 


## Coincident index, annual average

cIdx_annual_avg <- 
ls_GSP_BEA_raw$df_coincIdx %>% 
	filter(state_abb == "US", year>= 1986) %>% 
	group_by(year) %>% 
	summarise(cIdx = mean(value)) %>% 
	mutate(cIdx_growth = cIdx/lag(cIdx) - 1)


df <- 
select(GSP_agg, year, RGSP_growth) %>% 
	left_join(select(GDP_target_annual_avg, year, GDP_growth), by = "year") %>% 
  left_join(select(cIdx_annual_avg, year, cIdx_growth), by = "year")
df

df %>% 
	gather(type, value, -year) %>% 
	qplot(x = year, y = value, color = type, geom = c("line", "point"), data = .)+
	scale_x_continuous(breaks = seq(1950, 2050, 2))+
	ggtitle("Annual growth: GDP, agg GSP and index")


# 1. Annual official GDP growth based on quarterly average GDP matches the growth 
#    of aggregate GSP better. 
# 2. The match is better after 1997, when NAICS replaced SIC
# 3. The growth coincident index is largely consistent with GDP growth, one notable 
#     exception is that trough of the 2001 recession in the index lags one year behind 
#     of the trough in GDP growth.



#*******************************************************************************
#        1.2  Compare annual growth of US and NY        ####
#*******************************************************************************


## Annual growth of GDP: BEA annual GSP data 
ls_GSP_BEA_raw$df_RGSP_BEA %>% 
	filter(state_abb %in% c("US", "NY"), year >= 1987) %>% 
	mutate(RGSP_SIC_growth   =  RGSP_SIC/lag(RGSP_SIC) - 1,
				 RGSP_NAICS_growth =  RGSP_NAICS/lag(RGSP_NAICS) - 1,
				 RGSP_growth = ifelse(year <1998, RGSP_SIC_growth, RGSP_NAICS_growth)) %>% 
	select(state_abb, year, RGSP_growth) %>% 
	qplot(x = year, y = RGSP_growth, color = state_abb, geom = "line", data = .) +
	ggtitle("Annual GDP growth: US vs. NY")



## Annual growth of coincident index
ls_GSP_BEA_raw$df_coincIdx %>% 
	filter(state_abb %in% c("US", "NY"), year>= 1986) %>% 
	group_by(state_abb, year) %>% 
	summarise(cIdx = mean(value)) %>% 
	mutate(cIdx_growth = cIdx/lag(cIdx) - 1) %>% 
	qplot(x = year, y = cIdx_growth, color = state_abb, geom = "line", data = .) + 
	ggtitle("Annual coincident index growth: US vs. NY")


# Notes:
# 1. NY GDP growth is slightly more volatile than US GDP growth
# 2. NY index growth is much more volatile than US index growth


#*******************************************************************************
#  2.1  Compare qarterly qtr-on-qtr growth of official GDP and interpolated growth   ####
#*******************************************************************************

## Official quarterly GDP and GDP growth (quarter on quarter) Year >= 1987
GDP_target_qtr <- 
	ls_financial$df_financial_q %>% 
	filter(year >=1986) %>% 
	mutate(qtr = case_when(month == 3 ~ 1,
												 month == 6 ~ 2,
												 month == 9 ~ 3, 
												 month == 12 ~ 4)
	) %>% 
	select(year, qtr, month, yearMon, GDP) %>% 
	mutate(GDP_growth = GDP/lag(GDP) - 1 )
GDP_target_qtr


## Quarterly coincident index: quarterly average of monthly values
df_monthQtr <- 
   tibble(month = 1:12,
   			  qtr   = rep(1:4, each = 3 ))
df_monthQtr 

cIdx_qtr <- 
ls_GSP_BEA_raw$df_coincIdx %>% 
	filter(state_abb == "US", year >= 1986) %>% 
	left_join(df_monthQtr, by = "month") %>% 
	group_by(year, qtr) %>% 
	summarise(cIdx = mean(value)) %>% 
	ungroup %>% 
	mutate(cIdx_growth = cIdx / lag(cIdx) - 1)

	
## Adjustment factor for coincident index
# adj_fct = [(1+annual GDP growth)/(1+annual index growth)]^(1/4)

# For now annual growth calculated using annual averages
# Ideally, should use year-start to year-end growth 


df_adjFactor <- 
	left_join(GDP_target_annual_avg, 
	   				cIdx_annual_avg) %>% 
	mutate(adj_fct = ((1 + GDP_growth) / (1 + cIdx_growth))^(1/4)  )
df_adjFactor


## Adjusting index grwoth
cIdx_qtr %<>% 
	left_join(select(df_adjFactor, year, adj_fct), by = "year") %>% 
	mutate(cIdx_growth_adj = cIdx_growth * adj_fct)




## Compare coincident index growth (unadjusted and adjusted) with official GDP growth, 
select(GDP_target_qtr, year, qtr, GDP_growth) %>% 
	left_join(cIdx_qtr %>% select(year, qtr, cIdx_growth, cIdx_growth_adj)) %>% 
	filter(year<2020) %>% 
	#filter(year<2010, year >=2000) %>%
	mutate(yearQtr = year + (qtr - 1) /4) %>% 
	select(-year, -qtr) %>% 
	gather(type, value, -yearQtr) %>% 
	qplot(x = yearQtr, y = value*100, color = type, geom = c("line", "point") , data=.) + 
	scale_x_continuous(breaks = seq(1950, 2050, 2)) + 
	ggtitle("Qtr-on-Qtr growth: GDP and coincident index")

## Q-on-q growth of the c-index is much less volatile than the q-on-q growth of GDP.

# GDP_target_qtr %>% 
# 	filter(year>2000)


#*******************************************************************************
#  2.2  Compare qarterly y-on-y growth of official GDP and interpolated growth   ####
#*******************************************************************************





















