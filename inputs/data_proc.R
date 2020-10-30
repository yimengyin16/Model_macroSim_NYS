#*******************************************************************************
#                          Notes                                            ####
#*******************************************************************************

# 1. Create a new variable LCap_TRI for total return index of large cap stocks (SP500)
  # 1926 - 2015-12: SBBI large cap total return index  
  # 2016-1 ~      : SP500TR from yahoo finance

# 2. Quarterly and annual data from monthly data. 
  # Quarterly data: use Mar, June, Sep, Dec series, Sherries and Zhang 2009, p20 
  # Annual data:    use June series, HSZ2016, p28



#*******************************************************************************
#                          Goals                                            ####
#*******************************************************************************

## Key points to make besides estimating parameters for simulation
#   - NY tax revnues are more volatile than national average/other sales-tax states,
#      this is partly due to higher impact of capital gains, and this trend is likely 
#      to sustain. 
#   - Less important: NY GDP is also more volatile/cyclical than national total, and this is partly
#      due to a higher dependency on financial sector (what if excluding NYC?). 



# Model 1: GDP and stock returns: quarter model
#  - sub-models
#      - US, quarterly single series 
#      - NY, quarterly single series 
#      - All states: quarterly panel
#  - Data: quarterly national GDP growth and stock return 
#          quarterly state GDP growth and stock return
#            - issue: quarter-on-quarter state GDP growths need to be seasonally adjusted 
#
#  - Goal: establish the relationship between NY GDP growth and asset returns
#      - approach 1: return ~ US GDP ~ NY GDP
#      - appraoch 2: return ~ NY GDP
#            - the business cycles may not be consistent with national GDP cycles



# Model 2: GDP and tax revenues, annual model
#  - sub-models: 
#     - US, 
#     - NY, 
#     - All states, panel
#  - Data: annual national GDP 
#          annual state GDP
#          annual national tax revenue by type
#          annual state tax revenue by type


# Model 3: GDP and tax revenues, quarterly model
#  - sub-models:
#     - US, 
#     - NY, 
#     - state panel
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
 

# Goal: four data frames:
#  1. Annual  national 
#  2. Quarter national 
#  3. Annual state  (at least NY)
#  4. Quarter state (at least NY)


#**********************************************************************
#                           Packages                               ####
#**********************************************************************

# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf





#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
dir_data_raw <- "inputs/data_proc/"
dir_data_out <- "inputs/data_proc/"


df_monthQtr <- 
  tibble(month = 1:12,
         qtr   = rep(1:4, each = 3 ))
df_monthQtr 


#**********************************************************************
#           1 Financial + US GDP data                   ####
#**********************************************************************


#**********************************************************************
#           1.1 loading raw Data                  ####
#**********************************************************************

# Loading saved data 
load(paste0(dir_data_raw, "dataRaw_financial.RData"))

# ls_financial_raw is loaded, which contains the following data frames:
#   - df_FRED, 
#   - df_SBBI_AppendA,
#   - df_SBBI_AppendB, 
#   - df_yahoo

df_financial <-
  Reduce(full_join,
         ls_financial_raw) %>%
  ungroup



#**********************************************************************
#           1.2 Examine and combine stock series                 ####
#**********************************************************************

# # Large Cap stock total return index.
# df1 <- df_financial%>% select(yearMon, LCapStock_TRI, SP500TR) %>% 
#   filter(yearMon >= as.yearmon("1988-1")) %>% 
#   mutate(LCapStock_TRI = LCapStock_TRI * SP500TR[1]/LCapStock_TRI[1],
#          diff = LCapStock_TRI / SP500TR - 1) 
# df1$diff %>% range(na.rm = T)
# 
# # Large Cap stock price index.
# df2 <- df_financial %>% select(yearMon, LCapStock_CAI, SP500) %>% 
#   filter(yearMon >= as.yearmon("1950-1")) %>% 
#   mutate(LCapStock_CAI = LCapStock_CAI * SP500[1]/LCapStock_CAI[1],
#          diff = LCapStock_CAI / SP500 - 1) 
# df2$diff %>% range(na.rm = T)


# LCapStock_TRI and SP500TR are consistent. SP500TR can be used to update LCapStock_TRI
# with data after 2016-1

# LCapStock_CAI and SP500 are consistent. SP500 can be used to update LCapStock_CAI
# with data after 2016-1

df_financial %<>% 
  mutate(LCapStock_TRI_ext = ifelse(yearMon <= as.yearmon("2015-12"),
                             LCapStock_TRI,
                             SP500TR * (LCapStock_TRI/SP500TR)[yearMon == as.yearmon("2015-12")]),
         LCapStock_CAI_ext = ifelse(yearMon <= as.yearmon("2015-12"),
                             LCapStock_CAI,
                             SP500 * (LCapStock_CAI/SP500)[yearMon == as.yearmon("2015-12")])
         # LCap_DI  = LCap_TRI/lag(LCap_TRI) * lag(LCap_CAI) - LCap_CAI
  )

df_financial %<>% 
  mutate(LCapStock_TRI_ext_real = LCapStock_TRI_ext / Inflation_Index,
         LCapStock_TRI_real     = LCapStock_TRI / Inflation_Index,
         
         CBond_TRI_real     = CBond_TRI     / Inflation_Index,
         LTGBond_TRI_real   = LTGBond_TRI   / Inflation_Index,
         MTGBond_TRI_real   = MTGBond_TRI   / Inflation_Index,
         TBills_TRI_real    = TBills_TRI    / Inflation_Index
         )



# #**********************************************************************
# #                 Examine bond return and yield data               
# #**********************************************************************
# 
# df4 <- 
#   df_dataAll %>% 
#   select(yearMon, CBond_TR, CBond_TRI, CBond_Yield_AAA)
# 


#**********************************************************************
#         1.3       Construct quarterly and annual data            ####
#**********************************************************************

## Quarterly data: third-month value
df_financial_q  <- df_financial %>% filter(month %in% c(3, 6, 9, 12))
## Quarterly data: first-month value
df_financial_q2 <- df_financial %>% filter(month %in% c(1, 4, 7, 10))



## Annual value
df_financial_y  <- df_financial %>% filter(month %in% 6)

# Annual GDP values constructed with different methods
 # GDP and GDP_mon6: GDP in June
 # GDP_avg: Quarterly average of GDP
df_financial_y %<>% 
  left_join(
    df_financial %>% 
      select(year, month, GDP) %>% 
      group_by(year) %>% 
      summarise(GDP_avg = mean(GDP), 
                .groups = "drop"),
    by = "year"
    ) %>% 
  mutate(GDP_mon6 = GDP)



#**********************************************************************
#         1.4        Examine bond return and yield data            ####        
#********************************************************************** 

# General notes
#  - Level and index variables are the most important variables. Growth and returns 
#    of any time interval can be calculated from them. 


#### Summary of monthly financial data

## Financial variables from FRED

#' GDP:           "GDPC1",           # Quarterly, Seasonally adjusted GDP level, billion
#' GDP_BEAgrowth  "A191RL1Q225SBEA", # Quarterly, seasonally adjusted GDP growth, annual rate
#'
#' TBill3m        "TB3MS",           # 3-Month Treasury-bill: secondary market rate, monthly,  not seaonally adjusted
#' TBond2y        "GS2",             # 2-Year  Treasury constant maturity rate, not seaonally adjusted
#' TBond10y       "GS10",            # 10-Year Treasury constant maturity rate, not seaonally adjusted
#' TBond20y       "GS20",            # 20-Year Treasury constant maturity rate, not seaonally adjusted
#' TBond30y       "GS30",            # 30-Year Treasury constant maturity rate, not seaonally adjusted
#' 
#' CPIU_SA        "CPIAUCSL",        # CPI-U, seasonally adjusted
#' CPIU_NA        "CPIAUCNS",        # CPI_U, NOT seasonally adjusted
#' CPIc_SA        "CPILFESL",        # core CPI: CPI-U less food and energy, seasonally adjusted 
#' 
#' GDPdeflator    "GDPDEF",          # GDP implicit price deflator,Quarterly, seasonally adjusted, 2009 = 100
#' GDPCTPI        "GDPCTPI",         # GDP Chain-type price index, Quarterly, seasonally adjusted, 2009 = 100
#' 
#' 
#' CBond_Yield_AAA #"AAA",           # Moody's Seasoned Aaa Corporate Bond Yield, monthly not seasonally adjusted
#' 
#' unrate_SA      "UNRATE"           # civilian unemployent rate, seasonally adjusted



# Asset return variables from SBBI: 
# Note: None is seasonally adjusted. 

#'  "LCapStock_TRI",        # Large-cap stock total return index 
#'  "LCapStock_CAI",        # Large-cap stock captial appreciation index
#'  "SCapStock_TRI",        # Small-cap stock total return index
#'  "CBond_TRI",            # Commercial(?) bond total return index
#'  "LTGBond_TRI",          # Long-term gov bond total return index
#'  "MTGBond_TRI",          # Medium-term gov bond total return index
#'  "TBills_TRI",           # Treasury bills (short-term) total return index
#'  "Inflation_Index"       # Inflation index (need description)  
#'
#'  "LTGBond_TR",           # Long-term gov bond total rate of return 
#'  "MTGBond_TR",           # Medium-term gov bond total rate of return
#'  "CBond_TR"              # Commercial bond total rate of return 
#'  "LTGBond_Yield",        # Long-term gov bond yield
#'  "MTGBond_Yield",        # Medium-term gov bond yield
#'
#'
#'  LCapStock_TRI_real      # LCapStock_TRI
#'  CBond_TRI_real          # CBond_TRI_real
#'  LTGBond_TRI_real        # LTGBond_TRI_real
#'  MTGBond_TRI_real        # MTGBond_TRI_real
#'  TBills_TRI_real         # TBills_TRI_real
#'  
#'  LCapStock_TRI_ext       # LCapStock_TRI extended by SP500TR after Jan 2016
#'  LCapStock_CAI_ext       # LCapStock_CAI extended by SP500 after Jan 2016
#'  LCapStock_TRI_ext_real  # LCapStock_TRI / inflation_Index




#**********************************************************************
#              1.5   Save data                                     ####
#**********************************************************************

ls_financial <- list(
  df_financial    = df_financial,
  df_financial_q  = df_financial_q,
  df_financial_q2 = df_financial_q2,
  df_financial_y  = df_financial_y
)
  

saveRDS(ls_financial,
        file =  paste0(dir_data_out, "dataProc_financial.rds"))




#**********************************************************************
#                   2 Tax revenue                                  ####
#**********************************************************************


#**********************************************************************
#                   2.1 loading raw data                           ####
#**********************************************************************

# Loading saved data 
load(paste0(dir_data_raw, "dataRaw_govRev.RData"))

# loaded, ls_govRev_raw  which includes:
#  - df_govRevA_nom
#  - df_govRevA_real
#  - df_govRevQ
#  - df_us_states

# ls_govRev_raw$df_us_states

#**********************************************************************
#                   2.2 Annual tax revenue, US and states          ####
#**********************************************************************

df_govRev_y_nom <- 
  ls_govRev_raw$df_govRevA_nom %>% 
  mutate(varname = paste0(varname,"_",nomReal, "_", type)) %>% 
  select(state_abb, year, varname, value) %>% 
  spread(varname, value) 
#df_govRev_y_nom


df_govRev_y_real <- 
ls_govRev_raw$df_govRevA_real %>%
  mutate(varname = paste0(varname,"_",nomReal, "_", type)) %>% 
  select(state_abb, year, varname, value) %>% 
  spread(varname, value) 
# df_govRev_y_real


df_govRev_y <- 
  full_join(df_govRev_y_real, 
            df_govRev_y_nom, 
            by = c("state_abb", "year"))


#**********************************************************************
#                   2.3 Quarterly tax revenue, US and states          ####
#**********************************************************************

df_govRev_q <- 
  ls_govRev_raw$df_govRevQ %>% 
  mutate(varname = paste0(varname,"_",nomReal, "_", "state")) %>% 
  select(state_abb, year, qtr, yearqtr, varname, value) %>% 
  spread(varname, value)



#**********************************************************************
#              2.4  Notes for df_RevGSP                            ####
#**********************************************************************

## Annual tax revenue 
#
# Variables
#  Indices: state, state_abb, year
#  Tax and revenue variables: 1977-2015
#     - Region: 
#        - 50 states + DC
#        - US (national total)
#     - suffix: 
#        - local
#        - state
#        - SL: state and local
#     - nominal/real
#        - nom
#        - real
#     - variables:   
#        urban code  Var name					   Var description
#        'R01',     'rev_tot',          'Total revenue', 
#        'R02',     'rev_tot_ownSrc',   'Total Rev-Own Sources',
#        'R03',     'rev_gen',          'General Revenue',
#        'R04',     'rev_gen_ownSrc',   'Gen Rev-Own Sources',
# 
#        'R05',     'tax_tot',          'Total Taxes',
#        'R06',     'tax_property',     'Property Taxes',
#        'R08',     'tax_sales_tot',    'Tot Sales & Gr Rec Tax',
#        'R09',     'tax_sales_gen',    'Total Gen Sales Tax (T09)',
#        'R10',     'tax_sales_select', 'Total select Sales Tax',
#        'R27',     'tax_indivIncome',  'Individual Income Tax (T40)',
#        'R28',     'tax_corpIncome',   'Corp Net Income Tax',
# 
#        'R36',     'chgs_Misc',        'Tot Chgs and Misc Rev'
#      - with missing values 
#'     - TODO: how real values are calculated.  


## Quarterly tax revenue

#' 1. Category: cat_idx = 3, Table 3 - Latest State Tax Collections by State and Type of Tax
#  2. Variables
#'     ~dt_idx, ~varname,
#'     37,      'tax_tot',   
#'     27,      'tax_indivIncome',
#'     29,      'tax_corpIncome',
#'     3 ,      'tax_sales_gen',
#'     - ,      'tax_sales_sel'             # sum of all selective sales tax (15, 5, 7, 9, 11-13)      
#'     1 ,      'tax_property'
#'     
#'     (not included):
#'     15,      'tax_sales_sel_other',
#'     5 ,      'tax_sales_sel_alc',
#'     7 ,      'tax_sales_sel_amuse',
#'     9 ,      'tax_sales_sel_fuels',
#'     11,      'tax_sales_sel_pm',         # pari-mutuels
#'     12,      'tax_sales_sel_utilities',
#'     13,      'tax_sales_sel_tobacco'



#**********************************************************************
#                   2.5 Saving Data                               ####
#**********************************************************************

ls_govRev <- list(
  df_govRev_y = df_govRev_y,
  df_govRev_q = df_govRev_q
)

saveRDS(ls_govRev,
         file = paste0(dir_data_out, "dataProc_govRev.rds"))


#**********************************************************************
#                3 GSP data                       ####
#**********************************************************************
# TODO:
#  1. Check details of coincident index: 
#       - seasonality, timing. 


# Goal:
#  1. Annual real GSP growth for all states: use BEA growth directly
#  2. quarterly GSP level index:
#       - Annual growth follows BEA RGSP annual growth 
#       - adjusting quarterly growth follows coincident index
#       - construct quarterly GSP (index)




## Key questions about the validation of the interpolation method. 

# 1. How do US aggregate of BEA RGSP compare to the national GDP? This will determine
#   whether it is appropriate to use the RGSP as the trend in the construction of quarterly state GSP growth

# 2. If applying the interpolation method to the national data, can the BEA data 
#   reproduce (approximately) the offical quarterly GDP growth? 

# 3. Are the coincident index seasonal? Any seasonal adjustment needed?



#**********************************************************************
#                3.1  loading data                       ####
#**********************************************************************

load(paste0(dir_data_raw, "dataRaw_GSP_BEA.RData"))

# df_GSP_FRED
# df_RGSP_BEA
# df_NGSP_BEA


#**********************************************************************
#                3.2  Annual GSP data                              ####
#**********************************************************************

#  GSP variables
#     - RGSP_SIC:   From BEA,  1963-1997, based on SIC
#     - RGSP_NAICS: From BEA,  1997-2016, based on NAICS


df_GSP_y <-
  full_join(ls_GSP_BEA_raw$df_RGSP_BEA,
            ls_GSP_BEA_raw$df_NGSP_BEA,
            by = c("state", "state_abb", "year")
            )

# Annual GDP growth rate, combining SIC and NAICS series 
df_GSP_y %<>% 
  mutate(RGSP_SIC_growth   =  RGSP_SIC/lag(RGSP_SIC) - 1,
         RGSP_NAICS_growth =  RGSP_NAICS/lag(RGSP_NAICS) - 1,
         RGSP_growth = ifelse(year <1998, RGSP_SIC_growth, RGSP_NAICS_growth),
         
         NGSP_SIC_growth   =  NGSP_SIC/lag(NGSP_SIC) - 1,
         NGSP_NAICS_growth =  NGSP_NAICS/lag(NGSP_NAICS) - 1,
         NGSP_growth = ifelse(year <1998, NGSP_SIC_growth, NGSP_NAICS_growth)
         ) %>% 
  select(-RGSP_SIC_growth, -RGSP_NAICS_growth, -NGSP_SIC_growth, -NGSP_NAICS_growth)



#**********************************************************************
#         3.3  Quarterly GSP data: interpolation            ####
#**********************************************************************

## 1. Adjustment factor for coincident index (1979~)
# adj_fct = [(1+annual GSP growth)/(1+annual index growth)]^(1/4)
# For now annual growth of C-index calculated using annual averages

# c-index and growth 1977~2019
cIdx_annual_avg <- 
  ls_GSP_BEA_raw$df_coincIdx %>% 
  group_by(state_abb, year) %>% 
  summarise(cIdx = mean(value), .groups = "drop") %>% 
  mutate(cIdx_growth = cIdx/lag(cIdx) - 1)

# merge annual growth of c-index with annual growth of real gsp 1988-2016
# then calculate adjustment factor
df_adjFactor <- 
  left_join(df_GSP_y, 
            cIdx_annual_avg,
            by = c("state_abb", "year")) %>% 
  mutate(adj_fct = ((1 + RGSP_growth) / (1 + cIdx_growth))^(1/4)  ) %>% 
  select(state_abb, year, adj_fct)
#df_adjFactor



## 2. Adjusting index grwoth
# - The original c-index data are monthly. 
# - Converting to quarterly data by taking quarterly average
# - Then apply adjustment factor constructed above

cIdx_qtr <- 
  ls_GSP_BEA_raw$df_coincIdx %>% 
  left_join(df_monthQtr, by = "month") %>% 
  group_by(state_abb, year, qtr) %>% 
  summarise(cIdx = mean(value)) %>% 
  ungroup %>% 
  mutate(cIdx_growth = cIdx / lag(cIdx) - 1)

cIdx_qtr %<>% 
  left_join(select(df_adjFactor, state_abb, year, adj_fct), by = c("state_abb", "year")) %>% 
  mutate(cIdx_growth_adj = cIdx_growth * adj_fct)


## 3. Constructing RGSP index based on adjusted c-index
 # Use year 1988 as the base year and set the value to 100 (the same for all states)
 # Apply the adjusted c-index growth to construct the RGSP index for all states
 # Then we can calculate quarterly year-on-year growth of RGSP

df_GSP_q <- 
  cIdx_qtr %>% 
  filter(year >= 1988) %>% 
  group_by(state_abb) %>% 
  mutate(yearQtr = year + (qtr - 1)/4) %>% 
  arrange(state_abb, yearQtr) %>%  # just for safety
  mutate(RGSP_idx = 100 * cumprod(1 + ifelse(yearQtr == min(yearQtr), 0, cIdx_growth_adj))) %>% 
  select(state_abb, year, qtr, cIdx, cIdx_growth, adj_fct, cIdx_growth_adj, RGSP_idx)


## Notes on variables
# cIdx          Coincident index, level, (quarterly avg of monthly values) 
# cIdx_growth   qtr-on-qtr growth rates of cIdx
# adj_fct       adjustment factors for cIdx_growth
# 
# cIdx_growth_adj:  Adjusted qtr-on-qtr growth rates of cIdx
# RGSP_idx:         Quarterly RGSP constructed based on cIdx_growth_adj, assuming 1988 value = 100


#**********************************************************************
#         3.4 saving data            ####
#**********************************************************************
ls_GSP <- 
  list(
    df_GSP_y = df_GSP_y,
    df_GSP_q = df_GSP_q
  )


saveRDS(ls_GSP,
        file = paste0(dir_data_out, "dataProc_GSP.rds"))




