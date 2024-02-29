library(tidyverse)
library(tidytext)
library(readxl)
library(e1071)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(plm)
library(robustbase)
library(DescTools)
library(dplyr)



setwd("C:\\Users\\endur\\Desktop\\laurea magistrale\\Financial\\First_Assignment")

DB <- read_excel("DB2_a.xlsx")

DB <- DB %>% group_by(Name) %>% 
  mutate(LEVERAGE_NATLOG_LAG = dplyr::lag(LEVERAGE_NATLOG))

DB <- DB %>% group_by(Name) %>% 
  mutate(MRKT_VALUE_BOOK_RATIO_LAG = dplyr::lag(`MRKT_VALUE_TO_BOOK`))

lower_threshold <- 0.1
upper_threshold <- 0.90

columns_to_wins = c("PRICE_OR_TRADE","TOTAL_ASSETS","TOT_NATLOG","TOT_GRWTRATE","MRKT_VALUE_TO_BOOK","MARKET_VALUE",
                    "MRKT_VALUE_NATLOG","MKT_VALUE_GRWTRATE","TOT_ASSETS_CMN_EQUITY_RATIO","LEVERAGE_NATLOG","LEVERAGE_GRWTRATE",
                    "LEVERAGE_NATLOG_LAG","MRKT_VALUE_BOOK_RATIO_LAG")

DB[columns_to_wins] <- lapply(DB[columns_to_wins], function(x) Winsorize(x, probs = c(lower_threshold, upper_threshold), na.rm = TRUE))




MOD1 <- plm(LEVERAGE_GRWTRATE ~ TOT_GRWTRATE + LEVERAGE_NATLOG_LAG + Entity_institute+Entity_real+ Y2005 + Y2006 + 
              Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + Y2014 + Y2015 + 
              Y2016 + Y2017 + Y2018 + Y2019 + Y2020 + Y2021 + Y2022, 
            data = DB, model = "within")
summary(MOD1)



MOD2 =plm(LEVERAGE_GRWTRATE ~ TOT_GRWTRATE + MRKT_VALUE_BOOK_RATIO_LAG + LEVERAGE_NATLOG_LAG+ Entity_institute+Entity_real+
            Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + Y2014 + Y2015 + 
            Y2016 + Y2017 + Y2018 + Y2019 + Y2020 + Y2021 + Y2022, 
          data = DB, model = "within")
summary(MOD2)

MOD3 = plm(LEVERAGE_GRWTRATE ~ MKT_VALUE_GRWTRATE + LEVERAGE_NATLOG_LAG + Entity_institute+Entity_real+Y2005 + Y2006+
              Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + Y2014 + Y2015 + 
             Y2016 + Y2017 + Y2018 + Y2019 + Y2020 + Y2021 + Y2022, 
           data = DB, model = "within")
summary(MOD3)

MOD4 = plm(LEVERAGE_GRWTRATE ~ MKT_VALUE_GRWTRATE + MRKT_VALUE_BOOK_RATIO_LAG +LEVERAGE_NATLOG_LAG+Entity_institute+Entity_real+Y2005 + Y2006 + 
             Y2007 + Y2008 + Y2009 + Y2010 + Y2011 + Y2012 + Y2013 + Y2014 + Y2015 + 
             Y2016 + Y2017 + Y2018 + Y2019 + Y2020 + Y2021 + Y2022, 
           data = DB, model = "within")
summary(MOD4)


