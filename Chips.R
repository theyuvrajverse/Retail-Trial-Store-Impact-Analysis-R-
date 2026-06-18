install.packages("tidyr")

library(data.table)
library(ggplot2)
library(tidyr)

filePath <- "C:/Users/asus/Downloads/QVI_data.csv"
data <- fread(filePath)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# measures over time for each store,Add a new month ID column in the data with the format yyyymm.
data[, YEARMONTH := as.numeric(format(as.Date(DATE, "%d-%m-%Y"), "%Y%m"))]
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY))
                        , by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#Filter to Stores with Full Observations
##Find stores that have data for all 12 months
storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])

#CREATE preTrialMeasures
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]

#createcorrelation function
calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  storeNumbers <- unique(inputTable[STORE_NBR != storeComparison, STORE_NBR])
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "corr_measure" = cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)],
                                                        inputTable[STORE_NBR == i, eval(metricCol)]))
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
  }
  return(calcCorrTable)
}

#Creating a function to calculate a standardised magnitude distance for a measure,looping through each control store.
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH],
                                   "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)]
                                                   - inputTable[STORE_NBR == i, eval(metricCol)]))
    
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  #Standardise the magnitude distance so that the measure ranges from 0 to 1
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), 
                              by = c("Store1", "YEARMONTH")]
  
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  
  return(finalDistTable)
}

#function which created to calculate correlations against store 77 using total sales and number of customers.
##Set trial store number
trial_store <- 77
##Calculate correlation for total sales
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
##Calculate correlation for number of customers
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

#used function to create magnitude
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

#combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))[, scoreNSales := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))[, scoreNCust := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

# merging our sales scores and customer scores into a single table and store with the highest score is then selected as the control store.
# Select control stores based on the highest matching store
score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

control_store <- score_Control[Store1 == trial_store, ][order(-finalControlScore)][2, Store2]

#check visually if the drivers areindeed similar in the period before the trial.
##We'll look at total sales first.
##Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                         "Trial",
                                                         ifelse(STORE_NBR == control_store,
                                                                "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH",
                                       "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                        100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#customer count trends by comparing the trial store to the control store and other stores
##number of customers.
pastCustomers <- measureOverTime[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                        ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, nCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]
##Plot customer count trends
ggplot(pastCustomers, aes(TransactionMonth, nCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", 
       y = "Number of customers", 
       title = "Number of customers by month")

#Assesment of trial#
##Comparison of results during trial
#Scale pre-trial control sales to match pre-trial trial store sales 
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
                                                   YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
                                                                                                         YEARMONTH < 201902, sum(totSales)]
# Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
                                                                          controlSales := totSales * scalingFactorForControlSales]
#Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
                                                                          controlSales := totSales * scalingFactorForControlSales]
#percentage difference between scaled control salesand trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales)/controlSales]

stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])#let's take the standard deviation based on the scaled percentage difference in the pre-trial period.


##Note that there are 8 months in the pre-trial period
##hence 8 - 1 = 7 degrees of freedom
degreesOfFreedom <- 7 

#t-values for the trial months,the 95th percentile
percentageDiff[, tValue := (percentageDiff - 0)/stdDev
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][, .(TransactionMonth, tValue, YEARMONTH)]
qt(0.95, df = degreesOfFreedom)# 95th percentile

#Create store type variables for plotting
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

# Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence
interval"]

# Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

# Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

#Calculate scaling factor
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & 
                                                  YEARMONTH < 201903, 
                                                sum(nCustomers)] /
  preTrialMeasures[STORE_NBR == control_store & 
                     YEARMONTH < 201903, 
                   sum(nCustomers)]
#Create scaled control customers data
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store,
][, controlCustomers := nCustomers * scalingFactorForControlCust
][, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
]
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7

##Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by =
                                        c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]
## Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence
interval"]
## Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
interval"]
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95,
                         pastCustomers_Controls5)
#creating graph
ggplot() +
  geom_rect(data = , aes(xmin = , xmax = , ymin = , ymax = , color = ),
            show.legend = FALSE) +
  geom_line() +
  labs() 

# Ensure data.table
setDT(preTrialMeasures)
setDT(measureOverTime)

# --- Step 0: Label store types on the FULL dataset ---
measureOverTime[, Store_type := fifelse(
  STORE_NBR == trial_store,   "Trial",
  fifelse(STORE_NBR == control_store, "Control", "Other")
)]

# --- Step 1: Scaling factor (pre-trial) ---
scalingFactorForControlCust <- 
  preTrialMeasures[STORE_NBR == trial_store  & YEARMONTH < 201903, sum(nCustomers)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201903, sum(nCustomers)]

# --- Step 2: Trial series (mean customers by month) ---
trial_ts <- measureOverTime[STORE_NBR == trial_store,
                            .(YEARMONTH, nCusts = mean(nCustomers)), by = YEARMONTH
][, Store_type := "Trial"]

# --- Step 3: Scaled Control series (mean * scaling factor) ---
control_ts <- measureOverTime[STORE_NBR == control_store,
                              .(YEARMONTH, nCusts = mean(nCustomers) * scalingFactorForControlCust), by = YEARMONTH
][, Store_type := "Control (scaled)"]

# --- Step 4: Combine with consistent columns ---
customerData <- rbind(trial_ts[, .(YEARMONTH, Store_type, nCusts)],
                      control_ts[, .(YEARMONTH, Store_type, nCusts)],
                      use.names = TRUE)

# --- Step 5: Transaction month ---
customerData[, TransactionMonth := as.Date(sprintf("%04d-%02d-01",
                                                   YEARMONTH %/% 100, YEARMONTH %% 100))]

# --- Step 6: Plot ---
ggplot(customerData, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(aes(xmin = as.Date("2019-03-01"),
                xmax = as.Date("2019-06-01"),
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey", alpha = 0.3, color = NA) +
  geom_line(aes(linetype = Store_type), linewidth = 0.8) +
  labs(x = "Month of operation",
       y = "Total number of customers",
       title = "Total number of customers by month") +
  theme_minimal()


# TRIAL STORE 86 - COMPLETE ANALYSIS
# =============================================================================

# Ensure data is data.table
setDT(data)

# Recalculate measures over time
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY))
                        , by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

# Set trial store
trial_store <- 86

# Calculate correlations
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

# Calculate magnitudes
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

# Create combined scores
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2")
)[, scoreNSales := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2")
)[, scoreNCust := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

# Combine scores and select control store
score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

control_store <- score_Control[Store1 == trial_store,
][order(-finalControlScore)][2, Store2]

print(paste("Control store for trial store 86:", control_store))

# Visual check - Total Sales
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month - Store 86 Pre-Trial")

# Visual check - Customer Count
measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                             ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of customers", title = "Number of customers by month - Store 86 Pre-Trial")

# Assessment of Trial - Sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, 
][, controlSales := totSales * scalingFactorForControlSales]

# Calculate percentage difference
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales)/controlSales]

# Calculate standard deviation
stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])
degreesOfFreedom <- 7

# Create sales comparison data
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

# Calculate confidence intervals
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

# Plot sales assessment
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE, fill = "grey", alpha = 0.3) +
  geom_line(aes(linetype = Store_type)) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month - Store 86 Trial Period")

# Assessment of Trial - Customers
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store,
][, controlCustomers := nCustomers * scalingFactorForControlCust
][, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))]

# Calculate percentage difference for customers
percentageDiff_cust <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                             measureOverTime[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")],
                             by = "YEARMONTH")[, percentageDiff := abs(controlCustomers - nCustomers)/controlCustomers]

stdDev_cust <- sd(percentageDiff_cust[YEARMONTH < 201902, percentageDiff])

# Create customer comparison data
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

# Calculate confidence intervals
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev_cust * 2)
][, Store_type := "Control 95th % confidence interval"]

pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev_cust * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment_cust <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

# Plot customer assessment
ggplot(trialAssessment_cust, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment_cust[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE, fill = "grey", alpha = 0.3) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", 
       title = "Total number of customers by month - Store 86 Trial Period")


# =============================================================================
# TRIAL STORE 88 - COMPLETE ANALYSIS
# =============================================================================


# Ensure data is data.table
setDT(data)

# Recalculate measures
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY))
                        , by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

# Set trial store
trial_store <- 88

# Calculate correlations
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

# Calculate magnitudes
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

# Create combined scores
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2")
)[, scoreNSales := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2")
)[, scoreNCust := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

# Combine scores and select control store
score_Control <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

control_store <- score_Control[Store1 == trial_store,
][order(-finalControlScore)][2, Store2]

print(paste("Control store for trial store 88:", control_store))

# Visual checks - Total Sales
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month - Store 88 Pre-Trial")

# Visual checks - Customer Count
measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                             ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][YEARMONTH < 201903, ]

ggplot(pastCustomers, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_line() +
  labs(x = "Month of operation", y = "Number of customers", title = "Number of customers by month - Store 88 Pre-Trial")

# Assessment of Trial - Sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, 
][, controlSales := totSales * scalingFactorForControlSales]

# Calculate percentage difference
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store, c("totSales", "YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff := abs(controlSales - totSales)/controlSales]

# Calculate standard deviation
stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])
degreesOfFreedom <- 7

# Create sales comparison data
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

# Calculate confidence intervals
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]

pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

# Plot sales assessment
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE, fill = "grey", alpha = 0.3) +
  geom_line() +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month - Store 88 Trial Period")

# Assessment of Trial - Customers
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)] /
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(nCustomers)]

measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store,
][, controlCustomers := nCustomers * scalingFactorForControlCust
][, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))]

# Calculate percentage difference for customers
percentageDiff_cust <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                             measureOverTime[STORE_NBR == trial_store, c("nCustomers", "YEARMONTH")],
                             by = "YEARMONTH")[, percentageDiff := abs(controlCustomers - nCustomers)/controlCustomers]

stdDev_cust <- sd(percentageDiff_cust[YEARMONTH < 201902, percentageDiff])

# Create customer comparison data
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by = c("YEARMONTH", "Store_type")
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
][Store_type %in% c("Trial", "Control"), ]

# Calculate confidence intervals
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev_cust * 2)
][, Store_type := "Control 95th % confidence interval"]

pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev_cust * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessment_cust <- rbind(pastCustomers, pastCustomers_Controls95, pastCustomers_Controls5)

# Plot customer assessment
ggplot(trialAssessment_cust, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment_cust[YEARMONTH < 201905 & YEARMONTH > 201901,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), 
                ymin = 0, ymax = Inf, color = NULL), 
            show.legend = FALSE, fill = "grey", alpha = 0.3) +
  geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", 
       title = "Total number of customers by month - Store 88 Trial Period")





