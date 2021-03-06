# Function to clean the housing data feature values and set correct data types
# for each feature
CleanData <- function(housingData) {
  # Unit type is one of a few predefined values so make it a factor
  housingData$UnitType <- as.factor(housingData$UnitType)
  
  # Number of beds and baths can be numeric (baths can be fractional)
  housingData$BR <- as.numeric(housingData$BR)
  housingData$BA <- as.numeric(housingData$BA)
  
  # Sq. footage is numeric too, need to strip the commas.
  housingData$SQFT <- as.numeric(gsub(",","", housingData$SQFT))
  
  # Asking and Sales Prices need to be cleaned (commas and dollar signs stripped) and converted to numerics
  housingData$AskingPrice <- as.numeric(gsub(",","", (gsub("\\$","", housingData$AskingPrice))))
  if("SalePrice" %in% colnames(housingData))
  {
    housingData$SalePrice <- as.numeric(gsub(",","", (gsub("\\$","", housingData$SalePrice))))
  }
  
  # Closing date can be converted to number of days since the beginning of time, let's use earliest closing date.
  beginningOfTime <- as.Date("02/28/07", "%m/%d/%y")
  housingData$ClosingDate <- as.numeric(as.Date(housingData$ClosingDate, "%m/%d/%y") - beginningOfTime)
  
  # Days on market needs to be cleaned ("N/A" values set to 0, need to think about this..) and converted to numerics
  housingData$DaysOnMarket <- as.numeric(gsub("N/A","0", housingData$DaysOnMarket))

  return(housingData)
}

housingData <- read.table(file="peterCouttsTrainingData.txt", header = TRUE)
cleanedData <- CleanData(housingData);

pricePredictionLm <- lm(SalePrice ~ UnitType + BR + BA + SQFT + AskingPrice + ClosingDate + DaysOnMarket, cleanedData)
# Test data
testData <- read.table(file="peterCouttsTestData.txt", header = TRUE)
cleanedTestData <- CleanData(testData)

predictedPrice <- predict(pricePredictionLm, cleanedTestData)
