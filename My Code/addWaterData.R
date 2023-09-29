require("lubridate")
require("dplyr")

# FUNCTION TO GET TRAILING DATA
#
# inputs:
# water quality dataframe
# the R function you want to apply
# data column you will use
# end date (assessment date)
# number of trailing days to apply the function over

# output:
# numerical output after applying function to the data column over the selected timespan

getTrailingData = function(dataframe, func, data, endDate, trailingDays) {
  # select only the times between assessment and assessment - trailingDays
  df <- subset(dataframe, as.Date(Date) <= as.Date(endDate) 
               & as.Date(Date) > as.Date(endDate) - trailingDays)
  
  # only consider rows in this timeframe with complete data
  df <- df[complete.cases(df), ]
  
  # apply the function to the data column
  return(func(df[,data]))
}

# read the csv file with banana river data
bananadata <- read.csv('bananaallfull.csv')
bananadata <- bananadata[,-c(1,11)]


# fix the first column name
colnames(bananadata)[1] <- 'Date'

# convert the date to a new format
bananadata$Date = parse_date_time(bananadata$Date, orders = 'ymdHMS')

# read the csv file with mat data
matdata <- read.csv('Shannon Wiener Merged All Data.csv')
#View(matdata)

# create a list of all locations, types of data, functions to apply, and
# durations over which we consider water data
locations = list('banana')
data = list('pH_ScientificUnits', 'Salinity_PartsPerThousand',
            'WaterTemp_Celcius', 'Turbidity_FNU', 'DissOxy_MilligramPerLitre')
functions = list('mean', 'min', 'max')
durations = list(1, 7, 14, 36, 50, 75, 100)

# create a df with every combination of location, data, function, and duration
df <- expand.grid(x = locations, y = data, z = functions, w = durations)

# iterate through mat data rows
for (row in 1:nrow(matdata)) {
  # get the date in the current row and convert to date_time type
  assessmentDate = parse_date_time(matdata[row, "Date"], orders = "m/d/y")
  
  # if the date didn't change, simply copy the previous data
  if (row > 1 && assessmentDate == oldAssessmentDate) {
    print('Reusing data from the previous row')
    matdata[row, 25:ncol(matdata)] = matdata[row - 1, 25:ncol(matdata)]
    
  # else, compute the values to fill in the row
  } else {
    print('Computing new data')
    for (i in 1:nrow(df)) {
      # write the name of the column to be filled in for the current row
      column = paste(df[i,]$x, df[i,]$y, df[i,]$z, df[i,]$w, 'days', sep = '-')
      
      # convert function name to an actual function
      func = match.fun(df[i,]$z[[1]])
      
      # fill in the row and column value
      matdata[row, column] = getTrailingData(bananadata, func, df[i,]$y[[1]],
                                             assessmentDate, df[i,]$w[[1]])
    }
  }
  
  # print a status update...
  print(paste('Processing row', row, 'of', nrow(matdata)))
  
  # save the previous assessment date
  oldAssessmentDate = assessmentDate
}

matdata3 <- matdata

getwd()
View(matdata)
write.csv(matdata3,"C:/Users/alyss/OneDrive/Documents/matdata3.csv")
