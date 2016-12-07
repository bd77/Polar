# ----------------------- #
# Process Polar M400 data #
# ----------------------- #

library(XML)

setwd('D:/Other/Polar')
#setwd(dir = "C:/Documenten/Statistiek/Polar")

csv.filename <- 'bart_degraeuwe_2016-09-28_12-24-44.csv'
csv.file.list <- list.files(pattern = ".csv$")

# initialize data frame for the general training data in the header of the csv file
df.csv.general <- data.frame()
# initialize data frame for the second-by-second data in the header of the csv file
df.training <- data.frame()
id <- 1

for (csv.filename in csv.file.list) {
  
  # csv files lezen, deel 1, algemene data
  df.csv.header <- read.csv(file = csv.filename, header = TRUE, nrows = 1)
  # format the date field
  df.csv.header$Date <- as.Date(df.csv.header$Date, format = "%d-%m-%Y")
  # add the id
  df.csv.header <- cbind(id = id, df.csv.header)
  # add the data of the training to the others
  df.csv.general <- rbind(df.csv.general, df.csv.header)
  # create date-time varible of the start time
  datetime.start <- as.POSIXct(paste(df.csv.header$Date, df.csv.header$Start.time), 
                               format = "%Y-%m-%d %H:%M:%S")
  
  # read the second-by-second data of the csv-file
  df.csv.data <- read.csv(file = csv.filename, header = TRUE, skip = 2)
  # create a date time vector for csv data
  datetimes.csv <- datetime.start + 0:(NROW(df.csv.data) - 1)
  df.csv.data <- cbind(df.csv.data, datetime = datetimes.csv)
  
  # df.csv.sbys <- rbind(df.csv.sbys, df.csv.data)
  
  # read gpx file
  # -------------
  gpx.filename <- sub(".csv", ".gpx", csv.filename)
  gpxfile <- htmlTreeParse(paste0(gpx.filename), useInternalNodes = TRUE)
  
  # Get all elevations, times and coordinates via the respective xpath
  times.str <- xpathSApply(gpxfile, path = '//trkpt/time', xmlValue)
  # format as date time and convert gpx from UTC to CET
  datetimes.gpx <- as.POSIXct(substr(times.str,1,19), format="%Y-%m-%dT%H:%M:%S", tz = "UTC") 
  attributes(datetimes.gpx)$tzone <- "CET"
  elevations <- as.numeric(xpathSApply(gpxfile, path = '//trkpt/ele', xmlValue))
  coords <- xpathSApply(gpxfile, path = '//trkpt', xmlAttrs)
  # Extract latitude and longitude from the coordinates
  lats <- as.numeric(coords['lat',])
  lons <- as.numeric(coords['lon',])
  
  df.gpx.data <- data.frame(datetime = datetimes.gpx,
                            elevation = elevations,
                            lon = lons, lat = lats)
  df.csv.gpx <- merge(df.csv.data, df.gpx.data, by = c('datetime'), 
                      all.x = TRUE, all.y = TRUE)
  df.csv.gpx <- cbind(id = id, df.csv.gpx)  
  
  # add df.csv.gpx 
  df.training <- rbind(df.training, df.csv.gpx)
  print(id)
  id <- id + 1
  # create plots of each training
  
}

# store the general and second-by-second training data
write.csv(df.training, file = 'trainingdata.txt')