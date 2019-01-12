# INSTALL AND LOAD PACKAGES ####################################################
packs <- c("cffdrs", "leaflet", "ggplot2", "dplyr", "raster", "rgeos",
           "sp", "lubridate", "httr", "rgdal", "ggmap", "gridExtra", "viridis",
           "lutz", "xtable")
new.packages <- packs[!(packs %in% installed.packages()[,'Package'])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = TRUE)
rm(packs, new.packages)

#### ALGORITHM VALIDATION ######################################################

rm(list = ls())

# Compare CFFDRS and GEFF algorithms
y <- structure(list(long = -91.82, lat = 45.98, hour = 18,
                    yr = 2017L, mon = 1L, day = 1L, temp = 17,
                    rh = 42, ws = 25, prec = 0),
               .Names = c("long", "lat", "hour", "yr", "mon", "day", "temp",
                          "rh", "ws", "prec"),
               row.names = 1L, class = "data.frame")
init <- data.frame(ffmc = 85, dmc = 6, dc = 15, lat = y$lat)
round(cffdrs::fwi(input = y, init = init, out = "fwi"), 2)
# ALGORITHM        & FFMC  & DMC  & DC    & ISI   & BUI  & FWI  & DSR\\
# R package cffdrs & 87.69 & 7.29 & 17.76 & 10.85 & 7.28 & 9.46 & 1.45\\

# GEFF - manual run of GEFF (Fortran code) from the above initial conditions
# Here are the results:
# ALGORITHM  & FFMC  & DMC  & DC    & ISI   & BUI  & FWI   & DSR\\
# ECMWF-GEFF & 87.70 & 8.54 & 19.01 & 10.80 & 8.49 & 10.10 & 0.0272*10.10^.77\\

# GET TIMEZONE DATA ############################################################

# Get time zone vector data:
# https://en.wikipedia.org/wiki/List_of_UTC_time_offsets

# Data is available from Natural Earth
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/timezones/

# Download, unzip and save shapefile in the folder data/ne_10m_time_zones

# GET DATA FROM SYNOP STATIONS #################################################
# The code in this section can only run within ECMWF internal network because it
# uses internal resources and tools (stvl)

rm(list = ls())

# Set suitable dates
myDates <- seq.Date(from = as.Date("2017-01-01"), to = as.Date("2017-12-31"),
                    by = "day")
myParams <- c("tp", "2t", "10dd", "10ff", "2d")
log_error <- NULL
# Loop over the hours
for (myHour in seq(0, 23, 1)){
  df <- matrix(NA, nrow = 0, ncol = 8)
  # Loop over the parameters
  for (j in seq_along(myParams)){
    myParam <- myParams[j]
    for (i in seq_along(myDates)){
      print(paste0(myHour, "UTC ", myParam, " ", myDates[[i]]))

      # Define request parameters
      params <- list(table = "observation",
                     dataset = "synop",
                     year = year(myDates[[i]]),
                     month = month(myDates[[i]]),
                     day = day(myDates[[i]]),
                     hour = myHour,
                     parameter = myParam,
                     get_data = "ascii")

      # Total precipitation should be accumulated over period = 24h*3600s
      if (myParam == "tp"){params[["period"]] <- 86400}

      # Make POST request
      y <- httr::POST("http://ecverify.ecmwf.int/cgi-bin/stvl/stvl_browse.py",
                      body = params)

      # Extract table (if available)
      x <- try(expr = read.table(text = content(y, "text", encoding = "utf-8"),
                                 sep = " "),
               silent = TRUE)
      if (class(x) != "try-error"){
        x$time <- myDates[[i]]
        x$param <- myParam
        x$hour <- myHour
        df <- rbind(df, x)
      }else{
        print("Data unavailable")
        log_error <- c(log_error,
                       paste0(myHour, "UTC ", myParam, " ", myDates[[i]]))
      }
    }
  }
  saveRDS(df, paste0("data/synop/tempdf_", myHour, ".rds"))
}

# Check log_error before proceeding.

rm(list = ls())

# Add local time to the attributes, using lutz::tz_lookup_coords().

for (myHour in 0:23){

  print(paste(myHour, "UTC"))

  # Read data.frame
  df <- readRDS(paste0("data/synop/tempdf_", myHour, ".rds"))
  names(df) <- c("lat", "long", "id", "z", "value", "date", "param", "UTC_time")
  # Remove the column with z coordinate and re-arrange
  df <- df[, c(3, 1, 2, 6, 8, 7, 5)]

  # Collate info on synop stations
  temp <- unique(df[, c("id", "lat", "long")])
  if (exists("stations_all")){
    stations_all <- rbind(stations_all, temp)
    stations_all <- unique(stations_all)
  }else{
    stations_all <- temp
  }

  # Convert from long to wide format
  spreadDF <- tidyr::spread(data = df, param, value); rm(df, temp)

  if (all(c("10dd", "10ff", "2d", "2t", "tp") %in% names(spreadDF))) {
    # Remove rows with incomplete records
    df <- spreadDF %>% filter(complete.cases(`10dd`, `10ff`, `2d`, `2t`, `tp`))
    rm(spreadDF)

    # Add time zone
    df$tzid <- lutz::tz_lookup_coords(lat = df$lat, lon = df$long,
                                      method = "fast") # accurate

    # Remove records with NA tzid or timezone = "uninhabited"
    temp <- which(is.na(df$tzid) | df$tzid == "uninhabited")
    if (length(temp) > 0) {
      df <- df[-temp, ]
    }

    # UTC time as needed by with_tz()
    df$timestamp_utc <- as.POSIXct(paste0(as.character(df$date), " ",
                                          myHour, ":00"),
                                   format = "%Y-%m-%d %H:%M", tz = "UTC")

    df <- df %>% group_by(tzid) %>%
      mutate(local_time = hour(with_tz(timestamp_utc, tzid)))

    # Remove records with NA local time
    temp <- which(is.na(df$local_time))
    if (length(temp) > 0) {
      df <- df[-temp, ]
    }

    # Keep only records corresponding to 11-13
    # This is to capture noon local time even witSupplementary informationh Daylight Saving Time
    temp <- which(df$local_time %in% 11:13)
    if (length(temp) > 0) {
      df <- df[temp, ]

      # Calculate relative humidity
      df$`2d` <- df$`2d` - 273.15
      df$`2t` <- df$`2t` - 273.15
      df$rh <- (6.11 * 10 ^ (7.5 * (df$`2d`) / (237.7 + (df$`2d`)))) /
        (6.11 * 10 ^ (7.5 * (df$`2t`) / (237.7 + (df$`2t`)))) * 100

      # Convert to match cffdrs::fwi() requirements
      df$ws <- df$`10ff` * 3.6
      df$yr <- lubridate::year(df$date)
      df$mon <- lubridate::month(df$date)
      df$day <- lubridate::day(df$date)

      # Add info about hemisphere and season
      df$hemisphere <- ifelse(df$lat >= 0, "North", "South")
      df$season <- NA
      # North Hemisphere
      df$season[which(df$lat >= 0 & between(df$mon, 4, 9))] <- "Dry"
      df$season[which(df$lat >= 0 &
                        (between(df$mon, 1, 3) |
                           between(df$mon, 10, 12)))] <- "Wet"
      # South Hemisphere
      df$season[which(df$lat < 0 & between(df$mon, 4, 9))] <- "Wet"
      df$season[which(df$lat < 0 &
                        (between(df$mon, 1, 3) |
                           between(df$mon, 10, 12)))] <- "Dry"

      if (exists("df_all")){
        df_all <- rbind(df_all, df)
      }else{
        df_all <- df
      }

      print(paste("records:", dim(df_all)[1],
                  "- stations:", dim(stations_all)[1]))

    } else {
      message("No data at local noon")
    }

  }

}

# Save all the stations as they are
saveRDS(stations_all, "data/stations_all.rds")
saveRDS(df_all, "data/df_all.rds")

# AVERAGE OVER THE 11-13 TIME WINDOW ###########################################
# (datasets used hereafter are available within the repo) ######################

rm(list = ls())
df_all <- readRDS("data/df_all.rds")

df <- df_all %>%
  group_by(id, lat, long, tzid, season, yr, mon, day) %>%
  summarise(temp = last(`2t`), prec = last(tp), rh = last(rh), ws = last(ws))
saveRDS(df, "data/df_12noon.rds")

# ANALYSE STATION DATA #########################################################
# (datasets used hereafter are available within the repo) ######################

rm(list = ls())

# How many synop stations recorded data in 2017?
stations_all <- readRDS("data/stations_all.rds") # 12777
# Some stations have slightly inconsistent coordinates
# We group by id (assuming it is unique) and keep only the most frequent coords!
stations_all <- stations_all %>% group_by(id) %>%
  summarize(lat = as.numeric(names(which.max(table(lat)))),
            long = as.numeric(names(which.max(table(long)))))
# There are 10080 stations, in the text we mention ~10,000
saveRDS(stations_all, "data/stations_all_unique.rds")

# How many SYNOP stations recorded all the necessary variables around local noon?
df <- readRDS("data/df_12noon.rds")
stations_12noon <- df %>% group_by(id) %>%
  summarize(lat = as.numeric(names(which.max(table(lat)))),
            long = as.numeric(names(which.max(table(long))))) # 3803 stations
saveRDS(stations_12noon, "data/stations_12noon_unique.rds")

# In the next step we will filter out the stations that have less than 30 days
# of recordings and groups of less than 3 stations in a single time zone.

### FWI OBSERVED VS MODELLED ###################################################

rm(list = ls())

# FWI from reanalysis
# Get fwi.nc from Zenodo: https://doi.org/10.5281/zenodo.1406194
# Extract 2017 and save it in the file "data/fwi2017re.nc"
fwi2017 <-  raster::brick("data/fwi2017re.nc")
names(fwi2017) <- seq.Date(from = as.Date("2017-01-01"),
                           to = as.Date("2017-12-31"),
                           by = "day")

# Load unique stations and data
stations <- readRDS("data/stations_12noon_unique.rds")
df <- readRDS("data/df_12noon.rds")

df_used <- data.frame(matrix(NA, nrow = 0, ncol = ncol(df) + 2))
names(df_used) <- c(names(df), "fwiobs", "fwimod")
for (i in seq_along(stations$id)){

  # We filter over the station
  dfx <- df %>% filter(id == stations$id[i])

  dim_dfx <- dim(dfx)[1]

  if (dim_dfx >= 30){

    if (dim_dfx > 366) message("Caution: two different stations were merged!")

    # Calculate the observed FWI
    common_lat <- as.numeric(names(sort(table(dfx$lat),decreasing = TRUE))[1])
    common_long <- as.numeric(names(sort(table(dfx$long),decreasing = TRUE))[1])
    pt <- data.frame(long = common_long, lat = common_lat)
    initial_condition <- data.frame(ffmc = 85, dmc = 6, dc = 15,
                                    lat = common_lat)
    dfx$fwiobs <- cffdrs::fwi(input = dfx,
                              init = initial_condition,
                              out = "fwi")$FWI

    # Extract the modelled FWI from reanalysis
    temp <- t(raster::extract(x = fwi2017, y = SpatialPoints(pt)))
    mytimestamps <- which(substr(names(fwi2017), 2, 11) %in%
                            paste0(dfx$yr, ".", sprintf("%02d", dfx$mon), ".",
                                   sprintf("%02d", dfx$day)))
    dfx$fwimod <- temp[mytimestamps]

    df_used <- dplyr::bind_rows(df_used, dfx)
    rm(mytimestamps, temp, initial_condition, dfx)
  }else{
    print(paste("Station n.", i, "discarded because contains < 30 records"))
  }

}

# Remove NAs
df <- df_used[complete.cases(df_used),]
saveRDS(df, "data/df_used_with_fwi.rds")

############################# TABLE 2 ##########################################

rm(list = ls())

df <- readRDS("data/df_used_with_fwi.rds")

# Bias and Anomaly correlation by time, id and season
dfx <- df %>%
  # select(id, lat, long, fwiobs, fwimod, tzid, season) %>%
  group_by(id, lat, long, season, tzid) %>%
  summarise(count = n(),
            bias = mean(fwiobs - fwimod, na.rm = TRUE),
            anomaly_correlation = cor(fwiobs - mean(fwiobs, na.rm = T),
                                      fwimod - mean(fwimod, na.rm = T),
                                      use = "complete.obs")) %>%
  filter(count >= 30) %>%
  na.omit %>%
  mutate(region = sapply(strsplit(tzid, "/"), `[`, 1),
         subregion = sapply(strsplit(tzid, "/"), `[`, 2)) %>%
  filter(region != "Etc") # remove undefined zones
saveRDS(dfx, "data/df_used.rds")

stations_used <- dfx %>% group_by(id) %>%
  summarize(lat = as.numeric(names(which.max(table(lat)))),
            long = as.numeric(names(which.max(table(long))))) # 3084 stations
saveRDS(stations_used, "data/stations_used.rds")

dfx_summary <- dfx %>%
  group_by(region, season) %>%
  summarise(count = n(),
            median_bias = round(median(bias), 2),
            median_ac = round(median(anomaly_correlation), 2))
xtable::xtable(dfx_summary) # copy-paste this into latex main.tex

# Check locations on interactive map
leaflet(data = stations_used) %>%
  addTiles() %>%
  addMarkers(~long, ~lat, label = ~as.character(id))

# How many stations are in the North hemisphere?
round(prop.table(table(stations_used$lat >= 0)), 2)

# How are stations distributed between seasons?
round(prop.table(table(dfx$season)), 2)

# # Bias in Europe
# dfx %>% filter(region == "Europe", season == "Dry") %>% pull(bias) %>% median()
# 
# View(dfx %>% group_by(region) %>% summarise(bias = IQR(bias)))
# View(dfx %>% group_by(region, season) %>% summarise(bias = IQR(bias)))
# 
# # Create a palette that maps factor levels to colors
# pal <- colorFactor(rev(c("#440154FF", "#414487FF", "#2A788EFF",
#                      "#22A884FF", "#7AD151FF", "#FDE725FF")),
#                    domain = levels(dfx$bias_cat))
# 
# leaflet(dfx %>% filter(season == "Dry")) %>% addTiles() %>%
#   addCircleMarkers(radius = 3, color = ~pal(bias_cat), label = ~bias)
############################# FIGURE 1 #########################################

rm(list = ls())

# REANALYSIS 2017 only
fwi2017 <- raster::brick("data/fwi2017re.nc")

# Load time zones vector data
timezonesmap <- rgdal::readOGR(dsn = "data/ne_10m_time_zones",
                               layer = "ne_10m_time_zones")

# These are all the synop stations
stations_all <- readRDS("data/stations_all_unique.rds")

# Load stations used
stations_used <- readRDS("data/stations_used.rds")

# Generate figure

# Convert to sp objects
sp::coordinates(stations_all) <- ~long+lat
sp::coordinates(stations_used) <- ~long+lat

# Use GEFF-RE grid to plot the world
world <- fwi2017[[1]]
world[world > 0] <- 0

pdf(file = "figures/FIG_synop.pdf", width = 10, height = 6.7)
raster::plot(world, col = "gray95", legend = FALSE)
raster::plot(timezonesmap, col = NA, border = "gray70", lty = 2, add = TRUE)
raster::plot(stations_all, col = "#FDE725FF", pch = 19, cex = 0.1, add = TRUE)
raster::plot(stations_used, col = "#21908CFF", pch = 19, cex = 0.1, add = TRUE)
legend(x = "bottom",
       legend = c("Stations recording in 2017",
                  "Stations used for validation"),
        col = c("#FDE725FF", "#21908CFF"),
       pch = 20, bty = "n", horiz = TRUE)
dev.off()

############################# FIGURE 2:3 #######################################
#### STATIAL AND TEMPORAL DISTRIBUTION OF ERRORS ###############################

rm(list = ls())

dfx <- readRDS("data/df_used.rds")

breaks_bias <- dput(round(as.numeric(quantile(dfx$bias,
                                         c(0.05, 0.25, 0.50, 0.75, 0.95))), 2))
breaks_ac <- c(-1, 0.50, 0.60, 0.70, 0.80, 0.90, 1)

dfx$bias_cat <- cut(dfx$bias, breaks = c(-Inf, breaks_bias, Inf))
dfx$ac_cat <- cut(dfx$anomaly_correlation, breaks = breaks_ac)

# Define bounding box
xy <- c(left = min(dfx$long), bottom = min(dfx$lat),
        right = max(dfx$long), top = max(dfx$lat))

# Get background map
gg <- ggmap::get_stamenmap(xy, zoom = 1, maptype = "toner-lite")

# Generate bias plot
pdf(file = "figures/FIG_bias.pdf", width = 6.7, height = 10)
ggmap(gg) +
  geom_point(data = dfx, aes(x = long, y = lat, col = bias_cat), size = 0.5) +
  xlab("Longitude") + ylab("Latitude") +
  scale_color_viridis(discrete = T, drop = FALSE, name = "Bias") +
  facet_grid(rows = vars(season)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold"),
        strip.background = element_rect(fill = "white", colour = "grey")) +
  guides(colour = guide_legend(reverse = T))
dev.off()

# Generate ac plot
pdf(file = "figures/FIG_ac.pdf", width = 6.7, height = 10)
ggmap(gg) +
  geom_point(data = dfx, aes(x = long, y = lat, col = ac_cat), size = 0.5) +
  xlab("Longitude") + ylab("Latitude") +
  scale_color_viridis(discrete = T, drop = FALSE, name = "Anomaly correlation") +
  facet_grid(rows = vars(season)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face="bold"),
        strip.background = element_rect(fill = "white", colour = "grey")) +
  guides(colour = guide_legend(reverse = T))
dev.off()

############################# FIGURE 4 #########################################
# 90th quantile to show dangerous FWI values in various places on the globe ####

# REANALYSIS from 1980-01-01 to 2018-06-30 (from Zenodo v2.2)
fwi <- raster::brick("/hugetmp/fire/geff/reanalysis/fwi.nc")

# no mask version
fwi90 <- raster::calc(fwi,
                      fun = function(x) {quantile(x,probs = c(.9),na.rm=TRUE)},
                      progress = "text")

# no-vegetation mask
novegmask <- raster::raster("data/maskGlobCover2009reanalysis.nc")

# masked version
fwi90_masked <- fwi90
fwi90_masked[!is.na(fwi90_masked) & is.na(novegmask)] <- 0

# Combine them in a stack
fig2 <- raster::stack(fwi90, fwi90_masked)

pdf(file = "figures/FIG_percentile90.pdf", width = 10, height = 13.3)
raster::plot(fig2, nc = 1, nr = 2,
             zlim = c(0, max(maxValue(fwi90))),
             col = colorRampPalette(c("lightgrey", "green4",
                                      "yellow", "red", "brown"))(100),
             main = c("(a)", "(b)"))
dev.off()

# Reduce size of pdfs
# tools::compactPDF(paths = "figures/", gs_quality = "printer")
