library(ggplot2)
library(ggmap)
library(xts)

path = '/home/kuol/Documents/Work/201404_R_exploration/maps/crime_denver_2009.csv'
# df = read.csv(path,stringsAsFactors=F, na.strings="unknown")
df = read.csv(path,colClasses=c("Date","factor","factor","numeric","numeric",
                                                       "factor","factor","factor"))
#levels(df$OFFENSE_TYPE_ID)
#levels(df$OFFENSE_CATEGORY_ID)

# subset: violent crimes
df_violent = subset(df, df$OFFENSE_CATEGORY_ID == "murder" | 
                      df$OFFENSE_CATEGORY_ID == "arson" |
                      df$OFFENSE_CATEGORY_ID == "robbery" |
                      df$OFFENSE_CATEGORY_ID == "aggravated-assault")

# Resctric to certain region
df_violent = subset(df_violent, df_violent$GEO_LON >= -105.062698 &
                      df_violent$GEO_LON <= -104.940733 &
                      df_violent$GEO_LAT >= 39.709498 &
                      df_violent$GEO_LAT <= 39.775033)

# df_violent$FIRST_OCCURRENCE_DATE = as.Date(df_violent$FIRST_OCCURRENCE_DATE)
# df_violent = df_violent[c("FIRST_OCCURRENCE_DATE", "OFFENSE_TYPE_ID", "OFFENSE_CATEGORY_ID",
#                           "GEO_LON", "GEO_LAT", "DISTRICT_ID", "NEIGHBORHOOD_ID")]
# df_violent = df_violent[order(df_violent[,1]),]
# 

## Order violent crimes
df_violent$OFFENSE_CATEGORY_ID = factor(df_violent$OFFENSE_CATEGORY_ID,
                                        levels = c("robbery","aggravated-assault",
                                                   "arson","murder")
                                        )

#== Order Days
df_violent$DAY = factor(df_violent$DAY, 
                        levels = c("Sunday","Monday","Tuesday","Wednesday",
                                   "Thursday","Friday","Saturday")
                        )


# # Convert data.frame to xts object, selecting all incidents in 2009, drop redundant column
# df_violent = xts(df_violent, order.by=df_violent$FIRST_OCCURRENCE_DATE)
# df_violent = df_violent["2009-01-01/2009-03-31"]
# df_violent$FIRST_OCCURRENCE_DATE = NULL

lat = c(39.709498,39.775033)
lon = c(-105.062698,-104.940733)

# df_violent = data.frame(FIRST_OCCURRENCE_DATA = index(df_violent), coredata(df_violent))
# df_test = head(df_violent,20)
# Map crimes
#theme_set(theme_bw(16))
Denver_map = qmap("Denver", zoom=13, color = "bw", legend = "topleft")
Denver_map +
  geom_point(data = df_violent, aes(x=GEO_LON, y=GEO_LAT, colour=OFFENSE_CATEGORY_ID,
                                    size = OFFENSE_CATEGORY_ID), alpha=0.5)


Denver_map + 
  stat_density2d(
    aes(x=GEO_LON, y=GEO_LAT, fill=..level.., alpha=..level..),
    size = 2, bins = 6, data = df_violent,
    geom = "polygon"
  ) +
  scale_fill_gradient('Violent
Crime
Density') +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

# houstonMap +
#   stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
#                  size = 2, bins = 4, data = violent_crimes, geom = 'polygon') +
#   scale_fill_gradient('Violent
# Crime
# Density') +
#   scale_alpha(range = c(.4, .75), guide = FALSE) +
#   guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

Denver_map + 
  stat_density2d(
    aes(x=GEO_LON, y=GEO_LAT, fill=..level.., alpha=..level..),
    size = 2, bins = 6, geom = "polygon",
    data = df_violent) + 
  #scale_fill_gradient("number of crimes",low="black", high="red") + 
  scale_fill_gradient("Violentn Crime Density",low="black",high="red") + 
  facet_wrap(~ DAY) +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))+ 
  theme(legend.position=c(0.75,0.4))

# TEST
# d = data.frame(lat=c(39.749498, 39.759498, 39.769498),
#                 lon=c(-105.000733, -105.00533, -105.010733))
# Denver_map + geom_point(data=d, aes(x=lon, y=lat), color="red", size=10, alpha=0.5)


theme_set(theme_bw(16))
houstonMap = qmap("houston", zoom=14, color = "bw", legend = "topleft")
houstonMap +
  geom_point(aes(x = lon, y = lat, colour = offense, size = offense),
             data = violent_crimes)


# Bin crimes
Denver_map + 
  stat_bin2d(
    aes(x=df_violent$GEO_LON, y=df_violent$GEO_LAT, 
        colour = df_violent$OFFENSE_CATEGORY_ID, fill = offense),
    size = .5, bins = 30, alpha = 1/2,
    data = violent_crimes
  )
)




