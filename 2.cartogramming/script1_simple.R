library(ggplot2) # the major graphing library 
library(maps)
library(mapdata)
library(rgeos)
library(maptools)
library(mapproj)
library(PBSmapping) # clipping vector maps
library(data.table)

# plot limits
xlim = c(-12, 55); ylim = c(20, 55)

worldmap = map_data("world")
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
worldmap = clipPolys(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)

land = "grey", water = "grey80", bgColor = "grey80"

dataFolder = "D:/_My Documents/Teaching/2015 Spring - Introduction to Text Mining for Students of the Humanities/Mapping/"
locsRaw = read.csv(paste0(dataFolder,"pleiades-locations-20150316.csv"), stringsAsFactors = F, header = T, sep=',')

locs = locsRaw

periodVar = "archaic"
perNum = paste0(" settlements in the ", periodVar," period (750-550 BC)")
fileName = "Pleiades_00A.png"

# periodVar = "classical"
# perNum = paste0(" settlements in the ", periodVar," period (550-330 BC)")
# fileName = "Pleiades_01C.png"

# periodVar = "hellenistic-republican"
# perNum = paste0(" settlements in the ", periodVar," period (330-30 BC)")
# fileName = "Pleiades_02H.png"
# 
# periodVar = "roman"
# perNum = paste0(" settlements in the ", periodVar," period (30 BC - 300 CE)")
# fileName = "Pleiades_03R.png"
#  
# periodVar = "late-antique"
# perNum = paste0(" settlements in the ", periodVar," period (300-640 CE)")
# fileName = "Pleiades_04A.png"

locs = locsRaw[ with(locsRaw,  grepl("settlement", featureTypes)) , ]
locs = locsRaw[ with(locsRaw,  grepl(periodVar, timePeriodsKeys)) , ]

# locs = locsRaw[ with(locsRaw,  grepl("settlement", featureTypes) &  pName == "2011-02-10_R2" ) , ]

locPleiades = geom_point(data = locs, color = "red", alpha = .5, size = .5, aes(y = reprLat, x = reprLong))

p = ggplot() +
  coord_map(xlim=xlim,ylim=ylim) +   geom_polygon(data=worldmap,aes(X,Y,group=PID), size = 0.1, colour=land, fill=water, alpha=1, ) +
  locPleiades + labs(y="",x="") +
  annotate("text", x=-11, y=21, hjust=0, label="Data: Pleiades Project", size=3, color="grey40") + 
  annotate("text", x=54, y=54, hjust=1, label=perNum, size=5, color="grey40") + 
  theme_grey()
print(p)

ggsave(file = paste0(dataFolder,fileName), plot = p, dpi=600,width = 10,height = 6)
