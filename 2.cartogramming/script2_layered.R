library(ggplot2)
library(maps)
library(mapdata)
library(rgeos)
library(maptools)
library(mapproj)
library(PBSmapping)
library(data.table)

# plot limits
xlim = c(-20, 80); ylim = c(15, 60)

worldmap = map_data("world") # loading worldmap data
setnames(worldmap, c("X","Y","PID","POS","region","subregion")) # clipping worldmap into the set limits
worldmap = clipPolys(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)

dataFolder = "D:/_My Documents/Teaching/2015 Spring - Introduction to Text Mining for Students of the Humanities/Mapping/"
locsRaw = read.csv(paste0(dataFolder,"pleiades-locations-20150316.csv"), stringsAsFactors = F, header = T, sep=',')

locs = locsRaw
land = "grey99"; water = "grey95"; bgColor = "grey80" # some color variables

#locs = locsRaw[ with(locsRaw,  grepl("settlement", featureTypes) &  grepl("late-antique", timePeriodsKeys)) , ]
locsPleiades = geom_point(data = locs, color = "grey60", alpha = .25, size = 1, aes(y = reprLat, x = reprLong))

settlementsPleiades = geom_point(data = locs[ with(locs,  grepl("settlement", featureTypes)) , ],
                                 color = "orange", alpha = 1, size = 1, aes(y = reprLat, x = reprLong))
fortsPleiades       = geom_point(data = locs[ with(locs,  grepl("fort", featureTypes)) , ],
                                 color = "navyblue", alpha = 1, size = 1, aes(y = reprLat, x = reprLong))
templesPleiades     = geom_point(data = locs[ with(locs,  grepl("temple", featureTypes)) , ],
                                 color = "red", alpha = 1, size = 1, aes(y = reprLat, x = reprLong))

p = ggplot() + coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=worldmap,aes(X,Y,group=PID), size = 0.1, colour=land, fill=water, alpha=1, ) +
  
  locsPleiades + annotate("text", x=78, y=56, hjust=1, label="locations", size=4, color="grey60") +
  settlementsPleiades + annotate("text", x=78, y=55, hjust=1, label="settlements", size=4, color="orange") +
  fortsPleiades + annotate("text", x=78, y=54, hjust=1, label="forts", size=4, color="navyblue") +
  templesPleiades + annotate("text", x=78, y=53, hjust=1, label="temples", size=4, color="red") + 
  
  annotate("text", x=-19, y=17, hjust=0, label="Data: Pleiades Project", size=4, color="grey40") + 
  annotate("text", x=78, y=58, hjust=1, label="all periods", size=8, color="grey40") + 
  labs(y="",x="") + theme_grey()

# print(p) # optional: prints out the graph for preview, but not required for ggsaving
ggsave(file = paste0(dataFolder,"Pleiades_Settlements_Forts_Temples.png"), plot=p, dpi=600, width=10, height=6)

