# R

library(ggplot2)
library(maps)
library(mapdata)
library(rgeos)
library(maptools)
library(mapproj)
library(PBSmapping)
library(data.table)

xlim=c(-12,55); ylim=c(20,60)

worldmap=map_data("world")
setnames(worldmap,c("X","Y","PID","POS","region","subregion"))
worldmap=clipPolys(worldmap,xlim=xlim,ylim=ylim,keepExtra=TRUE)

dataFolder="" # ideally, full path to the folder
csvName=paste0(dataFolder,"pleiades-locations-20150316.csv")
locsRaw=read.csv(csvName,stringsAsFactors=F,header=T,sep=',')
# url: http://atlantides.org/downloads/pleiades/dumps/
# ---: download the latest csv, unzip 

periods=rbind(
  c("archaic","750-550BC"),
  c("classical","550-330BC"),
  c("hellenistic-republican","330-30BC"),
  c("roman","30BC-300CE"),
  c("late-antique","300-640CE")
  )

features=rbind(
  c("","locations"),
  c("settlement","settlements"),
  c("fort","forts"),
  c("temple","temples"),
  c("villa","villas"),
  c("station","stations"),
  c("theatre","theatres"),
  c("amphitheatre","amphitheatres"),
  c("church","churches"),
  c("bridge","bridges"),
  c("bath","baths"),
  c("cemetery","cemeteries"),
  c("plaza","plazas"),
  c("arch","archs")
)

land="grey"; water="grey80"; bgColor="grey80"
locPleiades=geom_point(data=locsRaw,color="grey70",alpha=.75,size=1,aes(y=reprLat,x=reprLong))

for (i in 1:nrow(features)) {
  locs=locsRaw[ with(locsRaw, grepl(features[i,1],featureTypes)),]
  for (ii in 1:nrow(periods)) {
    locPer=locs[ with(locs,grepl(periods[ii,1],timePeriodsKeys)),]
    locPer=geom_point(data=locPer,color="red",alpha=.75,size=1,aes(y=reprLat,x=reprLong))
    
    dataLabel="Data: Pleiades Project"
    fName=paste0(dataFolder,"Pleiades_",features[i,2],sprintf("%02d",ii),".png")
    header=paste0(features[i,2]," in the ",periods[ii,1]," period (",periods[ii,2],")")
    
    p=ggplot()+
      coord_map(xlim=xlim,ylim=ylim)+
      geom_polygon(data=worldmap,aes(X,Y,group=PID),size=0.1,colour=land,fill=water,alpha=1)+
      annotate("text",x=-11,y=21,hjust=0,label=dataLabel,size=3,color="grey40")+
      annotate("text",x=54,y=59,hjust=1,label=header,size=5,color="grey40")+ 
      locPleiades+ locPer+ labs(y="",x="")+theme_grey()
    
    ggsave(file=fName,plot=p,dpi=600,width=7,height=6)
  }
}