####################################################
# MAPS FOR LASA 2018
# 17-05-2018
###################################################

# Libraries
library(psych)
require(grDevices)
library(rgeos)
library(maptools)
library(ggplot2)
library(sp)
library(ggthemes)
library(grid)
library(reshape2)
library(viridis)



#Multiplot Function
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Data for maps
data<- read_delim("C:/Users/Palop/Dropbox/GIGA/Conferences/LASA 2018/Analysis EMIX Paper/data_typology.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

#Create LAC map
world <- readShapeSpatial("C:/Users/palop/Dropbox/GIGA/Conferences/LASA 2018/Analysis EMIX Paper/TM_WORLD_BORDERS_SIMPL-0.3.shp")
plot(world)
as.data.frame(world)
LAC <-world[world$ISO3 == "ARG" |world$ISO3 == "CHL" |world$ISO3 == "BLZ" |world$ISO3 == "BOL" |
              world$ISO3 == "BRA" |world$ISO3 == "COL" |world$ISO3 == "CRI" |world$ISO3 == "CUB" |
              world$ISO3 == "DOM" |world$ISO3 == "ECU" |world$ISO3 == "SLV" |world$ISO3 == "GTM" |
              world$ISO3 == "HND" |world$ISO3 == "JAM" |world$ISO3 == "MEX" |world$ISO3 == "NIC" |
              world$ISO3 == "PAN" |world$ISO3 == "PRY" |world$ISO3 == "PER" |world$ISO3 == "TTO" |
              world$ISO3 == "URY" |world$ISO3 == "VEN" |world$ISO3 == "GUY" |world$ISO3 == "SUR"|
              world$ISO3 == "GUF" |world$ISO3 == "HTI",]
LAC <- fortify(LAC, region = "ISO3")


#Create typology 1
p1_bw <- ggplot() +
  geom_map(data=data, aes(map_id = ISO, fill=typology1), 
           map= LAC) + 
  expand_limits(x =LAC$long, y= LAC$lat)+
  ggtitle("Typology 1")+ theme_bw()+
  theme(legend.position=c(0.2, 0.3), legend.title=element_blank(), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.key.size = unit(0.5, "cm"))+
  ylab("Latitude") + xlab("Longitude")+
  scale_fill_gradient2(limits=c(0,1), low="white", high="black")+
  geom_path(data=LAC, aes(x=long, y=lat, group=group), color="grey")+ coord_equal()
p1_bw


#Create typology 1 color
p1 <- ggplot() +
  geom_map(data=data, aes(map_id = ISO, fill=typology1), 
           map= LAC) + 
  expand_limits(x =LAC$long, y= LAC$lat)+
  ggtitle("Typology 1")+ theme_void()+
  scale_fill_viridis(trans = "log", breaks=c(0,1,2,3,4), name="Group", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(7, units = "mm"), 
                                                                                             label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Typology 1",
    subtitle = "State dedication to Emigrant Policies", 
    caption = "Data: EMIX Emigrant Policies Index"
  ) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.3, 0.09)
  ) +
  geom_path(data=LAC, aes(x=long, y=lat, group=group), color="grey")+ coord_equal()
p1


p2 <- ggplot() +
  geom_map(data=data, aes(map_id = ISO, fill=typology2), 
           map= LAC) + 
  expand_limits(x =LAC$long, y= LAC$lat)+
  ggtitle("Typology 2")+ theme_void()+
  scale_fill_viridis(trans = "log", breaks=c(0,1,2,3,4, 5), name="Group", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(7, units = "mm"), 
                                           label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Typology 2",
    subtitle = "Policy Mixes", 
    caption = "Data: EMIX Emigrant Policies Index"
  ) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.3, 0.09)
  ) +
  geom_path(data=LAC, aes(x=long, y=lat, group=group), color="grey")+ coord_equal()
p2

#Multiplot
multiplot(p1,p2, cols= 2)


####################################################################
# PLOTS FOR FIRST CLUSTER
# First, run analysis of ECPR Joint Sessions 31012018_analysis.R
# Themes: http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
######################################################################

ggplot(typo1, aes(x=POL, y=ADM, 
                  color = as.factor(cluster))) + 
  geom_point(size = 2) +
  geom_text_repel(aes(label = X1)) + ylab("Administration") + 
  xlab("Policies") +theme_economist() + theme(legend.position="none") +
  scale_color_wsj() + ggtitle("Emigrant policies scope")

