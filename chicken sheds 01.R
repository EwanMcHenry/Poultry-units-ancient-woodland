##------ Fri Apr 08 09:41:44 2022 ------##
# Ewan McHenry
setwd("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS")
# libraries ----
library(tidyverse)
library(sf) # for gis
library(units) # for set_units()
library(viridis)
library(htmltools)
library(RColorBrewer)
library(ggpubr)
library(plotly)
library(scales)#prety_breaks()
#devtools::install_github("ropensci/PostcodesioR")
library(PostcodesioR)
library(leaflet)
library(data.table)
library(htmlwidgets)
library(leaflet.providers)
library(gridExtra)
library(ggiraph)
library(cowplot)

source("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\Ewans functions.R")


# load data ----

count = st_read("Data\\administrative boundaries\\counties\\Counties_and_Unitary_Authorities_December_2016_Full_Clipped_Boundaries_in_England_and_Wales.shp") %>% st_transform( 27700)
awi = st_read("Data\\ancient woodland\\original\\AWI_joined_v4.02.shp") %>% st_transform( 27700)

hs.chck00 = read.csv("misc GIS jobs\\chicken sheds\\Data\\IPU location data H&Sshire02.csv")
hs.chck01 = read.csv("misc GIS jobs\\chicken sheds\\Data\\IPUs Shropshire and Herefordshire 2024 working version.csv") # recieved from Alison Caffyn
pw.chck = read.csv("misc GIS jobs\\chicken sheds\\Data\\LOAD Prepped for IPU all (at 20210311 - revised for load v2).csv")

# curation ----
constants = data.frame(row = 1)
## IPU data ----

# make sure that all of hs.chck00 info is within hs.chck01

# fill in blank post codes using eastings and northings




hs.chck00[which(!paste0(hs.chck00$Decision.Date ,hs.chck00$Easting )  %in%
        paste0(hs.chck01$Decision.Date, hs.chck01$Easting)
),] %>% 
  filter(!is.na(Decision.Date)) %>%
  arrange(Post.Code)


hs.chck00 %>%
  group_by(Post.Code) %>% 
  filter(n() > 1) %>% arrange(Post.Code, desc(Easting )) %>% print(n = 100)

hs.chck00 %>%
  filter(!hs.chck00$Post.Code %in% hs.chck01$Post.Code )

# which postcode-decision date combos from hs.chck00 are not in hs.chck01
hs.chck = hs.chck00 %>% left_join(hs.chck01, by = c("post.code", "decsion.date"))



### merging the old and updated herifordshire and shropshire datasets
# pull out those with same post code and decision date
hs.chck00 = hs.chck00 %>% mutate(older = 1)



# avoiding rows with the same post code date and decision



hs.chck = hs.chck00 %>% add_row(hs.chck01)


### join chcken datasets
hs.chck = hs.chck %>% rename(Post.Code = post.code,
                 Eastings = Coordinates , Northings = Coordinates.1 ,
                 Decision = decision, Decision.date = decsion.date,
                 Application.Units = new.sheds, older = X)
pw.chck$older = NA
pw.chck$Application.Units = pw.chck$Application.Units %>% as.numeric()

chck = pw.chck %>%  add_row(hs.chck)
### chck to sf
chck = chck %>% 
  st_as_sf(coords = c("Eastings", "Northings"), crs = 27700) 

### format date
chck$Decision.date =  as.Date(chck$Decision.date,  "%d/%m/%Y")
chck$year =  format(chck$Decision.date, "%Y") %>% as.numeric()
chck$year[is.na(chck$year)] = chck$Year[is.na(chck$year)]  
### remove refused and withdrawn applications unique(chck$Decision)
chck = chck[!(chck$Decision %in% c("refused", "withdrawn", "refused - at appeal", "refused/appeal dismissed",
                                   "deemed withdrawn")),]
chck$Decision[chck$Decision %in% c("approved ","approved JR") ] = "approved"

### curate units - number of new sheds
chck$Application.Units[chck$Application.Units == "?"] = NA
chck$Application.Units[chck$Application.Units == "n.a."] = NA
chck$Application.Units = as.numeric(chck$Application.Units)

### postcode and names ----
#### curate postcode
chck$Post.Code[chck$Post.Code == " SY3 8EU" ] = "SY3 8EU" 
chck$Post.Code[chck$Post.Code == " SY5 6HA" ] = "SY5 6HA" 
chck$Post.Code[chck$Post.Code == "?? TF9 2JY?" ] = "TF9 2LG"  # found by grid ref
chck$Post.Code[chck$Post.Code == "SY8"   ] = ""
chck$Post.Code[chck$Post.Code == "SY 4 4HE"   ] = "SY4 4HE"
chck$Post.Code[chck$Post.Code == "not known"] = ""
chck$Post.Code[chck$Post.Code == "HR2  9DW" ] = "HR29DW" 

#### loop finds postcode from coords for all those without
replace.code = NA
coords = NA
for ( i in 1: sum(chck$Post.Code == "")){
  coords = st_coordinates(chck[chck$Post.Code == "",][i,] %>% st_transform(4326)) %>% as.numeric()
  replace.code[i] = reverse_geocoding(coords[1], coords[2] , limit = 1, wideSearch = 200)[[1]]$postcode
}
chck$Post.Code [chck$Post.Code == ""] = replace.code

#### all farms without name == postcode
chck$no.name = 0
chck$no.name[is.na(chck$Farm.Name)] = 1
chck$Farm.Name[is.na(chck$Farm.Name)] = chck$Post.Code[is.na(chck$Farm.Name)]
### curate farm name
chck$Farm.Name[chck$Farm.Name =="Siluria Farm, Lower Yardo"  ] = "Siluria Farm" 
chck$Farm.Name[chck$Farm.Name =="Pertheirin"  ] = "Pertherin" 
chck$Farm.Name[chck$Farm.Name =="Holbach Mill, Land adj Holbach Mill"  ] = "Holbach Mill" 
chck$Farm.Name[chck$Farm.Name =="Holbach Mill, Land adj Holbach Mill"  ] = "Holbach Mill" 
chck$Farm.Name[chck$Farm.Name =="Dolydre "  ] = "Dolydre" 
chck$Farm.Name[chck$Farm.Name =="Dol y Dre"  ] = "Dolydre" 
chck$Farm.Name[chck$Farm.Name =="Bryndyrnod (Coed Bryndyrnod)"  ] = "Bryndyrnod" 
chck$Farm.Name[chck$Farm.Name =="Blackwood "  ] = "Blackwood" 
chck$Farm.Name[chck$Farm.Name =="Glangwden (Llan Gwden)"  ] = "Glangwden" 
chck$Farm.Name[chck$Farm.Name =="Trelystan (Land nr Muslop Farm)"  ] = "Trelystan" 
chck$Farm.Name[chck$Farm.Name =="Lower Croscynon (Upper Croscynon)"  ] = "Lower Croscynon" 

#### do any with same name have diff postcode or visa versa
a = 1
farms.w.multiple = NA
for ( i in 1: length(unique(chck$Farm.Name))){
  if( length(unique(chck$Post.Code[chck$Farm.Name == unique(chck$Farm.Name)[i] ]))>1){
    farms.w.multiple[a] = unique(chck$Farm.Name)[i]
    a = a +1
  }
}
chck$Post.Code[chck$Farm.Name == "Black Hall Farm"] = "SY15 6HR"
chck$Post.Code[chck$Farm.Name == "Bryn Thomas"] = "LD1 5SY"
chck$Post.Code[chck$Farm.Name == "Bryndu"] = "LD1 6TU"
chck$Farm.Name[chck$Farm.Name == "Ceunant" & chck$Post.Code == "SY22 6BT"] = "Ceunant02"
chck$Farm.Name[chck$Farm.Name == "Church House Farm" & chck$Post.Code == "LD1 6SE"] = "Church House Farm02"

chck$Post.Code[chck$Farm.Name == "Cwm"] = "SY16 3JG"

a = 1
code.w.multiple = NA
names.code.list = list(NA)
for ( i in 1: length(unique(chck$Post.Code))){
  if( length(unique(chck$Farm.Name[chck$Post.Code == unique(chck$Post.Code)[i] ]))>1){
    code.w.multiple[a] = unique(chck$Post.Code)[i]
    names.code.list[[a]] = chck$Farm.Name[chck$Post.Code == code.w.multiple[a] ] %>% unique()
    a = a +1
  }
}   

### total numbers ----

#### find where total numbers inconsistent within farm
farms.diff.totalshed = NA
farms.diff.totalchck = NA
a = 1
aa=2
for ( i in 1: length(unique(chck$Farm.Name))){
  if( length(unique(chck$Total.number.of.sheds.on.farm[chck$Farm.Name == unique(chck$Farm.Name)[i] ]))>1){
    farms.diff.totalshed[a] = unique(chck$Farm.Name)[i]
    a = a +1
  }
  if( length(unique(chck$Total.number.of.birds.on.farm[chck$Farm.Name == unique(chck$Farm.Name)[i] ]))>1){
    farms.diff.totalchck[aa] = unique(chck$Farm.Name)[i]
    aa = aa +1
  }
}
chck$Total.number.of.birds.on.farm[chck$Farm.Name == "Bryn Thomas" ] = 100000 
chck$Total.number.of.sheds.on.farm[chck$Farm.Name == "Bryn Thomas" ] = 2 

#### where no total = sum of all approved on farm
replacement.vec = rep(NA, times = dim(chck)[1])
 for ( i in 1: length(unique(chck$Farm.Name[is.na(chck$Total.number.of.sheds.on.farm)]))){
   replacement.vec[chck$Farm.Name == unique(chck$Farm.Name[is.na(chck$Total.number.of.sheds.on.farm)])[i]] = 
    rep(
      sum( # sum all applications approved on that farm
        chck$Application.Units[chck$Farm.Name == unique(chck$Farm.Name[is.na(chck$Total.number.of.sheds.on.farm)])[i] ], 
      na.rm = T), 
      times = length(chck$Total.number.of.sheds.on.farm[chck$Farm.Name == unique(chck$Farm.Name[is.na(chck$Total.number.of.sheds.on.farm)])[i]])
    )
}
chck$Total.number.of.sheds.on.farm[is.na(chck$Total.number.of.sheds.on.farm)] = 
  replacement.vec[is.na(chck$Total.number.of.sheds.on.farm)]

### more curation ----
#### cut out only most recent for each farm to make new df
chck$date.order = order(chck$Decision.date)
chck$application.id = 1:dim(chck)[1]
chck2 = chck[as.logical(ave(chck$date.order, chck$Farm.Name, FUN = function(x) x == max(x))),]

### remove where most recent record for farm was before 1992 or "older" 
constants$cut.year = 1992
chck2 = chck2[ is.na(chck2$year) | chck2$year >= constants$cut.year,]
chck2 = chck2[chck2$older == "newer"| is.na(chck2$older),]
chck2$farm = 1
chck2$Total.number.of.birds.on.farm = format(chck2$Total.number.of.birds.on.farm, big.mark=",")
chck2$Total.number.of.birds.on.farm = gsub(" ", "", chck2$Total.number.of.birds.on.farm)
chck2$Total.number.of.birds.on.farm[chck2$Total.number.of.birds.on.farm == "NA"] = "Unknown number of"
chck2$n.birds.lab = paste(chck2$Total.number.of.birds.on.farm, "birds")

constants$n.tot.sheds = sum(chck2$Total.number.of.sheds.on.farm)

write.csv(chck2, "misc GIS jobs\\chicken sheds\\Data\\curated.ipu.data.csv")

## counties and AW data ----
### cut out interested counties, buffer and clip out all AWI 
areas = count[count$ctyua16nm %in% c("Herefordshire, County of", 
                                     "Telford and Wrekin", "Shropshire", "Powys"),] %>% st_simplify( preserveTopology = T, dTolerance = 100)
constants$awi.buff = 1000
areas.buff = areas %>% st_simplify( preserveTopology = T, dTolerance = 100) %>% # first simplify hack to reduce run time. this is a rough buffer to negate edge effects, so can be v rough
  st_buffer( dist = constants$awi.buff) %>%
  st_simplify( preserveTopology = FALSE, dTolerance = 1000) %>% 
  st_union()
awi.here <- st_intersection(awi, areas.buff)#st_crop(awi, areas.buff)

awi.here$Site_Name[is.na(awi.here$Site_Name)] = "No name"

## hex grid ----
constants$hexdist.h = 5000 
constants$hexdist.v = 5000
hex.grid0 = st_make_grid(areas.buff, c(constants$hexdist.h, constants$hexdist.v), what = "polygons", square = F) %>% 
  st_sf() %>%
  # add grid ID
  mutate(grid_id = 1:dim(.)[1]) %>% 
  # intersect with landscape - note that grid id is from original UK-wide grid, allows easy cross-ID
  st_intersection(., areas %>% st_union()) %>% 
  # st_make_valid() %>%  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% 
  dplyr::select(grid_id)

hex.grid0$hexarea = st_area(hex.grid0) %>% 
  set_units(value = "ha") %>% 
  as.numeric()

hex.grid0 = hex.grid0[hex.grid0$hexarea>0,]

### hex aw, sheds and farms ----
# aw
hex.grid.aw = st_intersection(hex.grid0, awi.here)

hex.grid.aw$awi.area.ha = st_area(hex.grid.aw) %>% 
  set_units(value = "ha") %>% 
  as.numeric()

hex.grid.awarea = hex.grid.aw %>% as.data.frame() %>% 
  group_by(grid_id) %>% 
  summarise(awi.area = sum(awi.area.ha)) 

constants$tot.awi = sum(hex.grid.awarea$awi.area)

### hex farms and sheds
hex.grid.chck = st_intersection(hex.grid0, chck2)

hex.grid.farms = hex.grid.chck %>% as.data.frame() %>% 
  group_by(grid_id) %>% 
  summarise(n.farms = sum(farm)) 

hex.grid.sheds = hex.grid.chck %>% as.data.frame() %>% 
  group_by(grid_id) %>% 
  summarise(n.sheds = sum(Total.number.of.sheds.on.farm)) 

# add to grid
hex.grid = left_join(hex.grid0,hex.grid.awarea, by = "grid_id" ) %>% 
  left_join(. ,hex.grid.farms, by = "grid_id" ) %>% 
  left_join(. ,hex.grid.sheds, by = "grid_id" )

hex.grid$awi.area[is.na(hex.grid$awi.area)] = 0
hex.grid$n.sheds[is.na(hex.grid$n.sheds)] = 0
hex.grid$n.farms[is.na(hex.grid$n.farms)] = 0

hex.grid$awi.per = 100* hex.grid$awi.area/hex.grid$hexarea

# analysis ----
## heatmaps ----
### AW heatmap ----
var.name = "awi.per"
main.title = NULL
sub.title = NULL
fill.scale.title = "Ancient woodland \ncover (%)"

colour.limits = c(0,20)
dividor = 1

aw.hexmap = map.ploter.ident (
  fill.scale.title = fill.scale.title , main.title = main.title , sub.title = sub.title,
 # background = areas,
  fillground = hex.grid, fillground2 = hex.grid,
  col.limits= colour.limits , 
  to.plot = hex.grid$awi.per,
  clr.breaks = colour.brks(lims = colour.limits),
  clr.labels = colour.lable(x = hex.grid$awi.per ,
                            lims = colour.limits , 
                            breaks = colour.brks(colour.limits ), dividor = dividor))



# variable = hex.grid.overlap$awipuoverlap.area
hex.grid01 = sf::st_cast(hex.grid, "MULTIPOLYGON")

gg.aw.hexmap = ggplot(data=hex.grid01) + 
  geom_sf(mapping = aes(fill = awi.per, 
                        text =  map(paste0(format(round(awi.area,0), big.mark=","), " ha (", format(round(awi.per,1), big.mark=","), " %) <br>"), HTML)
  ), colour = "black", size = 0.1) +
  geom_sf(mapping = aes(),fill = NA, data = areas, colour = "black", size = 0.1) +
  scale_fill_gradient( low = "white", high = "darkgreen",
                       name = fill.scale.title,
                       limits = colour.limits, 
                       oob = scales::squish, 
                       breaks = colour.brks(lims = colour.limits),
                       labels = colour.lable(x = hex.grid01$awi.per ,
                                             lims = colour.limits , 
                                             breaks = colour.brks(colour.limits ), dividor = dividor),
                       #option = "magma",direction = -1 
                       guide = guide_colorbar(
                         direction = "horizontal", barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"), draw.ulim = F,
                         title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
  labs(x = NULL, y = NULL , title = main.title, subtitle = sub.title#, caption = ""
  )+
  theme_map() +
  theme(legend.position = "bottom") 

plty.aw.hexmap = ggplotly(gg.aw.hexmap, tooltip = "text", 
                                    dynamicTicks = T) %>%
  config(displayModeBar = FALSE) %>% 
  layout(legend = list(orientation = 'h'))

### farms heatmap ----
var.name = "n.farms"
main.title = "IPU Farms"
sub.title = ""
fill.scale.title = "Number of farms"

dividor = 1
colour.limits = c(0,5)
clr.breaks = colour.brks(lims = colour.limits)
clr.labels = colour.lable(x = hex.grid$n.farms ,
                          lims = colour.limits , 
                          breaks = colour.brks(colour.limits ), dividor = dividor)


gg.farm.hexmap = ggplot(data=hex.grid01) + 
  geom_sf(mapping = aes(fill = n.farms, 
                        text =  map(paste0(n.farms, " farms<br>", n.sheds, " sheds"), HTML)
  ), colour = "black", size = 0.1) +
  geom_sf(mapping = aes(),fill = NA, data = areas, colour = "black", size = 0.1) +
  scale_fill_gradient( low = "white", high = "darkblue",
                       name = fill.scale.title,
                       limits = colour.limits, 
                       oob = scales::squish, 
                       breaks = colour.brks(lims = colour.limits),
                       labels = colour.lable(x = hex.grid01$n.farms ,
                                             lims = colour.limits , 
                                             breaks = colour.brks(colour.limits ), dividor = dividor),
                       #option = "magma",direction = -1 
                       guide = guide_colorbar(
                         direction = "horizontal", barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"), draw.ulim = F,
                         title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
  labs(x = NULL, y = NULL , title = NULL, subtitle = sub.title#, caption = ""
  )+
  theme_map() +
  theme(legend.position = "bottom") +
  ggspatial::annotation_scale(location = 'br')

plty.farm.hexmap = ggplotly(gg.farm.hexmap, tooltip = "text", 
                          dynamicTicks = T) %>%
  config(displayModeBar = FALSE) %>% 
  layout(legend = list(orientation = 'h'))

### sheds heatmap ----
var.name = "n.sheds"
main.title = "IPU sheds"
sub.title = ""
fill.scale.title = "Number of sheds"

dividor = 1
colour.limits = c(0,15)
# variable = hex.grid$n.sheds

gg.sheds.hexmap = ggplot(data=hex.grid01) + 
  geom_sf(mapping = aes(fill = n.sheds, 
                        text =  map(paste0(n.farms, " farms<br>", n.sheds, " sheds"), HTML)
  ), colour = "black", size = 0.1) +
  geom_sf(mapping = aes(),fill = NA, data = areas, colour = "black", size = 0.1) +
  scale_fill_gradient( low = "white", high = "black",
                       name = fill.scale.title,
                       limits = colour.limits, 
                       oob = scales::squish, 
                       breaks = colour.brks(lims = colour.limits),
                       labels = colour.lable(x = hex.grid01$n.sheds ,
                                             lims = colour.limits , 
                                             breaks = colour.brks(colour.limits ), dividor = dividor),
                       #option = "magma",direction = -1 
                       guide = guide_colorbar(
                         direction = "horizontal", barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"), draw.ulim = F,
                         title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
  labs(x = NULL, y = NULL , title = NULL, subtitle = sub.title#, caption = ""
  )+
  theme_map() +
  theme(legend.position = "bottom") +
  ggspatial::annotation_scale(location = 'br')

plty.sheds.hexmap = ggplotly(gg.sheds.hexmap, tooltip = "text", 
                            dynamicTicks = T) %>%
  config(displayModeBar = FALSE) %>% 
  layout(legend = list(orientation = 'h'))

## distance to nearest AWI ----
chck2$awi.dist = st_distance(chck2, 
                             awi.here[st_nearest_feature(chck2,awi.here),], by_element=TRUE) %>% as.numeric()

### hist - farms min aw distance ----
gg.farms.mindist = ggplot(chck2[chck2$awi.dist<constants$awi.buff,], aes(awi.dist)) + 
  geom_histogram(aes(y=cumsum(..count..), 
                     text = map(paste0(cumsum(..count..), " farms <br>", round(x,0), " m"), HTML)
                    ),
                    bins = 100)+
  theme_pubr()+
  scale_y_continuous(breaks = pretty_breaks(5)) +
  labs(x = "Distance to ancient woodland (m)",title = NULL, 
       y = "Farms (cum. N)", subtitle = NULL#, caption = ""
  ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.title = element_text(size = 12 , face = "bold"),
        legend.text =  element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12)
  )  
plty.farms.mindist = ggplotly(gg.farms.mindist, tooltip = "text", 
                              dynamicTicks = T) %>%
  config(displayModeBar = FALSE)



### hist - shed min aw distance ----

chck3 = chck2[rep(1:dim(chck2)[1], times = chck2$Total.number.of.sheds.on.farm %>% ceiling() %>% as.numeric()),]
gg.sheds.mindist = ggplot(chck3[chck3$awi.dist<constants$awi.buff,], aes(awi.dist)) + 
  geom_histogram(aes(y=cumsum(..count..), 
                     text = map(paste0(cumsum(..count..), " sheds <br>", round(x,0), " m"), HTML)
  ),
  bins = 100)+
  theme_pubr()+
  scale_y_continuous(breaks = pretty_breaks(5)) +
  labs(x = "Distance to ancient woodland (m)", title = NULL, 
       y = "Sheds (cum. N)", subtitle = NULL#, caption = ""
  ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.title = element_text(size = 12 , face = "bold"),
        legend.text =  element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title =  element_text(size = 12)
  ) 
plty.sheds.mindist = ggplotly(gg.sheds.mindist, tooltip = "text", 
                              dynamicTicks = T) %>%
  config(displayModeBar = FALSE)

## awi area close to IPUs ----
## total area of awi within different distances from sheds 
distances = seq(from = 0, to = 1000, by = 50)
awi.chck.area.list = list(NA)
awi.chk.areas = data.frame(distance = distances, awiarea = NA)

for ( i in 1: dim(awi.chk.areas)[1]){
  awi.chck.area.list[[i]] =  chck2 %>% st_buffer(dist = awi.chk.areas$distance[i]) %>% st_union() %>% 
    st_intersection(awi.here) 
  awi.chk.areas$awiarea[i] = awi.chck.area.list[[i]] %>% st_area() %>% set_units(value = "ha") %>% sum()
}

# awi.chk.areas = rbind(awi.chk.areas[1,],awi.chk.areas)
# awi.chk.areas[1,]= 0

gg.aw.ipu.dist.area = ggplot(awi.chk.areas) + 
  geom_line(aes(x = distance/1000,  y = awiarea, 
                text = map(paste0(format(round(awiarea,0), big.mark=","), " ha  of anceint woodland<br> < ", round(distance,0), " m from an IPU"), HTML)
  ))+
  theme_pubr()+
  scale_y_continuous(breaks = pretty_breaks(3)) +
  scale_x_continuous(breaks = pretty_breaks(3)) +
  labs(x = "Distance from IPUs (km)",title = NULL, 
       y = "AW area (ha)", 
       subtitle = "Area of ancient woodland in varying proximity to intensive poultry units (IPUs) in Herefordshire, Telford and Wrekin, Shropshire and Powys"
       #, caption = ""
  ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.title = element_text(size = 12 , face = "bold"),
        legend.text =  element_text(size = 12),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title =  element_text(size = 12)
  ) 
plty.aw.ipu.dist.area = ggplotly(gg.aw.ipu.dist.area, tooltip = "text", 
                              dynamicTicks = T) %>%
  config(displayModeBar = FALSE ) %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))


my_breaks <- c(400, 800,1000)
clrs <- brewer.pal(length(my_breaks), "YlOrRd") %>% rev()

gg.aw.ipu.dist.area.col = ggplot(awi.chk.areas) + 
  geom_ribbon(data = awi.chk.areas[awi.chk.areas$distance<=400,], aes(x = distance/1000, ymax = awiarea, ymin = rep(0, sum(awi.chk.areas$distance<=400))), fill = clrs[1] )+
  geom_ribbon(data = awi.chk.areas[awi.chk.areas$distance>=400 & awi.chk.areas$distance<=800,], aes(x = distance/1000, ymax = awiarea, ymin = rep(0, sum(awi.chk.areas$distance>=400 & awi.chk.areas$distance<=800))), fill = clrs[2] )+
  geom_ribbon(data = awi.chk.areas[awi.chk.areas$distance>= 800,], aes(x = distance/1000, ymax = awiarea, ymin = rep(0, sum(awi.chk.areas$distance>=800))), fill = clrs[3] )+
  
#  geom_area(aes(x = distance/1000,  y = awiarea, fill = area.col)) +
  geom_line(aes(x = distance/1000,  y = awiarea, 
                text = map(paste0(format(round(awiarea,0), big.mark=","), " ha  of anceint woodland<br> < ", round(distance,0), " m from an IPU"), HTML)
  ))+
  theme_pubr()+
  scale_y_continuous(breaks = pretty_breaks(3)) +
  scale_x_continuous(breaks = pretty_breaks(3), lim = c(0,1)) +
  labs(x = "Distance from IPUs (km)",title = NULL, 
       y = "AW area (ha)", 
       subtitle = "Area of ancient woodland in varying proximity to intensive poultry units (IPUs) in Herefordshire, Telford and Wrekin, Shropshire and Powys"
       #, caption = ""
  ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.title = element_text(size = 12 , face = "bold"),
        legend.text =  element_text(size = 12),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title =  element_text(size = 12)
  ) 
plty.aw.ipu.dist.area.col = ggplotly(gg.aw.ipu.dist.area.col, tooltip = "text", 
                                 dynamicTicks = T) %>%
  config(displayModeBar = FALSE ) %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))


## heatmap - area of overlap ----
constants$overlap.dist = 1000
threatenedaw = awi.chck.area.list[[which(distances == constants$overlap.dist)]]
hex.grid.awipuoverlap = st_intersection(hex.grid, threatenedaw)

hex.grid.awipuoverlap$awipuoverlap.ha = st_area(hex.grid.awipuoverlap) %>% 
  set_units(value = "ha") %>% 
  as.numeric()

hex.grid.awipuoverlaparea = hex.grid.awipuoverlap %>% as.data.frame() %>% 
  group_by(grid_id) %>% 
  summarise(awipuoverlap.area = sum(awipuoverlap.ha)) 

hex.grid.overlap = left_join(hex.grid,hex.grid.awipuoverlaparea, by = "grid_id" )

hex.grid.overlap$awipuoverlap.area[is.na(hex.grid.overlap$awipuoverlap.area)] = 0

hex.grid.overlap$awipuoverlap.peraw = 100 *hex.grid.overlap$awipuoverlap.area/hex.grid.overlap$awi.area
hex.grid.overlap$awipuoverlap.peraw[is.na(hex.grid.overlap$awipuoverlap.peraw)] = 0


var.name = "awipuoverlap.area"
main.title = "Ancient woodland threatened by IPUs"
sub.title = paste("Area of ancient woodland within", constants$overlap.dist,"m of intensive poultry units in Herefordshire, Telford and Wrekin, Shropshire and Powys" )
fill.scale.title = "Threatened Ancient\n woodland (ha)"

colour.limits = c(0,100)
dividor = 1
# variable = hex.grid.overlap$awipuoverlap.area
hex.grid.overlap01 = sf::st_cast(hex.grid.overlap, "MULTIPOLYGON")
 
aw.threatned.hexmap = ggplot(data=hex.grid.overlap01) + 
  geom_sf(mapping = aes(fill = awipuoverlap.area, 
                        text =  map(paste0(format(round(awipuoverlap.area,0), big.mark=","), " ha <br>", format(round(awipuoverlap.peraw,0), big.mark=","), "% of all AW" ), HTML)
                          ), colour = "black", size = 0.1) +
  geom_sf(mapping = aes(),fill = NA, data = areas, colour = "black", size = 0.1) +
  scale_fill_gradient( low = "white", high = "red",
                       name = fill.scale.title,
                        limits = colour.limits, 
                        oob = scales::squish, 
                        breaks = colour.brks(lims = colour.limits),
                        labels = colour.lable(x = hex.grid.overlap01$awipuoverlap.area ,
                                              lims = colour.limits , 
                                              breaks = colour.brks(colour.limits ), dividor = dividor),
                        #option = "magma",direction = -1 
                        guide = guide_colorbar(
                          direction = "horizontal", barheight = unit(2, units = "mm"),
                          barwidth = unit(50, units = "mm"), draw.ulim = F,
                          title.position = 'top', title.hjust = 0.5, label.hjust = 0.5))+
  labs(x = NULL, y = NULL , title = NULL, subtitle = sub.title#, caption = ""
  )+
  theme_map() +
  theme(legend.position = "bottom") 

plty.aw.threatned.hexmap = ggplotly(aw.threatned.hexmap, tooltip = "text", 
                                 dynamicTicks = T) %>%
  config(displayModeBar = FALSE)

# threatened graph and heatmap join ----
# threat.join.plot = subplot(plty.aw.ipu.dist.area, plty.aw.threatned.hexmap
#                            ) 
# 
# %>% 
#   layout(title = 'Ancient woodland Threatened by IPUs')


## points of units, coloured by N sheds ----
# leaflet map ----
# title
rr <- tags$div(
  HTML(paste0('<strong>Intentive Poultry Units (IPUs) and Ancient Woodland</strong><br>Dr. Ewan McHenry, ', format(Sys.time(), '%d %B, %Y')))
)  


ipus.m <- leaflet() %>%
  setView(lat = median(chck2$Latitude, na.rm = T)-0.2, lng = median(chck2$Longitude, na.rm = T)+0.2 , zoom = 8) %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>% 
  # study area areas
  addPolygons(data = areas  %>% st_simplify(dTolerance = 500) %>% st_transform(4326), stroke = T, 
              color = "black" ,fillColor =  NA, weight = 1, smoothFactor = 1,
              opacity = 0.9, fillOpacity = 0) %>%
  addPolygons(data = areas %>% st_buffer(dist = 700) %>% st_simplify(dTolerance = 500) %>% st_union() %>% st_transform(4326), stroke = T, 
              color = "black" ,fillColor =  NA, weight = 5, smoothFactor = 1,
              opacity = 0.9, fillOpacity = 0) %>%
  #aw
  addPolygons(data = awi.here %>% st_simplify(dTolerance = 100) %>% st_transform(4326), stroke = T, color = "green" ,fillColor =  "forestgreen", weight = 0.5, smoothFactor = 0.5,
              opacity = 0.3, fillOpacity = 0.2, 
              popup = paste0(
                "<strong>", awi.here$Site_Name, "</strong>", "<br>",
                "<strong>", awi.here$nu_cat, "</strong><br>"
                ), group = "Ancient woodland") %>%
  addPolygons(data = threatenedaw %>% st_simplify(dTolerance = 100) %>% st_transform(4326), stroke = T, color = "black" ,fillColor =  "grey9", weight = 0.5, smoothFactor = 0.5,
              opacity = 0.3, fillOpacity = 0.6, 
              popup = paste0(
                "<strong>", awi.here$Site_Name, "</strong>", "<br>",
                "<strong>", awi.here$nu_cat, "</strong><br>"
              ), group = "Threatened Ancient woodland") %>%
  addCircleMarkers(data = chck2 %>% st_transform(4326),
             popup = paste0(
               "<strong>", chck2$Total.number.of.sheds.on.farm, " sheds </strong>", "<br>",
               "Approved ", chck2$year, "<br>", chck2$n.birds.lab), group = "IPUs",
             clusterOptions = markerClusterOptions()
             ) %>% 
  addLayersControl(
    overlayGroups = c( 
      "Ancient woodland", "Threatened Ancient woodland", "IPUs"),
    options = layersControlOptions(collapsed = F)
  ) %>% 
  hideGroup( list("Threatened Ancient woodland", "Ancient woodland")) %>% 
  addControl(rr, position = "bottomleft")

# save ----
saveWidget(ipus.m, file="misc GIS jobs\\chicken sheds\\IPU leaflet01.html")

# save objects ----
save(awi.chk.areas, ipus.m,  chck, chck2, chck3,gg.aw.ipu.dist.area, plty.farms.mindist,
     plty.sheds.mindist, plty.aw.hexmap, hex.grid01, hex.grid.overlap, 
     plty.farm.hexmap,
     plty.sheds.hexmap, plty.aw.ipu.dist.area, plty.aw.ipu.dist.area.col,  
     plty.aw.threatned.hexmap, constants, 
     gg.farm.hexmap, gg.sheds.hexmap, gg.farms.mindist, gg.sheds.mindist, gg.aw.ipu.dist.area,
     file =  "misc GIS jobs\\chicken sheds\\r_objects.RData")


# render Rmarkdown

library(rmarkdown)
setwd("D:\\Users\\Ewan McHenry\\OneDrive - the Woodland Trust\\GIS\\misc GIS jobs\\chicken sheds")
load("r_objects.RData")
render("IPU analysis.rmd",output_file =  
         "IPU analysis.html")
render("IPU analysis.rmd",output_file =  
         "IPU analysis.aspx")

