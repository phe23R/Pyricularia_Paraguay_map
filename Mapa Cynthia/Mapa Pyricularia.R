rm(list = ls())

pacman::p_load(
  "memisc",
  "assertthat",
  "sqldf",
  "magrittr",
  "dplyr",
  "reshape2",
  "ggplot2",
  "oz",
  "scatterpie",
  "rgdal",
  "maptools",
  "map",
  "ggmap",
  "tidyr",
  "ggspatial",
  "ggsn",
  "maps",
  "grid",
  "ggpubr"
)



## set working directory to folder of script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Helper functions

#############################################################################
### Datos Pyricularia

# Getting the coordinates of each country
loc_data <- read.csv(
  paste0("./Data/", "Mapa Cinthia.csv"),
  header = TRUE,
  sep = ";",
  dec = ".",
  fileEncoding = "Latin1",
  stringsAsFactors = F
)  %>%
  select(Site, Latitud,  Longitud) %>%
  unique() %>% rename(Location = "Site")

# Getting the coordinates of each country
data <- read.csv(
  paste0("./Data/", "Mapa Cinthia.csv"),
  header = TRUE,
  sep = ";",
  dec = ".",
  fileEncoding = "Latin1",
  stringsAsFactors = F
)

df.fre.host = table(data$Site, data$Host) %>%
  data.frame() %>%
  rename(Location = "Var1")  %>%
  mutate(Freq = Freq)  %>%
  pivot_wider(names_from = Var2, values_from = Freq) %>%
  full_join(., loc_data, by = "Location") %>%
  rename(longitude = "Longitud")  %>%
  rename(latitude = "Latitud")  %>%
  mutate(
    Total = `Avena strigosa`   +
      `Brachiaria sp.`   +
      `Bromus catharticus`     +
      `Cenchrus echinatus`   +
      `Digitaria horizontalis` +
      `Digitaria insularis`   +
      `Digitaria sp.`      +
      `Eleusine indica`     +
      `Lolium multiflorum`   +
      `Rynchelyntrum roseum`  +
      `Stenothaphrum secundatum` +
      `Triticum aestivum`
  )  %>%
  mutate(multiplier = Total / sum(Total))


sum(df.fre.host$Total)

names(df.fre.host)



df.fre.Sp = table(data$Site, data$Species) %>%
  data.frame() %>%
  rename(Location = "Var1")  %>%
  mutate(Freq = Freq)  %>%
  pivot_wider(names_from = Var2, values_from = Freq) %>%
  full_join(., loc_data, by = "Location") %>%
  rename(longitude = "Longitud")  %>%
  rename(latitude = "Latitud") %>%
  mutate(Total =  `Pyricularia grisea` +
           `Pyricularia oryzae` +
           `Pyricularia pennisetigena`)  %>%
  mutate(multiplier = Total / sum(Total))


sum(df.fre.host$Total)

names(df.fre.Sp)



#############################################################################
#############################################################################

# https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf


library(pacman)
p_load(ggmap)


myLocation <- c(-65,-28, -50,-16)

myMap <- get_map(
  location = myLocation,
  source = "stamen",
  maptype = "terrain",
  crop = F
)

ggmap(myMap)

df.fre.host$multiplier


mapplot1 = ggmap(myMap)  +
  geom_scatterpie(
    aes(
      x = longitude,
      y = latitude,
      group = Location,
      r = multiplier * 1
    ),
    data = df.fre.host,
    cols = colnames(df.fre.host[, c(2:13)]),
    alpha = .8,
    color = NA
  ) +
  geom_scatterpie_legend(df.fre.host$Total/sum(df.fre.host$Total) * 1,
                         x = -50,
                         y = -15,
                         n = 2) +
  scale_fill_brewer(palette = "Paired") +
  geom_text(
    aes(
      x = longitude,
      y = latitude,
      group = Location,
      label = Location
    ),
    data = df.fre.host,
    stat = "identity",
    check_overlap = T,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
  ) +
  labs(title = "Causes of death by country", x = "Longitude", y = "Latitude") +
  theme(legend.position = "top")


mapplot1


#######################################################################
######################################################################



world <- map_data('world')
p <- ggplot(world, aes(long, lat)) +
  geom_map(map = world,
           aes(map_id = region),
           fill = NA,
           color = "black") +
  coord_equal() +
  xlim(-65, -50) + ylim(-30,-17) + geom_scatterpie(
    aes(
      x = longitude,
      y = latitude,
      group = Location,
      r = multiplier * 2
    ),
    data = df.fre.host,
    cols = colnames(df.fre.host[, c(2:13)]),
    alpha = .8,
    color = NA
  ) +
  geom_scatterpie_legend(
    df.fre.host$Total/sum(df.fre.host$Total) * 2,
    x = -54,
    y = -19,
    n = 4,
    labeller = function(x)
      sum(df.fre.host$Total) * x / 2
  ) +
  scale_fill_brewer(palette = "Paired") + theme_bw()  +
  geom_text(
    aes(
      x = longitude,
      y = latitude,
      group = Location,
      label = Location
    ),
    data = df.fre.host,
    stat = "identity",
    position = position_dodge(width = 1),
    hjust = 0.5,
    vjust = 0.5,
    size = 5,
    angle = 0,
    check_overlap = F,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
  ) +
  labs(title = "Host sample by location", x = "Longitude", y = "Latitude") +
  theme(text = element_text(size=20),
    legend.position =  c(0.25, 0.25),
        legend.text = element_text(face = "italic",
                                   size = 14),
        legend.title = element_blank(),
        legend.background = element_rect(fill='transparent'))  + 
  annotate(
          "text",
          x = -60,
          y = -22,
          label = "Paraguay",
          size = 8
        ) +
  annotate(
    "text",
    x = -58,
    y = -28,
    label = "Argentina",
    size = 8,
    color = "Darkgray"
  )  +
  annotate(
    "text",
    x = -63,
    y = -18,
    label = "Bolivia",
    size = 8,
    color = "Darkgray"
  ) +
  annotate(
    "text",
    x = -53,
    y = -22,
    label = "Brazil",
    size = 8,
    color = "Darkgray"
  ) +
  annotate(
    "text",
    x = -53,
    y = -17.8,
    label = "Samples",
    size = 6,
    color = "gray"
  )

p


# Save plot
ggsave(
  filename = "./Output/1. Map plot - Host sample by location.png",
  p,
  width = 13,
  height = 8.5,
  units = "in",
  device = "png"
)


ggsave(
  filename = "./Output/1. Map plot - Host sample by location.svg",
  plot = p,
  width = 21,
  height = 21,
  units = "cm",
  dpi = 600,
  scale = 1
)



world <- map_data('world')
p.sp <- ggplot(world, aes(long, lat)) +
  geom_map(map = world,
           aes(map_id = region),
           fill = NA,
           color = "black") +
  coord_equal() +
  xlim(-65, -50) + ylim(-30,-17) + geom_scatterpie(
    aes(
      x = longitude,
      y = latitude,
      group = Location,
      r = multiplier * 2
    ),
    data = df.fre.Sp,
    cols = colnames(df.fre.Sp[, c(2:4)]),
    alpha = .8,
    color = NA
  ) +
  geom_scatterpie_legend(
    df.fre.Sp$Total/sum(df.fre.Sp$Total) * 2,
    x = -54,
    y = -19,
    n = 4,
    labeller = function(x)
      sum(df.fre.Sp$Total) * x / 2
  ) +
  scale_fill_brewer(palette = "Paired") + theme_bw()  +
  geom_text(
    aes(
      x = longitude,
      y = latitude,
      group = Location,
      label = Location
    ),
    data = df.fre.Sp,
    stat = "identity",
    position = position_dodge(width = 1),
    hjust = 0.5,
    vjust = 0.5,
    size = 5,
    angle = 0,
    check_overlap = F,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
  ) +
  labs(title = "Species sample by location", x = "Longitude", y = "Latitude") +
  theme(text = element_text(size=20),
    legend.position =  c(0.2, 0.4),
        legend.text = element_text(face = "italic",
                                   size = 14),
        legend.title = element_blank(),
        legend.background = element_rect(fill='transparent'))  + 
  annotate(
    "text",
    x = -60,
    y = -22,
    label = "Paraguay",
    size = 8
  ) +
  annotate(
    "text",
    x = -58,
    y = -28,
    label = "Argentina",
    size = 8,
    color = "Darkgray"
  )  +
  annotate(
    "text",
    x = -63,
    y = -18,
    label = "Bolivia",
    size = 8,
    color = "Darkgray"
  ) +
  annotate(
    "text",
    x = -53,
    y = -22,
    label = "Brazil",
    size = 8,
    color = "Darkgray"
  ) +
  annotate(
    "text",
    x = -53,
    y = -17.8,
    label = "Samples",
    size = 6,
    color = "gray"
  )

p.sp


# Save plot
ggsave(
  filename = "./Output/1. Map plot - Species sample by location.png",
  p.sp,
  width = 13,
  height = 8.5,
  units = "in",
  device = "png"
)


ggsave(
  filename = "./Output/1. Map plot - Species sample by location.svg",
  plot = p.sp,
  width = 21,
  height = 21,
  units = "cm",
  dpi = 600,
  scale = 1
)


figure <- ggarrange(
  p,
  p.sp,
  labels = c("A", "B"),
  ncol = 2,
  nrow = 1
)
figure


# Save plot
ggsave(
  filename = "./Output/1. Map plot - sample by location.png",
  figure,
  width = 11 * 2,
  height = 11,
  units = "in",
  device = "png"
)


ggsave(
  filename = "./Output/1. Map plot - sample by location.svg",
  plot =  figure,
  width = 21 * 2,
  height = 21,
  units = "cm",
  dpi = 600,
  scale = 1
)
