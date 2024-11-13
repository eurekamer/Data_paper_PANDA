# Script to draw map of the site
# 2024-28-10

# Install necessary libraries and load it

list.of.packages <- c("rnaturalearth", "ggplot2","ggspatial","ggrepel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(rnaturalearth)
library(ggplot2)
library(ggspatial)
library(ggrepel)

# Personalized theme
theme_graphic <- function(base_family = "sans", ...) {
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      legend.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      plot.margin = margin(0, 0.1, 0.1, 0, "cm"),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm")
    )
}

# Map of the sampling locations
## Download countries map
world <- ne_download(scale = 10, type = "countries", category = "cultural", returnclass = "sf")

## Retrieve coordinate of the sites
sites <- read.csv(file.path("Data","original_datasets","id_site.csv"), header=TRUE, sep=";",fileEncoding = "Windows-1252")
colnames(sites)[3:4] <- c("latitude", "longitude")
sites$operateur2 <- substr(sites$operateur,10,10)

## Draw the map
sampling_location <- ggplot(data = world) +
  geom_sf(fill = "grey85") +
  coord_sf(xlim= c(-6, 2.1), ylim= c(44.4, 51.5)) +
  theme_graphic() +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(
    location = "tl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  annotate(geom = "text", x = 0, y = 47, label = "FRANCE", fontface = "italic", color = "grey22", size = 6) +
  annotate(geom = "text", x = -3.5, y = 45.6, label = "Bay of Biscay", fontface = "italic", color = "grey22", size = 4) +
  annotate(
    geom = "text", x = -0.8, y = 50.25, label = "English Channel", fontface = "italic", color = "grey22",
    size = 4, angle = 15
  ) +
  geom_point(data = sites, aes(x = longitude, y = latitude, colour= operateur2), size = 2.9) +
  geom_text_repel(data= sites, aes(x=longitude, y= latitude, label= id_site), 
                  colour="black", size=3, max.overlaps = Inf)+
  labs(x = "Longitude", y = "Latitude", colour= "Dataset")
sampling_location

## Save the map
ggsave(sampling_location, file = file.path("Figures","Figure1_Map.jpeg"),
       width = 220, height = 150, dpi = 400,units = "mm",
       type = "cairo")