---
title: "Visualizing EV Network accessibility in Lithuania"
author: "Paulius Alaburda"
date: 2024-09-27
categories:
  - R
draft: true
---

A while ago I built a script that would transform an XML file into a data frame that could be visualised using `mapboxapi`. I lost the script so I am rewriting it as a blogpost!

## Getting the data

My country's road administration provides a public API that lists all stations in the country including their coordinates. Let's use a combination of `httr`, `xml2` and `tidyverse` to get the data into a dataframe. There's quite a bit of unnesting but it's necessary to extract a data frame:

```{r}
#| warning: false
#| message: false

library(httr)
library(xml2)
library(tidyverse)

rs <- GET("https://ev.lakd.lt/publicdata/EnergyInfrastructureTablePublication")

rs_xml <- content(rs, "raw") %>% read_xml()

list_xml <-  as_list(rs_xml)

xml_df = tibble::as_tibble(list_xml) %>%
  unnest_longer(payload)

stations <- xml_df %>%
  filter(payload_id  == "energyInfrastructureSite") %>%
  unnest_wider(payload) %>%
  unnest_wider(energyInfrastructureStation)

stations_selected <- stations %>%
  select(lastUpdated,energyProvider,siteLocation,numberOfRefillPoints,electricEnergyMix)

stations_wide <- stations_selected %>%
  unnest_wider(energyProvider, names_repair = "unique") %>%
  unnest_wider(siteLocation) %>%
  unnest_wider(pointByCoordinates) %>%
  unnest_wider(pointCoordinates) %>%
  select(c(latitude,longitude)) %>%
  unnest(c(latitude,longitude)) %>%
  unnest(c(latitude,longitude))

```

## Where are stations located?

```{r}

sites <- st_as_sf(stations_wide, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
    geom_sf() +
    geom_sf(data = sites, size = 2, shape = 23, fill = "cyan") +
    coord_sf(ylim = c(53.5, 56.5), xlim = c(20.3, 27.3), expand = FALSE)

```

## Pulling an isochrone

Now that we have the coordinates, we're going to call the `mb_isochrone` function from the `mapboxapi` to get a drive-time isochrone for each EV station in Lithuania. At first, I didn't define the `generalize` parameter - that resulted in leaflet taking forever to render a map. Instead, I am using a value of 2500 to create more simple isochrones. This does not speed up the API calls but it helps later when rendering the map.

```{r}

library(mapboxapi)
library(leaflet)

get_isochrone <- function(data) {

  isochrone <- purrr::map2_df(data$longitude, data$latitude, ~mb_isochrone(location = c(.x, .y), profile = "driving", time = 10:30, generalize = 2500))

  return(isochrone)

}

single_iso <- get_isochrone(stations_wide %>% sample_n(50))

```

```{r}

library(sf)
library(fasterize)

isos <- single_iso %>%
  filter(time > 11)

isos_proj <- st_transform(isos, 32618)

template <- raster(isos_proj, resolution = 1000)

iso_surface <- fasterize(isos_proj, template, field = "time", fun = "min")

pal <- colorNumeric("plasma", isos$time, na.color = "transparent")

ev_map <- leaflet() %>%
  addMapboxTiles(style_id = "light-v9",
                 username = "mapbox",
                 scaling_factor = "0.5x") %>%
  addRasterImage(iso_surface, colors = pal, opacity = 0.5) %>%
  addLegend(values = single_iso$time, pal = pal,
            title = "Drive-time from<br>EV station")

ev_map


```
