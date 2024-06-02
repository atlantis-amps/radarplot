#' Code to create a radar plot
#'  
#' @author Hem Nalini Morzaria Luna
#' @date November 2023
#' # https://github.com/Azure/doAzureParallel


# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#devtools::install_github("ricardo-bion/ggradar", 
#                         dependencies = TRUE)
#https://github.com/ricardo-bion/ggradar

#https://github.com/palaeoverse-community/rphylopic

# List of packages for session
.packages = c("devtools", "tidyverse","stringr","R.utils","here", 
              "ggradar", "rphylopic")
install.packages(.packages, dependencies = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#each column should be an indicator, first column are scenarios
radar.data <-
  tibble(
    scenario = c("Base scenario", "1°C warming", "1.5°C warming"),
    `Chinook abundance` = c(0.98, 0.45, 0.25),
    `Biomass of spawning herring` = c(0.45, 0.62, 0.35),
    `No. of Southern Residents` = c(0.54, 0.65, 0.72),
    `Eeelgrass area` = c(0.7, 0.8, 0.95),
    `Contaminants in juvenile salmon` = c(0.52, 0.85, 0.74),
    `Primary production` = c(0.45, 0.74, 0.78),
    `Dungeness crab catch` = c(0.87, 0.58, 0.36),
    `Harvesteable seafood abundance` = c(0.6, 0.78, 0.37)
  )

# Color for scenarios
# See for colors 
lcols <- c("#caf0f8","#00b4d8","#03045e")

radar.plot <- ggradar(radar.data,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        group.colours = lcols, 
        values.radar = c(0,0.5,1),
        legend.position = "bottom",
        axis.label.size = 3.5)
      #  plot.extent.x.sf = 1.1,
      #  plot.extent.y.sf = 1.1)

ggsave(filename="radar_example.png",radar.plot, scale = 1, width = 12, height = 9)
