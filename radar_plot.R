#' @title Code to create a radar plot for Vital Sign Indicators
#'  
#' @author Hem Nalini Morzaria Luna
#' @date May 2024
#' @description
#' https://vitalsigns.pugetsoundinfo.wa.gov/
#' Assumes output has been processed by PS_review_calibration_runs.R
#' to produce numbers, weight-at-age and biomass files  
# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#devtools::install_github("ricardo-bion/ggradar", 
#                         dependencies = TRUE)
#https://github.com/ricardo-bion/ggradar

#https://github.com/palaeoverse-community/rphylopic

# List of packages for session
.packages = c("devtools", "tidyverse","stringr","R.utils","here", 
              "ggradar", "rphylopic", "googlesheets4", "googledrive","scales", "sysfonts")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

##USER CHANGE FROM HERE

# Specify run directories
folder.paths <- c(here::here("Run_mv1_v6716_15yrs_salmonnoresidents_35","outputFolder"),
                  here::here("Run_mv1_v6716_15yrs_salmonnoresidents_36","outputFolder"))

scenario.names <- c("Base scenario", "1.5°C warming")
startyear <- 2011 #model start year
runtime <- 15 #length of runs in years
outputfrequency <- 73 #output frequency in days

###END PARAMETERS TO MODIFY


googlesheets4::gs4_deauth()

vital.signs<- googlesheets4::read_sheet(ss="1PxZQwlBadcxI0G-Z7i8DMfrgsqAoWperfA76rXq4yIs", sheet = "vital_signs")

no.folder.paths <- 1:length(folder.paths) 
  
get_datafile <- function(no.path, folder.paths, thisparameter, runtime, vital.signs, scenario.names){

  this.folderpath <- folder.paths[no.path]
  this.scenario <- scenario.names[no.path]

  vital.signs.sel <- vital.signs %>% 
    dplyr::filter(output_type==thisparameter) %>%
    dplyr::mutate(ind_no=1:nrow(.)) %>% 
    tidyr::separate_rows(func_group, sep = "\n")
  
    if(thisparameter=="abundance"){
    
    this.file <- readr::read_csv(paste0(this.folderpath,"/Nums.csv"))
    
    data.period <- runtime-5
    
    this.data <- this.file %>% 
      dplyr::filter(Year>data.period) %>% 
      dplyr::group_by(code, longname) %>%
      dplyr::summarise(abundance=mean(variable))
    
    vital.signs.param <- vital.signs.sel %>% 
      dplyr::rename(code=func_group) %>% 
      dplyr::left_join(this.data, by="code") %>%
      dplyr::group_by(indicator_short) %>%
      dplyr::summarise(abundance=sum(abundance)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(scenario=this.scenario, parameter = thisparameter) %>% 
      dplyr::mutate(norm_value=scales::rescale(abundance, to=c(0,1))) %>% 
      dplyr::select(-abundance)
    
      }
    
  if(thisparameter=="biomass"){
   
    this.file <- readr::read_csv(paste0(this.folderpath,"/biomass.csv"))
    
    data.period <- runtime-5
    
    this.data <- this.file %>% 
      dplyr::filter(Year>data.period) %>% 
      dplyr::group_by(Code, name, longname) %>%
      dplyr::summarise(biomass=mean(biomass))
    
    this.data.end <- this.file %>% 
      dplyr::filter(Year>data.period) %>% 
      dplyr::group_by(Code, name, longname) %>%
      dplyr::summarise(end_biomass=mean(biomass))

    this.data.start <- this.file %>% 
      dplyr::filter(Year>5) %>% 
      dplyr::group_by(Code, name, longname) %>%
      dplyr::summarise(start_biomass=mean(biomass))
    
    this.data.selec <- this.data.end %>% 
      dplyr::left_join(this.data.start, by=c("Code","name","longname")) %>% 
      dplyr::mutate(biomass=end_biomass-start_biomass) %>% 
      dplyr::select(Code, name, longname, biomass)
    
   vital.signs.param <- vital.signs.sel %>% 
     dplyr::rename(Code=func_group) %>% 
     dplyr::left_join(this.data, by="Code") %>%
     dplyr::group_by(indicator_short) %>%
     dplyr::summarise(biomass=sum(biomass)) %>% 
     dplyr::ungroup() %>% 
     dplyr::mutate(scenario=this.scenario, parameter = thisparameter) %>% 
     dplyr::mutate(norm_value=scales::rescale(biomass, to=c(0,1))) %>% 
     dplyr::select(-biomass)
  
  }
  
  return(vital.signs.param) 
  
}

radar.plot.data.biomass <- lapply(no.folder.paths, get_datafile, folder.paths, thisparameter="biomass", runtime, vital.signs, scenario.names) %>% 
  bind_rows()

radar.plot.data.abundance <- lapply(no.folder.paths, get_datafile, folder.paths, thisparameter="abundance", runtime, vital.signs, scenario.names) %>% 
  bind_rows()

#https://docs.google.com/spreadsheets/d/1PxZQwlBadcxI0G-Z7i8DMfrgsqAoWperfA76rXq4yIs/edit#gid=0
#each column should be an indicator, first column are scenarios

make_radarplot <- function(radar.plot.data, thisparameter){
  
  radar.data <- radar.plot.data %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from=indicator_short, values_from=norm_value) %>% 
    dplyr::mutate(scenario=as.factor(scenario))  
  
  # Color for scenarios
  # See for colors 
  lcols <- c("#caf0f8","#00b4d8","#03045e")
  
  #Add nice font
  #download.file("
radar.data <- radar.plot.data %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-parameter) %>%
  tidyr::pivot_wider(names_from=indicator_short, values_from=norm_value) %>% 
  dplyr::mutate(scenario=as.factor(scenario))  


# Color for scenarios
# See for colors 
lcols <- c("#caf0f8","#00b4d8","#03045e")

#Add nice font
#download.file("https://github.com/ricardo-bion/ggtech/blob/master/Circular%20Air-Light%203.46.45%20PM.ttf", "~/Circular Air-Light 3.46.45 PM.ttf", method = "curl")

#extrafont::font_import(pattern = 'Circular', prompt = FALSE)

radar.plot <- ggradar::ggradar(radar.data,
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
     #   group.colours = c("#caf0f8","#00b4d8","#03045e"), 
        values.radar = c(0,0.5,1),
        legend.position = "bottom",
        axis.label.size = 4.5,
        font.radar = "roboto")
      #  plot.extent.x.sf = 1.1,
      #  plot.extent.y.sf = 1.1)

ggplot2::ggsave(filename=paste0("radar_",thisparameter,".png"),radar.plot, scale = 1, width = 12, height = 9)

}

sysfonts::font_add_google("Roboto", "roboto")

#add phylopic icon to plot
#beaver_plot + add_phylopic(beaver_pic, alpha = 1, x = 10, y = 37.4, ysize = 10)

make_radarplot(radar.plot.data=radar.plot.data.biomass, thisparameter="biomass")
make_radarplot(radar.plot.data=radar.plot.data.abundance, thisparameter="abundance")