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
functionalgroup.file <- "PugetSoundAtlantisFunctionalGroups.csv" #should be saved in same directory as this code
this.output.nc <- "AMPS_OUT.nc" #name of output nc file
###END PARAMETERS TO MODIFY

#Get data from output files

thistimestep <- (runtime*365)/outputfrequency #this is max number of timesteps/73 output frequency

maxtimestep <- runtime*365

# make detailed Biomass, biomass per plot, Rn Sn, and Nums plots

fg.list <- readr::read_csv(here::here(functionalgroup.file)) %>% 
  dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, name, longname) %>% 
  dplyr::filter(!Code %in% c("DIN","DL"))

#add for spatial biomass
#variabletypes <- c("BoxBiomass","Biomass","StructN","ResN","Nums","Wage")
variabletypes <- c("Biomass","StructN","ResN","Nums","Wage")

vert.groups <- fg.list %>% 
  filter(IsTurnedOn==1) %>% 
  filter(GroupType == "FISH" | GroupType == "SHARK" | GroupType == "BIRD"| GroupType == "MAMMAL") %>% 
  dplyr::select(name) %>% .$name


#get values from NC file
get.nc.data <- function(eachgroup,thisncfile){
  
  print(paste("Analyzing this group",eachgroup))
  
  this.sprow <- fg.list %>% 
    filter(name==eachgroup) 
  
  print(this.sprow)
  
  group.ages <- paste(eachgroup,1:this.sprow$NumCohorts,sep="")
  
  print(group.ages)
  
  #make names for nc variables
  
  varlist <- c("_ResN","_Nums","_StructN","_Wage")
  
  var.listdata <- list()
  
  for(eachvar in 1:length(varlist)){
    
    eachvarlist <- varlist[eachvar]
    
    print(eachvarlist)
    
    name.var <- paste(group.ages,eachvarlist,sep="")
    
    variable.type <- gsub("_","",eachvarlist)
    
    if(eachvarlist == "_ResN" | eachvarlist == "_StructN" | eachvarlist == "_Wage") {
      
      for(eachage in 1:length(name.var)) {
        
        if(eachvarlist == "_Wage"){
          
          eachvarlist = "_ResN"
          name.var <- paste(group.ages,eachvarlist,sep="")
          variable.type <- gsub("_","",eachvarlist)
          eachvarlist = "_Wage"
        }
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- RNetCDF::var.get.nc(thisncfile, eachvariable)
        
        if(eachvarlist == "_Wage") {
          
          thisData<-thisData*20*5.7*(3.65/2.65)/1000000
          
          variable.type = "Wage"
        }
        
        thisData[thisData==0]<-NA  # Replace 0's with NA
        thisData <- thisData[1:7,2:89,1:thistimestep]
        thisDataMeanMg <-apply(thisData,3,mean,na.rm = TRUE) #Get mean size over time, averaging over depth and location 
        
        thisY <-tibble(variable=thisDataMeanMg/thisDataMeanMg[1]) %>%  # Normalize by initial value
          mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
        
        if(eachvarlist == "_Wage") {
          
          thisY <-tibble(variable=thisDataMeanMg) %>%  # Normalize by initial value
            mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
          
        }
        
        listname <- paste(thisY$group[1],"_",thisY$variable_type[1],sep="")
        var.listdata[[listname]] <- thisY  
        
      } 
      
    } else if (eachvarlist == "_Nums") {
      
      for(eachage in 1:length(name.var)) {
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- RNetCDF::var.get.nc(thisncfile, eachvariable)
        thisData[thisData==0]<-NA  # Replace 0's with NA
        print(dim(thisData))
        thisData <- thisData[1:7,2:89,1:thistimestep]
        #thisData <- thisData[1:7,2:89,1:51] #use this for 10 year runs
        thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location  
        thisY <-tibble(variable=thisDataNums) %>%  # Normalize by initial value
          mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
        
        var.listdata[[eachvariable]] <- thisY  
        
      }
      
    }
  }
  
  
  thissp.data <- var.listdata %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(code = this.sprow$Code, longname = this.sprow$longname) %>% 
    dplyr::rename(atlantis_group = group) %>% 
    dplyr::mutate(Year=(time*outputfrequency)/365)
  
  
  print(paste("Done with group",eachgroup))
  
  
  return(thissp.data)
}

get_outdata <- function(eachfolder){
  
  thispath <- folder.paths[eachfolder]
  
  this.output.biomass <- readr::read_delim(paste0(thispath,"/AMPS_OUTBiomIndx.txt")) %>% 
    dplyr::select(Time:DIN) %>% 
    gather(Code,biomass, -Time) %>% 
    left_join(fg.list, by="Code") %>% 
    filter(Time <= maxtimestep) %>% 
    mutate(Year = Time/365) %>% 
    dplyr::select(-IsTurnedOn, -GroupType, -NumCohorts, -Time) 
  
  readr::write_csv(this.output.biomass, paste0(thispath,"/biomass.csv"))
  #  this.output.box.biomass <-  read_delim(here("outputFolder","/AMPS_OUTBoxBiomass.txt")) %>%
  #    gather(Code,biomass, -Time, -Box) %>%
  #    left_join(fg.list, by="Code") %>%
  #    mutate(Year = Time/365) %>% 
  #    filter(Time <= maxtimestep)
  #  
  #  max.year <- this.output.box.biomass %>% pull(Year) %>% max
  #    
  #  this.output.box.biomass.df <-  this.output.box.biomass %>% 
  #    filter(Year == max.year) %>%
  #    dplyr::select(-Code, -Time, -GroupType, -IsTurnedOn, -NumCohorts, -name) %>%
  #    dplyr::rename(id=Box) %>% 
  #    left_join(shape.file.df, by="id")
  #  
  # ggplot() +
  #   geom_polygon(data = this.output.box.biomass.df, aes( x = long, y = lat, group = group, fill=biomass, color=biomass)) +
  #   theme_void() +
  # facet_wrap(~ longname)
  
  nc <- RNetCDF::open.nc(paste0(thispath,"/",this.output.nc))
  nc.data <- RNetCDF::read.nc(nc)
  
  group.atlantis.data <- lapply(vert.groups, get.nc.data, thisncfile = nc) %>% 
    bind_rows()
  
  readr::write_csv(group.atlantis.data, paste0(thispath,"/Nums_ResN_W.csv"))
  
} 

folder.length <- 1:length(folder.paths)

lapply(folder.length, get_outdata)


# Authenticate with Google

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
    
    this.file <- readr::read_csv(paste0(this.folderpath,"/Nums_ResN_W.csv")) %>% 
      dplyr::filter(variable_type=="Nums")
    
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
      dplyr::mutate(scenario=this.scenario, parameter = thisparameter) 

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
     dplyr::mutate(scenario=this.scenario, parameter = thisparameter) 
   
   
  }
  
  return(vital.signs.param) 
  
}

radar.plot.data.biomass <- lapply(no.folder.paths, get_datafile, folder.paths, thisparameter="biomass", runtime, vital.signs, scenario.names) %>% 
  bind_rows()

radar.plot.data.biomass.norm <- radar.plot.data.biomass %>% 
  dplyr::group_by(indicator_short, parameter) %>%
  dplyr::mutate(max_val = max(biomass)) %>% 
  dplyr::mutate(corr_val = biomass/max_val) %>%
  dplyr::ungroup() %>% 
  dplyr::select(-biomass)

radar.plot.data.abundance <- lapply(no.folder.paths, get_datafile, folder.paths, thisparameter="abundance", runtime, vital.signs, scenario.names) %>% 
  dplyr::bind_rows() 

radar.plot.data.abundance.norm <- radar.plot.data.abundance %>% 
  dplyr::group_by(indicator_short, parameter) %>%
  dplyr::mutate(max_val = max(abundance)) %>% 
  dplyr::mutate(corr_val = abundance/max_val) %>%
  dplyr::ungroup()%>% 
  dplyr::select(-abundance)

#  dplyr::group_by(indicator_short, parameter) %>%
#  dplyr::mutate(norm_value=scales::rescale(corr_abun, to=c(0,1))) %>% 
#  dplyr::select(-corr_abun)

#https://docs.google.com/spreadsheets/d/1PxZQwlBadcxI0G-Z7i8DMfrgsqAoWperfA76rXq4yIs/edit#gid=0
#each column should be an indicator, first column are scenarios

make_radarplot <- function(radar.plot.data, thisparameter){
  
  # Color for scenarios
  # See for colors 
  lcols <- c("#caf0f8","#00b4d8","#03045e")
  
 radar.data <- radar.plot.data %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-parameter, -max_val) %>%
  tidyr::pivot_wider(names_from=indicator_short, values_from=corr_val) %>% 
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

ggplot2::ggsave(filename=paste0("radar_",thisparameter,".png"), radar.plot, scale = 1, width = 12, height = 9)

}

sysfonts::font_add_google("Roboto", "roboto")

#add phylopic icon to plot
#beaver_plot + add_phylopic(beaver_pic, alpha = 1, x = 10, y = 37.4, ysize = 10)

make_radarplot(radar.plot.data=radar.plot.data.biomass.norm, thisparameter="biomass")
make_radarplot(radar.plot.data=radar.plot.data.abundance.norm, thisparameter="abundance")