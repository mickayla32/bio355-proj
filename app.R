#### Final Project Shiny App ####


#source("../scripts/source_file.R", local = TRUE)
# Libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(tmap)
library(rsconnect)
library(shinydashboard)
library(lubridate)
library(leaflet)
library(sf)
library(rlang)
library(dataRetrieval)
library(leaflet.extras)
library(devtools)
library(auk)
library(reshape)
library(readr)
library(sp)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(tidyverse)
library(missMDA) # A new library?
library(DT)
library(car)
library(visreg) # A new library, not taught in class
library(broom)
library(piecewiseSEM)

# Data
load("data/ebd_merged_clean_4.RData")
ebd_merged_clean_4$year <- year(ymd(ebd_merged_clean_4$observation_date))
ebd_merged_clean_4$month <- month(ymd(ebd_merged_clean_4$observation_date)) 
ebd_merged_clean_4$day <- day(ymd(ebd_merged_clean_4$observation_date))

ebd_merged_clean_4 <- ebd_merged_clean_4 %>%
  mutate("month_name" = month.abb[month])

ebd_merged_clean_4$month_name <- factor(ebd_merged_clean_4$month_name, levels = c("Jan", "Feb", "Mar", "Apr",
                                                                                  "May", "Jun", "Jul", "Aug", "Sep",
                                                                                  "Oct", "Nov", "Dec"))

## Explore Maps tab map:

# filter ebd
filtered_ebd <- ebd_merged_clean_4 %>%
  
  # each point is 1 rwbb sighting-- pres_abs = 1
  dplyr::filter(pres_abs == 1)

# filter ebd for landcover map, us only
filtered_ebd_us <- ebd_merged_clean_4 %>%
  dplyr::filter(country == "United States",
         pres_abs == 1)


##### MELT DATA AND MAKE REGRESSION MATRIX WITH ENVIRONMENTAL VARIABLES  #####

# make a new dataframe with only columns that you want to include in the matrix
# for this first matrix we are going to use only the interval variables 
dfc<-ebd_merged_clean_4

#check desired column number(s)
names(dfc[c(1:7,9,12,40,41)])

# Create a data frame with only desired variables
dfy<-dfc[c(1:7,9,12,40,41)]

#check that you have the correct columns in your new data frame
names(dfy)

#convert to data.frame
dfy <- as.data.frame(dfy) 

# double check that dfy is data frame 
# note: output should say [1] "data.frame"
class(dfy)

# change variable names
dfy <- dfy %>%
  dplyr::rename(`Human Impact Mean` = hii_mean,
                `Mean Temp` = temp_mean,
                `Min Temp` = temp_min,
                `Max Temp` = temp_max,
                `Precipitation` = precip,
                `Elevation` = elevation)

names(dfy)

#use the melt function to stack data
# input format: output_data <-melt(input_data, id= c("columns you want to preserve/not stack")
melt_dfy<-melt(dfy, id = c("chckls","species_observed", "country", "state_code","pres_abs"))


# check columns
# note: in the output you should see: (a) preserved columns, (b)"variable", and (c) "value"
names(melt_dfy) 

##### MELT DATA AND MAKE REGRESSION MATRIX WITH LANDCOVER TYPES  #####

# make a new dataframe with only columns that you want to include in the matrix
# for this first matrix we are going to use only the interval variables 
lc_df<-ebd_merged_clean_4

#check desired column number(s)
names(lc_df[c(1,9,40,41,44:53, 57:60)])

# Create a data frame with only desired variables
lc_df_2 <- lc_df[c(1,9,40,41,44:53, 57:60)]

#check that you have the correct columns in your new data frame
names(lc_df_2)

#convert to data.frame
lc_df_2 <- as.data.frame(lc_df_2) 

# double check that dfy is data frame 
# note: output should say [1] "data.frame"
class(lc_df_2)

# change variable names
lc_df_2 <- lc_df_2 %>%
  dplyr::rename(`Temperate Needleleaf Forest` = p_temp_needleleaf_forest,
                `Subpolar Taiga Needleleaf Forest` = p_subpolartaiga_needleleaf_forest,
                `Tropical Broadleaf Evergreen Forest` = p_trop_broad_evergreen,
                `Tropical Broadleaf Deciduous Forest` = p_trop_broad_deciduous,
                `Temperate Broadleaf Deciduous Forest` = p_temp_broad_deciduous,
                `Mixed Forest` = p_mixed_forest,
                `Tropical Shrubland` = p_trop_shrubland,
                `Temperate Shrubland` = p_temp_shrubland,
                `Tropical Grassland`= p_trop_grassland,
                `Temperate Grassland`= p_temp_grassland,
                `Wetland`= p_wetland,
                `Cropland`= p_cropland,
                `Barren`= p_barren.x,
                `Urban`= p_urban.x)

names(lc_df_2)

#use the melt function to stack data
# input format: output_data <-melt(input_data, id= c("columns you want to preserve/not stack")
melt_lc_df_2<-melt(lc_df_2, id = c("chckls","species_observed", "country","pres_abs"))


# check columns
# note: in the output you should see: (a) preserved columns, (b)"variable", and (c) "value"
names(melt_lc_df_2) 


################## 
names(ebd_merged_clean_4[c(77:96)])
##### MELT DATA AND MAKE REGRESSION MATRIX WITH ANTHRO LANDCOVER TYPES  #####

# make a new dataframe with only columns that you want to include in the matrix
# for this first matrix we are going to use only the interval variables 
anthro_df<-ebd_merged_clean_4

#check desired column number(s)
names(anthro_df[c(1,9,40,41, 77:96)])

# Create a data frame with only desired variables
anthro_df <- anthro_df[c(1,9,40,41, 77:96)]

#check that you have the correct columns in your new data frame
names(anthro_df)

#convert to data.frame
anthro_df <- as.data.frame(anthro_df) 

# double check that dfy is data frame 
# note: output should say [1] "data.frame"
class(anthro_df)

# change variable names
anthro_df <- anthro_df %>%
  dplyr::rename(`Dense Settlement` = p_dense_settlement,
                `Urban` = p_urban.y,
                `Dense Settlement` = p_dense_settlement,
                `Irrigated Village` = p_irrigated_village,
                `Cropped Pastoral Village` = p_cropped_pastoral_village,
                `Pastoral Village` = p_pastoral_village,
                `Rainfed Village` = p_rainfed_village,
                `Rainfed Mosaic Village` = p_rainfed_mosaic_village,
                `Residential Irrigated Cropland` = p_residential_irrigated_cropland,
                `Residential Rainfed Mosaic`= p_residential_rainfed_mosaic,
                `Populated Irrigated Cropland`= p_populated_irrigated_cropland,
                `Populated Rainfed Cropland`= p_populated_rainfed_cropland,
                `Remote Cropland`= p_remote_cropland,
                `Residential Rangeland`= p_residential_rangeland,
                `Populated Rangeland`= p_populated_rangeland,
                `Remote Rangeland` = p_remote_rangeland,
                `Populated Forest` = p_populated_forest,
                `Remote Forest` = p_remote_forest,
                `Wild Forest` = p_wild_forest,
                `Sparse Trees` = p_sparse_trees,
                `Barren` = p_barren.y)

names(anthro_df)

#use the melt function to stack data
# input format: output_data <-melt(input_data, id= c("columns you want to preserve/not stack")
melt_anthro_df<-melt(anthro_df, id = c("chckls","species_observed", "country","pres_abs"))


# check columns
# note: in the output you should see: (a) preserved columns, (b)"variable", and (c) "value"
names(melt_anthro_df) 


#################### LANDCOVER MODEL ######################
lc_mod <- glm(pres_abs ~ p_temp_needleleaf_forest +
                p_subpolartaiga_needleleaf_forest +
                p_trop_broad_evergreen + 
                p_trop_broad_deciduous +
                p_temp_broad_deciduous +
                p_mixed_forest +
                p_trop_shrubland +
                p_temp_shrubland +
                p_trop_grassland +
                p_temp_grassland +
                p_wetland +
                p_cropland +
                p_barren.x +
                p_urban.x,
              data = ebd_merged_clean_4,
              family = binomial(link = "logit"))

lc_names_long <- c("Temperate Needleleaf Forest",
                   "Subpolar Taiga Needleleaf Forest",
                   "Tropical Broadleaf Evergreen Forest",
                   "Tropical Broadleaf Deciduous Forest",
                   "Temperate Broadleaf Deciduous Forest",
                   "Mixed Forest",
                   "Tropical Shrubland",
                   "Temperate Shrubland",
                   "Tropical Grassland",
                   "Temperate Grassland",
                   "Wetland",
                   "Cropland",
                   "Barren",
                   "Urban")

lc_names <- c("p_temp_needleleaf_forest",
              "p_subpolartaiga_needleleaf_forest",
              "p_trop_broad_evergreen", 
              "p_trop_broad_deciduous",
              "p_temp_broad_deciduous",
              "p_mixed_forest",
              "p_trop_shrubland",
              "p_temp_shrubland",
              "p_trop_grassland",
              "p_temp_grassland",
              "p_wetland",
              "p_cropland",
              "p_barren.x",
              "p_urban.x")

names(lc_names) <- lc_names_long

#### ANOVA TABLE OUTPUT

tidy_lc_mod <- tidy(Anova(lc_mod))

################ ANTHRO LANDCOVER MODEL #################
########### DOING A PCA ON ANTHROPOGENIC LANDCOVER TYPES, SINCE THERE ARE SO MANY ############

# get the relevant columns (variables) from the dataset for PCA
abiome_c <- (ebd_merged_clean_4[77:96]) 

# run PCA()
abiome_pca<- PCA(abiome_c, scale.unit= TRUE, graph= FALSE)

# get variables (provides a list of matrices containing all the results for the active variables) 
#note: must be done before generating corr plots!
var <- get_pca_var(abiome_pca)

#LOOK AT RESULTS: (1) get the eigenvalues of your PCA
eig_val_anth <- get_eigenvalue(abiome_pca)

#LOOK AT THE RESULTS: (2) generate a Scree plot of PCA
#anth_scree <- fviz_eig(abiome_pca, addlabels = TRUE, ylim = c(0, 50))

#LOOK AT THE RESULTS: (3) examine the quality of representation of the variables (i.e., "cos2") 
#corr plot (cos2):
#anth_cos2_corr <- corrplot(var$cos2, is.corr=FALSE)
# graph total cos2 on Dim.1 + Dim.2:
#anth_cos2_dims <- fviz_cos2(abiome_pca, choice = "var", axes = 1:2)

#LOOK AT THE RESULTS: (4) Contributions of variables to Principle Components
#head(var$contrib, 4)
#corr plot
#anth_contrib_corr <- corrplot(var$contrib, is.corr=FALSE)
# Dim.1 contribution graph (red line = expected average contribution)
#anth_contrib_dim1 <- fviz_contrib(abiome_pca, choice = "var", axes = 1, top = 10)
# Dim.2 contribution graph (red line = expected average contribution)
#anth_contrib_dim2 <- fviz_contrib(abiome_pca, choice = "var", axes = 2, top = 10)

#The most important (or, contributing) variables can be highlighted on the correlation plot as follow (PCA without Cos2):
corr_plot_anth <- fviz_pca_var(abiome_pca, col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#LOOK AT THE RESULTS: (6) Dimension description
#res.desc <- dimdesc(abiome_pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
#res.desc$Dim.1  
# Description of dimension 2
#res.desc$Dim.2 

#### Extract PC values and bind to data 

#extract individual values from the PCA
ind_anth <- get_pca_ind(abiome_pca)

#check to make sure you have the right thing 
#head(ind_anth$coord)
#class(ind_anth)

#transform "factoextra" class object into a "data.frame" class object
df_pca_anth <- data.frame(ind_anth$coord)
#class(df_pca_anth)
#^ df_pca_anth is a df of just my individual pc values (dim1-dim5)

#bind the extracted pc scores to dfx, creating a new data frame
ebd_merged_clean_4 <-cbind(ebd_merged_clean_4, df_pca_anth)

#rename columns
names(ebd_merged_clean_4)[names(ebd_merged_clean_4) == 'Dim.1'] <- 'PC1_developed'
names(ebd_merged_clean_4)[names(ebd_merged_clean_4) == 'Dim.2'] <- 'PC2_developed'
names(ebd_merged_clean_4)[names(ebd_merged_clean_4) == 'Dim.3'] <- 'PC3_developed'
names(ebd_merged_clean_4)[names(ebd_merged_clean_4) == 'Dim.4'] <- 'PC4_developed'
names(ebd_merged_clean_4)[names(ebd_merged_clean_4) == 'Dim.5'] <- 'PC5_developed'

#### MODEL WITH PC1 ####

anthro_mod <- glm(pres_abs ~ PC1_developed,
                  data = ebd_merged_clean_4,
                  family = binomial(link = "logit"))

anthro_names <- "PC1_developed"

anthro_names_long <- "Principle Component 1: Anthropogenic Landcover Types"

# anthro_names <- c("p_urban.y",
#                   "p_dense_settlement",
#                   "p_irrigated_village",
#                   "p_cropped_pastoral_village",
#                   "p_pastoral_village",
#                   "p_rainfed_village",
#                   "p_rainfed_mosaic_village",
#                   "p_residential_irrigated_cropland",
#                   "p_residential_rainfed_mosaic",
#                   "p_populated_irrigated_cropland",
#                   "p_populated_rainfed_cropland",
#                   "p_remote_cropland",
#                   "p_residential_rangeland",
#                   "p_populated_rangeland",
#                   "p_remote_rangeland",
#                   "p_populated_forest",
#                   "p_remote_forest",
#                   "p_wild_forest",
#                   "p_sparse_trees",
#                   "p_barren.y")
# 
# anthro_names_long <- c("Urban",
#                        "Dense Settlement",
#                        "Irrigated Village",
#                        "Cropped Pastoral Village",
#                        "Pastoral Village",
#                        "Rainfed Village",
#                        "Rainfed Mosaic Village",
#                        "Residential Irrigated Cropland",
#                        "Residential Rainfed Mosaic",
#                        "Populated Irrigated Cropland",
#                        "Populated Rainfed Cropland",
#                        "Remote Cropland",
#                        "Residential Rangeland",
#                        "Populated Rangeland",
#                        "Remote Rangeland",
#                        "Populated Forest",
#                        "Remote Forest",
#                        "Wild Forest",
#                        "Sparse Trees",
#                        "Barren")

names(anthro_names) <- anthro_names_long
#### ANOVA TABLE OUTPUT

tidy_anthro_mod <- tidy(Anova(anthro_mod))

###################### ENVIRONMENTAL VARIABLE NAMES ######################3

env_mod <- glm(pres_abs ~ hii_mean +
                 temp_mean +
                 temp_min +
                 temp_max +
                 precip +
                 elevation,
               data = ebd_merged_clean_4,
               family = binomial(link = "logit"))

env_names <- c("hii_mean",
               "temp_mean",
               "temp_min",
               "temp_max",
               "precip",
               "elevation")

env_names_long <- c("Human Impact Mean",
                    "Temperature Mean",
                    "Temperature Minimum",
                    "Temperature Maximum",
                    "Precipitation",
                    "Elevation")

names(env_names) <- env_names_long

#### ANOVA TABLE OUTPUT

tidy_env_mod <- tidy(Anova(env_mod))


###########################################################

# The app

# UI
ui <- navbarPage(theme = shinytheme("journal"),
                 "Exploring the Effects of Landcover Type on Red-Winged Blackbirds (Agelaius phoeniceus)",
                 
                 tabPanel("Home", icon = icon("home"),
                          fluidRow(
                            
                            includeMarkdown("scripts/Home_Page.Rmd")
                            
                          )
                 ), # End of Home tab
                 
                 tabPanel("Maps", icon = icon("globe-americas"),
                          
                          # BASIC MAP
                          tabsetPanel(type = "tabs",

                                      tabPanel("Basic Map",
                                               #tabsetPanel(   
                                              
                                                 #tabPanel("Interactive", 
                                                          
                                                          titlePanel("Red-Winged Blackbird Interactive Map: Basic"),
                                                          
                                                          sidebarLayout(
                                                            
                                                            sidebarPanel(
                                                              
                                                              selectInput("yearInput", "Choose a Year",
                                                                          choices = unique(filtered_ebd$year)),
                                                              selectInput("monthInput", "Choose a Month",
                                                                          choice = sort(unique(filtered_ebd$month))),
                                                              selectInput("countryInput", "Choose a Country",
                                                                          choices = sort(unique(filtered_ebd$country)),
                                                                          selected = "United States")
                                                          
                                                            ), # End of sidebar Panel
                                                            
                                                            # mainPanel for Basic Map tab
                                                            mainPanel(
                                                              
                                                              #uiOutput("map_1")
                                                              
                                                              uiOutput("ui1"),
                                                              
                                                              leafletOutput("map_output")
                                                              
                                                            ) # End of mainPanel
                                                            
                                                          ) # End of sidebarLayout
                                                          
                                                # ), # End of tabPanel (Basic, interactive)
                                                 
                                              
                                      ), # End of Basic Map tab
                                      
                                      # LANDCOVER MAP 
                                      tabPanel("Land Cover",
                                               #tabsetPanel(
                                                 
                                                 #tabPanel("Interactive",
                                                          
                                                          titlePanel("Red-Winged Blackbird Interactive Map: Land Cover Type"),
                                                          
                                                          sidebarLayout(
                                                            
                                                            sidebarPanel(
                                                              selectInput("yearInput_2", "Choose a Year",
                                                                          choices = unique(filtered_ebd$year)),
                                                              selectInput("monthInput_2", "Choose a Month",
                                                                          choice = sort(unique(filtered_ebd$month))),
                                                              selectInput("countryInput_2", "Choose a Country",
                                                                          choices = "United States"),
                                                              
                                                              includeMarkdown("scripts/map2_legend.Rmd")
                                                              
                                                            ), # End of Landcover sidebarPanel
                                                            
                                                            mainPanel(
                                                              
                                                              #uiOutput("map_2")
                                                              
                                                              uiOutput("ui2"),
                                                              
                                                              leafletOutput("map_output2"),
                                                              br(),
                                                              br(),
                                                              includeMarkdown("scripts/landcover_text.Rmd")
                                                              
                                                              
                                                            ) # End of mainPanel
                                                            
                                                            
                                                          ) # End of Landcover sidebarLayout
                                                          
                                          
                                      ), # End of LANDCOVER TAB
                                      
                                      # ANTHROPOGENIC BIOMES
                                      tabPanel("Anthropogenic Biomes",
                                                      
                                                          titlePanel("Red-Winged Blackbird Interactive Map: Anthropogenic Biomes"),
                                                          
                                                          sidebarLayout(
                                                            
                                                            sidebarPanel(
                                                              selectInput("yearInput_3", "Choose a Year",
                                                                          choices = unique(filtered_ebd$year)),
                                                              selectInput("monthInput_3", "Choose a Month",
                                                                          choice = sort(unique(filtered_ebd$month))),
                                                              selectInput("countryInput_3", "Choose a Country",
                                                                          choices = sort(unique(filtered_ebd$country)),
                                                                          selected = "United States"),
                                                              
                                                              includeMarkdown("scripts/map3_legend.Rmd")
                                                              
                                                            ), # End of anthro biomes sidebarPanel
                                                            
                                                            mainPanel(
                                                              
                                                              #uiOutput("map_2")
                                                              
                                                              uiOutput("ui3"),
                                                              
                                                              leafletOutput("map_output3"),
                                                              br(),
                                                              br(),
                                                              includeMarkdown("scripts/anthrobiomes_text.Rmd")
                                                              
                                                              
                                                            ) # End of mainPanel
                                                            
                                                            
                                                          ) # End of Anthro biome sidebarLayout
                                                          
                                              
                                      ), # End of ANTHROPOGENIC BIOMES
                                      
                                      
                                      # Agricultural Expansion (2015)
                                      tabPanel("Agriculture Expansion",
                                           
                                                          titlePanel("Red-Winged Blackbird Interactive Map: Agricultural Expansion"),
                                                          
                                                          sidebarLayout(
                                                            
                                                            sidebarPanel(
                                                              selectInput("yearInput_4", "Choose a Year",
                                                                          choices = unique(filtered_ebd$year)),
                                                              selectInput("monthInput_4", "Choose a Month",
                                                                          choice = sort(unique(filtered_ebd$month))),
                                                              selectInput("countryInput_4", "Choose a Country",
                                                                          choices = sort(unique(filtered_ebd$country)),
                                                                          selected = "United States"),
                                                              
                                                              includeMarkdown("scripts/map4_legend.Rmd")
                                                              
                                                            ), # End of AGRICULTURE EXPANSION sidebarPanel
                                                            
                                                            mainPanel(
                                                              
                                                              uiOutput("ui4"),
                                                              
                                                              leafletOutput("map_output4"),
                                                              br(),
                                                              br(),
                                                              includeMarkdown("scripts/agriculture_text.Rmd")
                                                              
                                                              
                                                              
                                                            ) # End of mainPanel
                                                            
                                                            
                                                          ) # End of AGRICULTURE EXPANSION sidebarLayout
                                          
                                      ) # End of 5th maps tab tabPanel- AGRICULTURE EXPANSION
                                      
                                      
                          ) # End of tabset Panel
                 ), # End of Maps tab
                 
                 tabPanel("Plots", icon = icon("chart-area"),
                          tabsetPanel(type = "tabs",
                                      
                                      # PLOT 1 TAB
                                      tabPanel("Histograms",
                                               
                                               titlePanel("Red-Winged Blackbird Sightings Throughout the Year, by Country"),
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(
                                                   selectInput("yearInput_hist_1", "Choose a Year",
                                                               choices = unique(filtered_ebd$year)),
                                                   selectInput("countryInput_hist_1", "Choose a Country",
                                                               choices = sort(unique(filtered_ebd$country)),
                                                               selected = "United States")
                                                   
                                                 ), # End of SidebarPanel  
                                                 
                                                 mainPanel(
                                                   
                                                   plotOutput("hist_1"),
                                                   br(),
                                                   includeMarkdown("scripts/histogram_text.Rmd")
                                                   
                                                 ) # End of mainPanel
                                                 
                                                 
                                               ) # End of sidebarLayout
                                               
                                               
                                      ), # End of Plot 1 Tab
                                      
                                      # PLOT 2 TAB  
                                      tabPanel("Logistic Regressions",
                                               tabsetPanel(type = "tabs",
                                                           
                                                           tabPanel("Environmental Variables",
                                                                    
                                                                    titlePanel("Probability of Occurrence of Red-Winged Blackbirds by Environmental Variables: Raw Data"),
                                                                    
                                                                    sidebarLayout(
                                                                      
                                                                      sidebarPanel(
                                                                        selectInput("variableInput_1", "Choose a Variable",
                                                                                    choices = unique(melt_dfy$variable)),
                                                                        br(),
                                                                        "A note on mean human impact: This data is, as stated on the CEC's website 'based on population density, built-up areas, roads, railroads, navigable rivers, coastlines, land use/land cover, and nighttime lights. HII values range from 0-64, with 0 representing no human influence, and 64 representing the maximum human influence.'",
                                                                        br(),
                                                                        br(),
                                                                        "A note on other environmental variables: These are not in raw values (i.e. actual temperature, precipitation, or elevation values)"
                                                                        
                                                                      ), # End of SidebarPanel  
                                                                      
                                                                      mainPanel(
                                                                        
                                                                        plotOutput("logreg_1")
                                                                        
                                                                      ) # End of mainPanel
                                                                      
                                                                      
                                                                    ) # End of sidebarLayout
                                                                    
                                                           ), # End of tabPanel Env Variables
                                                           
                                                           tabPanel("Landcover Types",
                                                                    
                                                                    titlePanel("Probability of Red-Winged Blackbird Occurrence by Landcover Type: Raw Data"),
                                                                    
                                                                    sidebarLayout(
                                                                      
                                                                      sidebarPanel(
                                                                        selectInput("landcoverInput_1", "Choose a Landcover Type",
                                                                                    choices = unique(melt_lc_df_2$variable))
                                                                        
                                                                      ), # End of sidebarPanel
                                                                      
                                                                      mainPanel(
                                                                        plotOutput("logreg_2")
                                                                        
                                                                      ) # End of mainPanel
                                                                      
                                                                    ) # End of sidebarLayout
                                                                    
                                                           ), # End of Landcover Types tabPanel
                                                           
                                                           tabPanel("Anthropogenic Landcover Types",
                                                                    
                                                                    titlePanel("Probability of Red-Winged Blackbird Occurrence by Anthropogenic Landcover Type: Raw Data"),
                                                                    
                                                                    sidebarLayout(
                                                                      
                                                                      sidebarPanel(
                                                                        selectInput("anthroInput_1", "Choose a Landcover Type",
                                                                                    choices = unique(melt_anthro_df$variable))
                                                                        
                                                                      ), # End of sidebarPanel
                                                                      
                                                                      mainPanel(
                                                                        
                                                                        plotOutput("logreg_3")
                                                                        
                                                                      ) # End of mainPanel
                                                                      
                                                                    ) # End of sidebarLayout
                                                                    
                                                           ) # End of Anthro tabPanel
                                                           
                                               ) # End of tabsetPanel    
                                               
                                      ), # End of Logistic regressions tab
                                      
                                      
                                      # PLOT 3 TAB
                                      tabPanel("Statistics",
                                               tabsetPanel(type = "tabs",
                                                           
                                                           tabPanel("Environmental Variables",
                                                                    titlePanel("Environmental Variables Logistic Regression Data, Fit to a Model"),
                                                                    
                                                                    sidebarLayout(
                                                                      
                                                                      sidebarPanel(
                                                                        selectInput("env_modInput", "Choose an Environmental Variable",
                                                                                    choices = env_names)
                                                                        
                                                                      ), # End of sidebarPanel
                                                                      
                                                                      mainPanel(
                                                                        
                                                                        plotOutput("env_modfit"),
                                                                        br(),
                                                                        "Anova Test Results",
                                                                        br(),
                                                                        DT::dataTableOutput("env_mod_table"),
                                                                        br()
                                                                        #"Pseudo-R-Squared Value",
                                                                        #br(),
                                                                        #DT::dataTableOutput("env_rsq_table")
                                                                        
                                                                        
                                                                      ) # End of mainPanel
                                                                      
                                                                    ) # End of sidebarLayout
                                                                    
                                                           ), # End of tabpanel Env Variables
                                                           
                                                           tabPanel("Landcover Types",
                                                                    titlePanel("Landcover Types Logistic Regression Data, Fit to a Model"),
                                                                    
                                                                    sidebarLayout(
                                                                      
                                                                      sidebarPanel(
                                                                        selectInput("lc_modInput", "Choose a Landcover Type",
                                                                                    choices = lc_names)
                                                                        
                                                                      ), # End of SidebarPanel  
                                                                      
                                                                      mainPanel(
                                                                        
                                                                        plotOutput("lc_modfit"),
                                                                        br(),
                                                                        "Anova Test Results",
                                                                        br(),
                                                                        DT::dataTableOutput("lc_mod_table"),
                                                                        br()
                                                                        #"Pseudo-R-Squared Value",
                                                                        #br(),
                                                                        #DT::dataTableOutput("lc_rsq_table")
                                                                        
                                                                        
                                                                      ) # End of mainPanel
                                                                      
                                                                      
                                                                    ) # End of sidebarLayout
                                                                    
                                                           ), # End of landcover types tabpanel
                                                           
                                                           tabPanel("Anthropogenic Land Cover Types",
                                                                    titlePanel("Anthropogenic Land cover Types, Fit to a Model"),
                                                                    
                                                                    sidebarLayout(
                                                                      
                                                                      sidebarPanel(
                                                                        
                                                                        selectInput("anthro_modInput", "Choose an Anthropogenic Land cover Type",
                                                                                    choices = anthro_names)
                                                                        
                                                                      ), # End of sidebarPanel
                                                                      
                                                                      mainPanel(
                                                                        
                                                                        plotOutput("anthro_modfit"),
                                                                        br(),
                                                                        "Model Test Results",
                                                                        br(),
                                                                        DT::dataTableOutput("anthro_mod_table"),
                                                                        br()
                                                                        #"Pseudo-R-Squared Value",
                                                                        #br(),
                                                                        #DT::dataTableOutput("anthro_rsq_table")
                                                                        
                                                                      ) # End of mainPanel
                                                                      
                                                                    ) # End of sidebarLayout
                                                                    
                                                           ), # end of tabpanel for anthro landcover type stats
                                                           
                                                           tabPanel("Anthropogenic Biomes PCA",
                                                                    titlePanel("Principle Components Analysis: Anthropogenic Land cover Types"),
                                                                    br(),
                                                                    "PCA was done on the anthropogenic land cover types since there are many of them. Rather than use them all as predictors, PC1 (Principle Component 1) of all anthropogenic land cover types was used as the predictor for the logistic regression model.
                                                                    Plot and table may take a moment to load.",
                                                                    
                                                                    mainPanel(
                                                                      
                                                                      plotOutput("anthro_pca_output"),
                                                                      br(),
                                                                      dataTableOutput("pca_table_output")
                                                                      
                                                                    ) # End of mainPanel
                                                                    
                                                           ) # end of tabPanel PCA
                                                           
                                               ) # end of tabsetpanel for stats 
                                               
                                      ) # End of Plot 3 Tab
                                      
                          ) # End of tabsetPanel-- plots tabs
                          
                 ), # End of PLOTS Tab
                 
                 tabPanel("Data", icon = icon("folder-open"),
                          titlePanel("Full Dataset"),
                          
                          mainPanel(
                            
                            DT::dataTableOutput("full_dataset")
                            
                          ) # End of mainPanel
                          
                 ), # End of Data tab
                 tabPanel("About", icon = icon("info"),
                          
                          includeMarkdown("scripts/about_text.Rmd")
                          
                          
                 ) # End of About Tab
                 
                 
) # End of navbarpage



# SERVER
server <- function(session, input, output) {
  
  # Make the filtered data object for the map- BASIC MAP
  filtered_map_data <- reactive({
    filtered_ebd %>%
      dplyr::filter(year == input$yearInput,
             month == input$monthInput,
             #day == input$dayInput,
             country == input$countryInput) %>%
      #state == input$stateInput 
      
      # Make sf?
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
  }) 
  
  # MAP OUTPUT 1- BASIC MAP
  output$map_output <- renderLeaflet({
    
    #  tmap_mode("view")
    
    # NO DATA MESSAGE-   
    validate(
      need(nrow(filtered_map_data()) != 0, 'No data to show. Please select a different month or year.')
    )
    
    # CREATE MAP- BASIC MAP   
    map <- tm_shape(filtered_map_data()) +
      tm_dots(col = "red", size = 0.05, alpha = 0.7) 
    
    
    lf  <- tmap_leaflet(map, mode = "view", show = TRUE, in.shiny = TRUE)
    
    #show leaflet widget
    lf %>% addTiles("https://maps.heigit.org/openmapsurfer/tiles/adminb/webmercator/{z}/{x}/{y}.png")
    
    
  }) # End of output$map_output- BASIC MAP
  
  
  ## LANDCOVER TYPE TAB
  
  # Make the filtered data object for the map- LANDCOVER TYPE
  filtered_map_data_2 <- reactive({
    filtered_ebd %>%
      dplyr::filter(year == input$yearInput_2,
             month == input$monthInput_2,
             #day == input$dayInput,
             country == input$countryInput_2) %>%
      
      # Make sf?
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
  }) 
  
  # MAP OUTPUT 2- LANDCOVER TYPE
  output$map_output2 <- renderLeaflet({
    
    # NO DATA MESSAGE-    
    validate(
      need(nrow(filtered_map_data_2()) != 0, 'No data to show. Please select a different month or year.')
    )
    
    # CREATE MAP- LANDCOVER TYPES    
    tmap_mode("view")
    
    map_2 <- tm_shape(filtered_map_data_2()) +
      tm_dots(col = "black", size = 0.05) 
    
    lf_2  <- tmap_leaflet(map_2, mode = "view", show = TRUE, in.shiny = TRUE)
    
    #show leaflet widget- LANDCOVER TYPE WMS
    lf_2  <- tmap_leaflet(map_2, mode = "view", show = TRUE, in.shiny = TRUE) %>%
      addTiles() %>%
      addWMSTiles("https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2013_Land_Cover_L48/ows?SERVICE=WMS&",
                  layers = "NLCD_2013_Land_Cover_L48",
                  options = WMSTileOptions(format = "image/png", 
                                           transparent = TRUE)) %>%
      addTiles("https://maps.heigit.org/openmapsurfer/tiles/adminb/webmercator/{z}/{x}/{y}.png")
    
    
  }) # End of output$map_output2 - LANDCOVER TYPE

  
  ## ANTHROPOGENIC BIOMES
  
  # Make the filtered data object for the map- ANTHRO BIOMES
  filtered_map_data_3 <- reactive({
    filtered_ebd %>%
      dplyr::filter(year == input$yearInput_3,
             month == input$monthInput_3,
             #day == input$dayInput,
             country == input$countryInput_3) %>%
      
      # Make sf?
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
  }) 
  
  # MAP OUTPUT 3- ANTHRO BIOMES
  output$map_output3 <- renderLeaflet({
    
    # NO DATA MESSAGE-   
    validate(
      need(nrow(filtered_map_data_3()) != 0, 'No data to show. Please select a different month or year.')
    )
    
    # CREATE MAP- ANTHRO BIOMES    
    tmap_mode("view")
    
    map_3 <- tm_shape(filtered_map_data_3()) +
      tm_dots(col = "black", size = 0.05) 
    
    
    lf_3  <- tmap_leaflet(map_3, mode = "view", show = TRUE, in.shiny = TRUE)
    
    #show leaflet widget- ANTHRO BIOMES WMS
    lf_3  <- tmap_leaflet(map_3, mode = "view", show = TRUE, in.shiny = TRUE) %>%
      addTiles() %>%
      addWMSTiles("https://sedac.ciesin.columbia.edu/geoserver/wms",
                  layers = "anthromes:anthromes-anthropogenic-biomes-world-v1",
                  options = WMSTileOptions(format = "image/png", 
                                           transparent = TRUE)) %>%
      addTiles("https://maps.heigit.org/openmapsurfer/tiles/adminb/webmercator/{z}/{x}/{y}.png")
    
    
  }) # End of output$map_output3 (4th map tab)
  
  
  ## AGRICULTURAL EXPANSION
  
  # Make the filtered data object for the map- AGRI EXPANSION
  filtered_map_data_4 <- reactive({
    filtered_ebd %>%
      dplyr::filter(year == input$yearInput_4,
             month == input$monthInput_4,
             #day == input$dayInput,
             country == input$countryInput_4) %>%
      #state == input$stateInput 
      
      # Make sf?
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
  }) 
  
  # MAP OUTPUT 4- AGRICULTURAL EXPANSION
  output$map_output4 <- renderLeaflet({
    
    # NO DATA MESSAGE-     
    validate(
      need(nrow(filtered_map_data_4()) != 0, 'No data to show. Please select a different month or year.')
    )
    
    # CREATE MAP- AGRICULTURAL EXPANSION    
    tmap_mode("view")
    
    map_4 <- tm_shape(filtered_map_data_4()) +
      tm_dots(col = "black", size = 0.05) 
    
    
    lf_4  <- tmap_leaflet(map_4, mode = "view", show = TRUE, in.shiny = TRUE)
    
    #show leaflet widget-- AGRICULTURAL EXPANSION WMS
    lf_4  <- tmap_leaflet(map_4, mode = "view", show = TRUE, in.shiny = TRUE) %>%
      addTiles() %>%
      addWMSTiles("https://sedac.ciesin.columbia.edu/geoserver/wms",
                  layers = "lulc:lulc-development-threat-index_agriculturalexpansion",
                  options = WMSTileOptions(format = "image/png", 
                                           transparent = TRUE)) %>%
      addTiles("https://maps.heigit.org/openmapsurfer/tiles/adminb/webmercator/{z}/{x}/{y}.png")
    
    
  }) # End of output$map_output4 (5th map tab)- AGRICULTURAL EXPANSION
  
  
  
  ######### END OF MAPS ############
  
  ######### BEGINNING OF PLOTS #############
  
  filtered_plot_data_1 <- reactive({
    filtered_ebd %>%
      dplyr::filter(year == input$yearInput_hist_1,
             country == input$countryInput_hist_1)
    
  }) # END OF FILTERED_PLOT_DATA_1 
  
  #### Histogram plot output ####
  output$hist_1 <- renderPlot({
    
    ggplot(data = filtered_plot_data_1(),
           mapping = aes(x = month_name)) +
      geom_histogram(stat = "count", color = "red", fill = "#FFC583") + 
      labs(x = "Month", y = "Number of Sightings") +
      theme_classic()
    
  })
  
  ##########################
  
  #### Regression Plot Environmental Variables ####
  
  filtered_plot_data_2 <- reactive({
    melt_dfy %>%
      dplyr::filter(variable == input$variableInput_1)
    
  })
  
  # Create matrix of regression plots for probability of occurrence by each landcover variable, divided by country
  output$logreg_1 <- renderPlot({
    reg_plot <-ggplot(filtered_plot_data_2(), aes(x=value, y=pres_abs))
    
    reg_plot + geom_point() +
      stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE)+
      scale_x_continuous("Variable Type") +
      scale_y_continuous("Probability of occurrence",limits = c(0.0, 1.0),
                         breaks = c(.0, 0.3, 0.6, 0.9))+
      facet_grid(country~variable) +
      theme_light()
    
  }) # End of renderPlot logreg_1
  
  #### Regression Plot Landcover Variables ####
  
  filtered_plot_data_3 <- reactive({
    melt_lc_df_2 %>%
      dplyr::filter(variable == input$landcoverInput_1)
    
  })
  
  # Create matrix of regression plots for probability of occurrence by each landcover variable, divided by country
  output$logreg_2 <- renderPlot({
    reg_plot <-ggplot(filtered_plot_data_3(), aes(x=value, y=pres_abs))
    
    reg_plot + geom_point() +
      stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE)+
      scale_x_continuous("Variable Type") +
      scale_y_continuous("Probability of occurrence",limits = c(0.0, 1.0),
                         breaks = c(.0, 0.3, 0.6, 0.9))+
      facet_grid(country~variable) +
      theme_light()
    
  }) # End of renderPlot logreg_2
  
  #### Regression Plot ANTHRO Landcover Variables ####
  
  filtered_plot_data_4 <- reactive({
    melt_anthro_df %>%
      dplyr::filter(variable == input$anthroInput_1)
    
  })
  
  # Create matrix of regression plots for probability of occurrence by each landcover variable, divided by country
  output$logreg_3 <- renderPlot({
    reg_plot <-ggplot(filtered_plot_data_4(), aes(x=value, y=pres_abs))
    
    reg_plot + geom_point() +
      stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE)+
      scale_x_continuous("Variable Type") +
      scale_y_continuous("Probability of occurrence",limits = c(0.0, 1.0),
                         breaks = c(.0, 0.3, 0.6, 0.9))+
      facet_grid(country~variable) +
      theme_light()
    
  }) # End of renderPlot logreg_3
  
  ########## STATISTICS TAB (WITHIN PLOTS TAB) #############
  
  #### Plot output landcover types
  output$lc_modfit <- renderPlot({
    
    visreg(lc_mod, gg=TRUE, xvar = input$lc_modInput, scale = "response")
    
  }) # End of lc_modfit renderplot
  
  #### Landcover table output ANOVA
  output$lc_mod_table <- DT::renderDataTable({
    
    tidy_lc_mod %>%
      dplyr::filter(term == input$lc_modInput)  
    
  }) # End of lc_mod_table renderDataTable
  
  #### Landcover table output for pseudo-r-squared
  # rsq_lc_test <- piecewiseSEM::rsquared(lc_mod, method = "mcfadden")
  # 
  # output$lc_rsq_table <- DT::renderDataTable({
  #   
  #   rsq_lc_test
  #   
  # }) # End of env_rsq_table renderDataTable
  
  #### Plot output Anthro landcover types
  
  output$anthro_modfit <- renderPlot({
    
    visreg(anthro_mod, gg=TRUE, xvar = input$anthro_modInput, scale = "response")
    
  }) # End of anthro_modfit renderplot
  
  #### Anthro Table output ANOVA
  output$anthro_mod_table <- DT::renderDataTable({
    
    tidy_anthro_mod %>%
      dplyr::filter(term == input$anthro_modInput)  
    
  }) # End of anthro_mod_table renderDataTable
  
  ### Anthro table output for pseudo-r-squared
  # rsq_anth_test <- piecewiseSEM::rsquared(anthro_mod, method = "mcfadden")
  # 
  # output$anthro_rsq_table <- DT::renderDataTable({
  # 
  #   rsq_anth_test
  # 
  # }) # End of anthro_rsq_table renderDataTable
  
#### Anthro PCA ouput
  
  output$anthro_pca_output <- renderPlot({
    
    corr_plot_anth
  })
  
  output$pca_table_output<- DT::renderDataTable({
    
    eig_val_anth
    
  })
    
  #### Plot output Environmental variables
  
  output$env_modfit <- renderPlot({
    
    visreg(env_mod, gg=TRUE, xvar = input$env_modInput, scale = "response")
    
  }) # End of env_modInput renderPlot
  
  #### Environmental variables table output ANOVA
  
  output$env_mod_table <- DT::renderDataTable({
    
    tidy_env_mod %>%
      dplyr::filter(term == input$env_modInput)  
    
  }) # End of env_mod_table renderDataTable
  
  #### Environmental variables table output for pseudo-r-squared
  # rsq_env_test <- piecewiseSEM::rsquared(env_mod, method = "mcfadden")
  # 
  # output$env_rsq_table <- DT::renderDataTable({
  #   rsq_env_test
  # 
  # }) # End of env_rsq_table renderDataTable
  
  ################## END OF PLOTS ####################
  
  ################ BEGINNING OF DATA TAB OUTPUT ##############
  
  output$full_dataset <- DT::renderDataTable({
    
    ebd_merged_clean_4
    
  })
  
  ################ END OF DATA TAB OUTPUT ##############
  
  
} # End of Server

# RUN 
shinyApp(ui = ui, server = server)
