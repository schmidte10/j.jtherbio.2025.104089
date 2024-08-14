#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#--- import libraries ---#
library(readr)
library(tidyverse)
library(chron)
library(lubridate)
#library(shiny)
library(ggpubr)
library(shinythemes)
library(DT)
library(Rcpp)
#--- set directory ---# 
#setwd("C:/Users/jc527762/OneDrive - James Cook University/PhD dissertation/Data/Chapter5_Enzymes/")

#--- import data ---#
#WhiteMuscleAll <- read_delim("import_files/Lauren_CS_filtered.txt", 
                             #delim = "\t", escape_double = FALSE, 
                             #col_types = cols(`creation_time` = col_datetime(format = "%d/%m/%Y %H:%M:%S %p")), 
                             #trim_ws = TRUE)

WhiteMuscleAll <- read_delim("Lauren_CS_filtered.txt", 
                             delim = "\t", escape_double = FALSE, 
                             col_types = cols(`creation_time` = col_datetime(format = "%d/%m/%Y %H:%M:%S %p")), 
                             trim_ws = TRUE)

#--- separate sample column into useful data ---# 
WhiteMuscleAll2 <-  
    WhiteMuscleAll %>% 
    separate(sample, c("MUSCLE_TYPE", "SPECIES", "SAMPLE_NO", "TEMPERATURE"), sep="_", remove = FALSE)  %>%
    unite("UNIQUE_SAMPLE_ID", c(MUSCLE_TYPE,SPECIES,SAMPLE_NO,TEMPERATURE), sep = "_", remove = FALSE) %>% 
    mutate(case_when((SPECIES == "Paus") ~ "Pomacentrus australis", 
                     (SPECIES == "Apoly") ~ "Acanthochromis polyacanthus",
                     (SPECIES == "Pamo") ~ "Pomacentrus amoinensis",
                     (SPECIES == "Pcoel") ~ "Pomacentrus coelestis",
                     (SPECIES == "Pmol") ~ "Pomacentrus moluccensis",
                     (SPECIES == "Adoed") ~ "Apogon doederlein",
                     (SPECIES == "Arub") ~ "Apogon rubrimacula",
                     TRUE ~ "unknown")) 

WhiteMuscleAll3 <- WhiteMuscleAll2 %>% 
    mutate(CUVETTE = as.character(CUVETTE))

#### ---- all points ---####
#--- import data ---#
#WhiteMuscleAll.original <- read_delim("./import_files/Lauren_CS_processed.txt", 
                                      #delim = "\t", escape_double = FALSE, 
                                      #col_types = cols(`creation_time` = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
                                      #trim_ws = TRUE) 

WhiteMuscleAll.original <- read_delim("Lauren_CS_processed.txt", 
                                      delim = "\t", escape_double = FALSE, 
                                      col_types = cols(`creation_time` = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
                                      trim_ws = TRUE) 

#--- separate sample column into useful data ---# 
WhiteMuscleAll2.original <-  
  WhiteMuscleAll.original %>% 
  separate(sample, c("MUSCLE_TYPE", "SPECIES", "SAMPLE_NO", "TEMPERATURE"), sep="_", remove = FALSE)  %>%
  unite("UNIQUE_SAMPLE_ID", c(MUSCLE_TYPE,SPECIES,SAMPLE_NO,TEMPERATURE), sep = "_", remove = FALSE) %>% 
  mutate(case_when((SPECIES == "Paus") ~ "Pomacentrus australis", 
                   (SPECIES == "Apoly") ~ "Acanthochromis polyacanthus",
                   (SPECIES == "Pamo") ~ "Pomacentrus amoinensis",
                   (SPECIES == "Pcoel") ~ "Pomacentrus coelestis",
                   (SPECIES == "Pmol") ~ "Pomacentrus moluccensis",
                   (SPECIES == "Adoed") ~ "Apogon doederlein",
                   (SPECIES == "Arub") ~ "Apogon rubrimacula",
                   TRUE ~ "unknown")) 

WhiteMuscleAll3.original <- WhiteMuscleAll2.original %>% 
  mutate(CUVETTE = as.character(CUVETTE))
#####

# Define UI for application that draws a histogram
ui <- fluidPage(
  #tags$head(
    #tags$style(type="text/css", "select { max-width: 240px; }"),
    #tags$style(type="text/css", ".span4 { max-width: 290px; }"),
    #tags$style(type="text/css", ".well { max-width: 280px; }")
  #),
    # Application title
  navbarPage("Enzyme Quailty Checks", theme = shinytheme("yeti"),
             tabPanel("White Muscle Data (Citrate synthase)", fluid = TRUE, icon = icon("fish"), 
                      
  

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            div(style="display:inline-block;",radioButtons(
                "TEMPERATURE",
                        "Temperature C",
                        c("10" = "10", 
                          "20" = "20",
                          "30" = "30",
                          "40" = "40",
                          "50" = "50"))),  
            #radioButtons("MUSCLE_TYPE",
                                                           #"Tissue type:",
                                                           #c("WM" = "White Muscle", 
                                                            # "HRT" = "Heart",
                                                            # "LVR" = "Liver",
                                                            # "GILLS" = "Gilss")),
            selectInput("SPECIES", "species:", 
                        c("Pomacentrus australis" = "Paus",
                          #"Acanthochromis polyacanthus"= "Apoly",
                          #"Pomacentrus amoinensis"= "Pamo",
                          #"Pomacentrus coelestis"= "Pcoel",
                          "Pomacentrus moluccensis"= "Pmol",
                          "Apogon doederlein"= "Adoed",
                          "Apogon rubrimacula"= "Arub"
                          )),
            selectInput("SAMPLE_NO", "Replicate:", 
                        c("1"="01", 
                          "2"="02",
                          "3"="03",
                          "4"="04",
                          "5"="05",
                          "6"="06",
                          "7"="07",
                          "8"="08",
                          "9"="09",
                          "10"="10"))
            ), 
        
          
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"), 
           DT::dataTableOutput("mytable")) 
        )
    ))
  )
    


# Define server logic required to draw a histogram
server <- function(input, output, session) { 
  
  WhiteMuscleAll3_Finder <- reactive({ 
    req(input$TEMPERATURE)  
    req(input$SPECIES)
    filter(WhiteMuscleAll3, TEMPERATURE %in% input$TEMPERATURE, 
           SPECIES %in% input$SPECIES, 
           SAMPLE_NO %in% input$SAMPLE_NO)}) 
  
  WhiteMuscleAll3.original_Finder <- reactive({ 
    req(input$TEMPERATURE)  
    req(input$SPECIES)
    filter(WhiteMuscleAll3.original, TEMPERATURE %in% input$TEMPERATURE, 
           SPECIES %in% input$SPECIES, 
           SAMPLE_NO %in% input$SAMPLE_NO)})
  
  
  
  output$plot <- renderPlot({ 
    input$TEMPERATURE
    input$SPECIES 
    input$SAMPLE_NO
    isolate({   
      ggplot(WhiteMuscleAll3_Finder(),aes(MINUTES, result)) + 
        geom_point(data = WhiteMuscleAll3.original_Finder(), mapping = aes(MINUTES, result), color = "red") +
        geom_point(color = "blue") +
        facet_wrap(~CUVETTE) + 
        geom_smooth(method = "lm") + 
        theme_bw() + 
        #ylim(-0.3,3)+
        ggtitle(paste(WhiteMuscleAll3_Finder()[1,8])) + 
        stat_regline_equation(label.y = 0.7) + 
        stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
                 label.y = 0.4)})
  }, height = 420, width = 700)  
    
    output$mytable = DT::renderDataTable({ 
      input$TEMPERATURE 
      input$SPECIES 
      input$SAMPLE_NO
      
      CS_activity <- WhiteMuscleAll3_Finder() %>% 
        group_by(UNIQUE_SAMPLE_ID, CUVETTE) %>% 
        do({
          mod = lm(result ~ MINUTES, data = .)
          data.frame(Intercept = coef(mod)[1],
                     Slope = coef(mod)[2], 
                     r2 = summary(mod)$r.squared)
        }) %>%
        ungroup() %>%
        datatable() %>%
        formatStyle('CUVETTE', target = "row",
                    backgroundColor = styleEqual(c("1","2","3"), c('lightblue','lightblue','lightblue'))) %>% 
        formatStyle('CUVETTE', target = "row",
                    backgroundColor = styleEqual(c("5"), c('springgreen')))
      
      
      
    }) 

}
# Run the application 
shinyApp(ui = ui, server = server)
