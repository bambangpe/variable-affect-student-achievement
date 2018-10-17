#---predict plotly-----
#library(mice) # imputation
library("ggplot2")
library("dplyr")
library("caret")
library("gridExtra")
#library(MASS)
library("leaps")
library("relaimpo")
library("mgcv")
library("randomForest")
library("rpart")
library("rpart.plot")
library("lattice")
library("plotly")
library("shiny")
library("graphics")
library("stats")


#####shiny Application   
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    ")
    ),
    fluidRow(
      column(width = 4, wellPanel(
        radioButtons("plot_type", "The Impact TEST and IP",
                     c("MBTI vs IP","Competitive vs IP", "Ambition vs IP","Pilihan Jurusan VS IP",
                       "Masuk Melalui Jalur vs IP", "Rencana setelah lulus VS IP", 
                       "Prediksi MBTI","Prediksi Competitive Spirit","Prediksi Pilihan Jurusan",
                       "Prediksi Masuk jalur", "Prediksi Rencana Setelah Lulus")
                     
                     
        )),
        helpText( 
          ("NOTE |"),
          ("MBTI vs IP : Variable yang berpengaruh thd IPK |"),
          ("Prediksi MBTI : Besarnya pengaruh Varb MBTI thd IPK dlm % |"),
          ("Limitasi IP >= 3.0 |"),
          ("Table MBTI, table detail dari varb MBTI dll |")
          
        ),
        mainPanel(
          plotlyOutput("trendPlot", width = 500, height = 350)
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("MBTI",
                 dataTableOutput("MBTI")),
        tabPanel("Competitive_Spirit",
                 dataTableOutput("Competitive_Spirit")),
        tabPanel("Pilihan_Jurusan",  
                 dataTableOutput("Pilihan_Jurusan")),
        tabPanel("Masuk_Melalui_Jalur",          
                 dataTableOutput("Masuk_Melalui_Jalur")),
        tabPanel("Rencana_Setelah_Lulus",          
                 dataTableOutput("Rencana_Setelah_Lulus"))
        
      )
    )
    )
  )

