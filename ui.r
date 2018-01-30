library(rhandsontable)
library(shiny)

ui <- shinyUI(fluidPage(
  
  titlePanel("Tonometria"),
  sidebarLayout(
    sidebarPanel(
      helpText("Kolumna 'Time' powinna zawierać czas pomiaru.", 
               "Kolumna 'IOP_Exp' powinna zawierać zmierzone IOP.", 
               "Przycisk 'Recalc' generuje dane w kolumnie 'IOP_bis'"),
      
      wellPanel(
        h3("Opcje Tabeli"),
        radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
      ),
      br(), 
      
      wellPanel(
        h3("Recalc"), 
        actionButton("recalc", "Recalc")
      ),
      
      wellPanel(
        h3("Save"), 
        actionButton("save", "Save table")
      ),    
      
      wellPanel(
        textOutput('result')
      )
      
    ),
    
    mainPanel(
      
      rHandsontableOutput("hot")
      
    )
  )
  
  
))

