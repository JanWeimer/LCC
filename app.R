library(shiny)
library(shinyjs)
library(shinyalert)

library(methods) # s4 classsystem
library(reticulate)

source("Classes.R")
source("new_object.R")

reticulate::use_condaenv("pdf")

reportlab <- reticulate::import("reportlab")



# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Create your own Resumé"),

    sidebarLayout(position = "left",
        sidebarPanel = sidebarPanel(
            actionButton("new_object", "Create New Object"),
            actionButton("save",       "Save Objects"),
            actionButton("render",     "Render Objects")
        ),
        
        mainPanel = mainPanel(
            tabsetPanel(
                tabPanel("Layout", uiOutput("pdf_view")),
                tabPanel("Objects")
            )
            
        )
    )

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    objects <- reactiveValues()
    
    
    
    num_obj <- reactive({length(names(objects))})
    
    output$pdf_view <- renderUI({
        input$render
  #      tags$iframe(style="height:600px; width:100%", src="hello.pdf")
    })
    
    observeEvent(eventExpr = input$new_object, 
                 handlerExpr = {
                     new_object(id = "1", reactive_values = objects,
                                name = paste0("obj-",  num_obj())) 
                     })
    
    observeEvent(eventExpr = input$save, 
                 handlerExpr = {
                     save <- reactiveValuesToList(objects)
                     saveRDS(save, "save")
                     shinyalert(text = "Values Saved")
                 })
    
    observeEvent(eventExpr = input$render, 
                 handlerExpr = {
                     pdf  <- reportlab$pdfgen$canvas$Canvas('www/hello.pdf') #pagesize = (595.27,841.89)
                     pdf$drawString(210, 705,'Hello100 World')  
                     pdf$line(200,700,300,700)
                     pdf$save()
                     
                     
                    shinyalert(text = "Exported")
                 })
}


# Run the application 
shinyApp(ui = ui, server = server)
