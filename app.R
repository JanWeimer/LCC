library(shiny)
library(shinyjs)
library(shinyalert)
library(sortable)

library(methods) # s4 classsystem
library(reticulate)

source("Classes.R")
source("new_object.R")
source("render_objects.R")
source("utils_sortable.R")

reticulate::use_condaenv("pdf")

reportlab <- reticulate::import("reportlab")


ns_sort1 <- NS("my_sort1")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Create your own Resumé"),
    
    
    sidebarLayout(position = "left",
        sidebarPanel = sidebarPanel(
            actionButton("save",       "Save Objects"),
            actionButton("render",     "Render Objects"),
            
            my_sortable_panel(id = "sort1", sort_objs = objects_to_tags(), 
                              title = "Objects"),
            my_sortable_panel(id = "trash", title = "Trash(not recoverble yet)")
            
        ),
        mainPanel = mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel("Objects",
                    #my_sortable_panel(id = "sort2", title = "Doc", "Hi")
                    my_new_sortable_panel(ns_sort1)
                    
                ),
                tabPanel("Layout", uiOutput("pdf_view"))
 
            )
            
        )
    ),
    
    my_sortable_sortable_js(id = "sort1", group = "group", pull = "clone",
                            put = FALSE),
    
    my_sortable_sortable_js(id = "trash", group = "group"),

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    objects <- reactiveValues()
    values  <- reactiveValues()
    
    # call modules
    callModule(my_new_sortable_panel_server, "my_sort1", ns_sort1, 
               objects)
    
    # server functions
    num_obj <- observe({
      
      length(names(objects))
      
      })
    
    output$pdf_view <- renderUI({
        # input$render
  #      tags$iframe(style="height:600px; width:100%", src="hello.pdf")
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
