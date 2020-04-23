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

# impor tthe python librarys
reticulate::use_condaenv("pdf")

reportlab <- reticulate::import("reportlab")
odf <- reticulate::import("odf")


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
    
    observeEvent(
      eventExpr = input$render, 
      handlerExpr = {
      # pdf  <- reportlab$pdfgen$canvas$Canvas('www/hello.pdf') #pagesize = (595.27,841.89)
      # pdf$drawString(210, 705,'Hello100 World')  
      # pdf$line(200,700,300,700)
      # pdf$save()
      odf_doc <- odf$opendocument$OpenDocumentText()
      
      # define styles of the odf 
      my_style_box <-  odf$style$Style(name = "box1", family = "graphic")
      mx_style_text <- odf$style$Style(name = "text", family = "paragraph")
      mx_style_title <- odf$style$Style(name = "title", family = "paragraph")
      
      my_style_box$addElement(odf$style$GraphicProperties(border = "none"))
      
      mx_style_text$addElement(
        odf$style$TextProperties(fontsize = "12pt", fontsizeasian= "12pt",
                                 fontsizecomplex = "12pt"))
      mx_style_text$addElement(
        odf$style$ParagraphProperties(lineheight = "20pt"))
      
      mx_style_title$addElement(
        odf$style$TextProperties(fontsize = "18pt", fontsizeasian= "18pt", 
                                 fontsizecomplex = "18pt")
        )
      mx_style_title$addElement(
        odf$style$ParagraphProperties(lineheight = "20pt")
      )
      
      # add the styles
      odf_doc$styles$addElement(my_style_box)
      odf_doc$styles$addElement(mx_style_text)
      odf_doc$styles$addElement(mx_style_title)
      
      # define content of the odf
      frame = odf$draw$Frame(height = "5.62cm", width = "5cm", x = "5cm", y = "5cm",
                        stylename = my_style_box)
      
      frame_txt = odf$draw$TextBox()
      text_1     = odf$text$P(text = "Title", stylename = mx_style_title)
      title_1    = odf$text$P(text = "Text",  stylename = mx_style_title)
    
      
      frame_txt$addElement(title_1)
      frame_txt$addElement(text_1)
      frame_txt$addElement(odf$text$P(text = "Text",  stylename = mx_style_title))
      frame_txt$addElement(odf$text$P(text = "Text",  stylename = mx_style_title))
      frame_txt$addElement(odf$text$P(text = "Text",  stylename = mx_style_title))
      frame_txt$addElement(odf$text$P(text = "Text",  stylename = mx_style_title))
      frame_txt$addElement(odf$text$P(text = "Text",  stylename = mx_style_title))
      frame_txt$addElement(odf$text$P(text = "Text",  stylename = mx_style_title))
      frame$addElement(frame_txt)
      
      
      line_1 <- odf$text$P(stylename = "standart") 
      line_1$addElement(
        odf$draw$Line(y1 = "5cm", y2 = "10.62cm", x1 = "4.8cm", x2 = "4.8cm")
      )
      
      # notes
      # lines are +2 font size
      
      
      # add the content  
      
      odf_doc$text$addElement(frame)
      odf_doc$text$addElement(line_1)
      
      
      odf_doc$save('www/hello.odt')
      
      shinyalert(text = "Exported")
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
