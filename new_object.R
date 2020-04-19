
new_object <- function(id, reactive_values, name){
  ns <- NS(id)
  
  callModule(new_object_server, id,
             id = id, reactive_values = reactive_values, name = name)
  
  showModal(modalDialog(
    useShinyjs(),
    useShinyalert(),
    
    actionButton(ns("my_title_text"),"Text + Title",icon = icon("align-justify")),
    actionButton(ns("my_text"),"Text",icon = icon("align-justify")),
    actionButton(ns("ok"),"OK"),
    
    
    hidden(
      textInput(ns("title"), "Title"),
      textInput(ns("text"), "Text")
    )
    
  ))
  a
}

new_object_server <- function(input, output, session, 
                              reactive_values, id, name) {
  
  values <- reactiveValues()
  values$class <- NA
  
  
  observeEvent(input$my_text, {
    values$class  <- "my_text"
    show("text")
    hide("title")
  })
  
  observeEvent(input$my_title_text, {
    values$class  <- "my_title_text"
    show("title")
    show("text")
  })
  
  observeEvent(input$ok, {
    if (is.na(values$class)) {
      shinyalert(text  = "Create an object first")
      return()
    } 
    
    object <- eval(call(values$class, text = input$text, title  = input$title))
    
    reactive_values[[name]] <- append(reactive_values[[name]], object)
    
    shinyalert(text  = "Object created")
    removeModal()

  })
}
