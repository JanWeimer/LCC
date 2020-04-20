

# dow not use the ui element ids in ns used in this moduel
my_new_object <- function(reactive_values, class, input, ns){
  callModule(module = my_new_object_server, id = "whatever", 
             reactive_values = reactive_values, 
             class = class, input2 = input)
  
  title <- textInput(ns("title"), "Title")
  text  <- textAreaInput(ns("text"), "Text")
  
  
  showModal(modalDialog(
    useShinyjs(),
    useShinyalert(),
    textInput(ns("name"), "Name"),
    
    if (class == "my_text") {
      text
    } else if (class == "my_title_text") {
      tagList(title, text)
    },
    footer = tagList(
      actionButton(ns("create"), "Create"),
      modalButton("Abort"))
  ))
}

my_new_object_server <- function(input, output, session,  
                              reactive_values, class, input2) {
  reset <- input2$create == 0
  
  
  observeEvent(input2$create, {

    if (length(reset) == 1) {
      if (!reset) return()
    }
    
    
    
    name   <- input2$name
    
    invalid_names <- c("insert objects here", "my_text", 
                       "my_line", "my_title_text", "")
    
    if (name %in% invalid_names) {
      shinyalert(text  = "Invalid name")
      return()
    }
    
    if (name %in% input2$document) {
      shinyalert(text  = "Name already exists")
      return()
    }
    
    object <- isolate({
      eval(call(class, text = input2$text, title  = input2$title))
    })

    #add the new object
    reactive_values[[name]]  <- object
    
    shinyalert(text  = "Object created")
  
    removeModal()

  })
  
}
