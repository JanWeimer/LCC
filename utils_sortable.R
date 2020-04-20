objects_to_tags <- function(objects = c("my_title_text", "my_text", "my_line")){

  lapply(
    objects,
    function(my_class) {
      tag(
        "p",
        list(
          class = my_class,
          tags$span(class = "glyphicon glyphicon-move"),
          tags$strong(my_class)
        )
      )
    }
  )
}


my_sortable_panel <- function(id, title, sort_objs = NULL) {
  tags$div(
    class = "panel panel-default",
    tags$div(class = "panel-heading", title),
    tags$div(
      class = "panel-body",
      id = id,
      sort_objs
    )
  )
}

my_sortable_sortable_js <- function(id, group, input_id = NA,
                                    pull = TRUE, put = TRUE) {
  sortable_js(
    id,
    options = sortable_options(
      group = list(
        name = group,
        pull = pull,
        put = put
      ),
      
    onSort =    { 
      if (!is.na(input_id)) sortable_js_capture_input(input_id)
    }
     
    )
  )
}


my_new_sortable_panel <- function(ns){

  tagList(
    uiOutput(ns("doc_obj")),
  )
  
}


my_new_sortable_panel_server <- function(input, output, session, ns,
                                         objects){
  
  docinput_names <- reactive({ input$document[input$document != "insert objects here"] })
  docinput_n     <- reactive({ length(docinput_names()) })
  objects_names  <- reactive({ names(objects)           })
  objects_n      <- reactive({ length(objects_names())  })
  
  # remove object 
  observe({
    
    if (objects_n() > docinput_n()) {
      isolate({
        remove_obj <- setdiff(objects_names(), docinput_names())
        .subset2(objects, "impl")$.values$remove(remove_obj)
        
        
        # this is for bugfixing the subset line does not trigger the reaction
        # reactive value objects_names
        objects$hi <- "hi"
        .subset2(objects, "impl")$.values$remove("hi")
      })
    } 
    
    if (docinput_n() > objects_n()) {
      isolate({
        add_obj <- setdiff(docinput_names(), objects_names())
  
        my_new_object(reactive_values = objects,
                      class =  add_obj, input = input, ns = ns)
  
        .subset2(objects, "impl")$.values$remove(add_obj)
      })
    }
  })

  # Re-Render the sortable panel
  output$doc_obj <- renderUI({
    names <- objects_names()
    
    if (length(names) == 0) names <- "insert objects here"
    
    # the panel
    tags$div(
      my_sortable_panel(id = "sort2", title = "Doc", objects_to_tags(names)),
      my_sortable_sortable_js(id = "sort2",  input_id = ns("document"),
                              group = "group")
    )

  })
}


