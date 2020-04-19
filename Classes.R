library(purrr)

# error handlers -------------
my_error_type_length <- function(obj, name, class, loc_err_msg) {
  loc_err_msg_1 <- paste0(loc_err_msg, name, " must be a ", class)
  if (!(class(obj) == class)) stop(loc_err_msg_1) 
  
  loc_err_msg_2 <- paste0(loc_err_msg, name, " must of length 1")
  if (length(obj) != 1) stop(loc_err_msg_2)  
}

check_slots <- function(class, envir) {
  # get all the vars that are used in the constructor
  slots <- getSlots(class)
  
  # check all input values
  pmap(.l = list(obj   = map(names(slots), .f = get, envir = envir),
                 name  = names(slots), 
                 class = slots),
       .f = my_error_type_length, loc_err_msg = envir$loc_err_msg)
}


# Class: my_text ------
 
setClass("my_text", 
  slots = c(
    x = "numeric", 
    y = "numeric",
    text = "character" 
  )
)



my_text <- function(text, x = 0, y = 0, 
                    loc_err_msg = "\nConstuctor error ('my_text'):\n", ...) {
  # Error Handleing
  check_slots("my_text", envir = environment())
  
  # return constructor
  new("my_text", 
      x = x, 
      y = y, 
      text = text)
}

my_text(c("1"))
# Class: my_line ------

setClass("my_line", 
         slots = c(
           x_left  = "numeric", 
           x_right = "numeric", 
           y       = "numeric",
           draw    = "logical"
         )
)

my_line <- function(x_left = 0, x_right = 0, y = 0, draw = TRUE,
                    loc_err_msg = "\nConstuctor error ('my_line'):\n", ...) {

  # Error Handleing
  check_slots("my_line", envir = environment())
  
  new("my_line", 
      x_left  = x_left, 
      x_right = x_right,
      y       = y, 
      draw    = draw)
}


# Class: my_title_text ------
setClass("my_title_text", 
         slots = c(
           text  = "my_text", 
           title = "my_text",
           line  = "my_line"
         )
)
 
my_title_text <- function(text, title, draw_line = TRUE,
                          x_titel = 0, y_titel = 0,
                          x_line_left = 0, x_line_right = 0, y_line = 0,
                          x_text = 0, y_text = -100, 
                          loc_err_msg = "\nConstuctor error ('my_title_text'):", ...) {
  
  
  loc_err_msg_tmp <- paste0(loc_err_msg, "\nIn TITLE: Constuctor error ('my_text'):\n")
  title = my_text(title, x_titel, y_titel, loc_err_msg_tmp)
  
  loc_err_msg_tmp <- paste0(loc_err_msg, "\nIn TEXT: Constuctor error ('my_text'):\n")
  text  = my_text(text, x_text, y_text, loc_err_msg_tmp)
  
  loc_err_msg_tmp <- paste0(loc_err_msg, "\nIn LINE: Constuctor error ('my_text'):\n")
  line  = my_line(x_left = x_line_left, x_right = x_line_right, y = y_line,
                  draw = draw_line, loc_err_msg_tmp)
  
  new("my_title_text", 
      title = title, 
      text  = text, 
      line  = line)
}


