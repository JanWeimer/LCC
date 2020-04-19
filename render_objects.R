setGeneric("my_render", function(object, pdf) {
  standardGeneric("my_render")
})


setMethod("my_render", signature("my_text"), function(object, pdf) {
  pdf$drawString(object@x, object@y, object@text)  
})


setMethod("my_render", signature("my_line"), function(object, pdf) {
  pdf$line(object@x_left, object@y, object@x_right, object@y)
})

setMethod("my_render", signature("my_text"), function(object, pdf) {
  my_render(object@text)
  my_render(object@title)
  my_render(object@line)
})

# pdf  <- reportlab$pdfgen$canvas$Canvas('www/hello.pdf') #pagesize = (595.27,841.89)
# 
# my_render(my_line(10, 100, 100), pdf)
# pdf$save()


