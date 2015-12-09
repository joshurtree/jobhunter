#Create a vector that is stored at the path specified
persistentVector <- function(path, default = NULL) {
  values <- NULL
  if (file.exists(path))
    load(path)
  else
    values <- default
  
  update <- function(newValues) {
    values <<- newValues
    save("values", file=path)
    values
  } 
  
  obj <- list(
    add = function(value) {
      update(c(values, value))
    },
    
    remove = function(value) {
      if (!is.null(value))
        update(values[values != value])
      else
        values
    },
    
    contains = function(value) {
      if (!is.null(value))
        value %in% values
      else
        FALSE
    },
    
    get.values = function() {
      values
    }
  )
  
  class(obj) <- append(class(obj), "persistentVector")
  obj
}
