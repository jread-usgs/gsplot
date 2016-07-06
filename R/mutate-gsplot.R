
#' set lim for gsplot
#' 
#' set the lim for views in gsplot object
#' 
#' @param object a gsplot object
#' @param value the new limits to use (vector), or a named list
#' @param side which side(s) to use (NULL when value is a list)
#' 
#' @examples 
#' myplot <- gsplot() %>% 
#'   points(1:3, 2:4) %>%
#'   lines(1:5, 5:1)
#' # numeric side:
#' lim(myplot, side=1)<-c(1,4)
#' 
#' # named side:
#' lim(myplot, 'side.1')<-c(1.1,3.9)
#' lim(myplot, 1)
#' 
#' # named list
#' lim(myplot) <- list('side.1' = c(1.1,3.9))
#' 
#' myplot
#' @export
`lim<-` <- function(object, value, side) UseMethod('lim<-', object)

#' @export
`lim<-.gsplot` <- function(object, value, side=NULL){
  
  if (is(value, 'list')){
    object <- `lim<-.gsplot_list`(object, value, side)
  } else {
    if (is.null(side)){
      stop('side must be specified unless using a list', call. = FALSE)
    } else if (is(side,'numeric')){
      side.name <- as.side_name(side)
    } else if (is(side,'character')){
      side.name <- side
    } else {
      stop('class', class(side), 'for side is not supported')
    }
    object <- `lim<-.gsplot_named_side`(object, value, side.name)
  } 
  return(object)
}

`lim<-.gsplot_list` <- function(object, value, side){
  if (!is.null(side)){
    stop('cannot specify side when using a list as input', call. = FALSE)
  }
  side.names <- names(value)
  for (side.name in side.names){
    object <- `lim<-.gsplot_named_side`(object, value=value[[side.name]], side.name)
  }
  return(object)
}

`lim<-.gsplot_named_side` <- function(object, value, side.name){
  stopifnot(length(side.name) == 1, is(side.name,'character'), side.name %in% side_names(object))
  # // hack to deal w/ lim(myplot, side=1)[1] <- 0.25 case w usr.lim setting
  set.i <- !is.na(value) & lim(object, side=side.name) != value
  object[[side.name]]$lim[set.i] <- value[set.i]
  object[[side.name]]$usr.lim <- set.i
  return(object)
}

#' @export
print.gsplot_limit <- function(x, ...) {
  class(x) <- class(x)[!class(x) %in% 'gsplot_limit']
  NextMethod(generic='print', object=x, ...)
}

#' @export
`[<-.gsplot_limit` <- function(x, i, j, value){
  # do something special here?
  NextMethod()
}

