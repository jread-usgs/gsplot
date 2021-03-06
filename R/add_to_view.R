
apply_extracted_args <- function(object, extracted.args, side=c(1,2)){
  
  for (j in seq_along(extracted.args)){
    fun.name <- names(extracted.args[j])
    object <- do.call(fun.name, args = append(list('object'=object), extracted.args[[j]])) 
  }
  return(object)
}

add_new_view <- function(object, view.name){
  if (view.name %in% view_names(object))
    stop(view.name, ' already exists, cannot add it.', call. = FALSE)
  view <- list(c())
  names(view) <- view.name
  
  last.view.i <- max(which_views(object), 0)
  object <- append(object, view, after = last.view.i)
  object <- add_new_par(object, field = view.name)
  return(object)
}
#' add function call to view
#' 
#' @param object a gsplot object
#' @param call.args a named list of function calls
#' @param side a numeric vector of side(s) appropriate for creating a view name
#' @param where location to put \code{call.args} (defaults to "last" if missing). 
#' Can be "first" or "last"
#' @return a modified object with the function added to the proper view
#' @keywords internal
add_to_view <- function(object, call.args, side, where){
  if (missing(where)){
    where <- 'last'
  }
  where <- match.arg(where, c('last','first'))
  view.name <- as.view_name(side)
  new.view <- !view.name %in% view_names(object)
  
  if (new.view){
    object <- add_new_view(object, view.name)
  }
  
  after <- switch(where,
                  first = 0,
                  last = length(object[[view.name]]))
  
  object[[view.name]] <- append(object[[view.name]], call.args, after=after)
  return(object)
}

#' extract the call arguments
#' 
#' extract the normal call arguments, and embedded function 
#' call arguments and return as lists named according to their 
#' rendering functions. 
#' 
#' @param fun.name the name of the rendering function
#' @param \dots arguments to \code{fun.name} or an embedded function 
#' within it. 
#' @param custom.config logical to use custom or global config file
#' @return list with arguments. List is named according to function 
#' names. 
#' @examples 
#' gs <- gsplot() %>% 
#'          points(x=1:5, y=1:5, xlim=c(0,10), ylim=c(0,10), 
#'                callouts(labels=c(rep(NA, 4), "oh")), 
#'                error_bar(offset.up=1))
#' gs
#' 
#' gsplot:::filter_arguments('points', x=2:6, y=2:6, ylim=c(-1, 11))$call.args
#' gsplot:::filter_arguments('points', x=1:5, y=1:5, xlim=c(0,10), ylim=c(0,10), 
#'                callouts(labels=c(rep(NA, 4), "oh")))$extracted.args
#' @keywords internal
filter_arguments <- function(fun.name, ..., custom.config = FALSE){
  dots <- separate_args(...)
  
  standard.eval.args <- standard_eval_arguments(dots$args)
  if (is.null(fun.name)){
    function.args <- NULL
  } else {
    function.args <- function_call_args(fun.name, standard.eval.args, custom.config = custom.config)
  }
  
  option.args <- standard.eval.args[!names(standard.eval.args) %in% c("", names(function.args[[1]]))]
  
  extracted.args <- nonstandard_eval_arguments(fun.name, dots$e.fun, parent.args=function.args[[fun.name]])
  
  arguments <- list('call.args' = function.args,
                    'option.args' = option.args,
                    'extracted.args' = extracted.args)
  return(arguments)
}

standard_eval_arguments <- function(.dots){
  args <- NULL
  if (!is.null(.dots)){
    args <- lazy_eval(.dots)
  } 
  return(args)
}

#' get the arguments that go into the function call, stripping out others and adding config defaults
#' 
#' @param fun.name the name of the rendering function
#' @param custom.config logical whether or not to use custom.config or global config defaults
#' @param .dots lazy_dots arguments
#' @keywords internal
function_call_args <- function(fun.name, all.args, custom.config=FALSE){

  fun.defaults <- function_defaults(fun.name)

  args <- set_args(fun.name, all.args, custom.config = custom.config, package=fun.defaults$package)
  call.args <- list(formal_arguments(args, fun.defaults$def.funs, 
                                     keep.names = names(config(fun.name, custom.config = custom.config))))
  names(call.args) <- fun.name
  return(call.args)
}

#' get the embedded arguments that go into the function call
#' 
#' @param fun.name the name of the rendering function
#' @param embedded.dots expressions to be evaluated within \code{parent.args} data
#' @param parent.args data that should be accessible when evaluating \code{embedded.dots}
#' @keywords internal
nonstandard_eval_arguments <- function(fun.name, embedded.dots, parent.args){

  args <- list()
  if (!is.null(embedded.dots)){
    for (i in seq_len(length(embedded.dots))){
      fun.name = names(embedded.dots)[i]
      package <- function_defaults(fun.name, out='package')
      embed.args = list(append(embedded.dots[[i]], function_args(package, fun.name, parent.args, drop=TRUE)))
      names(embed.args) <- fun.name
      args <- append(args, embed.args)
    }
  }
  return(args)
}