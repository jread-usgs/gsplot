#' gsplot hovertext
#'
#' Add callout lines and text to a plot.
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{x}} {numeric value for x-coordinate of callout}
#'  \item{\code{y}} {numeric value for y-coordinate of callout}
#'  \item{\code{labels}} {text to be added to callout}
#' }
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#'  
#' @rdname hovertext
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, xlim=c(0,3),ylim=c(0,3),
#'             col="blue", pch=18, hovertext(labels='point'))
#' gsNew
hovertext <- function(object, ...) {
  override("gsplot", "hovertext", object, ...)
}


hovertext.gsplot <- function(object, ..., side=c(1,2)){
  
  set_window_args(object, fun.name='hovertext', ..., legend.name=NULL, side=side, package='gsplot', def.funs=c(graphics::arrows, hovertext.default))
}
#' Default for adding hovertext to a plot.
#' 
#' @param x values for over location
#' @param y values for over location
#' @param labels text to be added to hover
#' @param angle over text angle
#' @param cex sizing for text
#' @param pch from parent call
#' 
#' @rdname hovertext
#' @export
hovertext.default <- function(x, y=NULL, labels=NA, angle='auto', cex=1, pch, ...){
  
  if (is.null(y)) {
    warning("y=NULL not currently supported in hovertext.default")
    return()
  }
  
  if (length(labels) < length(x)){
    labels <- rep(labels, length.out = length(x))
  }
  
  invisible()
  
}