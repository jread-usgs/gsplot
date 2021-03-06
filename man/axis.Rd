% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis.R
\name{axis}
\alias{axis}
\alias{axis.gsplot}
\title{gsplot axis}
\usage{
axis(object, ...)

axis.gsplot(object, ..., n.minor = 0, tcl.minor = NA, reverse = NULL,
  append = FALSE)
}
\arguments{
\item{object}{gsplot object}

\item{\dots}{Further graphical parameters may also be supplied as arguments. See 'Details'.}

\item{n.minor}{number of minor ticks between major ticks}

\item{tcl.minor}{tick length for minor ticks (used if \code{n.minor} > 0). Use NA (the default) to have this 
calculated automatically as half of the value for \code{par('tcl')} at the time of rendering the axis.}

\item{reverse}{flip the orientation of the axis?}

\item{append}{replace or append an existing axis for this side (logical)}
}
\description{
Formats axes for the plotting region.  See \code{\link[graphics]{axis}} for more details.
}
\details{
Additional graphical parameter inputs: 
\itemize{
 \item{\code{side}} {integer indicating the bottom(1), left(2), top(3), or right(4) side of the plot to draw the axis}
 \item{\code{labels}} {logical value indicating whether numerical values are shown at the tickmarks, or a character vector of labels for each tickmark}
 }
}
\examples{
gs <- gsplot() \%>\%
   points(x=1:5, y=1:5, legend.name="Stuff") \%>\%
   lines(2:6, y=2:6, ylim=c(0,10)) \%>\%
   background_color(col="lightgoldenrod") \%>\%
   axis(side=c(3,4),labels=FALSE) \%>\%
   legend("topright")
gs

gs <- gsplot() \%>\%
   points(y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),las=0) \%>\%
   axis(side=c(4), labels=FALSE) \%>\%
   axis(side=c(1,3), n.minor=4)
gs

gs <- gsplot() \%>\%
   points(1:10, 1:10) \%>\%
   axis(1, at = seq(0,10,by=0.1),labels=FALSE, tcl=0.15) \%>\%
   axis(2, reverse=TRUE)
gs

gs <- gsplot() \%>\%
   points(1:5, c(1,10,100,1000,10000), log="y") \%>\%
   axis(side=c(2,4), n.minor=4, las=1)
gs

gs <- gsplot() \%>\%
   lines(1:5, c(1,10,100,1000,10000), log="y", axes=FALSE) \%>\%
   axis(side=c(2,4), labels=FALSE, n.minor=4)
gs

gs <- gsplot(xaxs='r', yaxs='r') \%>\%
   lines(1:5, c(1,10,100,1000,10000), log="y", xaxt='n') \%>\%
   axis(side=c(2,4), labels=FALSE, n.minor=4)
gs

gs <- gsplot() \%>\% 
  points(runif(30, 1,5), runif(30,0.5,3.5)) \%>\% 
  axis(side=1, at=seq(1,5,by=0.25),las=3) \%>\%
  axis(side=c(3,4), labels=FALSE)
gs

usrDef <- gsplot(mar=c(4,4,4,4)) \%>\% 
  points(x=1, y=2, side=c(3,2), cex=3, xlab='cat',log='x') \%>\% 
  points(x=3:10,y=4:11, side=c(1,2), log='y')
usrDef
}
\seealso{
\code{\link[graphics]{axis}}
}
