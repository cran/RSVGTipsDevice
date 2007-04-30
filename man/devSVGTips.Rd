\name{devSVGTips}
\alias{devSVGTips}
\title{A graphics device for SVG with dynamic tips and hyperlinks}
\usage{
devSVGTips(file = "Rplots.svg", width = 10, height = 8,
       bg = "white", fg = "black", onefile=TRUE,
       xmlHeader=FALSE, useStyleAttributes=FALSE,
       toolTipMode = 1, title = "R SVG Plot")
}
\arguments{
  \item{file}{the file where output will appear.}
  \item{width}{The width of the plot in inches.}
  \item{height}{the height of the plot in inches.}
  \item{bg}{the background color for the plot.}
  \item{fg}{the foreground color for the plot.}
  \item{onefile}{merge plot calls into onefile or separate them to separate pages.}
  \item{xmlHeader}{Print XML header or not (it is recommended to not print a header)}
  \item{useStyleAttributes}{Specify shape attributes in a
    \code{style} attribute or as plain attributes (plain attributes is recommended)}
  \item{toolTipMode}{Mode of toop tips: 0 (no tool tips), 1, or 2 (number of lines of tool tip).}
  \item{title}{Title of plot.}
}
\description{
  This function starts up an SVG graphics device that will write to a
  file.  The SVG shapes have optional tooltips and/or
  hyperlinks.
}
\details{
  This graphics do not appear in any window while they are being drawn
  -- SVG commands are written to a file and can be viewed in an SVG
  viewer after the plot is completed.

  After the plot is completed, the function \code{\link{dev.off}} must
  be called to close the graphics device and flush all unwritten SVG
  commands to the file.
}
\seealso{
  \link[=RSVGTipsDevice.future]{Design and future} of the RSVGTips device.
  \link[=RSVGTipsDevice]{Overview} of the RSVGTips device (many more examples).
  \code{\link{setSVGShapeToolTip}}, \code{\link{setSVGShapeURL}}, \code{\link{getSVGToolTipMode}}, \code{\link{pictex}}, \code{\link{postscript}}, \code{\link{Devices}}.
}
\examples{
library("RSVGTipsDevice")
devSVGTips("svgplot1.svg", toolTipMode=1, title="SVG example plot 1: shapes and points, tooltips are title + 1 line")
plot(c(0,10),c(0,10), type="n", xlab="x", ylab="y", main="Example SVG plot with title+ 1 line tips (mode=1)")
setSVGShapeToolTip(title="A rectangle", desc="that is yellow")
rect(1,1,4,6, col='yellow')
setSVGShapeToolTip(title="1st circle with title only")
points(5.5,7.5,cex=20,pch=19,col='red')
setSVGShapeToolTip(title="A triangle", desc="big and green")
polygon(c(3,6,8), c(3,6,3), col='green')
# no tooltips on these points
points(2:8, 8:2, cex=3, pch=19, col='black')
# tooltips on each these points
invisible(sapply(1:7, function(x) {setSVGShapeToolTip(title=paste("point", x)); points(x+1, 8-x, cex=3, pch=1, col='black')}))
dev.off()
}
\keyword{device}
