\name{setSVGShapeContents}
\alias{setSVGShapeContents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set raw shape contents (XML text) for the next graphics shape drawn with the RSVGTips device.}
\description{
  Set raw shape contents (XML text) for the next graphics shape drawn
  with the RSVGTips device.  This function provides lower-level access
  than \code{\link{setSVGShapeToolTip}}.
}
\usage{
setSVGShapeContents(contents)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contents}{ A character vector containing XML text. }
}
\details{
  This function sets text that will be included in the SVG commands
  output for the next
  graphics shape drawn with \code{\link{RSVGTipsDevice}}.
}
\value{
  Returns an invisible NULL.
}
\author{ Tony Plate \email{tplate@acm.org}}
\seealso{ \code{\link{RSVGTipsDevice}} }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ device }
