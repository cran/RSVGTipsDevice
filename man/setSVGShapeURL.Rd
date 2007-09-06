\name{setSVGShapeURL}
\alias{setSVGShapeURL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set raw shape URL (XML text) for the next graphics shape drawn with the RSVGTips device.}
\description{
  Set a URL to use as a hyperlink for the next graphics shape drawn
  with the RSVGTips device.
}
\usage{
setSVGShapeURL(url)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{ A full valid URL, e.g., \code{"http://www.r-project.org"}}
}
\details{
  This function sets a hyperlink that will be included in the SVG commands
  output for the next
  graphics shape drawn with \code{\link{RSVGTipsDevice}}.

  Note that the URL is used as-is, with no substitutions made for
  special characters.  Consequently, it is possible to produce an
  invalid XML file by supplying a URL with special characters (i.e.,
  "<", ">", "&", and single- and double-quotes.)
}
\value{
  Returns an invisible NULL.
}
\author{ Tony Plate \email{tplate@acm.org}}
\seealso{ \code{\link{RSVGTipsDevice}} }
\examples{
\dontrun{setSVGShapeURL("http://www.r-project.org")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ device }
