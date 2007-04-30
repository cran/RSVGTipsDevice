\name{setSVGShapeToolTip}
\alias{setSVGShapeToolTip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Set the tool tip for the next graphics shape drawn with the RSVGTips device. }
\description{
  Set the tool tip for the next graphics shape drawn with the RSVGTips device.
}
\usage{
setSVGShapeToolTip(title = NULL, desc = NULL, desc1 = desc, desc2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title}{ A title for the tool tip box. (optional)}
  \item{desc}{ The text of the tool tip. }
  \item{desc1}{ The first line of text of the tool tip. }
  \item{desc2}{ The second line of text of the tool tip (used when
  the tool tip mode is 2.) }
}
\details{
  This function sets tool tip text that will be used by the next
  graphics shape drawn with \code{\link{RSVGTipsDevice}}.

  The RSVGTips device "consumes" the tooltip with the first shape that
  it draws, e.g., using \code{rect}, \code{points}, \code{polygon},
  etc.  If multiple objects are drawn (e.g., by supplying a vector to
  \code{points}), only the first shape drawn will have a tooltip.  Note
  that \code{lines} does NOT use a tooltip.
}
\value{
  Returns an invisible NULL.
}
\author{ Tony Plate \email{tplate@acm.org}}
\seealso{ Examples here: \code{\link{RSVGTipsDevice}} }
\examples{
\dontrun{setSVGShapeToolTip(title="A rectangle", desc="that is yellow")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ device }
