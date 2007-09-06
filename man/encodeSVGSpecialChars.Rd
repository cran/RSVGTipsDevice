\name{encodeSVGSpecialChars}
\alias{encodeSVGSpecialChars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Encode SVG special chars (ampersand, etc) }
\description{
  Encode SVG special chars (ampersand, etc) as the appropriate XML encoding.
}
\usage{
encodeSVGSpecialChars(x, sub.special = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A vector of character data to be encode. }
  \item{sub.special}{ If \code{FALSE}, do not encode. }
}
\details{
  The following substitutions are made:
  \tabular{rl}{
  \code{\&} \tab \&amp; \cr
  \code{\'} \tab \&apos; \cr
  \code{\"} \tab \&quot; \cr
  \code{\<} \tab \&lt; \cr
  \code{\>} \tab \&gt;
  }

}
\value{
  The vector \code{x} with encodings as appropriate.
}
\author{ Tony Plate \email{tplate@acm.org}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ device }
