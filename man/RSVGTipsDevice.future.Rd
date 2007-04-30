\name{RSVGTipsDevice.future}
\alias{RSVGTipsDevice.future}
\docType{package}
\title{Design and future of the SVG Graphics Driver with dynamic tips}
\description{
  The design of this driver is that it can be used with ordinary R
  plotting commands.  The extra information needed for tooltips and
  hyperlinks is supplied by a call to a separate function call before
  the call to the plotting function that generates the shape.
  Extra information for only one shape can be supplied this way, so
  shapes that have tooltips or hyperlinks must be drawn one at a time.
}
\section{SVG coding style}{
  It appears that preferred SVG coding styles have changed over time.
  The defaults in this driver are those suggested at
  http://jwatt.org/svg/authoring/.

  In particular:
  \itemize{
    \item The driver output by default does NOT include a DOCTYPE DTD (it is
    recommended at http://jwatt.org/svg/authoring/ to NOT include a
    DOCTYPE declaration.)
    \item Use shape properties directly rather than specifying them via
    style property, e.g., prefer this:
    \code{<circle fill="red" stroke="blue" ... />} over this: \code{<circle style="fill:red; stroke:blue;" ... />}.
  }
}
\section{Limitations}{
  \itemize{
    \item
    This driver currently does not have any font metric information,
    so the use of \code{\link{plotmath}} is not supported.
    \item
    This device will not successfully record tool tips and hyperlinks if
    it is called by \code{dev.copy} - the functions to add tool tips and
    hyperlinks are ignored by other devices.
  }
}
\section{Future development possibilities}{
  \itemize{
    \item Add font metric information to support the use of \code{\link{plotmath}}.
    \item Clean up the two-line tooltip SVG code:
    The two-line tooltip mode displays OK in the default SVG renderers in Firefox (under Windows
    2000/XP and Linux) but does not display in the Batik standalone SVG
    viewer.  It probably needs declarations to extend XML elements:
    http://www.w3.org/TR/SVG/extend.html
    23.5 Adding private elements and attributes to the DTD.
    \item Provide post-processing to use CSS (cascading style sheets) to make SVG files with
    lots of elements more compact.
  }
}
\references{
  www Consortum
  \emph{W3C Scalable Vector Graphics (SVG)}
  http://www.w3.org/Graphics/SVG/Overview.htm8
  
  SVG authoring guidelines http://jwatt.org/svg/authoring

  "SVG Essentials", J. David Eisenberg, O'Reilly and Associates, 2002.
}
\seealso{
  \link[=RSVGTipsDevice]{Overview} of the RSVGTips device.

  \code{\link{pictex}}, \code{\link{postscript}}, \code{\link{Devices}}.
  
}
\keyword{ package }
