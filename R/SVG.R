devSVGTips <- function (file = "Rplots.svg", width = 10, height = 8,
                        bg = "white", fg = "black", onefile = TRUE,
                        xmlHeader = FALSE, useStyleAttributes=FALSE,
                        toolTipMode=1, toolTipFontSize=10, toolTipOpacity=1.0,
                        title="R SVG Plot")
{
    dev <- .C("do_SVG", as.character(file),
              as.character(bg), as.character(fg),
              as.double(width), as.double(height),
              as.logical(FALSE), as.logical(xmlHeader),
              as.character(title), as.integer(toolTipMode),
              as.integer(toolTipFontSize), as.double(toolTipOpacity),
              as.logical(onefile), as.logical(useStyleAttributes),
              PACKAGE="RSVGTipsDevice")

      invisible(dev)
}

setSVGShapeContents <- function(contents)
{
    if (names(dev.cur()) == "devSVG")
        .C("SetSvgShapeContents", as.character(contents), PACKAGE="RSVGTipsDevice")
    invisible(NULL)
}

setSVGShapeURL <- function(url)
{
    if (names(dev.cur()) == "devSVG")
        .C("SetSvgShapeURL", as.character(url), PACKAGE="RSVGTipsDevice")
    invisible(NULL)
}

setSVGShapeToolTip <- function(title=NULL, desc=NULL, desc1=desc, desc2=NULL) {
    if (names(dev.cur()) != "devSVG")
        return(invisible(NULL))
    contents <- character(0)
    toolTipMode <- getSVGToolTipMode()
    if (toolTipMode>0) {
        if (!is.null(title))
            contents <- c(contents, paste("<title>", title, "</title>", sep=""))
        if (toolTipMode==1) {
            if (!is.null(desc1))
                contents <- c(contents, paste("<desc>", desc1, "</desc>", sep=""))
        } else {
            if (!is.null(desc1))
                contents <- c(contents, paste("<desc1>", desc1, "</desc1>", sep=""))
            if (!is.null(desc2))
                contents <- c(contents, paste("<desc2>", desc2, "</desc2>", sep=""))
        }
        if (length(contents))
        .C("SetSvgShapeContents", as.character(paste(contents, collapse="\n")), PACKAGE="RSVGTipsDevice")
    }
    invisible(NULL)
}

getSVGToolTipMode <- function()
{
    if (names(dev.cur()) != "devSVG")
        return(-1)
    return(.C("GetSvgToolTipMode", mode=integer(1), PACKAGE="RSVGTipsDevice")$mode)
}

#SvgDevicePoint <- function(x,y)
#{
#       tmp <- .C('GetSvgDevicePoint',as.double(x),as.double(y))
#
#       c(tmp[[1]],tmp[[2]])
#}
#
#SvgUserPoint <- function(x,y)
#{
#       tmp <- .C('GetSvgUserPoint',as.double(x),as.double(y))
#
#       c(tmp[[1]],tmp[[2]])
#}
#
#SvgDeviceBoundry <- function()
#{
#       w <- 0.0
#       h <- 0.0
#       tmp <- .C('GetSvgDeviceBoundry',w,h)
#
#       c(tmp[[1]],tmp[[2]])
#}
#
#SvgDevicePoints <- function(x,y)
#{
#       tmp <- .C('GetSvgDevicePoints',as.double(x),
#                               as.double(y),n=as.integer(length(x)))
#
#       list(x=tmp[[1]],y=tmp[[2]])
#}
#
#MetaSvg <- function(size, box.size, points, values)
#{
#       str <- paste("<MetaSvg width=\"",size[1],"\" height=\"",
#              size[2],"\" rect.width=\"",box.size[1],"\" rect.height=\"",
#              box.size[2],"\">",sep="")
#
#       if(length(points) > 0){
#              buf <- paste("<point x=\"",points$x,"\" y=\"",
#              points$y,"\" value=\"",values,"\"/>",sep="",collapse="\n")
#
#       }
#
#       str <- paste(str,buf,"</MetaSvg>",sep="")
#}










