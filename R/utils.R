unused_dots <- quanteda:::unused_dots

friendly_class_undefined_message <- quanteda:::friendly_class_undefined_message

message_error <- quanteda:::message_error

#' Prepare font parameter for plotting
#'
#' This function prepares font parameter for use with \pkg{ggplot2} and
#' \pkg{graphics} APIs. No validation is performed - fonts are passed through
#' and any invalid fonts will be handled by the graphics system.
#' @param font name of a font to be used in plotting.
#' @return character string
#' @keywords internal
check_font <- function(font) {
    if (is.null(font)) {
        font <- ""
    }
    return(font)
}
