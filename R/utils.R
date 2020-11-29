unused_dots <- quanteda:::unused_dots

friendly_class_undefined_message <- quanteda:::friendly_class_undefined_message

message_error <- quanteda:::message_error

#' Check if font is available on the system
#'
#' This function checks if custom font is available to \pkg{ggplot2} and
#' \pkg{graphics} APIs.
#' @param font name of a font to be checked if available on the system.
#' @return character string
#' @keywords internal
check_font <- function(font) {
    if (is.null(font)) {
        font <- ""
    } else {
        msg <- paste0(font, " is not found on your system.")
        if (.Platform$OS.type == "windows") {
            if (!font %in% names(grDevices::windowsFonts()))
                stop(msg, " Run extrafont::font_import() and ",
                     "extrafont::loadfonts(device = \"win\") to use custom fonts.")
        } else {
            if (!font %in% c("sans", "serif", "mono", extrafont::fonts()))
                stop(msg, " Run extrafont::font_import() to use custom fonts.")
        }
    }
    return(font)
}
