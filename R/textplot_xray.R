#' Plot the dispersion of key word(s)
#'
#' Plots a dispersion or "x-ray" plot of selected word pattern(s) across one or
#' more texts. The format of the plot depends on the number of
#' [kwic][quanteda::kwic] class objects passed: if there is only one document,
#' keywords are plotted one below the other. If there are multiple documents the
#' documents are plotted one below the other, with keywords shown side-by-side.
#' Given that this returns a \pkg{ggplot2} object, you can modify the plot by
#' adding \pkg{ggplot2} layers (see example).
#' @param ... any number of [kwic][quanteda::kwic] class objects
#' @param scale whether to scale the token index axis by absolute position of
#'   the token in the document or by relative position. Defaults are absolute
#'   for single document and relative for multiple documents.
#' @param sort whether to sort the rows of a multiple document plot by document
#'   name
#' @return a \pkg{ggplot2} object
#' @section Known Issues:
#' These are known issues on which we are working to solve in future versions:
#' * `textplot_xray()` will not display the patterns correctly when
#'   these are multi-token sequences.
#' * For dictionaries with keys that have overlapping value matches to tokens in
#'   the text, only the first match will be used in the plot.  The way around this
#'   is to produce one kwic per dictionary key, and send them as a list to
#'   `textplot_xray`.
#' @examples
#' library("quanteda")
#' toks <- data_corpus_inaugural |>
#'   corpus_subset(Year > 1970) |>
#'   tokens()
#' # compare multiple documents
#' textplot_xray(kwic(toks, pattern = "american"))
#' textplot_xray(kwic(toks, pattern = "american"), scale = "absolute")
#'
#' # compare multiple terms across multiple documents
#' textplot_xray(kwic(toks, pattern = "america*"),
#'               kwic(toks, pattern = "people"))
#'
#' \dontrun{
#' # how to modify the ggplot with different options
#' library("ggplot2")
#' tplot <- textplot_xray(kwic(toks, pattern = "american"),
#'                        kwic(toks, pattern = "people"))
#' tplot + aes(color = keyword) + scale_color_manual(values = c('red', 'blue'))
#'
#' # adjust the names of the document names
#' docnames(toks) <- apply(docvars(toks, c("Year", "President")), 1, paste, collapse = ", ")
#' textplot_xray(kwic(toks, pattern = "america*"),
#'               kwic(toks, pattern = "people"))
#' }
#' @export
#' @keywords textplot
textplot_xray <- function(..., scale = c("absolute", "relative"),
                          sort = FALSE) {
    UseMethod("textplot_xray")
}

#' @export
textplot_xray.default <- function(..., scale = c("absolute", "relative"),
                                  sort = FALSE) {
    stop(friendly_class_undefined_message(..., "textplot_xray"))
}

#' @importFrom quanteda is.kwic ntoken
#' @export
textplot_xray.kwic <- function(..., scale = c("absolute", "relative"),
                               sort = FALSE) {
    kwics <- list(...)
    if (!all(vapply(kwics, is.kwic, logical(1))))
        stop("objects to plot must be kwic objects")

    # create a single data.frame from kwics
    x <- do.call(rbind, lapply(kwics, as.data.frame))
    
    # get the vector of ntokens
    ntokensbydoc <- if (is.null(attr(kwics[[1]], "ntoken"))) {
        # if v3
        unlist(lapply(kwics, function(y) ntoken(attr(y, "tokens"))))
    } else {
        # if pre-v3
        unlist(lapply(kwics, attr, "ntoken"))
    }
    x$ntokens <- ntokensbydoc[x$docname]

    # replace "found" keyword with patterned keyword
    x$keyword <- unlist(lapply(kwics, function(l) l[["pattern"]]))


    multiple_documents <- length(unique(x$docname)) > 1

    # Deal with the scale argument:
    # if there is a user-supplied value, use that after passing through
    # match.arg; if not, use relative for multiple documents and absolute
    # for single documents
    if (!missing(scale)) {
        scale <- match.arg(scale)
    }
    else {
        if (multiple_documents) {
            scale <- "relative"
        } else {
            scale <- "absolute"
        }
    }

    # Deal with the sort argument:
    x$docname <- if (sort) {
        factor(x$docname) # levels are sorted by default
    } else {
        factor(x$docname, levels = unique(x$docname))
    }
    
    if (scale == "relative")
        x$from <- x$from / x$ntokens

    from <- ntokens <- NULL
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = from, y = 1)) +
        ggplot2::geom_segment(ggplot2::aes(xend = from, yend = 0)) +
        ggplot2::theme(axis.line = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       panel.spacing = grid::unit(0.1, "lines"),
                       panel.border = ggplot2::element_rect(colour = "gray", fill = NA),
                       strip.text.y = ggplot2::element_text(angle = 0)
        )

    if (scale == "absolute")
        plot <- plot +
          ggplot2::geom_rect(ggplot2::aes(xmin = ntokens, xmax = max(x$ntokens),
                                          ymin = 0, ymax = 1), fill = "gray90")

    if (multiple_documents) {
        # If there is more than one document, put documents on the panel y-axis
        # and keyword(s) on the panel x-axis
        plot <- plot + ggplot2::facet_grid(docname ~ keyword) +
            ggplot2::labs(y = "Document", title = paste("Lexical dispersion plot"))
    }
    else {
        # If not, put keywords on the panel y-axis and the doc name in the title
        plot <- plot + ggplot2::facet_grid(keyword~.) +
            ggplot2::labs(y = "", title = paste("Lexical dispersion plot, document:",
                                                x$docname[[1]]))
    }

    if (scale == "relative") {
        plot <- plot + ggplot2::labs(x = "Relative token index")
    }
    else {
        plot <- plot + ggplot2::labs(x = "Token index")
    }

    plot
}
