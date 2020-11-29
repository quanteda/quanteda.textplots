pdf(file = tempfile(".pdf"), width = 10, height = 10)

test_that("test plot.kwic scale argument default", {

    sda <- kwic(texts(data_corpus_inaugural)[[1]], "american")
    sdp <- kwic(texts(data_corpus_inaugural)[[1]], "people")
    mda <- kwic(data_corpus_inaugural, "american")
    mdp <- kwic(data_corpus_inaugural, "people")

    # Single document, should be absolute
    p <- textplot_xray(sda)
    expect_equal(p$labels$x, "Token index")

    # Single document, multiple keywords, should be absolute
    p <- textplot_xray(sda, sdp)
    expect_equal(p$labels$x, "Token index")

    # Multiple documents, should be relative
    p <- textplot_xray(mda)
    expect_equal(p$labels$x, "Relative token index")

    # Multiple documents, multiple keywords, should be relative
    p <- textplot_xray(mda, mdp)
    expect_equal(p$labels$x, "Relative token index")

    # Explicit overrides
    p <- textplot_xray(sda, scale = "absolute")
    expect_equal(p$labels$x, "Token index")
    p <- textplot_xray(sda, sdp, scale = "absolute")
    expect_equal(p$labels$x, "Token index")
    p <- textplot_xray(mda, scale = "absolute")
    expect_equal(p$labels$x, "Token index")
    p <- textplot_xray(mda, mdp, scale = "absolute")
    expect_equal(p$labels$x, "Token index")

    p <- textplot_xray(sda, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")
    p <- textplot_xray(sda, sdp, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")
    p <- textplot_xray(mda, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")
    p <- textplot_xray(mda, mdp, scale = "relative")
    expect_equal(p$labels$x, "Relative token index")


})


test_that("test plot.kwic facet order parameter", {

    p <- textplot_xray(kwic(data_corpus_inaugural, "american"), sort = TRUE)
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    if (identical(plot_docnames, character(0))) {
        plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$docname))
    }
    expect_true(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )
    p <- textplot_xray(kwic(data_corpus_inaugural, "american"),
                       kwic(data_corpus_inaugural, "people"),
                       sort = TRUE)
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    if (identical(plot_docnames, character(0))) {
        plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$docname))
    }
    expect_true(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )

    # Default should be false
    p <- textplot_xray(kwic(data_corpus_inaugural[c(53:54, 1:2)], "american"),
                       kwic(data_corpus_inaugural[c(53:54, 1:2)], "people"))
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    if (identical(plot_docnames, character(0))) {
        plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$docname))
    }
    expect_false(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )

})

# test_that("test plot.kwic keeps order of keywords passed", {
#     p <- textplot_xray(kwic(data_corpus_inaugural, "people"), 
#                        kwic(data_corpus_inaugural, "american"), sort = TRUE)
#     keywords <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$keyword))
#     if (identical(keywords, character(0))) {
#         keywords <- as.character(unique(ggplot2::ggplot_build(p)$layout$layout$keyword))
#     }
#     expect_equal(
#         keywords,
#         c("people", "american")
#     )
# })

test_that("multiple patterns display correctly in textplot_kwic", {
    skip("For interactive visual inspection only")
    toks <- tokens(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))

    kwic_char_f <- kwic(toks, "f", window = 3)
    kwic_char_u <- kwic(toks, "u", window = 3)
    kwic_char_uf <- kwic(toks, c("u", "f"), window = 3)
    kwic_char_fu <- kwic(toks, c("f", "u"), window = 3)
    kwic_dict_u <- kwic(toks, dictionary(list(ukey = "u")), window = 3)
    kwic_dict_f <- kwic(toks, dictionary(list(fkey = "f")), window = 3)
    kwic_dict_uf <- kwic(toks, dictionary(list(ukey = "u", fkey = "f")), window = 3)
    kwic_dict_fu <- kwic(toks, dictionary(list(fkey = "f", ukey = "u")), window = 3)
    kwic_dict_uf_jm <- kwic(toks, dictionary(list(ufkey = c("u", "f"),
                                                  jmkey = c("j", "m"))), window = 3)

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_u, kwic_char_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_u, kwic_dict_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_f, kwic_dict_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_uf, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_fu, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_uf, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_fu, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_uf_jm, scale = "absolute")
})

test_that("phrasal patterns display correctly in textplot_kwic", {
    skip("For interactive visual inspection only")
    toks <- tokens(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))

    kwic_char_bc <- kwic(toks, phrase("b c"), window = 3)
    kwic_dict_bc <- kwic(toks, dictionary(list(bc = "b c")), window = 3)
    kwic_list_bc <- kwic(toks, list(bc = c("b", "c")), window = 3)

    # label OK: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_bc, scale = "absolute")
    # label OK: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_bc, scale = "absolute")
    # label OK: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_list_bc, scale = "absolute")
})

test_that("textplot_xray works with new kwic, one token phrase", {
    data_corpus_inauguralpost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralpost70, "american")
    expect_silent(textplot_xray(knew))
})

test_that("textplot_xray works with new kwic, two token phrase", {
    data_corpus_inauguralpost70 <- corpus_subset(data_corpus_inaugural, Year > 1970)
    knew <- kwic(data_corpus_inauguralpost70, phrase("american people"))
    expect_silent(textplot_xray(knew))
})

dev.off()
