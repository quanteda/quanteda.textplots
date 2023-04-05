pdf(file = tempfile(".pdf"), width = 10, height = 10)

test_that("test textplot_scale1d wordfish in the most basic way", {
    skip_if_not_installed("quanteda.textmodels")
    require("quanteda.textmodels")
    wf <- textmodel_wordfish(dfm(tokens(data_corpus_irishbudget2010)), dir = c(6, 5))
    expect_false(identical(textplot_scale1d(wf, sort = TRUE),
                           textplot_scale1d(wf, sort = FALSE)))
    expect_silent(textplot_scale1d(wf, sort = TRUE,
                                   groups = quanteda::docvars(data_corpus_irishbudget2010, "party")))
    expect_silent(textplot_scale1d(wf, sort = FALSE,
                                   groups = quanteda::docvars(data_corpus_irishbudget2010, "party")))

    expect_silent(
        textplot_scale1d(wf, doclabels = apply(quanteda::docvars(data_corpus_irishbudget2010,
                                                       c("name", "party")),
                                               1, paste, collapse = " "))
    )

    p1 <- textplot_scale1d(wf, margin = "features", sort = TRUE)
    p2 <- textplot_scale1d(wf, margin = "features", sort = FALSE)
    p1$plot_env <- NULL
    p2$plot_env <- NULL
    expect_equal(p1, p2, check.environment = FALSE)
})

test_that("test textplot_scale1d wordscores in the most basic way", {
    skip_if_not_installed("quanteda.textmodels")
    mt <- dfm(tokens(data_corpus_irishbudget2010))
    ws <- quanteda.textmodels::textmodel_wordscores(mt, c(rep(NA, 4), -1, 1, rep(NA, 8)))
    pr <- suppressWarnings(predict(ws, mt, force = TRUE))
    
    ca <- quanteda.textmodels::textmodel_ca(mt)

    expect_false(identical(textplot_scale1d(pr, sort = TRUE),
                           textplot_scale1d(pr, sort = FALSE)))
    expect_silent(textplot_scale1d(pr, sort = TRUE,
                                   groups = quanteda::docvars(data_corpus_irishbudget2010, "party")))
    expect_silent(textplot_scale1d(pr, sort = FALSE,
                                   groups = quanteda::docvars(data_corpus_irishbudget2010, "party")))

    expect_silent(textplot_scale1d(pr, doclabels = apply(quanteda::docvars(data_corpus_irishbudget2010,
                                                                 c("name", "party")),
                                                         1, paste, collapse = " ")))

    expect_silent(textplot_scale1d(ca))
    expect_silent(textplot_scale1d(ca, groups = quanteda::docvars(data_corpus_irishbudget2010, "party")))
    
    p1 <- textplot_scale1d(ws, margin = "features", sort = TRUE)
    p2 <- textplot_scale1d(ws, margin = "features", sort = FALSE)
    p1$plot_env <- NULL
    p2$plot_env <- NULL
    expect_equal(p1, p2, check.environment = FALSE)

    expect_error(
        textplot_scale1d(ws, margin = "documents"),
        "This margin can only be run on a predicted wordscores object"
    )
    expect_error(
        suppressWarnings(textplot_scale1d(predict(ws), margin = "features")),
        "This margin can only be run on a fitted wordscores object"
    )
})

dev.off()
