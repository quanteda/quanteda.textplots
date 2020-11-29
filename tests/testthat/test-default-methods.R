test_that("test default textplot methods", {
    expect_error(
        textplot_keyness(TRUE),
        "textplot_keyness\\(\\) only works on keyness objects"
    )
    expect_error(
        textplot_wordcloud(TRUE),
        "textplot_wordcloud\\(\\) only works on dfm, keyness objects"
    )
    expect_error(
        textplot_xray(TRUE),
        "textplot_xray\\(\\) only works on kwic objects"
    )
    expect_error(
        textplot_scale1d(TRUE),
        "textplot_scale1d\\(\\) only works on predict.textmodel_wordscores, textmodel_ca, textmodel_wordfish, textmodel_wordscores objects"
    )
})
