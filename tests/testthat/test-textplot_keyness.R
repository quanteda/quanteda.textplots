library("quanteda")

pdf(file = tempfile(".pdf"), width = 10, height = 10)

test_that("test textplot_keyness: show_reference works correctly ", {
    skip("until quanteda.textstats is updated")
    presdfm <- corpus_subset(data_corpus_inaugural, President %in% c("Obama", "Trump")) |>
        tokens(remove_punct = TRUE) |>
        tokens_remove(stopwords("en")) |>
        dfm()
    presdfm <- dfm_group(presdfm, groups = presdfm$President)
    result <- quanteda.textstats::textstat_keyness(presdfm, target = "Trump")

    k <- 10
    p1 <- textplot_keyness(result, show_reference = FALSE, n = k)
    p2 <- textplot_keyness(result, show_reference = TRUE, n = k)

    # raises error when min_count is too high
    expect_error(textplot_keyness(result, show_reference = FALSE, min_count = 100),
                 "Too few words in the documents")
    # plot with two different fills when show_reference = TRUE
    expect_equal(dim(table(ggplot2::ggplot_build(p1)$data[[1]]$colour)), 1)
    expect_equal(dim(table(ggplot2::ggplot_build(p2)$data[[1]]$colour)), 2)

    # number of words plotted doubled when show_reference = TRUE
    expect_equal(nrow(ggplot2::ggplot_build(p1)$data[[1]]), k)
    expect_equal(nrow(ggplot2::ggplot_build(p2)$data[[1]]), 2 * k)

    # works with integer colour
    expect_silent(textplot_keyness(result, color = 1:2))

    # test that textplot_keyness works with pallette (vector > 2 colours)
    expect_silent(textplot_keyness(result, show_reference = TRUE,
                                   color = RColorBrewer::brewer.pal(6, "Dark2")))
})

dev.off()
