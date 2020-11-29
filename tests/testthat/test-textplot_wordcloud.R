pdf(file = tempfile(".pdf"), width = 10, height = 10)

test_that("test textplot_wordcloud works for dfm objects", {
    mt <- dfm(data_corpus_inaugural[1:5])
    mt <- dfm_trim(mt, min_termfreq = 10)
    expect_silent(textplot_wordcloud(mt))
})

test_that("test textplot_wordcloud works for keyness objects", {
    tstat <- head(data_corpus_inaugural, 2) %>%
        dfm(remove_punct = TRUE, remove = stopwords("en")) %>%
        quanteda.textstats::textstat_keyness(target = 1)
    expect_silent(textplot_wordcloud(tstat, max_words = 50))
    expect_silent(textplot_wordcloud(tstat, comparison = FALSE, max_words = 50))
})

test_that("test textplot_wordcloud comparison works", {
    skip_on_travis()
    skip_on_cran()
    skip_on_os("linux")
    testcorp <- corpus_reshape(corpus(data_char_sampletext))
    set.seed(1)
    docvars(testcorp, "label") <- sample(c("A", "B"), size = ndoc(testcorp), replace = TRUE)
    docnames(testcorp) <- paste0("text", 1:ndoc(testcorp))
    testdfm <- dfm(testcorp, remove = stopwords("english"))
    testdfm_grouped <- dfm(testcorp, remove = stopwords("english"), groups = "label")

    jpeg(filename = tempfile(".jpg"), width = 5000, height = 5000)
    expect_silent(
        textplot_wordcloud(testdfm_grouped, comparison = TRUE)
    )
    expect_silent(
        textplot_wordcloud(testdfm_grouped, random_order = FALSE)
    )
    expect_silent(
        textplot_wordcloud(testdfm_grouped, ordered_color = FALSE)
    )
    expect_error(
        textplot_wordcloud(dfm(data_corpus_inaugural[1:9]), comparison = TRUE),
        "Too many documents to plot comparison, use 8 or fewer documents"
    )

    dfmsmall <- dfm(data_corpus_inaugural[1:9], groups = "President", 
                    remove = stopwords("en"), remove_punct = TRUE) %>%
        dfm_trim(min_termfreq = 20)
    expect_silent(textplot_wordcloud(dfmsmall, comparison = TRUE))
    expect_silent(textplot_wordcloud(dfmsmall, color = 1:5))
    expect_warning(
        textplot_wordcloud(dfmsmall, scale = 1:4),
        "scale is deprecated"
    )
    expect_warning(
        textplot_wordcloud(dfmsmall, random.order = TRUE),
        "random.order is deprecated; use random_order instead"
    )
    expect_warning(
        textplot_wordcloud(dfmsmall, max.words = 10),
        "max.words is deprecated; use max_words instead"
    )

    dev.off()
    expect_error(
        textplot_wordcloud(testdfm, comparison = TRUE),
        "Too many documents to plot comparison, use 8 or fewer documents\\."
    )
})

test_that("test textplot_wordcloud raise deprecation message", {
    jpeg(filename = tempfile(".jpg"), width = 5000, height = 5000)
    mt <- dfm(data_corpus_inaugural[1:5])
    mt <- dfm_trim(mt, min_termfreq = 10)
    expect_warning(textplot_wordcloud(mt, min.freq = 10), "min.freq is deprecated")
    expect_warning(textplot_wordcloud(mt, use.r.layout = 10), "use.r.layout is no longer use")
    dev.off()
})

test_that("plotting empty dfms after trimming is caught (#1755)", {
    dfmat <- dfm(c("Azymuth", "Compass", "GPS", "Zenith"))
    expect_error(
        textplot_wordcloud(dfmat, min_count = 2),
        "No features left after trimming with min_count = 2"
    )
})

dev.off()
