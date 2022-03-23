library("quanteda")

test_that("test textplot_network", {
    #skip_on_os("linux")
    txt <- "A D A C E A D F E B A C E D"
    testfcm <- fcm(tokens(txt), context = "window", window = 3, tri = FALSE)
    testdfm <- dfm(tokens(txt))
    expect_silent(textplot_network(testfcm, vertex_color = "red", offset = 0.1))
    expect_silent(textplot_network(testdfm, offset = 0.1))
    expect_error(textplot_network(testfcm, min_freq = 100),
                 "There is no co-occurence higher than the threshold")

    # works with interger colour
    expect_silent(textplot_network(testfcm, vertex_color = 2))
    expect_silent(textplot_network(testfcm, edge_color = 2))
})

test_that("test textplot_network works with vectorlized argument", {
    #skip_on_os("linux")
    txt <- "A D A C E A D F E B A C E D"

    testfcm <- fcm(tokens(txt), context = "window", window = 3, tri = FALSE)
    expect_silent(textplot_network(testfcm, vertex_color = rep(c(1, 2), nrow(testfcm) / 2)))
    expect_silent(textplot_network(testfcm, vertex_size = Matrix::rowSums(testfcm) / 5))
    expect_silent(textplot_network(testfcm, vertex_labelcolor = rep(c(1, NA), nrow(testfcm) / 2)))
    expect_silent(textplot_network(testfcm, vertex_labelsize = Matrix::rowSums(testfcm) / 5))
})

test_that("textplot_network error when fcm is too large", {
    testdfm <- fcm(tokens(data_corpus_inaugural[1:5]))
    expect_error(textplot_network(testdfm, min_freq = 1, offset = 0, omit_isolated = FALSE),
                 "fcm is too large for a network plot")
})

test_that("test textplot_network font-selection", {
    skip_on_os("linux")
    txt <- tokens("A D A C E A D F E B A C E D")
    testfcm <- fcm(txt, context = "window", window = 3, tri = FALSE)
    testdfm <- dfm(txt)
    expect_silent(textplot_network(testfcm, offset = 0.1,
                                   vertex_labelfont = "serif"))
    expect_silent(textplot_network(testdfm, offset = 0.1,
                                   vertex_labelfont = "sans"))
    expect_error(textplot_network(testfcm, min_freq = 0.1,
                                  vertex_labelfont = "not_a_real_font"),
                 "not_a_real_font is not found on your system")
})

test_that("raises error when dfm is empty (#1419)", {
    mx <- dfm_trim(quanteda::data_dfm_lbgexample, 1000)
    expect_error(textplot_network(mx),
                 quanteda.textplots:::message_error("dfm_empty"))
    expect_error(textplot_network(fcm(mx)),
                 quanteda.textplots:::message_error("fcm_empty"))
    expect_error(textplot_wordcloud(mx),
                 quanteda.textplots:::message_error("dfm_empty"))

})

test_that("remove_edges is working", {
    mt <- fcm(tokens(c("a a b", "a b", "c b")))
    expect_identical(colnames(quanteda.textplots:::remove_edges(mt, 1, TRUE)),
                     c("a", "b", "c"))
    expect_identical(colnames(quanteda.textplots:::remove_edges(mt, 2, TRUE)),
                     c("a", "b"))
    expect_equivalent(Matrix::diag(quanteda.textplots:::remove_edges(mt, 1, FALSE)),
                     c(0, 0, 0))
})

# test_that("error when fcm is ordered", {
#
#     mt <- fcm(c("a a b", "a b", "c b"), ordered = FALSE)
#     expect_silent(as.network(mt))
#     mt2 <- fcm(c("a a b", "a b", "c b"), ordered = TRUE)
#     expect_error(as.network(mt2),
#                  "Cannot plot ordered fcm")
# })

test_that("as.network.fcm works", {
    txt <- tokens(c("a a a b b c", "a a c e", "a c e f g"))
    mat <- fcm(txt)
    net <- as.network(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(network::is.network(net))
    expect_identical(network::network.vertex.names(net), featnames(mat))
    expect_identical(network::get.vertex.attribute(net, "frequency"), 
                     unname(quanteda.textplots:::get_margin(mat)))
    expect_silent(as.network(mat, min_freq = 3, omit_isolated = TRUE))
})

test_that("as.network.fcm works with window", {
    txt <- tokens(c("a a a b b c", "a a c e", "a c e f g"))
    mat <- fcm(txt, contex = "window", window = 2)
    net <- as.network(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(network::is.network(net))
    expect_identical(network::network.vertex.names(net), featnames(mat))
    expect_identical(network::get.vertex.attribute(net, "frequency"), 
                     unname(quanteda.textplots:::get_margin(mat)))
    expect_silent(as.network(mat, min_freq = 3, omit_isolated = TRUE))
})

test_that("as.igraph.fcm works", {
    skip_if_not_installed("igraph")
    txt <- tokens(c("a a a b b c", "a a c e", "a c e f g"))
    mat <- fcm(txt)
    net <- as.igraph(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(igraph::is.igraph(net))
    expect_identical(igraph::vertex_attr(net, "name"), featnames(mat))
    expect_identical(igraph::vertex_attr(net, "frequency"), 
                     unname(quanteda.textplots:::get_margin(mat)))
    expect_silent(as.igraph(mat, min_freq = 3, omit_isolated = TRUE))
})

test_that("as.igraph.fcm works with window", {
    skip_if_not_installed("igraph")
    skip_on_os("linux")
    txt <- tokens(c("a a a b b c", "a a c e", "a c e f g"))
    mat <- fcm(txt, contex = "window", window = 2)
    net <- as.igraph(mat, min_freq = 1, omit_isolated = FALSE)
    expect_true(igraph::is.igraph(net))
    expect_identical(igraph::vertex_attr(net, "name"), featnames(mat))
    expect_identical(igraph::vertex_attr(net, "frequency"), 
                     unname(quanteda.textplots:::get_margin(mat)))
    expect_silent(as.igraph(mat, min_freq = 3, omit_isolated = TRUE))
})
