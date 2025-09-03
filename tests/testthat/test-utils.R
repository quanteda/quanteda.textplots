test_that("check_font is working", {
    skip_on_cran()
    # check_font no longer validates fonts, just passes them through
    expect_equal(quanteda.textplots:::check_font("XXXXX"), "XXXXX") 
    expect_equal(quanteda.textplots:::check_font("sans"), "sans")
    expect_equal(quanteda.textplots:::check_font("serif"), "serif")
    expect_equal(quanteda.textplots:::check_font("mono"), "mono")
    expect_equal(quanteda.textplots:::check_font(NULL), "")
})
