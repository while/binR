context("Quantile binning")

test_that("Test computing breaks on one variable", {
        dat <- data.frame(x=-100:100)
        obj <- binR(~x, dat)
        expect_equal(obj$breaks$x, c(-Inf,-50,0,50,Inf))
})

test_that("Test computing breaks on one variable all pos", {
        dat <- data.frame(x=0:100)
        obj <- binR(~x, dat)
        expect_equal(obj$breaks$x, c(0,25,50,75,Inf))
})

test_that("Test computing breaks on one variable all neg", {
        dat <- data.frame(x=-100:0)
        obj <- binR(~x, dat)
        expect_equal(obj$breaks$x, c(-Inf,-75,-50,-25,0))
})
