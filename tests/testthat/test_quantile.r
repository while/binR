context("Quantile binning")

test_that("Test computing breaks on one variable", {
        dat <- data.frame(x=-100:100, y=rep(1, 201))
        obj <- binR(y~x, dat, algorithm="quantile")
        expect_equal(obj$breaks$x, c(-Inf,-50,0,50,Inf))
})

test_that("Test computing breaks on one variable all pos", {
        dat <- data.frame(x=0:100, y=rep(1, 101))
        obj <- binR(y~x, dat, algorithm="quantile")
        expect_equal(obj$breaks$x, c(0,25,50,75,Inf))
})

test_that("Test computing breaks on one variable all neg", {
        dat <- data.frame(x=-100:0, y=rep(1, 101))
        obj <- binR(y~x, dat, algorithm="quantile")
        expect_equal(obj$breaks$x, c(-Inf,-75,-50,-25,0))
})

test_that("Test computing nonstandard breaks on one variable", {
        dat <- data.frame(x=0:100, y=rep(1, 101))
        obj <- binR(y~x, dat, algorithm="quantile", probs=c(0.1, 0.5, 0.9, 1))
        expect_equal(obj$breaks$x, c(0,50,90,Inf))
})
