context("rpart binning")

test_that("Test computing breaks on one variable", {
        dat <- data.frame(x=sample(100))
        dat$y <- factor(dat$x > 50 | dat$x < 10)

        obj <- binR(y~x, dat, algorithm="rpart")
        expect_equal(obj$breaks$x, c(0,9.5,50.5,Inf))
})

