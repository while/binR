context("rpart binning")

test_that("Test computing breaks on one variable", {
        dat <- data.frame(x=sample(-100:100, 201))
        dat$y <- factor(dat$x > 50 | dat$x < -10)

        obj <- binR(y~x, dat, algorithm="rpart")
        expect_equal(obj$breaks$x, c(-Inf,-10.5,50.5,Inf))
})

test_that("Test computing breaks on one variable all pos", {
        dat <- data.frame(x=sample(100))
        dat$y <- factor(dat$x > 50 | dat$x < 10)

        obj <- binR(y~x, dat, algorithm="rpart")
        expect_equal(obj$breaks$x, c(0,9.5,50.5,Inf))
})

test_that("Test computing breaks on one variable all neg", {
        dat <- data.frame(x=sample(-100:0, 101))
        dat$y <- factor(dat$x < -50 | dat$x > -10)

        obj <- binR(y~x, dat, algorithm="rpart")
        expect_equal(obj$breaks$x, c(-Inf,-50.5,-9.5,0))
})


test_that("Test computing breaks on two vars", {
        set.seed(11)
        dat <- data.frame(x1=sample(-100:100, 100),
                          x2=sample(100))
        dat$y <- factor(dat$x1 > 50 | dat$x1 + dat$x2 < -10 | dat$x2 > 50)

        obj <- binR(y~x1 + x2, dat, algorithm="rpart")
        expect_equal(obj$breaks$x1, c(-Inf, -40.5, -14.5, 29.5, 46.5, Inf))
        expect_equal(obj$breaks$x2, c(0.0, 21.5, 40.5, 50.5, Inf))
})
