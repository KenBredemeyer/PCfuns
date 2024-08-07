# test
set.seed(1234)
missing.i <- sample(1:20, 6)
x_up <- 1:20; x_up[missing.i] <- NA



x_up
impute_trend(x_up)

x_down <- 20:1; x_down[missing.i] <- NA
x_down
impute_trend(x_down)

x <- sample(0:5, size = 25, replace = TRUE)
x_miss.i <- sample(1:25, 8)
x[x_miss.i] <- NA
x
impute_trend(x)