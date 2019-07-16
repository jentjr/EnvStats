library(EnvStats)

# Example 6-1, p. 6-18
test_that("predIntNormTestPower test 1", {
  x <- predIntNormTestPower(n = 10, k = 1,
                            delta.over.sigma = (15 - 6) / 2,
                            pi.type = "upper", conf.level = 0.99)
  expect_true(round(x, 7) == 0.9012473)
})

# Example 6-3, pp. 6-20 to 6-21
stats   <- summaryStats(Sulfate.ppm ~ 1, data = EPA.09.Ex.6.3.sulfate.df,
                        data.name = "Sulfate", digits = 1)

n     <- stats[, "N"]
SD    <- stats[, "SD"]
delta <- c(0, 200)

test_that("predIntNormTestPower test 2",{
  x <- predIntNormTestPower(n = n, k = 1, delta.over.sigma = 75 / SD,
                            pi.type = "upper", conf.level = 0.99)
  expect_true(round(x, 7) == 0.3916402)
})
