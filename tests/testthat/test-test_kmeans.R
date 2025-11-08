test_that("kmeans clustering works",
          {
            set.seed(123)
            data <- matrix(rnorm(100), ncol = 2)
            clusters <- kmeans_simple(data, centers = 3)
            expect_equal(length(clusters), nrow(data))
            expect_true(all(clusters %in% 1:3))
          })
