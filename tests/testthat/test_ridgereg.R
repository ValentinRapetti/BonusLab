data("iris")


test_that("lenreg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda = 1))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda = 1))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris))
  expect_error(ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = -1))
})

# ridgereg <- lm.ridge
# 

test_that("class is correct", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  expect_s3_class(ridgereg_mod, "ridgereg")
})


test_that("print() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  expect_output(print(ridgereg_mod), "ridgereg\\(")
  expect_output(print(ridgereg_mod), "data = iris")
  expect_output(print(ridgereg_mod), "lambda = 1")
  expect_output(print(ridgereg_mod), "Sepal\\.Width")
  #expect_output(print(linreg_mod),"linreg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  #expect_output(print(linreg_mod),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})


test_that("pred() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  expect_equal(round(unname(predict(ridgereg_mod)[c(1,5,7)]),1) , round(c(1.8,  1.5,  1.1),1) )   
})



test_that("coef() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  ridge_builtin <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  expect_true(all(round(unname(coef(ridgereg_mod)[2:3]),2) %in% round(unname(ridge_builtin$coef),2)))
})
