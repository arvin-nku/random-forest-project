#testdata
example_tree <- list(
  list(
    node = c(1, 2, 3, 4, 5),
    name = c("root", "split1", "split2", "leaf1", "leaf2"),
    split_index = c(1, 1, 1, 1, 1),
    split_point = c(0.2, 0.5, 1.5, 1.0, 2.0),
    c_value = c(0, 1, 2, 1.5, 2.5),
    A = list(
      list(c(1, 2))
    )
  )
)

example_x <- matrix(c(0.1, 1.2, 0.6, 1.8), ncol = 2)
example_type_reg <- "reg"
example_type_cla <- "cla"


#test invalid input
test_that("prediction function handles invalid inputs", {
  expect_error(prediction(list_tree = NULL, list_x = example_x, type = example_type_reg))
  expect_error(prediction(list_tree = example_tree, list_x = NULL, type = example_type_reg))
  expect_error(prediction(list_tree = example_tree, list_x = example_x, type = NULL))
  expect_error(prediction(list_tree = example_tree, list_x = example_x, type = "invalid_type"))
})
