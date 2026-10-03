same_as_dt_tree <- function(model) {
  reference <- xgboost::xgb.model.dt.tree(model = model, use_int_id = TRUE)
  parsed <- xgb_trees(model)$trees
  for (col in c("Tree", "Node", "Feature", "Yes", "No", "Missing")) {
    expect_identical(parsed[[col]], reference[[col]], label = col)
  }
  for (col in c("Split", "Gain", "Cover")) {
    expect_equal(parsed[[col]], reference[[col]], tolerance = 1e-6, label = col)
  }
  expect_true(all(lengths(parsed$Categories) == 0))
}

test_that("JSON tree parser matches xgb.model.dt.tree for numeric models", {
  x <- as.matrix(mtcars[, -1])
  same_as_dt_tree(xgboost(x, mtcars$mpg, nrounds = 5, max_depth = 3, verbosity = 0))
  same_as_dt_tree(xgboost(x, factor(mtcars$cyl), nrounds = 3, max_depth = 2, verbosity = 0))
  # unnamed matrix: features referred to by index
  same_as_dt_tree(xgboost(unname(x), mtcars$mpg, nrounds = 3, max_depth = 2, verbosity = 0))
})

test_that("JSON tree parser exposes categorical splits", {
  set.seed(1)
  n <- 200
  df <- data.frame(age = rnorm(n), cat = factor(sample(c("lo", "mid", "hi"), n, TRUE)))
  y <- as.integer(df$age * (df$cat == "hi") > 0)
  model <- xgboost(df, factor(y), nrounds = 3, max_depth = 2, verbosity = 0)

  parsed <- xgb_trees(model)
  expect_identical(parsed$categorical, "cat")
  cat_nodes <- parsed$trees[Feature == "cat"]
  expect_gt(nrow(cat_nodes), 0)
  expect_true(all(lengths(cat_nodes$Categories) > 0))
  expect_true(all(unlist(cat_nodes$Categories) %in% 0:2))
  # members of the set go to the right child, which is `Yes`
  dump <- xgboost::xgb.dump(model)
  first <- parsed$trees[Feature == "cat"][1]
  expect_match(dump[grep(sprintf("^%d:\\[cat:", first$Node), dump)][1], sprintf("yes=%d,no=%d", first$Yes, first$No))
})

# xgb.dump() is the only xgboost tabulation that supports categorical splits:
#   0:[cat:{1,3,4}] yes=2,no=1,missing=1,gain=5.3,cover=59.4
#   1:[age<-0.0313] yes=3,no=4,missing=4,gain=18.6,cover=14.5
#   3:leaf=-0.335,cover=44.9
parse_xgb_dump <- function(model) {
  lines <- xgboost::xgb.dump(model, with_stats = TRUE)
  tree <- cumsum(grepl("^booster\\[", lines)) - 1L
  lines <- trimws(lines)
  keep <- !grepl("^booster\\[", lines)
  lines <- lines[keep]
  tree <- tree[keep]
  field <- function(name) {
    suppressWarnings(as.numeric(sub(sprintf(".*%s=([^,]+).*", name), "\\1", lines)))
  }
  leaf <- grepl(":leaf=", lines)
  node <- as.integer(sub(":.*", "", lines))
  feature <- ifelse(leaf, "Leaf", sub("^[0-9]+:\\[([^<:]+).*", "\\1", lines))
  categorical <- grepl("\\{", lines)
  categories <- lapply(seq_along(lines), function(i) {
    if (!categorical[i]) {
      return(integer(0))
    }
    as.integer(strsplit(sub(".*\\{([^}]*)\\}.*", "\\1", lines[i]), ",")[[1]])
  })
  data.table::data.table(
    Tree = tree,
    Node = node,
    Feature = feature,
    Split = ifelse(
      leaf | categorical,
      NA_real_,
      suppressWarnings(as.numeric(sub("^[0-9]+:\\[[^<]+<([^]]+)\\].*", "\\1", lines)))
    ),
    Yes = ifelse(leaf, NA_integer_, as.integer(field("yes"))),
    No = ifelse(leaf, NA_integer_, as.integer(field("no"))),
    Missing = ifelse(leaf, NA_integer_, as.integer(field("missing"))),
    Gain = ifelse(leaf, field("leaf"), field("gain")),
    Cover = field("cover"),
    Categories = categories
  )
}

test_that("JSON tree parser matches the text dump for categorical models", {
  set.seed(2)
  n <- 300
  df <- data.frame(
    age = rnorm(n),
    cat = factor(sample(LETTERS[1:6], n, TRUE)),
    grp = factor(sample(c("a", "b"), n, TRUE))
  )
  y <- factor(df$age * (df$cat %in% c("B", "E")) + (df$grp == "b") > 0)
  model <- xgboost(df, y, nrounds = 8, max_depth = 4, verbosity = 0)

  reference <- parse_xgb_dump(model)
  data.table::setorder(reference, Tree, Node)
  parsed <- xgb_trees(model)$trees
  expect_gt(sum(lengths(reference$Categories) > 1), 0)

  for (col in c("Tree", "Node", "Feature", "Yes", "No", "Missing", "Categories")) {
    expect_identical(parsed[[col]], reference[[col]], label = col)
  }
  for (col in c("Split", "Gain", "Cover")) {
    expect_equal(parsed[[col]], reference[[col]], tolerance = 1e-5, label = col)
  }
})
