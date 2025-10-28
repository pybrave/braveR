# file: tests/testthat/test-diffSummary.R

library(testthat)
library(jsonlite)  # 用于解析 JSON
# 如果你的包名是 braveR，需要加载你的包
# library(braveR)

test_that("diffSummary generates correct file and content", {

  # --------------------------
  # 1. 准备输入数据
  # --------------------------
  tbl <- c(Up = 18, Down = 22, NS = 78)
  criteria <- "p < 0.05 & abs(coef) > 0"
  title <- "Microbiome summary"
  filename <- "test_summary"

  # 自动创建 output 目录
  output_dir <- "output"
  if (!dir.exists(output_dir)) dir.create(output_dir)

  file_path <- file.path(output_dir, paste0(filename, ".diff"))

  # --------------------------
  # 2. 调用 diffSummary 函数
  # --------------------------
  returned_path <- diffSummary(tbl, criteria, title, filename)

  # --------------------------
  # 3. 检查文件是否生成
  # --------------------------
  expect_true(file.exists(file_path))
  expect_equal(returned_path, file_path)

  # --------------------------
  # 4. 检查文件内容
  # --------------------------
  content <- fromJSON(file_path)

  expect_equal(content$total, sum(tbl))
  expect_equal(content$up, unname(tbl["Up"]))
  expect_equal(content$down, unname(tbl["Down"]))
  expect_equal(content$ns, unname(tbl["NS"]))
  expect_equal(content$criteria, criteria)
  expect_equal(content$title, title)

  # --------------------------
  # 5. 清理测试文件（可选）
  # --------------------------
  unlink(file_path)
})
