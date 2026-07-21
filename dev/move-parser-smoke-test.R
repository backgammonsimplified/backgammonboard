if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install `devtools` before running this smoke test.", call. = FALSE)
}

devtools::load_all(".", reset = TRUE)

examples <- c(
  "13/8",
  "13/8 6/5",
  "24/18/13",
  "13/8(2)",
  "bar/24",
  "6/off",
  "13/8*",
  "6/5*/3",
  "13/10(2) 8/5(2)"
)

for (notation in examples) {
  cat("\n", strrep("=", 64), "\n", sep = "")
  cat(notation, "\n")
  cat(strrep("-", 64), "\n", sep = "")
  print(board_moves(notation))
}

cat("\nPASS: move-parser smoke examples completed.\n")
