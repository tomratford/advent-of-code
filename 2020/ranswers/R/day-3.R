day3p1 <- function(input, slope.x, slope.y) {
  lst <- get_me_all_lines(3,input) |> strsplit("")
  matrix <- do.call(rbind, lst)
  dims <- dim(matrix)
  pos.x = 1
  pos.y = 1

  tree_count <- 0
  while (pos.y <= dims[1]) {
    if (matrix[pos.y,pos.x] == "#")
      tree_count <- tree_count + 1

    temp.x <- pos.x + slope.x
    pos.x <- temp.x %% dims[2]
    if (pos.x == 0) pos.x <- dims[2]
    pos.y <- pos.y + slope.y
  }
  tree_count
}

day3p2 <- function(input) {
  xs <- c(1,3,5,7,1)
  ys <- c(1,1,1,1,2)
  vals <- mapply(\(x,y) day3p1(input, x, y), xs, ys)
  Reduce(`*`, vals)
}
