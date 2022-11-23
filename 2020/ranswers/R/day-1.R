day1 <- function(input) {
  #parsing
  con <- file(input, "r")
  lines <- readLines(con)
  close(con)
  nums <- as.numeric(lines)

  for (i in seq_along(nums)) {
    ans <- Find(\(x) nums[i] + x == 2020, nums[-1:-i])
    if (!is.null(ans)) {
      return(ans * nums[i])
    }
  }
  "No sol found"
}

day1p2 <- function(input) {
  #parsing
  con <- file(input, "r")
  lines <- readLines(con)
  close(con)
  nums <- as.numeric(lines)

  for (i in seq_along(nums)) {
    for (j in seq_along(nums[-1:-i])) {
      ans <- Find(\(x) nums[i] + nums[j] + x == 2020, nums[-1:-j])
      if (!is.null(ans)) {
        return(ans * nums[i] * nums[j])
      }
    }
  }
  "No sol found"
}
