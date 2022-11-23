day2p1 <- function(file) {
  lines <- get_me_all_lines(2, file)
  words <- lapply((lines),\(x) unlist(strsplit(x, " ")))

  good_passwords <- 0
  for (line in words) {
    letter <- substr(line[[2]],1,1)
    count <- gsub(paste0("[^",letter,"]"),"",line[[3]]) |> nchar()
    limits <- unlist(strsplit(line[[1]],"-"))
    parsed_test <- parse(
      text = paste0(limits[1],"<=",count, "&&",count,"<=",limits[2])
    )
    if (eval(parsed_test))
      good_passwords <- good_passwords + 1
  }
  good_passwords
}

day2p2 <- function(file) {
  lines <- get_me_all_lines(2, file)
  words <- lapply((lines),\(x) unlist(strsplit(x, " ")))

  good_passwords <- 0
  for (line in words) {
    letter <- substr(line[[2]],1,1)
    limits <- unlist(strsplit(line[[1]],"-"))

    check1 <- substr(line[[3]], limits[1], limits[1])
    check2 <- substr(line[[3]], limits[2], limits[2])

    if ((check1 == letter && check2 != letter) ||
        (check1 != letter && check2 == letter))
      good_passwords <- good_passwords + 1
  }
  good_passwords
}
