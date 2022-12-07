#!/usr/bin/env Rscript

if (interactive()) {
  argv <- file.choose()
} else {
  argv <- commandArgs(T)
}

# read input
input <- readLines(argv) |>
  strsplit(" ")

#functions to move and parse commands
cd <- function(path) {
  if (path == "..")
    setwd("..")
  else {
    if (!dir.exists(path))
      dir.create(path)
    setwd(path)
  }
}

parse <- function(cmd) {
  if (cmd[1] == "$") {
    if (cmd[2] == "cd")
      cd(cmd[3])
  }
  else if (cmd[1] != "dir") {
      writeLines(cmd[1],cmd[2])
  }
}

iamhere <- getwd()
root <- paste0(tempdir(),"\\day7")
dir.create(root)
setwd(root)
for (x in input[-1]) {
  parse(x)
}
setwd(iamhere)

# function to solve problem
solve <- function(dir) {
  sol[[dir]] <<- 0L
  allfiles <- list.files(dir,full.names = T)
  dirs <- list.dirs(dir, full.names = T, recursive = F)
  for (name in setdiff(allfiles,dirs))
    sol[[dir]] <<- sol[[dir]] + as.integer(readLines(name)[1])
  for (d in list.dirs(dir, recursive = F))
    solve(d)
}

#part 1
sol <- list()
solve(root)

for (dir in names(sol)) {
  sol[[dir]] <- sol[[dir]] + sum(unlist(sol[startsWith(names(sol), paste0(dir,"/"))]))
}

nums <- unlist(sol)

print(sum(nums[nums < 100000]))

#part 2
rootspace <- sol[[1]]
unused <- 70000000 - rootspace 
required <- 30000000 - unused

sort(nums[nums > required])[1]
