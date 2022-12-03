#!/usr/bin/env Rscript

if (interactive()) {
  argv <- file.choose()
} else {
  argv <- commandArgs(T)
}

library(dplyr)
library(tidyr)

input <- readLines(argv) %>%
  as.data.frame() %>%
  `colnames<-`("both")

score <- 1:52
names(score) <- c(letters,LETTERS)

part1 <- input %>%
  mutate(
    str_length = nchar(both),
    comp1 = substr(both,1,str_length / 2) %>% strsplit(""),
    comp2 = substr(both, (str_length / 2) + 1, str_length) %>% strsplit(""),
    val = mapply(\(x,y) intersect(x, y), comp1, comp2),
    pts = score[val]
  )

sum(part1$pts)

part2 <- input %>%
  mutate(
    group = seq(0,n()-1) %/% 3,
    column = seq(0,n()-1) %% 3
  ) %>%
  pivot_wider(
    values_from = both,
    names_prefix = "elf",
    names_from = column,
  ) %>%
  mutate_at(
    c("elf0","elf1","elf2"),
    \(x) strsplit(x,"")
  ) %>%
  mutate(
    val = mapply(\(x,y,z) intersect(intersect(x, y),z), elf0, elf1, elf2),
    pts = score[val]
  )

sum(part2$pts)
