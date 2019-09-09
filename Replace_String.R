rm(list = ls())
setwd("C:\\R")

library(base)
a = ("I love to play football bcz my favourite sportsperson is ronald bcz i love cars")
b = gsub("bcz", "because", a)
b