# Question 1
library(tidyverse)
bfi <- read_csv("bfi2.csv")
library(apaTables)
apa.cor.table(bfi)
library(cocor)
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi))

# Question 2
cocor(~A1+C1|A1+E1, data=as.data.frame(bfi))

# Question 3
bfi_men <- bfi %>% filter(gender==1) %>% select(-gender)
bfi_women <- bfi %>% filter(gender==2) %>% select(-gender)

apa.cor.table(bfi_men)
apa.cor.table(bfi_women)

bfi_men <- as.data.frame(bfi_men)
bfi_women <- as.data.frame(bfi_women)

cocor(~A1+E1|A1+E1, data=list(bfi_men, bfi_women))

# Question 4

library(cocor)
?cocor.dep.groups.overlap

r.jk <- .59
r.jh <- .16
r.kh <- .38
n <- 30

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n, alternative = "two.sided",
                         test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

# Question 5

r.jk <- .59
r.hm <- .19
r.jh <- .16
r.jm <- .83
r.kh <- .38
r.km <- .67
n <- 30

cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n,
                            alternative = "two.sided", test = "all", alpha = 0.05,
                            conf.level = 0.95, null.value = 0, data.name = NULL,
                            var.labels = NULL, return.htest = FALSE)

# Question 6

r1.jk <- .59
r2.hm <- .03
n1 <- 30
n2 <- 3000
cocor.indep.groups(r1.jk, r2.hm, n1, n2, alternative = "two.sided",
                   test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                   data.name = NULL, var.labels = NULL, return.htest = FALSE)

