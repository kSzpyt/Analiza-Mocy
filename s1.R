#shapiro dobry dla ma łych prob (6000 xD)
#jb test słabo działa dla małych prób

library(dplyr)
library(tidyr)
library(tseries)
source("s2.R")

foo <- foo



l <- c(5, 10, 20, 50)
n <- 1000
alpha <- 0.05


dane <- lapply(l, function(x)
{
  list("ts" = rt(x, 2), 
       "unif" = runif(x, 1, 5), 
       "exp" = rexp(x)
       )
  
}) 
names(dane) <- c("5el", "10el", "20el", "50el")

############## sztos


tib <- tibble("5" = 0, "10" = 0, "20" = 0, "50" = 0)

for(a in 1:4)
{
  pvalues <- sapply(dane, function(x)
  {
    shapiro.test(x)$p.value
  })
  tib <- rbind(tib, pvalues)
  #mean(pvalues[1] < alpha)
}
tib
mean(tib[4] < 0.05)
################# 

tib <- tibble("ts" = 0, "unif" = 0, "exp" = 0, "n" = 0)

for (a in 1:length(l))
{
  pvalues <- lapply(dane[[a]], function(x)
  {
    list("sh" = shapiro.test(x)$p.value,
         "jb" = jarque.bera.test(x)$p.value)
  })
  tib <- rbind(tib, c(pvalues, a))
  
}

df <- as.data.frame(pvalues, ncol = 2)
m <- matrix(df, ncol = 3)
m <- cbind(m, 1)
m <- as.numeric(m)
m <- matrix(m, ncol = 3)
colnames(m) <- c("ts", "unif", "exp", "n")
bm <- rbind(tib, m)
bm[[1]]

cbind




pvalues <- lapply(dane[[1]], function(x)
{
  list("sh" = shapiro.test(x)$p.value,
       "jb" = jarque.bera.test(x)$p.value)
})

pvalues[[1]]


aaa <- dane[[1]]
aaa

lapply(aaa, function(x)
  {
  shapiro.test(x)$p.value
})













