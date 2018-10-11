#shapiro dobry dla ma łych prob (6000 xD)
#jb test słabo działa dla małych prób

library(dplyr)
library(tidyr)
library(fBasics)
library(tseries)
source("losowanie.R")
#source("s2.R")

#foo <- foo



l <- c(8, 10, 20, 50)
n <- 1000
alpha <- 0.05

dane <- gen(l)


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
    list("sw" = shapiro.test(x)$p.value,
         "jb" = jarque.bera.test(x)$p.value)
  })
  
  
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


df <- data.frame()

pvalues <- lapply(dane[[1]], function(x)
{
  list("sw" = shapiro.test(x)$p.value,
       "jb" = jarque.bera.test(x)$p.value)
})
df <- rbind(df, as.data.frame(pvalues))
x <- data.frame()
x <- rbind(x, a)
a <- as.data.frame(pvalues)
as_tibble(a)


##########
tib2 <- data.frame("sw" = 0, "jb" = 0)
tib2 <- rbind(tib2, c(0, 0), c(0, 0))
rownames(tib2) <- c("ts", "unif", "exp")
##########

dane[[1]]
pvalues[[1]][1]


aaa <- dane[[1]]
aaa

lapply(aaa, function(x)
  {
  shapiro.test(x)$p.value
})








x<- c(1:20)

df <- data.frame()
for (a in l)
{
  for (b in 1:n) 
  {
    data <- gen(l)
    pvalues <- lapply(data[[1]], function(x)
    {
      list("sw" = shapiroTest(x)@test$p.value,
           "jb" = jarqueberaTest(x)@test$p.value,
           "ks" = ksnormTest(x)@test$p.value[1],
           "chi" = pchiTest(x)@test$p.value[1],
           "ad" = adTest(x)@test$p.value[1])
    })
   df <- rbind(df, c(as.data.frame(pvalues), "length" = a))
    
  }
}

df <- cbind(df[,-length(df)] < alpha, "length" = df[,length(df)])
df <- as_tibble(df)
df2 <- df %>%
  group_by(length) %>%
  summarise_all(funs(mean))
df
df2

df[1,]











a <- ksnormTest(rnorm(50))
a
str(a)
a@test$p.value[1]


ks.test(x)

ksnormTest(x)

