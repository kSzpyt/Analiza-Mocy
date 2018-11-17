#shapiro dobry dla ma łych prob (6000 xD)
#jb test słabo działa dla małych prób

library(dplyr)
library(tidyr)
#library(fBasics)
library(tseries)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(nortest)
library(Hmisc)
library(car)
source("losowanie.R")

set.seed(2137)

now <- Sys.time()
len <- c(10, 20, 50, 100)
sym <- 100
alpha <- 0.05


############## sztos
###########################################################

df <- data.frame()
for (a in len)
{
  for (b in 1:sym) 
  {
    data <- gen(a)
    pvalues <- lapply(data[[1]], function(x)
    {
      list(
           "sw" = shapiro.test(x)$p.value,
           "jb" = jarque.bera.test(x)$p.value,
           "li" = lillie.test(x)$p.value,
           "chi" = pearson.test(x)$p.value,
           "ad" = ad.test(x)$p.value)
    })
    df <- rbind(df, c(as.data.frame(pvalues), "length" = a))
    
  }
}

df_pomoc <- df
df <- cbind(df[,-length(df)] < alpha, "length" = df[,length(df)])
df <- as_tibble(df)
df2 <- df %>%
  group_by(length) %>%
  summarise_all(funs(mean))
df2

mali <- list()
for (c in 1:4)
  {
  m <- matrix(df2[c,-1], ncol = 5, byrow = TRUE)
  m <- as.data.frame(m)
  colnames(m) <- c("sw", "jb", "li", "chi", "ad")
  rownames(m) <- c("ts", "unif", "exp")
  mali[[c]] <- m
  
}

names(mali) <- len
mali




df_pomoc <- as_tibble(df_pomoc)
df_pomoc2 <- cbind(df_pomoc[,-length(df_pomoc)] < alpha, "length" = df_pomoc[,length(df_pomoc)])

df_pomoc3 <- df_pomoc2 %>%
  group_by(length) %>%
  summarise_all(funs(mean))


charts <- list(0, 0, 0, 0)

for (x in 1:length(len))
{
  aaa <- df_pomoc3[which(df_pomoc3$length == len[x]),-1]
  
  aa <- as_tibble(melt(aaa))
  aa <- aa %>%
    separate(variable, c("test", "dist"))
  
  aa$test <- as.factor(aa$test)
  levels(aa$test) <- c("ts", "exp", "unif")
  
  a <- aa %>%
    ggplot(aes(x = dist, y = value, fill = test)) + 
    geom_bar(stat="identity", position = position_dodge()) + 
    coord_flip()+
    geom_text(aes(label = value), size = 4.5, position = position_dodge(0.9), hjust = -0.2, color = "black") +
    scale_fill_brewer(palette="Paired") +
    labs(title = paste0("Długość próbki: ", len[x]))
  
  charts[[x]] <- a
}
charts

Sys.time() - now

#lieliefors
x <- seq(from=-10, to=10, by=.1)
y <- pnorm(x)
plot(x, y, type='l')


dat <- gen(c(10, 50, 100 ,200))
a <- dat[[1]][[1]]
length(a)


par(mfrow = c(1,3))
plot(ecdf(dat10[[1]][[1]]))
lines(x, y, type='l', col = "red", lwd = 4)

plot(ecdf(dat10[[1]][[2]]))
lines(x, y, type='l', col = "red", lwd = 4)

#to jest zajebiste
#porównanie dystrybuant
x <- seq(from=-10, to=10, by=.1)
y <- pnorm(x)
par(mfrow = c(4,3))
for (m in 1:4) {
  for (n in 1:3) {
    plot(ecdf(dat[[m]][[n]]), main = paste(as.character(names(dat[[m]][n])), as.character(length(dat[[m]][[n]]))))
    lines(x, y, type='l', col = "red", lwd = 4)
  }
}
##############

#zajebiscie x2
#to mam dla shapiro
par(mfrow = c(4,3))
for (m in 1:4) {
  for (n in 1:3) {
    qqnorm(dat[[m]][[n]],  main = paste(as.character(names(dat[[m]][n])), as.character(length(dat[[m]][[n]]))))
    qqline(rnorm(100), col = "red", lwd = 4)
  }
}
##########################
plot(density(dat[[1]][[1]]))
# tu ejst prbolim
par(mfrow = c(4,3))
for (m in 1:4) {
  for (n in 1:3) {
    plot(density(dat[[m]][[n]]), main = paste(as.character(names(dat[[m]][n])), as.character(length(dat[[m]][[n]]))))
    lines(density(rnorm(100)), col = "red", type = "l", lwd = 4)
  }
}

dat <- gen(c(10, 20, 50 ,100))
par(mfrow = c(2, 2))
plot(density(rnorm(100)))
plot(density(dat[[4]][[1]]))
plot(density(dat[[4]][[2]]))
plot(density(dat[[4]][[3]]))

plot(density(dat[[4]][[1]]))
plot(density(rt(150, 2)))


par(mfrow = c(2, 2))
plot(dnorm(seq(-5,5,length = 100),), main = "norm", type = "l")
plot(dt(seq(-10, 10, 0.01), 2), main = "t-studnet")
plot(dexp(seq(-10,10,0.01), 1), main = "exp")
plot(dunif(seq(-10,10,0.01)), main = "unif")
