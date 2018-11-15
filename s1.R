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




plot(dnorm(seq(-10,10,0.01)), type = "l")
plot(dexp(seq(-10,10,0.01)))
plot(dunif(seq(-10,10,0.01)))
