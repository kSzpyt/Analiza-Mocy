#shapiro dobry dla ma łych prob (6000 xD)
#jb test słabo działa dla małych prób

library(dplyr)
library(tidyr)
library(fBasics)
library(tseries)
library(ggplot2)
library(reshape2)
source("losowanie.R")


l <- c(10, 20, 50, 100)
n <- 1000
alpha <- 0.05


############## sztos
###########################################################
now <- Sys.time()
df <- data.frame()
for (a in l)
{
  for (b in 1:n) 
  {
    data <- gen(a)
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

halp <- df
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
  colnames(m) <- c("sw", "jb", "ks", "chi", "ad")
  rownames(m) <- c("ts", "unif", "exp")
  mali[[c]] <- m
  
}

names(mali) <- l
mali

Sys.time() - now


halp <- as_tibble(halp)
halp2 <- cbind(halp[,-length(halp)] < alpha, "length" = halp[,length(halp)])

halp3 <- halp2 %>%
  group_by(length) %>%
  summarise_all(funs(mean))

charts <- list(0, 0, 0, 0)

for (x in 1:4)
{
  pomocy <- halp3[which(halp3$length == l[x]),-1]
  
  aa <- as_tibble(melt(pomocy))
  aa <- aa %>%
    separate(variable, c("test", "dist"))
  
  a <- aa %>%
    ggplot(aes(x = dist, y = value, fill = test)) + 
    geom_bar(stat="identity", position = position_dodge()) + 
    coord_flip()+
    geom_text(aes(label = value), size = 4.5, position = position_dodge(0.9)) +
    scale_fill_brewer(palette="Paired")
  
  charts[[x]] <- a
}
charts


