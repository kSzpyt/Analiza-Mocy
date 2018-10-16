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

data1 <- as_tibble(mali[[1]])
data1 %>%
  ggplot() + 
  geom_bar(x = "ts")



aaa <- data1[1,][[3]][[1]]
ggplot(data = aaa, aes(x = ts))+
  geom_bar(stat="identity")


halp <- as_tibble(halp)

pomocy <- halp[which(halp$length == 10), 1:5]

aa <- as_tibble(melt(pomocy))
aa %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_bar(stat="identity") + 
  coord_flip()



halp[,1:5] %>%
  ggplot() + 
  geom_bar(aes(x = 1:4000, y = factor))









