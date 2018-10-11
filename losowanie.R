gen <- function(y)
{
  l <- y
  dane <- lapply(l, function(x)
  {
    list("ts" = rt(x, 2), 
         "unif" = runif(x, 1, 5), 
         "exp" = rexp(x)
    )
    
  }) 
  #names(dane) <- c("5el", "10el", "20el", "50el")
  return(dane)
}