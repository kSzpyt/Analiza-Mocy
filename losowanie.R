gen <- function(y)
{
  l <- y
  dane <- lapply(l, function(x)
  {
    list("ts" = rt(x, 2), 
         "unif" = runif(x, -1, 1), 
         "exp" = rexp(x, 10000)
    )
    
  }) 
  return(dane)
}
