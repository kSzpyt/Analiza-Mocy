gen <- function(y)
{
  l <- y
  dane <- lapply(l, function(x)
  {
    list("ts" = rt(x, 2), 
         "unif" = runif(x, -5, 5), 
         "exp" = rexp(x, 1)
    )
    
  }) 
  return(dane)
}
