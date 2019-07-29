f <- function(x) exp(x) -2
(result <- uniroot(f, c(0,1)))
result$root
polyroot(c(2,3,1))
polyroot(c(4,4,1))

f <- expression(a*x^4)
D(f,'x')

DD<- function(expr, name, order=1){
  if(order<1) stop("ordew must be >=1")
  if(order ==1) D(expr, name)
  else DD(D(expr, name), name, order -1)
}

DD(f,"x", 3)

f <- deriv(~ x^2, "x", func=T)
f(-2)
