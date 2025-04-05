newtonraphson <- function(a, b, c, n) {

f <- function(x) {
  return(a * x^2 + b * x + c)
}

f_derivative <- function(x) {
  return(2 * a * x + b)
}
x0 = 1
eps = 1e-6
x_old <- x0

for (i in 1:n) {
  x_new = x_old - f(x_old) / f_derivative(x_old)
  if(abs(x_new - x_old) < eps) {
    return(x_new)
  }
    x_old <- x_new
}
  return(x_old)
}

newtonraphson(1,0, -2, 1000)
