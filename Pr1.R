# Problem 1 - OOP Programming
# a. Define rational class 

# This code come from the lecture note of STATS 506 

library(Rcpp)

# call gcd
cppFunction("
int C_gcd(int x, int y) {
 return  std::gcd(x, y);
}")

# call lcm 
cppFunction("
int C_lcm(int x, int y) {
 return  std::lcm(x, y);
}")

# Define the Rational class

setClass("rational",
         slots = c(numerator = "numeric",
                   denominator = "numeric"))

#' @title Rational constructor
#' @param a numerator of rational 
#' @param b denominator of rational 
#' @return The rational object
rational <- function(a,b) {
                return(new("rational", numerator = a, denominator = b)) 
            }

# Define a validator 

setValidity("rational", function(object){
  if (length(object@numerator) != 1) {
    stop(paste("numerator should be a numeric value"))
  }
  if (length(object@denominator) != 1) {
    stop(paste("denominator should be a numeric value"))
  }
  if (object@denominator == 0) {
    stop(paste("Denominator should not be zero"))
  }
  return(TRUE)
})

# Define a show method
setMethod("show", "rational",
          function(object) {
            if(object@numerator==0) cat(0)
            else if(object@denominator==1) cat(object@numerator)
            else cat(object@numerator, "/", object@denominator)
            return(invisible(object))
          })

# Define a simplify method

setGeneric("simplify",
           function(object) {
             standardGeneric("simplify")
           })

setMethod("simplify", "rational",
          function(object) {
            numerator <- object@numerator
            denominator <- object@denominator
            gcd_value <- C_gcd(numerator, denominator)
            object@numerator <- numerator / gcd_value
            object@denominator <- denominator / gcd_value 
            return(object)
            })

# Define a quotient method

setGeneric("quotient",
           function(object, ...) {
             standardGeneric("quotient")
           })

#' A quotient method
#' @param object A `rational`
#' @param digits The number of digits after the decimal point  
#' @return quotient of the `rational`
setMethod("quotient", "rational",
          function(object, digits = NULL ) {
            numerator <- object@numerator
            denominator <- object@denominator
            quotient <- numerator/denominator
            if(!is.null(digits)) {
              cat(round(numerator/denominator, digits = digits+1))
            } 
            else cat(quotient)
            return(invisible(quotient))
          })

# define Addition, subtraction, multiplication, division

#' @title `rational` '+' arithmetic.
#' @param e1 A `rational`
#' @param e2 A `rational`
#' @return A `rational` of e1+e2
setMethod("+", signature(e1 = "rational",
                         e2 = "rational"),
          function(e1, e2) {
            n1 <- e1@numerator
            d1 <- e1@denominator 
            n2 <- e2@numerator
            d2 <- e2@denominator 
            lcm <- C_lcm(d1, d2)
            n_plus <- n1*(lcm/d1) + n2*(lcm/d2)
            d_plus <- lcm 
            e_plus <- simplify(rational(n_plus, d_plus))
            return(invisible(e_plus))
          })

#' @title `rational` '-' arithmetic.
#' @param e1 A `rational`
#' @param e2 A `rational`
#' @return A `rational` of e1-e2
setMethod("-", signature(e1 = "rational",
                         e2 = "rational"),
          function(e1, e2) {
            n1 <- e1@numerator
            d1 <- e1@denominator 
            n2 <- e2@numerator
            d2 <- e2@denominator 
            lcm <- C_lcm(d1, d2)
            n_minus <- n1*(lcm/d1) - n2*(lcm/d2)
            d_minus <- lcm 
            e_minus <- simplify(rational(n_minus, d_minus))
            return(invisible(e_minus))
          })

#' @title `rational` '*' arithmetic.
#' @param e1 A `rational`
#' @param e2 A `rational`
#' @return A `rational` of e1*e2
setMethod("*", signature(e1 = "rational",
                         e2 = "rational"),
          function(e1, e2) {
            n1 <- e1@numerator
            d1 <- e1@denominator 
            n2 <- e2@numerator
            d2 <- e2@denominator 
            n_mutiple <- n1*n2
            d_mutiple <- d1*d2 
            e_mutiple <- simplify(rational(n_mutiple, d_mutiple))
            return(invisible(e_mutiple))
          })

#' @title `rational` '/' arithmetic.
#' @param e1 A `rational`
#' @param e2 A `rational`
#' @return A `rational` of e1/e2
setMethod("/", signature(e1 = "rational",
                         e2 = "rational"),
          function(e1, e2) {
            n1 <- e1@numerator
            d1 <- e1@denominator 
            n2 <- e2@numerator
            d2 <- e2@denominator 
            n_divide <- n1*d2
            d_divide <- d1*n2 
            e_divide <- simplify(rational(n_divide, d_divide))
            return(invisible(e_divide))
          })

# b. Use the rational class

r1 <- rational(24, 6)
r2 <- rational(7, 230)
r3 <- rational(0, 4)
r1
r3
r1 + r2
r1 - r2
r1 * r2
r1 / r2
r1 + r3
r1 * r3
r2 / r3
quotient(r1)
quotient(r2)
quotient(r2, digits = 3)
quotient(r2, digits = 3.14)
quotient(r2, digits = "avocado")
q2 <- quotient(r2, digits = 3)
q2
quotient(r3)
simplify(r1)
simplify(r2)
simplify(r3)

#c.validator check

a <- 3
b <- 0
c <- c(1,0)
d <- "a"

rational(a,b) # denominator is zero
rational(a,c) # denominator is not a single value 
rational(c,a) # numerator is not a single vlaue
rational(d,a) # numerator is a character
rational(a,d) # denominator is a character
rational(d,d) # numerator and denominator are both characters 


