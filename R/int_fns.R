#Functions List:
#               1.  count()
#               2.  add.m()



#Function     - count()
#Desctription - Counts the number of times a value
#               appears in a given vector.
#Version      - 0.1:  Primarily internal use only

#'@title Count Occurences in a Vector
#'
#'@description    Counts the number of times the given number
#'n appears in the given vector vect.
#'
#'@param vect a vector of values
#'
#'@param n a number to be compared to the vector
#'
#'@examples
#'count(vect = 1:5, n = 4)
count<-function(vect,n)
{
  c<-length(vect[vect %in%n])
  return(c)

}

#Function     - add.m()
#Desctription - Does Modular addition.
#Version      - 0.1:  Primarily internal use only
#'@title Modular Addition
#'
#'@description Adds two numbers and then returns the
#'remainder after dividing by the modulo.
#'
#'@param a The first summand - integer
#'
#'@param b The second summand - integer
#'
#'@param m The modulo value - integer
#'
#'@examples
#'add.m(a=3 b= 9, n = 10)
add.m<-function(a,b,m)
{
  c<-(a+b)%%m
  return(c)

}
