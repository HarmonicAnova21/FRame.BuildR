#Function List:
#             1.  is.group()
#             2.  build.grp()


#Function     - is.group()
#Desctription - Takes a vector of n integers and determines if
#               it is a cyclic integer group of order n (or
#               scalar - integer - multiple).
#Version      - 0.1:  can only handle groups that are Z/nZ or
#               scalar (integer) multiple.
#'@title Check if a set is a Group
#'
#'@description Takes a vector of n integers and determines if
#'it is a cyclic integer group of order n (or scalar - integer
#'- multiple).
#'
#'@param g a vector of integers
#'
#'@examples
#'is.group(g = as.integer(0:5))
is.group<-function(g)
{
  #General Result
  result<-list(res = FALSE,response = 'Null, function failed')
  #check for integer values
  if(!is.integer(g))
  {
    result$response<-'Groups Must be Integer-valued'
    return(result)
  }
  #Since these are a subset of the integers, use 1-step
  #subgroup test:
  h1<-matrix(g,ncol=1)
  h1<-sort(h1)
  n<-length(h1)
  h2<-matrix(0:(n-1),ncol=1)
  c<-h1/h2
  c2<-unique(c[complete.cases(c),])
  if(length(c2)!=1)
    {
      result$response<-'Not a scalar of an Integer Group'
      return(result)
    }
  if((c2-as.integer(c2))!=0)
    {
      result$response<-'Not an integer scalar of an Integer Group'
      return(result)
    }
  int<-c2*n
  t<-expand.grid(g,-g)
  t2<-add.m(t[,1],t[,2],int)
  sgtest<-t2%in%g
  if(all(sgtest))
  {
    result$res<-TRUE
    result$response<-''
  }else
  {
    result$response<-'Not a Group, not closed by addition'
  }
  return(result)
}


#Function     - build.grp()
#Desctription - Takes a vector of integers and then calls
#               is.group and determines if the given vector
#               is a group (currently fixed as integer cyclic)
#               Finally, it constructs a group object and
#               returns it.
#Version      - 0.1:  group must be an integer cyclic. Future
#               functionality may include the option for non-
#               integer groups.

#'@title Build a Group Object
#'
#'@description Takes a vector of integers and then calls
#'is.group and determines if the given vector
#'is a group (currently fixed as integer cyclic)
#'Finally, it constructs a group object and returns it.
#'
#'@param g a vector of integers
#'
#'@examples
#'build.group(g = as.integer(0:5))
#'@export
build.group<-function(g)
{
  test<-is.group(g)
  if(!test$res)
  {
    stop(paste('Failed',test$response,sep = ':'))
  }
  #g2<-matrix(g,ncol=1)
  grp<-new("Group", group =g, modulus = length(g))
  return(grp)

}
