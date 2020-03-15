#Function List:
#             1.  is.diff.set()
#             2.  build.ds()


#Function     - is.diff.set()
#Desctription - Takes a vector of integers and a group.
#               It then  determines if the vector constitutes
#               a difference set by constructing the difference
#               table and counting the occurences.
#Version      - 0.1:  assumes group is a group object based
#               on an integer cyclic group.

#'@title Check if a set is a Difference Set for a Group
#'
#'@description Takes a vector of n integers and determines if
#'it is a difference set for the given group object
#'
#'@param ds a vector of integers
#'
#'@param h a group object (built via build.group function)
#'
#'@examples
#'is.diff.set(ds = as.integer(c(0,1,3), h=group)

is.diff.set<-function(ds,h)
{
  #General Result
  result<-list(res = FALSE,response = 'Null, function failed')
  #check for integer values, save integer group
  if(!is.integer(ds))
  {
    result$response<-'Difference Set Must be Integer-valued'
    return(result)
  }
  if(class(h)[1]!='Group')
  {
    result$repsonse<-'H is not of type Group'
    return(result)
  }

  #Determine the integrality conditions can be satisfied
  D<-length(ds)   # Size of the Difference Set
  d_count<-D*(D-1) # Number of non-zero differences
  hs<-h@modulus
  delta<-d_count/(hs-1)
  if(d_count%%(hs-1)!=0)
  {
    result$response<-'Not a Difference Set.  Set size is incompatible with Group size.'
  }

  hg<-matrix(h@group,ncol=1)

  #Create difference vectors
  t<-expand.grid(ds,-ds)

  #Create vector of differences
  diff.t<-add.m(t[,1],t[,2],hs)

  #Make sure only elements of H appear
  chk1<-matrix(sort(unique(diff.t)),ncol=1)
  if(length(chk1)!=hs)
  {
    result$response<-'Not A Difference Set - Not all group members appear in Difference Table'
    return(result)
  }
  #Determine if all elements appear equally
  d<-apply(hg,MARGIN=1,count,vect = diff.t)
  if(d[1] != D)
  {
    result$response<-'Not a Difference Set - Some Elements are Equal Mod H'
  }
  if(all(d[-1]%in%delta))
  {
    result$res<-TRUE
    result$response<-''

  }else
  {
    result$response<- 'Not a Difference Set - Not All Elements Appear equally in the Difference Table.'

  }
  return(result)
}


#Function     - build.ds()
#Desctription - Takes a vector of integers and a group.
#               It then calls is.diff.set and determines
#               if the given vector is a difference set.
#               Finally, it constructs a difference set
#               object and returns it.
#Version      - 0.1:  assumes group is a group object based
#               on an integer cyclic group.

#'@title Construct a Difference Set Object
#'
#'@description Takes a vector of n integers and a group object
#'then it calls is.diff.set, if it passes, the function constructs
#'a difference set object for building a frame.
#'
#'@param ds a vector of integers
#'
#'@param h a group object (built via build.group function)
#'
#'@examples
#'build.ds(ds = as.integer(c(0,1,3), h=group)
#'
#'@export
build.ds<-function(ds,h)
{
  test<-is.diff.set(ds,h)
  if(!test$res)
  {
    stop(paste('Failed',test$response,sep = ':'))
  }
  dset<-new("Difference Set", set =ds, group = h)
  return(dset)

}
