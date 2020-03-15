#Functions List:
#               1.  is.bibd()
#               2.  is.hadamard()




#Function     - is.bibd
#Desctription - takes in a matrix, checks to see if it is the
#               adjacency matrix of a (V,K,Lambda = 1) matrix.
#               specifically, that there is only one point of
#               overlap between columns, and that it has the
#               appropriate number of columns and rows, as well
#               as determining that all values (B,V,K,R) are
#               integers.
#Version      - 0.1:  internal use only

#'@title          Check if a matrix is a BIBD
#'
#'@description    Takes a matrix and determines if it meets
#'the requirements for the adjacency matrix of a BIBD(V,K,1).
#'This means that it has V columns, B rows, and only 1,0 values.
#'Each column has a 1 in exactly K rows and any two columns have
#'a 1 in exactly 1 shared row.
#'
#'@param bibd a {0,1}-valued integer matrix.
#'
#'@examples
#'is.bibd(bibd = matrix)

is.bibd<-function(bibd)
{
  result<-list(res = FALSE,response = 'Null, function failed',v=0,k=0,lambda=0,b=0,r=0)
  if(!is.matrix(bibd)){
    result$response<-"BIBD must be a matrix"
    return(result)
  }
  if(!is.integer(bibd))
  {
    result$repsonse<-"BIBD matrix must be integer-valued"
    return(result)
  }
  if(max(bibd)!=1)
    {
    result$response<-'BIBD matrix must be 0,1-valued'
    return(result)
  }
  if(min(bibd)!=0)
  {
    result$response<-'BIBD matrix must be 0,1-valued'
    return(result)
  }
  k<-unique(rowSums(bibd))
  b<-length(bibd[,1])
  v<-length(bibd[1,])
  r<-unique(colSums(bibd))
  if(length(k)!=1)
  {
    result$response<-'Rows do not contain the same number of points'
    return(result)
  }
  if(length(r)!=1)
  {
    result$response<-'Columns do not contain the same number of points'
    return(result)
  }
  if(b*k!=v*r)
  {
    result$response<-'B*K does not equal V*R'
    return(result)
  }
  if(r*(k-1)/(v-1)!=1)
  {
    result$response<-'Vectors share more than one point of overlap'
  }
  else
  {
    result$res<-TRUE
    result$response<-''
  }
  return(result)
}

#Function     - is.hadamard
#Desctription - takes in a matrix, checks to see if it is a
#               hadamard matrix:  square, all values are equal
#               to 1 in modulus (|x|^2=1), and all columns are
#               orthogonal.
#Version      - 0.1:  internal use only

#'@title          Check if a matrix is a hadamard
#'
#'@description    Takes a matrix and determines if it is a
#'hadamard matrix (possibly complex, square matrix with
#'entries all equal 1 modulus)
#'
#'@param hmat A square matrix.
#'
#'@examples
#'is.hadamard(hmat = matrix)
is.hadamard<-function(hmat)
{
  result<-list(res = FALSE,response = 'Null, function failed',v=0,k=0,lambda=0,b=0,r=0)
  if(!is.matrix(hmat))
  {
    result$response<-"Hadamard must be a matrix"
    return(result)
  }
  if(length(hmat[,1])!=length(hmat[1,]))
  {
    result$response<-'Hadamard Matrix must be square'
    return(result)
  }
  size<-length(hmat[,1])
  nI<-size*diag(size)
  ID_test<-hmat%*%Conj(t(hmat))- nI
  if(norm(ID_test,type = '2')>10^(-7))
  {
    result$response<-'Columns of the Hadamard matrix are not orthogonal'
    return(result)
  }
  mod_test<-c(Mod(hmat))
  if(length(unique(mod_test))!=1)
  {
    result$response<-'Hadamard Matrix entries are not all equal to modulus 1'
    return(result)
  }
  if(unique(mod_test)!=1)
  {
    result$response<-'Hadamard Matrix entries are not all equal to modulus 1'
  }
  else
  {
    result$res<-TRUE
    result$response<-''
  }
  return(result)
}
