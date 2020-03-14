#Functions List:
#               1.  frame.ds()
#               2.  frame.stein()

#'@title          Construct a Frame From a Difference Set
#'
#'@description    Builds the character table from the
#'given group and extracts the rows associated with the
#'given difference set.
#'
#'@param ds a difference set object constructed via
#'build.ds function
#'
#'@examples
#'frame.ds(diff.set.object)
#'
#'@export


#Function     - frame.ds
#Desctription - takes in a difference set, constructs the
#               character table, and extracts appropriate
#               rows.  It then builds a frame object and
#               returns it.
#Version      - 0.1:  Dependent on difference set object.

frame.ds<-function(ds)
{
  if(class(ds)[1]!='Difference Set')
  {
    stop(paste('Failed: ds is not a Difference Set Object'))
  }
  #take difference set and group elements from ds object
  set<-ds@set+1
  grp<-ds@group

  #build character table
  len<-grp@modulus
  G<-grp@group
  cht<-matrix(exp(2i*pi*G%*%t(G)/len),ncol=len)


  #extract rows and save descriptors
  dimsn<-length(set)
  phi<-matrix(cht[set,],nrow = dimsn)


  #save object
  Phi<-new('Frame', matrix = phi, dimension = dimsn, size = len)

  #return object
  return(Phi)

}
#'@title Construct a Frame via Steiner Method
#'
#'@description Builds a Frame by embedding the
#'rows of the given hadamard matrix into the
#'adjacency matrix of the given BIBD(V,K,1).
#'
#'@param bibd The adjacency matrix for a (V,K,1)
#'bibd where the columns represent the vectors
#'and the rows represent the blocks.
#'
#'@param had A hadamard matrix (square matrix with
#'every entry equal to 1 modulus) where the number
#'of rows and columns are equal to the BIBD value
#'value for K - the number of blocks that contain
#'a given vector.
#'
#'@examples
#'frame.stein(bibd,had)
#'
#'@export
#Function     - frame.stein
#Desctription - takes in a BIBD(V,K,1) and a hadamard matrix
#               correct size and generates a frame from it.
#Version      - 0.1:  Cannot generate BIBDs or Hadamard matrices
#               but can check them.

frame.stein<-function(bibd,had)
{
  bibtest<-is.bibd(bibd)
  hadtest<-is.hadamard(had)
  if(!bibtest$res)
  {
    stop(paste('Failed',bibtest$response,sep=': '))
  }
  if(!hadtest$res)
  {
    stop(paste('Failed',hadtest$response,sep=': '))
  }
  v<-length(bibd[1,])
  b<-length(bibd[,1])
  r<-sum(bibd[,1])
  k<-length(had[,1])
  if(r!=k)
  {
    stop('Failed: Hadamard dimension must be equal to R, the number
         of blocks a given vector is adjacent to in the adjancency
         matrix')
  }
  vects<-v*k
  dim<-b
  stein<-matrix(rep(0,vects*dim),ncol=vects)
  for(j in 1:length(bibd[1,]))
  {
    lp<-1
    for(i in 1:length(bibd[,1]))
    {
      if(bibd[i,j]==1)
      {
        colstart<-k*(j-1)+1
        cols<-colstart:(colstart+k-1)
        stein[i,cols]<-had[lp,]
        lp<-lp+1
      }
    }
  }
  Phi<-new('Frame',matrix = stein,dimension = b,size = vects)
  return(Phi)
}
