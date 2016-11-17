#1.1

myspline=function(X,Y, knots){
  m=length(knots)
  n=length(X)
  H=matrix(0,nrow=n,ncol=m+2) #new features
  H[,1]=1
  H[,2]=X
  for(i in 1:m){
    for(j in 1:n){
      #MISSING: Insert equation computing basis function values from X values.
    }
  }
  #MISSING: use H matrix and Y in order to get the optimal basis function coefficients 'beta'
  
  #predicted values
  Yhat=H%*%beta
  
  #MISSING: plot the original and predicted data in one graph

}