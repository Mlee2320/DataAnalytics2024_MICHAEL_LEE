cv.folds <- function(n,folds=3)
 ## randomly split the n samples into folds
 {
  split(sample(n),rep(1:folds,length=length(y)))
 }
svp <- ksvm(x,y,type="C-svc",kernel="vanilladot",C=1,scaled=c(),cross=5)
print(cross(svp))

