##############################Question 3

#model for generating the edges of A
set.seed(5225)
y=rnorm(100)
alist=matrix(nrow=1,ncol=2,data=0)

for(i in 1:99) for(j in (i+1):100){
  p=exp(-6*abs(y[i]-y[j]))
  success=(runif(1)<p)
  if(success==1) alist=rbind(alist,c(i,j))
}

alist=alist[-1,]
ell=nrow(alist)
ell

#creating the matrix A
A=matrix(nrow=100,ncol=100,data=0)
for(k in 1:ell) A[alist[k,1],alist[k,2]]=1
A=A+t(A)

#removing 10% of the edges of A
bsample=sample(c(1:ell),48,replace=FALSE)
blist=alist[-bsample,]

#creating the matrix of B
B=matrix(nrow=100,ncol=100,data=0)
blength=nrow(blist)
for(k in 1:blength) B[blist[k,1],blist[k,2]]=1
B=B+t(B)

#using B^2 for link prediction
C=B%*%B
pred=C*(1-B)*(1-diag(100))
sort(-pred)[1:96]

#predicting 39 of the missing edges to be those with the largest entries
#of B^2 in which b_ij=0 and i not equal to j.

sum((pred>5)*A)/2

#we only got 8 out of the 39 predictions correct.
#but keep in mind that there are 4515 non-edge node pairs in B and
#only 48 of them are edges in A

################# (a)
library(expm)
alphas = c(0.4, 0.65)
for (alpha in alphas) {
  D = expm::expm(alpha * A)
  pred_a = D * (1 - B) * (1-diag(100))
  sort(-pred_a)[1:96]
  correct_predictions = sum((pred_a > 5) * A)/2
  cat("Alpha =", alpha, "Correct Predictions =", correct_predictions, "\n")
}

################# (c)
node_degrees = colSums(B)
s = matrix(nrow=100, ncol=100, data=0)
for (i in 1:100) {
  for (j in 1:100) {
    s[i, j] = node_degrees[i] * node_degrees[j]
  }
}
pred_c = s * (1 - A) * (1-diag(100))
sort(-pred_c)[1:39]
correct_predictions_c = sum((pred_c > 5) * A)/2
correct_predictions


