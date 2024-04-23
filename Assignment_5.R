##############################Question 5

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

############################ (a)
A.lap = diag(rowSums(A)) - A
lambda = eigen(A.lap)
isolated_nodes = which(rowSums(A) == 0)
x = rep(0, 100)
lambda_x = lambda$values[abs(lambda$values) > 1e-10]
i = which.min(abs(lambda_x))
min_eigenvector = lambda$vectors[, i]
non_n = setdiff(1:100, isolated_nodes)
x[non_n] = min_eigenvector[non_n]
x

############################ (b)
corr = cor(x, y)
fisher = 0.5 * log((1 + corr) / (1 - corr))
n = 100 - sum(is.na(x))
se = 1 / sqrt(n - 3)
z = fisher / se
p_value = 2 * (1 - pnorm(abs(z)))
corr
p_value

