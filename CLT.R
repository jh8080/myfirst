
set.seed(1234)            # setting random seed
nit=10000;                # number of Monte Carlo iterations
nvec=c(10,30,50,1000)     # sample sizes
# Type 1: N(0,1)
# Type 2: Student-t(df=3)
# Type 3: Chisq(df=3)
# Type 4: uniform(0,1))
Type=3

# Simulation begins
mat=matrix(NA, nrow=nit,ncol=length(nvec))
for (i in 1:nit){
  for (j in 1:length(nvec)){
    if( Type == 1) x=rnorm(nvec[j])
    if( Type == 2) x=rt(nvec[j],df=3)
    if( Type == 3) x=(rchisq(nvec[j],df=3)-3)
    if( Type == 4) x=(runif(nvec[j])-0.5)
    mat[i,j]= mean(x)/(sd(x)/sqrt(nvec[j]))
  }
}

# Plotting 
par(mfrow=c(2,2))
par(mar = c(2, 2, 2, 2)) # Set the margin on all sides to 2
sz = paste("n =",nvec,"")
for(i in 1:length(nvec)){
plot(density(mat[,i]),xlim=c(-3,3),ylim=c(0,0.5),main=sz[i],xlab="",col="red",lwd=2)
curve(dnorm(x, mean=0, sd=1), 
      col="black", lwd=2, add=TRUE, yaxt="n")}
