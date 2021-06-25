Wr=get_Wr(B=B,T=T,m=m, p=8, n_matter=10, bloque = "X")$Wr
Wr=Wr %>% data.frame()

library(nleqslv)

YY =rnorm(nrow(Wr)) ### Respuesta
beta0=0
beta= numeric(ncol(Wr)-3)
sigma2=1
delta2=1
Theta=c(beta0,sigma2, delta2, beta)
XY=cbind(YY,Wr[-c(1,2,ncol(Wr))])
n_em=nrow(XY)
etiqu_falt = rowSums(apply(XY, 2,is.na))>0
X_obs=XY[!etiqu_falt,-1]
Y_obs=XY[!etiqu_falt,1]
X_mis=XY[etiqu_falt,-1]
Y_mis=XY[etiqu_falt,1]


Qtheta = function(Theta){
  sigma2=abs(Theta[2])
  delta2=abs(Theta[3])
  beta0=Theta[1]
  beta=Theta[-c(1,2,3)]
  xitxi=0
  for(fila in i:nrow(X_obs)){
    xitxi=t(t(X_obs[i,]))%*%(t(X_obs[i,]))
  }
  -n_em*log(sigma2)-n_em*log(delta2)-
    sum((Y_obs-beta0-as.matrix(X_obs)%*%t(t(beta)))^2)/(2*sigma2)-
  xitxi/(2*delta2)-(nrow(X_obs)-nrow(X_mis))*(delta2*t(beta)%*%(
    diag(length(beta))-delta2^2*t(t(beta))%*% t(beta)/c(sigma2+delta2*t(beta)%*% t(t(beta))))%*%t(t(beta)))-
    (Y_obs-beta0-(c(delta2*t(beta)%*% t(t(beta)))*(Y_obs-beta0))/c(sigma2+delta2*t(beta)%*% t(t(beta)))^2)
    
}