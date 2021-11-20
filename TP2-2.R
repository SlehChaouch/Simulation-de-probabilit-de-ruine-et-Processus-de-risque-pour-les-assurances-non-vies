U=100 # réserve initiale
c=1000 # montant de cotisation
lambda=10 #moyenne de nombre des sinistres déclarées par jour
mu=90 #moyenne des montants de remboursements
pareto=function(x,alpha,x0)
{if(x<x0) {return(list(0))}
  else {return(list((alpha*x0^alpha)/x^(alpha+1)))}  
}
tr = function(U,c,lambda,mu)
{
  
  # Determination du nombre de sinistre durant une année 
  a=rexp(6000,lambda)#generation de 5000 V.A exponentielle i.i.d de parametre lambda = 10
  # qui modelisent le temps entre les sinistres
  a=cumsum(a)
  n=min(which(a>365))-1#le nombre de sinistre durant une année
  
  t=1:365 # Vecteur temps
  R=c(U) #les R[T_n]
  nbr=c()#vecteur contenant le nombre d'accident par jour
  
  
  a=a[1:n]
  
  Rt=c(U)
  
  for(i in 2:n)
  {
    Rt[i]=Rt[i-1]+(a[i]-a[i-1])*c-as.numeric(pareto(46,2,mu/2))
    if(Rt[i]<=0)
    {return(list(i))}
  }
  
  return(0)
  
  
}
N=100
u=seq(50,1000,length.out = 50)
t=c()
k=0
M=c()
for(l in 1:length(u))
{for (i in 1:length(u))
  {
  if(tr(u[i], c=1000, lambda = 10, mu=90)!=0)
  {t[k]=as.numeric(tr(u[i], c=1000, lambda = 10, mu=90))
   k=k+1}
}

M[l]=mean(t)}
plot(u,M,type='l')