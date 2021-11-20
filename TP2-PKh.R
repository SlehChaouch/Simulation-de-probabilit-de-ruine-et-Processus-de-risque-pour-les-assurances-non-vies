
#pareto=function(x,alpha,x0)
#{if(x<x0) {return(list(0))}
#  else {return(list((alpha*x0^alpha)/x^(alpha+1)))}  
#}


sim = function(U,c,lambda,mu)
{
  
  # Determination du nombre de sinistre durant une année 
  a=rexp(6000,lambda)#generation de 5000 V.A exponentielle i.i.d de parametre lambda = 10
  # qui modelisent le temps entre les sinistres
  a=cumsum(a)
  n=min(which(a>365))-1#le nombre de sinistre durant une annéeas
  
  t=1:365 # Vecteur temps
  R=c(U) #les R[T_n]
  nbr=c()#vecteur contenant le nombre d'accident par jour
  
  
  a=a[1:n]
  
  Rt=c(U)
  L=c() #vecteur contenant Li
  dif=c(U) #vecteur contenant les nouvelles valeurs de U initialisé à chaque ruine par rapport au dernier U
  k=1
  for(i in 2:n)
  {
    Rt[i]=Rt[i-1]+(a[i]-a[i-1])*c-rexp(1,1/mu)
    if(Rt[i]<dif[k])
    {L[k]=dif[k]-Rt[i]
     k=k+1
     dif[k]=dif[k-1]-L[k-1]}
  }
  
  if(sum(L)>U){return(list(1))}
  else {return(list(0))}
  
  
}

N=1000
sum=0
for(i in 1:N)
{sum=sum+as.numeric(sim(U = 100, c=1000, lambda = 10, mu=90))}
sum/N


