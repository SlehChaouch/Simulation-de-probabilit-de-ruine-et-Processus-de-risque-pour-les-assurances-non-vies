U=100 # réserve initiale
c=1000 # montant de cotisation
lambda=10 #moyenne de nombre des sinistres déclarées par jour
mu=90 #moyenne des montants de remboursements
sim = function(U,c,lambda,mu)
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
                Rt[i]=Rt[i-1]+(a[i]-a[i-1])*c-rexp(1,1/mu)
                if(Rt[i]<=0)
                {return(list(1))}
        }
        
        return(list(0))
        
        
}
N=1000
sum=0
f=seq(50,1000,length.out = 50)
M=c()
for (j in 1:length(f))
{for (i in 1:N)
{sum = sum + as.numeric((sim(f[j], c=1000, lambda = 10, mu=90)))}
        M[j]=sum/N}