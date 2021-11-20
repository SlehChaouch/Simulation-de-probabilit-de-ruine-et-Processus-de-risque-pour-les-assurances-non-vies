nu=90 #moyenne de s montants de remboursements
c=1000# montant de cotisation
lambda=10 #moyenne de nombre des sinistres declarees par jour
n=4000 #nombre de simulations
u=0 
V=365 #nombre de jours
delta=rexp (n, (1/lambda))# les durees inter???arrives
T_n=c(0,cumsum (delta))# temps d'arrivees des sinistres
t =1:(n+1)
T_n= V *T_n/max(T_n) #normaliser la distribution au cours d'un an
which (T_n<=t )
N_t=sum (T_n<=t ) # nombre totale de sinistres
Z_n=rexp (N_t, ( 1 / nu))# montants de remboursement
T_n=T_n[ which (T_n<=t ) ] # temps d'arrivees des sinistres

R_Tn=rep(0,N_t) # valeurs de Rt aux instants T_n
for(i in 1:(N_t-1))#
{
  R_Tn[ i +1]=R_Tn[ i ]+ c * ( (T_n[ i +1]-T_n[ i ]))-Z_n[ i ]
}

k=10 # nombre d'instants entre les sinistres
T_t=0
# discretisation du temps entre les temps d'arrives des sinistres T_i
for(i in 1 :N_t)
{
  T_t=c (T_t, seq (T_n[ i ] ,T_n[ i +1], length.out= k ) )
}
T_t=unique (T_t)

R_t=rep ( 0 , length(T_t ) )
for(i in T_n)
{
  R_Tn[ which (T_n==i ) ] = R_t [ which (T_t==i ) ]
} # R_t = R_Tn aux instants Tn
#calcul des R_t aux instants entre les T_i
b=seq(1, length(T_t ), k-1) #indices des T_n dans T_t
m=b[1]
h=1:length (R_Tn) #indice des T_n
d=1 #compteur de vecteur d
j=h[1] 
for(i in 2:(length(R_t)-1) )
{if( ( ( b [ d+1]> i ) ) & ( b [ d]< i ) )
  {R_t [ i ]=R_Tn[ j ]+ c *(T_t [ i ]-T_n[ j ] ) }
  else # si on n'est pas entre les instants T_n e t T_n+1
  {d=d+1
    m=b [ d ]
    j=h [ d ]
  }
}
plot(T_t, R_t, type ='s' , main='processus de risque')