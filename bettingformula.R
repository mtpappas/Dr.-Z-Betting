#Optimal Bets with up to $50 wealth


#use when show pool is $1200 or less

smshow50<- function(m, w, wi, s, si){
  q<- wi/w
  s1<- 10+183*(q^2)-135*(q^3)-(11*si)/((q*s)-(0.80*si))
  a<- (m/50)*(s/1200)*s1
  round(a, digits = 2)
}
  
#use when pool is between 6000 - 100000

medshow50<- function(m,w,wi,s,si){
  q<- wi/w
  s1<- (10+183*(q^2))-(135*(q^3))-((11*si)/((q*s)-(0.80*si)))
  a<- (m/50)*s1
  round(a, digits = 2)
}

#Optimal Place Betting Amounts

#use when place pool is up to $2000
smplace50<- function(m,w,wi,p,pi){
  q<-wi/w
  p1<- (39*q)+(52*(q^2))-(25*q*pi)/((q*p)-(0.75*pi))
  a<-(m/50)*(p/2000)*p1
  round(a, digits = 2)
}


#use when place pool is $6000 to $100000
medplace50<- function(m,w,wi,p,pi){
  q<-wi/w
  p1<- (39*q)+(52*(q^2))-(25*q*pi)/((q*p)-(0.75*pi))
  a<-(m/50)*p1
  round(a, digits = 2)
}

