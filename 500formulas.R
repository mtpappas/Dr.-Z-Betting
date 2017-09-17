#formulas for wealth between $50 and $500

#use when show pool is less than $1200
xsshow500<- function(m,w,wi,s,si){
  q<- (wi/w)
  s1<- (10+(183*(q^2)))-(135*(q^3))-((11*si)/((q*s)-(0.80*si)))
  s2<- 9+(994*(q^3)) - (464*(q^2)) - (150*q*si)/((q*s)-(0.80*si))
  a<- ((500 - m)/450)*(s/1200)*s1+((m-50)/450)*(s/1200)*s2
  round(a, digits = 2)
}



#use when show pool is $1200 to $6000
smshow500<- function(m,w,wi,s,si){
  q<- (wi/w)
  s1<- (10+(183*(q^2)))-(135*(q^3))-((11*si)/((q*s)-(0.80*si)))
  s2<- 9+(994*(q^3)) - (464*(q^2)) - (150*q*si)/((q*s)-(0.80*si))
  s3<- 86+ 1516*(q^2)- 968*(q^3) - ((90.7*si)/(q*s)-(0.85*si))
  a<- ((500-m)/450)*s1+ ((m-50)/450)*(((6000-s)/4800)*s2)+(((m-50)/450)*((s-1200)/4800)*s3)
  round(a, digits = 2)
}

#Use when show pool is $6000 and $100000
medshow500<- function(m,w,wi,s,si){
  q<- (wi/w)
  s1<- 10+(183*(q^2))-(135*(q^3))-((11*si)/((q*s)-(0.80*si)))
  s3<- 86+ 1516*(q^2)- 968*(q^3) - (90.7*si)/((q*s)-(0.85*si))
  s4<- 131+(2150*(q^2))-(1778*(q^3)) - (150*si)/((q*s)- (0.70*si))
  a<- (((500-m)/450)*s1) + ((m-50)/450)*(((100000-s)/94000)*s3)+((m-50)/450)*((s-6000)/94000)*s4
  round(a, digits = 2)
}

#use when show pool is greater than $100,000
lgshow500<- function(m,w,wi,s,si){
  q<- (wi/w)
  s1<- 10+(183*(q^2))-(135*(q^3))-((11*si)/((q*s)-(0.80*si)))
  s4<- 131+(2150*(q^2))-(1778*(q^3)) - (150*si)/((q*s)- (0.70*si))
  a<- (((500-m)/450)*s1) + ((m-50)/450)*s4
  round(a, digits = 2)
}




