showbet50<- function(m, w, wi, s, si){
  q<- wi/w
  s1<- 10+183*(q^2)-135*(q^3)-(11*si)/((q*s)-(0.80*si))
  if(s <= 1200){
    a<- (m/50)*(s/1200)*s1
    round(a, digits = 2)
  } else if(s >= 6000){
    a<- (m/50)*s1
    round(a, digits = 2)
  } else(print("No Bet"))
  }

placebet50<- function(m,w,wi,p,pi){
  q<- wi/w
  p1<-(39*q)+(52*(q^2))-((25*q*pi)/((q*p)-(0.75*pi)))
  if(p<2000){
    a<-(m/50)*(p/2000)*p1
    round(a, digits = 2)
  } else if(p> 6000 && p < 100000){
    a<-(m/50)*p1
    round(a, digits = 2)
  } else print("no bet")
}



