#Formulas For Expected Dollar Return


#Use this formula to calculate expected dollar return for place bets
explace<- function(w, wi, p, pi, c= 1.14){
  q<- wi/w
  P<- pi/p
  a<- 0.319+0.559*(q/P)
  if(a < c) print("Not a Dr. Z Bet")
  else
    round(a, digits = 2)
}

#Use this formula to calculate expected dollar return for show bets
exshow<- function(w, wi, s, si, c= 1.14){
  q<- wi/w
  S<- si/s
  a<- 0.543+0.369*(q/S)
  if(a<c) print("Not a Dr. Z Bet")
  else
    round(a, digits = 2)
}
