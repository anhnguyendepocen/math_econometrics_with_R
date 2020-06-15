# Julio Huato
# 5/13/2020
# Linear Production Possibilities Frontier

rm(list = ls())

# PPF single economy
ppf <- function(A0, A1, B0, B1) {
  if(A1 > A0 & B1 <= B0){
    print(paste("Resources or productivity cannot be nfinite! Check your data!"))
  }  
  dA = A1 - A0
  dB = B1 - B0
  mcA= abs(dB/dA) # Marginal cost of A
  mcB= abs(dA/dB) # Marginal cost of B
  cost=data.frame(mcA, mcB)
  return(cost)
}

# Example: A0=100, A1=0, B0=0, B1=40
ppf(100, 0, 0, 40)

rm(list = ls())

# PPF two economies
ppf2 <- function(Ah0, Ah1, Bh0, Bh1, Af0, Af1, Bf0, Bf1) {
  if(Ah1 > Ah0 & Bh1 <= Bh0 | Af1 > Af0 & Bf1 <= Bf0 ){
    print(paste("Resources or productivity cannot be infinite! Check your data!"))
  }  
  dAh = Ah1 - Ah0
  dBh = Bh1 - Bh0
  dAf = Af1 - Af0
  dBf = Bf1 - Bf0
  vinterch = Bh0 - (dBh/dAh)*Ah0
  vintercf = Bf0 - (dBf/dAf)*Af0
  hinterch = -vinterch/(dBh/dAh)
  hintercf = -vintercf/(dBf/dAf)
  mcAh= abs(dBh/dAh) # Marginal cost of A
  mcAf= abs(dBf/dAf) # Marginal cost of A
  print(paste("The marginal cost of A at Home is", mcAh))
  print(paste("The marginal cost of B at Home is", mcAh^-1))
  print(paste("The marginal cost of A at Foreign is", mcAf))
  print(paste("The marginal cost of B at Foreign is", mcAf^-1))
  ifelse( (vinterch > vintercf) & (hinterch > hintercf),
          print("Home has an absolute advantage over Foreign"), 
          ifelse( (vinterch < vintercf) & (hinterch < hintercf),
                  print("Foreign has an absolute advantage over Home"),
                  print("No economy has an absolute advantage over the other."))
          )
  ifelse( mcAh < mcAf, print("Home has a comparative advantage in producing good A (and a comparative disadvantage in producing B)."), 
          ifelse( mcAh == mcAf, print("No economy has any comparative advantage."), 
                  print("Foreign has a comparative advantage in producing good A (and a comparative disadvantage in producing B)") ) )
    }

# Example: Ah0=100, Ah1=0, Bh0=0, Bh1=40, Af0=50, Af1=0, Bf0=0, Bf1=125 
ppf2(100, 0, 0, 40, 50, 0, 0, 125)

# Example: Ah0=100, Ah1=0, Bh0=0, Bh1=40, Af0=50, Af1=0, Bf0=0, Bf1=20
ppf2(100, 0, 0, 40, 50, 0, 0, 25)
