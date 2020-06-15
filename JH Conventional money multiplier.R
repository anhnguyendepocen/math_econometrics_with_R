# Julio Huato
# 5/12/2020
# The conventional money multiplier

# Primary data
B <- 100 # Base money (funds held as federal reserves)
r <- .2 # Required reserve ratio
dB <-  # Change in B from new monetary policy (open-market ops)
# If the Fed buys T's in the open markets paying with reserves: dB>0
# If the Fed sells T's in the open markets charging reserves: dB<0
  
# Money multiplier
m <- 1/r
m

# Total checkable deposits: M
M <- m*B
M  

# Change in total deposits
dM <- m*dB
dM

# To remove all objects
rm(list = ls())

# Level function
M_supply <- function(B, r) {
  if(r <= 0 | r >= 1){
    print(paste(r, " must be positive and less than 1."))
  }  
  m = 1/r
  M = m*B
  print(paste("The money multiplier is", m))
  print(paste("The money supply is", M))
}

# Example: B=100, r=.2
M_supply(100, .2)

# To remove all objects
rm(list = ls())

# Change function: fiscal policy (dGbar)
dM_supply <- function(B, r, dB) {
  if(r <= 0 | r >= 1){
    print(paste(r, " must be positive and less than 1."))
  }  
  m = 1/r
  M=m*B
  dM = m*dB
  print(paste("The money multiplier is", m))
  print(paste("The change in the money supply is", dM))
  print(paste("The old money supply was", M))
  print(paste("The new money supply is", M+dM))
}

# Example: Cbar=20, Ibar=40, Gbar=40, Tbar=40, X_Qbar=0, c=.6, dGbar = 2
dM_supply(100, .2, 4)
