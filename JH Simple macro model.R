# Julio Huato
# 5/11/2020

# The simple macroeconomic model
# based on 
# (1) Keynes, John Maynard (1936/2018), 
# The General Theory of Employment, 
# Interest, and Money. Springer, and
# (2) Hicks, John R. (1937). 
# "Mr. Keynes and the 'Classics': 
# A Suggested Interpretation. 
# Econometrica, pp.147-159.

# Primary data
Cbar <- 100
c <- .9
Ibar <- 160
Gbar <- 140
Tbar <- 140
X_Qbar <- 0

# Aggregate autonomous spending
AS <- (Cbar + Ibar + Gbar + X_Qbar)
AS

# Autonomous spending multiplier
Am <- 1/(1-.9)
Am

# Tax multiplier
Tm <- -(.9/(1-.9))
Tm

# Equilibrium income
Ystar <- Am*AS+Tm*Tbar
Ystar

# Equilibrium consumption
Cstar <- Cbar + c*(Ystar-Tbar)
Cstar

# Equilibrium saving
Star <- Ystar - Cstar - Tbar
Star

# Fiscal policy data
dGbar <- 4
dTbar <- -4

# Effect of delta G on Y*
dYstarG <- Am*(dGbar)
dYstarG

# Effect of delta T on Y*
dYstarT <- Tm*(dTbar)
dYstarT

# To remove all objects
rm(list = ls())

# Level function
macro_eq <- function(Cbar, Ibar, Gbar, Tbar, X_Qbar, c) {
  # To use the function, type dmacro_eq(Cbar, Ibar, 
  # Tbar, X_Qbar, c). Make sure 0 < c < 1.
  if(c <= 0 | c >= 1){
    print(paste(c, " must be positive and less than 1."))
  }  
  A = (Cbar + Ibar + Gbar + X_Qbar)
  Am = 1/(1-c)
  Tm = -c/(1-c)
  Ystar = Am*A + Tm*Tbar
  Cstar = Cbar + c*(Ystar-Tbar)
  Star = Ystar - Tbar - Cstar
  print(paste("The autonomous-spending multiplier is", Am))
  print(paste("The tax multiplier is", Tm))
  print(paste("The equilibrium income level is Y*=", Ystar))
  print(paste("The equilibrium disposable-income level is Y*_d=", Ystar-Tbar))
  print(paste("The equilibrium level of consumption spending is C*=", Cstar))
  print(paste("The equilibrium level of household saving is S*=", Star))
  print(paste("Government borrowing (deficit) is B_g=", Gbar-Tbar))
  print(paste("Firm borrowing is B_f=", Ibar))
  print(paste("ROW borrowing is B_row=", X_Qbar))
  }

# Example: Cbar=20, Ibar=40, Gbar=40, Tbar=40, X_Qbar=0, c=.6
macro_eq(Cbar=20, Ibar=40, Gbar=40, Tbar=40, X_Qbar=0, c=.6)

# To remove all objects
rm(list = ls())

# Change function: fiscal policy (dGbar)
dmacro_eq <- function(Cbar, Ibar, Gbar, Tbar, X_Qbar, c, dGbar) {
  # To use the function, type dmacro_eq(Cbar, Ibar, 
  # Tbar, X_Qbar, c, dGbar). Make sure 0 < c < 1.
  if(c <= 0 | c >= 1){ 
    print(paste(c, " must be positive and less than 1."))
  }  
  A = (Cbar + Ibar + Gbar + X_Qbar)
  Am = 1/(1-c)
  Tm = -c/(1-c)
  Ystar = Am*A + Tm*Tbar
  Cstar = Cbar + c*(Ystar-Tbar)
  Star = Ystar - Tbar - Cstar
  econ_eq <- list(A, Am, Tm, Ystar, Cstar, Star)
  dYstar_dGbar <- Am*dGbar 
  print(paste("The change in Y* caused by dGbar is dY*=", dYstar_dGbar))
  print(paste("The old equilibrium income level is Y*_0=", Ystar))
  print(paste("The new equilibrium income level is Y*_1=", Ystar+dYstar_dGbar))
}

# Example: Cbar=20, Ibar=40, Gbar=40, Tbar=40, X_Qbar=0, c=.6, dGbar = 2
dmacro_eq(20, 40, 40, 40, 0, .6, 2)

# To remove all objects
rm(list = ls())

# Change function: fiscal policy (dTbar)
dmacro_eq <- function(Cbar, Ibar, Gbar, Tbar, X_Qbar, c, dTbar) {
  # To use the function, type dmacro_eq(Cbar, Ibar, 
  # Tbar, X_Qbar, c, dTbar). Make sure 0 < c < 1.
  if(c <= 0 | c >= 1){
    print(paste(c, " must be positive and less than 1."))
  }  
  A = (Cbar + Ibar + Gbar + X_Qbar)
  Am = 1/(1-c)
  Tm = -c/(1-c)
  Ystar = Am*A + Tm*Tbar
  Cstar = Cbar + c*(Ystar-Tbar)
  Star = Ystar - Tbar - Cstar
  econ_eq <- list(A, Am, Tm, Ystar, Cstar, Star)
  dYstar_dTbar <- Tm*dTbar 
  print(paste("The change in Y* caused by dTbar is dY*=", dYstar_dTbar))
  print(paste("The old equilibrium income level is Y*_0=", Ystar))
  print(paste("The new equilibrium income level is Y*_1=", Ystar+dYstar_dTbar))
}

# Example: Cbar=20, Ibar=40, Gbar=40, Tbar=40, X_Qbar=0, c=.6, dTbar = -2
dmacro_eq(20, 40, 40, 40, 0, .6, -2)
