# Julio Huato

rm(list = ls()) # To remove all objects from the R Environment
# THE SIMPLE INCOME-DETERMINATION MODEL AS AN R function

# This is a R function that requires us to input the values of the parameter
# c and of the exogenous variables Cbar, Ibar, Gbar, Tbar, XQbar, and the
# function returns the equilibrium values: Ystar, YDstar, Cstar, and Sstar.
# The exogenous variables are in monetary units and so are the equilibrium
# levels of income (Ystar), consumption spending (Cstar), and saving (Sstar).
# Note that the default value of XQbar is set to 0. That means that one does not
# need to input XQbar if it is indeed 0.  Only if it is not 0, one should type it in.
incdet <- function(c, Cbar, Ibar, Gbar, Tbar, XQbar = 0) {
# The next few lines help the user of the function to make sure the data meet the 
# macroeconomic restrictions of the model.
      ifelse( c <= 0 | c >=1,
           print(paste("Check the data!  The marginal propensity to consume c must be a positive number smaller than 1.")),
           ifelse( Cbar <0 | Ibar <0 | Gbar <0 | Tbar <0 | XQbar < 0,
                   print(paste("Check the data! Cbar, Ibar, Gbar, Tbar, and X_Qbar must be be non-negative.")),
                   print("The data seem fine."))
   )  
# The next few lines determine the multipliers:
   asm = 1/(1-c) # Autonomous spending multiplier
   ntm = - c/(1-c) # Net taxes multiplier
# These lines determine the equilibrium levels of income,
# consumption spending, and saving:
   Ystar = asm*(Cbar + Ibar + Gbar + XQbar) + ntm *Tbar
   Cstar = Cbar + c*(Ystar - Tbar)
   Sstar = Ystar - Tbar - Cstar
# These lines report the results:
   print(paste("The autonomous spending multiplier is", asm ))
   print(paste("The net-tax multiplier is", ntm ))
   print(paste("The equilibrium income level is", Ystar, "monetary units."))
   print(paste("The equilibrium disposable income level is", Ystar - Tbar, "monetary units."))
   print(paste("The equilibrium consumption spending level is", Cstar, "monetary units."))
   print(paste("The equilibrium saving level is", Sstar, "monetary units."))
}

# Example
# Let c = .8, Cbar=20, Ibar=40, Gbar=50, Tbar=50, XQbar=0 (in $ billions)
# One could type the command as:
# incdet(c=.8, Cbar=20, Ibar=40, Gbar=50, Tbar=50, XQbar=0), but
# one can just type the following and obtain the same results:
incdet(.8, 20, 40, 50, 50)


rm(list = ls()) # To remove all objects from the R Environment
# R FUNCTION TO DETERMINE THE CHANGES IN THE INCOME-DETERMINATION MODEL

# This is a more general R function.  In addition to 
# c, Cbar, Ibar, Gbar, Tbar, X_Qbar, on must also input 
# any changes in the parameter c (i.e. dc) or changes in these exogenous 
# variables: dCbar, dIbar, dGbar, dTbar, dX_Qbar.
# Then, the function returns Ystar, YDstar, Cstar, and Sstar before the change 
# (Ystar0, YDstar0, Cstar0, Sstar0),
# the changes in Ystar, Cstar, and Sstar caused by the change(s) in 
# the exogenous variable(s), i.e.
# dYstar, dCstar, and dSstar, as well as the new levels and after (1) the change(s).

# You must type the parameter change and/or the exogeneous change(s) 
# in the R function.  See the example:
dincdet <- function(c0, Cbar0, Ibar0, Gbar0, Tbar0, XQbar0=0, 
                    dc=0, dCbar=0, dIbar=0, dGbar=0, dTbar=0, dXQbar=0) {
   ifelse( c0 <= 0 | c0 >=1,
           print(paste("Check the data!  The marginal propensity to consume c must be a positive number smaller than 1.")),
           ifelse( Cbar0<0 | Ibar0<0 | Gbar0<0 | Tbar0<0 | XQbar0<0,
                   print(paste("Check the data! Cbar, Ibar, Gbar, Tbar, and X_Qbar must be be positive.")),
                   print("The data seem fine."))
   )  
   Ystar0 = (1/(1-c0))*(Cbar0 + Ibar0 + Gbar0 + XQbar0) - (c0/(1-c0))*Tbar0
   Cstar0 = Cbar0 + c0*(Ystar0 - Tbar0)
   Sstar0 = Ystar0 - Tbar0 - Cstar0
   dYstar = (1/(1-(c0+dc)))*(dCbar + dIbar + dGbar + dXQbar)-((c0 + dc)/(1-(c0+dc)))*dTbar
   Ystar1 = Ystar0 + dYstar
   Cstar1 = (Cbar0+dCbar) + (c0+dc)*(Ystar1 - (Tbar0+dTbar))
   Sstar1 = Ystar1 - (Tbar0+dTbar) - Cstar1
   print(paste("The initial autonomous spending multiplier is", 1/(1-c0)))
   print(paste("The final autonomous spending multiplier is", 1/(1-c0-dc)))
   print(paste("The initial net-tax multiplier is", -c0/(1-c0)))
   print(paste("The final net-tax multiplier is", -(c0+dc)/(1-c0-dc)))
   print(paste("The initial equilibrium income level is", Ystar0, "monetary units."))
   print(paste("The change in the equilibrium income level is", dYstar, "monetary units."))
   print(paste("The final equilibrium income level is", Ystar1, "monetary units."))
   print(paste("The initial equilibrium disposable income level is", Ystar0 - Tbar0, "monetary units."))
   print(paste("The final equilibrium disposable income level is", Ystar1 - Tbar0 - dTbar, "monetary units."))
   print(paste("The initial equilibrium consumption spending level is", Cstar0, "monetary units."))
   print(paste("The final equilibrium consumption spending level is", Cstar1, "monetary units."))
   print(paste("The initial equilibrium saving level is", Sstar0, "monetary units."))
   print(paste("The final equilibrium saving level is", Sstar1, "monetary units."))
}

# Example
# Let c = .8, Cbar=20, Ibar=40, Gbar=50, Tbar=50, X_Qbar=0 (in $ billions), and dGbar=4
# One could type: dincdet(c0=.8, Cbar0=20, Ibar0=40, Gbar0=50, Tbar0=50, XQbar0=0, 
#                          dc=0, dCbar=0, dIbar=0, dGbar=4, dTbar=0, dXQbar=0)
# But, more simply, one can type:
dincdet(.8, 20, 40, 50, 50, dGbar=4)

