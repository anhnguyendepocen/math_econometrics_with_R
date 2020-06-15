# Julio Huato 
# Competitive market (linear) model 
# with welfare analysis

rm(list = ls())

# PPF single economy
cmequil <- function(a, b, c, d) {
  ifelse( b >= 0, 
          print(paste("Check the data!  Demand's slope must be negative.")), 
                      ifelse( d <=0, 
                              print(paste("Check the data! Supply's slope must be positive.")),
                              print("The data seem fine."))
          )  
  qstar = (c-a)/(b-d)
  pstar = a + b*qstar
  print(paste("The equilibrium quantity exchanged is", qstar, "units."))
  print(paste("The equilibrium market price is", pstar, "$/unit."))
}

# Example: 
# Vertical intercept of the demand curve=120. 
# Slope of the demand curve=-2 (negative!).
# Verticial intercept of the supply curve=20
# (positive or negative, but less than the 
# vertical intercept of the demand curve). 
# Slope of the supply curve (positive)=3.
cmequil(120, -2, 20, 3)

rm(list = ls())

# PPF single economy
cmequilw <- function(a, b, c, d) {
  ifelse( b >= 0, 
          print(paste("Check the data!  Demand's slope must be negative.")), 
          ifelse( d <=0, 
                  print(paste("Check the data! Supply's slope must be positive.")),
                  print("The data seem fine."))
  )  
  qstar = (c-a)/(b-d)
  pstar = a + b*qstar
  revenue = pstar*qstar
  benefitv <- integrate(function(q){a+b*q}, 0, qstar)
  benefit <- benefitv[[1]]
  costv <- ifelse( c >=0, 
                  integrate(function(q){c+d*q}, 0, qstar),
                  integrate(function(q){c+d*q}, -(c/d), qstar))
  cost <- costv[[1]]
  conssurp= benefit - revenue
  prodsurp= revenue - cost
  tsurplus = benefit - cost
  print(paste("The equilibrium quantity exchanged is", qstar, "units."))
  print(paste("The equilibrium market price is $", round(pstar,2),"/unit."))
  print(paste("The equilibrium revenue=expenditure is $", round(revenue, 2)))
  print(paste("The equilibrium buyers' benefit is $", round(benefit, 2)))
  print(paste("The equilibrium sellers' cost is $", round(cost, 2)))
  print(paste("The equilibrium consumer surplus is $", round(conssurp, 2)))
  print(paste("The equilibrium producer surplus is $", round(prodsurp, 2)))
  print(paste("The equilibrium total surplus is $", round(tsurplus, 2)))
}

# Example: 
# Vertical intercept of the demand curve=120. 
# Slope of the demand curve=-2 (negative!).
# Verticial intercept of the supply curve=20
# (positive or negative, but less than the 
# vertical intercept of the demand curve). 
# Slope of the supply curve (positive)=3.
cmequilw(120, -2, 20, 3)

# Example: 
# Vertical intercept of the demand curve=120. 
# Slope of the demand curve=-2 (negative!).
# Verticial intercept of the supply curve=-20
# (positive or negative, but less than the 
# vertical intercept of the demand curve). 
# Slope of the supply curve (positive)=3.
cmequilw(120, -2, -20, 3)

rm(list = ls())
