# Julio Huato
# 5/13/2020
# Elasticity

rm(list = ls())

# Price-elasticity of demand
p_elast_d <- function(p0, p1, q0, q1) {
  if(p1 > p0 & q1 >= q0){
    print(paste("This violates the law of demand!  As p increases, q must drop."))
  }  
  dq = (q1-q0)
  dp = (p1-p0)
  qbar = (q0+q1)/2
  pbar = (p0+p1)/2
  dslope= dq/dp
  pEd = abs((dslope)*(pbar/qbar))
  print(paste("The slope of the demand curve in the segment is", round(dslope, 3)))
  print(paste("The price elasticity of demand is", round(pEd, 3) ))
  print(paste("In other words, a 1% increase in the price leads to a", round(pEd, 3), "% decrease in the quantity demanded."))
  ifelse(round(pEd, 3) < 1 , print(paste("The demand for this good is price-inelastic.")), 
    ifelse(round(pEd, 3)==1, print(paste("The demand for this good is price unit-elastic.")), 
    print(paste("The demand for this good is price-elastic."))
    )
    )
  }
# Example: p0=20, p1=24, q0=40, q1=38
p_elast_d(20, 24, 40, 38)

# Example: p0=20, p1=24, q0=40, q1=30
p_elast_d(20, 24, 40, 30)

# Example: p0=20, p1=24, q0=40, q1=33.3333
p_elast_d(20, 24, 40, 33+(1/3))


rm(list = ls())

# Income-elasticity of demand
y_elast_d <- function(y0, y1, q0, q1) {
  dq = (q1-q0)
  dy = (y1-y0)
  qbar = (q0+q1)/2
  ybar = (y0+y1)/2
  yshift = dq/dy
  yEd = yshift*(ybar/qbar)
  print(paste("The income-elasticity of demand is", round(yEd, 3) ))
  if( yshift <= 0 ){
    print(paste("This good is inferior. A 1% increase in buyers' income leads to a", round(abs(yEd), 3), "% decrease in the quantity demanded."))
  } else {
    print(paste("This good is superior.  A 1% increase in buyers' income leads to a", round(yEd, 3), "% increase in the quantity demanded."))
  }
}
# Example: p0=20, p1=24, q0=40, q1=38
y_elast_d(20000, 22000, 400, 420)

# Example: p0=20, p1=24, q0=40, q1=38
y_elast_d(20000, 22000, 400, 360)

rm(list = ls())

# Cross-price elasticity of demand
xp_elast_d <- function(pB0, pB1, qA0, qA1) {
  dqA = (qA1-qA0)
  dpB = (pB1-pB0)
  qAbar = (qA0+qA1)/2
  pBbar = (pB0+pB1)/2
  xpshift = dqA/dpB
  xpEd = xpshift*(pBbar/qAbar)
  print(paste("The cross-price elasticity of demand is", round(xpEd, 3) ))
  ifelse( xpshift < 0, print( 
    paste( "A and B are complementary goods. A 1% increase in the price of B leads to a", round(abs(xpEd), 3), "% decrease in the quantity demanded for A." ) ), 
          ifelse( xpshift == 0, print("A and B are unrelated. A 1% increase in the price of B leaves the quantity demanded for A unchanged."),
                  print( paste( "A and B are substitute goods. A 1% increase in the price of B leads to a", round(abs(xpEd), 3), "% increase in the quantity demanded for A." )) 
                  ) 
    )
}
# Example: p0=20, p1=24, q0=40, q1=38
xp_elast_d(10, 14, 100, 120)

# Example: p0=20, p1=24, q0=40, q1=38
xp_elast_d(20000, 22000, 400, 360)

rm(list = ls())

# Price-elasticity of supply
p_elast_s <- function(p0, p1, q0, q1) {
  if(p1 > p0 & q1 <= q0){
    print(paste("This violates the law of supply!  As p increases, q must also increase."))
  }  
  dq = (q1-q0)
  dp = (p1-p0)
  qbar = (q0+q1)/2
  pbar = (p0+p1)/2
  sslope= dq/dp
  pEs = (sslope)*(pbar/qbar)
  print(paste("The slope of the supply curve in the segment is", round(sslope, 3)))
  print(paste("The price elasticity of supply is", round(pEs, 3) ))
  print(paste("In other words, a 1% increase in the price leads to a", round(pEs, 3), "% increase in the quantity supplied."))
  if(pEs < 1 ){
    print(paste("The supply of this good is price-inelastic."))
  } else {print(paste("This supply of this good is price-elastic."))}
}
# Example: p0=20, p1=24, q0=40, q1=38
p_elast_s(20, 24, 40, 48)

# Example: p0=20, p1=24, q0=40, q1=42
p_elast_s(20, 24, 40, 42)

rm(list = ls())

