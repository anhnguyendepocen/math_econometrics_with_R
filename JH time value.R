# JH
# 5/15/2020
# TIME VALUE UNDER PERFECT FORESIGHT
# PRESENT VALUE

# Coupon bond, ordinary annuity, and simple discount bond
rm(list=ls())

cbpv_fn <- function(yrcoupon, drate, ymaturity, facev) {
  cbpvalue <- (yrcoupon/drate)*(1 - (1+drate)^-ymaturity) + facev*(1+drate)^-ymaturity
  print(paste("The present value of the bond is $", round(cbpvalue, 2) ))
}

# Example: Simple discount bond
# Annual coupon payment= $0. 
# Annual discount rate=5% p.a.
# Maturity of the bond (in years)=10.
# Face value of the bond=$10,000.
cbpv_fn(0, .05, 10, 10000)

# Example: Ordinary annuity
# Annual coupon payment= $2,000. 
# Annual discount rate=5% p.a.
# Maturity of the bond (in years)=10.
# Face value of the bond=$0.
cbpv_fn(2000, .05, 10, 0)

# Example: Coupon bond
# Annual coupon payment= $2,000. 
# Annual discount rate=5% p.a.
# Maturity of the bond (in years)=10.
# Face value of the bond=$10,000.
cbpv_fn(2000, .05, 10, 10000)

rm(list=ls())

# Perpetutity or consol
ppv_fn <- function(yrcoupon, drate) {
  ppvalue <- yrcoupon/drate
  print(paste("The present value of the consol is $", round(ppvalue, 2) ))
}

# Example: Consol
# Annual coupon payment= $2,000. 
# Annual discount rate=5% p.a.
ppv_fn(2000, .05)


