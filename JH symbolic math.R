# create a SymPy variable called x
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_231/bin') 
library(rJava)
library(rSymPy)

sympy("var('x')")
sympy("y = x*x")
sympy("y")
sympy("limit(1/x, x, oo)")
# the next line fails under jython even without R
# and seems to corrupt the rest of the session
# sympy("(1/cos(x)).series(x, 0, 10)")
sympy("diff(sin(2*x), x, 1)")
sympy("diff(sin(2*x), x, 2)")
sympy("integrate(exp(-x), (x, 0, oo))")
sympy("xr = Symbol('xr', real=True)")
sympy("exp(I*xr).expand(complex=True)")
# Matrices are stored row by row (unlike R matrices)
cat(sympy("A = Matrix([[1,x], [y,1]])"), "\n")
cat(sympy("A**2"), "\n")
sympy("diff(4*x**3+2*x**2, x)")
sympy("diff(exp(2*x), x)")
sympy("diff(log(x), x)")
