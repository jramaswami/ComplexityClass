# Let R = 2.5 x_sub0 = 0.2
# Use the equation for the Logistic Map:
# x_subt+1 = R (x_subt - x_subt ^ 2)
# to calculate x_sub1, x_sub2, ... until
# you reach a fixed point.  What is 
# the fixed point?

n = 0
R = 2.5
x_subtplus1 = 0
x_subt = 0.2
fixed = 5

print (n, x_subt)

while fixed > 0:
    x_subtplusone = R * (x_subt - (x_subt ** 2))
    if x_subtplusone == x_subt:
        fixed = fixed - 1

    x_subt = x_subtplusone
    n = n + 1
    print (n, x_subt)

