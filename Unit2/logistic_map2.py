n = 0
R = 1
x_subt = 0.2

print(n, x_subt)
for n in range(1,10001):
    x_subtplusone = R * x_subt * (1 - x_subt)
    print(n, x_subtplusone)
    x_subt = x_subtplusone

