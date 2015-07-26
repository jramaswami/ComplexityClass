initial_population = 100
max_population = 2 * initial_population

population = initial_population
n = 0
print (n, population)

while population < max_population:
    population = int(1.1 * population)
    n = n + 1
    print (n, population)
