# ~38 frames, 24 pixels jump

q = 20
b = 16/q
a = -8/(q*q)

result = []
prev = 0
for x in range(0,q+1):
    y = 3*(a*x*x + b*x)

    result.append(-int(y-prev))
    prev = int(y)

result.pop(0)
print("\tdc.b\t"+",".join(map(str,result)))

print("\tdc.b\t"+",".join([str(-x) for x in reversed(result)]))

