# generates and prints a random flow network

from random import randrange

V = randrange(2, 1000 + 1)
s = randrange(0, V)
t = randrange(0, V)
while t == s:
    t = randrange(0, V)

print(V, s, t)

E = randrange(V + 2, V * (V - 1) - V)

graph = [set() for _ in range(V)]

for i in range(E):
    u = randrange(0, V)
    v = randrange(0, V)
    while u == v or v in graph[u] or u in graph[v]:
        v = randrange(0, V)
        u = randrange(0, V)
    c = randrange(1, 100 + 1)
    print(u, v, c)
    graph[u].add(v)
    graph[v].add(u)

