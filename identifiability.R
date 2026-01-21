library(dosearch)

# Figure 1b

graph <- "
x -> w1
w1 -> w2
w2 -> y
z2 -> y
z2 -> w1
z1 -> y
z1 -> x
z2 -> z1
"

data <- "p(x, y, w1, w2, z2)"

query <- "p(y|do(x))"

dosearch(data, query, graph)

# Figure 2 (Transportability example)

graph <- "
x -> w1
w1 -> w2
w2 -> y
z2 -> y
z2 -> w1
z1 -> y
z1 -> x
z2 -> z1
t1 -> z2
"

data <- "p(y, w1, z2 | do(x), t1) 
         p(z1, z2)
"

query <- "p(y|do(x))"

dosearch(data, query, graph, transportability = "t1")

# Portland backdoor

graph <- "
x -> y
z -> x
z -> y
t1 -> z
"

data <- "
p(x, y, z | t1)
p(z)
"

query <- "p(y|do(x))"

dosearch(data, query, graph, transportability = "t1", control = list(heuristic = FALSE))

# Portland two-door

graph <- "
z -> w
z -> l
z -> w
z -> y
l -> y
l -> x
x -> w
w -> y
s -> y
"

data <- "
p(x, y, z, w | t1)
p(z)
"

query <- "p(y|do(x))"

dosearch(data, query, graph, control = list(heuristic = FALSE))
