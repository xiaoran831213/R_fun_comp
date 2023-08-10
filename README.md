# R_fun_comp
Function Composition for R language

Suppose f() and g() are functions, the composition operation works as
- (f o g)(x) = f(g(x))
Where (f o g)() is the function composed of f() and g(), and **o** is the composition operator.

Specifically, define a composition operator **%.%** that
- allows partial argument assignment to the component functions.
- ensures environment sensitive function like ls() act normally.
- the body of composed function shows actual syntax of component functions.
  * an improvement over the tidyverse "%>%" operator.

Inspired by the pipeOp **|>** since R4.1, the operator is an answer to stackoverflow post:
- https://stackoverflow.com/questions/4918819/function-composition-in-r-and-high-level-functions
