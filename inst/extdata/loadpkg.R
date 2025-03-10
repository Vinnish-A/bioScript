
# test --------------------------------------------------------------------

library(tidyverse)

vec_all = c('a', 'b', 'c')

## part1 ----

vec1 = vec_all[[1]]
cat(vec1, file = 'tmp/vec1.txt')

### part1.1 ----

vec2 = vec_all[[2]]
cat(vec2, file = 'tmp/vec2.txt')

## part2 ----

vec3 = vec_all[[3]]
cat(vec3, file = 'tmp/vec3.txt')
