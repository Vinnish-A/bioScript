
# test --------------------------------------------------------------------

vec_all = c('a', 'b', 'c', 'd')

## part2 ----

vec4 = vec_all[[4]]
cat(vec4, file = 'tmp/vec4.txt')

## part1 ----

vec1 = vec_all[[1]]
cat(vec1, file = 'tmp/vec1.txt')

### part1.1 ----

vec2 = vec_all[[2]]
cat(vec2, file = 'tmp/vec2.txt')

#### part1.1.1 ----

c = 'a'

### part1.2 ----

vec3 = vec_all[[3]]
cat(vec3, file = 'tmp/vec3.txt')
