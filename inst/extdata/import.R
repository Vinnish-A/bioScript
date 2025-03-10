
# test --------------------------------------------------------------------

vec_all = c('a', 'b', 'c', 'd')

a = function(x) {

  1 + x

}

b = function(x) {

  a(x)

}

## part1 ----

vec1 = vec_all[[1]]
cat(vec1)

cal = a(1)

### part1.1 ----

c = function(x) {

  b(x)

}

vec2 = vec_all[[2]]
cat(vec2)

### part1.2 ----

vec3 = vec_all[[3]]
cat(vec3)

## part2 ----

vec4 = withNothing(vec_all[[4]])
cat(vec4)
