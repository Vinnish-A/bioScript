
test_that(
  'param env', {
    expect_equal(
      withNothing(a + b, a = 4, b = 5),
      9
    )
  }
)
