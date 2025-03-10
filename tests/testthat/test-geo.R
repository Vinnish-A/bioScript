
test_that(
  'find n file', {
    expect_equal(
      length(listGEO('GSE129516')),
      3
    )
  }
)
