
# test-source -------------------------------------------------------------


## buildContent ----

test_that(
  'buildContent back to root', {
    expect_equal(
      buildContent('part1$', fileWhenTest('test.R')),
      buildContent('part1.1$', fileWhenTest('test.R'), T)
    )
  }
)

test_that(
  'buildContent finds wrong with wrong-format file', {
    expect_error(
      buildContent('part1', fileWhenTest('overlevel.R'))
    )
  }
)

test_that(
  'buildAllContent finds wrong with wrong-format file', {
    expect_error(
      buildAllContent('part1', fileWhenTest('overlevel.R'))
    )
  }
)

## findWhere ----

test_that(
  'findWhere finds wrong with file cotaining same subtitles', {
    expect_error(
      findWhere(buildFile(fileWhenTest('subSame.R')), 'partA')
    )
  }
)

test_that(
  'findWhere finds wrong with file cotaining subtitle which doesn\'t exist', {
    expect_error(
      findWhere(buildFile(fileWhenTest('subSame.R')), 'partZ')
    )
  }
)

test_that(
  'Regular expressions are applicable in findWhere.', {
    expect_equal(
      findWhere(buildFile(fileWhenTest('subSame.R')), 'partC$'),
      'partC'
    )
  }
)

test_that(
  'findWhere can find the subtitle with unique path.', {
    expect_equal(
      findWhere(buildFile(fileWhenTest('subSame.R')), 'partH'),
      c('partC', 'partA', 'partH')
    )
  }
)
