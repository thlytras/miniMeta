# parseArguments() must accept literal forest() arguments and must never
# evaluate code coming from the user.

marker <- function() file.path(tempdir(), "miniMeta_pwned")

test_that("literal arguments are parsed as before", {
  expect_identical(
    parseArguments('col.diamond="red", sm="RR", common=FALSE'),
    list(col.diamond="red", sm="RR", common=FALSE))
  expect_identical(parseArguments('xlim=c(0.5, 2), fontsize=10'),
    list(xlim=c(0.5, 2), fontsize=10))
  expect_identical(parseArguments('at=c(-1, 0, 1)'), list(at=c(-1, 0, 1)))
  expect_identical(parseArguments('label.left=NULL, digits=NA'), list(label.left=NULL, digits=NA))
  expect_identical(parseArguments('xlim=c(-Inf, Inf)'), list(xlim=c(-Inf, Inf)))
  expect_identical(parseArguments('leftcols=c("studlab"), test.overall=T'),
    list(leftcols="studlab", test.overall=TRUE))
  expect_identical(parseArguments(''), list())
  expect_identical(parseArguments('  '), list())
  expect_identical(parseArguments(NULL), list())
})

test_that("unnamed arguments are rejected", {
  expect_s3_class(parseArguments('"red"'), "try-error")
  expect_s3_class(parseArguments('a=1, 2'), "try-error")
})

test_that("syntax errors are reported, not thrown", {
  expect_s3_class(parseArguments('a=c(1,'), "try-error")
})

test_that("code is never executed", {
  unlink(marker())
  attacks <- c(
    sprintf('x=file.create("%s")', marker()),
    sprintf('x=system("touch %s")', marker()),
    sprintf('a=1), x=file.create("%s"', marker()),          # close getArgs() early
    sprintf('x=(function() file.create("%s"))()', marker()),
    sprintf('x=`file.create`("%s")', marker()),
    sprintf('x=c(1, file.create("%s"))', marker()),
    sprintf('x=-file.create("%s")', marker()),
    sprintf('x=list(a=file.create("%s"))', marker()),
    sprintf('x=eval(parse(text="file.create(\'%s\')"))', marker()),
    sprintf('x=1; file.create("%s")', marker()),
    sprintf('x=1\nfile.create("%s")', marker()),
    'x=Sys.getenv()', 'x=.GlobalEnv', 'x=ls', 'x=1+1', 'x=1:1e9', 'x=-"a"'
  )
  for (a in attacks) {
    res <- parseArguments(a)
    expect_s3_class(res, "try-error")
  }
  expect_false(file.exists(marker()))
})
