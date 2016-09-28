context('preprocessor args')

test_that('empty preprocessor args work', {
	expect_identical(args_preprocessors(character(0L)), character(0L))
	expect_identical(args_preprocessors('other stuff'), character(0L))
	expect_identical(args_preprocessors('%\\VignetteIndexEntry{aname}'), character(0L))
})

test_that('one preprocessor arg works', {
	expect_identical(args_preprocessors('%\\VignettePreprocessors{foo.Bar}'),     '--Exporter.preprocessors=["foo.Bar"]')
	expect_identical(args_preprocessors('%\\VignettePreprocessors{  foo.Bar\t}'), '--Exporter.preprocessors=["foo.Bar"]')
})

test_that('comma separated preprocessor args work', {
	expect_identical(args_preprocessors('%\\VignettePreprocessors{foo.Bar,baz.Bam}'),        '--Exporter.preprocessors=["foo.Bar","baz.Bam"]')
	expect_identical(args_preprocessors('%\\VignettePreprocessors{  foo.Bar\t, baz.Bam\t}'), '--Exporter.preprocessors=["foo.Bar","baz.Bam"]')
})
