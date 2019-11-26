context('preprocessor args')

check_installed <- function() {
	r <- tryCatch({
		system3('jupyter', c('nbconvert', '--version'), TRUE)
	}, error = function(e) list(code = 128L, out = toString(e)))
	
	if (r$code == 0L) {
		message('Found nbconvert v', r$out)
	} else {
		skip(r$out)
	}
}

expect_creates_file <- function(name, code) {
	if (file.exists(name)) file.remove(name)
	force(code)
	created <- file.exists(name)
	if (created) file.remove(name)
	expect_true(created)
}

test_that('the test vignette can be built', {
	check_installed()
	expect_creates_file('test-vignette.pdf', {
		tools::buildVignette('test-vignette.ipynbmeta', weave = TRUE, tangle = FALSE)
	})
})

test_that('the test script can be created', {
	check_installed()
	expect_creates_file('test-vignette.r', {
		tools::buildVignette('test-vignette.ipynbmeta', weave = FALSE, tangle = TRUE)
		expect_identical(readChar('test-vignette.r', 20L), "'hi!'\n")
	})
})
