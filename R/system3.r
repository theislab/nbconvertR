system3 <- function(command, args = character(), capture = FALSE, ...) {
	stopifnot(is.logical(capture) && length(capture) == 1L)
	
	carg <- if (capture) TRUE else ''
	r <- system2('jupyter', shQuote(args), carg, carg, wait = TRUE, ...)
	
	if (capture) {
		out <- r
		code <- attr(out, 'status')
		if (is.null(code)) code <- 0L
		msg <- attr(out, 'errmsg')
		if (is.null(msg)) msg <- NA_character_
		attributes(out) <- NULL
	} else {
		out <- NA_character_
		code <- r
		msg <- NA_character_
	}
	
	list(out = out, code = code, msg = msg)
}
