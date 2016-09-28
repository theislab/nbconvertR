preprocessors_re <- '^\\s*%\\s*\\\\VignettePreprocessors\\{\\s*([^}]+?)\\s*\\}\\s*$'

args_preprocessors <- function(lines) {
	preprocessors_lines <- grep(preprocessors_re, lines, value = TRUE)
	preprocessors <- unlist(lapply(
		sub(preprocessors_re, '\\1', preprocessors_lines),
		function(pps) strsplit(pps, '\\s*,\\s*')))
	
	if (length(preprocessors) > 0L) {
		sprintf('--Exporter.preprocessors=["%s"]', paste(preprocessors, collapse = '","'))
	} else character(0L)
}
