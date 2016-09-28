metaext_re <- '[.]ipynbmeta$'

#' Jupyter/IPython Notebook Conversion
#' 
#' An R interface for using Jupyter/IPython system calls to convert .ipynb notebooks unsing meta information.
#' When passed "<filename>.ipynbmeta" it will convert "<filename>.ipynb" to "<filename>.<ext>".
#' 
#' Apart from the standard \code{VignetteIndexEntry{<name>}} and \code{VignetteEngine{<namespace>::<name>}} directives,
#' it also understands \code{VignetteTemplate{<format>}{<filename>}}, which will pass a \code{--template} parameter to \code{nbconvert}
#' 
#' @param file   A file with a .ipynbmeta extension that contains vignette metadata lines
#' @param fmt    A format supported by \code{nbconvert}. "script" will create an .r file, and "slides" a reveal.js-powered html presentation.
#' @param quiet  Suppress command output if TRUE
#' @param ...    Ignored for now
#' @return  The filename of the resulting document, script or presentation
#' 
#' @examples \dontrun{
#' path <- system.file('doc/test-vignette.ipynbmeta', package = 'nbconvertR')
#' nbconvert(path, 'pdf')
#' }
#' 
#' @export
nbconvert <- function(
	file,
	fmt = c('html', 'latex', 'markdown', 'pdf', 'rst', 'script', 'slides'),
	#encoding = 'UTF-8',
	quiet = FALSE,
	...) {
	
	fmt <- match.arg(fmt)
	ext <- switch(fmt, html = '.html', latex = '.tex', markdown = '.md',
	              pdf = '.pdf', rst = '.rst', script = '.r', slides = '.slides.html')
	
	lines <- readLines(file)
	
	ipynb_file <- sub(metaext_re, '.ipynb', file)
	
	args <- c(
		'nbconvert',
		args_template(lines, fmt),
		'--to', fmt,
		ipynb_file)
	
	output <- if (quiet) FALSE else ''
	
	ret <- system2('jupyter', args, output, output, wait = TRUE)
	
	if (ret != 0) stop(sprintf('The call %s failed with exit status %s', dQuote(paste(shQuote(c('jupyter', args)), collapse = ' ')), ret))
	
	filename <- sub(metaext_re, ext, basename(file))
	file.path(getwd(), filename)
}

.onLoad = function(libname, pkgname) {
	tools::vignetteEngine(
		'nbconvert',
		weave  = function(file, ...) nbconvert(file, 'latex',  ...),
		tangle = function(file, ...) nbconvert(file, 'script', ...),
		pattern = metaext_re,
		package = 'nbconvertR')
}
