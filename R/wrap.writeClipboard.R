#' Write to clipboard
#'
#' @description Writes a string to the clipboard and removes the trailing newline. The
#' function delegates the primary operations to writeClipboard (package: utils). (This
#' is a helper function that will be called by other functions in this package.)
#'
#' @param string The string to write to the clipboard
#'
#' @seealso writeClipboard (package: utils)
#'
#' @examples
#' wrap.writeClipboard(string = "text")
#'
#' @import utils
#' @export
wrap.writeClipboard <- function(string) {
  writeClipboard(charToRaw(paste0(string, ' ')))
  }
