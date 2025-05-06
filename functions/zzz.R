.onLoad <- function(libname, pkgname) {
  if (Sys.info()[["sysname"]] == "Darwin") {
    brew <- try(system2("brew", c("--prefix", "zlib"), stdout = TRUE), silent = TRUE)
    if (!inherits(brew, "try-error") && dir.exists(brew)) {
      Sys.setenv(DYLD_FALLBACK_LIBRARY_PATH = file.path(brew, "lib"))
    }
  }
}
