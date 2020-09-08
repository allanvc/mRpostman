.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Substantial changes without backward compatibility were implemented between versions 0.3.1 and 0.9.0.0.')
  packageStartupMessage('Check the changelog at https://allanvc.github.io/mRpostman/news/index.html or the "Migrating code to mRpostman\'s R6 sintaxe" vignette.')
}
