.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Substantial changes without backward compatibility were implemented between versions 0.3.1 and 0.9.0.XXXX')
  packageStartupMessage('Please, refer to the changelog at https://allanvc.github.io/mRpostman/news/index.html or the "Migrating old code to the new mRpostman\'s syntax" vignette.')
}
