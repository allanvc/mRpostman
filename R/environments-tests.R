


.pkgenv <- new.env(parent=emptyenv())



pkgenv2 <- new.env(parent=emptyenv())


.pkgenv[["x"]] <- 10

rm(.pkgenv)


foo <- function(valor) {

  .pkgenv <<- new.env(parent=emptyenv())

  .pkgenv[["x"]] <- valor

}


foo(99)

.pkgenv[["x"]]

# melhor criar um environment que o usuario pode ver
