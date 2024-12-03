library(bio3d)
snippet <- function(x) {
  y <- read.pdb(x)
  y.chainA <- trim.pdb(y, chain="A", elety="CA")
  y.b <- y.chainA$atom$b
  plotb3(y.b, sse=y.chainA, typ="l", ylab="Bfactor")
}

snippet("1Y31")


