POPid2geo <- function(ids, long, latg, zg) { # TODO: include in rdafCESM
  # ids : DIMENSION(3), vector of indices within a 3D irregular POP grid
  # long: 2D longitude grid
  # latg: 2D latitude grid
  # zg  : 1D depth grid
  x <- long[ids[1],ids[2]]
  y <- latg[ids[1],ids[2]]
  z <- zg[ids[3]]
  c(x,y,z)                                   # [lon,lat,z]
}
