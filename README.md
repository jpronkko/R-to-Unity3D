# R-to-Unity3D
Proto for a package to send plotting commands from an R client to a Unity3D server via a simple ascii protocol over TCP. The package uses base R TCP socket commands to send data to the plotting server. You need to have the plotting server running before sending commands from R. See https://github.com/jpronkko/vrplotunity3d for the server. You can install this package to R via install_github("jpronkko/R-to-Unity3D"). For this to work you need to have the developer tools installed in R, which can be installed via install.packages("devtools") and used via library(devtools). 

Commands include vrplot, vrclear, vrtitle, vrlabels.

An example:
```
  x <- seq(-1, 1, length.out = 100)
  y <- seq(-1, 1, length.out = 100)
  z <- sin(x)
  colInd = rep(1:10, length.out = 100)
  vrplot("Plot1", x, y, z, colInd, type="cube")
  vrlabels("Plot1", "X stuff", "Y stuff", "Z stuff")
```

 
