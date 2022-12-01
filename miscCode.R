# Pima County Climate Summary
# MAC 11/17/22

library(sf)

parks <- sf::st_read( "/home/crimmins/RProjects/ClimMicroApps/shapes/pima.gdb", layer = "pk_mprop")

pastures <- sf::st_read( "/home/crimmins/RProjects/ClimMicroApps/shapes/pima.gdb", layer = "pastures")

plot(st_geometry(parks))
plot(st_geometry(pastures))