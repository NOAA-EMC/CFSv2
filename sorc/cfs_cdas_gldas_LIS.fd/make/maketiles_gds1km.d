maketiles_gds1km.o maketiles_gds1km.d : maketiles_gds1km.F
maketiles_gds1km.o : lisdrv_module.o
maketiles_gds1km.o : grid_module.o
maketiles_gds1km.o : spmdMod.o
maketiles_gds1km.o : opendap_module.o
