opendap_module.o opendap_module.d : opendap_module.F
opendap_module.o : time_manager.o
opendap_module.o : lisdrv_module.o
opendap_module.o : grid_spmdMod.o
opendap_module.o : tile_spmdMod.o
opendap_module.o : spmdMod.o
