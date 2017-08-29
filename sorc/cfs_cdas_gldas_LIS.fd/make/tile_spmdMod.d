tile_spmdMod.o tile_spmdMod.d : tile_spmdMod.F
tile_spmdMod.o : misc.h
tile_spmdMod.o : spmdMod.o
tile_spmdMod.o : tile_module.o
