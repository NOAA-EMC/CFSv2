noah_gather.o noah_gather.d : noah_gather.F
noah_gather.o : misc.h
noah_gather.o : tile_spmdMod.o
noah_gather.o : noah_varder.o
noah_gather.o : noahpardef_module.o
