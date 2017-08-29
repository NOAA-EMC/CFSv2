noah_scatter.o noah_scatter.d : noah_scatter.F
noah_scatter.o : misc.h
noah_scatter.o : tile_spmdMod.o
noah_scatter.o : noah_varder.o
noah_scatter.o : noahpardef_module.o
