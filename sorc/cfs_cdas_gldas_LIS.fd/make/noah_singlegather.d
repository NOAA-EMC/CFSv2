noah_singlegather.o noah_singlegather.d : noah_singlegather.F
noah_singlegather.o : misc.h
noah_singlegather.o : lisdrv_module.o
noah_singlegather.o : tile_spmdMod.o
noah_singlegather.o : noah_varder.o
noah_singlegather.o : noahpardef_module.o
