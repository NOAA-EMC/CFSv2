noah_atmdrv.o noah_atmdrv.d : noah_atmdrv.F
noah_atmdrv.o : lisdrv_module.o
noah_atmdrv.o : spmdMod.o
noah_atmdrv.o : tile_spmdMod.o
noah_atmdrv.o : noah_varder.o
