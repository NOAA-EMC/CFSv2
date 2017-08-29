noah_gribout.o noah_gribout.d : noah_gribout.F
noah_gribout.o : lisdrv_module.o
noah_gribout.o : lis_module.o
noah_gribout.o : drv_output_mod.o
noah_gribout.o : noah_varder.o
noah_gribout.o : time_manager.o
