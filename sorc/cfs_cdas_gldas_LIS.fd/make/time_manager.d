time_manager.o time_manager.d : time_manager.F
time_manager.o : misc.h
time_manager.o : spmdMod.o
time_manager.o : lis_module.o
time_manager.o : precision.o
