obsradforcing_module.o obsradforcing_module.d : obsradforcing_module.F
obsradforcing_module.o : misc.h
obsradforcing_module.o : lisdrv_module.o
obsradforcing_module.o : grid_spmdMod.o
obsradforcing_module.o : radforcing_pluginMod.o
obsradforcing_module.o : driverpardef_module.o
