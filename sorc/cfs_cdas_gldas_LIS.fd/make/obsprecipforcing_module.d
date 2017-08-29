obsprecipforcing_module.o obsprecipforcing_module.d : obsprecipforcing_module.F
obsprecipforcing_module.o : misc.h
obsprecipforcing_module.o : lisdrv_module.o
obsprecipforcing_module.o : precipforcing_pluginMod.o
obsprecipforcing_module.o : spmdMod.o
obsprecipforcing_module.o : grid_spmdMod.o
