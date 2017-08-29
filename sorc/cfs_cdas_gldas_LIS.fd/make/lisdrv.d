lisdrv.o lisdrv.d : lisdrv.F
lisdrv.o : misc.h
lisdrv.o : precision.o
lisdrv.o : lisdrv_module.o
lisdrv.o : lsm_module.o
lisdrv.o : baseforcing_module.o
lisdrv.o : obsprecipforcing_module.o
lisdrv.o : obsradforcing_module.o
lisdrv.o : spmdMod.o
