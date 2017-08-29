read_umdavhrr_lc.o read_umdavhrr_lc.d : read_umdavhrr_lc.F
read_umdavhrr_lc.o : misc.h
read_umdavhrr_lc.o : lisdrv_module.o
read_umdavhrr_lc.o : spmdMod.o
