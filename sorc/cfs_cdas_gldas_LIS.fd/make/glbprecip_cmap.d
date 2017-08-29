glbprecip_cmap.o glbprecip_cmap.d : glbprecip_cmap.F
glbprecip_cmap.o : lisdrv_module.o
glbprecip_cmap.o : obsprecipforcing_module.o
glbprecip_cmap.o : cmapdomain_module.o
