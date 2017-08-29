getgdas.o getgdas.d : getgdas.F
getgdas.o : lisdrv_module.o
getgdas.o : baseforcing_module.o
getgdas.o : time_manager.o
getgdas.o : gdasdomain_module.o
