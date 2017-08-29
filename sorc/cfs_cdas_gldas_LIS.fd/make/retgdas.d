retgdas.o retgdas.d : retgdas.F
retgdas.o : lis_module.o
retgdas.o : lisdrv_module.o
retgdas.o : time_manager.o
retgdas.o : baseforcing_module.o
retgdas.o : gdasdomain_module.o
retgdas.o : bilinear_interpMod.o
retgdas.o : conserv_interpMod.o
