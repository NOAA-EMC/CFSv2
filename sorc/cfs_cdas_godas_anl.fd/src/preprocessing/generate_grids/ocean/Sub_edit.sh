#!/bin/sh
#
#
# @ job_type = parallel
# @ output = llpara.$(jobid).out
# @ error = llpara.$(jobid).err
# @ total_tasks = 1 
## @ node = 3
# @ blocking = unlimited
# @ preferences = Feature == "dev"
# @ network.MPI = csss,shared,us
# @ class = dev
# @ wall_clock_limit = 0:30:00
# @ queue

csh Run_edit.csh

