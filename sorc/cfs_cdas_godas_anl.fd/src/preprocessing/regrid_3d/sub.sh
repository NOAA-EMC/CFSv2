#!/bin/sh
#
#
# @ job_type = parallel
# @ output = llpara.$(jobid).out
# @ error = llpara.$(jobid).err
# @ total_tasks = 4 
## @ node = 3
# @ blocking = unlimited
# @ preferences = Feature == "dev"
# @ network.MPI = csss,shared,us
# @ class = dev
# @ wall_clock_limit = 00:40:00
# @ queue

csh run.csh
