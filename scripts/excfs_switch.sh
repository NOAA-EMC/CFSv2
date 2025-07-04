#!/bin/ksh                                                                                
####################################################################################
####  UNIX Script Documentation Block
# Script Name: excfs_swith.sh.sms
# Purpose: This script should be submit before normal machine switch procedure.
#          The script suspend climprod* suites and transfer_cfsv2 suite, then
#          cancel the remainning running job, resubmit all the transfer jobs
#
# Author:  Xiaoxue Wang		Org:PMB		Date: 2011-01-25
#
#################################################################################### 

set -x
#
# Suspend climprod00,06,12,18 suite and transfer_cfsv2 suite
#
ecflow_client --suspend /climprod00
ecflow_client --suspend /climprod06
ecflow_client --suspend /climprod12
ecflow_client --suspend /climprod18
ecflow_client --suspend /transfers_wcoss/cfsv2

#
# Check the running cfs and transfer_cfs jobs and cancel it
#

whoami

bjobs -u nwprod -w | grep jcfs | awk '{print $1}' > cancel.list
bjobs -u nwprod -w | grep transfer_cfs | awk '{print $1}' >> cancel.list

for id in `cat cancel.list`
do
  bkill $id
done

sleep 60

bjobs -u nwprod -w | grep jcfs | awk '{print $1}' > cancel.list
bjobs -u nwprod -w | grep transfer_cfs | awk '{print $1}' >> cancel.list

for id in `cat cancel.list`
do
  bkill $id
done

sleep 60

ecflow_client --requeue /transfers_wcoss/cfsv2

#
# Resubmit each transfer_cfs job before machine switch
#

ecflow_client --run /transfers_wcoss/cfsv2/cfs00/cfs00_m1
ecflow_client --run /transfers_wcoss/cfsv2/cfs00/cfs00_m2
ecflow_client --run /transfers_wcoss/cfsv2/cfs00/cfs00_m3
ecflow_client --run /transfers_wcoss/cfsv2/cfs00/cfs00_m4
ecflow_client --run /transfers_wcoss/cfsv2/cfs06/cfs06_m1
ecflow_client --run /transfers_wcoss/cfsv2/cfs06/cfs06_m2m3m4
ecflow_client --run /transfers_wcoss/cfsv2/cfs12/cfs12_m1
ecflow_client --run /transfers_wcoss/cfsv2/cfs12/cfs12_m2m3m4
ecflow_client --run /transfers_wcoss/cfsv2/cfs18/cfs18_m1
ecflow_client --run /transfers_wcoss/cfsv2/cfs18/cfs18_m2m3m4
ecflow_client --run /transfers_wcoss/cfsv2/cfs_cdas1
ecflow_client --run /transfers_wcoss/cfsv2/cfs_nwgescdas1
ecflow_client --run /transfers_wcoss/cfsv2/cfs_recovery

#ecflow_client --suspend /transfers_wcoss/cfsv2

#
# set aborted all CFS currently active jobs 
#
echo "ssh ecfprod@$ECF_NODE<< 'EOF'" > ecf_command_file
echo "set -x" >> ecf_command_file
echo "python /ecf/ecfnets/python/node_status.py | grep climprod | grep active>/home/ecfprod/clim_switch.out" >> ecf_command_file
echo "hostname" >> ecf_command_file
echo "ls -l /home/ecfprod/clim_switch.out" >> ecf_command_file
echo "for task in \`cat /home/ecfprod/clim_switch.out |cut -f1 -d\" \"\`; do "  >> ecf_command_file
echo "ecflow_client --force aborted \$task" >> ecf_command_file
echo "done; exit" >> ecf_command_file
echo "EOF" >> ecf_command_file
chmod 775 ecf_command_file

./ecf_command_file

