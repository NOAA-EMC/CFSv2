#!/bin/tcsh 
#=======================================================================
#         Contact : Zhi Liang   email : Zhi.Liang@noaa.gov
#
#  This runscritp can can edit the topography of input grid_spec file "orig_grid" 
#  according to the ascii input file "grid_edits". Then it will output the 
#  new grid_spec file "mod_grid". 
#  This preprocessing program was tested on the sgi origin3000 system at GFDL.
#  In order to run on other system, some changes may be needed.
#=======================================================================
  set echo
# set DEBUG                                            # uncomment this to debug your run with totalview
  set platform     = ibm                               # A unique identifier for your platform
  set name         = "edit_grid"                       # name of the grid file will be generated
  set root         = $cwd:h #:h:h:h                      # The directory you created when you checkout
  set root         = $root:h
  set root         = $root:h
  set root         = $root:h
  set tooldir      = $cwd                              # directory of the tool
  set sharedir     = $root/src/shared                  # directory of the shared code.
  set includedir   = $root/include                     # fms include directory   
  set workdir      = $tooldir/workdir                  # where the tool is run and output is produced
  set executable   = $tooldir/exec/edit_grid.exe       # executable created after compilation
  set mkmfTemplate = $root/bin/mkmf.template.$platform # path to template for your platform
  if ( $?DEBUG ) then
    set executable    = $tooldir/debug/edit_grid.exe 
    set mkmfTemplate  = $root/bin/mkmf.debugtemplate.$platform         
  endif
  set mkmf         = $root/bin/mkmf                    # path to executable mkmf
  set cppDefs      = ( "-Duse_netCDF -Duse_libMPI" )                # list of cpp #defines to be passed to the source files

# grid to be edited and grid_edits test file
  set grid_edits = grid_edits.txt                      # text file to specify the edit region and new depth.
  set orig_grid  = $cwd/ocean_grid.nc         

# list the source code
  
  set CORE      = " $tooldir/{edit_grid.F90,topog.f90,grids_type.f90,grids_util.f90} "
  set UTILITIES    = "$sharedir/{axis_utils,constants,fms,mpp,horiz_interp,platform,memutils}"
  set UTILITIES    = " $UTILITIES $sharedir/mpp/include $includedir " 
  set srclist   = ( $CORE $UTILITIES )

# the following compiler choice are for GFDL user only. If the following command


# compile the model code and create executable

#--------------------------------------------------------------------------------------------------------
# setup directory structure
  if ( ! -d $workdir )         mkdir $workdir

  cd $workdir

# get executable  
  cp $executable $executable:t

# if the grid_edits file does not exist, create one here.
  if( ! -f $grid_edits) then
     cat >$grid_edits <<EOF
561:640,20:23,0
696:715,17:24,0
697:701,24:26,0
310:345,13:16,0
358:426,13:16,0
410:427,16:23,0
414:417,76:79,0
418:422,52:57,0
529:542,14:17,0
520:529,14:15,0
514:528,10:13,0
507:513,10:11,0
38:40,231:244,0
44:48,192:204,0
50:54,171:111,0
70:94,162:164,0
80:86,175:198,0
105:115,180:189,0
130:144,170:174,0
79:82,156:158,0
87:90,198:200,0
81:85,246:265,0
85:88,249:251,0
83:85,243:246,0
85:87,228:239,0
150:152,170:174,0
330:332,285:289,0
391:402,274:275,0
412:422,264:265,0
413:424,233:244,0
425:438,233:239,0
395:405,231:233,0
598:617,288:291,0
645:647,247:249,0
647:647,242:244,0
672:673,282:285,0
674:674,283:285,0
550:557,300:301,0
551:556,304,305,0
549:550,302:303,0
608:620,312:312,0
609:620,350:352,0
630:636,358:360,0
629:637,322:325,0
648:671,302:332,0
125:126,315,333,0
121:124,311:316,0
104:107,297:301,0
109:113,298:300,0
113:115,298:300,0
314:316,324:330,0
460:464,189:192,0
619:632,303:304,0
424:425,328:329,0
455:459,191:198,0
71:80,388:389,0 
373:376,401:403,0
379:384,364:370,0
406:408,406:407,0
412:414,396:397,0
416:420,390:392,0
447:451,403:405,0
457:464,403:407,0
479:483,399:400,0
684:687,405:406,0
705:708,401:404,0
37:48,408:410,0
455:460,368:377,0
454:472,396:400,0
681:705,394:408,0
698:705,373:394,0
627:641,360:365,0
37:62,29:30,0
72:105,28:29,0 
470:491,6:6,0
541:561,20:20,0
630:642,24:25,0
672:674,29:30,0
495:501,398:398,0
582:585,393:393,0
104:134,375:376,0
524:525,383:384,0
 395, 360, 0.
 396, 360, 0.
 429, 360, 0.
 582, 360, 0.
 583, 360, 0.
 609, 360, 0.
 610, 360, 0.
 611, 360, 0.
 258:260,352:353,0.
 252:253,346:347,0.
 426:427,356:357,0.
 429:430,80:81,0.
 431:431,81:81,0.
EOF
  endif

# --- set up namelist  

  cat >input.nml <<!
    &edit_grid_nml
       orig_grid   = '$orig_grid' 
       mod_grid    = '$name.nc'
       grid_edits  = '$grid_edits'
       /
!

  if ( $?DEBUG ) then
     totalview $executable:t > fms.out
  else
     $executable:t >fms.out
  endif
  cat fms.out

# rename ascii output file
  mv fms.out $name.fms.out
  mv logfile.out $name.logfile.out

  unset echo
