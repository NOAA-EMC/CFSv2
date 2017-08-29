#!/bin/ksh
set -x

################################################################################
# Check file script for analysis files
# Checks for appropriate file size
#
# Patrick Tripp - October 2010
#

if [[ $# -ne 2 ]] ; then
  echo "Usage: chk_file <ftype> <fname>"
  err_exit 1
fi

ftype=$1
fname=$2

echo "ftype is $ftype"
echo "fname is $fname"

rval=0
esize=-1024   # prevent non-set esize and 0 size files
              # from slipping through error checks


# abias cnvstat gsistat radstat oznstat satang prepbufr
# noah.rst noahbin.lis
# noahgrb.lis

#  exact
#  -----
#  SCORES
#  abias  139314
#  acar
#  acft
#  raob
#  .sfc
#  noah.rst 37691952
#  satang 1084552
#  
#  smallest
#  --------
#  adpsfc 
#  adpupa 
#  aircft
#  cnvstat 55367680 - 27683840
#  gsistat 1786994 - 893497
#  oznstat 716800 - 300000
#  prepqa 
#  prepqc 
#  prepqf 
#  prepbufr 28397320 - 14198660
#  radstat 1076336640 - 538168320
#  LIS.E574. 
#  noahgrb.lis 7116918 - 3558459
#  noahbin.lis 99123328 - 49561664
#  aircar 

#  cmap
#  tcinform
#  tcvitals
#  atcfunix

## FOR HIC

# sanl,sf00,sf06 - 511378568 (exact)
# sfcanl,bf00,bf06 - 272591284 (exact)
# ocnanl.tar - 1242286080 (exact)
# bf06.LIS 272591284 (exact)

case $ftype in

  # First in list are exact expected file sizes
  SCORES)
    esize=31176
    ;;
  biascr|abias)
    esize=139314
    ;;
  acar|f00.acar|f06.acar)
    esize=972
    ;;
  acft|f00.acft|f06.acft)
    esize=2916
    ;;
  raob|f00.raob|f06.raob)
    esize=20628
    ;;
  .sfc.|f00.sfc|f06.sfc)
    esize=384
    ;;
  noah|noah.rst)
    esize=37691952
    ;;
  satang)
    esize=1084552
    ;;
  sanl|sf00|sf06)
    esize=511378568
    ;;
  sfcanl|bf00|bf06)
    esize=272591284
    ;;
  ocnanl.tar)
    esize=1242286080
    ;;
  bf06.LIS)
    esize=272591284
    ;;

  # The following vary in size, using ~50% of the smallest found
  noahbin.lis)
    esize=49561664
    ;;
  noahgrb.lis)
    esize=3558459
    ;;
  adpsfc|adpsfc.anl|adpsfc.fcs)
    esize=700000
    ;;
  adpupa|adpupa.mand.anl|adpupa.mand.fcs)
    esize=37000
    ;;
  aircft|aircft.anl|aircft.fcs)
    esize=1100000
    ;;
  cnvstat)
    esize=27683840
    ;;
  gsistat)
    esize=893497
    ;;
  oznstat)
    esize=300000
    ;;
  prepqa|prepqc)
    esize=1200000
    ;;
  prepqf)
    esize=18000000
    ;;
  prepbufr)
    esize=14198660
    ;;
  radstat)
    esize=538168320
    ;;
  LIS.E574.)
    esize=3500000
    ;;
  aircar|aircar.anl|aircar.fcs)
    esize=650000
    ;;
  cmap)
    esize=110000
    ;;
  tcinform)
    esize=0
    ;;
  tcvitals)
    # Tropicl cyclones can be zero for now
    esize=0
    ;;
  atcfunix)
    esize=50
    ;;
  *)
    exit 0
    ;;
esac


fsize=0

#

if [[ -e $fname ]] ; then
  fsize=`ls -l $fname | awk '{print $5}'`

  if [[ $fsize -lt $esize ]] ; then
    echo "WARNING: $fname size:$fsize expected > $esize"
    ((rval+=1))
  fi
else
  echo "WARNING: $fname not found"
  ((rval+=1))
fi

export err=$rval; err_chk
#exit $rval

