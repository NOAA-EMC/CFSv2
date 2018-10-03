set -ax

fixdir=`pwd`

cd $fixdir/EmisCoeff/Big_Endian
ln -fs Nalli.EK-PDF.W_W-RefInd.EmisCoeff.bin EmisCoeff.bin

cd $fixdir/SpcCoeff/Big_Endian
ln -fs airs281_aqua.SpcCoeff.bin  airs281SUBSET_aqua.SpcCoeff.bin
ln -fs cris399_npp.SpcCoeff.bin   cris_npp.SpcCoeff.bin

cd $fixdir/TauCoeff
ln -fs ODPS/Big_Endian > Big_Endian
cd $fixdir/TauCoeff/Big_Endian
ln -fs airs281_aqua.TauCoeff.bin airs281SUBSET_aqua.TauCoeff.bin
ln -fs cris399_npp.TauCoeff.bin cris_npp.TauCoeff.bin
