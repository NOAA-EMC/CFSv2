//-------------------------------------------------------------------------
//     NASA GSFC Land Information Systems LIS 3.0                         !
// Released May 2004
//
// See SOFTWARE DISTRIBUTION POLICY for software distribution policies
//
// The LIS source code and documentation are in the public domain,
// available without fee for educational, research, non-commercial and
// commercial purposes.  Users may distribute the binary or source
// code to third parties provided this statement appears on all copies and
// that no charge is made for such copies.
//
// NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
// SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
// IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
// LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
//
// See COPYRIGHT.TXT for copyright details.
//
//-------------------------------------------------------------------------
//BOP
//
//  !ROUTINE: LIS_elevdiff_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different elevdiff options
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)(float*);
} ELEVDIFF_TABLE; 
ELEVDIFF_TABLE elevdiff_table[2];

//BOP
// !ROUTINE: registerreadelevdiff
//  
// !DESCRIPTION: Registers the routines to open and 
// read elevdiff data
// 
// !INTERFACE:
void FTN(registerreadelevdiff)(int *i,void (*func)())
  //EOP
{ 
  elevdiff_table[*i].func = func; 
}

//BOP
// !ROUTINE: readelevdiff
//  
// !DESCRIPTION: Delegates the routines for 
// reading elevdiff files
// 
// !INTERFACE:
void FTN(readelevdiff)(int *i,float *array)
//EOP
{ 
  elevdiff_table[*i].func(array); 
}
