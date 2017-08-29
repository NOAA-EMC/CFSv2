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
//  !ROUTINE: LIS_soils_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different soils options
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)(float*);
} LC_TABLE; 
LC_TABLE lc_table[6];

typedef struct
{ 
  void (*func)(float*);
} MASK_TABLE; 
MASK_TABLE mask_table[6];

//BOP
// !ROUTINE: registerrreadlc
//  
// !DESCRIPTION: Registers the routines to open and 
// read landcover data
// 
// !INTERFACE:
void FTN(registerreadlc)(int *i,void (*func)())
  //EOP
{ 
  lc_table[*i].func = func; 
}

//BOP
// !ROUTINE: readlandcover
//  
// !DESCRIPTION: Delegates the routines for 
// reading landcover
//
// !INTERFACE:
void FTN(readlandcover)(int *i,float *array)
//EOP
{ 
  lc_table[*i].func(array); 
}

//BOP
// !ROUTINE: registerrreadmask
//  
// !DESCRIPTION: Registers the routines to open and 
// read mask data
// 
// !INTERFACE:
void FTN(registerreadmask)(int *i,void (*func)())
  //EOP
{ 
  mask_table[*i].func = func; 
}

//BOP
// !ROUTINE: readlandmask
//  
// !DESCRIPTION: Delegates the routines for 
// reading mask
//
// !INTERFACE:
void FTN(readlandmask)(int *i,float *array)
//EOP
{ 
  mask_table[*i].func(array); 
}
