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
} SAND_TABLE; 
SAND_TABLE sand_table[6];

typedef struct
{ 
  void (*func)(float*);
} CLAY_TABLE; 
CLAY_TABLE clay_table[6];

typedef struct
{ 
  void (*func)(float*);
} SILT_TABLE; 
SILT_TABLE silt_table[6];

//BOP
// !ROUTINE: registerreadsand
//  
// !DESCRIPTION: Registers the routines to open and 
// read sand data
// 
// !INTERFACE:
void FTN(registerreadsand)(int *i,void (*func)())
  //EOP
{ 
  sand_table[*i].func = func; 
}

//BOP
// !ROUTINE: readsand
//  
// !DESCRIPTION: Delegates the routines for 
// reading sand files
// 
// !INTERFACE:
void FTN(readsand)(int *i,float *array)
//EOP
{ 
  sand_table[*i].func(array); 
}
//BOP
// !ROUTINE: registerreadclay
//  
// !DESCRIPTION: Registers the routines to open and 
// read clay data
// 
// !INTERFACE:
void FTN(registerreadclay)(int *i,void (*func)())
  //EOP
{ 
  clay_table[*i].func = func; 
}
//BOP
// !ROUTINE: readsand
//  
// !DESCRIPTION: Delegates the routines for 
// reading sand files
// 
// !INTERFACE:
void FTN(readclay)(int *i,float *array)
//EOP
{ 
  clay_table[*i].func(array); 
}

//BOP
// !ROUTINE: registerreadsilt
//  
// !DESCRIPTION: Registers the routines to open and 
// read silt data
// 
// !INTERFACE:
void FTN(registerreadsilt)(int *i,void (*func)())
  //EOP
{ 
  silt_table[*i].func = func; 
}
//BOP
// !ROUTINE: readsand
//  
// !DESCRIPTION: Delegates the routines for 
// reading sand files
// 
// !INTERFACE:
void FTN(readsilt)(int *i,float *array)
//EOP
{ 
  silt_table[*i].func(array); 
}




