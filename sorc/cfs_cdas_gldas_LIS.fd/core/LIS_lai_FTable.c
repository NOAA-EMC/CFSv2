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
//  !ROUTINE: LIS_lai_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different lai options
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)(float*, float*, float*, float*);
} LAI_TABLE; 
LAI_TABLE lai_table[6];

typedef struct
{ 
  void (*func)(float*, float*, float*, float*);
} SAI_TABLE; 
SAI_TABLE sai_table[6];

//BOP
// !ROUTINE: registerreadlai
//  
// !DESCRIPTION: Registers the routines to open and 
// read lai data
// 
// !INTERFACE:
void FTN(registerreadlai)(int *i,void (*func)())
  //EOP
{ 
  lai_table[*i].func = func; 
}

//BOP
// !ROUTINE: readlai
//  
// !DESCRIPTION: Delegates the routines for 
// reading lai files
// 
// !INTERFACE:
void FTN(readlai)(int *i,float *array1, float *array2, float* wt1, float* wt2)
//EOP
{ 
  lai_table[*i].func(array1, array2, wt1, wt2); 
}
//BOP
// !ROUTINE: registerreadsai
//  
// !DESCRIPTION: Registers the routines to open and 
// read sai data
// 
// !INTERFACE:
void FTN(registerreadsai)(int *i,void (*func)())
  //EOP
{ 
  sai_table[*i].func = func; 
}
//BOP
// !ROUTINE: readsai
//  
// !DESCRIPTION: Delegates the routines for 
// reading sai files
// 
// !INTERFACE:
void FTN(readsai)(int *i,float *array1, float *array2, float* wt1, float* wt2)
//EOP
{ 
  sai_table[*i].func(array1, array2, wt1, wt2); 
}

































