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
//  !ROUTINE: LIS_forcing_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different forcing options
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)();
} FORCING_GET_TABLE; 
FORCING_GET_TABLE forcing_get[6];

typedef struct
{ 
  void (*func)(float*);
} FORCING_NATRES_TABLE; 
FORCING_NATRES_TABLE forcing_natres[6];

typedef struct
{ 
  void (*func)();
} TINTERP_TABLE; 
TINTERP_TABLE tinterp[6];

//BOP
// !ROUTINE: registerget
//  
// !DESCRIPTION: Registers the routines to open and 
// read model forcing
// 
// !INTERFACE:
void FTN(registerget)(int *i,void (*func)())
  //EOP
{ 
  forcing_get[*i].func = func; 
}
//BOP
// !ROUTINE: getf
//  
// !DESCRIPTION: 
//  Delegates the routine to open and read the appropriate
//  model forcing
// 
// !INTERFACE:
void FTN(getf)(int *i)
  //EOP
{ 
  forcing_get[*i].func(); 
}
//BOP
// !ROUTINE: registertimeinterp
//  
// !DESCRIPTION: Registers the routines to 
//  perform temporal interpolation
// 
// !INTERFACE:
void FTN(registertimeinterp)(int *i,void (*func)())
  //EOP
{ 
  tinterp[*i].func = func; 
}
//BOP
// !ROUTINE: timeinterp
//  
// !DESCRIPTION: Delegates the routine to 
//  perform temporal interpolation
// 
// !INTERFACE:
void FTN(timeinterp)(int *i)
  //EOP
{ 
  tinterp[*i].func(); 
}
//BOP
// !ROUTINE: registerdefnat
//  
// !DESCRIPTION: 
// Registers functions that define the native domain 
// for model forcing products
// 
// !INTERFACE:
void FTN(registerdefnat)(int *i, void (*func)(float*))
  //EOP
{ 
  forcing_natres[*i].func = func; 
}
//BOP
// !ROUTINE: defnatres
//  
// !DESCRIPTION: 
//  Delegates the routine to open and read the appropriate
//  model forcing
// 
// !INTERFACE:
void FTN(defnatres)(int *i, float *kgds)
  //EOP
{   
  forcing_natres[*i].func(kgds); 
}
