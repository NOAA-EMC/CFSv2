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
//  !ROUTINE: LIS_pcpforcing_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different observed precipitation 
//   forcing options
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)();
} PFORCING_GET_TABLE; 
PFORCING_GET_TABLE pforcing_get[5];

typedef struct
{ 
  void (*func)();
} PFORCING_NATRES_TABLE; 
PFORCING_NATRES_TABLE pforcing_natres[5];

typedef struct
{ 
  void (*func)();
} RFORCING_TI_TABLE; 
RFORCING_TI_TABLE pforcing_ti[5];

//BOP
// !ROUTINE: registerpget
//  
// !DESCRIPTION: Registers the routines to open and 
// read model forcing
// 
// !INTERFACE:
void FTN(registerpget)(int *i,void (*func)())
  //EOP
{ 
  pforcing_get[*i].func = func; 
}
//BOP
// !ROUTINE: glbprecip
//  
// !DESCRIPTION: 
//  Delegates the routine to open and read the appropriate
//  model forcing
// 
// !INTERFACE:
void FTN(glbprecip)(int *i)
  //EOP
{ 
  pforcing_get[*i].func(); 
}
//BOP
// !ROUTINE: register_defnatpcp
//  
// !DESCRIPTION: Registers the functions 
// that defines the native domain for observed
// precipitation products
// 
// !INTERFACE:
void FTN(registerdefnatpcp)(int *i, void (*func)())
//EOP
{ 
  pforcing_natres[*i].func = func; 
}
//BOP
// !ROUTINE: defNatResPcp
//  
// !DESCRIPTION: 
//  Delegates the routine to open and read the appropriate
//  model forcing
// 
// !INTERFACE:
void FTN(defnatrespcp)(int *i)
//EOP
{   
  pforcing_natres[*i].func(); 
}

//BOP
// !ROUTINE: registerpti
//  
// !DESCRIPTION: Registers the routines 
// to temporally interpolate precipitation 
// forcing routines
// 
// !INTERFACE:
void FTN(registerpti)(int *i,void (*func)())
  //EOP
{ 
  pforcing_ti[*i].func = func; 
}
//BOP
// !ROUTINE: timeinterppcp
//  
// !DESCRIPTION: 
//  Delegates the routine to temporaly interpolate
//  radiation forcing
// 
// !INTERFACE:
void FTN(timeinterppcp)(int *i)
  //EOP
{ 
  pforcing_ti[*i].func(); 
}
