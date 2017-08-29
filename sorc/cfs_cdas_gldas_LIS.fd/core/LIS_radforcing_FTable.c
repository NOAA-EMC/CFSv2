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
//  !ROUTINE: LIS_radforcing_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different observed radiation
//   forcing options
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)();
} RFORCING_GET_TABLE; 
RFORCING_GET_TABLE rforcing_get[5];

typedef struct
{ 
  void (*func)();
} RFORCING_NATRES_TABLE; 
RFORCING_NATRES_TABLE rforcing_natres[5];

typedef struct
{ 
  void (*func)();
} RFORCING_TI_TABLE; 
RFORCING_TI_TABLE rforcing_ti[5];


//BOP
// !ROUTINE: registerrget
//  
// !DESCRIPTION: Registers the routines to open and 
// read model forcing
// 
// !INTERFACE:
void FTN(registerrget)(int *i,void (*func)())
  //EOP
{ 
  rforcing_get[*i].func = func; 
}
//BOP
// !ROUTINE: getrad
//  
// !DESCRIPTION: 
//  Delegates the routine to open and read the appropriate
//  model forcing
// 
// !INTERFACE:
void FTN(getrad)(int *i)
  //EOP
{ 
  rforcing_get[*i].func(); 
}
//BOP
// !ROUTINE: registerdefnatrad
//  
// !DESCRIPTION: Registers the functions 
// that defines the native domain for observed
// radiation products
// 
// !INTERFACE:
void FTN(registerdefnatrad)(int *i, void (*func)())
//EOP
{ 
  rforcing_natres[*i].func = func; 
}
//BOP
// !ROUTINE: defnatresrad
//  
// !DESCRIPTION: 
//  Delegates the routine to open and read the appropriate
//  model forcing
// 
// !INTERFACE:
void FTN(defnatresrad)(int *i)
//EOP
{   
  rforcing_natres[*i].func(); 
}
//BOP
// !ROUTINE: registerrti
//  
// !DESCRIPTION: Registers the routines 
// to temporally interpolate radiation 
// forcing routines
// 
// !INTERFACE:
void FTN(registerrti)(int *i,void (*func)())
  //EOP
{ 
  rforcing_ti[*i].func = func; 
}
//BOP
// !ROUTINE: timeinterprad
//  
// !DESCRIPTION: 
//  Delegates the routine to temporaly interpolate
//  radiation forcing
// 
// !INTERFACE:
void FTN(timeinterprad)(int *i)
  //EOP
{ 
  rforcing_ti[*i].func(); 
}
