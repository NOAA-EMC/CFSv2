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
//  !ROUTINE: LIS_domain_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different 
//  domain definitions
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)();
} DOMAIN_GET_TABLE; 
DOMAIN_GET_TABLE domain_get[10];

typedef struct
{ 
  void (*func)();
} INPUT_FUNC_TABLE; 
INPUT_FUNC_TABLE input_func[10];

//BOP
// !ROUTINE: registerdomain
//  
// !DESCRIPTION: Registers the routines to open and 
// read model forcing
// 
// !INTERFACE:
void FTN(registerdomain)(int *i,void (*func)())
  //EOP
{ 
  domain_get[*i].func = func; 
}
//BOP
// !ROUTINE: getrad
//  
// !DESCRIPTION: 
//  Delegates the routine to create the domain
//
// 
// !INTERFACE:
void FTN(makedomain)(int *i)
  //EOP
{ 
  domain_get[*i].func(); 
}


//BOP
// !ROUTINE: registerinput
//  
// !DESCRIPTION: Registers the routines to
// read the input card file
// 
// !INTERFACE:
void FTN(registerinput)(int *i,void (*func)())
  //EOP
{ 
  input_func[*i].func = func; 
}
//BOP
// !ROUTINE: readinput
//  
// !DESCRIPTION: 
//  Delegates the input card file
//
// 
// !INTERFACE:
void FTN(readinput)(int *i)
  //EOP
{ 
  input_func[*i].func(); 
}
