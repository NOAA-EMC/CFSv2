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
// !ROUTINE: LIS_lsm_FTable
//  
//
// !DESCRIPTION:
//   Function table implementation for different land surface schemes
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
typedef struct
{ 
  void (*func)(int*);
} LSM_INI_TABLE; 
LSM_INI_TABLE lsm_ini[8];

typedef struct
{ 
  void (*func)();
} LSM_RUN_TABLE; 
LSM_RUN_TABLE lsm_run[8];

typedef struct
{ 
  void (*func)();
} LSM_SETUP_TABLE; 
LSM_SETUP_TABLE lsm_setup[8];

typedef struct
{ 
  void (*func)();
} LSM_DYNSETUP_TABLE; 
LSM_DYNSETUP_TABLE lsm_dynsetup[8];

typedef struct
{ 
  void (*func)(int*);
} LSM_RESTART_TABLE; 
LSM_RESTART_TABLE lsm_restart[8];

typedef struct
{ 
  void (*func)();
} LSM_OUTPUT_TABLE; 
LSM_OUTPUT_TABLE lsm_output[8];

typedef struct
{ 
  void (*func)(int*, float*);
} LSM_F2T_TABLE; 
LSM_F2T_TABLE lsm_f2t[8];

typedef struct
{ 
  void (*func)();
} LSM_WRITERST_TABLE; 
LSM_WRITERST_TABLE lsm_wrst[8];

//static int nfuncs_ini = 0; 
//static int nfuncs_run = 0; 
//static int nfuncs_setup = 0; 
//static int nfuncs_dynsetup = 0; 
//static int nfuncs_restart = 0; 
//static int nfuncs_output = 0 ; 
//static int nfuncs_f2t = 0 ; 
//static int nfuncs_wrst = 0 ; 
//BOP
// !ROUTINE: registerlsmini
//  
// !DESCRIPTION: Registers the routines for LSM 
// initializations
// 
// !INTERFACE:
void FTN(registerlsmini)(int *i, void (*func)(int*))
//EOP
{ 
  lsm_ini[*i].func = func; 
}
//BOP
// !ROUTINE: lsmini
//  
// !DESCRIPTION: Delegates the routines for LSM
// initializations
// 
// !INTERFACE:
void FTN(lsmini)(int *i,int *nch)
//EOP
{ 
  lsm_ini[*i].func(nch); 
}
//BOP
// !ROUTINE: registerlsmrun
//  
// !DESCRIPTION: Registers the routines for LSM 
// runs
// 
// !INTERFACE:
void FTN(registerlsmrun)(int *i, void (*func)())
//EOP
{ 
  lsm_run[*i].func = func; 
}
//BOP
// !ROUTINE: lsmrun
//  
// !DESCRIPTION: Delegates the routines for LSM 
// runs
// 
// !INTERFACE:
void FTN(lsmrun)(int *i)
//EOP
{ 
  lsm_run[*i].func(); 
}

//BOP
// !ROUTINE: registerlsmsetup
//  
// !DESCRIPTION: Registers the routines for LSM 
// setup
// 
// !INTERFACE:
void FTN(registerlsmsetup)(int *i, void (*func)())
//EOP
{ 
  lsm_setup[*i].func = func; 
}
//BOP
// !ROUTINE: lsmsetup
//  
// !DESCRIPTION: Delegates the routines for LSM 
// setup
// 
// !INTERFACE:
void FTN(lsmsetup)(int *i)
//EOP
{ 
  lsm_setup[*i].func(); 
}
//BOP
// !ROUTINE: registerlsmdynsetup
//  
// !DESCRIPTION: Registers the routines for  
// time dependent LSM setups
// 
// !INTERFACE:
void FTN(registerlsmdynsetup)(int *i, void (*func)())
//EOP
{ 
  lsm_dynsetup[*i].func = func; 
}
//BOP
// !ROUTINE: lsmdynsetup
//  
// !DESCRIPTION: Delegates the routines for 
// time dependent LSM setups
// 
// !INTERFACE:
void FTN(lsmdynsetup)(int *i)
//EOP
{ 
  lsm_dynsetup[*i].func(); 
}
//BOP
// !ROUTINE: registerlsmstart
//  
// !DESCRIPTION: Registers the routines for LSM
// restart from a previously saved state
// 
// !INTERFACE:
void FTN(registerlsmrestart)(int *i, void (*func)(int*))
//EOP
{ 
  lsm_restart[*i].func = func; 
}
//BOP
// !ROUTINE: lsmstart
//  
// !DESCRIPTION: Delegates the routines for LSM
// restart from a previously saved state
// 
// !INTERFACE:
void FTN(lsmrestart)(int *i, int *rw)
//EOP
{ 
  lsm_restart[*i].func(rw); 
}

//BOP
// !ROUTINE: registerlsmoutput
//  
// !DESCRIPTION: Registers the routines for writing
// LSM output 
// 
// !INTERFACE:
void FTN(registerlsmoutput)(int *i, void (*func)())
//EOP
{ 
  lsm_output[*i].func = func; 
}
//BOP
// !ROUTINE: lsmoutput
//  
// !DESCRIPTION: Delegates the routines for writing
// LSM output 
// 
// !INTERFACE:
void FTN(lsmoutput)(int *i)
//EOP
{ 
  lsm_output[*i].func(); 
}

//BOP
// !ROUTINE: registerlsmf2t
//  
// !DESCRIPTION: Registers the routines for 
// transferring forcing to model tiles
// 
// !INTERFACE:
void FTN(registerlsmf2t)(int *i, void (*func)(int*, float*))
//EOP
{ 
  lsm_f2t[*i].func = func; 
}
//BOP
// !ROUTINE: lsmf2t
//  
// !DESCRIPTION: Delegates the routines for 
// transferring forcing to model tiles
// 
// !INTERFACE:
void FTN(lsmf2t)(int *i, int *index, float *forcing)
//EOP
{ 
  lsm_f2t[(*i)].func(index, forcing); 
}

//BOP
// !ROUTINE: registerlsmwrst
//  
// !DESCRIPTION: Registers the routines to write
// restart files for different LSMs
// 
// !INTERFACE:
void FTN(registerlsmwrst)(int *i, void (*func)())
//EOP
{ 
  lsm_wrst[*i].func = func; 
}
//BOP
// !ROUTINE: lsmwrst
//  
// !DESCRIPTION: Delegates the routines to 
// write restart routines
// 
// !INTERFACE:
void FTN(lsmwrst)(int *i)
//EOP
{ 
  lsm_wrst[(*i)].func(); 
}
