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
#include <string.h>
#include "ftn.h"

void FTN(lis_log_msg)(char *, int);
void FTN(lis_log_blocked_msg)(char *, int);

//BOP
// !ROUTINE: lis_log_msgC.F90
//
// !DESCRIPTION:
//  This routine is a C wrapper to the lis\_log\_msg routine.
//
// !REVISION HISTORY:
//  12 Mar 2004: James Geiger; Initial version
//
// !INTERFACE:
void lis_log_msgC(char * string)
//EOP
{
//BOC
   FTN(lis_log_msg)(string, strlen(string));
//EOC
}

//BOP
// !ROUTINE: lis_log_blocked_msgC.F90
//
// !DESCRIPTION:
//  This routine is a C wrapper to the lis\_log\_blocked\_msg routine.
//
// !REVISION HISTORY:
//  02 Sep 2004: James Geiger; Initial version
//
// !INTERFACE:
void lis_log_blocked_msgC(char * string)
//EOP
{
//BOC
   FTN(lis_log_blocked_msg)(string, strlen(string));
//EOC
}
