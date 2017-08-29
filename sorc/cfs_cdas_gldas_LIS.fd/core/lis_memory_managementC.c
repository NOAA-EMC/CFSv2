#include <stdio.h>
#include <stdlib.h>
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
#include <string.h>
#include "ftn_drv.h"
void lis_log_msgC(char *);

void FTN(endrun)(void);

//BOP
// !ROUTINE: lis_calloc
// 
// !DESCRIPTION: 
// 
// Generic call to the C calloc function used in LIS.
//
// !REVISION HISTORY:
// Mar 2004, James Geiger; Initial Specification
// !INTERFACE:
void * lis_calloc(size_t n, size_t size, char * caller)
//EOP
{
   void * ptr;
   char * msg;

   int count;
   int len;

   ptr = calloc(n,size);

   if ( ptr == NULL )
   {
      len = 30 + strlen(caller) + 1;
      msg = (char *) malloc(len);
      count = sprintf(msg,"ERR: %s -- Cannot allocate memory",caller);

      if ( count != len-1 )
      {
         lis_log_msgC("ERR: lis_calloc -- "
                      "Cannot allocate memory for msg string");
         FTN(endrun)();
      }
      
      lis_log_msgC(msg);
      FTN(endrun)();
   }

   return ptr;
}

//BOP
// !ROUTINE: lis_malloc
// 
// !DESCRIPTION: 
// 
// Generic call to the C malloc function used in LIS.
//
// !REVISION HISTORY:
// Mar 2004, James Geiger; Initial Specification
// !INTERFACE:
void * lis_malloc(size_t size, char * caller)
//EOP
{
   void * ptr;
   char * msg;

   int count;
   int len;

   ptr = malloc(size);

   if ( ptr == NULL )
   {
      len = 30 + strlen(caller) + 1;
      msg = (char *) malloc(len);
      count = sprintf(msg,"ERR: %s -- Cannot allocate memory",caller);

      if ( count != len-1 )
      {
         lis_log_msgC("ERR: lis_malloc -- "
                      "Cannot allocate memory for msg string");
         FTN(endrun)();
      }
      
      lis_log_msgC(msg);
      FTN(endrun)();
   }

   return ptr;
}
