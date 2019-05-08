/*
sstavg

 Reads several T/S netcdf restart files, does a weighted average of SST only
  writes a new restart file with averaged SST
 The files given on the command line are restart tarfiles.  The output tarfile
  will be a copy of the first input file, but with the weighted average SST.
 The T/S restart file is assumed to be named ocean_temp_salt.res.nc

 sstavg -w w1 w2 w3 ... tar1 tar2 tar3 ... tarout

 24 May 2010 David Behringer - original code

*/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <netcdf.h>

void sumSST(), wrtOutTar(), usage();

int nfls, eqWgts, first = 1;
float *wgt, sum;
double *rdtemp, *sumsst;
char **tarf, *tarout;
char *alfL = {"abcdefghijklmnopqrstuvwxyz"};
char *alfU = {"ABCDEFGHIJKLMNOPQRSTUVWXYZ"};
char *tsFile = {"ocean_temp_salt.res.nc"};
int imx, jmx, kmx;

int ncid, ncout, ncstat, ndims, nvars, natts, xdimid, vid;
int ncout, nodims, novars, vido, vsiz;
size_t dsiz, start[4], count[4];
char prog[41], rname[20];

main(argc, argv)
int argc;
char *argv[];
{
  FILE *fs;
  int n, len;
  float tno;
  char str[131];

  if (argc < 2) {
    usage();
    exit(0);
  }
  eqWgts = 0;
  if (strcmp(argv[1],"-w")) {
    nfls = argc - 2;
    eqWgts = 1;
  } else {
    nfls = (argc-3)/2;
    if ((argc-1)%2) {
      printf("An equal number of weights and tars plus an output must be given.\n");
      usage();
      exit(0);
    }
  }

  wgt = (float*)malloc(4*nfls);
  tarf = (char**)malloc(4*nfls);

  if (eqWgts) {
    for (n = 1; n < argc-1; n++) {
      *(tarf+n-1) = strdup(argv[n]);
      *(wgt+n-1) = 1.0;
      if (!strpbrk(*(tarf+n-1),alfL) && !strpbrk(*(tarf+n-1),alfU)) {
        printf("The argument %s appears to be a number.\n", *(tarf+n-1));
        printf("If weights are specified, the -w flag must precede them.\n");
        usage();
        exit(0);
      }
    }
  } else {
    for (n = 2; n < nfls+2; n++) {
      sscanf(argv[n],"%f",wgt+n-2);
    }
    for (n = nfls+2; n < argc-1; n++) {
      *(tarf+n-nfls-2) = strdup(argv[n]);
    }
  }
  tarout = strdup(argv[argc-1]);

  system("ls *.tar > sa_scrtch");
  fs = fopen("sa_scrtch","r");
/*  while (fgets(str,80,fs)) { */
  while (fgets(str,120,fs)) {
    len = strlen(str) - 1;
    str[len] = '\0';
    if (!strcmp(str,tarout)) {
      fclose(fs);
      system("rm sa_scrtch");
      printf("Output file %s exists and cannot be overwritten.\n", tarout);
      usage();
      exit(0);
    }
  }
  fclose(fs);
  system("rm sa_scrtch");

  sum = 0.0;
  for (n = 0; n < nfls; n++) {
    sum += *(wgt+n);
  }
  for (n = 0; n < nfls; n++) {
    *(wgt+n) /= sum;
  }

  for (n = 0; n < nfls; n++) {
    sumSST(n);
    first = 0;
  }

  wrtOutTar();

}

/* ============================================================ */

void sumSST(nf)
int nf;
{
  int i, j, k, n;
/*  char cmd[81]; */
  char cmd[121];

/* untar the TS file from the restart  */

  sprintf(cmd,"tar -xvf %s %s", *(tarf+nf), tsFile);
  system(cmd);

/*  get SST from TS restart file  */

  ncstat = nc_open(tsFile, NC_NOWRITE, &ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot open %s\n", tsFile);
    exit(0);
  }

  ncstat = nc_inq(ncid, &ndims, &nvars, &natts, &xdimid);
  if (ncstat != NC_NOERR) {
    printf("Inquiry failed for %s\n", tsFile);
    exit(0);
  }
  for (n = 0; n < ndims; n++) {
    ncstat = nc_inq_dim(ncid, n, rname, &dsiz);
    if (ncstat != NC_NOERR) {
      printf("Cannot read dimension no. %d of %s\n", n, tsFile);
      exit(0);
    }
    if (strstr(rname,"xaxis_1") != NULL) {
      i = dsiz;
    } else if(strstr(rname,"yaxis_1") != NULL) {
      j = dsiz;
    } else if(strstr(rname,"zaxis_1") != NULL) {
      k = dsiz;
    }
  }

  if (first) {
    imx = i;
    jmx = j;
    kmx = k;
    rdtemp = (double*)malloc(8*imx*jmx);
    sumsst = (double*)malloc(8*imx*jmx);
    *start = 0;
    *(start+1) = 0;
    *(start+2) = 0;
    *(start+3) = 0;
    *count = 1;
    *(count+1) = 1;
    *(count+2) = jmx;
    *(count+3) = imx;
  } else {
    if (i != imx || j != jmx || k != kmx) {
      printf("%s and %s dimension mismatch\n", *(tarf+1), *(tarf+nf));
      printf("%s:  %d, %d, %d\n", *(tarf+1),imx,jmx,kmx);
      printf("%s:  %d, %d, %d\n", *(tarf+nf),i,j,k);
      exit(0);
    }
  }

  strcpy(rname,"temp");
  ncstat = nc_inq_varid(ncid,rname,&vid);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, tsFile);
    exit(0);
  }
  ncstat = nc_get_vara_double(ncid,vid,start,count,rdtemp);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, tsFile);
    exit(0);
  }
  if (first) {
    for (n = 0; n < imx*jmx; n++) {
      *(sumsst+n) = *(rdtemp+n) * *(wgt+nf);
    }
  } else {
    for (n = 0; n < imx*jmx; n++) {
      *(sumsst+n) += *(rdtemp+n) * *(wgt+nf);
    }
  }

  ncstat = nc_close(ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot close %s without error\n", tsFile);
  }

  sprintf(cmd,"rm %s", tsFile);
  system(cmd);
}

/* ============================================================ */

void wrtOutTar()
{
  int i, j, k, n;
  char cmd[81];

/* untar the entire first input tar */

  sprintf(cmd,"tar -xvf %s", *tarf);
  system(cmd);

/* overwrite top level of temperture field with averaged SST */

  ncstat = nc_open(tsFile, NC_WRITE, &ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot open %s\n", tsFile);
    exit(0);
  }

  strcpy(rname,"temp");
  ncstat = nc_inq_varid(ncid,rname,&vid);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, tsFile);
    exit(0);
  }
  *start = 0;
  *(start+1) = 0;
  *(start+2) = 0;
  *(start+3) = 0;
  *count = 1;
  *(count+1) = 1;
  *(count+2) = jmx;
  *(count+3) = imx;
  ncstat = nc_put_vara_double(ncid,vid,start,count,sumsst);
  if (ncstat != NC_NOERR) {
    printf("Cannot put %s variable put %s\n", rname, tsFile);
    exit(0);
  }

  ncstat = nc_close(ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot close %s without error\n", tsFile);
  }

/* retar  */

  sprintf(cmd,"tar -cvf %s *res*", tarout);
  system(cmd);

  sprintf(cmd,"rm *res*");
  system(cmd);

}

/* ============================================================ */

void usage()
{
  printf("Usage:\n");
  printf(" %s sstavg -w w1 w2 w3 ... tar1 tar2 tar3 ... tarout\n");
  printf("   -w  if present, weights follow; if absent, equal weights assumed\n");
  printf("   w1 w2 w3 ...       weights sum(wi) = 1.0\n");
  printf("   tar1 tar2 tar3 ... equal number of tarfiles\n");
  printf("   tarout             output tarfile\n");
  printf("  The weights will be adjusted to sum to 1.0\n");
  printf("  The tarfiles contain netcdf restart files for\n");
  printf("  temperature and salinity: ocean_temp_salt.res.nc\n");
  printf("  The output tarout is the same as tar1, except the\n");
  printf("  SST is a weighted average of the SSTs from all the\n");
  printf("  input tarfiles.\n");
}
