/*   rstrt2tm rewrites restart fields in time_mean format  */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "netcdf.h"
#include "Tm.h"

void rwTSFields(), rwUVFields(), usage();
unsigned int YearDay();

int imx, jmx, kmx;
float *varo, mV;
double *vard;
double Tm, dZero;
int inclCor = 0, addRstr = 0, dbg = 0;
char *cmn[13] = {"dummy", "January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December"};
unsigned short dpm[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
int year, month, day, hour, year0, month0, day0, hour0;
YMD tm;

int ncid, ncout, ncstat, ndims, nvars, natts, xdimid, vid;
int ncout, nodims, novars, vido, vsiz;
size_t dsiz, start[4], count[4];
char rname[20];
char prog[121], tsFile[121], eFile[121], corFile[121], rstFile[121], uvFile[121], dtFile[121];
char tpTag[121], tpFile[121], outTag[121], outFile[121];

main(argc, argv)
int argc;
char *argv[];
{
  FILE *fs;
  int n;
  char str[131];

  strcpy(tsFile,"EMPTY");
  strcpy(corFile,"EMPTY");
  strcpy(rstFile,"EMPTY");
  strcpy(eFile,"EMPTY");
  strcpy(uvFile,"EMPTY");
  strcpy(dtFile,"EMPTY");
  strcpy(outTag,"EMPTY");
  strcpy(tpTag,"EMPTY");
  strcpy(prog,argv[0]);
  n = 1;
  while (n < argc) {
    if (!strcmp(argv[n],"-t")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -t has a netCDF filename as argument.\n");
        usage();
        exit(0);
      }
      strcpy(tsFile, argv[n]);
    } else if (!strcmp(argv[n],"-c")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -c has a netCDF filename as argument.\n");
        usage();
        exit(0);
      }
      strcpy(corFile, argv[n]);
      inclCor = 1;
    } else if (!strcmp(argv[n],"-r")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -r has a netCDF filename as argument.\n");
        usage();
        exit(0);
      }
      strcpy(rstFile, argv[n]);
      addRstr = 1;
    } else if (!strcmp(argv[n],"-e")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -e has a netCDF filename as argument.\n");
        usage();
        exit(0);
      }
      strcpy(eFile, argv[n]);
    } else if (!strcmp(argv[n],"-u")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -u has a netCDF filename as argument.\n");
        usage();
        exit(0);
      }
      strcpy(uvFile, argv[n]);
    } else if (!strcmp(argv[n],"-d")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -d has an ASCII filename as argument.\n");
        usage();
        exit(0);
      }
      strcpy(dtFile, argv[n]);
    } else if (!strcmp(argv[n],"-o")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -o has a filetag as an argument.\n");
        usage();
        exit(0);
      }
      strcpy(outTag, argv[n]);
    } else if (!strcmp(argv[n],"-tp")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("The option -tp has a filetag as an argument.\n");
        usage();
        exit(0);
      }
      strcpy(tpTag, argv[n]);
    } else if (!strcmp(argv[n],"-h")) {
      usage();
      exit(0);
    } else {
      printf("Error: %s is not an option.\n", argv[n]);
      usage();
      exit(0);
    }
    n++;
  }
  if (!strcmp(tsFile,"EMPTY")) {
    printf("Error: a TS input netCDF global restart file must be given.\n");
    usage();
    exit(0);
  }
  if (!strcmp(eFile,"EMPTY")) {
    printf("Error: an Eta input netCDF global restart file must be given.\n");
    usage();
    exit(0);
  }
  if (!strcmp(uvFile,"EMPTY")) {
    printf("Error: a UV input netCDF global restart file must be given.\n");
    usage();
    exit(0);
  }
  if (!strcmp(dtFile,"EMPTY")) {
    printf("Error: a Date input ASCII restart file must be given.\n");
    usage();
    exit(0);
  }
  if (!strcmp(tpTag,"EMPTY")) {
    printf("Error: A template tag must be given.\n");
    usage();
    exit(0);
  }
  if (!strcmp(outTag,"EMPTY")) {
    printf("Error: An output tag must be given.\n");
    usage();
    exit(0);
  }
  fs = fopen(dtFile, "r");
  fgets(str,120,fs);
  fgets(str,120,fs);
  sscanf(str,"%d%d%d%d", &year0, &month0, &day0, &hour0);
  fgets(str,120,fs);
  sscanf(str,"%d%d%d%d", &year, &month, &day, &hour);
  fclose(fs);
  printf("Date: %2d %s %d\n", day, *(cmn+month), year);
  tm.year = year;
  tm.month = month;
  tm.day = day;
  tm.hour = hour;
  tm.year0 = year0;
  tm.yday = YearDay(tm);
  Tm = (double)tm.yday + (double)tm.hour/24.0;
  dZero = 0.0;

  rwTSFields();

  rwUVFields();

}

/* ============================================================ */

void rwTSFields()
{
  int i, j, k, n;
  char cmd[81];

/* Rewrite variables on temperature grid. */

  strcpy(tpFile,tpTag);
  strcat(tpFile,"TS.nc");
  sprintf(outFile,"%4d%2.2d%2.2d%2.2d.%sTS.nc",tm.year,tm.month,tm.day,tm.hour,outTag);
  sprintf(cmd,"cp %s %s", tpFile, outFile);
  system(cmd);
  printf("Writing restart data to %s\n", outFile);

  ncstat = nc_open(outFile, NC_WRITE, &ncout);
  if (ncstat != NC_NOERR) {
    printf("Cannot open %s\n", outFile);
    exit(0);
  }

  ncstat = nc_inq(ncout, &nodims, &novars, &natts, &xdimid);
  if (ncstat != NC_NOERR) {
    printf("Inquiry failed for %s\n", outFile);
    exit(0);
  }

  for (n = 0; n < nodims; n++) {
    ncstat = nc_inq_dim(ncout, n, rname, &dsiz);
    if (ncstat != NC_NOERR) {
      printf("Cannot read dimension no. %d of %s\n", n, outFile);
      exit(0);
    }
    if (strstr(rname,"xt")) {
      imx = dsiz;
    } else if(strstr(rname,"yt")) {
      jmx = dsiz;
    } else if(strstr(rname,"zt")) {
      kmx = dsiz;
    }
  }
    
/*  get temperature and salinity from TS restart file  */

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
    if (strstr(rname,"xa") != NULL) {
      i = dsiz;
    } else if(strstr(rname,"ya") != NULL) {
      j = dsiz;
    } else if(strstr(rname,"za") != NULL) {
      k = dsiz;
    }
  }

  if (i != imx || j != jmx || k != kmx) {
    printf("%s and %s dimension mismatch\n", outFile, tsFile);
    printf("%s:  %d, %d, %d\n", outFile,imx,jmx,kmx);
    printf("%s:  %d, %d, %d\n", tsFile,i,j,k);
    exit(0);
  }

  strcpy(rname,"temp");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  vsiz = imx*jmx*kmx;
  varo = (float*)realloc(varo,4*vsiz);
  vard = (double*)realloc(vard,8*vsiz);
  *start = 0;
  *(start+1) = 0;
  *(start+2) = 0;
  *(start+3) = 0;
  *count = 1;
  *(count+1) = kmx;
  *(count+2) = jmx;
  *(count+3) = imx;
  ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_inq_varid(ncid,rname,&vid);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, tsFile);
    exit(0);
  }
  ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, tsFile);
    exit(0);
  }
  for (n = 0; n < vsiz; n++) {
    if (*(varo+n) != mV) *(varo+n) = *(vard+n);
  }
  ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }

  strcpy(rname,"salt");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  vsiz = imx*jmx*kmx;
  varo = (float*)realloc(varo,4*vsiz);
  vard = (double*)realloc(vard,8*vsiz);
  ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_inq_varid(ncid,rname,&vid);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, tsFile);
    exit(0);
  }
  ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, tsFile);
    exit(0);
  }
  for (n = 0; n < vsiz; n++) {
    if (*(varo+n) != mV) *(varo+n) = *(vard+n);
  }
  ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }

  ncstat = nc_close(ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot close %s without error\n", tsFile);
    exit(0);
  }

/*  get free surface from restart file  */

  ncstat = nc_open(eFile, NC_NOWRITE, &ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot open %s\n", eFile);
    exit(0);
  }

  ncstat = nc_inq(ncid, &ndims, &nvars, &natts, &xdimid);
  if (ncstat != NC_NOERR) {
    printf("Inquiry failed for %s\n", eFile);
    exit(0);
  }
  for (n = 0; n < ndims; n++) {
    ncstat = nc_inq_dim(ncid, n, rname, &dsiz);
    if (ncstat != NC_NOERR) {
      printf("Cannot read dimension no. %d of %s\n", n, eFile);
      exit(0);
    }
    if (strstr(rname,"xa") != NULL) {
      i = dsiz;
    } else if(strstr(rname,"ya") != NULL) {
      j = dsiz;
    }
  }

  if (i != imx || j != jmx) {
    printf("%s and %s dimension mismatch\n", outFile, eFile);
    printf("%s:  %d, %d\n", outFile,imx,jmx);
    printf("%s:  %d, %d\n", eFile,i,j);
    exit(0);
  }

  strcpy(rname,"eta_t");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  vsiz = imx*jmx;
  varo = (float*)realloc(varo,4*vsiz);
  vard = (double*)realloc(vard,8*vsiz);
  *start = 0;
  *(start+1) = 0;
  *(start+2) = 0;
  *count = 1;
  *(count+1) = jmx;
  *(count+2) = imx;
  ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_inq_varid(ncid,rname,&vid);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, eFile);
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
  ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, eFile);
    exit(0);
  }
  for (n = 0; n < vsiz; n++) {
    if (*(varo+n) != mV) *(varo+n) = *(vard+n);
  }
  *start = 0;
  *(start+1) = 0;
  *(start+2) = 0;
  *count = 1;
  *(count+1) = jmx;
  *(count+2) = imx;
  ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }

  ncstat = nc_close(ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot close %s without error\n", eFile);
    exit(0);
  }

/*  get temperature and salinity corrections from COR restart file  */

  if (inclCor) {
    ncstat = nc_open(corFile, NC_NOWRITE, &ncid);
    if (ncstat != NC_NOERR) {
      printf("Cannot open %s\n", corFile);
      exit(0);
    }
  
    ncstat = nc_inq(ncid, &ndims, &nvars, &natts, &xdimid);
    if (ncstat != NC_NOERR) {
      printf("Inquiry failed for %s\n", corFile);
      exit(0);
    }
    for (n = 0; n < ndims; n++) {
      ncstat = nc_inq_dim(ncid, n, rname, &dsiz);
      if (ncstat != NC_NOERR) {
        printf("Cannot read dimension no. %d of %s\n", n, corFile);
        exit(0);
      }
      if (strstr(rname,"xa") != NULL) {
        i = dsiz;
      } else if(strstr(rname,"ya") != NULL) {
        j = dsiz;
      } else if(strstr(rname,"za") != NULL) {
        k = dsiz;
      }
    }
  
    if (i != imx || j != jmx || k != kmx) {
      printf("%s and %s dimension mismatch\n", outFile, corFile);
      printf("%s:  %d, %d, %d\n", outFile,imx,jmx,kmx);
      printf("%s:  %d, %d, %d\n", corFile,i,j,k);
      exit(0);
    }
  
    strcpy(rname,"tcor");
    ncstat = nc_inq_varid(ncout, rname, &vido);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, outFile);
      exit(0);
    }
    vsiz = imx*jmx*kmx;
    varo = (float*)realloc(varo,4*vsiz);
    vard = (double*)realloc(vard,8*vsiz);
    *start = 0;
    *(start+1) = 0;
    *(start+2) = 0;
    *(start+3) = 0;
    *count = 1;
    *(count+1) = kmx;
    *(count+2) = jmx;
    *(count+3) = imx;
    ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, outFile);
      exit(0);
    }
    ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
      exit(0);
    }
    ncstat = nc_inq_varid(ncid,rname,&vid);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, corFile);
      exit(0);
    }
    ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, corFile);
      exit(0);
    }
    for (n = 0; n < vsiz; n++) {
      if (*(varo+n) != mV) *(varo+n) = *(vard+n);
    }
    ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot rewrite %s variable to %s\n", rname, outFile);
      exit(0);
    }
  
    strcpy(rname,"scor");
    ncstat = nc_inq_varid(ncout, rname, &vido);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, outFile);
      exit(0);
    }
    vsiz = imx*jmx*kmx;
    varo = (float*)realloc(varo,4*vsiz);
    vard = (double*)realloc(vard,8*vsiz);
    ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, outFile);
      exit(0);
    }
    ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
      exit(0);
    }
    ncstat = nc_inq_varid(ncid,rname,&vid);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, corFile);
      exit(0);
    }
    ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, corFile);
      exit(0);
    }
    for (n = 0; n < vsiz; n++) {
      if (*(varo+n) != mV) *(varo+n) = *(vard+n);
    }
    ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot rewrite %s variable to %s\n", rname, outFile);
      exit(0);
    }
  
    ncstat = nc_close(ncid);
    if (ncstat != NC_NOERR) {
      printf("Cannot close %s without error\n", corFile);
      exit(0);
    }
  }

/*  get temperature and salinity corrections from RSTR restart file  */

  if (addRstr) {
    ncstat = nc_open(rstFile, NC_NOWRITE, &ncid);
    if (ncstat != NC_NOERR) {
      printf("Cannot open %s\n", rstFile);
      exit(0);
    }
  
    ncstat = nc_inq(ncid, &ndims, &nvars, &natts, &xdimid);
    if (ncstat != NC_NOERR) {
      printf("Inquiry failed for %s\n", rstFile);
      exit(0);
    }
    for (n = 0; n < ndims; n++) {
      ncstat = nc_inq_dim(ncid, n, rname, &dsiz);
      if (ncstat != NC_NOERR) {
        printf("Cannot read dimension no. %d of %s\n", n, rstFile);
        exit(0);
      }
      if (strstr(rname,"xa") != NULL) {
        i = dsiz;
      } else if(strstr(rname,"ya") != NULL) {
        j = dsiz;
      }
    }
  
    if (i != imx || j != jmx) {
      printf("%s and %s dimension mismatch\n", outFile, rstFile);
      printf("%s:  %d, %d\n", outFile,imx,jmx);
      printf("%s:  %d, %d\n", rstFile,i,j);
      exit(0);
    }
  
    strcpy(rname,"tcor");
    ncstat = nc_inq_varid(ncout, rname, &vido);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, outFile);
      exit(0);
    }
    vsiz = imx*jmx;
    varo = (float*)realloc(varo,4*vsiz);
    vard = (double*)realloc(vard,8*vsiz);
    *start = 0;
    *(start+1) = 0;
    *(start+2) = 0;
    *(start+3) = 0;
    *count = 1;
    *(count+1) = 1;
    *(count+2) = jmx;
    *(count+3) = imx;
    ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, outFile);
      exit(0);
    }
    ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
      exit(0);
    }
    strcpy(rname,"trstr");
    ncstat = nc_inq_varid(ncid,rname,&vid);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, rstFile);
      exit(0);
    }
    ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, rstFile);
      exit(0);
    }
    for (n = 0; n < vsiz; n++) {
      if (*(varo+n) != mV) *(varo+n) = *(vard+n);
    }
    strcpy(rname,"tcor");
    ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot rewrite %s variable to %s\n", rname, outFile);
      exit(0);
    }
  
    strcpy(rname,"scor");
    ncstat = nc_inq_varid(ncout, rname, &vido);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, outFile);
      exit(0);
    }
    vsiz = imx*jmx;
    varo = (float*)realloc(varo,4*vsiz);
    vard = (double*)realloc(vard,8*vsiz);
    ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, outFile);
      exit(0);
    }
    ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
      exit(0);
    }
    strcpy(rname,"srstr");
    ncstat = nc_inq_varid(ncid,rname,&vid);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s id from %s\n", rname, rstFile);
      exit(0);
    }
    ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
    if (ncstat != NC_NOERR) {
      printf("Cannot read %s variable from %s\n", rname, rstFile);
      exit(0);
    }
    for (n = 0; n < vsiz; n++) {
      if (*(varo+n) != mV) *(varo+n) = *(vard+n);
    }
    strcpy(rname,"scor");
    ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
    if (ncstat != NC_NOERR) {
      printf("Cannot rewrite %s variable to %s\n", rname, outFile);
      exit(0);
    }
  
    ncstat = nc_close(ncid);
    if (ncstat != NC_NOERR) {
      printf("Cannot close %s without error\n", rstFile);
      exit(0);
    }
  }

/* Fill in time variables.   */

  strcpy(rname,"time");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&Tm);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }
  strcpy(rname,"average_T1");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&Tm);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }
  strcpy(rname,"average_T2");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&Tm);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }
  strcpy(rname,"average_DT");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&dZero);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }

  ncstat = nc_close(ncout);
  if (ncstat != NC_NOERR) {
    printf("Cannot close %s without error\n", outFile);
  }
}

/* ============================================================ */

void rwUVFields()
{
  int i, j, k, n;
  char cmd[81];

/* Rewrite variables on temperature grid. */

  strcpy(tpFile,tpTag);
  strcat(tpFile,"UV.nc");
  sprintf(outFile,"%4d%2.2d%2.2d%2.2d.%sUV.nc",tm.year,tm.month,tm.day,tm.hour,outTag);
  sprintf(cmd,"cp %s %s", tpFile, outFile);
  system(cmd);
  printf("Writing restart data to %s\n", outFile);

  ncstat = nc_open(outFile, NC_WRITE, &ncout);
  if (ncstat != NC_NOERR) {
    printf("Cannot open %s\n", outFile);
    exit(0);
  }

  ncstat = nc_inq(ncout, &nodims, &novars, &natts, &xdimid);
  if (ncstat != NC_NOERR) {
    printf("Inquiry failed for %s\n", outFile);
    exit(0);
  }

  for (n = 0; n < nodims; n++) {
    ncstat = nc_inq_dim(ncout, n, rname, &dsiz);
    if (ncstat != NC_NOERR) {
      printf("Cannot read dimension no. %d of %s\n", n, outFile);
      exit(0);
    }
    if (strstr(rname,"xu")) {
      imx = dsiz;
    } else if(strstr(rname,"yu")) {
      jmx = dsiz;
    } else if(strstr(rname,"zt")) {
      kmx = dsiz;
    }
  }

/*  get velocity components from velocity restart file  */

  ncstat = nc_open(uvFile, NC_NOWRITE, &ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot open %s\n", uvFile);
    exit(0);
  }

  ncstat = nc_inq(ncid, &ndims, &nvars, &natts, &xdimid);
  if (ncstat != NC_NOERR) {
    printf("Inquiry failed for %s\n", uvFile);
    exit(0);
  }
  for (n = 0; n < ndims; n++) {
    ncstat = nc_inq_dim(ncid, n, rname, &dsiz);
    if (ncstat != NC_NOERR) {
      printf("Cannot read dimension no. %d of %s\n", n, uvFile);
      exit(0);
    }
    if (strstr(rname,"xa") != NULL) {
      i = dsiz;
    } else if(strstr(rname,"ya") != NULL) {
      j = dsiz;
    } else if(strstr(rname,"za") != NULL) {
      k = dsiz;
    }
  }

  if (i != imx || j != jmx || k != kmx) {
    printf("%s and %s dimension mismatch\n", outFile, uvFile);
    printf("%s:  %d, %d, %d\n", outFile,imx,jmx,kmx);
    printf("%s:  %d, %d, %d\n", uvFile,i,j,k);
    exit(0);
  }

  strcpy(rname,"u");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  vsiz = imx*jmx*kmx;
  varo = (float*)realloc(varo,4*vsiz);
  vard = (double*)realloc(vard,8*vsiz);
  *start = 0;
  *(start+1) = 0;
  *(start+2) = 0;
  *(start+3) = 0;
  *count = 1;
  *(count+1) = kmx;
  *(count+2) = jmx;
  *(count+3) = imx;
  ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_inq_varid(ncid,rname,&vid);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, uvFile);
    exit(0);
  }
  ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, uvFile);
    exit(0);
  }
  for (n = 0; n < vsiz; n++) {
    if (*(varo+n) != mV) *(varo+n) = *(vard+n);
  }
  ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }

  strcpy(rname,"v");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  vsiz = imx*jmx*kmx;
  varo = (float*)realloc(varo,4*vsiz);
  vard = (double*)realloc(vard,8*vsiz);
  ncstat = nc_get_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_get_att_float(ncout,vido,"missing_value",&mV);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s missing_value attribute from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_inq_varid(ncid,rname,&vid);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, uvFile);
    exit(0);
  }
  ncstat = nc_get_vara_double(ncid,vid,start,count,vard);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s variable from %s\n", rname, uvFile);
    exit(0);
  }
  for (n = 0; n < vsiz; n++) {
    if (*(varo+n) != mV) *(varo+n) = *(vard+n);
  }
  ncstat = nc_put_vara_float(ncout,vido,start,count,varo);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }

  ncstat = nc_close(ncid);
  if (ncstat != NC_NOERR) {
    printf("Cannot close %s without error\n", uvFile);
    exit(0);
  }

/* Fill in time variables.   */

  strcpy(rname,"time");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&Tm);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }
  strcpy(rname,"average_T1");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&Tm);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }
  strcpy(rname,"average_T2");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&Tm);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }
  strcpy(rname,"average_DT");
  ncstat = nc_inq_varid(ncout, rname, &vido);
  if (ncstat != NC_NOERR) {
    printf("Cannot read %s id from %s\n", rname, outFile);
    exit(0);
  }
  ncstat = nc_put_vara_double(ncout,vido,start,count,&dZero);
  if (ncstat != NC_NOERR) {
    printf("Cannot rewrite %s variable to %s\n", rname, outFile);
    exit(0);
  }

  ncstat = nc_close(ncout);
  if (ncstat != NC_NOERR) {
    printf("Cannot close %s without error\n", outFile);
  }
}

/* ============================================================ */

unsigned int YearDay(date)
YMD date;
{
  int i;
  unsigned int yd;

  yd = 0;
  for (i = 0; i < date.month-1; i++)
    yd += dpm[i];
  yd += date.day;
  if (date.month > 2 && !(date.year%4) && ((date.year%100) || !(date.year%400)))    yd++;

  for (i = date.year0; i < date.year; i++)
    if (i%4)
      yd += 365;
    else
      if ((i%100) || !(i%400))
        yd += 366;
      else
        yd += 365;

  return(yd);
}

/* ============================================================ */

void usage()
{
  printf("Usage:\n");
  printf(" %s -t tsFile -c corFile -e eFile\n", prog);
  printf("   -t tsFile  - input TS restart netCDF file\n");
  printf("   -c corFile - input Cor restart netCDF file [optional]\n");
  printf("   -r rstFile - input Rstr restart netCDF file [optional]\n");
  printf("   -e eFile   - input Eta restart netCDF file\n");
  printf("   -u uvFile  - input UV restart netCDF file\n");
  printf("   -d dtFile  - input Date restart ASCII file\n");
  printf("   -o outTag  - tag for output files\n");
  printf("   -tp tpTag  - tag for template files\n");
}
