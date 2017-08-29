#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "netcdf.h"
#include "Tm.h"

int igx, jgx, kgx, ioff;

void openIfile(), getIfile(), closeIfile();
void initOfile(), putOfile(), closeOfile();
void usage();
int LeapYear();

int ncido, ndims, dimids[4], xdid, ydid, zdid, tdid;
int nvars, xvid, yvid, zvid, tvid, fvid;

int ncidi, ndimsi, dimidsi[4], xdidi, ydidi, zdidi, tdidi;
int nvarsi, xvidi, yvidi, zvidi, tvidi, mvidi, fvidi;
char fname[21], lname[21], unit[21];

int icx, jcx, kcx, ncx, nmx, mon, sdte, *ndy, ndays;
double *xc, *yc, *zc, *tme, *tmd;
float *c, *cm, *t;
float mV;

YMD *tm, dte, dtep;
int year, month, day, year0, lp;
int dpm[24] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
               31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
char *cmo[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

float dayf, ydayf;
float ydayr[14];
float *dt, *dtm, *dtp;
int *im, *ip;

char *source = "NODC WOA";
char *tcal = "JULIAN";
char *gridName = "MOM4 0.5 deg tripolar";
char tunits[31];

char iFile[51], oFile[51], prog[21];

main(argc,argv)
int argc;
char *argv[];
{
  int i, n, nn, nmx, jm, jp;
  float ddt;

  strcpy(prog,argv[0]);
  strcpy(iFile,"EMPTY");
  strcpy(oFile,"EMPTY");
  sdte = -1;
  ndays = -1;
  n = 1;
  while (n < argc) {
    if (!strcmp(argv[n],"-f")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("Error: for option -f a NetCDF must be given.\n");
        usage();
        exit(0);
      }
      strcpy(iFile,argv[n]);
    } else if (!strcmp(argv[n],"-o")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("Error: for option -o a NetCDF must be given.\n");
        usage();
        exit(0);
      }
      strcpy(oFile,argv[n]);
    } else if (!strcmp(argv[n],"-d")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("Error: for option -d a date must be given.\n");
        usage();
        exit(0);
      }
      sscanf(argv[n],"%d",&sdte);
    } else if (!strcmp(argv[n],"-n")) {
      n++;
      if (n >= argc || !strncmp(argv[n],"-",1)) {
        printf("Error: for option -n an integer must be given.\n");
        usage();
        exit(0);
      }
      sscanf(argv[n],"%d",&ndays);
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

  if (!strcmp(iFile,"EMPTY")) {
    printf("Error: An input file must be given.\n");
    usage();
    exit(0);
  }

  if (!strcmp(oFile,"EMPTY")) {
    printf("Error: An output file must be given.\n");
    usage();
    exit(0);
  }

  if (sdte < 0) {
    printf("Error: A start date for the run must be given.\n");
    usage();
    exit(0);
  }

  if (ndays < 0) {
    printf("Error: The number of days for the run must be given.\n");
    usage();
    exit(0);
  }

  year0=sdte/10000;
  lp = LeapYear(year0);
  dte.year=sdte/10000;
  dte.month=(sdte/100)%100;
  dte.day=sdte%100;
  dte.year0=year0;
  dte.yday=YearDay(dte) + ndays;
  if (lp && dte.yday >= 59) {
    ndays += 3;
  } else {
    ndays += 2;
  }

  tm = (YMD*)malloc(ndays*sizeof(YMD));
  tmd = (double*)malloc(ndays*sizeof(double));
  ndy = (int*)malloc(ndays*sizeof(int));

  tm->year=sdte/10000;
  tm->month=(sdte/100)%100;
  tm->day=sdte%100;
  tm->year0 = tm->year;
  tm->yday = YearDay(*tm);
  tm->yday -= 1;
  CalendarDay(tm);
  *ndy = tm->yday;

  *tmd = 0.0;
  for (n = 1; n < ndays; n++) {
    (tm+n)->yday = (tm+n-1)->yday + 1;
    (tm+n)->year0 = tm->year0;
    CalendarDay(tm+n);
    *(ndy+n) = (tm+n)->yday;
    *(tmd+n) = (double)n;
  }

  year = tm->year;
  month = tm->month;
  day = tm->day;
  if (day == 0) {
    if (month == 1) {
      year--;
      month = 12;
      day = 31;
    } else {
      printf("Error calculating date:%d-%2.2d-%2.2d\n", year, month, day);
      exit(0);
    }
  }

  sprintf(tunits,"days since %4.4d-%2.2d-%2.2d 00:00:00", year, month, day);
  
  if (lp) {
    nmx = 366;
  } else {
    nmx = 365;
  }
  for (n = 0; n < ndays; n++) {
    *(ndy+n) = *(ndy+n) > 0 ? *(ndy+n) : nmx;
    *(ndy+n) = *(ndy+n) <= nmx ? *(ndy+n) : *(ndy+n) - nmx;
  }

  dt = (float*)malloc(nmx*sizeof(float));
  dtm = (float*)malloc(nmx*sizeof(float));
  dtp = (float*)malloc(nmx*sizeof(float));
  im = (int*)malloc(nmx*sizeof(int));
  ip = (int*)malloc(nmx*sizeof(int));

  for (n = 0; n < 12; n++) {
    dte.year = year0;
    dte.month = n+1;
    dte.day = 1;
    dte.year0 = year0;

    dtep.year = year0;
    dtep.month = n+1;
    dtep.day = *(dpm+n+lp*12);
    dtep.year0 = year0;

    dte.yday = YearDay(dte);
    dtep.yday = YearDay(dtep);
    
    dayf = (float)(1 + dtep.yday - dte.yday)*0.5;
    dayf += (float)dte.day - 1.0;
    ydayf = (float)dte.yday + dayf - 1.0;
    *(ydayr+n+1) = ydayf;
  }
  *ydayr = -15.5;
  *(ydayr+13) = (float)nmx + 15.5;

  for (n = 0; n < nmx; n++) {
    dte.yday = n+1;
    CalendarDay(&dte);
    *(dt+n) = 0.5 + (float)n;
    for (i = 1; i < 14; i++) {
      if (*(dt+n) >= *(ydayr+i-1) && *(dt+n) < *(ydayr+i)) {
        *(ip+n) = i-1 < 12 ? i-1 : 0;
        *(im+n) = i-2 >= 0 ? i-2 : 11; 
        ddt = *(ydayr+i) - *(ydayr+i-1);
        *(dtm+n) = (*(dt+n) - *(ydayr+i-1))/ddt;
        *(dtp+n) = (*(ydayr+i) - *(dt+n))/ddt;
        jm = *(im+n);
        jp = *(ip+n);
        break;
      }
    }
  }

  openIfile();
  initOfile();

  for (n = 0; n < ndays; n++) {
    nn = *(ndy+n) - 1;
    jm = *(im+nn);
    jp = *(ip+nn);
    mon = jm+1;
    getIfile();
    for (i = 0; i < ncx; i++) {
      *(cm+i) = *(c+i);
    }
    mon = jp+1;
    getIfile();
    for (i = 0; i < ncx; i++) {
      if (*(c+i) != mV && *(cm+i) != mV) {
        *(t+i) = *(cm+i) * *(dtp+nn) + *(c+i) * *(dtm+nn);
      } else {
        *(t+i) = mV;
      }
    }

    day = n+1;
    putOfile();
  }

  closeIfile();
  closeOfile();
  
}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

int LeapYear(yr)
int yr;
{
  int lp;

  if (yr%4) {
    lp = 0;
  } else {
    if ((yr%100) || !(yr%400)) {
      lp = 1;
    } else {
      lp = 0;
    }
  }
  return(lp);
}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

void openIfile()
{
  int i, j, k, ks, n, lf, ydy, stat;
  size_t len, start[4], count[4];
  char *sp, str[41];

  stat = nc_open(iFile, NC_NOWRITE, &ncidi);
  if (stat != NC_NOERR) {
    printf("Cannot open %s\n", iFile);
    exit(0);
  }

  stat = nc_inq_ndims(ncidi, &ndimsi);
  if (stat != NC_NOERR) {
    printf("Cannot get number of dimensions\n");
    exit(0);
  }

  for (n = 0; n < ndimsi; n++) {
    stat = nc_inq_dim(ncidi, n, str, &len);
    if (stat != NC_NOERR) {
      printf("Cannot get dimensions\n");
      exit(0);
    }
    if (!strncmp(str,"xt_ocean",8)) {
      xdidi = n;
      icx = len;
    } else if (!strncmp(str,"yt_ocean",8)) {
      ydidi = n;
      jcx = len;
    } else if (!strncmp(str,"st_ocean",8)) {
      zdidi = n;
      kcx = len;
    } else if (!strncmp(str,"time",4)) {
      tdidi = n;
      nmx = len;
    }
  }

  xc = (double*)malloc(icx*sizeof(double));
  yc = (double*)malloc(jcx*sizeof(double));
  zc = (double*)malloc(kcx*sizeof(double));
  tme = (double*)malloc(nmx*sizeof(double));
  ncx = icx*jcx*kcx;
  c = (float*)malloc(ncx*sizeof(float));
  cm = (float*)malloc(ncx*sizeof(float));
  t = (float*)malloc(ncx*sizeof(float));

  stat = nc_inq_nvars(ncidi, &nvarsi);
  if (stat != NC_NOERR) {
    printf("Cannot get number of variables\n");
    exit(0);
  }

  for (n = 0; n < nvarsi; n++) {
    stat = nc_inq_varname(ncidi, n, str);
    if (stat != NC_NOERR) {
      printf("Cannot get variables\n");
      exit(0);
    }
    if (!strncmp(str,"xt_ocean",8)) {
      xvidi = n;
    } else if (!strncmp(str,"yt_ocean",8)) {
      yvidi = n;
    } else if (!strncmp(str,"st_ocean",8)) {
      zvidi = n;
    } else if (!strncmp(str,"time",4)) {
      tvidi = n;
    } else if (!strncmp(str,"temp",4)) {
      fvidi = n;
      strcpy(fname,"temp");
    } else if (!strncmp(str,"salt",4)) {
      fvidi = n;
      strcpy(fname,"salt");
    }
  }

  stat = nc_get_att_float(ncidi, fvidi, "missing_value", &mV);
  if (stat != NC_NOERR) {
    printf("Cannot get missing_value attribute for %s\n", fname);
    exit(0);
  }
  stat = nc_get_att_text(ncidi, fvidi, "long_name", lname);
  if (stat != NC_NOERR) {
    printf("Cannot get long_name attribute for %s\n", fname);
    exit(0);
  }
  stat = nc_get_att_text(ncidi, fvidi, "units", unit);
  if (stat != NC_NOERR) {
    printf("Cannot get units attribute for %s\n", fname);
    exit(0);
  }

  stat = nc_get_var_double(ncidi, xvidi, xc);
  if (stat != NC_NOERR) {
    printf("Cannot get variable %s\n", "xt_ocean");
    exit(0);
  }
  stat = nc_get_var_double(ncidi, yvidi, yc);
  if (stat != NC_NOERR) {
    printf("Cannot get variable %s\n", "yt_ocean");
    exit(0);
  }
  stat = nc_get_var_double(ncidi, zvidi, zc);
  if (stat != NC_NOERR) {
    printf("Cannot get variable %s\n", "st_ocean");
    exit(0);
  }
  stat = nc_get_var_double(ncidi, tvidi, tme);
  if (stat != NC_NOERR) {
    printf("Cannot get variable %s\n", "time");
    exit(0);
  }
}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

void getIfile()
{
  int n, stat;
  size_t len, start[4], count[4];

  if (mon > nmx) {
    printf("Record %d greater than maximum: %d\n", mon, nmx);
    exit(0);
  }

  *start = mon - 1;
  *count = 1;
  *(start+1) = 0;
  *(count+1) = kcx;
  *(start+2) = 0;
  *(count+2) = jcx;
  *(start+3) = 0;
  *(count+3) = icx;
  stat = nc_get_vara_float(ncidi, fvidi, start, count, c);
  if (stat != NC_NOERR) {
    printf("Cannot get variable %s\n", fname);
    exit(0);
  }
}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

void closeIfile()
{
  int i, stat;

  stat = nc_close(ncidi);
  if (stat != NC_NOERR) {
    printf("Error closing %s\n", iFile);
  }

}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

void initOfile()
{
  int i, ii, n, stat;
  size_t len;
  char str[41];

/* define dimensions */

  stat = nc_create(oFile, NC_NOCLOBBER, &ncido);
  if (stat != NC_NOERR) {
    printf("Cannot open %s\n", oFile);
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  len = icx;
  stat = nc_def_dim(ncido, "xt_ocean", len, &xdid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = jcx;
  stat = nc_def_dim(ncido, "yt_ocean", len, &ydid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = kcx;
  stat = nc_def_dim(ncido, "st_ocean", len, &zdid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = ndays;
  stat = nc_def_dim(ncido, "time", NC_UNLIMITED, &tdid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

/* define variables */

  ndims = 1;
  stat = nc_def_var(ncido, "xt_ocean", NC_DOUBLE, ndims, &xdid, &xvid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  stat = nc_def_var(ncido, "yt_ocean", NC_DOUBLE, ndims, &ydid, &yvid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  stat = nc_def_var(ncido, "st_ocean", NC_DOUBLE, ndims, &zdid, &zvid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  stat = nc_def_var(ncido, "time", NC_DOUBLE, ndims, &tdid, &tvid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  ndims = 4;
  *dimids = tdid;
  *(dimids+1) = zdid;
  *(dimids+2) = ydid;
  *(dimids+3) = xdid;
  stat = nc_def_var(ncido, fname, NC_FLOAT, ndims, dimids, &fvid);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

/* add attributes */

  len = strlen("tcell longitude");
  stat = nc_put_att_text(ncido, xvid, "long_name", len, "tcell longitude");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen("degrees_E");
  stat = nc_put_att_text(ncido, xvid, "units", len, "degrees_E");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen("X");
  stat = nc_put_att_text(ncido, xvid, "cartesian_axis", len, "X");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  len = strlen("tcell latitude");
  stat = nc_put_att_text(ncido, yvid, "long_name", len, "tcell latitude");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen("degrees_N");
  stat = nc_put_att_text(ncido, yvid, "units", len, "degrees_N");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen("Y");
  stat = nc_put_att_text(ncido, yvid, "cartesian_axis", len, "Y");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  len = strlen("tcell depth");
  stat = nc_put_att_text(ncido, zvid, "long_name", len, "tcell depth");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen("meters");
  stat = nc_put_att_text(ncido, zvid, "units", len, "meters");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen("Z");
  stat = nc_put_att_text(ncido, zvid, "cartesian_axis", len, "Z");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  len = strlen("time");
  stat = nc_put_att_text(ncido, tvid, "long_name", len, "time");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen(tunits);
  stat = nc_put_att_text(ncido, tvid, "units", len, tunits);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen("T");
  stat = nc_put_att_text(ncido, tvid, "cartesian_axis", len, "T");
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen(tcal);
  stat = nc_put_att_text(ncido, tvid, "calendar", len, tcal);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  len = strlen(lname);
  stat = nc_put_att_text(ncido, fvid, "long_name", len, lname);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = strlen(unit);
  stat = nc_put_att_text(ncido, fvid, "units", len, unit);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  len = 1;
  stat = nc_put_att_float(ncido, fvid, "missing_value", NC_FLOAT, len, &mV);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  len = strlen(source);
  stat = nc_put_att_text(ncido, NC_GLOBAL, "source", len, source);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  len = strlen(gridName);
  stat = nc_put_att_text(ncido, NC_GLOBAL, "grid", len, gridName);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  stat = nc_enddef(ncido);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }


/* fill variables */

  stat = nc_put_var_double(ncido, xvid, xc);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  stat = nc_put_var_double(ncido, yvid, yc);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
  stat = nc_put_var_double(ncido, zvid, zc);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

void putOfile()
{
  int n, stat;
  size_t len, start[4], count[4];

  n = day-1;
  *start = n;
  *count = 1;

  stat = nc_put_vara_double(ncido, tvid, start, count, tmd+n);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }

  *start = day-1;
  *count = 1;
  *(start+1) = 0;
  *(count+1) = kcx;
  *(start+2) = 0;
  *(count+2) = jcx;
  *(start+3) = 0;
  *(count+3) = icx;

  stat = nc_put_vara_float(ncido, fvid, start, count, t);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

void closeOfile()
{
  int stat;

  stat = nc_close(ncido);
  if (stat != NC_NOERR) {
    printf(" %s\n", nc_strerror(stat));
    exit(0);
  }
}

/* mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm */

void usage()
{
  printf("Usage:\n");
  printf(" %s -f iFile [options]\n", prog);
  printf("   -f iFile - netcdf temperature climatology\n");
  printf("   -d date  - start date for model run (yyyymmdd)\n");
  printf("   -n num   - number of days in the run\n");
  printf("   -o oFile - netcdf output file\n");
}
