/*
 * furthest-point code
 * Matthew Skala <mska@itu.dk>
 */

/*
 * Note this is ONLY meant to work with the "vectors" space from the SISAP
 * library, and only a modified version of that in which the symbol "DB" has
 * been made extern.  That is because of the need to access coordinates of
 * vectors, which are abstracted out in the standard database interface.
 */

/**********************************************************************/

/* external stuff */

/* boilerplate includes */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <stdlib.h>
#include <math.h>
#include "../../obj.h"
#include "../../index.h"
#include "../../basics.h"
#include <time.h>
#include <sys/times.h>
#include <unistd.h>

/* interface we must implement */
Index build (char *dbname, int n, int *argc, char ***argv);
void freeIndex (Index S, bool closedb);
void saveIndex (Index S, char *fname);
Index loadIndex (char *fname);
int search (Index S, Obj obj, Tdist r, bool show);
Tdist searchNN (Index S, Obj obj, int k, bool show);

/* data structure from the space implementation */
typedef struct sEuclDB
   { float *nums;  /* coords all together */
     int nnums;	  /* number of vectors (with space for one more) */
     int coords;  /* coordinates */
     float (*df) (float *p1, float *p2, int k); /* distance to use */
   } EuclDB;
extern EuclDB DB;
#define db(p) (DB.nums + DB.coords*(int)(p))

/**********************************************************************/

/* index data structure */

#define MAGIC 36553368

typedef struct _RTA {
   int rank,times_achieved;
} RTA;

typedef struct _PDIST_PAIR {
   Obj obj;
   union {
      float pdist;
      RTA rta;
   } u;
} PDIST_PAIR;

typedef struct _FURTHEST_INDEX {
   int magic;
   int num_dimensions,num_points,num_projections,algorithm;
   char *dbname;
   PDIST_PAIR *pdp;
   float *projections;
} FURTHEST_INDEX;

/**********************************************************************/

/* Ziggurat Method standard normal pseudorandom number generator code
 * from George Marsaglia and Wai Wan Tsang (2000).
 * "The Ziggurat Method for Generating Random Variables". Journal of
 * Statistical Software 5 (8).
 */

static unsigned long jz,jsr=123456789;

#define SHR3 (jz=jsr, jsr^=(jsr<<13), jsr^=(jsr>>17), jsr^=(jsr<<5),jz+jsr)
#define UNI (.5 + (signed) SHR3*.2328306e-9)
#define IUNI SHR3

static long hz;
static unsigned long iz, kn[128], ke[256];
static float wn[128],fn[128], we[256],fe[256];

#define RNOR (hz=SHR3, iz=hz&127, (fabs(hz)<kn[iz])? hz*wn[iz] : nfix())
#define REXP (jz=SHR3, iz=jz&255, (    jz <ke[iz])? jz*we[iz] : efix())

/* nfix() generates variates from the residue when rejection in RNOR occurs. */

float nfix(void) {
   const float r=3.442620;     /* The start of the right tail */
   static float x,y;

   while (1) {
      x=hz*wn[iz];      /* iz==0, handles the base strip */
      if (iz==0) {
	 do {
	    x=-log(UNI)*0.2904764;
	    y=-log(UNI);
	 } while (y+y<x*x);
	 return (hz>0)?r+x:-r-x;
      }
      /* iz>0, handle the wedges of other strips */
      if (fn[iz]+UNI*(fn[iz-1]-fn[iz])<exp(-0.5*x*x))
	return x;
      
     /* initiate, try to exit loop*/
      hz=SHR3;
      iz=hz&127;
      if (fabs(hz)<kn[iz])
	return hz*wn[iz];
   }  
}

/* efix() generates variates from the residue when rejection in REXP occurs. */
float efix(void) {
   float x;
   
   while (1) {
      if (iz==0)
	return 7.69711-log(UNI);          /* iz==0 */
      x=jz*we[iz];
      if (fe[iz]+UNI*(fe[iz-1]-fe[iz])<exp(-x))
	return x;
      
      /* initiate, try to exit loop */
      jz=SHR3;
      iz=(jz&255);
      if(jz<ke[iz])
	return jz*we[iz];
   }
}

/*--------This procedure sets the seed and creates the tables------*/

void zigset(unsigned long jsrseed) {
   const double m1 = 2147483648.0, m2 = 4294967296.0;
   double dn=3.442619855899,tn=dn,vn=9.91256303526217e-3, q;
   double de=7.697117470131487, te=de, ve=3.949659822581572e-3;
   int i;

   jsr^=jsrseed;
   
   /* Set up tables for RNOR */
   q=vn/exp(-.5*dn*dn);
   kn[0]=(dn/q)*m1;
   kn[1]=0;
   
   wn[0]=q/m1;
   wn[127]=dn/m1;
   
   fn[0]=1.;
   fn[127]=exp(-.5*dn*dn);
   
   for (i=126;i>=1;i--) {
      dn=sqrt(-2.*log(vn/dn+exp(-.5*dn*dn)));
      kn[i+1]=(dn/tn)*m1;
      tn=dn;
      fn[i]=exp(-.5*dn*dn);
      wn[i]=dn/m1;
    }
   
/* Set up tables for REXP */
   q=ve/exp(-de);
   ke[0]=(de/q)*m2;
   ke[1]=0;
   
   we[0]=q/m2;
   we[255]=de/m2;
   
   fe[0]=1.;
   fe[255]=exp(-de);
   
   for (i=254;i>=1;i--) {
      de=-log(ve/de+exp(-de));
      ke[i+1]= (de/te)*m2;
      te=de;
      fe[i]=exp(-de);
      we[i]=de/m2;
   }
}

/**********************************************************************/

static int pdist_compar(const void *xv,const void *yv) {
   float x,y;
   
   x=((PDIST_PAIR *)xv)->u.pdist;
   y=((PDIST_PAIR *)yv)->u.pdist;
   
   if (x<y)
     return -1;
   if (x>y)
     return 1;
   return 0;
}

static int inv_pdist_compar(const void *xv,const void *yv) {
   float x,y;
   
   x=((PDIST_PAIR *)xv)->u.pdist;
   y=((PDIST_PAIR *)yv)->u.pdist;
   
   if (x<y)
     return 1;
   if (x>y)
     return -1;
   return 0;
}

static int rta_compar(const void *xv,const void *yv) {
   int x,y;
   
   x=((PDIST_PAIR *)xv)->u.rta.rank;
   y=((PDIST_PAIR *)yv)->u.rta.rank;
   
   if (x<y)
     return -1;
   if (x>y)
     return 1;

   x=((PDIST_PAIR *)xv)->u.rta.times_achieved;
   y=((PDIST_PAIR *)yv)->u.rta.times_achieved;
   
   if (x<y)
     return 1;
   if (x>y)
     return -1;

   return 0;
}

static char *algoname[3]={"by value","by rank","query dependent"};

Index build(char *dbname,int n,int *argc,char ***argv) {
   FURTHEST_INDEX *rval;
   int i,j,k;
   float x;

   /* allocate memory and set up data structure */

   rval=(FURTHEST_INDEX *)malloc(sizeof(FURTHEST_INDEX));
   rval->magic=MAGIC;
   rval->num_dimensions=DB.coords;
   rval->num_points=n;

   if (*argc<1) {
      fprintf(stderr,"usage:  <standard params> <l> [algorithm]\n");
      exit(1);
   }
   rval->num_projections=atoi((*argv)[0]);
   if (rval->num_projections<1) {
      fprintf(stderr,"bad number of projections %d\n",rval->num_projections);
   }
   if (*argc>1) {
      if (strcmp((*argv)[1],"by_rank")==0)
	rval->algorithm=1;
      else if (strcmp((*argv)[1],"query")==0)
	rval->algorithm=2;
      else {
	 fprintf(stderr,"unknown command-line parameter");
	 exit(1);
      }
   } else
     rval->algorithm=0;
   
   rval->dbname=(char *)malloc(strlen(dbname)+1);
   strcpy(rval->dbname,dbname);
   
   rval->pdp=(PDIST_PAIR *)malloc(sizeof(PDIST_PAIR)*(rval->num_projections+1)
				  *rval->num_points);
   
   printf("Generating index for %d points, %d dimensions, "
	  "%d projections, %s.\n",
	  rval->num_points,rval->num_dimensions,rval->num_projections,
	  algoname[rval->algorithm]);
   
   /* generate random projection directions */
   /* TODO allow adjustable seed */

   zigset(MAGIC+time(NULL)+170*getpid());
   rval->projections=(float *)malloc(sizeof(float)*rval->num_projections
				     *rval->num_dimensions);
   for (i=0;i<rval->num_projections*rval->num_dimensions;i++)
     rval->projections[i]=RNOR/sqrt((float)rval->num_dimensions);
   
   /* project all the points */
   for (i=0;i<rval->num_projections;i++)
     for (j=0;j<rval->num_points;j++) {
	x=0.0;
	for (k=0;k<rval->num_dimensions;k++)
	  x+=(rval->projections[i*rval->num_dimensions+k]*(db(j+1)[k]));
	rval->pdp[(i+1)*rval->num_points+j].obj=j+1;
	rval->pdp[(i+1)*rval->num_points+j].u.pdist=x;
     }
   
   /* compute master ranks */
   if (rval->algorithm==1) {
      
      /* based on ranks and times achieved */
      
      /* sort within each projection */
      for (i=1;i<=rval->num_projections;i++)
	qsort(rval->pdp+i*rval->num_points,rval->num_points,
	      sizeof(PDIST_PAIR),pdist_compar);
      
      /* assign rank numbers */
      for (i=1;i<=rval->num_projections;i++)
	for (j=0;j<rval->num_points;j++)
	  rval->pdp[i*rval->num_points+j].u.rta.rank=j;
      
      /* find most extreme rank number for each point */
      for (j=0;j<rval->num_points;j++) {
	 rval->pdp[j].obj=j+1;
	 rval->pdp[j].u.rta.rank=rval->num_points;
	 rval->pdp[j].u.rta.times_achieved=0;
      }
      for (i=1;i<=rval->num_projections;i++)
	for (j=0;j<rval->num_points;j++) {
	   if (rval->pdp[i*rval->num_points+j].u.rta.rank<
	       rval->pdp[rval->pdp[i*rval->num_points+j].obj-1].u.rta.rank) {
	      rval->pdp[rval->pdp[i*rval->num_points+j].obj-1].u.rta.rank=
		rval->pdp[i*rval->num_points+j].u.rta.rank;
	      rval->pdp[rval->pdp[i*rval->num_points+j].obj-1].u.rta.times_achieved=1;
	   } else if (rval->pdp[i*rval->num_points+j].u.rta.rank==
		      rval->pdp[rval->pdp[i*rval->num_points+j].obj-1].u.rta.rank)
		rval->pdp[rval->pdp[i*rval->num_points+j].obj-1].u.rta.times_achieved++;
	}

      /* sort on those */
      qsort(rval->pdp,rval->num_points,sizeof(PDIST_PAIR),rta_compar);

   } else {
      
      /* based on projected value */

      /* sort within each projection (extra) */
      for (i=1;i<=rval->num_projections;i++) {
	 qsort(rval->pdp+i*rval->num_points,rval->num_points,
	       sizeof(PDIST_PAIR),pdist_compar);
      }
      
      /* find most extreme projected value for each point */
      for (j=0;j<rval->num_points;j++) {
	 rval->pdp[j].obj=j+1;
	 rval->pdp[j].u.pdist=1.0e38;
      }
      for (i=1;i<=rval->num_projections;i++)
	for (j=0;j<rval->num_points;j++)
	  if (rval->pdp[i*rval->num_points+j].u.pdist<
	      rval->pdp[rval->pdp[i*rval->num_points+j].obj-1].u.pdist) {
	     rval->pdp[rval->pdp[i*rval->num_points+j].obj-1].u.pdist=
	       rval->pdp[i*rval->num_points+j].u.pdist;
	  }
      
      /* sort on those */
      qsort(rval->pdp,rval->num_points,sizeof(PDIST_PAIR),pdist_compar);
   }
   
   return rval;
}

void freeIndex(Index S,bool closedb) {
   FURTHEST_INDEX *fi=(FURTHEST_INDEX *)S;
   
   if (fi==NULL) {
      fprintf(stderr,"Attempting to free a null pointer as an index.\n");
      exit(1);
   }
   if (fi->magic!=MAGIC) {
      fprintf(stderr,"Attempting to free an index with bad magic number.\n");
      exit(1);
   }
   
   free(fi->dbname);
   free(fi->pdp);
   free(fi->projections);
   free(fi);
   
   if (closedb)
     closeDB();
}

void saveIndex(Index S,char *fname) {
   FURTHEST_INDEX *fi=(FURTHEST_INDEX *)S;
   FILE *ifile;
   
   if (fi==NULL) {
      fprintf(stderr,"Attempting to save a null pointer as an index.\n");
      exit(1);
   }
   if (fi->magic!=MAGIC) {
      fprintf(stderr,"Attempting to save an index with bad magic number.\n");
      exit(1);
   }
   
   ifile=fopen(fname,"wb");
   if (ifile==NULL) {
      fprintf(stderr,"Error opening index file.\n");
      exit(1);
   }
   
   if (fwrite(fi,sizeof(int),5,ifile)!=5) {
      fprintf(stderr,"Error writing index header.\n");
      exit(1);
   }
   
   if (fprintf(ifile,"%.200s\n",fi->dbname)<0) {
      fprintf(stderr,"Error writing database name.\n");
      exit(1);
   }

   if (fwrite(fi->pdp,sizeof(PDIST_PAIR),
	      (fi->num_projections+1)*fi->num_points,
	      ifile)
       !=(fi->num_projections+1)*fi->num_points) {
      fprintf(stderr,"Error writing permutations.\n");
      exit(1);
   }

   if (fwrite(fi->projections,sizeof(float),
	      fi->num_projections*fi->num_dimensions,ifile)
       !=fi->num_projections*fi->num_dimensions) {
      fprintf(stderr,"Error writing projection vectors.\n");
      exit(1);
   }
   
   fclose(ifile);
}

extern int errno;

Index loadIndex(char *fname) {
   FURTHEST_INDEX *fi;
   FILE *ifile;
   
   fi=(FURTHEST_INDEX *)malloc(sizeof(FURTHEST_INDEX));
   
   ifile=fopen(fname,"rb");
   if (ifile==NULL) {
      fprintf(stderr,"Error opening index file.\n");
      exit(1);
   }
   
   if (fread(fi,sizeof(int),5,ifile)!=5) {
      fprintf(stderr,"Error reading index header.\n");
      exit(1);
   }
   if (fi->magic!=MAGIC) {
      fprintf(stderr,"Bad magic number in index file.\n");
      exit(1);
   }
   
   fprintf(stderr,
	   "Reading index for %d points, %d dimensions, "
	   "%d projections, %s.\n",
	   fi->num_points,fi->num_dimensions,fi->num_projections,
	   algoname[fi->algorithm]);
   
   fi->dbname=(char *)malloc(201);
   if (fgets(fi->dbname,201,ifile)<0) {
      fprintf(stderr,"Error reading database name.\n");
      exit(1);
   }
   fi->dbname[strlen(fi->dbname)-1]='\0';
   
   fi->pdp=(PDIST_PAIR *)malloc(sizeof(PDIST_PAIR)*
				(fi->num_projections+1)*fi->num_points);
   if (fread(fi->pdp,sizeof(PDIST_PAIR),
	     (fi->num_projections+1)*fi->num_points,
	     ifile)
       !=(fi->num_projections+1)*fi->num_points) {
      fprintf(stderr,"Error %d reading permutations.\n",errno);
      exit(1);
   }

   fi->projections=(float *)malloc(sizeof(float)*
				   fi->num_projections*fi->num_dimensions);
   if (fread(fi->projections,sizeof(float),
	     fi->num_projections*fi->num_dimensions,ifile)
       !=fi->num_projections*fi->num_dimensions) {
      fprintf(stderr,"Error %d reading projection vectors.\n",errno);
      exit(1);
   }
   
   fclose(ifile);

   openDB(fi->dbname);
   
   return (Index)fi;
}

Tdist searchNN(Index S,Obj obj,int k,bool show) {
   FURTHEST_INDEX *fi=(FURTHEST_INDEX *)S;
   int i,j,n;
   Obj *found,tmp_obj,new_obj;
   Tdist rval,*found_dist,tmp_dist,new_dist;
   
   if (fi==NULL) {
      fprintf(stderr,"Attempting to search a null pointer.\n");
      exit(1);
   }
   if (fi->magic!=MAGIC) {
      fprintf(stderr,"Attempting to search an index with bad magic number.\n");
      exit(1);
   }
   
   found=(Obj *)malloc(sizeof(Obj)*k);
   found_dist=(Tdist *)malloc(sizeof(Tdist)*k);

   n=0;
   for (i=0;i<fi->num_points;i++) {
      
      new_obj=fi->pdp[i].obj;
      new_dist=distance(obj,new_obj);

      /* fprintf(stderr,"distance %d to %d is %f\n",obj,new_obj,new_dist); */

      for (j=0;j<n;j++)
	if (new_dist>found_dist[j]) {
	   tmp_dist=new_dist;
	   new_dist=found_dist[j];
	   found_dist[j]=tmp_dist;
	   
	   tmp_obj=new_obj;
	   new_obj=found[j];
	   found[j]=tmp_obj;
	}
      
      if (n<k) {
	 found[n]=new_obj;
	 found_dist[n]=new_dist;
	 n++;
      }
   }
   
   if (n<=0) {
      fprintf(stderr,"Cannot find any points in database??\n");
      exit(1);
   }
   
   if (show)
     for (i=0;i<n;i++)
       printobj(found[i]);
   
   rval=found_dist[0];
   free(found);
   free(found_dist);
/*    fprintf(stderr,"returning %f\n",rval); */
   return rval;
}

int search(Index S,Obj obj,Tdist r,bool show) {
   FURTHEST_INDEX *fi=(FURTHEST_INDEX *)S;
   int i,j,n;
   Tdist target_dist;
   float approx,best_approx;
   int *next_to_try,found_next,found_in_proj;
   float x,*projected_query,y,z;
   
   if (fi==NULL) {
      fprintf(stderr,"Attempting to search a null pointer.\n");
      exit(1);
   }
   if (fi->magic!=MAGIC) {
      fprintf(stderr,"Attempting to search an index with bad magic number.\n");
      exit(1);
   }
   
   target_dist=searchNN(S,obj,1,0);
   
   best_approx=0;
   n=0;

   if (fi->algorithm==2) {
      
      /* QUERY DEPENDENT BY PROJECTED VALUE */
      
      next_to_try=(int *)malloc(fi->num_projections*sizeof(int));
      projected_query=(float *)malloc(fi->num_projections*sizeof(float));
      
      for (i=0;i<fi->num_projections;i++) {
	 next_to_try[i]=0;
	 x=0.0;
	 for (j=0;j<fi->num_dimensions;j++)
	   x+=(fi->projections[i*fi->num_dimensions+j]*(db(obj)[j]));
	 projected_query[i]=x;
      }
      
      for (i=0;i<r;i++) {
	 found_next=fi->pdp[fi->num_points+next_to_try[0]].obj;
	 y=fabs(fi->pdp[fi->num_points+next_to_try[0]].u.pdist
		-projected_query[0]);
	 found_in_proj=0;
	 
	 for (j=1;j<fi->num_projections;j++) {
	    z=fabs(fi->pdp[fi->num_points*(j+1)+next_to_try[j]].u.pdist
		   -projected_query[j]);
	    if (z>y) {
	       y=z;
	       found_next=fi->pdp[fi->num_points*(j+1)+
				  next_to_try[j]].obj;
	       found_in_proj=j;
	    }
	 }
	 
	 next_to_try[found_in_proj]++;
	 
	 if (distance(obj,found_next)>=target_dist) {
	    if (show)
	      printobj(found_next);
	    n=1;
	 }
	 approx=((float)distance(obj,found_next))/(float)target_dist;
	 if (approx>best_approx)
	   best_approx=approx;
      }
      
      free(projected_query);
      free(next_to_try);
      
   } else {
      
      /* QUERY INDEPENDENT BY RANK OR PROJECTED VALUE */
      
      for (i=0;i<r;i++) {
	 if (distance(obj,fi->pdp[i].obj)>=target_dist) {
	    if (show)
	      printobj(fi->pdp[i].obj);
	    n=1;
	 }
	 approx=((float)distance(obj,fi->pdp[i].obj))/(float)target_dist;
	 if (approx>best_approx)
	   best_approx=approx;
      }
   }
   
   printf("FURTHESTDATA %f %d\n",1.0/best_approx,n);   
   return 0;
}
