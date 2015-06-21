#ifdef SPEC_CPU
# define THORN_IS_Cactus
#endif /* SPEC_CPU */
 /*@@
   @file      Stagger.c
   @date      Thu Jan 27 15:32:28 2000
   @author    Gerd Lanfermann
   @desc 
   Stuff to deal with staggering.
   @enddesc 
   @version $Header: /cactus/Cactus/src/main/Stagger.c,v 1.23 2001/11/07 00:19:15 goodale Exp $
 @@*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cctk_Flesh.h"
#include "cctk_FortranString.h"
#include "cctk_Groups.h"
#include "cctk_Types.h"
#include "cctk_WarnLevel.h"

#include "cctk_Stagger.h"
#include "cctki_Stagger.h"

static const char *rcsid = "$Header: /cactus/Cactus/src/main/Stagger.c,v 1.23 2001/11/07 00:19:15 goodale Exp $";

CCTK_FILEVERSION(main_Stagger_c)

/********************************************************************
 *********************     Fortran Wrappers   ***********************
 ********************************************************************/
void CCTK_FCALL cctk_groupstaggerindexgi_
                           (int *stagcode, int *gindex);
void CCTK_FCALL cctk_groupstaggerindexgn_
                           (int *scode, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_staggerindex_
                           (int *scode, ONE_FORTSTRING_ARG);
void CCTK_FCALL cctk_staggerdirindex_
                           (int *dsi, int *dir, int *gsi);
void CCTK_FCALL cctk_groupstaggerdirarray_
                           (int *ierr, int *dindex, int *dim, int *gsc);
void CCTK_FCALL cctk_groupstaggerdirarraygi_
                           (int *ierr, int *dindex, int *dim, int *gi);
void CCTK_FCALL cctk_staggerdirname_
                           (int *dsc, int *dir, ONE_FORTSTRING_ARG);

/********************************************************************
 *********************     External Routines   **********************
 ********************************************************************/


 /*@@
   @routine    CCTK_GroupStaggerIndexGI
   @date       
   @author     
   @desc       retuns the stagger code for a given group index

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/

int CCTK_GroupStaggerIndexGI(int gindex)
{
  cGroup group;
  int sc;
  CCTK_GroupData(gindex, &group);
  sc = group.stagtype;
  return(sc);
}

void CCTK_FCALL cctk_groupstaggerindexgi_
                           (int *stagcode, int *gindex) 
{
  *stagcode = CCTK_GroupStaggerIndexGI(*gindex);
}


 /*@@
   @routine    CCTK_GroupStaggerIndexGN
   @date       
   @author     Gerd Lanfermann
   @desc       returns the stagger index for a given group name
   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
 
int CCTK_GroupStaggerIndexGN(const char *gname) 
{
  int gindex;
  gindex = CCTK_GroupIndex(gname);
  return(CCTK_GroupStaggerIndexGI(gindex));
}

void CCTK_FCALL cctk_groupstaggerindexgn_
                           (int *scode, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(gname)
  int gindex;
  gindex = CCTK_GroupIndex(gname);
  *scode = CCTK_GroupStaggerIndexGI(gindex);
  free(gname);
}
  

 /*@@
   @routine    CCTK_StaggerIndex
   @date       
   @author     Gerd Lanfermann
   @desc       returns the stagger index for a given stagger name

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
 
int CCTK_StaggerIndex(const char *stype) 
{
  int i,scode,base,dim,m;

  base = 1;
  scode= 0;
  dim  = strlen(stype);

  for (i=0;i<dim;i++) 
  {

    switch (toupper(stype[i]))
    {
      case 'M':m=0; break;
      case 'C':m=1; break;
      case 'P':m=2; break;
      default:
        CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
		   "CCTK_StaggerIndex: Unknown stagger type %s",stype);
        return(-1);
    }
    scode+= m*base;
    base  = CCTK_NUM_STAGGER * base;
  }
  return(scode);
}

void CCTK_FCALL cctk_staggerindex_
                           (int *scode, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE(sname);
  *scode = CCTK_StaggerIndex(sname);
  free(sname);
}


/*@@
   @routine    CCTK_StaggerDirIndex
   @date       
   @author     Gerd Lanfermann
   @desc       returns the stagger index in a direction <dir>
               when given the staggerindex <sc>.

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
 
int CCTK_StaggerDirIndex(int dir, int si) 
{
  int val,b,dsi=0;
  static int hash[4],hashed=0;

  if (hashed==0) 
  {
    hash[0]= 1;
    hash[1]= 3;
    hash[2]= 9;
    hash[3]=27;
    hashed = 1;
  }

  for (b=CCTK_NSTAG;b>=0;b--) 
  {
    val = (int)(si / hash[b]);
    si  = si % hash[b];
    if (dir==b) 
    {
      dsi = val;
      break;
    }
  }
  return(dsi);
}



void CCTK_FCALL cctk_staggerdirindex_
                           (int *dsi, int *dir, int *gsi) 
{
  /* accept fortran indexing [1..]: decrease the directional index
     for the call to the C routine.  */
  *dsi  = CCTK_StaggerDirIndex((*dir)-1, *gsi);
} 



/*@@
   @routine    CCTK_StaggerDirIndexArray
   @date       
   @author     Gerd Lanfermann
   @desc       returns the stagger index for all direction in 
               an array <dindex> of size <dim> when given the staggerindex <sc>.

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
 
int CCTK_StaggerDirArray(int *dindex , int dim, int sindex) 
{
  int val,b;
  static int hash[4],hashed=0;

  if (hashed==0) 
  {
    hash[0]= 1;
    hash[1]= 3;
    hash[2]= 9;
    hash[3]=27;
    hashed = 1;
  }

  if (dim>4) 
  {
    CCTK_VWarn(1,__LINE__,__FILE__,"Cactus", 
	      "CCTK_StaggerDirArray: Dimension %d exceeds maximum of 4",dim);
    return(-1);
  }

  for (b=CCTK_NSTAG;b>=0;b--) 
  {
    val       = (int)(sindex / hash[b]);
    sindex    = sindex % hash[b];
    if (b<dim) dindex[b] = val;
  }
  return(0);
}


void CCTK_FCALL cctk_groupstaggerdirarray_
                           (int *ierr, int *dindex, int *dim, int *gsc) 
{
  /* accept fortran indexing [1..]: decrease the directional index
     for the call to the C routine.  */
  *ierr = CCTK_StaggerDirArray(dindex, *dim, *gsc);
} 

 /*@@
   @routine    CCTK_GroupStaggerDirArrayGI
   @date       
   @author     Gerd Lanfermann
   @desc       returns the stagger index for all direction in 
               an array <dindex> of size <dim> when given the group
               index <gi>

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
 
int CCTK_GroupStaggerDirArrayGI(int *dindex, int dim, int gi) 
{
  int si,ierr;
  si  = CCTK_GroupStaggerIndexGI(gi);
  ierr= CCTK_StaggerDirArray(dindex, dim, si);
  return ierr;
}

void CCTK_FCALL cctk_groupstaggerdirarraygi_
                           (int *ierr, int *dindex, int *dim, int *gi) 
{
  *ierr = CCTK_GroupStaggerDirArrayGI(dindex, *dim, *gi);
} 


 /*@@
   @routine    CCTK_StaggerDirName
   @date       
   @author     Gerd Lanfermann
   @desc       returns the directional staggering in direction <dir> 
               for a given stagger name <stype>.

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
 
int CCTK_StaggerDirName(int dir, const char *stype) 
{
  int scode;
  char hs[7]="MMMMMM";

  sprintf(hs,"%s",stype);

  if (dir> (int) strlen(hs)) 
  {
    CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
	      "CCTK_StaggerDirName: Stagger name too short for direction %d",
	      dir);
  }

  switch (toupper(hs[dir]))
  {
    case 'M': scode = 0; break;
    case 'C': scode = 1; break;
    case 'P': scode = 2; break;
    default:
      CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
		"CCTK_StaggerDirName: Unknown stagger type %s",hs);
      return(-1);
  }
  return(scode);
}

void CCTK_FCALL cctk_staggerdirname_
                           (int *dsc, int *dir, ONE_FORTSTRING_ARG) 
{
  ONE_FORTSTRING_CREATE(sname);

  *dsc = CCTK_StaggerDirName((*dir)-1,sname);

  free(sname);
}






 /*@@
   @routine    CCTKi_ParseStaggerString
   @date       
   @author     Gerd Lanfermann
   @desc       returns the stagger index for a string. Similar routines 
               as CCTK_StaggerIndexName, but does more error checking since 
               it is called during Group setup and if things go wrong, the used
               has a better idea where he specified wrong settings.

   @enddesc 
   @calls     
   @calledby   
   @history 
 
   @endhistory 

@@*/
 
int CCTKi_ParseStaggerString(int dim,
                             const char *imp, 
                             const char *gname,
                             const char *stype) 
{
  int i,m;
  int base  = 1;
  int scode = 0;
  char hs[11];

  if (dim>10) 
  {
    CCTK_VWarn(0,__LINE__,__FILE__,"Cactus",
	       "CCTKi_ParseStaggerString: Dimension %d exceeds maximum of 10",
	       dim);
  }

  /* change possible SHORTCUTS into the official notation*/
  if (CCTK_Equals(stype,"NONE"))
  {
    sprintf(hs,"MMMMMMMMMM");
  }
  else if (CCTK_Equals(stype,"CELL")) 
  {
    sprintf(hs,"CCCCCCCCCC");
  }
  else 
  {  
    if ((int) strlen(stype)!=dim) 
    {  
      CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
		"CCTKi_ParseStaggerString: Staggering %s for %s unequal to group dimension %d",
		stype,gname,dim);
    }

    sprintf(hs,"%s",stype);
  }
  
  for (i=0;i<dim;i++) 
  {
    switch (toupper(hs[i]))
    {
      case 'M':m=0; break;
      case 'C':m=1; break;
      case 'P':m=2; break;
      default:
        CCTK_VWarn(1,__LINE__,__FILE__,"Cactus",
		   "CCTKi_ParseStaggerString: Unknown stagger type %s for %s::%s",
		   stype,imp,gname);
        return(-1);
    }
    scode+= m*base;
    base  = 3 * base;
  }

  return scode;
}
