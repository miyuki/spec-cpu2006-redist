/*@@
   @header    Symmetry.h
   @date      Sun 7th Mar 1999
   @author    Gerd Lanfermann
   @desc 
   The extensions to the GH structure for 3D grid symmetry Treatment
   We'll have six int array for every GF, which holds a flag for which symmetry or 
   (physical) bnd-condition to apply at the grid faces.

   * These tables are set by SetSymmetry(GF,int,int,int) 
     during initialization. 
   * Default values ?
   * The information is used during evolution by Einstein_DoBound(GF), 
     Einstein_DoSym(GF)
   @enddesc
   @history
   @endhistory
   @version $Header: /cactus/CactusBase/CartGrid3D/src/Symmetry.h,v 1.5 2000/05/10 12:03:55 allen Exp $
 @@*/

#ifndef _SYMMETRY_H_
#define _SYMMETRY_H_

#define GFSYM_UNSET -42
#define GFSYM_NOSYM -41

typedef struct Symmetry 
{

  /* Symmetry[0..GF-1][0..dim-1] */
  /* in each direction [0,..dim-1], this will hold the symmetry 
     operation across that plane, iff the grid layout requires this.
     this compares to the {sx,sy,sz} of Cactus3.2  */
  int **GFSym;

} SymmetryGHex;

#ifdef __cplusplus
extern "C" 
{
#endif

int SetCartSymVI(cGH *GH, int *sym, int vi);
int SetCartSymGI(cGH *GH, int *sym, int vi);
int SetCartSymVN(cGH *GH, int *sym, const char *vn); 
int SetCartSymGN(cGH *GH, int *sym, const char *vn); 

int CartSymVI(cGH *GH, int vi);
int CartSymGI(cGH *GH, int gi);
int CartSymVN(cGH *GH, const char *vn);
int CartSymGN(cGH *GH, const char *gn);

#ifdef __cplusplus
}
#endif

#endif /* _SYMMETRY_H_ */
