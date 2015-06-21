 /*@@
   @header    cctk_Coord.h
   @date      Mon April 12 1999
   @author    Gabrielle Allen
   @desc
              Prototypes and constants for coordinate functions
   @enddesc
   @version   $Header: /cactus/Cactus/src/include/cctk_Coord.h,v 1.12 2001/12/29 11:19:10 allen Exp $
 @@*/

#ifndef _CCTK_COORD_H_
#define _CCTK_COORD_H_

#ifdef __cplusplus
extern "C"
{
#endif

int CCTK_CoordDir(const char *name,
                  const char *systemname);

int CCTK_CoordIndex(int dir,
                    const char *name,
                    const char *systemname);

const char *CCTK_CoordName (int dir, const char *systemname);

int CCTK_CoordRange(const cGH  *GH,
                    CCTK_REAL  *coord_lower,
                    CCTK_REAL  *coord_upper,
                    int         coord_dir,
                    const char *coord_name,
                    const char *system_name);

int CCTK_CoordRangePhysIndex (const cGH *GH,
                              int *lower,
                              int *upper,
                              int coorddir,
                              const char *coordname,
                              const char *systemname);

int CCTK_CoordRegisterData(int dir,
                           const char *gv,
                           const char *name,
                           const char *systemname);

#define CCTK_CoordRegisterSystem(a,b) \
        CCTKi_CoordRegisterSystem (a,CCTK_THORNSTRING,b)
int CCTKi_CoordRegisterSystem(int dim, 
			      const char *implementation, 
			      const char *systemname);

int CCTK_CoordSystemDim(const char *systemname);

int CCTK_CoordSystemHandle(const char *systemname);

const char *CCTK_CoordSystemName(int handle);

int CCTK_CoordLocalRange(const cGH  *GH,
                         CCTK_REAL  *lower,
                         CCTK_REAL  *upper,
                         int         coord_dir,
                         const char *coord_name,
                         const char *system_name);

int CCTK_CoordRegisterRange(cGH        *GH,
                            CCTK_REAL   coord_min,
                            CCTK_REAL   coord_max,
                            int         coord_dir,
                            const char *coord_name,
                            const char *system_name);

int CCTK_CoordRegisterRangePhysIndex(cGH        *GH,
                                     int         coord_min,
                                     int         coord_max,
                                     int         coord_dir,
                                     const char *coord_name,
                                     const char *system_name);

int CCTK_NumCoordSystems (void);

const char *CCTK_CoordSystemImplementation (int handle);

#ifdef __cplusplus
}
#endif

#endif /* _CCTK_COORD_H_ */
