/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf-3/nctest/tests.h,v 1.6 2004/11/16 21:33:07 russ Exp $
 *********************************************************************/

#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern void	test_nccreate	PROTO((
				       const char*
				       ));
extern void	test_ncopen	PROTO((
				       const char*
				       ));
extern void	test_ncredef	PROTO((
				       const char*
				       ));
extern void	test_ncendef	PROTO((
				       const char*
				       ));
extern void	test_ncclose	PROTO((
				       const char*
				       ));
extern void	test_ncinquire	PROTO((
				       const char*
				       ));
extern void	test_ncsync	PROTO((
				       const char*
				       ));
extern void	test_ncabort	PROTO((
				       const char*
				       ));
extern void	test_ncdimdef	PROTO((
				       const char*
				       ));
extern void	test_ncdimid	PROTO((
				       const char*
				       ));
extern void	test_ncdiminq	PROTO((
				       const char*
				       ));
extern void	test_ncdimrename	PROTO((
					       const char*
					       ));
extern void	test_ncvardef	PROTO((
				       const char*
				       ));
extern void	test_ncvarid	PROTO((
				       const char*
				       ));
extern void	test_ncvarinq	PROTO((
				       const char*
				       ));
extern void	test_ncvarput1	PROTO((
				       const char*
				       ));
extern void	test_ncvarget1	PROTO((
				       const char*
				       ));
extern void	test_ncvarput	PROTO((
				       const char*
				       ));
extern void	test_ncvarget	PROTO((
				       const char*
				       ));
extern void	test_ncvarputg	PROTO((
				       const char*
				       ));
extern void	test_ncvargetg	PROTO((
				       const char*
				       ));
extern void	test_ncrecinq	PROTO((
				       const char*
				       ));
extern void	test_ncrecput	PROTO((
				       const char*
				       ));
extern void	test_ncrecget	PROTO((
				       const char*
				       ));
extern void	test_ncvarrename	PROTO((
					       const char*
					       ));
extern void	test_ncattput	PROTO((
				       const char*
				       ));
extern void	test_ncattinq	PROTO((
				       const char*
				       ));
extern void	test_ncattget	PROTO((
				       const char*
				       ));
extern void	test_ncattcopy	PROTO((
				       const char*,
				       const char*
				       ));
extern void	test_ncattname	PROTO((
				       const char*
				       ));
extern void	test_ncattrename	PROTO((
					       const char*
					       ));
extern void	test_ncattdel	PROTO((
				       const char*
				       ));
extern void	test_nctypelen	PROTO((
					void
				       ));
extern int	test_varputget	PROTO((
				       int
				       ));
extern int	test_varputgetg	PROTO((
				       int
				       ));
extern int	test_slabs	PROTO((
				       int
				       ));

#ifdef __cplusplus
}
#endif
