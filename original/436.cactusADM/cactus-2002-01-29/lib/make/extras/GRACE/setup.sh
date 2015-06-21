#! /bin/sh
# /*@@
#   @file      setup.sh
#   @date      
#   @author    Erik Schnetter
#   @desc 
#   
#   @enddesc 
#   @history 
#   @hdate Thu Jan 27 21:06:23 2000 @hauthor Tom Goodale
#   @hdesc Placed in main distribution and changed GRACE to GRACE
#   @endhistory
#   @version $Header: /cactus/Cactus/lib/make/extras/GRACE/setup.sh,v 1.1 2000/01/28 03:15:04 goodale Exp $ 
# @@*/

if test -n "$GRACE"; then

    echo "Configuring with GRACE."

    if test -z "$GRACE_DIR"; then
	echo "GRACE selected but no GRACE_DIR set"
        exit 2
    fi
    if test -z "$GRACE_ARCH"; then
	echo "GRACE selected but no GRACE_ARCH set"
        exit 2
    fi
    GRACE_INC_DIRS="$GRACE_DIR/include $GRACE_RNPL_INC_DIRS $GRACE_HDF_INC_DIRS $GRACE_IEEEIO_INC_DIRS $GRACE_MPI_INC_DIRS"
    GRACE_LIB_DIRS="$GRACE_DIR/lib/$GRACE_ARCH $GRACE_RNPL_LIB_DIRS $GRACE_HDF_LIB_DIRS $GRACE_IEEEIO_LIB_DIRS $GRACE_MPI_LIB_DIRS"
    GRACE_LIBS="grace $GRACE_RNPL_LIBS $GRACE_HDF_LIBS $GRACE_IEEEIO_LIBS $GRACE_MPI_LIBS"

#    CCTK_WriteLine cctk_Extradefs.h "#define GRACE"

    CCTK_WriteLine make.extra.defn "GRACE_INC_DIRS = $GRACE_INC_DIRS"
    CCTK_WriteLine make.extra.defn "GRACE_LIB_DIRS = $GRACE_LIB_DIRS"
    CCTK_WriteLine make.extra.defn "GRACE_LIBS     = $GRACE_LIBS"

    CCTK_WriteLine make.extra.defn ""

    CCTK_WriteLine make.extra.defn 'SYS_INC_DIRS += $(GRACE_INC_DIRS)'
    CCTK_WriteLine make.extra.defn 'LIBDIRS      += $(GRACE_LIB_DIRS)'
    CCTK_WriteLine make.extra.defn 'LIBS         += $(GRACE_LIBS)'

fi
