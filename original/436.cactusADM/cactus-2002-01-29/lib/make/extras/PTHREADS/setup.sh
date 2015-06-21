#! /bin/sh
# /*@@
#   @file      setup.sh
#   @date      Wed Oct 25 16:32:39 2000
#   @author    Tom Goodale
#   @desc 
#   Setup for compilation with pthreads
#   @enddesc 
# @@*/
    
if test "X$PTHREADS" = "Xyes"; then

echo "Configuring with PTHREADS"

# Write the data out to the header and make files.

CCTK_WriteLine cctk_Extradefs.h "#define CCTK_PTHREADS 1"

# the PTHREADS_DEFINE variable is set by configure
if test "X$PTHREADS_DEFINE" != "X"; then
  CCTK_WriteLine cctk_Extradefs.h "#define $PTHREADS_DEFINE 1"
fi


# the PTHREAD_LIBS variable is set by configure
CCTK_WriteLine make.extra.defn "PTHREADS_LIBS = $PTHREAD_LIBS"
CCTK_WriteLine make.extra.defn ""
CCTK_WriteLine make.extra.defn ""
CCTK_WriteLine make.extra.defn 'LIBS     += $(PTHREADS_LIBS)'

fi  # if PTHREADS
