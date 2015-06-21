
StagLeap_alpha: Wrapper.F StagLeap.F
	f90 -O3 -c Wrapper.F -o Wrapper.o
	f90 -O3 -c StagLeap.F -o StagLeap.o
	f90 StagLeap.o Wrapper.o -o StagLeap

StagLeap_sun: Wrapper.F StagLeap.F
	f90 -O3 -c Wrapper.F -o Wrapper.o
	f90 -O3 -c StagLeap.F -o StagLeap.o
	f90 StagLeap.o Wrapper.o -o StagLeap

StagLeap_origin: Wrapper.F StagLeap.F
	f90 -cpp -O3 -OPT:roundoff=3:IEEE_arithmetic=3:const_copy_limit=100000 -r10000 -64 -mips4 -c Wrapper.F -o Wrapper.o
	f90 -cpp -O3 -OPT:roundoff=3:IEEE_arithmetic=3:const_copy_limit=100000 -r10000 -64 -mips4 -c StagLeap.F -o StagLeap.o
	f90 -64 -Wl,"-woff 84","-woff 85" StagLeap.o Wrapper.o -o StagLeap

NewStagLeap_origin: NewWrapper.F NewStagLeap.F
	f90 -cpp -O3 -OPT:roundoff=3:IEEE_arithmetic=3:const_copy_limit=100000 -r10000 -64 -mips4 -c NewWrapper.F -o NewWrapper.o
	f90 -cpp -O3 -OPT:roundoff=3:IEEE_arithmetic=3:const_copy_limit=100000 -r10000 -64 -mips4 -c NewStagLeap.F -o NewStagLeap.o
	f90 -64 -Wl,"-woff 84","-woff 85" NewStagLeap.o NewWrapper.o -o NewStagLeap


# IA64

# note that cc has to used as a linker otherwise it wouldn't link
StagLeap_ia64_intel: Wrapper.F StagLeap.F
	efc -132 -w95 -O3 -c $?
	cc StagLeap.o Wrapper.o -L${IA64ROOT}/lib -lIEPCF90 -lF90 -o $@

NewStagLeap_ia64_intel: NewWrapper.F NewStagLeap.F
	efc -132 -w95 -O3 -c $?
	cc NewStagLeap.o NewWrapper.o -L${IA64ROOT}/lib -lIEPCF90 -lF90 -o $@

StagLeap_ia64_sgi: Wrapper.F StagLeap.F
	sgif90 -cpp -O3 -c Wrapper.F -o Wrapper.o
	sgif90 -cpp -O3 -c StagLeap.F -o StagLeap.o
	sgif90 StagLeap.o Wrapper.o -o StagLeap

NewStagLeap_ia64_sgi: NewWrapper.F NewStagLeap.F
	sgif90 -cpp -O3 -c NewWrapper.F -o NewWrapper.o
	sgif90 -cpp -O3 -c NewStagLeap.F -o NewStagLeap.o
	sgif90 NewStagLeap.o NewWrapper.o -o NewStagLeap

# COUNTING FPOs on ORIGIN

StagLeap_count: Wrapper.F StagLeap.F
	f90  -TARG:madd=no -64 -mips4 -c Wrapper.F -o Wrapper.o
	f90  -TARG:madd=no -64 -mips4 -c StagLeap.F -o StapLeap.o
	f90 -64 -Wl,"-woff 84","-woff 85" StagLeap.o Wrapper.o -o StagLeap

clean:
	rm *.o StagLeap 
