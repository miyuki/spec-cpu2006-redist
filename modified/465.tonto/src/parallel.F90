module PARALLEL_MODULE

#  include "parallel.use"

  implicit none

#  include "macros"
#  include "parallel.int"


#ifdef MPI
#include "mpif.h"
#endif

contains

  subroutine initialise(self)
    PARALLEL :: self
  ! Initialise the parallel environment.
STACK("PARALLEL:initialise")
START_TIMER("PARALLEL:initialise")
#ifdef MPI
    call create_(self%mpi_status,MPI_STATUS_SIZE)
    self%mpi_status = 0
    call MPI_init(self%mpi_status)
    call MPI_comm_size(MPI_COMM_WORLD,self%nprocs,self%mpi_status)
    call MPI_comm_rank(MPI_COMM_WORLD,self%rank,self%mpi_status)
    self%do_parallel = TRUE
#else
    self%nprocs = 1
    self%rank = 0
    self%do_parallel = FALSE
#endif
    STOP_TIMER("PARALLEL:initialise")
     CHECK
  end subroutine

  subroutine finalise(self)
    PARALLEL :: self
  ! Finalise the parallel environment.
STACK("PARALLEL:finalise")
START_TIMER("PARALLEL:finalise")
#ifdef MPI
    call MPI_finalize(self%mpi_status)
    call destroy_(self%mpi_status)
#endif
    self%do_parallel = FALSE
    STOP_TIMER("PARALLEL:finalise")
     CHECK
  end subroutine

!*******************************************************************************
! Inquiry routines.
!*******************************************************************************

  PURE function n_proc(self) result(res)
    PARALLEL :: self
  ! Return the number of processors available.
    IN :: self
    INT :: res
    if (self%do_parallel) then
      res = self%nprocs
    else
      res = 1
    end if
    STOP_TIMER("PARALLEL:n_proc")
  end function

  PURE function this_proc(self) result(res)
    PARALLEL :: self
  ! Return the index of this processor.  First index is zero!
    IN :: self
    INT :: res
    if (self%do_parallel) then
      res = self%rank
    else
      res = 0
    end if
    STOP_TIMER("PARALLEL:this_proc")
  end function

!*******************************************************************************
! Summation routines.
!*******************************************************************************

  subroutine sum_symmetric_matrices(self,mat)
    PARALLEL :: self
  ! This routine adds the versions of "mat" from all processors, and gives the
  ! result to all processors.  The lower triangle of the resultant symmetric
  ! matrix is forced to be the same as the upper triangle.
    REALMAT(:,:), INOUT :: mat
    REALVEC(:), PTR :: invec,outvec
    INT :: tri_size
    STACK("PARALLEL:sum_symmetric_matrices")
    START_TIMER("PARALLEL:sum_symmetric_matrices")
    if (self%do_parallel) then
      self%mpi_status = 0
      tri_size = tri_size_(mat)
      call create_(invec,tri_size)
      call create_(outvec,tri_size)
      call compress_to_triangle_(mat,invec)
#ifdef MPI
      call MPI_ALLREDUCE(invec,outvec,tri_size,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:sum_symmetric_matrices ... wtf?")
#endif
      call uncompress_from_triangle_(mat,outvec)
      call destroy_(outvec)
      call destroy_(invec)
    else
      ! do nothing, just return what we were given.
    end if
    STOP_TIMER("PARALLEL:sum_symmetric_matrices")
     CHECK
  end subroutine

  subroutine sum_matrices(self,mat)
    PARALLEL :: self
  ! This routine adds the versions of "mat" from all processors, and gives the
  ! result to all processors.
    REALMAT(:,:), INOUT :: mat
    REALMAT(:,:), PTR :: tempmat
    STACK("PARALLEL:sum_matrices")
    START_TIMER("PARALLEL:sum_matrices")
    if (self%do_parallel) then
      self%mpi_status = 0
      call create_(tempmat,size(mat,1),size(mat,2))
#ifdef MPI
      call MPI_ALLREDUCE(mat,tempmat,size(mat),MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,self%mpi_status)
      mat = tempmat
#else
      DIE("PARALLEL:sum_matrices ... wtf?")
#endif
      call destroy_(tempmat)
    else
      ! do nothing, just return what we were given.
    end if
    STOP_TIMER("PARALLEL:sum_matrices")
     CHECK
  end subroutine

  subroutine sum_vectors(self,vec)
    PARALLEL :: self
  ! This routine adds the versions of "vec" from all processors, and gives the
  ! result to all processors.
    REALVEC(:), INOUT :: vec
    REALVEC(:), PTR :: tempvec
    STACK("PARALLEL:sum_vectors")
    START_TIMER("PARALLEL:sum_vectors")
    if (self%do_parallel) then
      self%mpi_status = 0
      call create_(tempvec,size(vec))
#ifdef MPI
      call MPI_ALLREDUCE(vec,tempvec,size(vec),MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,self%mpi_status)
      vec = tempvec
#else
      DIE("PARALLEL:sum_vectors ... wtf?")
#endif
      call destroy_(tempvec)
    else
      ! do nothing, just return what we were given.
    end if
    STOP_TIMER("PARALLEL:sum_vectors")
     CHECK
  end subroutine

  subroutine sum_vectors_1(self,vec)
    PARALLEL :: self
  ! This routine adds the versions of "vec" from all processors, and gives the
  ! result to all processors.
    CPXVEC(:), INOUT :: vec
    CPXVEC(:), PTR :: tempvec
    STACK("PARALLEL:sum_vectors_1")
    START_TIMER("PARALLEL:sum_vectors_1")
    if (self%do_parallel) then
      self%mpi_status = 0
      call create_(tempvec,size(vec))
#ifdef MPI
      call MPI_ALLREDUCE(vec,tempvec,size(vec),MPI_DOUBLE_COMPLEX,MPI_SUM,MPI_COMM_WORLD,self%mpi_status)
      vec = tempvec
#else
      DIE("PARALLEL:sum_vectors_1 ... wtf?")
#endif
      call destroy_(tempvec)
    else
      ! do nothing, just return what we were given.
    end if
    STOP_TIMER("PARALLEL:sum_vectors_1")
     CHECK
  end subroutine

!*******************************************************************************
! Broadcast variables to all processors.
!*******************************************************************************

  subroutine broadcast(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    INT :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast")
    START_TIMER("PARALLEL:broadcast")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,1,MPI_INTEGER,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast")
     CHECK
  end subroutine

  subroutine broadcast_1(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    STR(*) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_1")
    START_TIMER("PARALLEL:broadcast_1")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,len(var),MPI_CHARACTER,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_1 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_1")
     CHECK
  end subroutine

  subroutine broadcast_2(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    REAL :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_2")
    START_TIMER("PARALLEL:broadcast_2")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,1,MPI_DOUBLE_PRECISION,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_2 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_2")
     CHECK
  end subroutine

  subroutine broadcast_3(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    CPX :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_3")
    START_TIMER("PARALLEL:broadcast_3")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,1,MPI_DOUBLE_COMPLEX,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_3 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_3")
     CHECK
  end subroutine

  subroutine broadcast_4(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    BIN :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_4")
    START_TIMER("PARALLEL:broadcast_4")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,1,MPI_LOGICAL,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_4 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_4")
     CHECK
  end subroutine

  subroutine broadcast_5(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    REALVEC(:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_5")
    START_TIMER("PARALLEL:broadcast_5")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_PRECISION,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_5 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_5")
     CHECK
  end subroutine

  subroutine broadcast_6(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    INTVEC(:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_6")
    START_TIMER("PARALLEL:broadcast_6")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_INTEGER,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_6 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_6")
     CHECK
  end subroutine

  subroutine broadcast_7(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    CPXVEC(:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_7")
    START_TIMER("PARALLEL:broadcast_7")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_COMPLEX,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_7 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_7")
     CHECK
  end subroutine

  subroutine broadcast_8(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    REALMAT(:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_8")
    START_TIMER("PARALLEL:broadcast_8")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_PRECISION,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_8 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_8")
     CHECK
  end subroutine

  subroutine broadcast_9(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    INTMAT(:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_9")
    START_TIMER("PARALLEL:broadcast_9")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_INTEGER,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_9 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_9")
     CHECK
  end subroutine

  subroutine broadcast_10(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    CPXMAT(:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_10")
    START_TIMER("PARALLEL:broadcast_10")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_COMPLEX,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_10 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_10")
     CHECK
  end subroutine

  subroutine broadcast_11(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    REALMAT3(:,:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_11")
    START_TIMER("PARALLEL:broadcast_11")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_PRECISION,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_11 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_11")
     CHECK
  end subroutine

  subroutine broadcast_12(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    INTMAT3(:,:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_12")
    START_TIMER("PARALLEL:broadcast_12")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_INTEGER,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_12 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_12")
     CHECK
  end subroutine

  subroutine broadcast_13(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    CPXMAT3(:,:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_13")
    START_TIMER("PARALLEL:broadcast_13")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_COMPLEX,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_13 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_13")
     CHECK
  end subroutine

  subroutine broadcast_14(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    REALMAT4(:,:,:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_14")
    START_TIMER("PARALLEL:broadcast_14")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_PRECISION,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_14 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_14")
     CHECK
  end subroutine

  subroutine broadcast_15(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    CPXMAT4(:,:,:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_15")
    START_TIMER("PARALLEL:broadcast_15")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_COMPLEX,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_15 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_15")
     CHECK
  end subroutine

  subroutine broadcast_16(self,var,proc)
    PARALLEL :: self
  ! Broadcast variable "var" from processor "proc" to all the other processors.
    CPXMAT5(:,:,:,:,:) :: var
    INT, IN :: proc
    STACK("PARALLEL:broadcast_16")
    START_TIMER("PARALLEL:broadcast_16")
    if (self%do_parallel) then
      self%mpi_status = 0
#ifdef MPI
      call MPI_BCAST(var,size(var),MPI_DOUBLE_COMPLEX,proc,MPI_COMM_WORLD,self%mpi_status)
#else
      DIE("PARALLEL:broadcast_16 ... wtf?")
#endif
    end if
    STOP_TIMER("PARALLEL:broadcast_16")
     CHECK
  end subroutine

  function do_io(self) result(res)
    PARALLEL :: self
  ! Return whether or not this processor should do the I/O operation.
    BIN :: res
    STACK("PARALLEL:do_io")
    START_TIMER("PARALLEL:do_io")
    res = self%rank == 0 OR (NOT self%do_parallel)
    STOP_TIMER("PARALLEL:do_io")
     CHECK
  end function

  subroutine synchronise_processors(self)
    PARALLEL :: self
  ! Synchronise all processors at this point.
STACK("PARALLEL:synchronise_processors")
START_TIMER("PARALLEL:synchronise_processors")
#ifdef MPI
    call MPI_BARRIER(MPI_COMM_WORLD,self%mpi_status)
#endif
    STOP_TIMER("PARALLEL:synchronise_processors")
     CHECK
  end subroutine

end
