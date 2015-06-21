!---------------------------------------------------------------------------
!
!  DIIS:  DIIS Extrapolation technique for vectors
!
! Copyright (C) Dylan Jayatilaka, 1997
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Library General Public License for more details.
!
! You should have received a copy of the GNU Library General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA  02111-1307, USA.
!
! $Id: diis.foo,v 1.2.2.4 2003/10/13 04:13:10 reaper Exp $
!---------------------------------------------------------------------------

module DIIS_MODULE

#  include "diis.use"

   implicit none

#  include "macros"
#  include "diis.int"


contains

   subroutine create(self,root_name,name,diis_kind,format,keep)
    DIIS :: self
   ! Create the DIIS object, but no archive name
      PTR :: self
      STR(STR_SIZE), optional :: root_name,name
      STR(STR_SIZE), optional :: diis_kind,format
      INT, IN, optional :: keep
      STACK("DIIS:create")
      START_TIMER("DIIS:create")
      nullify(self)
      allocate(self)
      ADD_MEMORY(DIIS_SIZE)
      call nullify_ptr_part_(self)
      call set_defaults_(self)
      call set_(self%archive,root_name,name,diis_kind,format)
      call set_keep_(self,keep)
     STOP_TIMER("DIIS:create")
      UNSTACK
   end subroutine

   subroutine destroy(self)
    DIIS :: self
   ! Destroy the DIIS object
      PTR :: self
      STACK("DIIS:destroy")
      START_TIMER("DIIS:destroy")
      if (NOT associated(self)) then; STOP_TIMER("DIIS:destroy") UNSTACK return; end if
      call delete_archives_(self)
      call destroy_ptr_part_(self)
      DELETE_MEMORY(DIIS_SIZE)
      deallocate(self)
     STOP_TIMER("DIIS:destroy")
      UNSTACK
   end subroutine

!   created result(res)
!   ! Returns true if self has been created
!      self :: PTR
!      res :: BIN
!      res = associated(self)
!   end

!   destroyed result(res)
!   ! Returns true if self has *not* been created
!      self :: PTR
!      res :: BIN
!      res = NOT associated(self)
!   end

   subroutine nullify_ptr_part(self)
    DIIS :: self
   ! Nullify the pointer parts of self
      STACK("DIIS:nullify_ptr_part")
      START_TIMER("DIIS:nullify_ptr_part")
      nullify(self%coeff)
      call nullify_ptr_part_(self%archive)
      nullify(self%error_items)
      nullify(self%parameter_items)
      nullify(self%diis_matrix)
      nullify(self%constraint_matrix)
      nullify(self%density_matrix)
     STOP_TIMER("DIIS:nullify_ptr_part")
      CHECK
   end subroutine

   subroutine destroy_ptr_part(self)
    DIIS :: self
   ! Destroy the pointer parts of self
      STACK("DIIS:destroy_ptr_part")
      START_TIMER("DIIS:destroy_ptr_part")
      call destroy_(self%coeff)
      if (self%in_core) then
        call destroy_(self%error_items)
        call destroy_(self%parameter_items)
        call destroy_(self%diis_matrix)
        call destroy_(self%constraint_matrix)
        call destroy_(self%density_matrix)
      end if
      call destroy_ptr_part_(self%archive)
     STOP_TIMER("DIIS:destroy_ptr_part")
      UNSTACK
   end subroutine

   subroutine create_copy(self,diis)
    DIIS :: self
   ! Make a copy of the diis object
      PTR :: self
      DIIS :: diis
      STACK("DIIS:create_copy")
      START_TIMER("DIIS:create_copy")
      call create_(self)
      call copy_(self,diis)
     STOP_TIMER("DIIS:create_copy")
      UNSTACK
   end subroutine

   subroutine copy(self,diis)
    DIIS :: self
   ! Make a copy of the diis object
      DIIS :: diis
      STACK("DIIS:copy")
      START_TIMER("DIIS:copy")
      self = diis
      call nullify_ptr_part_(self)
      if (associated(diis%error_items)) call create_copy_(self%error_items,diis%error_items)
      if (associated(diis%parameter_items)) call create_copy_(self%parameter_items,diis%parameter_items)
      if (associated(diis%diis_matrix)) call create_copy_(self%diis_matrix,diis%diis_matrix)
      if (associated(diis%constraint_matrix)) call create_copy_(self%constraint_matrix,diis%constraint_matrix)
      if (associated(diis%density_matrix)) call create_copy_(self%density_matrix,diis%density_matrix)
      call copy_(self%archive,diis%archive)
      if (associated(diis%coeff)) call create_copy_(self%coeff,diis%coeff)
     STOP_TIMER("DIIS:copy")
      UNSTACK
   end subroutine

   subroutine delete_archives(self)
    DIIS :: self
   ! Delete the archive files on disk.
     INT :: i
     STR(STR_SIZE) :: name
     STACK("DIIS:delete_archives")
     START_TIMER("DIIS:delete_archives")
     call destroy_(self%error_items)
     call destroy_(self%parameter_items)
     call destroy_(self%diis_matrix)
     call destroy_(self%constraint_matrix)
     call destroy_(self%density_matrix)
     if (NOT self%in_core) then
       name = self%archive%name
       call set_name_(self%archive, trim(name) // ",mat" )
       if (exists_(self%archive))  call delete_(self%archive)
       do i=1, self%n_vec
         call set_name_(self%archive,trim(name) // ",p" // trim(to_str_(i)))
         if (exists_(self%archive))  call delete_(self%archive)
         call set_name_(self%archive,trim(name) // ",e" // trim(to_str_(i)))
         if (exists_(self%archive))  call delete_(self%archive)
       end do
       call set_name_(self%archive,trim(name))
     end if
     self%new=0
     self%n_vec = 0
     self%error_length = ZERO
     STOP_TIMER("DIIS:delete_archives")
      CHECK
   end subroutine

   subroutine cleanup(self)
    DIIS :: self
   ! Cleanup the DIIS files and matrices, but don't resort back to default
   ! parameters.
     STACK("DIIS:cleanup")
     START_TIMER("DIIS:cleanup")
     call delete_archives_(self)
     call destroy_ptr_part_(self)
     self%n_vec = 0
     self%new = 0
     self%error_length = ZERO
     call destroy_(self%coeff)
     STOP_TIMER("DIIS:cleanup")
      CHECK
   end subroutine

   subroutine set_defaults(self)
    DIIS :: self
   ! Set up the default settings
      STACK("DIIS:set_defaults")
      START_TIMER("DIIS:set_defaults")
      self%keep  = DIIS_KEEP
      self%n_vec = 0
      self%new = 0
      self%error_length = ZERO
      call set_defaults_(self%archive)
      call destroy_ptr_part_(self) ! ensure ptr parts are nullified first !
      self%in_core = TRUE
     STOP_TIMER("DIIS:set_defaults")
      CHECK
   end subroutine

   subroutine set_keep(self,keep)
    DIIS :: self
   ! Set the number of DIIS objects to keep
      INT, IN :: keep
      STACK("DIIS:set_keep")
      START_TIMER("DIIS:set_keep")
      self%keep = keep
     STOP_TIMER("DIIS:set_keep")
      CHECK
   end subroutine

   subroutine set_archive_root_name(self,root_name)
    DIIS :: self
   ! Set the DIIS archive to have root name "root_name".
      STR(*) :: root_name
      STACK("DIIS:set_archive_root_name")
      START_TIMER("DIIS:set_archive_root_name")
      call set_root_name_(self%archive,root_name)
     STOP_TIMER("DIIS:set_archive_root_name")
      CHECK
   end subroutine

   subroutine set_archive_name(self,name)
    DIIS :: self
   ! Set the DIIS archive name.
      STR(*) :: name
      STACK("DIIS:set_archive_name")
      START_TIMER("DIIS:set_archive_name")
      call set_name_(self%archive,name)
     STOP_TIMER("DIIS:set_archive_name")
      CHECK
   end subroutine

   subroutine set_archive_format(self,format)
    DIIS :: self
   ! Set the DIIS archive format.
      STR(*) :: format
      STACK("DIIS:set_archive_format")
      START_TIMER("DIIS:set_archive_format")
      call set_format_(self%archive,format)
     STOP_TIMER("DIIS:set_archive_format")
      CHECK
   end subroutine

!*************************************************************************


   function worst_item(self) result(res)
    DIIS :: self
   ! Return the index of the item with the worst error in the DIIS archive
      INT :: res
      REALMAT(:,:), PTR :: mat1
      REALVEC(:), PTR :: diag
      INT :: dim
      STACK("DIIS:worst_item")
      START_TIMER("DIIS:worst_item")
      dim = dimension_(self)
      call create_(diag,dim)
      call create_(mat1,dim,dim)
      call get_old_diis_matrix_(self,mat1)
      call get_diagonal_(mat1,diag)             ! the error vector lengths^2
      res = index_of_largest_value_(diag)   ! Worst item has the largest error
      call destroy_(mat1)
      call destroy_(diag)
     STOP_TIMER("DIIS:worst_item")
      CHECK
   end function

   function next_replacement(self) result(res)
    DIIS :: self
   ! Return the index of the next item in the DIIS expansion
   ! which is to be replaced/saved
      INT :: res
      STACK("DIIS:next_replacement")
      START_TIMER("DIIS:next_replacement")
      if (self%n_vec<=self%keep) then
         res = self%n_vec
      else
         res = worst_item_(self)
      end if
     STOP_TIMER("DIIS:next_replacement")
      CHECK
   end function

   subroutine save_pair(self,par,err,item)
    DIIS :: self
   ! Save/replace the current vectors on an archive with item number "item",
   ! or item number ".new" if "item" is not present.
      REALVEC(:) :: par,err
      INT, optional :: item
      INT :: i
      STACK("DIIS:save_pair")
      START_TIMER("DIIS:save_pair")
      i = self%new
      if (present(item)) i = item
      call save_item_(self,err,"e",i)
      call save_item_(self,par,"p",i)
     STOP_TIMER("DIIS:save_pair")
      CHECK
   end subroutine

   subroutine save_item(self,mat,name,i)
    DIIS :: self
   ! Save "mat" with "name" and number "i" to disk.
     REALMAT(:,:) :: mat
     STR(*) :: name
     INT, optional, IN :: i
     STR(STR_SIZE) :: old_name
     STACK("DIIS:save_item")
     START_TIMER("DIIS:save_item")
     if (self%in_core) then
       select case(name)
         case ("mat")
           call destroy_(self%diis_matrix)
           call create_(self%diis_matrix,size(mat,1),size(mat,2))
           self%diis_matrix(:,:) = mat
         case default
           DIE("DIIS:save_item ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call write_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if
     STOP_TIMER("DIIS:save_item")
      CHECK
   end subroutine

   subroutine save_item_1(self,vec,name,i)
    DIIS :: self
   ! Save "mat" with "name" and number "i" to disk.
     REALVEC(:) :: vec
     STR(*) :: name
     INT, optional, IN :: i
     INT :: ii
     STR(STR_SIZE) :: old_name
     STACK("DIIS:save_item_1")
     START_TIMER("DIIS:save_item_1")
     ii = 1
     if (present(i)) ii=i
     if (self%in_core) then
       select case(name)
         case ("p")
           if (NOT associated(self%parameter_items)) call create_(self%parameter_items,size(vec),self%keep)
           self%parameter_items(:,ii) = vec
         case ("e")
           if (NOT associated(self%error_items)) call create_(self%error_items,size(vec),self%keep)
           self%error_items(:,ii) = vec
         case ("constraint")
           if (NOT associated(self%constraint_matrix)) call create_(self%constraint_matrix,size(vec),self%keep)
           self%constraint_matrix(:,ii) = vec
         case default
           DIE("DIIS:save_item_1 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call write_(self%archive,vec)
       call set_name_(self%archive,old_name)
     end if
     STOP_TIMER("DIIS:save_item_1")
      CHECK
   end subroutine

   subroutine save_item_2(self,mat,name,i)
    DIIS :: self
   ! Save "mat" with "name" and number "i" to disk.
     OPMATRIX :: mat
     STR(*) :: name
     INT, optional, IN :: i
     INT :: ii
     STR(STR_SIZE) :: old_name
     STACK("DIIS:save_item_2")
     START_TIMER("DIIS:save_item_2")
     ii = 1
     if (self%in_core) then
       select case(name)
         case ("density")
           select case (number_kind_(mat))
             case ("real")
               call compress_(mat)
               if (NOT associated(self%density_matrix))&
                 call create_(self%density_matrix,size(mat%triangle),self%keep)
               self%density_matrix(:,ii) = mat%triangle
               call uncompress_(mat)
             case ("complex")
               call compress_(mat)
               if (NOT associated(self%density_matrix)) &
                 call create_(self%density_matrix,size(mat%square),self%keep)
               self%density_matrix(:,ii) = mat%square
               call uncompress_(mat)
           end select
         case default
           DIE("DIIS:save_item_2 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call write_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if
     STOP_TIMER("DIIS:save_item_2")
      CHECK
   end subroutine

   subroutine get_item(self,mat,name,i)
    DIIS :: self
   ! Read "mat" with "name" and number "i" to disk.
     REALMAT(:,:) :: mat
     STR(*) :: name
     INT, optional, IN :: i
     STR(STR_SIZE) :: old_name
     STACK("DIIS:get_item")
     START_TIMER("DIIS:get_item")
     if (self%in_core) then
       select case(name)
         case ("mat")
           mat = self%diis_matrix(:,:)
         case default
           DIE("DIIS:get_item ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call read_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if
     STOP_TIMER("DIIS:get_item")
      CHECK
   end subroutine

   subroutine get_item_1(self,vec,name,i)
    DIIS :: self
   ! Read "mat" with "name" and number "i" to disk.
     REALVEC(:) :: vec
     STR(*) :: name
     INT, optional, IN :: i
     STR(STR_SIZE) :: old_name
     INT :: ii
     STACK("DIIS:get_item_1")
     START_TIMER("DIIS:get_item_1")
     ii = 1
     if (present(i)) ii=i
     if (self%in_core) then
       select case(name)
         case ("p")
           vec = self%parameter_items(:,ii)
         case ("e")
           vec = self%error_items(:,ii)
         case ("constraint")
           vec = self%constraint_matrix(:,ii)
         case ("density")
           vec = self%density_matrix(:,ii)
         case default
           DIE("DIIS:get_item_1 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call read_(self%archive,vec)
       call set_name_(self%archive,old_name)
     end if
     STOP_TIMER("DIIS:get_item_1")
      CHECK
   end subroutine

   subroutine get_item_2(self,mat,name,i)
    DIIS :: self
   ! Read "mat" with "name" and number "i" to disk.
     OPMATRIX :: mat
     STR(*) :: name
     INT, optional, IN :: i
     STR(STR_SIZE) :: old_name
     STACK("DIIS:get_item_2")
     START_TIMER("DIIS:get_item_2")
     if (self%in_core) then
       select case(name)
         case default
           DIE("DIIS:get_item_2 ... unknown matrix " // trim(name))
       end select
     else
       old_name = self%archive%name
       if (present(i)) then
         call set_name_(self%archive,trim(old_name) // "," // trim(name) // trim(to_str_(i)))
       else
         call set_name_(self%archive,trim(old_name) // "," // trim(name))
       end if
       call read_(self%archive,mat)
       call set_name_(self%archive,old_name)
     end if
     STOP_TIMER("DIIS:get_item_2")
      CHECK
   end subroutine

   subroutine get_error_item(self,i,err)
    DIIS :: self
   ! Get the error item "i" in vector "err"
      INT :: i
      REALVEC(:) :: err
      STACK("DIIS:get_error_item")
      START_TIMER("DIIS:get_error_item")
      call get_item_(self,err,"e",i)
     STOP_TIMER("DIIS:get_error_item")
      CHECK
   end subroutine

   subroutine get_parameter_item(self,i,par)
    DIIS :: self
   ! Get the error item "i" in vector "err"
      INT :: i
      REALVEC(:) :: par
      STACK("DIIS:get_parameter_item")
      START_TIMER("DIIS:get_parameter_item")
      call get_item_(self,par,"p",i)
     STOP_TIMER("DIIS:get_parameter_item")
      CHECK
   end subroutine

   subroutine save_diis_mat(self,mat)
    DIIS :: self
   ! Save the DIIS matrix to disk
      REALMAT(:,:) :: mat
      STACK("DIIS:save_diis_mat")
      START_TIMER("DIIS:save_diis_mat")
      call save_item_(self,mat,"mat")
     STOP_TIMER("DIIS:save_diis_mat")
      CHECK
   end subroutine

   subroutine get_diis_mat(self,mat)
    DIIS :: self
   ! Get the DIIS matrix from disk
      REALMAT(:,:) :: mat
      STACK("DIIS:get_diis_mat")
      START_TIMER("DIIS:get_diis_mat")
      call get_item_(self,mat,"mat")
     STOP_TIMER("DIIS:get_diis_mat")
      CHECK
   end subroutine

   subroutine get_old_diis_matrix(self,mat)
    DIIS :: self
   ! Get the old DIIS matrix to "mat", if it is smaller
      REALMAT(:,:) :: mat
      STACK("DIIS:get_old_diis_matrix")
      START_TIMER("DIIS:get_old_diis_matrix")
      if (self%n_vec==1) then
         mat = ZERO
      else if (self%n_vec<=self%keep) then
         call get_diis_mat_(self,mat(1:self%n_vec,1:self%n_vec))
      else
        call get_diis_mat_(self,mat)
      end if
     STOP_TIMER("DIIS:get_old_diis_matrix")
      CHECK
   end subroutine

   subroutine make_diis_matrix(self,mat,err,old_err)
    DIIS :: self
   ! Make the current diis matrix "mat" using "err" as the current error vector
   ! for item ".new". "old_err" is used to hold the old error vectors.
      REALMAT(:,:) :: mat
      REALVEC(:) :: err,old_err
      INT :: dim,old
      STACK("DIIS:make_diis_matrix")
      START_TIMER("DIIS:make_diis_matrix")
      call get_old_diis_matrix_(self,mat)
      dim = dimension_(self)
      do old = 1,dim-1                         ! Calculate the DIIS matrix
         call get_error_item_(self,old,old_err)
         ! max and min are because we are doing only the upper triangle -
         ! symmetric reflect later....
         mat(max(self%new,old),min(self%new,old)) = dot_product(err,old_err)
         mat(dim,old) = ONE
      end do
      mat(dim,dim) = ZERO
      call symmetric_reflect_(mat)
      call save_diis_mat_(self,mat)
      self%error_length = sqrt(mat(self%new,self%new))
     STOP_TIMER("DIIS:make_diis_matrix")
      CHECK
   end subroutine

   subroutine remake_diis_matrix(self,err_vec_dim)
    DIIS :: self
   ! Rebuild the DIIS matrix using the error vectors on disk.  Also resets
   ! ".new".  "dimension" is the length of an error vector.
      INT, IN :: err_vec_dim
      REALVEC(:), PTR :: err_i,err_j
      REALMAT(:,:), PTR :: mat
      INT :: dim,i,j
      STACK("DIIS:remake_diis_matrix")
      START_TIMER("DIIS:remake_diis_matrix")
      dim = dimension_(self)
      call create_(err_i,err_vec_dim)
      call create_(err_j,err_vec_dim)
      call create_(mat,dim,dim)
      mat = ZERO
      do i=1,dim-1
        call get_error_item_(self,i,err_i)
        do j=1,i
          call get_error_item_(self,j,err_j)
          mat(i,j) = dot_product(err_i,err_j)
        end do
        mat(dim,i) = ONE
      end do
      mat(dim,dim) = ZERO
      call symmetric_reflect_(mat)
      self%new   = next_replacement_(self)
      call save_diis_mat_(self,mat)
      self%error_length = sqrt(mat(self%new,self%new))
      call destroy_(err_j)
      call destroy_(err_i)
      call destroy_(mat)
     STOP_TIMER("DIIS:remake_diis_matrix")
      CHECK
   end subroutine

   subroutine update(self,par,old_par,coeff)
    DIIS :: self
   ! Update the parameter vector "par", using "old_par" as space
      REALVEC(:) :: par,old_par,coeff
      INT :: old,dim
      STACK("DIIS:update")
      START_TIMER("DIIS:update")
      dim = dimension_(self)
      par = ZERO
      do old = 1,dim-1                         ! Form revised parameter vector
         call get_parameter_item_(self,old,old_par)
         par = par + coeff(old)*old_par
      end do
     STOP_TIMER("DIIS:update")
      CHECK
   end subroutine

   function dimension(self) result(res)
    DIIS :: self
   ! Return the dimension of the DIIS matrix
      INT :: res
      STACK("DIIS:dimension")
      START_TIMER("DIIS:dimension")
      res = min(self%n_vec,self%keep)+1
     STOP_TIMER("DIIS:dimension")
      CHECK
   end function

   subroutine extrapolate(self,par,err)
    DIIS :: self
   ! DIIS extrapolation of "par", using "err" as the error vector.
      REALVEC(:) :: par,err
      INT :: dim,j
      REALVEC(:), PTR :: rhs,coeff,diag
      REALMAT(:,:), PTR :: mat1
      STACK("DIIS:extrapolate")
      START_TIMER("DIIS:extrapolate")
      ENSURE(file_name_(self%archive)/=" ","DIIS:extrapolate ... no archive name specified")
      self%n_vec = self%n_vec+1
      dim    = dimension_(self)
      self%new   = next_replacement_(self)
      call save_pair_(self,par,err,item=self%new)
      call create_(mat1,dim,dim)
      call make_diis_matrix_(self,mat1,err,par)  ! Calculate the new diis matrix
      call times_(mat1(self%new,self%new),1.02d0)    ! Weight the last vector a bit more
      call get_parameter_item_(self,self%new,par)
      if (self%n_vec>1) then
         call create_(coeff,dim)
         call create_(rhs,dim)
         coeff = ZERO                  ! the diis coefficients
         rhs = ZERO                    ! right hand side of the linear equations
         rhs(dim) = ONE

         ! Rescale using the diagonals to make solution more numerically stable.
         call create_(diag,dim-1)
         call get_diagonal_(mat1(1:dim-1,1:dim-1),diag)
         forall (j=1:dim-1)
           mat1(1:dim-1,j) = mat1(1:dim-1,j)/diag(:)
         end forall
         mat1(1:dim-1,dim) = TOL(8)/diag(:)
         call destroy_(diag)

         mat1(dim,:) = ONE
         mat1(dim,dim) = ZERO

         call solve_linear_equation_(mat1,rhs,coeff)      ! Solve diis equations

         call destroy_(rhs)
         call update_(self,par,err,coeff)
         call get_error_item_(self,self%new,err)                   ! Put back old error item
         call destroy_(coeff)
      end if
      call destroy_(mat1)
     STOP_TIMER("DIIS:extrapolate")
      CHECK
   end subroutine

   subroutine extrapolate_1(self,par,err)
    DIIS :: self
   ! DIIS extrapolation of "par", using "err" as the error vector.
      REALMAT(:,:) :: par,err
      REALVEC(:), PTR :: par_vec,err_vec
      STACK("DIIS:extrapolate_1")
      START_TIMER("DIIS:extrapolate_1")
      call create_(par_vec,size(par))
      call create_(err_vec,size(err))
      par_vec = reshape(par,(/size(par)/))
      err_vec = reshape(err,(/size(err)/))
      call extrapolate_(self,par_vec,err_vec)
      par = reshape(par_vec,(/size(par,1),size(par,2)/))
      err = reshape(err_vec,(/size(err,1),size(err,2)/))
      call destroy_(err_vec)
      call destroy_(par_vec)
     STOP_TIMER("DIIS:extrapolate_1")
      CHECK
   end subroutine

!***************************************************************************

   subroutine put(self,out)
    DIIS :: self
   ! Prints out the DIIS data to file "out"
     TEXTFILE :: out
     STACK("DIIS:put")
     START_TIMER("DIIS:put")
     call flush_(out)
     call text_(out,"DIIS data: ")
     call show_(out,"Archive root name         = ", self%archive%root_name)
     call show_(out,"No to keep                = ", self%keep,real_width=TRUE)
     call show_(out,"Stored in core            = ", self%in_core,real_width=TRUE)
     call flush_(out)
     STOP_TIMER("DIIS:put")
      CHECK
   end subroutine

end
