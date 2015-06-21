! Copyright (C) Dylan Jayatilaka, 1999
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
! $Id: run_mol.foo,v 1.9.2.6 2003/11/13 06:34:19 reaper Exp $

program run_MOL

#  include "run_mol.use"

   use MOL_main_MODULE
!SPEC   use PARALLEL_MODULE

   implicit none

#  include "macros"
#  include "run_mol.int"


   MOL, PTR :: m DEFAULT_NULL

   call initialize_(tonto)

   call initialise_(tonto_parallel)

   call create_stdin_(stdin)
   call open_(stdin)
   call create_stdout_(stdout)
   call open_(stdout)

   call main_(m)

   call report_(tonto)

   call finalise_(tonto_parallel)
end program
