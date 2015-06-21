!-------------------------------------------------------------------------------
!
! MOL: An object representation of a molecule.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
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
! $Id: mol_main.foo,v 1.53.2.31 2004/04/21 09:12:56 reaper Exp $
!-------------------------------------------------------------------------------

module MOL_main_MODULE

#  include "mol_main.use"

   use REALVEC_MODULE, only: minimise_BFGS

   use MOL_MODULE

   implicit none

#  include "macros"
#  include "mol_main.int"


   MOL, PTR, private :: saved_self

contains

   subroutine read_CX_file_name(self)
    MOL :: self
   ! Read the CX file name
      STACK("MOL:read_CX_file_name")
      START_TIMER("MOL:read_CX_file_name")
      call read_(stdin,self%CX_file_name)
     STOP_TIMER("MOL:read_CX_file_name")
      CHECK
   end subroutine

   subroutine read_CIF_file_name(self)
    MOL :: self
   ! Read the CIF file name
      TEXTFILE :: file
      STACK("MOL:read_CIF_file_name")
      START_TIMER("MOL:read_CIF_file_name")
      WARN_IF(self%CIF_file_name/=" ","MOL:read_CIF_file_name ... a CIF file name already exists")
      WARN_IF(self%CIF_data_block_name/=" ","MOL:read_CIF_file_name ... a CIF data block name already exists")
      call read_(stdin,self%CIF_file_name)
      ENSURE(exists_(file,name=self%CIF_file_name),"MOL:read_CIF_file_name ... no CIF file exists named: "//trim(self%CIF_file_name))
     STOP_TIMER("MOL:read_CIF_file_name")
      CHECK
   end subroutine

   subroutine read_CIF_data_block_name(self)
    MOL :: self
   ! Read the CIF data block name, set the job name to be this block name.
      STACK("MOL:read_CIF_data_block_name")
      START_TIMER("MOL:read_CIF_data_block_name")
      ENSURE(self%CIF_file_name/=" ","MOL:read_CIF_data_block_name ... specify CIF file name first")
      call read_(stdin,self%CIF_data_block_name)
      self%name = trim(filename_head_(self%CIF_file_name)) // "_" &
           // self%CIF_data_block_name ! Set name to the crystal data name
      WARN("MOL:read_CIF_data_block_name ... Job name has now been set to: "//trim(self%name))
     STOP_TIMER("MOL:read_CIF_data_block_name")
      CHECK
   end subroutine

   subroutine find_CIF_crystal_data_block(self,found)
    MOL :: self
   ! Find the *first* block of crystal and atom information from the CIF file,
   ! and set the locator ".CIF_data_block_name".  The name of the molecule (job)
   ! is set to be the name of the cif file concatenated with the data block
   ! name.  
      BIN, optional, OUT :: found
      STACK("MOL:find_CIF_crystal_data_block")
      START_TIMER("MOL:find_CIF_crystal_data_block")
      ENSURE(self%CIF_file_name/=" ","MOL:find_CIF_crystal_data_block ... no CIF file name")
      call create_(self%cif,self%CIF_file_name)
      call find_CIF_crystal_data_block_(self,self%cif,found)
      call destroy_(self%cif)
     STOP_TIMER("MOL:find_CIF_crystal_data_block")
      CHECK
   end subroutine

   subroutine find_CIF_crystal_data_block_1(self,cif,found)
    MOL :: self
   ! Find the *first* block of crystal and atom information from the "cif" file
   ! (starting from line cif.start_of_data), and set the locator
   ! ".CIF_data_block_name".  The name of the molecule (job) is set to be the
   ! name of the cif file concatenated with the data block name.  
      CIF :: cif
      BIN, optional, OUT :: found
      STACK("MOL:find_CIF_crystal_data_block_1")
      START_TIMER("MOL:find_CIF_crystal_data_block_1")
      ENSURE(associated(cif%file),"MOL:find_CIF_crystal_data_block_1 ... no CIF file name")
      ENSURE(cif%file%name/=" ","MOL:find_CIF_crystal_data_block_1 ... no CIF file name")
      if (NOT is_open_(cif%file)) call open_(cif)
      call find_crystal_data_block_(cif,found)
      if (present(found)) then
         if (NOT found) then; STOP_TIMER("MOL:find_CIF_crystal_data_block_1") CHECK return; end if
      end if
      self%CIF_data_block_name = cif%data_block_name ! <<<<<<<<<
      self%name = trim(filename_head_(cif%file%name)) // "_" &
           // cif%data_block_name ! Set name to the crystal data name
      WARN("MOL:find_CIF_crystal_data_block_1 ... Job name has now been set to: "//trim(self%name))
     STOP_TIMER("MOL:find_CIF_crystal_data_block_1")
      CHECK
   end subroutine

   subroutine process_CIF(self)
    MOL :: self
   ! Process a CIF file, and try to read crystal and atom information, either
   ! from the current ".CIF_data_block_name", or from the first data block in
   ! the CIF file which is accetable.
      PTR :: self
      STACK("MOL:process_CIF")
      START_TIMER("MOL:process_CIF")
      if (self%CIF_data_block_name==" ") then
         WARN("MOL:process_CIF ... no CIF data block as specified, looking for first acceptable data block")
         call find_CIF_crystal_data_block_(self)
      end if
      call process_CIF_data_block_(self)
     STOP_TIMER("MOL:process_CIF")
      UNSTACK
   end subroutine

   subroutine process_CIF_data_block(self)
    MOL :: self
   ! Read a named block from the .cif file with name .CIF_file_name and from the
   ! data block with name .CIF_data_block_name, and extract crystal and atom
   ! information. NOTE: .cif is created and destroyed in this routine.
      STACK("MOL:process_CIF_data_block")
      START_TIMER("MOL:process_CIF_data_block")
      ENSURE(self%CIF_file_name/=" ","MOL:process_CIF_data_block ... no CIF file name")
      ENSURE(self%CIF_data_block_name/=" ","MOL:process_CIF_data_block ... no CIF data block name")
      ENSURE(NOT associated(self%cif),"MOL:process_CIF_data_block ... the cif file is created")
      call create_(self%cif,self%CIF_file_name)
      call open_(self%cif)
      call find_data_block_(self%cif,self%CIF_data_block_name)
      call process_CIF_data_block_(self,self%cif) 
      call destroy_(self%cif)
     STOP_TIMER("MOL:process_CIF_data_block")
      UNSTACK
   end subroutine

   subroutine process_CIF_data_block_1(self,cif)
    MOL :: self
   ! From the current named data block from the "cif", extract crystal and atom
   ! information. 
      CIF :: cif
      STACK("MOL:process_CIF_data_block_1")
      START_TIMER("MOL:process_CIF_data_block_1")
      ENSURE(cif%data_block_name==self%CIF_data_block_name,"MOL:process_CIF_data_block_1 ... inconsistent CIF data block name")
      ENSURE(cif%start_of_data>0,"MOL:process_CIF_data_block_1 ... no start of data in CIF")
      ENSURE(cif%end_of_data>cif%start_of_data,"MOL:process_CIF_data_block_1 ... no end of data in CIF")
      if (NOT is_open_(cif%file)) call open_(cif)
      call read_crystal_from_CIF_(self,cif)       ! Read crystal info
      call read_atoms_from_CIF_(self,cif)         ! Read in atoms of asymmetric unit
     STOP_TIMER("MOL:process_CIF_data_block_1")
      CHECK
   end subroutine

   subroutine read_crystal_from_CIF(self,cif)
    MOL :: self
   ! Read crystal unitcell and spacegroup information (but NOT atom positions)
   ! from a CIF file called "cif".
      CIF :: cif
      STACK("MOL:read_crystal_from_CIF")
      START_TIMER("MOL:read_crystal_from_CIF")
      call destroy_(self%crystal)
      call create_(self%crystal)
      call read_CIF_(self%crystal,cif)
      if (associated(self%atom)) then
         call resolve_axis_system_(self)
         call make_reduced_group_data_(self%crystal,self%atom)
      end if
     STOP_TIMER("MOL:read_crystal_from_CIF")
      UNSTACK
   end subroutine

   subroutine read_atoms_from_CIF(self,cif)
    MOL :: self
   ! Read atom information from a Crystallographic Information File, "cif".
      CIF :: cif
      STACK("MOL:read_atoms_from_CIF")
      START_TIMER("MOL:read_atoms_from_CIF")
      if (associated(self%atom)) then
         call nullify_basis_part_(self%atom)
         call destroy_(self%atom)
      end if
      call read_CIF_(self%atom,cif)
      self%mult = default_multiplicity_(self)
      call set_atom_info_(self)
      call resolve_basis_info_(self)
      if (associated(self%crystal)) then
         call resolve_axis_system_(self)
         call make_reduced_group_data_(self%crystal,self%atom)
      end if
     STOP_TIMER("MOL:read_atoms_from_CIF")
      UNSTACK
   end subroutine

   subroutine process_CIF_for_CX(self)
    MOL :: self
   ! Process and entire .cif file by reading all acceptable data blocks (i.e.
   ! those with crystal and atom information) and then writing this information
   ! to a crystal explorer information file. 
      PTR :: self
      BIN :: found
      CIF, PTR :: cif
      TEXTFILE, PTR :: CX_file
      STACK("MOL:process_CIF_for_CX")
      START_TIMER("MOL:process_CIF_for_CX")
      ENSURE(self%CX_file_name/=" ","MOL:process_CIF_for_CX ... no CX file name")
      call create_(CX_file,self%CX_file_name)
      call open_(CX_file,for="write")
      call create_(cif,self%CIF_file_name)
      do
         call find_CIF_crystal_data_block_(self,cif,found)
         if (NOT found) exit            ! No more crystal data blocks
         call process_CIF_data_block_(self,cif)   ! Read new crystal, atom info
         call create_(self%cluster,self%crystal,self%atom)
         self%cluster%radius = ZERO       
         self%cluster%add_criteria = "fragment"
         self%cluster%start_with_fragment = TRUE
         self%cluster%defragment = FALSE
         call make_info_(self%cluster)             ! Create the cluster
         call put_(self%cluster)
         call put_CX_data_(self,CX_file)          ! Write out the data
         call destroy_(self)                       ! Destroy self, create from cif
         call create_(self)
         cif%start_of_data = cif%end_of_data  
      end do
      call destroy_(cif)
      call destroy_(CX_file)
     STOP_TIMER("MOL:process_CIF_for_CX")
      UNSTACK
   end subroutine

   subroutine put_CX_data(self)
    MOL :: self
   ! Output information that Crystal Explorer requires to the file 
   ! with name ".CX_file_name".
      TEXTFILE, PTR :: CX_file
   STACK("MOL:put_CX_data")
   START_TIMER("MOL:put_CX_data")
   ENSURE(self%CX_file_name/=" ","MOL:put_CX_data ... no CX file name")
      call create_(CX_file,self%CX_file_name)
      call open_(CX_file,for="write")
      call put_CX_data_(self,CX_file)
      call close_(CX_file)
      call destroy_(CX_file)
     STOP_TIMER("MOL:put_CX_data")
      CHECK
   end subroutine

   subroutine put_CX_data_1(self,CX_file)
    MOL :: self
   ! Output information that Crystal Explorer requires to file "CX_file".
   ! NOTE: this requires a CIF file to have been read, and a valid data block
   ! name to be stored in the cif object.
     TEXTFILE, PTR :: CX_file
     TEXTFILE, PTR :: save
     STR(STR_SIZE) :: CX_label
   STACK("MOL:put_CX_data_1")
   START_TIMER("MOL:put_CX_data_1")
   ENSURE(associated(self%cluster),"MOL:put_CX_data_1 ... no cluster data")
   ENSURE(self%cluster%info_made,"MOL:put_CX_data_1 ... no cluster information")
   ENSURE(associated(self%cluster%crystal),"MOL:put_CX_data_1 ... no cluster crystal data")
   ENSURE(self%cluster%n_fragment_atoms>0,"MOL:put_CX_data_1 ... no fragment size")
   ENSURE(self%CIF_data_block_name/=" ","MOL:put_CX_data_1 ... no CIF data block name")
     CX_label = self%CIF_data_block_name
     call flush_(stdout)
     call text_(stdout,"Writing Crystal Explorer data file: " // trim(CX_file%name))
     call flush_(stdout)
     save   => stdout                   ! Redirect stdout to out
     stdout => CX_file
     call flush_(stdout)
     call text_(stdout,"Crystal Explorer data")
     call show_(stdout,"begin crystal ",CX_label)
     call put_CX_(self%cluster%crystal,CX_label)  ! Crystal data
     call put_CX_(self%cluster,CX_label)          ! Cluster/fragment positions
     call text_(stdout,"end crystal")
     if (associated(self%isosurface)) then
     if (associated(self%isosurface%atom)) then ! Isosurface information
        call put_CX_(self%isosurface,CX_label)    ! &
                               ! ,.cluster.fragment_atom_indices &
                               ! ,.cluster.nonfragment_atom_indices)  
     end if
     end if
     call text_(stdout,"Spartan data")
     call put_spartan_(self%cluster,CX_label)     ! Spartan data
     stdout => save                     ! Put back stdout
     STOP_TIMER("MOL:put_CX_data_1")
      CHECK
   end subroutine

   subroutine make_monomer_MOs(self)
    MOL :: self
   ! Make the monomer molecular orbital matrix from symmetrically orthonormalised
   ! group (monomer) molecular orbitals. The promolecule orbitals are ordered by group.
   ! The orbitals and the corresponding promolecule density are *not* archived.
   ! This will destroy any existing restricted density matrix and MOs.
      REALMAT(:,:), PTR :: S
      STACK("MOL:make_monomer_MOs")
      START_TIMER("MOL:make_monomer_MOs")
      call make_group_density_(self,MOs=TRUE,output=FALSE)
      call create_(S,self%n_bf,self%n_bf)
      call get_overlap_matrix_(self,S)
      call change_basis_(S,self%molecular_orbitals%restricted)
      call text_(stdout,"S in the monomer basis:")
      call put_(stdout,S)
      call destroy_(S)
     STOP_TIMER("MOL:make_monomer_MOs")
      UNSTACK
   end subroutine

   subroutine make_promol_MOs(self)
    MOL :: self
   ! Make the promolecule molecular orbital matrix from symmetrically orthonormalised
   ! group (monomer) molecular orbitals. The promolecule orbitals are ordered by group.
   ! The orbitals and the corresponding density are *not* archived.
   ! This will destroy any existing restricted density matrix and MOs.
      REALMAT(:,:), PTR :: S
      STACK("MOL:make_promol_MOs")
      START_TIMER("MOL:make_promol_MOs")
      call make_group_density_(self,MOs=TRUE,output=FALSE)
      call symorthonormalise_occupied_MOs_(self)
      call create_(S,self%n_bf,self%n_bf)
      call get_overlap_matrix_(self,S)
      call change_basis_(S,self%molecular_orbitals%restricted)
      call text_(stdout,"S in the promolecule monomer basis:")
      call put_(stdout,S)
      call destroy_(S)
     STOP_TIMER("MOL:make_promol_MOs")
      UNSTACK
   end subroutine

!   optimise_geometry
!   ! Optimise the Hartree-Fock SCF energy gradient.
!      k,i :: INT
!      p :: REALVEC*
!      energy :: REAL
!      self :: target
!      p.create(3*.n_atom)
!      k = 0
!      do i = 1,.n_atom
!         p(k+1) = .atom(i).pos(1)
!         p(k+2) = .atom(i).pos(2)
!         p(k+3) = .atom(i).pos(3)
!         k = k + 3
!      end
!      saved_self => self
!      call minimise_BFGS(hf_scf_energy,hf_scf_gradient,p,energy,TOL(3),TOL(3),TOL(1),25)
!      stdout.show("Optimised energy = ",energy)
!      stdout.text("Optimised geometry:")
!      stdout.put(p,format="column")
!      p.destroy
!   end
!
!   hf_scf_energy(p) result (res) ::: selfless
!   ! Evaluates the total HF SCF energy (including nuclear)
!      p :: REALVEC
!     res :: REAL
!     self :: MOL*; self => saved_self
!   ENSURE(.n_atom*3==p.dim,"wrong size for p")
!     if (NOT .atom.geometry_vector.same_as(p,tol=TOL(9))) then
!        .atom.set_geometry_from_vector(p)
!        .delete_scf_integrals
!        .hf_scf
!     end
!     res = .scf_energy
!   end
!
!   the_scf_energy result (res)
!   ! Evaluates the total HF SCF energy (including nuclear)
!     res :: REAL
!     .scfdata.output = FALSE
!     .delete_scf_integrals
!     .hf_scf
!     res = .scf_electronic_energy(.density_matrix)
!     .scfdata.output = TRUE
!   end
!
!   the_e2_energy result (res)
!   ! Evaluates the total HF SCF energy (including nuclear)
!     res :: REAL
!   ! .scfdata.output = FALSE
!   ! .delete_scf_integrals
!   ! .hf_scf
!     .delete_scf_integrals
!     .make_fock_matrix(core=FALSE)
!     res = .scf_electronic_energy(.density_matrix,core=FALSE)
!   ! .scfdata.output = TRUE
!   end
!
!   hf_scf_gradient(p) result (g) ::: selfless
!   !  Evaluate the HF SCF gradient at geometry "g" and return the
!   !  answer as a vector "res"
!       p :: REALVEC
!       g :: REALVEC(size(p))
!      i,k :: INT
!      self :: MOL*; self => saved_self
!   ENSURE(.n_atom*3==p.dim,"wrong size for p")
!      if (NOT .atom.geometry_vector.same_as(p,tol=TOL(9))) then
!        .atom.set_geometry_from_vector(p)
!        .delete_scf_integrals
!        .hf_scf
!      end
!      g = ZERO
!      .add_S_gradient_term(g)
!      .add_T_gradient_term(g)
!      .add_NA_gradient_term(g)
!      .add_2e_gradient_term(g)
!   end
!
!   hf_scf_energy_gradient
!   ! Calculate the Hartree-Fock SCF energy gradient.
!      g :: REALVEC*
!      P :: REALMAT*
!      delta, val :: REAL
!   ENSURE(.density_matrix.restricted.created,"no density matrix")
!      stdin.read(delta)
!      g.create(3*.n_atom)
!      g = ZERO
!      .add_S_gradient_term(g)
!      .make_fd_gradient(S_energy,delta)
!      .add_T_gradient_term(g)
!      .make_fd_gradient(T_energy,delta)
!      .add_NA_gradient_term(g)
!      .make_fd_gradient(NA_energy,delta)
!      .add_2e_gradient_term(g)
!      .make_fd_gradient(e2_energy,delta)
!      .make_fd_gradient(the_e2_energy,delta)
!      stdout.text("Energy gradient:")
!      stdout.put(g,format="column")
!      val = .e2_energy
!      stdout.show("e2_energy     = ",val)
!      val = .the_e2_energy
!      stdout.show("the_e2_energy = ",val)
!      g.destroy
!      .make_fd_gradient(the_scf_energy,delta)
!   end
!
!   energy_gradient(gradient)
!   ! Calculate the Hartree-Fock SCF energy gradient.
!       gradient :: REALVEC
!       g :: REALVEC*
!       P :: REALMAT*
!   ENSURE(.density_matrix.restricted.created,"no density matrix")
!      g.create(3*.n_atom)
!      g = ZERO
!      .add_S_gradient_term(g)
!      .add_T_gradient_term(g)
!      .make_fd_gradient(T_energy)
!      .add_NA_gradient_term(g)
!      .make_fd_gradient(NA_energy)
!      .add_2e_gradient_term(g)
!      .make_fd_gradient(e2_energy)
!      gradient = gradient + g
!      g.destroy
!   end
!
!   make_e_weighted_density_matrix(D) ::: private
!   ! Make the eigenvalue weighted density matrix used in the overlap
!   ! derivative energy gradient term
!     D :: REALMAT
!     X,MO :: REALMAT*
!     eigenvalues :: REALVEC*
!     eig_archive,MO_archive :: ARCHIVE
!     eig_archive.set(.name,"orbital_energies",genre="restricted")
!     MO_archive.set(.name,"molecular_orbitals",genre="restricted")
!   ENSURE(eig_archive.exists,"no eigenvalues")
!   ENSURE(MO_archive.exists,"no molecular orbitals")
!     X.create(.n_bf,.n_bf)
!     MO.create(.n_bf,.n_bf)
!     eigenvalues.create(.n_bf)
!     MO_archive.read(MO)
!     eig_archive.read(eigenvalues)
!     eigenvalues = TWO*eigenvalues
!     eigenvalues(.n_e/2+1:) = ZERO
!     X.to_product_with_diagonal(MO,eigenvalues)
!     D.to_product_of(X,MO,transpose_b=TRUE)
!     eigenvalues.destroy
!     MO.destroy
!     X.destroy
!   end
!
!   add_S_gradient_term(gradient) ::: private
!   ! Make the overlap derivative contribution to the SCF gradient by contracting
!   ! The overlap derivatives with the densitu matrix.
!     gradient :: REALVEC
!     g :: REALVEC*
!     SAx,SAy,SAz, D :: REALMAT*
!     q,fa,la,fb,lb,atom_a,atom_b, ax,ay,az,bx,by,bz :: INT
!     val_x,val_y,val_z :: REAL
!     sh :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.created,  "no atom list")
!     g.create(3*.n_atom); g = ZERO
!     D.create(.n_bf,.n_bf)
!     .make_e_weighted_density_matrix(D) ! includes factor of 2
!     do q=1,.n_shell_pairs
!       .get_shell_pair(sh,q,fa,la,fb,lb,atom_a,atom_b)
!       if (atom_a/=atom_b) then
!       SAx.create(sh.a.n_comp,sh.b.n_comp)
!       SAy.create(sh.a.n_comp,sh.b.n_comp)
!       SAz.create(sh.a.n_comp,sh.b.n_comp)
!       sh.make_S_1st_deriv_ints(SAx,SAy,SAz)
!       val_x = SAx.trace_product_with(D(fb:lb,fa:la))
!       val_y = SAy.trace_product_with(D(fb:lb,fa:la))
!       val_z = SAz.trace_product_with(D(fb:lb,fa:la))
!       SAz.destroy; SAy.destroy; SAx.destroy
!       ax = 3*atom_a - 2; ay = 3*atom_a - 1; az = 3*atom_a
!       bx = 3*atom_b - 2; by = 3*atom_b - 1; bz = 3*atom_b
!       g(ax) = g(ax) + val_x ; g(ay) = g(ay) + val_y ; g(az) = g(az) + val_z
!       g(bx) = g(bx) - val_x ; g(by) = g(by) - val_y ; g(bz) = g(bz) - val_z
!       end
!       sh.destroy_ptr_part
!     end
!     D.destroy
!     stdout.flush
!     stdout.text("S gradient term:")
!     stdout.put(-TWO*g,format="column")
!     gradient = gradient - TWO*g ! factor TWO for upper half of shell pairs
!     g.destroy
!   end
!
!   add_T_gradient_term(gradient) ::: private
!   ! Add the kinetic derivative contribution to the SCF "gradient" by contracting
!   ! The kinetic integral derivatives with the densitu matrix.
!     gradient :: REALVEC
!     g :: REALVEC*
!     TAx,TAy,TAz, P :: REALMAT*
!     q,fa,la,fb,lb,atom_a,atom_b, ax,ay,az,bx,by,bz :: INT
!     val_x,val_y,val_z :: REAL
!     arch :: ARCHIVE
!     sh :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.created,  "no atom list")
!     arch.set(.name,"density_matrix",genre="restricted")
!   ENSURE(arch.exists,"no density")
!     g.create(3*.n_atom); g = ZERO
!     P.create(.n_bf,.n_bf)
!     arch.read(P)
!     do q=1,.n_shell_pairs
!       .get_shell_pair(sh,q,fa,la,fb,lb,atom_a,atom_b)
!       if (atom_a/=atom_b) then
!       TAx.create(sh.a.n_comp,sh.b.n_comp)
!       TAy.create(sh.a.n_comp,sh.b.n_comp)
!       TAz.create(sh.a.n_comp,sh.b.n_comp)
!       sh.make_T_1st_deriv_ints(TAx,TAy,TAz)
!       val_x = TAx.trace_product_with(P(fb:lb,fa:la))
!       val_y = TAy.trace_product_with(P(fb:lb,fa:la))
!       val_z = TAz.trace_product_with(P(fb:lb,fa:la))
!       TAz.destroy; TAy.destroy; TAx.destroy
!       ax = 3*atom_a - 2; ay = 3*atom_a - 1; az = 3*atom_a
!       bx = 3*atom_b - 2; by = 3*atom_b - 1; bz = 3*atom_b
!       g(ax) = g(ax) + val_x ; g(ay) = g(ay) + val_y ; g(az) = g(az) + val_z
!       g(bx) = g(bx) - val_x ; g(by) = g(by) - val_y ; g(bz) = g(bz) - val_z
!       end
!       sh.destroy_ptr_part
!     end
!     P.destroy
!     stdout.flush
!     stdout.text("T gradient:")
!     stdout.put(TWO*g,format="column")
!     gradient = gradient + TWO*g ! factor for upper half of shell pairs
!     g.destroy
!   end
!
!   add_NA_gradient_term(gradient) ::: private
!   ! Add the nuclear attraction derivative contribution to the SCF "gradient" by contracting
!   ! the nuclear attraction integral derivatives with the densitu matrix. This term includes
!   ! the Helmann-Feynman contribution.
!     gradient :: REALVEC
!     g :: REALVEC*
!     NAx,NAy,NAz, NBx,NBy,NBz, P :: REALMAT*
!     q,fa,la,fb,lb,atom_a,atom_b,c, ax,ay,az,bx,by,bz,cx,cy,cz :: INT
!     val_ax,val_ay,val_az, val_bx,val_by,val_bz, val_cx,val_cy,val_cz, Z, fac :: REAL
!     arch :: ARCHIVE
!     sh :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.created,  "no atom list")
!     arch.set(.name,"density_matrix",genre="restricted")
!   ENSURE(arch.exists,"no density")
!     g.create(3*.n_atom); g = ZERO
!     P.create(.n_bf,.n_bf)
!     arch.read(P)
!     do q=1,.n_shell_pairs
!       .get_shell_pair(sh,q,fa,la,fb,lb,atom_a,atom_b)
!       NAx.create(sh.a.n_comp,sh.b.n_comp)
!       NAy.create(sh.a.n_comp,sh.b.n_comp)
!       NAz.create(sh.a.n_comp,sh.b.n_comp)
!       NBx.create(sh.a.n_comp,sh.b.n_comp)
!       NBy.create(sh.a.n_comp,sh.b.n_comp)
!       NBz.create(sh.a.n_comp,sh.b.n_comp)
!       fac = TWO; if (fa==fb) fac = ONE
!       do c=1,.n_atom
!         if (atom_a==c AND atom_b==c) cycle
!         sh.make_NA_1st_deriv_ints(NAx,NAy,NAz,NBx,NBy,NBz,.atom(c).pos)
!         Z = - .atom(c).atomic_number * fac ! for upper half of shell pair
!         val_ax = Z * NAx.trace_product_with(P(fb:lb,fa:la))
!         val_ay = Z * NAy.trace_product_with(P(fb:lb,fa:la))
!         val_az = Z * NAz.trace_product_with(P(fb:lb,fa:la))
!         val_bx = Z * NBx.trace_product_with(P(fb:lb,fa:la))
!         val_by = Z * NBy.trace_product_with(P(fb:lb,fa:la))
!         val_bz = Z * NBz.trace_product_with(P(fb:lb,fa:la))
!         ax = 3*atom_a - 2; ay = 3*atom_a - 1; az = 3*atom_a
!         bx = 3*atom_b - 2; by = 3*atom_b - 1; bz = 3*atom_b
!         cx = 3*c      - 2; cy = 3*c      - 1; cz = 3*c
!         val_cx = -(val_ax + val_bx)
!         val_cy = -(val_ay + val_by)
!         val_cz = -(val_az + val_bz)
!         g(ax) = g(ax) + val_ax ; g(ay) = g(ay) + val_ay ; g(az) = g(az) + val_az
!         g(bx) = g(bx) + val_bx ; g(by) = g(by) + val_by ; g(bz) = g(bz) + val_bz
!         g(cx) = g(cx) + val_cx ; g(cy) = g(cy) + val_cy ; g(cz) = g(cz) + val_cz
!       end
!       NBz.destroy; NBy.destroy; NBx.destroy
!       NAz.destroy; NAy.destroy; NAx.destroy
!       sh.destroy_ptr_part
!     end
!     P.destroy
!     stdout.flush
!     stdout.text("NA gradient:")
!     stdout.put(g,format="column")
!     gradient = gradient + g
!     g.destroy
!   end
!
!   add_2e_gradient_term(gradient) ::: private
!   ! Add the two electron derivative contributions to the "gradient".
!     gradient :: REALVEC
!     g :: REALVEC*
!     P :: REALMAT*
!     max_I,max_P :: REALVEC*
!     sh :: SHELL4
!     AA,BB,CC :: REALMAT5*
!     ab,cd, sa,sb,sc,sd, atom_a,atom_b,atom_c,atom_d :: INT
!     a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz :: INT
!     fac,P_dc,P_db,P_cb,P_ba,P_da,P_ca,cutoff,dens,P_max,IP_max :: REAL
!     AAx,AAy,AAz,BBx,BBy,BBz,CCx,CCy,CCz,DDx,DDy,DDz,SSx,SSy,SSz :: REAL
!     skip :: BIN
!     ab_same,cd_same,ac_same :: BIN
!     arch :: ARCHIVE
!     arch.set(.name,"density_matrix",genre="restricted")
!   ENSURE(arch.exists,"no density")
!     g.create(3*.n_atom); g = ZERO
!     P.create(.n_bf,.n_bf)
!     arch.read(P)
!     cutoff = .scfdata.eri_cutoff
!     max_I.create(.n_shell_pairs)
!     max_P.create(.n_shell_pairs)
!     .make_max_abab_integrals(max_I)
!     .make_max_density_elements(max_P,P)
!     P_max = maxval(max_P)
!     IP_max = maxval(max_I) * P_max
!     do ab = 1, .n_shell_pairs
!       if (max_I(ab)*IP_max < cutoff)  cycle                         ! Rough Schwarz test, but quick.
!       .get_shell_pair_indices(ab,sa,sb,fa,la,fb,lb,atom_a,atom_b)   ! a & b shell indices.
!       ax = 3*atom_a-2; ay = 3*atom_a-1; az = 3*atom_a
!       bx = 3*atom_b-2; by = 3*atom_b-1; bz = 3*atom_b
!       ab_same = atom_a==atom_b
!       .set_shell_quartet_ab(sh,sa,sb)
!       do cd = 1,ab
!         if (max_I(ab)*max_I(cd)*P_max < cutoff)  cycle              ! Rough Schwarz test
!         .get_shell_pair_indices(cd,sc,sd,fc,lc,fd,ld,atom_c,atom_d) ! c & d shell indices.
!         cx = 3*atom_c-2; cy = 3*atom_c-1; cz = 3*atom_c
!         dx = 3*atom_d-2; dy = 3*atom_d-1; dz = 3*atom_d
!         cd_same = atom_c==atom_d
!         ac_same = atom_a==atom_c
!         if (ab_same AND cd_same AND ac_same) cycle ! by translational invariance
!         ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
!         skip = .schwarz_inequality_test(cutoff,ab,cd,sa,sb,sc,sd,max_P,max_I)
!         if (skip) cycle
!         .set_shell_quartet_cd(sh,sc,sd)
!         ! Evaluate the integrals' coincidence factors
!         fac = ONE
!         if (sa==sb)            fac = HALF * fac
!         if (sc==sd)            fac = HALF * fac
!         if (sa==sc AND sb==sd) fac = HALF * fac
!         AA.create(fa,la,fb,lb,fc,lc,fd,ld,1,3)
!         BB.create(fa,la,fb,lb,fc,lc,fd,ld,1,3)
!         CC.create(fa,la,fb,lb,fc,lc,fd,ld,1,3)
!         sh.make_ERI_derivatives(AA,BB,CC)
!         AA = fac*AA; BB = fac*BB; CC = fac*CC
!         do d = fd,ld
!         do c = fc,lc
!           P_dc = P(d,c)
!           do b = fb,lb
!             P_db = P(d,b)
!             P_cb = P(c,b)
!             do a = fa,la
!                P_ba = P(b,a)
!                P_da = P(d,a)
!                P_ca = P(c,a)
!                dens = FOUR*P_ba*P_dc - P_ca*P_db - P_da*P_cb
!                AAx = AA(a,b,c,d,1)*dens; AAy = AA(a,b,c,d,2)*dens; AAz = AA(a,b,c,d,3)*dens
!                BBx = BB(a,b,c,d,1)*dens; BBy = BB(a,b,c,d,2)*dens; BBz = BB(a,b,c,d,3)*dens
!                CCx = CC(a,b,c,d,1)*dens; CCy = CC(a,b,c,d,2)*dens; CCz = CC(a,b,c,d,3)*dens
!                DDx = -(AAx + BBx + CCx)
!                DDy = -(AAy + BBy + CCy)
!                DDz = -(AAz + BBz + CCz)
!                g(ax) = g(ax) + AAx; g(ay) = g(ay) + AAy; g(az) = g(az) + AAz
!                g(bx) = g(bx) + BBx; g(by) = g(by) + BBy; g(bz) = g(bz) + BBz
!                g(cx) = g(cx) + CCx; g(cy) = g(cy) + CCy; g(cz) = g(cz) + CCz
!                g(dx) = g(dx) + DDx; g(dy) = g(dy) + DDy; g(dz) = g(dz) + DDz
!             end
!           end
!         end
!         end
!         CC.destroy
!         BB.destroy
!         AA.destroy
!         sh.destroy_cd
!       end
!       sh.destroy_ab
!     end
!     max_P.destroy
!     max_I.destroy
!     P.destroy
!     stdout.flush
!     stdout.text("2e gradient:")
!     stdout.put(g,format="column")
!     gradient = gradient + g
!     g.destroy
!   end
!
!   make_fd_gradient(func,delta) ::: private
!   ! Make the finite difference gradient "g" of the value of function "func"
!   ! with respect to nuclear perturbations
!      interface
!         func(self) result (res)
!            self :: MOL
!            res :: REAL
!         end
!      end
!      delta :: REAL, optional
!      g,p :: REALVEC*
!      del,e_p,e_m :: REAL
!      a,i,ai :: INT
!      del = TOL(7)
!      if (present(delta)) del = delta
!      g.create(3*.n_atom)
!      p.create(3*.n_atom)
!      .atom.get_geometry_vector(p)
!      do a = 1,.n_atom
!      do i = 1,3
!         ai = 3*(a-1)+i
!         p(ai) = p(ai) + del
!         .atom.set_geometry_from_vector(p)
!         .delete_scf_integrals
!         e_p   = func(self)
!         p(ai) = p(ai) - TWO*del
!         .atom.set_geometry_from_vector(p)
!         .delete_scf_integrals
!         e_m   = func(self)
!         g(ai) = (e_p - e_m)/(TWO*del)
!         p(ai) = p(ai) + del
!    ! stdout.show("   ai =",ai)
!    ! stdout.show("   ep =",e_p)
!    ! stdout.show("   em =",e_m)
!         ! put back geometry
!         .atom.set_geometry_from_vector(p)
!      end
!      end
!      p.destroy
!      stdout.flush
!      stdout.text("Finite difference gradient:")
!      stdout.put(g,format="column")
!      g.destroy
!   end
!
!   S_energy result (res)
!   ! Make the energy weighted S energy
!     res :: REAL
!     D,S :: REALMAT*
!     D.create(.n_bf,.n_bf)
!     S.create(.n_bf,.n_bf)
!     .make_e_weighted_density_matrix(D) ! includes factor of 2
!     .make_overlap_matrix(S)
!     res = - S.trace_product_with(D)
!     S.destroy
!     D.destroy
!   end
!
!   T_energy result (res)
!   ! Make the kinetic energy
!     res :: REAL
!     T :: REALMAT*
!     T.create(.n_bf,.n_bf)
!     .get_kinetic_matrix(T)
!     res = .expectation(T)
!     T.destroy
!   end
!
!   NA_energy result (res)
!   ! Make the electron-nuclear attraction energy
!     res :: REAL
!     Z :: REALMAT*
!     Z.create(.n_bf,.n_bf)
!     .get_nuclear_matrix(Z)
!     res = .expectation(Z)
!     Z.destroy
!   end

   subroutine optimise_orbitals(self)
    MOL :: self
   ! Optimise the Hartree-Fock orbitals directly using Fletcher Powell
      REAL :: energy
      REALVEC(:), PTR :: MO
      target :: self; saved_self => self
      STACK("MOL:optimise_orbitals")
      START_TIMER("MOL:optimise_orbitals")
      call get_initial_guess_(self)
      call shrink_(self%molecular_orbitals%restricted,self%n_bf,self%n_a)
      call create_(MO,self%n_bf*self%n_a)
      MO = reshape(self%molecular_orbitals%restricted,(/self%n_bf*self%n_a/))
#ifndef NOGENERIC
      call minimise_BFGS(the_r_hf_energy,the_r_hf_energy_MO_gradient,MO,energy,TOL(3),TOL(3),step=TOL(2))
#else
      call REALVEC_minimise_BFGS(MOL_the_r_hf_energy,MOL_the_r_hf_energy_MO_gradient,MO,energy,TOL(3),TOL(3),step=TOL(2))
#endif
    ! call minimise_FRPR_(the_r_hf_energy,the_r_hf_energy_MO_gradient,MO,energy,TOL(3),TOL(3),step=TOL(2))
      call show_(stdout,"Optimised energy = ",energy)
      call text_(stdout,"Optimised orbitals:")
      call put_(stdout,MO)
      call destroy_(MO)
     STOP_TIMER("MOL:optimise_orbitals")
      CHECK
   end subroutine

   function the_r_hf_energy(MO) result(res)
   ! Make the restricted Hartree-Fock energy from "MO", a vector representation
   ! of the occupied molecular orbitals. Includes nuclear contribution.
     REALVEC(:) :: MO
     REAL :: res
     MOL, PTR :: self; self => saved_self
     STACK("MOL:the_r_hf_energy")
     START_TIMER("MOL:the_r_hf_energy")
     self%molecular_orbitals%restricted = reshape(MO,(/self%n_bf,self%n_a/))
     call archive_molecular_orbitals_(self)
     call make_non_ortho_scf_density_(self) ! writes density & occupations to disk
     call make_fock_matrix_(self)
     res = scf_energy_(self)
     call show_(stdout,"hf energy =",res)
     STOP_TIMER("MOL:the_r_hf_energy")
      CHECK
   end function

   function r_hf_energy(self,MO) result(res)
    MOL :: self
   ! Make the Hartree-Fock energy without any intermediates.
   ! Includes nuclear contribution.
     REALMAT(:,:) :: MO
     REAL :: res
     REALMAT(:,:), PTR :: P,N
     STACK("MOL:r_hf_energy")
     START_TIMER("MOL:r_hf_energy")
     call create_(P,self%n_bf,self%n_bf)
     call create_(N,self%n_a ,self%n_a)
     call make_r_non_ortho_scf_density_(self,P,MO,N) ! writes density to disk
     call destroy_(N)
     res = e1_energy_(self,P) + e2_energy_(self,P) + nuclear_energy_(self)
     call destroy_(P)
     call show_(stdout,"hf energy =",res)
     STOP_TIMER("MOL:r_hf_energy")
      CHECK
   end function

   function e1_energy(self,P) result(res)
    MOL :: self
   ! Make the one-electron energy directly from the density matrix "P"
   ! without constructing any intermediates
     REALMAT(:,:), target :: P
     REAL :: res
     REALMAT(:,:), PTR :: X,Pp
     INT :: q,fa,la,fb,lb, c
     REAL :: fac,this_res
     SHELL2 :: sh
     STACK("MOL:e1_energy")
     START_TIMER("MOL:e1_energy")
     ENSURE(self%basis_info_made,"MOL:e1_energy ... no basis set")
     ENSURE(associated(self%atom),"MOL:e1_energy ... no atom list")
     ENSURE(size(P,1)==self%n_bf,"MOL:e1_energy ... wrong size, P")
     ENSURE(is_square_(P),"MOL:e1_energy ... wrong shape, P")
     res = ZERO
     do q = 1,self%n_shell_pairs
        call get_shell_pair_(self,sh,q,fa,la,fb,lb)
        fac = TWO
        if (fa==fb) fac = ONE
        call create_(X,fa,la,fb,lb)
        call get_kei_(sh,X)
        Pp => P(fb:lb,fa:la)
        this_res = trace_product_with_(X,Pp)
        do c = 1,self%n_atom
           call get_nuc_(sh,X,mass_(self%atom(c)),self%atom(c)%pos)
           this_res = this_res - self%atom(c)%atomic_number*trace_product_with_(X,Pp)
        end do
        res = res + this_res * fac
        call destroy_(X)
        call destroy_ptr_part_(sh)
     end do
     STOP_TIMER("MOL:e1_energy")
      CHECK
   end function

   function e2_energy(self,P) result(res)
    MOL :: self
   ! Make the two electron electrostatic energy directly from the density
   ! matrix "P" without constructing any intermediates.
     REAL :: res
     REALMAT(:,:) :: P
     REALVEC(:), PTR :: max_I,max_P
     REALMAT4(:,:,:,:), PTR :: I
     SHELL4 :: sh
     INT :: ab,cd, sa,sb,sc,sd, atom_a,atom_b,atom_c,atom_d
     INT :: a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld
     REAL :: fac,P_dc,P_db,P_cb,cutoff,dens,P_max,IP_max
     BIN :: skip
     STACK("MOL:e2_energy")
     START_TIMER("MOL:e2_energy")
     ENSURE(self%basis_info_made,"MOL:e2_energy ... no basis set")
     ENSURE(associated(self%atom),"MOL:e2_energy ... no atom list")
     ENSURE(size(P,1)==self%n_bf,"MOL:e2_energy ... wrong size, P")
     ENSURE(is_square_(P),"MOL:e2_energy ... wrong shape, P")
     cutoff = eri_cutoff_(self%scfdata)
     call create_(max_I,self%n_shell_pairs)
     call create_(max_P,self%n_shell_pairs)
     call make_max_abab_integrals_(self,max_I)
     call make_max_density_elements_(self,max_P,P)
     P_max  = maxval(max_P)
     IP_max = maxval(max_I) * P_max
     res = ZERO
     do ab = 1, self%n_shell_pairs
       if (max_I(ab)*IP_max < cutoff)  cycle                         ! Rough Schwarz test, but quick.
       call get_shell_pair_indices_(self,ab,sa,sb,fa,la,fb,lb,atom_a,atom_b)   ! a & b shell indices.
       call set_shell_quartet_ab_(self,sh,sa,sb)
       do cd = 1,ab
         if (max_I(ab)*max_I(cd)*P_max < cutoff)  cycle              ! Rough Schwarz test
         call get_shell_pair_indices_(self,cd,sc,sd,fc,lc,fd,ld,atom_c,atom_d) ! c & d shell indices.
         ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
         skip = schwarz_inequality_test_(self,cutoff,ab,cd,sa,sb,sc,sd,max_P,max_I)
         if (skip) cycle
         call set_shell_quartet_cd_(self,sh,sc,sd)
         ! Evaluate the integrals' coincidence factors
         fac = ONE
         if (sa==sb)            fac = HALF * fac
         if (sc==sd)            fac = HALF * fac
         if (sa==sc AND sb==sd) fac = HALF * fac
         call create_(I,fa,la,fb,lb,fc,lc,fd,ld)
         call get_ERI_(sh,I)
         do d = fd,ld
         do c = fc,lc
           P_dc = P(d,c)
           do b = fb,lb
             P_db = P(d,b)
             P_cb = P(c,b)
             do a = fa,la
                dens = FOUR*P(b,a)*P_dc - P(c,a)*P_db - P(d,a)*P_cb
                res  = res + fac * I(a,b,c,d) * dens
             end do
           end do
           end do
         end do
         call destroy_(I)
         call destroy_cd_(sh)
       end do
       call destroy_ab_(sh)
     end do
     call destroy_(max_P)
     call destroy_(max_I)
     STOP_TIMER("MOL:e2_energy")
      CHECK
   end function

   function the_r_hf_energy_MO_gradient(MO) result(g)
   ! Evaluate the gradient of the restricted Hartree-Fock energy with
   ! respect to the molecular orbitals, (F - SDF) MO N, where "MO" is
   ! the matrix of the occupied molecular orbital coefficients.
     REALVEC(:) :: MO
!     g  :: REALMAT(saved_self.n_bf,saved_self.n_a)
     REALVEC(size(MO)) :: g
     MOL, PTR :: self; self => saved_self
     STACK("MOL:the_r_hf_energy_MO_gradient")
     START_TIMER("MOL:the_r_hf_energy_MO_gradient")
     g = r_hf_energy_MO_gradient_(self,reshape(MO,(/self%n_bf,self%n_a/)),make_fock=FALSE)
     ! The fock matrix is not made because in the minimiser the gradient is
     ! only ever called after the energy routine. This saves a fock build.
     STOP_TIMER("MOL:the_r_hf_energy_MO_gradient")
      CHECK
   end function

   function r_hf_energy_MO_gradient(self,MO,make_fock) result(g)
    MOL :: self
   ! Evaluate the gradient of the restricted Hartree-Fock energy with
   ! respect to the molecular orbitals, 2(2F - SPF) MO N, where "MO" is
   ! the matrix of the occupied molecular orbital coefficients.
   ! If "make_fock" is present and FALSE, the molecular_orbitals are defined
   ! from "MO", the MO's are archived, and the updated density matrix and
   ! fock matrix are not made but assumed to be pre-existing.
     REALMAT(:,:) :: MO
     BIN, optional :: make_fock
     REALVEC(self%n_bf*self%n_a) :: g
     BIN :: do_fock
     REALMAT(:,:), PTR :: W,X,N,S, F,P,g1
     REAL :: norm
     ARCHIVE :: archive
!     e,e0,ep,em :: REAL
!     b,i :: INT
!     gg :: REALMAT*
     STACK("MOL:r_hf_energy_MO_gradient")
     START_TIMER("MOL:r_hf_energy_MO_gradient")
     ENSURE(self%scfdata%using_MO_gradient_update,"MOL:r_hf_energy_MO_gradient ... not allowed")
     ENSURE(size(MO,2)==self%n_a,"MOL:r_hf_energy_MO_gradient ... wrong dimension 2, MO")
     do_fock = TRUE
     if (present(make_fock)) do_fock = make_fock
     if (do_fock) then ! this assumes that a previous energy was called
        self%molecular_orbitals%restricted = MO
        call archive_molecular_orbitals_(self)
        call make_non_ortho_scf_density_(self) ! writes density & occupations to disk
        call make_fock_matrix_(self)
     end if
     call create_(g1,self%n_bf,self%n_a)
     call create_(X,self%n_bf,self%n_bf)
     call create_(N,self%n_a ,self%n_a )
     call create_(S,self%n_bf,self%n_bf)
     call create_(W,self%n_bf,self%n_bf)
     F => self%fock_matrix%restricted
     P => self%density_matrix%restricted
     call get_overlap_matrix_(self,S)
     call set_(archive,self%name,"non_ortho_occupation_matrix",genre="restricted")
     call to_product_of_(W,S,P)
     X = -TWO*F
     call plus_product_of_(X,W,F)
     call destroy_(W)
     call destroy_(S)
     X = -X
     call create_(S,self%n_bf,self%n_a)
     call read_(archive,N)
     call to_product_of_(S,MO,N)
     call destroy_(N)
     call to_scaled_product_of_(g1,TWO,X,S)
     call destroy_(S)
     call destroy_(X)
     norm = trace_product_with_(g1,transpose(g1))
     call show_(stdout,"hf energy gradient =",norm)
     g = reshape(g1,(/size(g)/))
     call destroy_(g1)
   ! stdout.text("g:")
   ! stdout.put(g)
     !
   ! stdout.text("evaluate finite diff g")
   ! e0 = .r_hf_energy(MO)
   ! gg.create(3,3)
   ! do b = 1,3
   ! do i = 1,3
   !    MO(b,i) = MO(b,i) + 0.0005
   !    ep = .r_hf_energy(MO)
   !    MO(b,i) = MO(b,i) - 0.0005
   !    MO(b,i) = MO(b,i) - 0.0005
   !    em = .r_hf_energy(MO)
   !    gg(b,i) = (ep - em)/0.001
   !    MO(b,i) = MO(b,i) + 0.0005
   ! end
   ! end
   ! stdout.text("finite diff g:")
   ! stdout.put(gg)
   ! gg.destroy
   ! stop
     STOP_TIMER("MOL:r_hf_energy_MO_gradient")
      CHECK
   end function

   subroutine make_non_ortho_scf_density(self)
    MOL :: self
   ! Make the non orthogonal density matrix from the molecular orbitals.
   ! NOTE: the final computed density matrix is written to an archive.
   ! NOTE: if any old density matrix exists, it is saved in an old archive.
   ! NOTE: the non orthogonal occupation matrix i.e. the back tranform of
   ! the inverse of the MO overlap matrix is written to an archive.
     REALMAT(:,:), PTR :: MO,D,N
     STR(STR_SIZE) :: orb_kind
     ARCHIVE :: archive
     STACK("MOL:make_non_ortho_scf_density")
     START_TIMER("MOL:make_non_ortho_scf_density")
     ENSURE(self%basis_info_made,"MOL:make_non_ortho_scf_density ... no basis set")
     ENSURE(associated(self%molecular_orbitals),"MOL:make_non_ortho_scf_density ... no molecular orbitals")
     ! Determine the kind of density matrix to be made
     if (NOT associated(self%scfdata)) then; orb_kind = spinorbital_kind_(self%molecular_orbitals)
     else;                         orb_kind = spinorbital_kind_(self%scfdata)
     end if
     ! Create space for the right kind of density matrix, or save old density matrix
     if (destroyed_(self%density_matrix,orb_kind)) then; call create_(self%density_matrix,orb_kind)
     else;        call archive_density_matrix_(self,archive_name="old_density_matrix")
     end if
     ! Now determine the kind of SCF (if any) associated with the density matrix
     if (NOT associated(self%scfdata)) then; orb_kind = guess_scf_kind_(self%molecular_orbitals)
     else;                         orb_kind = self%scfdata%scf_kind
     end if
     call set_(archive,self%name,"non_ortho_occupation_matrix")
     select case (orb_kind)
       case ("rhf","restricted_hartree_fock","xray_rhf","noninteracting-group-rhf")
         ENSURE(created_(self%molecular_orbitals,"restricted"),"MOL:make_non_ortho_scf_density ... no MO's")
         ENSURE(self%mult==1,"MOL:make_non_ortho_scf_density ... this is not a singlet state")
         MO => self%molecular_orbitals%restricted
         D  => self%density_matrix%restricted
         call create_(N,self%n_a,self%n_a)
         call make_r_non_ortho_scf_density_(self,D,MO,N)
         call set_genre_(archive,"restricted")
         call write_(archive,N)
         call destroy_(N)
       case default
         DIE("MOL:make_non_ortho_scf_density ... unknown SCF kind, "//trim(orb_kind))
     end select
     call archive_density_matrix_(self)
     STOP_TIMER("MOL:make_non_ortho_scf_density")
      UNSTACK
   end subroutine

   subroutine make_r_non_ortho_scf_density(self,P,MO,N)
    MOL :: self
   ! Make the restricted non-orthogonal density matrix "P" from the molecular
   ! orbitals "MO". The cofactor/occupation matrix "N" is also returned if
   ! required. This density includes a factor of two.
     REALMAT(:,:) :: P,MO,N
     REALMAT(:,:), PTR :: S
     STACK("MOL:make_r_non_ortho_scf_density")
     START_TIMER("MOL:make_r_non_ortho_scf_density")
     ENSURE(self%basis_info_made,"MOL:make_r_non_ortho_scf_density ... no basis set")
     ENSURE(is_square_(P),"MOL:make_r_non_ortho_scf_density ... wrong size, P")
     ENSURE(is_square_(N),"MOL:make_r_non_ortho_scf_density ... wrong size, N")
     ENSURE(size(P,1)==self%n_bf,"MOL:make_r_non_ortho_scf_density ... wrong size, P")
     ENSURE(size(N,1)==self%n_a ,"MOL:make_r_non_ortho_scf_density ... wrong size, N")
     ENSURE(self%mult==1,"MOL:make_r_non_ortho_scf_density ... this is not a singlet state")
     ENSURE(size(MO,2)==self%n_a,"MOL:make_r_non_ortho_scf_density ... wrong size, N")
     call create_(S,self%n_bf,self%n_bf)
     call get_overlap_matrix_(self,S)
     call change_basis_(S,N,MO)
    ! stdout.text("N^-1:")
    ! stdout.put(N)
     call to_inverse_of_(N,N)
    ! stdout.text("N:")
    ! stdout.put(N)
     call back_transform_(N,P,MO)
     P = TWO*P
     call destroy_(S)
     STOP_TIMER("MOL:make_r_non_ortho_scf_density")
      UNSTACK
   end subroutine

!   make_r_fock_matrix(F,P,direct,core,r12,test)
!   ! Make a new restricted Fock matrix "F" from the density matrix "P".
!   ! If present and TRUE , "direct" means calculate integrals on the fly
!   ! If present and FALSE, "core" removes the core matrix contribution
!   ! If present and FALSE, "r12"  removes the two electron contribution
!     direct,core,r12,test :: BIN, optional
!     P,F :: REALMAT
!     J,K :: REALMAT*
!     do_direct,add_core,add_r12,do_test :: BIN
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(F.dim1==.n_bf,"wrong shape, F")
!   ENSURE(P.dim1==.n_bf,"wrong shape, P")
!   ENSURE(F.is_square,"wrong shape, P")
!   ENSURE(P.is_square,"wrong shape, P")
!     do_direct= FALSE
!     do_test= FALSE
!     add_core = TRUE
!     add_r12  = TRUE
!     if (present(direct)) do_direct = direct
!     if (present(test))   do_test  = test
!     if (present(core))   add_core = core
!     if (present(r12))    add_r12  = r12
!     if (add_r12) then
!        J.create(.n_bf,.n_bf)
!        K.create(.n_bf,.n_bf)
!        if (do_direct) then; .make_r_JK_direct(J,K,P)
!        else;                .make_r_JK_disk(J,K,P)
!        end
!        F = J - HALF*K
!        K.destroy
!        J.destroy
!     else
!        F = ZERO
!     end
!     if (add_core) .add_core_hamiltonian(F)
!   end
!
!   put_energy_decomposition
!   ! Put out the energy decomposition specified in the .atom_group array
!      T,Z,C,K,P,E_T,E_Z,E_C,E_K :: REALMAT*
!      n_group,i,j,ia,ib,a,b, fa,la,fb,lb :: INT
!      fac :: REAL
!      unit :: STR
!      arch1 :: ARCHIVE
!   ENSURE(.atom_group.created,"no atom group information")
!      arch1.set(.name,"density_matrix,restricted")
!   ENSURE(arch1.exists,"no density matrix")
!      P.create(.n_bf,.n_bf)
!      arch1.read(P)
!      .density_matrix.create("restricted")
!      .density_matrix.restricted = P
!      T.create(.n_bf,.n_bf); .get_kinetic_matrix(T)
!      Z.create(.n_bf,.n_bf); .get_nuclear_matrix(Z)
!      C.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf); .make_r_JK_direct(C,K,P)
!      .make_r_fock(direct=TRUE,core=FALSE,r12=TRUE)
!      n_group = size(.atom_group)
!      E_T.create(n_group,n_group); E_T = ZERO
!      E_Z.create(n_group,n_group); E_Z = ZERO
!      E_C.create(n_group,n_group); E_C = ZERO
!      E_K.create(n_group,n_group); E_K = ZERO
!      do i = 1,n_group
!      do j = 1,n_group
!      do ia = 1,size(.atom_group(i).element)
!      do ib = 1,size(.atom_group(j).element)
!         a = .atom_group(i).element(ia)
!         b = .atom_group(j).element(ib)
!         fa = .first_basis_fn_for_atom(a)
!         la = .last_basis_fn_for_atom(a)
!         fb = .first_basis_fn_for_atom(b)
!         lb = .last_basis_fn_for_atom(b)
!         fac = ONE
!       ! if (i/=j) fac = TWO
!         E_T(i,j) = E_T(i,j) + fac*T(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!         E_Z(i,j) = E_Z(i,j) + fac*Z(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!         E_C(i,j) = E_C(i,j) + fac*C(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!         E_K(i,j) = E_K(i,j) - fac*K(fa:la,fb:lb).trace_product_with(P(fb:lb,fa:la))
!      end
!      end
!      end
!      end
!      E_C = HALF*E_C
!      E_K = HALF*HALF*E_K
!      stdout.text(" ")
!      stdout.text("F:")
!      stdout.put(.fock_matrix.restricted)
!      stdout.text(" ")
!      stdout.text("C-K/2:")
!      stdout.put(C-HALF*K)
!      stdout.text(" ")
!      stdout.text("Energy decomposition in AU ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      stdout.text(" ")
!      stdout.text("Exchange interaction terms")
!      stdout.put(E_K)
!      stdout.text(" ")
!      fac = sum(E_T+E_Z+E_C+E_K)
!      stdout.show("SCF electronic energy =",fac)
!      stdout.show("SCF energy            =",fac+.atom.nuclear_energy)
!      stdout.show("Kinetic energy        =",sum(E_T))
!      !
!      unit = "kcal/mol"
!      fac = unit.conversion_factor
!      E_T = fac*E_T
!      E_Z = fac*E_Z
!      E_C = fac*E_C
!      E_K = fac*E_K
!      stdout.text(" ")
!      stdout.text("Energy decomposition in kcal/mol ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      stdout.text(" ")
!      stdout.text("Exchange interaction terms")
!      stdout.put(E_K)
!      stdout.text(" ")
!      E_K.destroy
!      E_C.destroy
!      E_Z.destroy
!      E_T.destroy
!      K.destroy
!      C.destroy
!      Z.destroy
!      T.destroy
!   end
!
!   put_energy_partition
!   ! Put out the energy decomposition specified in the .atom_group array
!   ! This routine calculates quadricentric contributions which are
!   ! comparable to the Fischer-Kollmar decomposition.
!      T,Z,ZZ,C,K,P,W,E_T,E_Z, EE :: REALMAT*
!      E_ZZ :: REALMAT3*
!      E_C,E_K :: REALMAT4*
!      n_group,x,y,i,j :: INT
!      fac :: REAL
!      unit :: STR
!      arch1 :: ARCHIVE
!      kinetic_energy,nuclear_attraction,nuclear_repulsion :: REAL
!      coulomb_repulsion,net_coulomb,bicentric_exchange :: REAL
!      exchange_attraction,total_interaction :: REAL
!   ENSURE(.atom_group.created,"no atom group information")
!      arch1.set(.name,"density_matrix,restricted")
!   ENSURE(arch1.exists,"no density matrix")
!      P.create(.n_bf,.n_bf)
!      arch1.read(P)
!      W.create(.n_bf,.n_bf)
!      T.create(.n_bf,.n_bf); .get_kinetic_matrix(T)
!      Z.create(.n_bf,.n_bf); .get_nuclear_matrix(Z)
!      n_group = size(.atom_group)
!      E_T.create(n_group,n_group); E_T = ZERO
!      E_Z.create(n_group,n_group); E_Z = ZERO
!      E_C.create(n_group,n_group,n_group,n_group); E_C = ZERO
!      E_K.create(n_group,n_group,n_group,n_group); E_K = ZERO
!      E_ZZ.create(n_group,n_group,n_group); E_ZZ = ZERO
!      ZZ.create(.n_bf,.n_bf)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,P,i,j)
!            if (i/=j) &
!            .atom_group_AO_subspace_set(W,P,j,i)
!            E_T(i,j) = T.trace_product_with(W)
!            E_Z(i,j) = Z.trace_product_with(W)
!            do x = 1,n_group
!               .make_nuclear_matrix(ZZ,.atom_group(x).element)
!               E_ZZ(i,j,x) = ZZ.trace_product_with(W)
!            end
!         end
!         end
!      ZZ.destroy
!      C.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf)
!      do x = 1,n_group
!      do y = 1,x
!         .make_r_JK_group(C,K,P,x,y)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,P,i,j)
!            if (i/=j) &
!            .atom_group_AO_subspace_set(W,P,j,i)
!            E_C(i,j,x,y) =  C.trace_product_with(W)
!            E_K(i,j,x,y) = -K.trace_product_with(W)
!         end
!         end
!      end
!      end
!      K.destroy
!      C.destroy
!      E_C = HALF*E_C
!      E_K = HALF*HALF*E_K
!      EE.create(n_group,n_group)
!      !
!      stdout.text(" ")
!      fac = sum(E_T+E_Z) + sum(E_C+E_K)
!      stdout.show("SCF electronic energy =",fac)
!      stdout.show("SCF energy            =",fac+.atom.nuclear_energy)
!      stdout.show("Kinetic energy        =",sum(E_T))
!!     stdout.text(" ")
!!     stdout.text("Energy decomposition in AU ...")
!!     stdout.text(" ")
!!     stdout.text("Kinetic interaction terms")
!!     stdout.put(E_T)
!!     stdout.text(" ")
!!     stdout.text("Nuclear attraction interaction terms")
!!     stdout.put(E_Z)
!!     stdout.text(" ")
!!     stdout.text("Nuclear attraction interaction terms ONLY for group 1 nuclei")
!!     stdout.put(E_ZZ(:,:,1))
!!     stdout.text(" ")
!!     stdout.text("Nuclear attraction interaction terms ONLY for group 2 nuclei")
!!     stdout.put(E_ZZ(:,:,2))
!!     EE(1,1) = .atom(.atom_group(1).element).nuclear_energy
!!     EE(2,2) = .atom(.atom_group(2).element).nuclear_energy
!!     EE(2,1) = .atom.nuclear_energy - EE(1,1) - EE(2,2)
!!     EE(1,2) = ZERO
!!     stdout.text(" ")
!!     stdout.text("Nuclear nuclear repulsion")
!!     stdout.put(EE)
!!     EE(1,1) = E_C(1,1,1,1)
!!     EE(2,1) = E_C(2,2,1,1)
!!     EE(1,2) = E_C(1,1,2,2)
!!     EE(2,2) = E_C(2,2,2,2)
!!     stdout.text(" ")
!!     stdout.text("Diagonal Coulomb repulsion interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_C(1,2,1,1)
!!     EE(2,1) = E_C(2,1,1,1)
!!     EE(1,2) = E_C(1,2,2,2)
!!     EE(2,2) = E_C(2,1,2,2)
!!     stdout.text(" ")
!!     stdout.text("Semi diagonal Coulomb repulsion interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_C(1,2,1,2)
!!     EE(2,1) = E_C(2,1,1,2)
!!     EE(1,2) = E_C(1,2,2,1)
!!     EE(2,2) = E_C(2,1,2,1)
!!     stdout.text(" ")
!!     stdout.text("Off diagonal Coulomb repulsion interaction terms")
!!     stdout.put(EE)
!!     stdout.text(" ")
!!     stdout.text("All Coulomb repulsion interaction terms")
!!     stdout.put(E_C)
!!     EE(1,1) = E_K(1,1,1,1)
!!     EE(2,1) = E_K(2,2,1,1)
!!     EE(1,2) = E_K(1,1,2,2)
!!     EE(2,2) = E_K(2,2,2,2)
!!     stdout.text(" ")
!!     stdout.text("Diagonal exchange interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_K(1,2,1,1)
!!     EE(2,1) = E_K(2,1,1,1)
!!     EE(1,2) = E_K(1,2,2,2)
!!     EE(2,2) = E_K(2,1,2,2)
!!     stdout.text(" ")
!!     stdout.text("Semi diagonal exchange interaction terms")
!!     stdout.put(EE)
!!     EE(1,1) = E_K(1,2,1,2)
!!     EE(2,1) = E_K(2,1,1,2)
!!     EE(1,2) = E_K(1,2,2,1)
!!     EE(2,2) = E_K(2,1,2,1)
!!     stdout.text(" ")
!!     stdout.text("Off diagonal exchange interaction terms")
!!     stdout.put(EE)
!!     stdout.text(" ")
!!     stdout.text("All exchange interaction terms")
!!     stdout.put(E_K)
!      !
!      unit = "kcal/mol"
!      fac = unit.conversion_factor
!      E_T = fac*E_T
!      E_Z = fac*E_Z
!      E_C = fac*E_C
!      E_K = fac*E_K
!      E_ZZ = fac*E_ZZ
!      stdout.text(" ")
!      stdout.text("Energy decomposition in kcal/mol ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 1 nuclei")
!      stdout.put(E_ZZ(:,:,1))
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 2 nuclei")
!      stdout.put(E_ZZ(:,:,2))
!      EE(1,1) = fac*.atom(.atom_group(1).element).nuclear_energy
!      EE(2,2) = fac*.atom(.atom_group(2).element).nuclear_energy
!      nuclear_repulsion = fac*.atom.nuclear_energy - EE(1,1) - EE(2,2)
!      EE(2,1) = nuclear_repulsion
!      EE(1,2) = ZERO
!      stdout.text(" ")
!      stdout.text("Nuclear nuclear repulsion")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,1,1,1)
!      EE(2,1) = E_C(2,2,1,1)
!      EE(1,2) = E_C(1,1,2,2)
!      EE(2,2) = E_C(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,1)
!      EE(2,1) = E_C(2,1,1,1)
!      EE(1,2) = E_C(1,2,2,2)
!      EE(2,2) = E_C(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,2)
!      EE(2,1) = E_C(2,1,1,2)
!      EE(1,2) = E_C(1,2,2,1)
!      EE(2,2) = E_C(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      EE(1,1) = E_K(1,1,1,1)
!      EE(2,1) = E_K(2,2,1,1)
!      EE(1,2) = E_K(1,1,2,2)
!      EE(2,2) = E_K(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,1)
!      EE(2,1) = E_K(2,1,1,1)
!      EE(1,2) = E_K(1,2,2,2)
!      EE(2,2) = E_K(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,2)
!      EE(2,1) = E_K(2,1,1,2)
!      EE(1,2) = E_K(1,2,2,1)
!      EE(2,2) = E_K(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal exchange interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All exchange interaction terms")
!      stdout.put(E_K)
!      kinetic_energy = E_T(2,1)
!      nuclear_attraction = sum(E_Z) - E_ZZ(1,1,1) - E_ZZ(2,2,2)
!      coulomb_repulsion = sum(E_C) - E_C(1,1,1,1) - E_C(2,2,2,2)
!      net_coulomb = nuclear_attraction + nuclear_repulsion + coulomb_repulsion
!      bicentric_exchange = E_K(1,1,2,2) + E_K(2,2,1,1)
!      exchange_attraction = sum(E_K) - E_K(1,1,1,1) - E_K(2,2,2,2)
!      total_interaction = kinetic_energy + net_coulomb + exchange_attraction
!      stdout.text(" ")
!      stdout.text("Interaction energies for "//trim(.name)//":")
!      stdout.text(" ")
!      stdout.show("Kinetic energy     =",kinetic_energy)
!      stdout.show("Exchange energy    =",exchange_attraction)
!      stdout.show("Kinetic + Exhange  =",kinetic_energy+exchange_attraction)
!      stdout.show("Bicentric Exchange =",bicentric_exchange)
!      stdout.show("Net Coulomb energy =",net_coulomb)
!      stdout.show("Total int. energy  =",total_interaction)
!      EE.destroy
!      E_ZZ.destroy
!      E_K.destroy
!      E_C.destroy
!      E_Z.destroy
!      E_T.destroy
!      Z.destroy
!      T.destroy
!      W.destroy
!      P.destroy
!   end
!
!   put_ortho_energy_partition
!   ! Put out the energy decomposition specified in the .atom_group array
!   ! This routine calculates quadricentric contributions which are
!   ! comparable to the Fischer-Kollmar decomposition in an orthogonalised
!   ! AO basis set
!      T,Z,ZZ,C,K,P,PP,W,E_T,E_Z, S,Sph,Smh, EE :: REALMAT*
!      E_ZZ :: REALMAT3*
!      E_C,E_K :: REALMAT4*
!      n_group,i,j,g,h, fa,la,fb,lb :: INT
!      fac,fac_ij,fac_xy :: REAL
!      unit :: STR
!      arch1 :: ARCHIVE
!      kinetic_energy,nuclear_attraction,nuclear_repulsion :: REAL
!      coulomb_repulsion,net_coulomb,bicentric_exchange :: REAL
!      exchange_attraction,total_interaction :: REAL
!   ENSURE(.atom_group.created,"no atom group information")
!      arch1.set(.name,"density_matrix,restricted")
!   ENSURE(arch1.exists,"no density matrix")
!      P.create(.n_bf,.n_bf)
!      PP.create(.n_bf,.n_bf)
!      arch1.read(P)
!      W.create(.n_bf,.n_bf)
!      T.create(.n_bf,.n_bf); .get_kinetic_matrix(T)
!      Z.create(.n_bf,.n_bf); .get_nuclear_matrix(Z)
!      S.create(.n_bf,.n_bf); .get_overlap_matrix(S)
!      Sph.create(.n_bf,.n_bf); Sph.to_sqrt(S)
!      Smh.create(.n_bf,.n_bf); Smh.to_inverse_sqrt(S)
!      n_group = size(.atom_group)
!      E_T.create(n_group,n_group); E_T = ZERO
!      E_Z.create(n_group,n_group); E_Z = ZERO
!      E_C.create(n_group,n_group,n_group,n_group); E_C = ZERO
!      E_K.create(n_group,n_group,n_group,n_group); E_K = ZERO
!      E_ZZ.create(n_group,n_group,n_group); E_ZZ = ZERO
!      ZZ.create(.n_bf,.n_bf)
!      PP = P
!      PP.change_basis(Sph)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,PP,i,j)
!            if (i/=j) &
!            .atom_group_AO_subspace_set(W,PP,j,i)
!            W.change_basis(Smh)
!            E_T(i,j) = T.trace_product_with(W)
!            E_Z(i,j) = Z.trace_product_with(W)
!            do g = 1,n_group
!               .make_nuclear_matrix(ZZ,.atom_group(g).element)
!               E_ZZ(i,j,g) = ZZ.trace_product_with(W)
!            end
!         end
!         end
!      ZZ.destroy
!      C.create(.n_bf,.n_bf)
!      K.create(.n_bf,.n_bf)
!      do g = 1,n_group
!      do h = 1,g
!         W = ZERO
!         .atom_group_AO_subspace_set(W,PP,g,h)
!         if (g/=h) &
!         .atom_group_AO_subspace_set(W,PP,h,g)
!         W.change_basis(Smh)
!         .make_r_JK_nosym(C,K,W)
!         do i = 1,n_group
!         do j = 1,i
!            W = ZERO
!            .atom_group_AO_subspace_set(W,PP,i,j)
!            if (i/=j) &
!            .atom_group_AO_subspace_set(W,PP,j,i)
!            W.change_basis(Smh)
!            E_C(i,j,g,h) =  C.trace_product_with(W)
!            E_K(i,j,g,h) = -K.trace_product_with(W)
!         end
!         end
!      end
!      end
!      K.destroy
!      C.destroy
!      E_C = HALF*E_C
!      E_K = HALF*HALF*E_K
!      EE.create(n_group,n_group)
!      !
!      stdout.text(" ")
!      fac = sum(E_T+E_Z) + sum(E_C+E_K)
!      stdout.show("SCF electronic energy =",fac)
!      stdout.show("SCF energy            =",fac+.atom.nuclear_energy)
!      stdout.show("Kinetic energy        =",sum(E_T))
!      !
!      unit = "kcal/mol"
!      fac = unit.conversion_factor
!      E_T = fac*E_T
!      E_Z = fac*E_Z
!      E_C = fac*E_C
!      E_K = fac*E_K
!      E_ZZ = fac*E_ZZ
!      stdout.text(" ")
!      stdout.text("Energy decomposition in kcal/mol ...")
!      stdout.text(" ")
!      stdout.text("Kinetic interaction terms")
!      stdout.put(E_T)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms")
!      stdout.put(E_Z)
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 1 nuclei")
!      stdout.put(E_ZZ(:,:,1))
!      stdout.text(" ")
!      stdout.text("Nuclear attraction interaction terms ONLY for group 2 nuclei")
!      stdout.put(E_ZZ(:,:,2))
!      EE(1,1) = fac*.atom(.atom_group(1).element).nuclear_energy
!      EE(2,2) = fac*.atom(.atom_group(2).element).nuclear_energy
!      nuclear_repulsion = fac*.atom.nuclear_energy - EE(1,1) - EE(2,2)
!      EE(2,1) = nuclear_repulsion
!      EE(1,2) = ZERO
!      stdout.text(" ")
!      stdout.text("Nuclear nuclear repulsion")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,1,1,1)
!      EE(2,1) = E_C(2,2,1,1)
!      EE(1,2) = E_C(1,1,2,2)
!      EE(2,2) = E_C(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,1)
!      EE(2,1) = E_C(2,1,1,1)
!      EE(1,2) = E_C(1,2,2,2)
!      EE(2,2) = E_C(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_C(1,2,1,2)
!      EE(2,1) = E_C(2,1,1,2)
!      EE(1,2) = E_C(1,2,2,1)
!      EE(2,2) = E_C(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal Coulomb repulsion interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All Coulomb repulsion interaction terms")
!      stdout.put(E_C)
!      EE(1,1) = E_K(1,1,1,1)
!      EE(2,1) = E_K(2,2,1,1)
!      EE(1,2) = E_K(1,1,2,2)
!      EE(2,2) = E_K(2,2,2,2)
!      stdout.text(" ")
!      stdout.text("Diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,1)
!      EE(2,1) = E_K(2,1,1,1)
!      EE(1,2) = E_K(1,2,2,2)
!      EE(2,2) = E_K(2,1,2,2)
!      stdout.text(" ")
!      stdout.text("Semi diagonal exchange interaction terms")
!      stdout.put(EE)
!      EE(1,1) = E_K(1,2,1,2)
!      EE(2,1) = E_K(2,1,1,2)
!      EE(1,2) = E_K(1,2,2,1)
!      EE(2,2) = E_K(2,1,2,1)
!      stdout.text(" ")
!      stdout.text("Off diagonal exchange interaction terms")
!      stdout.put(EE)
!      stdout.text(" ")
!      stdout.text("All exchange interaction terms")
!      stdout.put(E_K)
!      kinetic_energy = E_T(2,1)
!      nuclear_attraction = sum(E_Z) - E_ZZ(1,1,1) - E_ZZ(2,2,2)
!      coulomb_repulsion = sum(E_C) - E_C(1,1,1,1) - E_C(2,2,2,2)
!      net_coulomb = nuclear_attraction + nuclear_repulsion + coulomb_repulsion
!      bicentric_exchange = E_K(1,1,2,2) + E_K(2,2,1,1)
!      exchange_attraction = sum(E_K) - E_K(1,1,1,1) - E_K(2,2,2,2)
!      total_interaction = kinetic_energy + net_coulomb + exchange_attraction
!      stdout.text(" ")
!      stdout.text("Interaction energies for "//trim(.name)//":")
!      stdout.text(" ")
!      stdout.show("Kinetic energy     =",kinetic_energy)
!      stdout.show("Exchange energy    =",exchange_attraction)
!      stdout.show("Kinetic + Exhange  =",kinetic_energy+exchange_attraction)
!      stdout.show("Bicentric Exchange =",bicentric_exchange)
!      stdout.show("Net Coulomb energy =",net_coulomb)
!      stdout.show("Total int. energy  =",total_interaction)
!      EE.destroy
!      E_ZZ.destroy
!      E_K.destroy
!      E_C.destroy
!      E_Z.destroy
!      E_T.destroy
!      Smh.destroy
!      Sph.destroy
!      S.destroy
!      Z.destroy
!      T.destroy
!      W.destroy
!      PP.destroy
!      P.destroy
!   end
!
!   make_r_JK_group(J,K,P,row_group,col_group)
!   ! Make the real coulomb matrix "J" and exchange matrix "K" matrix from the
!   ! density matrix "P" which is non-zero only for rows belonging to basis
!   ! functions on atoms in group "row_group" and also columns for basis functions
!   ! on atoms in the "col_group", as specified in ".atom_group".
!     J,K,P :: REALMAT
!     row_group,col_group :: INT
!     PP :: REALMAT*
!      I :: REALMAT4*
!     max_I,max_P :: REALVEC*
!     sh4 :: SHELL4
!     ab,cd,i_a,i_b,i_c,i_d :: INT
!     a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     I_abcd,factor,cutoff :: REAL
!     skip :: BIN
!   ENSURE(.atom_group.created,"no atom group information")
!   ENSURE(row_group<=size(.atom_group),"no such row group")
!   ENSURE(col_group<=size(.atom_group),"no such column group")
!   ENSURE(row_group>0,"row group index must be positive")
!   ENSURE(col_group>0,"column group index must be positive")
!     J = ZERO
!     K = ZERO
!     cutoff = SCFDATA_ERI_LIMIT
!     PP.create(.n_bf,.n_bf)
!     PP = ZERO
!     .atom_group_AO_subspace_set(PP,P,row_group,col_group)
!     if (row_group/=col_group) &
!     .atom_group_AO_subspace_set(PP,P,col_group,row_group)
!     max_I.create(.n_shell_pairs)
!     max_P.create(.n_shell_pairs)
!     .make_max_abab_integrals(max_I)
!     .make_max_density_elements(max_P,PP)
!     do ab = 1, .n_shell_pairs
!       .get_shell_pair_indices(ab,i_a,i_b)    ! a & b shell indices.
!       fa = .first_basis_fn_for_shell(i_a)
!       fb = .first_basis_fn_for_shell(i_b)
!       la = .last_basis_fn_for_shell(i_a)
!       lb = .last_basis_fn_for_shell(i_b)
!       .set_shell_quartet_ab(sh4,i_a,i_b)
!       do cd = 1,ab
!         .get_shell_pair_indices(cd,i_c,i_d)  ! c & d shell indices.
!         fc = .first_basis_fn_for_shell(i_c)
!         fd = .first_basis_fn_for_shell(i_d)
!         lc = .last_basis_fn_for_shell(i_c)
!         ld = .last_basis_fn_for_shell(i_d)
!                                        ! ab|cd < sqrt(ab|ab) * sqrt(cd|cd) test.
!         skip = .schwarz_inequality_test(cutoff,ab,cd,i_a,i_b,i_c,i_d,max_P,max_I)
!         if (skip) cycle
!                                              ! calculate ab|cd
!         .set_shell_quartet_cd(sh4,i_c,i_d)
!         factor = ONE                         ! Evaluate the integrals'
!         if (i_a==i_b) factor = HALF          ! coincidence factors
!         if (i_c==i_d) factor = HALF * factor
!         if (i_a==i_c AND i_b==i_d) factor = HALF * factor
!         I.create(fa,la,fb,lb,fc,lc,fd,ld)
!         sh4.get_ERI(I)
!           do d = fd,ld
!           do c = fc,lc
!           do b = fb,lb
!           do a = fa,la
!            I_abcd = factor * I(a,b,c,d)
!            J(a,b) = J(a,b) + I_abcd*PP(d,c) ! These are symmetric
!            J(a,b) = J(a,b) + I_abcd*PP(c,d)
!            J(b,a) = J(b,a) + I_abcd*PP(d,c)
!            J(b,a) = J(b,a) + I_abcd*PP(c,d)
!            J(c,d) = J(c,d) + I_abcd*PP(b,a)
!            J(c,d) = J(c,d) + I_abcd*PP(a,b)
!            J(d,c) = J(d,c) + I_abcd*PP(b,a)
!            J(d,c) = J(d,c) + I_abcd*PP(a,b)
!            K(a,d) = K(a,d) + I_abcd*PP(b,c) ! These are asymmetric
!            K(a,c) = K(a,c) + I_abcd*PP(b,d)
!            K(b,c) = K(b,c) + I_abcd*PP(a,d)
!            K(b,d) = K(b,d) + I_abcd*PP(a,c)
!            K(d,a) = K(d,a) + I_abcd*PP(c,b)
!            K(c,a) = K(c,a) + I_abcd*PP(d,b)
!            K(c,b) = K(c,b) + I_abcd*PP(d,a)
!            K(d,b) = K(d,b) + I_abcd*PP(c,a)
!           end
!           end
!           end
!           end
!         I.destroy
!         sh4.destroy_cd
!       end
!       sh4.destroy_ab
!     end
!     max_P.destroy
!     max_I.destroy
!     PP.destroy
!   end

!   make_pcc_b_field
!   !
!      n_pt,i,j,n, k :: INT
!      aa,bb,cc :: REAL
!      pt :: REALMAT*
!      dma :: CPXMAT3*
!      re,im :: REAL
!      n_pt = 59*75
!    ! n_pt = 25
!      pt.create(n_pt,3)
!      aa=7.4/sqrt(2.0)
!    ! aa=0.4/sqrt(2.0)
!      bb=0.2/sqrt(2.0)
!      n = 0
!      do i=1,75
!         cc=5.4
!    !    cc=0.6
!         do j=1,59
!            n = n+1
!            cc = cc - 0.2
!            pt(n,1)= aa
!            pt(n,2)=-aa
!            pt(n,3)= cc
!         end
!         aa = aa-bb
!      end
!      dma.create(160,160,4)
!      open(unit=99,file='opdens.c',form='formatted',status='old')
!      do i=1,160
!      do j=1,160
!         do k=1,4
!            read(99,*) re,im
!            dma(i,j,k) = cmplx(re,im,kind=CPX_KIND)
!         enddo
!      enddo
!      enddo
!      close(99)
!      .density_matrix.destroy("general_complex")
!      .density_matrix.create(160,"general_complex")
!      .density_matrix.general_complex.alpha_alpha_set_to(dma(:,:,1))
!      .density_matrix.general_complex.alpha_beta_set_to(dma(:,:,2))
!      .density_matrix.general_complex.beta_alpha_set_to(dma(:,:,3))
!      .density_matrix.general_complex.beta_beta_set_to(dma(:,:,4))
!      dma.destroy
!      .make_b_field_grid(pt)
!      pt.destroy
!   end
!
!   make_b_field_grid(pt)
!   ! Make the total b_field "b" on ".grid"
!      pt :: REALMAT
!      archive :: ARCHIVE
!      b,w :: REALMAT*
!      complex :: BIN
!      n_pt :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .density_matrix.created, "no density matrix")
!      complex = .density_matrix.spinorbital_kind == "general_complex"
!   ENSURE(complex, "no general complex density matrix")
!      .make_spin_b_field_grid(pt)
!      .make_current_b_field_grid(pt)
!      n_pt = size(pt,1)
!      b.create(n_pt,3)
!      w.create(n_pt,3)
!      archive.set(.name,"spin_b_field_density_grid")
!      archive.read(b)
!      b = 0
!      archive.set(.name,"current_b_field_density_grid")
!      archive.read(w)
!      b = b + w
!      w.destroy
!      archive.set(.name,"b_field_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1)
!      archive.set(.name,"b_fie7d_norm_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, norm=TRUE)
!      archive.set(.name,"b_field_density_grid,normalised,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, normalise=TRUE)
!      b.destroy
!   end
!
!   make_spin_b_field_grid(pt)
!   ! Make the b_field due to the spin density on ".grid"
!      pt :: REALMAT
!      archive :: ARCHIVE
!       b :: REALMAT*
!      complex :: BIN
!      n_pt :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .density_matrix.created, "no density matrix")
!      complex = .density_matrix.spinorbital_kind == "general_complex"
!   ENSURE(complex, "no general complex density matrix")
!      n_pt = size(pt,1)
!      b.create(n_pt,3)
!      .make_spin_b_field_grid(b,.density_matrix.general_complex,pt)
!      archive.set(.name,"spin_b_field_density_grid")
!      archive.write(b)
!      archive.set(.name,"spin_b_field_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1)
!      archive.set(.name,"spin_b_field_norm_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, norm=TRUE)
!      archive.set(.name,"spin_b_field_density_grid,normalised,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, normalise=TRUE)
!      b.destroy
!   end
!
!   make_spin_b_field_grid(bb,dens,pt)
!   ! Make the b_field "bb" due to the spin density on ".grid" using complex general
!   ! AO density matrix "dens"
!      bb,pt :: REALMAT
!      dens :: CPXMAT, target
!      Daa,Dba,Dbb :: CPXMAT*
!      Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz :: REALMAT*
!      q,a,b,fa,fb,la,lb,k, n_pt :: INT
!      fac :: REAL
!      shell :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.created,  "no atom list")
!      n_pt = size(pt,1)
!      bb = ZERO
!      do q = 1,.n_shell_pairs
!        .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!        Daa => dens(fb:lb,fa:la) ! alpha-alpha block
!        fb = fb + .n_bf
!        lb = lb + .n_bf
!        Dba => dens(fb:lb,fa:la) ! beta-alpha block
!        fa = fa + .n_bf
!        la = la + .n_bf
!        Dbb => dens(fb:lb,fa:la) ! beta-beta block
!        fac = ONE; if (a==b) fac=HALF
!        Mxx.create(shell.a.n_comp,shell.b.n_comp)
!        Mxy.create(shell.a.n_comp,shell.b.n_comp)
!        Mxz.create(shell.a.n_comp,shell.b.n_comp)
!        Myx.create(shell.a.n_comp,shell.b.n_comp)
!        Myy.create(shell.a.n_comp,shell.b.n_comp)
!        Myz.create(shell.a.n_comp,shell.b.n_comp)
!        Mzx.create(shell.a.n_comp,shell.b.n_comp)
!        Mzy.create(shell.a.n_comp,shell.b.n_comp)
!        Mzz.create(shell.a.n_comp,shell.b.n_comp)
!        do k = 1,n_pt
!          shell.make_magnetic_S_ints(Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz,pt(k,:))
!          bb(k,1) = bb(k,1) + Mxx.trace_of_product( real(Dba))
!          bb(k,1) = bb(k,1) + Mxy.trace_of_product(aimag(Dba))
!          bb(k,1) = bb(k,1) + fac*Mxz.trace_of_product(real(Daa))
!          bb(k,1) = bb(k,1) - fac*Mxz.trace_of_product(real(Dbb))
!          bb(k,2) = bb(k,2) + Myx.trace_of_product( real(Dba))
!          bb(k,2) = bb(k,2) - Myy.trace_of_product(aimag(Dba))
!          bb(k,2) = bb(k,2) + fac*Myz.trace_of_product(real(Daa))
!          bb(k,2) = bb(k,2) - fac*Myz.trace_of_product(real(Dbb))
!          bb(k,3) = bb(k,3) + Mzx.trace_of_product( real(Dba))
!          bb(k,3) = bb(k,3) + Mzy.trace_of_product(aimag(Dba))
!          bb(k,3) = bb(k,3) + fac*Mzz.trace_of_product(real(Daa))
!          bb(k,3) = bb(k,3) - fac*Mzz.trace_of_product(real(Dbb))
!        end
!        Mzz.destroy; Mzy.destroy; Mzx.destroy
!        Myz.destroy; Myy.destroy; Myx.destroy
!        Mxz.destroy; Mxy.destroy; Mxx.destroy
!        shell.destroy_ptr_part
!      end
!      bb.zero_small_values(TOL(10))
!      fac = -G_FACTOR*HALF
!      bb = fac*bb
!   end
!
!   make_current_b_field_grid(pt)
!   ! Make the b_field due to the current density on ".grid"
!      pt :: REALMAT
!      archive :: ARCHIVE
!       b :: REALMAT*
!      complex :: BIN
!      n_pt :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.density_matrix.created, "no density matrix")
!      .make_ao_density_matrix
!      complex = .density_matrix.spinorbital_kind.includes("complex")
!   ENSURE(complex, "no complex density matrix")
!      n_pt = size(pt,1)
!      b.create(n_pt,3)
!      .make_current_b_field_grid(b,.density_matrix.restricted_complex,pt)
!      archive.set(.name,"current_b_field_density_grid")
!      archive.write(b)
!      archive.set(.name,"current_b_field_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1)
!      archive.set(.name,"current_b_field_norm_density_grid,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, norm=TRUE)
!      archive.set(.name,"current_b_field_density_grid,normalised,gnuplot",format="ascii")
!      archive.write_gnuplot(b,75,59, 1, normalise=TRUE)
!      b.destroy
!   end
!
!   make_current_b_field_grid(bb,dens,pt)
!   ! Make the magnetic field B field "bb" due to the paramagnetic current
!   ! density at a set of points "pt" using a complex AO density matrix "dens"
!      bb,pt :: REALMAT
!      dens :: CPXMAT, target
!      Dba :: CPXMAT*
!      Jx,Jy,Jz :: REALMAT*
!      q,a,b,fa,fb,la,lb,k, n_pt :: INT
!      fac :: REAL
!      shell :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.created,  "no atom list")
!      n_pt = size(pt,1)
!      bb = ZERO
!      do q = 1,.n_shell_pairs
!        .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!        Dba => dens(fb:lb,fa:la)
!        fac = ONE; if (a==b) fac=HALF
!        Jx.create(shell.a.n_comp,shell.b.n_comp)
!        Jy.create(shell.a.n_comp,shell.b.n_comp)
!        Jz.create(shell.a.n_comp,shell.b.n_comp)
!        do k = 1,n_pt
!          shell.make_magnetic_jp_ints(Jx,Jy,Jz,pt(k,:))
!          bb(k,1) = bb(k,1) + Jx.trace_of_product(aimag(Dba))
!          bb(k,2) = bb(k,2) + Jy.trace_of_product(aimag(Dba))
!          bb(k,3) = bb(k,3) + Jz.trace_of_product(aimag(Dba))
!        end
!        Jz.destroy; Jy.destroy; Jx.destroy
!        shell.destroy_ptr_part
!      end
!   !  bb.zero_small_values(TOL(10))
!      bb = -HALF*bb
!   end
!
!   make_spin_b_field_grid(bb,pt,Dx,Dy,Dz)
!   ! Make the magnetic B field "bb" due to the spin density at a set of points
!   ! given in "pt" using three real symmetric AO density matrices "Dx", "Dy" and
!   ! "Dz" corresponding to the Sx, Sy and Sz densities. Note: the contribution
!   ! is added to whatever is already in bb.
!      bb,pt :: REALMAT
!      Dx,Dy,Dz :: REALMAT, target
!      DD :: REALMAT3*
!      M :: REALMAT4*
!      Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz :: REALMAT*
!      q,a,b,fa,fb,la,lb,n, n_pt, i,j,k :: INT
!      eps :: REALMAT3(3,3,3)
!      fac :: REAL
!      shell :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.created,"no atom list")
!   ENSURE(pt.dim2==3,"no basis set")
!   ENSURE(Dx.dim1==.n_bf AND Dx.is_square,"wrong shape for Dx")
!   ENSURE(Dy.dim1==.n_bf AND Dy.is_square,"wrong shape for Dy")
!   ENSURE(Dz.dim1==.n_bf AND Dz.is_square,"wrong shape for Dz")
!      n_pt = size(pt,1)
!      bb = ZERO
!      do q = 1,.n_shell_pairs
!        .get_shell_pair(shell,q,a,b,fa,la,fb,lb)
!        DD.create(shell.a.n_comp,shell.b.n_comp,3)
!        DD(:,:,1) = Dx(fb:lb,fa:la)
!        DD(:,:,2) = Dy(fb:lb,fa:la)
!        DD(:,:,3) = Dz(fb:lb,fa:la)
!        fac = ONE; if (fa==fb) fac=HALF
!        M.create(shell.a.n_comp,shell.b.n_comp,3,3)
!        Mxx => M(:,:,1,1); Mxy => M(:,:,1,2); Mxz => M(:,:,1,3)
!        Myx => M(:,:,2,1); Myy => M(:,:,2,2); Myz => M(:,:,2,3)
!        Mzx => M(:,:,3,1); Mzy => M(:,:,3,2); Mzz => M(:,:,3,3)
!        do n = 1,n_pt
!          shell.make_magnetic_S_ints(Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz,pt(n,:))
!          do i = 1,3
!          do j = 1,3
!          do k = 1,3
!             eps(i,j,k) = M(:,:,i,j).trace_product_with(DD(:,:,k))
!          end
!          end
!          end
!          bb(n,1) = bb(n,1) + eps(:,:,1).trace - eps(1,:,:).trace
!          bb(n,2) = bb(n,2) + eps(:,:,2).trace - eps(2,:,:).trace
!          bb(n,3) = bb(n,3) + eps(:,:,3).trace - eps(3,:,:).trace
!        end
!        M.destroy
!        DD.destroy
!        shell.destroy_ptr_part
!      end
!   !  bb.zero_small_values(TOL(10))
!      bb = -bb ! this is -g_e mu_b; the final answer is in units of mu_0/4pi
!   end
!
!   make_pcc_structure_factors
!   !
!       S :: REALMAT*
!      mo,Smo :: REALMAT*
!      in :: TEXTFILE*
!      k_pts :: REALMAT*
!      ftnew,ft :: CPXMAT3*
!       W :: CPXMAT*
!      ftnew_nabla,ft_nabla :: CPXMAT4*
!      shell :: SHELL2*
!      n_refl,q,i,j :: INT
!      fa,la,fb,lb,a,b :: INT
!      fac :: REAL
!      mo.create(160,4)
!      in.create("mo35")
!      in.open(for="read")
!      in.read(mo,order="by_column")
!      stdout.text("mo")
!      stdout.put(mo)
!
!      S.create(160,160)
!      .get_overlap_matrix(S)
!
!      Smo.create(4,4)
!      Smo.create(4,4)
!      S.change_basis(Smo,mo)
!      stdout.text("Smo")
!      stdout.put(Smo)
!
!      n_refl = .crystal%n_refl
!
!      k_pts.create(n_refl,3)
!      .crystal.make_k_pts(k_pts)
!
!      shell.create
!      ftnew.create(4,4,1)
!      ftnew = ZERO
!      do q = 1,.n_shell_pairs
!          .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!          ft.create(n_refl,shell%a%n_comp,shell%b%n_comp)
!          shell.make_ft(ft,k_pts)
!          fac = ONE
!          if (a==b) fac = HALF
!          do i = 1,n_refl
!             ftnew(:,:,i) = ftnew(:,:,i) + &
!                fac*matmul( transpose(mo(fa:la,:)), matmul(ft(i,:,:),mo(fb:lb,:))) + &
!                fac* transpose( matmul( transpose(mo(fa:la,:)), matmul(ft(i,:,:),mo(fb:lb,:))) )
!          end
!          ft.destroy
!          shell.destroy_ptr_part
!      end
!
!      do i = 1,n_refl
!         stdout.put(ftnew(:,:,i))
!      end
!      ftnew.destroy
!
!      ftnew_nabla.create(4,4,3,1)
!      ftnew_nabla = ZERO
!      do q = 1,.n_shell_pairs
!          .get_shell_pair(q,shell,a,b,fa,la,fb,lb)
!          ft_nabla.create(n_refl,shell%a%n_comp,shell%b%n_comp,3)
!          shell.make_ft_nabla(ft_nabla,k_pts)
!          fac = ONE
!          if (a==b) fac = HALF
!          do i = 1,n_refl
!          do j = 1,3
!             ftnew_nabla(:,:,j,i) = ftnew_nabla(:,:,j,i) + &
!                fac*matmul( transpose(mo(fa:la,:)), matmul(ft_nabla(i,:,:,j),mo(fb:lb,:))) - &
!                fac* transpose( matmul( transpose(mo(fa:la,:)), matmul(ft_nabla(i,:,:,j),mo(fb:lb,:))) )
!          end
!          end
!          ft_nabla.destroy
!          shell.destroy_ptr_part
!      end
!
!      do i = 1,n_refl
!      do j = 1,3
!          stdout.show("component =",j)
!          stdout.put(ftnew_nabla(:,:,j,i))
!      end
!      end
!      ftnew_nabla.destroy
!      shell.destroy
!   end
!
!   make_pcc_L_matrices
!   !
!       S :: REALMAT*
!      mo,Smo :: REALMAT*
!      Lx,Ly,Lz :: REALMAT*
!      Lxmo,Lymo,Lzmo :: REALMAT*
!      in :: TEXTFILE*
!      mo.create(160,4)
!      in.create("mo35")
!      in.open(for="read")
!      in.read(mo,order="by_column")
!      stdout.text("mo")
!      stdout.put(mo)
!
!      S.create(160,160)
!      .get_overlap_matrix(S)
!
!      Smo.create(4,4)
!      Smo.create(4,4)
!      S.change_basis(Smo,mo)
!      stdout.text("Smo")
!      stdout.put(Smo)
!
!      Lx.create(.n_bf,.n_bf); Lx = ZERO
!      Ly.create(.n_bf,.n_bf); Ly = ZERO
!      Lz.create(.n_bf,.n_bf); Lz = ZERO
!      .make_L_matrices(Lx,Ly,Lz)
!
!    ! std_output.text("Lx")
!    ! std_output.put(Lx)
!    ! std_output.text("Ly")
!    ! std_output.put(Ly)
!    ! std_output.text("Lz")
!    ! std_output.put(Lz)
!
!      Lxmo.create(4,4)
!      Lymo.create(4,4)
!      Lzmo.create(4,4)
!      Lx.change_basis(Lxmo,mo)
!      Ly.change_basis(Lymo,mo)
!      Lz.change_basis(Lzmo,mo)
!
!      stdout.text("Lxmo")
!      stdout.put(Lxmo)
!      stdout.text("Lymo")
!      stdout.put(Lymo)
!      stdout.text("Lzmo")
!      stdout.put(Lzmo)
!      S.destroy
!      Smo.destroy
!      Lxmo.destroy; Lymo.destroy; Lzmo.destroy
!      Lx.destroy; Ly.destroy; Lz.destroy
!   end
!
!   test_ZORA_SO_matrices
!   !
!      SOx,SOy,SOz :: REALMAT*
!      ZOx,ZOy,ZOz, T :: REALMAT*
!      T.create(.n_bf,.n_bf)
!      SOx.create(.n_bf,.n_bf); SOy.create(.n_bf,.n_bf); SOz.create(.n_bf,.n_bf)
!      ZOx.create(.n_bf,.n_bf); ZOy.create(.n_bf,.n_bf); ZOz.create(.n_bf,.n_bf)
!      .get_nuclear_matrix(SOz)
!      .make_ENA_matrix(ZOz)
!      stdout.text("NA"); stdout.put(SOz)
!      stdout.text("numerical NA"); stdout.put(ZOz)
!      .get_spin_orbit_matrices(SOx,SOy,SOz)
!      .get_1e_ZORA_matrices(T,ZOx,ZOy,ZOz)
!      stdout.text("SOx"); stdout.put(SOx)
!      stdout.text("ZOx"); stdout.put(ZOx)
!      stdout.text("SOy"); stdout.put(SOy)
!      stdout.text("ZOy"); stdout.put(ZOy)
!      stdout.text("SOz"); stdout.put(SOz)
!      stdout.text("ZOz"); stdout.put(ZOz)
!      SOx.destroy; SOy.destroy; SOz.destroy
!      ZOx.destroy; ZOy.destroy; ZOz.destroy
!      T.destroy
!   end
!
!   test_dftgrid
!   !
!      Z,NZ,DZ :: REALMAT*
!      Z.create(.n_bf,.n_bf)
!      NZ.create(.n_bf,.n_bf)
!      DZ.create(.n_bf,.n_bf)
!      .make_nuclear_matrix(DZ,output=TRUE)
!      .make_nuclear_ints(Z)
!      .get_ENA_matrix(NZ)
!      stdout.text("DZ"); stdout.put(DZ)
!      stdout.text("Z"); stdout.put(Z)
!      stdout.text("NZ"); stdout.put(NZ)
!      .test_num
!      Z.destroy; NZ.destroy; DZ.destroy
!   end
!
!   make_nuclear_ints(Z,output)
!   ! Calculate the nuclear attraction matrix "Z"
!   ! if output is present and FALSE then do not make output archive
!      Z :: REALMAT
!     output :: BIN, OPTIONAL
!     do_output :: BIN
!     atom :: ATOM*
!     Z_c :: REALMAT*
!     q,c,fa,la,fb,lb :: INT
!     shell :: SHELL2
!     archive :: ARCHIVE
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.created,  "no atom list")
!     do_output=TRUE
!     if (present(output)) do_output=output
!     Z = ZERO
!       do q=1,.n_shell_pairs
!         .get_shell_pair(shell,q,fa,la,fb,lb)
!         Z_c.create(shell.a.n_comp,shell.b.n_comp)
!         do c=1,.n_atom
!           atom=>.atom(c)
!         ! shell.get_nuc(Z_c, atom.mass,atom.pos)
!           shell.make_nuclear_attraction_ints(Z_c, atom.pos)
!           Z(fa:la,fb:lb) = Z(fa:la,fb:lb) - atom.atomic_number * Z_c
!         end
!         Z_c.destroy
!         shell.destroy_ptr_part
!       end
!     Z.make_symmetric
!     if (do_output) then
!       archive.set(.name,"nuc_matrix")
!       archive.write(Z)
!     end
!   end
!
!   test_num
!   !
!     ZZ :: REALMAT*
!     q,fa,la,fb,lb :: INT
!     shell :: SHELL2
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.created,  "no atom list")
!   ENSURE( .dftgrid.created, "need to specify dftgrid for ZORA")
!        q = 529
!        .get_shell_pair(shell,q,fa,la,fb,lb)
!        write(*,*) "fa=",fa,"la=",la,"fb=",fb,"lb=",lb
!        ZZ.create(shell.a.n_comp,shell.b.n_comp)
!        .dftgrid.make_matrix_elements_of(nuc_pot,self,shell,ZZ)
!        stdout.text("ZZ")
!        stdout.put(ZZ)
!        ZZ.destroy
!        shell.destroy_ptr_part
!   end
!
!   nuc_pot(values,pts)
!   ! Calculate the nuclear potential "values" of a given set of "pts".
!   ! This is usefule for numerical integration of nuclear attraction integrals.
!      values :: REALVEC
!      pts :: REALMAT
!      n_pts,n,i :: INT
!      Z_n,r_ni :: REAL
!      pos :: REALVEC(3)
!      n_pts = size(pts,1)
!      values = ZERO
!      do n = 1,.n_atom
!         Z_n = .atom(n).atomic_number
!         pos = .atom(n).pos
!         do i = 1,n_pts
!            r_ni = pos.distance_to(pts(i,:))
!            values(i) = values(i) + Z_n/r_ni
!         end
!      end
!   end
!
!   get_ENA_matrix(Z)
!   ! Set "Z" to the numerically calculated electron nuclear attraction matrix.
!   ! If archives exist, read them; otherwise make them.
!       Z :: REALMAT
!      archive :: ARCHIVE
!      archive.set(.name,"ENA_matrix")
!      if (archive.exists) then
!         archive.read(Z)
!      else
!         .make_ENA_matrix(Z)
!      end
!   end
!
!   test_spin_orbit_B_matrices
!   !
!      Lx,Ly,Lz :: REALMAT*
!      fac :: REAL
!      fac = (TWO*EIGHT*SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)/G_FACTOR
!      Lx.create(.n_bf,.n_bf)
!      Ly.create(.n_bf,.n_bf)
!      Lz.create(.n_bf,.n_bf)
!      .make_spin_orbit_B_matrices(Lx,Ly,Lz)
!      stdout.text("x")
!      stdout.put( Lx)
!      stdout.text("y")
!      stdout.put( Ly)
!      stdout.text("z")
!      stdout.put( Lz)
!      .make_SOB_matrices(Lx,Ly,Lz)
!      stdout.text("x")
!      stdout.put( Lx)
!      stdout.text("y")
!      stdout.put( Ly)
!      stdout.text("z")
!      stdout.put( Lz)
!      Lx.destroy; Ly.destroy; Lz.destroy
!   end
!
!   make_SOB_matrices(SOBx,SOBy,SOBz)
!   ! Calculate the gauge modified (B field) spin orbit matrices "SOBx" "SOBy" and "SOBz"
!      SOBx,SOBy,SOBz :: REALMAT
!      Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz,Qx,Qy,Qz,Qss :: REALMAT*
!      q,c,a,b,fa,la,fb,lb,atom_a,atom_b :: INT
!      fac,Z :: REAL
!      shell :: SHELL2
!      archive :: ARCHIVE
!      debug :: BIN
!      n_a,n_b :: INT
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.created,  "no atom list")
!      fac = G_FACTOR/(TWO*EIGHT*(SPEED_OF_LIGHT_AU)**2)
!      SOBx = ZERO; SOBy = ZERO; SOBz = ZERO
!      do q=1,.n_shell_pairs
!         .get_shell_pair(q,shell,fa,la,fb,lb)
!         n_a = shell%a%n_comp; n_b = shell%b%n_comp
!         Qss.create(n_a,n_b)
!         Qx.create(n_a,n_b);  Qy.create(n_a,n_b);  Qz.create(n_a,n_b)
!         Qxx.create(n_a,n_b); Qxy.create(n_a,n_b); Qxz.create(n_a,n_b)
!         Qyx.create(n_a,n_b); Qyy.create(n_a,n_b); Qyz.create(n_a,n_b)
!         Qzx.create(n_a,n_b); Qzy.create(n_a,n_b); Qzz.create(n_a,n_b)
!         do c = 1,.n_atom
!            Z = .atom(c)%atomic_number
!            shell.make_spin_orbit_B_ints(Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz, .atom(c)%pos, .gauge_origin)
!            Qss = Qxx + Qyy + Qzz
!            Qx = Qss*.B_field(1) - Qxx*.B_field(1) - Qxy*.B_field(2) - Qxz*.B_field(3)
!            Qy = Qss*.B_field(2) - Qyx*.B_field(1) - Qyy*.B_field(2) - Qyz*.B_field(3)
!            Qz = Qss*.B_field(3) - Qzx*.B_field(1) - Qzy*.B_field(2) - Qzz*.B_field(3)
!            SOBx(fa:la,fb:lb) = SOBx(fa:la,fb:lb) - Z*Qx
!            SOBy(fa:la,fb:lb) = SOBy(fa:la,fb:lb) - Z*Qy
!            SOBz(fa:la,fb:lb) = SOBz(fa:la,fb:lb) - Z*Qz
!         end
!         shell.destroy_ptr_part
!         Qzz.destroy; Qzy.destroy; Qzx.destroy
!         Qyz.destroy; Qyy.destroy; Qyx.destroy
!         Qxz.destroy; Qxy.destroy; Qxx.destroy
!         Qz.destroy;  Qy.destroy;  Qx.destroy;  Qss.destroy
!      end
!      SOBx.make_symmetric
!      SOBy.make_symmetric
!      SOBz.make_symmetric
!    ! archive.set(.name,"SOB_x_matrix"); archive.write(SOBx)
!    ! archive.set(.name,"SOB_y_matrix"); archive.write(SOBy)
!    ! archive.set(.name,"SOB_z_matrix"); archive.write(SOBz)
!   end
!
!   make_pcc_densities
!   ! Calculate pcc's densities for the paper
!      mo :: REALMAT*
!      mo35,mo48,mo49,mo50 :: REALVEC*
!      p35,p48,p49,p50 :: REALVEC*
!      g35,g48,g49,g50 :: REALMAT*
!      j,s :: REALMAT*
!      in :: TEXTFILE*
!      n_pt,k :: INT
!      mu,nu,rt3,fac :: REAL
!      archive :: ARCHIVE
!
!      mo35.create(160)
!      mo48.create(160)
!      mo49.create(160)
!      mo50.create(160)
!      in.create("mo35")
!      in.open(for="read")
!      in.read(mo35)
!      in.read(mo48)
!      in.read(mo49)
!      in.read(mo50)
!
!      n_pt = .grid%n_pt
!      p35.create(n_pt); g35.create(n_pt,3)
!      p48.create(n_pt); g48.create(n_pt,3)
!      p49.create(n_pt); g49.create(n_pt,3)
!      p50.create(n_pt); g50.create(n_pt,3)
!      .make_orbital_grid(p35,mo35)
!      .make_orbital_grid(p48,mo48)
!      .make_orbital_grid(p49,mo49)
!      .make_orbital_grid(p50,mo50)
!      .make_nabla_orbital_grid(g35,mo35)
!      .make_nabla_orbital_grid(g48,mo48)
!      .make_nabla_orbital_grid(g49,mo49)
!      .make_nabla_orbital_grid(g50,mo50)
!
!      j.create(n_pt,3)
!      s.create(n_pt,3)
!      nu = 0.15578d0
!      mu = 0.19107d0
!      rt3 = ONE/sqrt(THREE)
!      fac = ONE/(ONE+mu*mu+nu*nu)
!      do k = 1,3
!         j(:,k) =      nu*(p35(:)*g48(:,k) - p48(:)*g35(:,k)) &
!                + HALF*mu*(p50(:)*g49(:,k) - p49(:)*g50(:,k))
!      end
!      j = fac*j
!
!      s(:,1) = -rt3*mu * (p35(:)*p49(:) - nu*p48(:)*p50(:))
!      s(:,2) = -rt3*mu * (p35(:)*p50(:) - nu*p48(:)*p49(:))
!      s(:,3) = (ONE+mu*mu/THREE) * p48(:)*p48(:) &
!             + (ONE+nu*nu+mu*mu/SIX) * (p49(:)*p49(:)+p50(:)*p50(:)) &
!             + (nu*nu+mu*mu/THREE) * p35(:)*p35(:)
!      s(:,3) = HALF*s(:,3)
!
!      archive.set(.name,"current_density")
!      archive.write(j)
!      archive.set(.name,"current_density,gnuplot",format="ascii")
!      archive.write_gnuplot(j, .grid%n_x, .grid%n_y, .grid%n_z)
!      archive.set(.name,"current_norm_density,gnuplot",format="ascii")
!      archive.write_gnuplot(j, .grid%n_x, .grid%n_y, .grid%n_z, norm=TRUE)
!      archive.set(.name,"current_density,normalised,gnuplot",format="ascii")
!      archive.write_gnuplot(j, .grid%n_x, .grid%n_y, .grid%n_z, normalise=TRUE)
!
!      archive.set(.name,"spin_density")
!      archive.write(s)
!      archive.set(.name,"spin_density,gnuplot",format="ascii")
!      archive.write_gnuplot(s, .grid%n_x, .grid%n_y, .grid%n_z)
!      archive.set(.name,"spin_norm_density,gnuplot",format="ascii")
!      archive.write_gnuplot(s, .grid%n_x, .grid%n_y, .grid%n_z, norm=TRUE)
!      archive.set(.name,"spin_density,normalised,gnuplot",format="ascii")
!      archive.write_gnuplot(s, .grid%n_x, .grid%n_y, .grid%n_z, normalise=TRUE)
!
!      s.destroy
!      j.destroy
!      g50.destroy; p50.destroy
!      g49.destroy; p49.destroy
!      g48.destroy; p48.destroy
!      g35.destroy; p35.destroy
!      in.close
!      in.destroy
!      mo50.destroy
!      mo49.destroy
!      mo48.destroy
!      mo35.destroy
!   end
!
!   integrate_property(make_property_grid) result (res)
!   ! Integrate a scalar molecular property, which is represented by a subroutine
!   ! "make_property_grid" which returns "values" of the property in a vector,
!   ! given "mol" as the molecule, and "pts" as a set of points. The result of
!   ! the integration is "res".
!      interface
!         make_property_grid(mol,values,pts)
!            mol :: MOL
!            values :: REALVEC
!            pts :: REALMAT
!         end
!      end
!      res :: REAL
!   ENSURE( .dftgrid.created,"no DFT grid information!")
!!      res = .dftgrid.integrate_molecular_property(make_property_grid,self)
!      res = ZERO
!   end
!
!   integrate_rho_numerically
!   ! Integrate the density numerically
!      res :: REAL
!      res = .integrate_property(make_density_grid_1)
!      stdout.show("numerically integrated charge =",res)
!   end
!
!   put_atom_kind_map
!   ! Check the atom kind map routine
!      res :: REAL
!      atom_kind,unique_atom :: INTVEC*
!      n_atom,n_kind :: INT
!      n_atom = .n_atom
!      atom_kind.create(n_atom)
!      .atom.make_atom_kind_list(atom_kind,n_kind)
!      stdout.text("atom kind")
!      stdout.put(atom_kind,"column")
!      unique_atom.create(n_kind)
!      .atom.make_unique_atom_list(unique_atom)
!      stdout.text("unique atom list")
!      stdout.put(unique_atom,"column")
!      unique_atom.destroy
!      atom_kind.destroy
!   end
!
!   test_eigen
!   !
!      n,i :: INT
!      dot :: REAL
!       m :: REALMAT*
!      eval :: REALVEC*
!      L,R,w :: REALMAT*
!      stdin.read(n)
!      m.create(n,n)
!      stdin.read(m)
!      eval.create(n)
!      L.create(n,n)
!      R.create(n,n)
!      m.solve_general_eigenproblem(eval,L,R)
!      stdout.show("n =",n)
!      stdout.text("eval:")
!      stdout.put(eval,"column")
!      stdout.text("Left eigenvectors:")
!      stdout.put(L)
!      stdout.text("right eigenvectors:")
!      stdout.put(R)
!      w.create(n,n)
!      w.to_product_of(L,R,transpose_a=TRUE)
!      stdout.text("(left)^T x (right) eigenvectors:")
!      stdout.put(w)
!      w.destroy
!      R.destroy
!      L.destroy
!      eval.destroy
!      m.destroy
!   end

! *************************************************************
! Determinant Variation Perturbation Theory (DVPT) SCF routines
! *************************************************************

!   DVPT_scf ::: recursive, leaky
!   ! Do a restricted determinant variation perturbation theory SCF calculation. The following
!   ! are produced as results: .molecular_orbitals, .orbital_energies, .density_matrix
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE( .atom.created,  "no atom list")
!     .make_DVPT_initial_mos
!     .make_DVPT_density_matrices
!     .make_DVPT_fock_matrices
!     .scfdata.set(nuclear_energy=.nuclear_energy)
!     .scfdata.reset(energy=.DVPT_scf_energy, kinetic_energy=ZERO)
!
!   ! stdout.text("D:")
!   ! stdout.put(.density_matrix.restricted)
!   ! stdout.text("F:")
!   ! stdout.put(.fock_matrix.restricted)
!   ! .scfdata.scf_kind="rhf"
!   ! .scfdata.reset(energy=.scf_energy, kinetic_energy=ZERO)
!   ! .scfdata.scf_kind="rdvpt"
!
!     .scfdata.put_banner
!     ! Begin iterations
!     do
!     ! .extrapolate_scf
!       .update_DVPT_orbitals
!       .schmidt_orthonormalise(.molecular_orbitals)
!       .make_DVPT_density_matrices
!       .make_DVPT_fock_matrices
!       .save_DVPT_results
!       .scfdata.update(energy=.DVPT_scf_energy, kinetic_energy=ZERO)
!       .scfdata.put_results
!       if (.scfdata.scf_done) exit
!     end
!     .scfdata.cleanup_diis
!     .fock_matrix.destroy("restricted")
!   end
!
!   update_DVPT_orbitals
!   ! Solve for the new DVPT orbitals and energies. An initial set of orbitals
!   ! and a Fock matrix must already exist in memory. The new molecular orbitals
!   ! "MO*U" are found from the old MOs solving : (MO^T F MO)U = MO^T S MO U E
!   ! where U is a general matrix (since F is not symmetric).
!     MO_energies :: REALVEC*
!     MO,F :: REALMAT*
!     G,U,left :: REALMAT*
!      i :: INT
!     MO_energies => .orbital_energies.restricted
!     MO => .molecular_orbitals.restricted
!     F  => .fock_matrix.restricted
!     G.create(.n_bf,.n_bf)
!     U.create(.n_bf,.n_bf)
!     left.create(.n_bf,.n_bf)
!     G = F
!     G.change_basis(MO)
!     ! Level shifting
!     if ( .scfdata.apply_level_shifting) then
!       do i = .n_a + 1, .n_bf
!         G(i,i) = .scfdata.level_shift + G(i,i)
!       end
!     end
!     G.solve_general_eigenproblem(MO_energies,left,U)
!     G.to_product_of(MO,U)
!     MO = G
!     left.destroy
!     U.destroy
!     G.destroy
!   end
!
!   save_DVPT_results
!   ! Save the DVPT results in various archives
!     archive :: ARCHIVE
!      m :: INT
!     .save_scf_results
!     m = .DVPT_order
!     archive.set(.name,"DVPT_mo_"// trim(m.to_str))
!     archive.write(.molecular_orbitals.restricted)
!   end
!
!   make_DVPT_initial_mos ::: leaky
!   ! Make the DVPT initial orbitals. Currently this is just the same
!   ! as the usual initial guess, but the MO's are written to disk.
!       m :: INT
!      MO :: REALMAT*
!      m_archive :: ARCHIVE
!      m = .DVPT_order
!      m_archive.set(.name,"DVPT_mo_"// trim(m.to_str))
!      .scfdata.scf_kind = "rhf"
!      .get_initial_guess
!      .scfdata.scf_kind = "rdvpt"
!      MO => .molecular_orbitals.restricted
!      m_archive.write(MO)
!   end
!
!   DVPT_order result (X)
!   ! Determine the DVPT order by looking for the highest number "X" for
!   ! which a DVPT_mo_X archive file exists.
!       X :: INT
!      archive :: ARCHIVE
!      order :: INT = 999
!      if (order/=999) then
!         X = order
!      else
!         X = 0
!         do
!            archive.set(.name,"DVPT_mo_"// trim(X.to_str))
!            if (NOT archive.exists) exit
!            X = X + 1
!         end
!         order = X
!      end
!   end
!
!   make_DVPT_density_matrices
!   ! Make all the DVPT density matrices up to the required order
!   ! NOTE: the indice order is opposite to the paper.
!      m,j :: INT
!      m = .DVPT_order
!      do j = 0,m
!         .make_DVPT_density_matrix(m,j)
!      end
!      .make_DVPT_eff_density_matrix
!   !  .scfdata.scf_kind = "rhf"
!   !  .make_scf_density_matrix
!   !  .scfdata.scf_kind = "rdvpt"
!   end
!
!   make_DVPT_eff_density_matrix
!   ! Make the effective DVPT fock matrix
!      m,i,j :: INT
!      fac,sum,det :: REAL
!      DD,D :: REALMAT*
!      d_archive :: ARCHIVE
!      .density_matrix.restricted.destroy
!      DD.create(.n_bf,.n_bf); DD = ZERO
!      D.create(.n_bf,.n_bf)
!      m   = .DVPT_order
!      do i = 0,m
!      do j = 0,i
!         fac = TWO
!         if (i==j) fac = ONE
!         .get_DVPT_density_matrix(D,det,i,j)
!         sum = sum + fac*det
!         D = fac*D
!         DD.plus(D)
!      end
!      end
!      DD = DD/sum
!      d_archive.set(.name,"DVPT_density_matrix")
!      d_archive.write(DD)
!      .density_matrix.restricted => DD
!      D.destroy
!   end
!
!   get_DVPT_density_matrix(D,determinant,left,right)
!   ! Get the determinant variation perturbation theory transition density matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!   ! Make the density if the archive does not exist.
!       D :: REALMAT
!      determinant :: REAL
!      left,right :: INT
!      d_archive :: ARCHIVE
!      label :: STR
!      label = trim(left.to_str) // trim(right.to_str)
!      d_archive.set(.name,"DVPT_density_"// label)
!      if (NOT d_archive.exists) .make_DVPT_density_matrix(left,right)
!      d_archive.read(D)
!      d_archive.set(.name,"DVPT_determinant_"// trim(left.to_str) // trim(right.to_str))
!      d_archive.read(determinant)
!   end
!
!   make_DVPT_density_matrix(left,right)
!   ! Make the determinant variation perturbation theory transition density matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!   ! The density and determinant are stored as archives on disk.
!      left,right :: INT
!      D,L,R :: REALMAT*
!      l_archive,r_archive,d_archive :: ARCHIVE
!      determinant :: REAL
!      D.create(.n_bf,.n_bf)
!      l_archive.set(.name,"DVPT_mo_"// trim(left.to_str))
!      r_archive.set(.name,"DVPT_mo_"// trim(right.to_str))
!   ENSURE(l_archive.exists, "no DVPT mo archive, no. ="// trim(left.to_str))
!   ENSURE(r_archive.exists, "no DVPT mo archive, no. ="// trim(right.to_str))
!      L.create(.n_bf,.n_bf)
!      R.create(.n_bf,.n_bf)
!      l_archive.read(L)
!      r_archive.read(R)
!      ! Make density matrix using occupied orbitals only
!      .make_DVPT_density_matrix(D,determinant,L(:,:.n_a),R(:,:.n_a))
!
!      !       stdout.show("det =",determinant)
!      !       stdout.text("MO:")
!      !       stdout.put(L(:,.n_a))
!      !       stdout.text("D:")
!      !       stdout.put(TWO*D)
!
!      R.destroy
!      L.destroy
!      d_archive.set(.name,"DVPT_density_"// trim(left.to_str) // trim(right.to_str))
!      d_archive.write(D)
!      d_archive.set(.name,"DVPT_determinant_"// trim(left.to_str) // trim(right.to_str))
!      d_archive.write(determinant)
!      D.destroy
!   end
!
!   make_DVPT_density_matrix(D,determinant,left,right)
!   ! Make the determinant variation perturbation theory transition density matrix "D" for a
!   ! left determinant of occupied orbitals "left" and a right determinant of occupied
!   ! orbitals "right". Also return the "determinant" of the orbital ovarlap matrix.
!       D :: REALMAT
!      left,right :: REALMAT
!      determinant :: REAL
!      S,O,C :: REALMAT*
!      dim :: INT
!   ENSURE(size(D,1)==size(D,2),"D is not square")
!   ENSURE(size(D,1)==.n_bf,"D has wrong size")
!   ENSURE(size(left,1) ==.n_bf,"left has wrong size")
!   ENSURE(size(right,1)==.n_bf,"right has wrong size")
!   ENSURE(size(left,2)==size(right,2),"left and and right are incompatible")
!      dim = size(left,2)
!      S.create(dim,dim)
!      O.create(.n_bf,.n_bf)
!      .get_overlap_matrix(O)
!      O.change_basis(S,left,right)
!      O.destroy
!      C.create(dim,dim)
!      .make_cofactor(C,determinant,S)
!      C.back_transform(D,right,left)
!      C.destroy
!      S.destroy
!   end
!
!   make_cofactor(C,determinant,S)
!   ! Make the cofactor matrix "C" and "determinant" for the matrix "S".
!   ! Note: this constructs the alpha-alpha part of the cofactor, but the
!   ! determinant is that for the full overlap matrix
!      C,S :: REALMAT
!      determinant :: REAL
!      eval,n :: REALVEC*
!      left,right,W :: REALMAT*
!      dim,i :: INT
!      dot :: REAL
!   ENSURE(size(C,1)==size(C,2),"C is not square")
!   ENSURE(size(S,1)==size(S,2),"incorrect size for array S")
!   ENSURE(size(S,1)==size(C,1),"array C and array S incompatible")
!      dim = size(C,1)
!      left.create(dim,dim)
!      right.create(dim,dim)
!      eval.create(dim)
!      ! Get the eigenvalues and determinant of S
!      S.solve_general_eigenproblem(eval,left,right)
!      determinant = product(eval)
!      determinant = determinant*determinant ! include beta part
!      ! Make the cofactor
!      n.create(dim)
!      n = determinant/eval
!      W.create(dim,dim)
!      W.to_product_with_diagonal(n,left,transpose_a=TRUE)
!      C = matmul(right,W)
!
!    ! stdout.text("*********make_cofactor**************")
!    ! stdout.text("S:")
!    ! stdout.put(S)
!    ! stdout.text("n:")
!    ! stdout.put(n,format="column")
!    ! stdout.text("L:")
!    ! stdout.put(left)
!    ! stdout.text("R:")
!    ! stdout.put(right)
!    ! W = matmul(right,transpose(left))
!    ! stdout.text("R L^T:")
!    ! stdout.put(W)
!    ! W = matmul(transpose(left),right)
!    ! stdout.text("L^T R:")
!    ! stdout.put(W)
!    ! stdout.text("C:")
!    ! stdout.put(C)
!    ! stdout.text("************************************")
!
!      W.destroy
!      n.destroy
!      eval.destroy
!      right.destroy
!      left.destroy
!   end
!
!   make_DVPT_fock_matrices ::: leaky
!   ! Make the DVPT fock matrices up to the required order,
!   ! including the effective fock matrix
!      m,j :: INT
!      m = .DVPT_order
!      do j = 0,m
!         .make_DVPT_fock_matrix(m,j)
!      end
!      .make_DVPT_eff_fock_matrix
!   !  .scfdata.scf_kind = "rhf"
!   !  .make_fock_matrix
!   !  .scfdata.scf_kind = "rdvpt"
!   end
!
!   make_DVPT_eff_fock_matrix ::: leaky
!   ! Make the effective DVPT fock matrix
!      m,j :: INT
!      E_m,E_mj,sum,det :: REAL
!      FF,F,W,D :: REALMAT*
!      f_archive :: ARCHIVE
!      .fock_matrix.restricted.destroy
!      FF.create(.n_bf,.n_bf); FF = ZERO
!      F.create(.n_bf,.n_bf)
!      W.create(.n_bf,.n_bf)
!      D.create(.n_bf,.n_bf)
!      m   = .DVPT_order
!      E_m = .DVPT_electronic_energy
!      sum = ZERO
!      do j = 0,m
!         .get_DVPT_fock_matrix(F,m,j)
!         .get_DVPT_density_matrix(D,det,m,j)
!         sum = sum + det
!         det = 1/(TWO*det)
!         FF.plus(F)
!         .add_core_hamiltonian(FF)
!         E_mj = E_m + det*F.trace_of_product(D)
!         .get_overlap_matrix(F)
!         W.to_product_of(D,F)
!         D.to_product_of(F,W)
!         FF = FF - E_mj*D
!      end
!      FF = FF/sum
!      f_archive.set(.name,"DVPT_effective_fock_"// trim(m.to_str))
!      f_archive.write(FF)
!      .fock_matrix.restricted => FF
!      D.destroy
!      W.destroy
!      F.destroy
!   end
!
!   get_DVPT_fock_matrix(F,left,right)
!   ! Get the determinant variation perturbation theory transition Fock matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!       F :: REALMAT
!      left,right :: INT
!      f_archive :: ARCHIVE
!      label :: STR
!      label = trim(left.to_str) // trim(right.to_str)
!      f_archive.set(.name,"DVPT_fock_"// label)
!      if (NOT f_archive.exists) .make_DVPT_fock_matrix(left,right)
!      f_archive.read(F)
!   end
!
!   make_DVPT_fock_matrix(left,right)
!   ! Make the determinant variation perturbation theory transition fock matrix
!   ! for a left determinant orbitals "left" and  a right determinant orbitals "right".
!   ! The Fock matrix is stored as an archive on disk.
!      left,right :: INT
!       F :: REALMAT*
!      f_archive :: ARCHIVE
!      label :: STR
!      F.create(.n_bf,.n_bf)
!      .make_DVPT_fock_matrix(F,left,right)
!      label = trim(left.to_str) // trim(right.to_str)
!      f_archive.set(.name,"DVPT_fock_"// label)
!      f_archive.write(F)
!      F.destroy
!   end
!
!   make_DVPT_fock_matrix(F,left,right)
!   ! Make the determinant variation perturbation theory transition fock matrix for
!   ! a left determinant of orbitals "left" and a right determinant of orbitals
!   ! "right". If present, "D" is used as the density matrix.
!       F :: REALMAT
!      left,right :: INT
!       P :: REALMAT*
!      determinant,fac :: REAL
!      P.create(.n_bf,.n_bf)
!      .get_DVPT_density_matrix(P,determinant,left,right)
!   !  .make_r_asymmetric_fock(P,F,direct=FALSE,core=FALSE,r12=TRUE)
!      .make_r_fock(P,F,direct=FALSE,core=FALSE,r12=TRUE)
!      fac = TWO/determinant
!      F = fac*F
!      P.destroy
!   end
!
!   make_r_fock(P,F,direct,core,r12) ::: leaky
!   ! Make a new restricted Fock matrix "F" from the density matrix "P".
!   ! If present and TRUE , "direct" means calculate integrals on the fly
!   ! If present and FALSE, "core" removes the core matrix contribution
!   ! If present and FALSE, "r12"  removes the two electron contribution
!     direct,core,r12 :: BIN, optional
!     P,F :: REALMAT
!     J,K :: REALMAT*
!     do_direct,add_core,add_r12 :: BIN
!
!     do_direct= FALSE
!     add_core = TRUE
!     add_r12  = TRUE
!     if (present(direct)) do_direct = direct
!     if (present(core))   add_core = core
!     if (present(r12))    add_r12  = r12
!     if (add_r12) then
!        J.create(.n_bf,.n_bf)
!        K.create(.n_bf,.n_bf)
!        if (do_direct) then; .make_r_JK_direct(J,K,P)
!        else;                .make_r_JK_disk(J,K,P)
!        end
!        F = J - HALF*K
!        K.destroy
!        J.destroy
!     else
!        F = ZERO
!     end
!     if (add_core) .add_core_hamiltonian(F)
!   end
!
!   make_r_asymmetric_fock(P,F,direct,core,r12) ::: leaky
!   ! Make a new restricted Fock matrix "F" from an asymmetric density matrix "P".
!   ! If present and TRUE , "direct" means calculate integrals on the fly
!   ! If present and FALSE, "core" removes the core matrix contribution
!   ! If present and FALSE, "r12"  removes the two electron contribution
!     direct,core,r12 :: BIN, optional
!     P,F :: REALMAT
!     J,K :: REALMAT*
!     do_direct,add_core,add_r12 :: BIN
!   ENSURE(size(F,1)==.n_bf, "Fock matrix dimensions wrong")
!   ENSURE(size(P,1)==.n_bf, "Density matrix dimensions wrong")
!     do_direct= FALSE
!     add_core = TRUE
!     add_r12  = TRUE
!     if (present(direct)) do_direct = direct
!     if (present(core))   add_core = core
!     if (present(r12))    add_r12  = r12
!     if (add_r12) then
!        J.create(.n_bf,.n_bf)
!        K.create(.n_bf,.n_bf)
!        if (do_direct) then; .make_r_asymmetric_JK_direct(J,K,P)
!        else;                .make_r_asymmetric_JK_disk(J,K,P)
!        end
!        F = J - HALF*K
!        K.destroy
!        J.destroy
!     else
!        F = ZERO
!     end
!     if (add_core) then
!       .add_core_hamiltonian(F)
!     end
!   end
!
!   make_r_asymmetric_JK_direct(J,K,P)
!   ! Make the real coulomb matrices "J" and exchange matrix "K"  from an
!   ! asymmetric density matrix "P" directly from the integrals.
!     J,K,P :: REALMAT
!      I :: REALMAT4*
!     shell4 :: SHELL4*
!     I_max :: REALVEC*
!     ac,ab,cd,aa,cc :: INT
!     a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     cutoff,I_abcd,factor :: REAL
!     skip :: BIN
!     J = ZERO
!     K = ZERO
!     cutoff = .scfdata.eri_cutoff
!     shell4.create
!     I_max.create(.n_shell_pairs)
!     .make_max_abab_integrals(I_max)
!     do ab = 1,.n_shell_pairs
!       .get_shell_pair(ab,a,b)
!       fa = .first_basis_fn_for_shell(a)
!       fb = .first_basis_fn_for_shell(b)
!       la = .last_basis_fn_for_shell(a)
!       lb = .last_basis_fn_for_shell(b)
!       do cd = 1,ab
!         .get_shell_pair(cd,c,d)
!         fc = .first_basis_fn_for_shell(c)
!         fd = .first_basis_fn_for_shell(d)
!         lc = .last_basis_fn_for_shell(c)
!         ld = .last_basis_fn_for_shell(d)
!         .get_shell_quartet(shell4,a,b,c,d)
!         skip = .schwarz_inequality_test(cutoff,ab,cd,fa,la,fb,lb,fc,lc,fd,ld,P,I_max)
!         if (skip) then
!            shell4.destroy_ptr_part
!            cycle
!         end
!         I.create(fa,la,fb,lb,fc,lc,fd,ld)
!         shell4.get_ERI(I)
!         factor = ONE                                ! Evaluate the integrals
!         if (a==b)          factor = HALF            ! Coincidence factors
!         if (c==d)          factor = HALF * factor
!         if (a==c AND b==d) factor = HALF * factor
!         do d = fd,ld
!         do c = fc,lc
!         do b = fb,lb
!         do a = fa,la
!            I_abcd = factor * I(a,b,c,d)
!            J(a,b) = J(a,b) + I_abcd*(P(d,c)+P(c,d)) ! These are symmetric
!            J(c,d) = J(c,d) + I_abcd*(P(b,a)+P(a,b))
!            K(a,d) = K(a,d) + I_abcd*P(b,c)          ! These are asymmetric
!            K(a,c) = K(a,c) + I_abcd*P(b,d)
!            K(b,c) = K(b,c) + I_abcd*P(a,d)
!            K(b,d) = K(b,d) + I_abcd*P(a,c)
!            K(d,a) = K(d,a) + I_abcd*P(c,b)
!            K(c,a) = K(c,a) + I_abcd*P(d,b)
!            K(c,b) = K(c,b) + I_abcd*P(d,a)
!            K(d,b) = K(d,b) + I_abcd*P(c,a)
!         end
!         end
!         end
!         end
!         I.destroy
!         shell4.destroy_ptr_part
!       end
!     end
!     I_max.destroy
!     shell4.destroy
!     .weight_diagonal_blocks(J,TWO)
!     J.make_symmetric
!   end
!
!   make_r_asymmetric_JK_disk(J,K,P)
!   ! Make the real coulomb matrices "J" and exchange matrix "K"  from an
!   ! asymmetric density matrix "P" using disk integrals.
!     J,K,P :: REALMAT
!     eri_archive,ind_archive :: ARCHIVE
!      I :: REALMAT4*
!     q,n_quartets,a,b,c,d,fa,fb,fc,fd,la,lb,lc,ld :: INT
!     I_abcd :: REAL
!     eri_archive.create(.name,"eri_integrals")
!     ind_archive.create(.name,"eri_index")
!   ENSURE(.basis_info_made, "no basis set")
!   ENSURE(.atom.created,  "no atom list")
!   ENSURE(eri_archive.exists, "no integral file")
!   ENSURE(ind_archive.exists, "no integral index file")
!     eri_archive.open(for="read-only",buffered=TRUE,type="real")
!     ind_archive.open(for="read-only",buffered=TRUE,type="int")
!     J = ZERO
!     K = ZERO
!     n_quartets = .n_shell_quartets
!     do
!        ind_archive.file.read(q)
!        if (q > n_quartets) exit
!        .get_shell_quartet(q,fa,la,fb,lb,fc,lc,fd,ld)
!        I.create(fa,la,fb,lb,fc,lc,fd,ld)
!        eri_archive.file.read(I)
!        do d = fd,ld
!        do c = fc,lc
!        do b = fb,lb
!        do a = fa,la
!           I_abcd = I(a,b,c,d)
!           J(a,b) = J(a,b) + I_abcd*(P(d,c)+P(c,d)) ! These are symmetric
!           J(c,d) = J(c,d) + I_abcd*(P(b,a)+P(a,b))
!           K(a,d) = K(a,d) + I_abcd*P(b,c)          ! These are asymmetric
!           K(a,c) = K(a,c) + I_abcd*P(b,d)
!           K(b,c) = K(b,c) + I_abcd*P(a,d)
!           K(b,d) = K(b,d) + I_abcd*P(a,c)
!           K(d,a) = K(d,a) + I_abcd*P(c,b)
!           K(c,a) = K(c,a) + I_abcd*P(d,b)
!           K(c,b) = K(c,b) + I_abcd*P(d,a)
!           K(d,b) = K(d,b) + I_abcd*P(c,a)
!        end
!        end
!        end
!        end
!        I.destroy
!     end
!     ind_archive.destroy
!     eri_archive.destroy
!     .weight_diagonal_blocks(J,TWO)
!     J.make_symmetric
!   end
!
!   get_shell_pair_indices_from(index,a,b)
!   ! Return the actual shell indicies "a" and "b" which map to "index"
!     index :: INT, IN
!     a,b :: INT, OUT
!     a  = (1+sqrt(8.0d0*index-7.0d0))/2
!     b  = index - a*(a-1)/2
!   end
!
!   DVPT_scf_energy result (res)
!   ! Evaluates the determinant variation perturbation theory SCF energy
!   ! (including the nuclear repulsion contribution)
!     res :: REAL
!     res = .DVPT_electronic_energy + .nuclear_energy
!   end
!
!   DVPT_electronic_energy result (res)
!   ! Evaluates the SCF electronic energy
!     res :: REAL
!     scf_kind :: STR
!     F,D :: REALMAT*
!     m,i,j :: INT
!     fac,det,sum :: REAL
!     m = .DVPT_order
!     scf_kind = .scfdata.scf_kind
!     select case (scf_kind)
!        case ("rdvpt","restricted_dvpt")
!           F.create(.n_bf,.n_bf)
!           D.create(.n_bf,.n_bf)
!           res = ZERO
!           sum = ZERO
!           do i = 0,m
!           do j = 0,i
!              fac = TWO
!              if (i==j) fac = ONE
!              .get_DVPT_density_matrix(D,det,i,j)
!              .get_DVPT_fock_matrix(F,i,j)
!              .add_core_hamiltonian(F)
!
!           !  stdout.show("i   =",i)
!           !  stdout.show("j   =",j)
!           !  stdout.show("det =",det)
!           !  stdout.text("D:")
!           !  stdout.put(TWO*D)
!           !  stdout.text("F:")
!           !  stdout.put(F)
!
!              .add_core_hamiltonian(F)
!              res = res + fac*F.trace_of_product(D)
!              sum = sum + fac*det
!           end
!           end
!           res = res/sum
!           D.destroy
!           F.destroy
!        case default; DIE("not yet implemented, " // .scfdata.scf_kind)
!     end
!   end

   subroutine make_spin_b_field_grid(self,bb,pt,Dx,Dy,Dz)
    MOL :: self
   ! Make the magnetic B field "bb" due to the spin density at a set of points
   ! given in "pt" using three real symmetric AO density matrices "Dx", "Dy" and
   ! "Dz" corresponding to the Sx, Sy and Sz densities. Note: the contribution
   ! is added to whatever is already in bb. Note: factor of half in the
   ! densities is assumed.
      REALMAT(:,:) :: bb,pt
      REALMAT(:,:) :: Dx,Dy,Dz
      REALMAT3(:,:,:), PTR :: DD
      REALMAT4(:,:,:,:), PTR :: M
      REALMAT(:,:), PTR :: Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz
      INT :: q,fa,fb,la,lb,n, n_pt, i,j,k
      REALMAT3(3,3,3) :: eps
      REAL :: fac
      SHELL2 :: shell
      STACK("MOL:make_spin_b_field_grid")
      START_TIMER("MOL:make_spin_b_field_grid")
      ENSURE(self%basis_info_made,"MOL:make_spin_b_field_grid ... no basis set")
      ENSURE(associated(self%atom),"MOL:make_spin_b_field_grid ... no atom list")
      ENSURE(size(pt,2)==3,"MOL:make_spin_b_field_grid ... no basis set")
      ENSURE(size(Dx,1)==self%n_bf AND is_square_(Dx),"MOL:make_spin_b_field_grid ... wrong shape for Dx")
      ENSURE(size(Dy,1)==self%n_bf AND is_square_(Dy),"MOL:make_spin_b_field_grid ... wrong shape for Dy")
      ENSURE(size(Dz,1)==self%n_bf AND is_square_(Dz),"MOL:make_spin_b_field_grid ... wrong shape for Dz")
      n_pt = size(pt,1)
      bb = ZERO
      do q = 1,self%n_shell_pairs
        call get_shell_pair_(self,shell,q,fa,la,fb,lb)
        call create_(DD,shell%b%n_comp,shell%a%n_comp,3)
        DD(:,:,1) = Dx(fb:lb,fa:la)
        DD(:,:,2) = Dy(fb:lb,fa:la)
        DD(:,:,3) = Dz(fb:lb,fa:la)
        fac = ONE; if (fa==fb) fac=HALF
        call create_(M,shell%a%n_comp,shell%b%n_comp,3,3)
        Mxx => M(:,:,1,1); Mxy => M(:,:,1,2); Mxz => M(:,:,1,3)
        Myx => M(:,:,2,1); Myy => M(:,:,2,2); Myz => M(:,:,2,3)
        Mzx => M(:,:,3,1); Mzy => M(:,:,3,2); Mzz => M(:,:,3,3)
        do n = 1,n_pt
          call make_magnetic_S_ints_(shell,Mxx,Mxy,Mxz,Myx,Myy,Myz,Mzx,Mzy,Mzz,pt(n,:))
          do i = 1,3
          do j = 1,3
          do k = 1,3
             eps(i,j,k) = trace_product_with_(M(:,:,i,j),DD(:,:,k))
          end do
          end do
          end do
          bb(n,1) = bb(n,1) + trace_(eps(:,:,1)) - trace_(eps(1,:,:))
          bb(n,2) = bb(n,2) + trace_(eps(:,:,2)) - trace_(eps(2,:,:))
          bb(n,3) = bb(n,3) + trace_(eps(:,:,3)) - trace_(eps(3,:,:))
        end do
        call destroy_(M)
        call destroy_(DD)
        call destroy_ptr_part_(shell)
      end do
   !  bb.zero_small_values(TOL(10))
      bb = -bb ! this is -g_e mu_b; the final answer is in units of mu_0/4pi
     STOP_TIMER("MOL:make_spin_b_field_grid")
      CHECK
   end subroutine

   subroutine make_spin_b_field(self)
    MOL :: self
   ! Make the magnetic B field "bb" due to the spin density at a set of points
   ! given in "pt"
      REALMAT(:,:), PTR :: B,pt,Dx,Dy,Dz
      CPXMAT3(:,:,:), PTR :: dmc
      REAL :: aa,bb
      REALVEC(3) :: xv
      INT :: n_pt,i,j,k,n
      STACK("MOL:make_spin_b_field")
      START_TIMER("MOL:make_spin_b_field")
      n_pt = 60*75
      n_pt = 56
      call create_(dmc,160,160,4)
      ! Read the density matrix
      open(unit=13,file="opdens",status="old")
      do i=1,160
      do j=1,160
      do k=1,4
         read(13,*) dmc(i,j,k)
      end do
      end do
      end do
      close(13)
      ! Make the points array
      call create_(pt,n_pt,3)
      aa=7.4/sqrt(2.0)
!      bb=0.2/sqrt(2.0)
      bb=2./sqrt(2.0)
      xv(1)=aa
      xv(2)=-xv(1)
      n = 0
!      do i=1,75
      do i=1,8
        xv(3)=5.4
   !     do j=1,59
        do j=1,6
          n = n + 1
          pt(n,:) = xv
    !      xv(3)=xv(3)-0.2
          xv(3)=xv(3)-2.
        end do
        n = n + 1
        pt(n,:) = xv ! make one more point
        xv(1)=xv(1)-bb
        xv(2)=-xv(1)
      end do
      call create_(Dx,160,160)
      call create_(Dy,160,160)
      call create_(Dz,160,160)
      Dx =  dmc(:,:,2) + dmc(:,:,3)  * 0.5     ! ab + ba
      Dy = (dmc(:,:,2) - dmc(:,:,3)) * (0,0.5) ! -i ab + i ba
      Dz =  dmc(:,:,1) - dmc(:,:,4)  * 0.5     ! aa - bb
      call create_(B,n_pt,3)
      call make_spin_b_field_grid_(self,B,pt,Dx,Dy,Dz)
      ! Write out the answer
      call flush_(stdout)
      call text_(stdout,"The magnetic field from the spin")
      call put_(stdout,B)
      call destroy_(B)
      call destroy_(Dz)
      call destroy_(Dy)
      call destroy_(Dx)
      call destroy_(pt)
     STOP_TIMER("MOL:make_spin_b_field")
      CHECK
   end subroutine

!   make_divergence_j_para
!   ! Make the divergence of the paramagnetic current at a set of points
!   ! given in "pt"
!      B,pt,Dx,Dy,Dz :: REALMAT*
!      dmc :: CPXMAT3*
!      aa,bb :: REAL
!      xv :: REALVEC(3)
!      n_pt,i,j,k,n :: INT
!      n_pt = 60*75
!      n_pt = 56
!      dmc.create(160,160,4)
!      ! Read the density matrix
!      open(unit=13,file="opdens",status="old")
!      do i=1,160
!      do j=1,160
!      do k=1,4
!         read(13,*) dmc(i,j,k)
!      end
!      end
!      end
!      close(13)
!      ! Make the points array
!      pt.create(n_pt,3)
!      aa=7.4/sqrt(2.0)
!!      bb=0.2/sqrt(2.0)
!      bb=2./sqrt(2.0)
!      xv(1)=aa
!      xv(2)=-xv(1)
!      n = 0
!!      do i=1,75
!      do i=1,8
!        xv(3)=5.4
!   !     do j=1,59
!        do j=1,6
!          n = n + 1
!          pt(n,:) = xv
!    !      xv(3)=xv(3)-0.2
!          xv(3)=xv(3)-2.
!        end
!        n = n + 1
!        pt(n,:) = xv ! make one more point
!        xv(1)=xv(1)-bb
!        xv(2)=-xv(1)
!      end
!      Dx.create(160,160)
!      Dy.create(160,160)
!      Dz.create(160,160)
!      Dx =  dmc(:,:,2) + dmc(:,:,3)  * 0.5     ! ab + ba
!      Dy = (dmc(:,:,2) - dmc(:,:,3)) * (0,0.5) ! -i ab + i ba
!      Dz =  dmc(:,:,1) - dmc(:,:,4)  * 0.5     ! aa - bb
!      B.create(n_pt,3)
!      .make_spin_b_field_grid(B,pt,Dx,Dy,Dz)
!      ! Write out the answer
!      stdout.flush
!      stdout.text("The magnetic field from the spin")
!      stdout.put(B)
!      B.destroy
!      Dz.destroy
!      Dy.destroy
!      Dx.destroy
!      pt.destroy
!   end

   subroutine main(self)
    MOL :: self
   ! Main molecule run-loop; assumes stdin and stdout have been defined.
      PTR :: self
      STACK("MOL:main")
      START_TIMER("MOL:main")
      call set_error_output_file_(tonto,stdout)
      call create_(self)
      call put_banner_(self)
      call read_keywords_(self)   ! do it here
      call flush_(stdout)
      call text_(stdout,time_taken_(std_time,"job "//'"'// trim(self%name) //'"'))
      call text_(stdout,cpu_time_taken_(std_time,"job "//'"'// trim(self%name) //'"'))
      call destroy_(self)
     STOP_TIMER("MOL:main")
      CHECK
   end subroutine

   subroutine put_banner(self)
    MOL :: self
   ! Put to stdout the current TONTO banner
     STACK("MOL:put_banner")
     START_TIMER("MOL:put_banner")
     call text_(stdout," ")
     call text_(stdout,"---------------------------------------------------------------")
     call text_(stdout,"                                                               ")
     call text_(stdout," T   O   N   T   O                                             ")
     call text_(stdout,"                                                               ")
     call text_(stdout," A free object-based system for computational chemistry        ")
     call text_(stdout,"                                                               ")
     call text_(stdout," Version 1.0                                                   ")
     #ifdef SPEC_CPU
     call text_(stdout," with minor modifications for SPEC                             ")
     #endif
     call text_(stdout,"                                                               ")
     call text_(stdout," For conditions of use, on-line documentation, and contributor ")
     call text_(stdout," and developer information see :-                              ")
     call text_(stdout,"                                                               ")
     call text_(stdout," www."//"theochem."//"uwa."//"edu."//"au/tonto/                ")
     call text_(stdout,"                                                               ")
     call text_(stdout," Dylan Jayatilaka                                              ")
     call text_(stdout," Daniel Grimwood                                               ")
     call text_(stdout,"---------------------------------------------------------------")
     call text_(stdout," ")
     call start_(std_time)
     call text_(stdout, start_time_(std_time) )
     STOP_TIMER("MOL:put_banner")
      CHECK
   end subroutine

   subroutine reset_molecule(self)
    MOL :: self
   ! Resets the mol data type and reruns the read routine
      PTR :: self
      STR(STR_SIZE) :: word
      STACK("MOL:reset_molecule")
      START_TIMER("MOL:reset_molecule")
      call destroy_(self)
      call create_(self)
      call read_(stdin,word); call to_lower_case_(word)         ! First keyword must be "name"
      ENSURE(word=="name","MOL:reset_molecule ... first keyword must be name")
      call read_(stdin,self%name)
     STOP_TIMER("MOL:reset_molecule")
      UNSTACK
   end subroutine

   recursive subroutine read_keywords(self)
    MOL :: self
   ! Read data from "stdin" using keyword style input.
   ! NOTE : this routine reuires a MOL pointer self variable
      PTR :: self
      STR(STR_SIZE) :: word
      STACK("MOL:read_keywords")
      START_TIMER("MOL:read_keywords")
      call read_(stdin,word)
      ENSURE(word=="{","MOL:read_keywords ... expecting an open bracket symbol, {")
      read_loop: do          ! Loop over keywords
         call read_(stdin,word)
         if (word=="}")      exit read_loop
         if (reverted_(stdin)) exit read_loop
         call process_keyword_(self,word)
      end do read_loop
     STOP_TIMER("MOL:read_keywords")
      UNSTACK
   end subroutine

   recursive subroutine process_keyword(self,keyword)
    MOL :: self
   ! Process a keyword "word". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
   ! NOTE : this routine reuires a MOL pointer self variable
      PTR :: self
      STR(*) :: keyword
      STR(STR_SIZE) :: word
      TIME :: timer
      STACK("MOL:process_keyword")
      START_TIMER("MOL:process_keyword")
      call start_(timer)
      word = keyword
      call to_lower_case_(word)
      if (includes_(word," ")) then
         call redirect_(stdin,(/word/))
         call read_keywords_(self)
         call revert_(stdin)
      else
         call flush_(stdout)
         call text_(stdout,"keyword found --> " // trim(word))
         select case (word)
           case ("}                             "); ! exit case
           case ("assign_natural_orbitals       "); call assign_natural_orbitals_(self)
           case ("atoms=                        "); call read_atoms_(self)
           case ("atom_groups=                  "); call read_atom_groups_(self)
           case ("basis_sets=                   "); call read_basis_sets_(self)
           case ("basis_set_directory=          "); call read_library_directory_(self%basis)
           case ("basis_set_kind=               "); call read_basis_set_kind_(self)
           case ("b_field=                      "); call read_B_field_(self)
           case ("canonicalize_mos              "); call canonicalize_mos_(self)
           case ("charge=                       "); call read_charge_(self)
           case ("cluster=                      "); call read_cluster_(self)
           case ("coppens_basis_sets=           "); call read_coppensbasis_sets_(self)
           case ("create_cluster                "); call create_cluster_(self)
           case ("crystal=                      "); call read_crystal_(self)
           case ("delete_scf_integrals          "); call delete_scf_integrals_(self)
           case ("delete_scf_archives           "); call delete_scf_archives_(self)
           case ("destroy_cluster               "); call destroy_cluster_(self)
           case ("dftgrid=                      "); call read_dftgrid_(self)
         ! case ("dvpt_scf                      "); .DVPT_scf
           case ("e_field=                      "); call read_E_field_(self)
           case ("fit_thermal_parameters        "); call fit_thermal_parameters_(self)
           case ("force_thermal_symmetry        "); call force_thermal_symmetry_(self)
           case ("gauge_origin=                 "); call read_gauge_origin_(self)
           case ("get_atom_density              "); call get_atom_density_(self)
           case ("get_ano_data                  "); call get_ANO_data_(self)
         ! case ("gradient                      "); .hf_scf_energy_gradient
           case ("group_charges=                "); call read_group_charges_(self)
           case ("integrate_density_numerically "); call integrate_density_numerically_(self)
           case ("isosurface=                   "); call read_isosurface_(self)
           case ("isosurface_plot               "); call isosurface_plot_(self)
           case ("plot_on_isosurface            "); call plot_on_isosurface_(self)
         ! case ("integrate_density_functional  "); .integrate_density_functional
           case ("make_ao_density_matrix        "); call make_ao_density_matrix_(self)
           case ("make_ao_sz_density_matrix     "); call make_ao_sz_density_matrix_(self)
           case ("make_atom_density             "); call make_atom_density_(self)
           case ("make_fermi_mobility_grid      "); call make_fermi_mobility_grid_(self)
           case ("make_fock_guess               "); call make_fock_guess_(self)
           case ("make_fock_matrix              "); call make_fock_matrix_(self)
           case ("make_group_density_matrix     "); call make_group_density_matrix_(self)
           case ("make_irrotational_jp_grid     "); call make_irrotational_jp_grid_(self)
           case ("make_monomer_mos              "); call make_monomer_MOs_(self)
           case ("make_mulliken_matrix          "); call make_mulliken_matrix_(self)
           case ("make_natural_orbitals         "); call make_natural_orbitals_(self)
         ! case ("make_pcc_b_field              "); .make_pcc_b_field
         ! case ("make_pcc_densities            "); .make_pcc_densities
         ! case ("make_pcc_L_matrices           "); .make_pcc_L_matrices
         ! case ("make_pcc_structure_factors    "); .make_pcc_structure_factors
           case ("make_pnd_scalar_magnetic_sf   "); call make_PND_scalar_magnetic_sf_(self)
           case ("make_promol_density_matrix    "); call make_promol_density_matrix_(self)
           case ("make_promol_mos               "); call make_promol_MOs_(self)
           case ("make_scf_density_matrix       "); call make_scf_density_matrix_(self,damp=FALSE)
           case ("make_spin_b_field             "); call make_spin_b_field_(self)
           case ("make_structure_factors        "); call make_structure_factors_(self)
           case ("make_symortho_density_matrix  "); call make_promol_density_matrix_(self)
           case ("make_sz_structure_factors     "); call make_sz_structure_factors_(self)
           case ("make_vib_averaged_rho_grid    "); call make_vib_averaged_rho_grid_(self)
           case ("make_weak_force_energy_shift  "); call make_weak_force_energy_shift_(self)
           case ("multiplicity=                 "); call read_multiplicity_(self)
           case ("name=                         "); call read_name_(self)
         ! case ("optimise_geometry             "); .optimise_geometry
           case ("optimise_orbitals             "); call optimise_orbitals_(self)
           case ("optimise_thermal_parameters=  "); call read_optimise_thermals_(self)
           case ("output_style_options=         "); call read_output_style_options_(self)
           case ("plot                          "); call plot_(self)
           case ("plotgrid=                     "); call read_plotgrid_(self)
         ! case ("plot_covalent_density_orbitals"); .plot_covalent_density_orbitals
         ! case ("plot_covalent_orbitals        "); .plot_covalent_orbitals
         ! case ("plot_ionic_density_orbitals   "); .plot_ionic_density_orbitals
         ! case ("plot_ionic_orbitals           "); .plot_ionic_orbitals
         ! case ("plot_subspace_density         "); .plot_subspace_density
           case ("pointgroup=                   "); call read_pointgroup_(self)
           case ("put                           "); call put_(self)
           case ("put_all_bonds                 "); call put_all_bonds_(self)
           case ("put_1e_properties             "); call put_1e_properties_(self)
           case ("put_all_atom_coord_info       "); call put_coord_info_(self%atom,TRUE)
           case ("put_ao_energy_partition       "); call put_AO_energy_partition_(self)
         ! case ("put_atom_kind_map             "); .put_atom_kind_map
           case ("put_chi2_vs_angle_plot        "); call put_chi2_vs_angle_plot_(self%crystal)
         ! case ("put_cluster_input             "); .put_cluster_input
           case ("put_crystal                   "); call put_crystal_(self)
           case ("put_crystal_reflection_data   "); call put_crystal_reflection_data_(self)
           case ("put_current_time              "); call put_current_time_(self,timer)
           case ("put_density_matrix            "); call put_density_matrix_(self)
         ! case ("put_electrostatic_energy      "); .put_electrostatic_energy
           case ("put_fock_matrix               "); call put_fock_matrix_(self)
           case ("put_f_calc                    "); call put_F_calc_(self%crystal)
           case ("put_fcalc_plots               "); call put_fcalc_plots_(self%crystal)
           case ("put_g_tensor_information      "); call put_g_tensor_information_(self)
           case ("put_labelled_qq_plot          "); call put_labelled_qq_plot_(self%crystal,self%name)
           case ("put_molecular_orbitals        "); call put_molecular_orbitals_(self)
           case ("put_mos_and_energies          "); call put_mos_and_energies_(self)
           case ("put_mo_energy_partition       "); call put_MO_energy_partition_(self)
           case ("put_roby_mmo_energy_partition "); call put_roby_mmo_energy_partition_(self)
           case ("put_roby_smo_energy_partition "); call put_roby_smo_energy_partition_(self)
           case ("put_sao_energy_partition      "); call put_SAO_energy_partition_(self)
           case ("put_scf_energy                "); call put_scf_energy_(self)
           case ("put_scf_energy_in_mo_pairs    "); call put_scf_energy_in_mo_pairs_(self)
           case ("put_plotgrid                  "); call put_plotgrid_(self)
           case ("put_pnd_sf                    "); call put_PND_sf_(self)
           case ("put_pointgroup                "); call put_pointgroup_(self)
           case ("put_qq_plot                   "); call put_qq_plot_(self%crystal,self%name)
           case ("put_sylvian_csizmadia_tensors "); call put_sylvian_csizmadia_tensors_(self)
           case ("put_time_taken                "); call put_time_taken_(self,timer)
           case ("put_total_time                "); call put_total_time_(self)
           case ("put_vrml                      "); call put_vrml_(self)
           case ("read_archive                  "); call read_archive_(self)
           case ("read_ascii_archive            "); call read_ascii_archive_(self)
           case ("read_g94_checkpoint_file      "); call read_g94_checkpoint_file_(self)
           case ("redirect                      "); call redirect_(self)
           case ("revert                        "); call revert_(self)
         ! case ("put_roby_shared_population    "); .put_roby_shared_population
         ! case ("roby_population_analysis      "); .roby_population_analysis
           case ("roby_analysis                 "); call roby_analysis_(self)
           case ("robydata=                     "); call read_robydata_(self)
           case ("scf                           "); call scf_(self)
           case ("scfdata=                      "); call read_scfdata_(self)
           case ("simulate_new_f_exp            "); call simulate_new_F_exp_(self%crystal)
           case ("slater_basis_sets=            "); call read_slaterbasis_sets_(self)
           case ("start_new_molecule            "); call reset_molecule_(self)
           case ("start_timer                   "); call start_(timer)
         ! case ("test_spin_orbit_b_matrices    "); .test_spin_orbit_B_matrices
         ! case ("test_zora_so_matrices         "); .test_ZORA_SO_matrices
         ! case ("test_dftgrid                  "); .test_dftgrid
         ! case ("test_eigen                    "); .test_eigen
           case ("unsave                        "); call unsave_(self)
           case ("write_wfn_file                "); call write_wfn_file_(self)
           case ("write_archive                 "); call write_archive_(self)
           case ("write_ascii_archive           "); call write_ascii_archive_(self)
         ! case ("e2_energy                     "); stdout.put(.e2_energy)
         ! case ("the_e2_energy                 "); stdout.put(.the_e2_energy)
         ! case ("hf_energy                     "); stdout.put(.hf_energy)
           ! Crystal Explorer commands ...
           case ("cif_data_block_name=          "); call read_CIF_data_block_name_(self)
           case ("cif_file_name=                "); call read_CIF_file_name_(self)
           case ("cx_file_name=                 "); call read_CX_file_name_(self)
           case ("cx_surface=                   "); call read_CX_surface_(self)
           case ("process_cif                   "); call process_CIF_(self)
           case ("process_cif_for_cx            "); call process_CIF_for_CX_(self)
           case ("put_cx_data                   "); call put_CX_data_(self)
           case  default ;               allocate(tonto%known_keywords(113))
           tonto%known_keywords(1) = "}                             "
           tonto%known_keywords(2) = "assign_natural_orbitals       "
           tonto%known_keywords(3) = "atoms=                        "
           tonto%known_keywords(4) = "atom_groups=                  "
           tonto%known_keywords(5) = "basis_sets=                   "
           tonto%known_keywords(6) = "basis_set_directory=          "
           tonto%known_keywords(7) = "basis_set_kind=               "
           tonto%known_keywords(8) = "b_field=                      "
           tonto%known_keywords(9) = "canonicalize_mos              "
           tonto%known_keywords(10) = "charge=                       "
           tonto%known_keywords(11) = "cluster=                      "
           tonto%known_keywords(12) = "coppens_basis_sets=           "
           tonto%known_keywords(13) = "create_cluster                "
           tonto%known_keywords(14) = "crystal=                      "
           tonto%known_keywords(15) = "delete_scf_integrals          "
           tonto%known_keywords(16) = "delete_scf_archives           "
           tonto%known_keywords(17) = "destroy_cluster               "
           tonto%known_keywords(18) = "dftgrid=                      "
           tonto%known_keywords(19) = "e_field=                      "
           tonto%known_keywords(20) = "fit_thermal_parameters        "
           tonto%known_keywords(21) = "force_thermal_symmetry        "
           tonto%known_keywords(22) = "gauge_origin=                 "
           tonto%known_keywords(23) = "get_atom_density              "
           tonto%known_keywords(24) = "get_ano_data                  "
           tonto%known_keywords(25) = "group_charges=                "
           tonto%known_keywords(26) = "integrate_density_numerically "
           tonto%known_keywords(27) = "isosurface=                   "
           tonto%known_keywords(28) = "isosurface_plot               "
           tonto%known_keywords(29) = "plot_on_isosurface            "
           tonto%known_keywords(30) = "make_ao_density_matrix        "
           tonto%known_keywords(31) = "make_ao_sz_density_matrix     "
           tonto%known_keywords(32) = "make_atom_density             "
           tonto%known_keywords(33) = "make_fermi_mobility_grid      "
           tonto%known_keywords(34) = "make_fock_guess               "
           tonto%known_keywords(35) = "make_fock_matrix              "
           tonto%known_keywords(36) = "make_group_density_matrix     "
           tonto%known_keywords(37) = "make_irrotational_jp_grid     "
           tonto%known_keywords(38) = "make_monomer_mos              "
           tonto%known_keywords(39) = "make_mulliken_matrix          "
           tonto%known_keywords(40) = "make_natural_orbitals         "
           tonto%known_keywords(41) = "make_pnd_scalar_magnetic_sf   "
           tonto%known_keywords(42) = "make_promol_density_matrix    "
           tonto%known_keywords(43) = "make_promol_mos               "
           tonto%known_keywords(44) = "make_scf_density_matrix       "
           tonto%known_keywords(45) = "make_spin_b_field             "
           tonto%known_keywords(46) = "make_structure_factors        "
           tonto%known_keywords(47) = "make_symortho_density_matrix  "
           tonto%known_keywords(48) = "make_sz_structure_factors     "
           tonto%known_keywords(49) = "make_vib_averaged_rho_grid    "
           tonto%known_keywords(50) = "make_weak_force_energy_shift  "
           tonto%known_keywords(51) = "multiplicity=                 "
           tonto%known_keywords(52) = "name=                         "
           tonto%known_keywords(53) = "optimise_orbitals             "
           tonto%known_keywords(54) = "optimise_thermal_parameters=  "
           tonto%known_keywords(55) = "output_style_options=         "
           tonto%known_keywords(56) = "plot                          "
           tonto%known_keywords(57) = "plotgrid=                     "
           tonto%known_keywords(58) = "pointgroup=                   "
           tonto%known_keywords(59) = "put                           "
           tonto%known_keywords(60) = "put_all_bonds                 "
           tonto%known_keywords(61) = "put_1e_properties             "
           tonto%known_keywords(62) = "put_all_atom_coord_info       "
           tonto%known_keywords(63) = "put_ao_energy_partition       "
           tonto%known_keywords(64) = "put_chi2_vs_angle_plot        "
           tonto%known_keywords(65) = "put_crystal                   "
           tonto%known_keywords(66) = "put_crystal_reflection_data   "
           tonto%known_keywords(67) = "put_current_time              "
           tonto%known_keywords(68) = "put_density_matrix            "
           tonto%known_keywords(69) = "put_fock_matrix               "
           tonto%known_keywords(70) = "put_f_calc                    "
           tonto%known_keywords(71) = "put_fcalc_plots               "
           tonto%known_keywords(72) = "put_g_tensor_information      "
           tonto%known_keywords(73) = "put_labelled_qq_plot          "
           tonto%known_keywords(74) = "put_molecular_orbitals        "
           tonto%known_keywords(75) = "put_mos_and_energies          "
           tonto%known_keywords(76) = "put_mo_energy_partition       "
           tonto%known_keywords(77) = "put_roby_mmo_energy_partition "
           tonto%known_keywords(78) = "put_roby_smo_energy_partition "
           tonto%known_keywords(79) = "put_sao_energy_partition      "
           tonto%known_keywords(80) = "put_scf_energy                "
           tonto%known_keywords(81) = "put_scf_energy_in_mo_pairs    "
           tonto%known_keywords(82) = "put_plotgrid                  "
           tonto%known_keywords(83) = "put_pnd_sf                    "
           tonto%known_keywords(84) = "put_pointgroup                "
           tonto%known_keywords(85) = "put_qq_plot                   "
           tonto%known_keywords(86) = "put_sylvian_csizmadia_tensors "
           tonto%known_keywords(87) = "put_time_taken                "
           tonto%known_keywords(88) = "put_total_time                "
           tonto%known_keywords(89) = "put_vrml                      "
           tonto%known_keywords(90) = "read_archive                  "
           tonto%known_keywords(91) = "read_ascii_archive            "
           tonto%known_keywords(92) = "read_g94_checkpoint_file      "
           tonto%known_keywords(93) = "redirect                      "
           tonto%known_keywords(94) = "revert                        "
           tonto%known_keywords(95) = "roby_analysis                 "
           tonto%known_keywords(96) = "robydata=                     "
           tonto%known_keywords(97) = "scf                           "
           tonto%known_keywords(98) = "scfdata=                      "
           tonto%known_keywords(99) = "simulate_new_f_exp            "
           tonto%known_keywords(100) = "slater_basis_sets=            "
           tonto%known_keywords(101) = "start_new_molecule            "
           tonto%known_keywords(102) = "start_timer                   "
           tonto%known_keywords(103) = "unsave                        "
           tonto%known_keywords(104) = "write_wfn_file                "
           tonto%known_keywords(105) = "write_archive                 "
           tonto%known_keywords(106) = "write_ascii_archive           "
           tonto%known_keywords(107) = "cif_data_block_name=          "
           tonto%known_keywords(108) = "cif_file_name=                "
           tonto%known_keywords(109) = "cx_file_name=                 "
           tonto%known_keywords(110) = "cx_surface=                   "
           tonto%known_keywords(111) = "process_cif                   "
           tonto%known_keywords(112) = "process_cif_for_cx            "
           tonto%known_keywords(113) = "put_cx_data                   "
           call unknown_(tonto,word,"MOL:process_keyword")
           deallocate(tonto%known_keywords)
         end select
      end if
     STOP_TIMER("MOL:process_keyword")
      UNSTACK
   end subroutine

   subroutine read_output_style_options(self)
    MOL :: self
   ! Read new output style options, for example change the
   ! width and number of field, or precision of output.
      STACK("MOL:read_output_style_options")
      START_TIMER("MOL:read_output_style_options")
      call read_keywords_(stdout,stdin)
     STOP_TIMER("MOL:read_output_style_options")
      CHECK
   end subroutine

   subroutine redirect(self)
    MOL :: self
   ! Redirect the input to the file whose name is the following string
   ! in the current input file.
      STR(STR_SIZE) :: name
      STACK("MOL:redirect")
      START_TIMER("MOL:redirect")
      call read_(stdin,name)
      call redirect_(stdin,name)
     STOP_TIMER("MOL:redirect")
      UNSTACK
   end subroutine

   subroutine revert(self)
    MOL :: self
   ! Revert back to the previously stored input file
      STACK("MOL:revert")
      START_TIMER("MOL:revert")
      call revert_(stdin)
     STOP_TIMER("MOL:revert")
      UNSTACK
   end subroutine

   subroutine put_sylvian_csizmadia_tensors(self)
    MOL :: self
   ! Put out the Sylvian-Csizmadia polarisability tensors.
   ! This routine will read the value of the Unsold denominator.
      REALMAT(:,:), PTR :: Dx,Dy,Dz, P,Pi,Pj,MOi,MOj
      REALMAT(:,:), PTR :: Qxx,Qyy,Qzz,Qxy,Qxz,Qyz
      REALMAT(:,:), PTR :: Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz
      INT :: x,y,z, i,j
      REAL :: delta,fac
      REALMAT(3,3) :: alpha
      REALMAT3(3,3,3) :: beta
      REALMAT3(:,:,:), PTR :: alpha1
      REALMAT4(:,:,:,:), PTR :: alpha2
   STACK("MOL:put_sylvian_csizmadia_tensors")
   START_TIMER("MOL:put_sylvian_csizmadia_tensors")
   ENSURE(associated(self%density_matrix),"MOL:put_sylvian_csizmadia_tensors ... no density matrix")
   ENSURE(self%basis_info_made,"MOL:put_sylvian_csizmadia_tensors ... no basis info")
   ENSURE(associated(self%atom),"MOL:put_sylvian_csizmadia_tensors ... no atom info")
      call flush_(stdout)
      call text_(stdout,"Sylvian-Csizmadia polarisability tensors")
      call flush_(stdout)
      call read_(stdin,delta)
      call show_(stdout,"Unsold denominator/a.u. =",delta)
      call create_(Dx,self%n_bf,self%n_bf); call create_(Dy,self%n_bf,self%n_bf); call create_(Dz,self%n_bf,self%n_bf)
      call create_(Qxx,self%n_bf,self%n_bf); call create_(Qyy,self%n_bf,self%n_bf); call create_(Qzz,self%n_bf,self%n_bf)
      call create_(Qxy,self%n_bf,self%n_bf); call create_(Qxz,self%n_bf,self%n_bf); call create_(Qyz,self%n_bf,self%n_bf)
      call create_(Oxxx,self%n_bf,self%n_bf); call create_(Oyyy,self%n_bf,self%n_bf); call create_(Ozzz,self%n_bf,self%n_bf)
      call create_(Oxxy,self%n_bf,self%n_bf); call create_(Oxxz,self%n_bf,self%n_bf)
      call create_(Oyyx,self%n_bf,self%n_bf); call create_(Oyyz,self%n_bf,self%n_bf)
      call create_(Ozzx,self%n_bf,self%n_bf); call create_(Ozzy,self%n_bf,self%n_bf)
      call create_(Oxyz,self%n_bf,self%n_bf)
      call get_dipole_matrices_(self,Dx,Dy,Dz)
      call get_quadrupole_matrices_(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
      call get_octupole_matrices_(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)
      call make_ao_density_matrix_(self)
      x = 1; y = 2; z = 3
      call create_(P,self%n_bf,self%n_bf)
      P = HALF*self%density_matrix%restricted
      alpha(x,x) = trace_product_with_(P,Qxx) - trace_product_with_(P,Dx,P,Dx)
      alpha(y,y) = trace_product_with_(P,Qyy) - trace_product_with_(P,Dy,P,Dy)
      alpha(z,z) = trace_product_with_(P,Qzz) - trace_product_with_(P,Dz,P,Dz)
      alpha(y,x) = trace_product_with_(P,Qxy) - trace_product_with_(P,Dy,P,Dx)
      alpha(z,x) = trace_product_with_(P,Qxz) - trace_product_with_(P,Dz,P,Dx)
      alpha(z,y) = trace_product_with_(P,Qyz) - trace_product_with_(P,Dz,P,Dy)
      call symmetric_reflect_(alpha)
      fac = FOUR/delta
      alpha = fac*alpha
      call flush_(stdout)
      call show_(stdout,"No. of occupiedf orbitals = ",self%n_a)
      call flush_(stdout)
      call text_(stdout,"Orbital eigenvalues:")
      call flush_(stdout)
      call put_(stdout,self%orbital_energies,"column")
      call flush_(stdout)
      call text_(stdout,"First polarisability:")
      call flush_(stdout)
      call put_(stdout,alpha)
      ! Evaluate the orbital contributions
      call create_(alpha1,3,3,self%n_a); alpha1 = ZERO
      call create_(alpha2,3,3,self%n_a,self%n_a); alpha2 = ZERO
      call create_(Pi,self%n_bf,self%n_bf)
      call create_(Pj,self%n_bf,self%n_bf)
      do i = 1,self%n_a
         MOi => self%molecular_orbitals%restricted(:,i:i)
         call to_product_of_(Pi,MOi,MOi,transpose_b=TRUE)
         alpha1(x,x,i) = trace_product_with_(Pi,Qxx) - trace_product_with_(Pi,Dx,Pi,Dx)
         alpha1(y,y,i) = trace_product_with_(Pi,Qyy) - trace_product_with_(Pi,Dy,Pi,Dy)
         alpha1(z,z,i) = trace_product_with_(Pi,Qzz) - trace_product_with_(Pi,Dz,Pi,Dz)
         alpha1(y,x,i) = trace_product_with_(Pi,Qxy) - trace_product_with_(Pi,Dy,Pi,Dx)
         alpha1(z,x,i) = trace_product_with_(Pi,Qxz) - trace_product_with_(Pi,Dz,Pi,Dx)
         alpha1(z,y,i) = trace_product_with_(Pi,Qyz) - trace_product_with_(Pi,Dz,Pi,Dy)
         call symmetric_reflect_(alpha1(:,:,i))
         do j = 1,(i-1)
            MOj => self%molecular_orbitals%restricted(:,j:j)
            call to_product_of_(Pj,MOj,MOj,transpose_b=TRUE)
            alpha2(x,x,i,j) = -trace_product_with_(Pi,Dx,Pj,Dx)-trace_product_with_(Pj,Dx,Pi,Dx)
            alpha2(y,y,i,j) = -trace_product_with_(Pi,Dy,Pj,Dy)-trace_product_with_(Pj,Dy,Pi,Dy)
            alpha2(z,z,i,j) = -trace_product_with_(Pi,Dz,Pj,Dz)-trace_product_with_(Pj,Dz,Pi,Dz)
            alpha2(y,x,i,j) = -trace_product_with_(Pi,Dy,Pj,Dx)-trace_product_with_(Pj,Dy,Pi,Dx)
            alpha2(z,x,i,j) = -trace_product_with_(Pi,Dz,Pj,Dx)-trace_product_with_(Pj,Dz,Pi,Dx)
            alpha2(z,y,i,j) = -trace_product_with_(Pi,Dz,Pj,Dy)-trace_product_with_(Pj,Dz,Pi,Dy)
            call symmetric_reflect_(alpha2(:,:,i,j))
            alpha2(:,:,j,i) = alpha2(:,:,i,j)
         end do
      end do
      call destroy_(Pj)
      call destroy_(Pi)
      alpha1 = fac*alpha1
      alpha2 = fac*alpha2
      call flush_(stdout)
      call text_(stdout,"First polarisability, orbital contributions:")
      call flush_(stdout)
      do i = 1,self%n_a
         call text_(stdout,"... for orbital "//trim(to_str_(i)))
         call put_(stdout,alpha1(:,:,i))
      end do
      call flush_(stdout)
      call text_(stdout,"First polarisability, orbital pair contributions:")
      call flush_(stdout)
      do i = 1,self%n_a
      do j = 1,(i-1)
         call text_(stdout,"... for orbitals "//trim(to_str_(i))//" and "//trim(to_str_(j)))
         call put_(stdout,alpha2(:,:,i,j))
      end do
      end do
      call flush_(stdout)
      call text_(stdout,"Sum of all orbital contributions:")
      call flush_(stdout)
      alpha = ZERO
      do i = 1,self%n_a
         alpha = alpha + alpha1(:,:,i)
         do j = 1,(i-1)
            alpha = alpha + alpha2(:,:,i,j)
         end do
      end do
      call put_(stdout,alpha)
      call flush_(stdout)
      call text_(stdout,"First polarisability, mono-orbital contributions:")
      call flush_(stdout)
      do i = 1,self%n_a
         do j = 1,self%n_a
            if (i==j) cycle
            alpha1(:,:,i) = alpha1(:,:,i) + HALF*alpha2(:,:,i,j)
         end do
         call text_(stdout,"... for orbital "//trim(to_str_(i)))
         call put_(stdout,alpha1(:,:,i))
      end do
      call flush_(stdout)
      call text_(stdout,"Sum of all mono-orbital contributions:")
      call flush_(stdout)
      alpha = ZERO
      do i = 1,self%n_a
         alpha = alpha + alpha1(:,:,i)
      end do
      call put_(stdout,alpha)
      call destroy_(alpha2)
      call destroy_(alpha1)
      beta(x,x,x) = trace_product_with_(P,Oxxx) - THREE*trace_product_with_(P,Dx,P,Qxx) &
                  + trace_product_with_(P,Dx,P,Dx,P,Dx)
      beta(y,y,y) = trace_product_with_(P,Oyyy) - THREE*trace_product_with_(P,Dy,P,Qyy) &
                  + trace_product_with_(P,Dy,P,Dy,P,Dy)
      beta(z,z,z) = trace_product_with_(P,Ozzz) - THREE*trace_product_with_(P,Dz,P,Qzz) &
                  + trace_product_with_(P,Dz,P,Dz,P,Dz)
      beta(y,x,x) = trace_product_with_(P,Oxxy) - TWO*trace_product_with_(P,Dx,P,Qxy) &
                  - trace_product_with_(P,Dy,P,Qxx) + trace_product_with_(P,Dy,P,Dx,P,Dx)
      beta(z,x,x) = trace_product_with_(P,Oxxz) - TWO*trace_product_with_(P,Dx,P,Qxz) &
                  - trace_product_with_(P,Dz,P,Qxx) + trace_product_with_(P,Dz,P,Dx,P,Dx)
      beta(y,y,x) = trace_product_with_(P,Oyyx) - TWO*trace_product_with_(P,Dy,P,Qxy) &
                  - trace_product_with_(P,Dx,P,Qyy) + trace_product_with_(P,Dy,P,Dy,P,Dx)
      beta(z,y,x) = trace_product_with_(P,Oxyz) - trace_product_with_(P,Dz,P,Qxy) &
                  - trace_product_with_(P,Dy,P,Qxz) - trace_product_with_(P,Dx,P,Qyz) &
                  + trace_product_with_(P,Dz,P,Dy,P,Dx)
      beta(z,z,x) = trace_product_with_(P,Ozzx) - TWO*trace_product_with_(P,Dz,P,Qxz) &
                  - trace_product_with_(P,Dx,P,Qzz) + trace_product_with_(P,Dz,P,Dz,P,Dx)
      beta(z,y,y) = trace_product_with_(P,Oyyz) - TWO*trace_product_with_(P,Dy,P,Qyz) &
                  - trace_product_with_(P,Dz,P,Qyy) + trace_product_with_(P,Dz,P,Dy,P,Dy)
      beta(z,z,y) = trace_product_with_(P,Ozzy) - TWO*trace_product_with_(P,Dz,P,Qyz) &
                  - trace_product_with_(P,Dy,P,Qzz) + trace_product_with_(P,Dz,P,Dz,P,Dy)
      call make_symmetric_(beta)
      fac = 12.0d0/delta
      beta = fac*beta
      call flush_(stdout)
      call text_(stdout,"Second polarisability:")
      call flush_(stdout)
      call put_(stdout,beta)
      call destroy_(P)
      call destroy_(Oxyz)
      call destroy_(Ozzy); call destroy_(Ozzx)
      call destroy_(Oyyz); call destroy_(Oyyx)
      call destroy_(Oxxz); call destroy_(Oxxy)
      call destroy_(Ozzz); call destroy_(Oyyy); call destroy_(Oxxx)
      call destroy_(Qyz); call destroy_(Qxz); call destroy_(Qxy)
      call destroy_(Qzz); call destroy_(Qyy); call destroy_(Qxx)
      call destroy_(Dz); call destroy_(Dy); call destroy_(Dx)
     STOP_TIMER("MOL:put_sylvian_csizmadia_tensors")
      CHECK
   end subroutine

end
