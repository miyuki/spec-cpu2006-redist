!-------------------------------------------------------------------------------
!
! TYPES : used to deposit all derived types used in TONTO.
!
! A separate module is required so that two modules can use each others types
! even if they can't use each others routines by with a "use" statement. In the
! C++ language every one of TONTO's objects are "friendly" with each other.
!
! (c) dylan jayatilaka, university of western australia, 1998
!
! $Id: types.foo,v 1.59.2.34 2004/04/21 09:12:56 reaper Exp $
!
!-------------------------------------------------------------------------------

module TYPES_MODULE

#  include "types.use"

   implicit none

#  include "macros"
#  include "types.int"




   type system_type

   INT :: error_status DEFAULT(0)
   ! Set non-zero on error.

   INT :: error_output_unit DEFAULT(SYSTEM_ERROR_OUTPUT_UNIT)
   ! The file unit number for error messages.

   INT :: memory_used DEFAULT(0)
   ! The amount of memory used by the program so far.

   INT :: memory_blocks_used DEFAULT(0)
   ! The current number of active memory blocks allocated using "create"
   ! commands

   INT :: max_memory_used DEFAULT(0)
   ! The maximum amount of memory used so far.

   INT :: max_memory_blocks_used DEFAULT(0)
   ! The maximum number of memory blocks allocated used by the program so far.

   INT :: memory_limit DEFAULT(SYSTEM_MEMORY_LIMIT)
   ! A soft memory limit. A warning is generated if this limit is exceeded bu
   ! the program is not stopped.

   BIN :: memory_limit_exceeded DEFAULT(FALSE)
   ! Set TRUE if the soft memory limit is exceeded.

   BIN :: memory_leak_detected DEFAULT(FALSE)
   ! Set TRUE if a memory leak has been detected. This is to prevent cascading
   ! memory leak reports from parent routines. However, this switch is set back
   ! to FALSE whenever a new routine is entered at a level below where the
   ! current leak occured, so it is not so useful.

   INT :: memory_leak_level DEFAULT(0)
   ! Gives the memory leak level below which leaks are not reported. This is to
   ! ensure that the same memory leak is not reported at lower levels. The
   ! variable is reset to 0 whenever a new routine is entered at a level lower
   ! than the leak.

   STR(STR_SIZE) :: memory_units DEFAULT(SYSTEM_MEMORY_UNITS)
   ! The units for acconting for memory usage.

   INT :: stack_level DEFAULT(0)
   ! The current call-stack level.

   INT :: max_stack_level DEFAULT(0)
   ! The maximum call-stack level.

   INT :: stack_show_level DEFAULT(-1)
   ! Enable printing of the call-stack, for all routines at this level or higher
   ! in the call-stack.

   INTVEC(:), PTR :: memory_used_stack DEFAULT_NULL
   ! An array which stores the amount of memory used by each routine at a given
   ! call-level in the call-stack.

   STRVEC(STR_SIZE,:), PTR :: call_stack DEFAULT_NULL
   ! Stores the name of each subroutine at each call-level, for routine traceback.

   BIN :: show_call_stack DEFAULT(FALSE)
   ! If TRUE, tells whether to show an indented call-stack as the program runs.
   ! The memory gained or lost by each routine is also displayed.  Be warned,
   ! setting this switch produces copious output.

   TEXTFILE, PTR :: io_file DEFAULT_NULL
   ! Last I/O file used

   INT :: time_stack_level DEFAULT(0)
   ! The current time call-stack level. This is essentially just the same as
   ! stack_level. It is not the same since we may want to check timings when the
   ! ENSURE statements are turned on -- call_stack always implies ENSURE.

   INTVEC(:), PTR :: time_call_stack DEFAULT_NULL
   ! Stores the *indices* of the routines called at each call-level.  This is
   ! used for subracting the childrens elspased times from parent routines.

   REALVEC(:), PTR :: time_strt_stack DEFAULT_NULL
   ! Stores the starting times for all routines in the time_call_stack.

   INT :: n_timed_routines DEFAULT(0)
   ! The number of routines that have currently been called and are being timed.

   REALVEC(:), PTR :: time_for_routine DEFAULT_NULL
   ! The time taken for a particular routine -- all routines called in the
   ! program will be timed in this stack.

   STRVEC(STR_SIZE,:), PTR :: name_for_routine DEFAULT_NULL
   ! The name for a particular timed routine -- all routines called in the
   ! program will be named in this stack.

   REAL :: time_limit DEFAULT(ZERO)
   ! Set a total time limit on a profile run. A zero value means no time limit.
   ! If the time limit is exceeded the programs stops and dumps a profile up to
   ! that point. Pretty good, huh.

   STRVEC(STR_SIZE,:), PTR :: known_keywords DEFAULT_NULL
   ! List of known keywords in the current case statement, if applicable
   ! This allows a nice error message to be returned saying what the allowed
   ! options in the case statement were.

   end type



  type parallel_type

    BIN :: do_parallel DEFAULT(FALSE)
    ! Whether or not to do parallel stuff.

    INT :: rank
    ! The number of the current processor

    INT :: nprocs DEFAULT(1)
    ! Number of processors

    INTVEC(:), PTR :: mpi_status DEFAULT_NULL
    ! Status field used for all MPI routines.

  end type



   type time_type

   INTVEC(5) :: started
   ! Contains real start time, in Julian day,h,m,s,ms

   INTVEC(5) :: stopped
   ! Contains real stop time, in Julian day,h,m,s,ms

   REAL :: start_cpu
   ! Contains CPU start time, in seconds

   REAL :: stop_cpu
   ! Contains CPU stop  time, in seconds

   end type



   type buffer_type

   INT :: item_start
   ! The position of the first character of the last item processed in
   ! the buffer string

   INT :: item_end
   ! The position of the last character of the last item processed in
   ! the buffer string

   INT :: item_index
   ! The item number of the last item processed in the buffer string

   INT :: n_items
   ! The total number of items in the string

   BIN :: analysed DEFAULT(FALSE)
   ! True, if the buffer string has been analysed

   STR(STR_SIZE) :: comment_chars DEFAULT(BUFFER_COMMENT_CHARS)
   ! The comment character symbols (concatenated) to be used in the
   ! buffer string

   STR(STR_SIZE) :: quote_chars DEFAULT(BUFFER_QUOTE_CHARS)
   ! The quote character symbols (concatenated) to be used in the
   ! buffer string

   STR(BSTR_SIZE) :: string DEFAULT(" ")
   ! This is the actual buffer string

   end type



   type unitnumber_type

   INT :: unit
   ! The unit number

   end type



   type file_type

   STR(STR_SIZE) :: name DEFAULT("unknown")
   ! The name of the file

   INT :: unit DEFAULT(0)
   ! The unit number

   INT :: record DEFAULT(1)
   ! The current record of the output

   INT :: io_status DEFAULT(0)
   ! Set non-zero if there is an error condition

   STR(STR_SIZE) :: action DEFAULT("readwrite")
   ! The type of action performed on this file

   STR(STR_SIZE) :: file_status DEFAULT("unknown")
   ! File status

   BIN :: buffered DEFAULT(FALSE)
   ! Whether to use buffering if the file is for numbers

   INT :: int_buffer_pos DEFAULT(1)
   ! The integer buffer position marker

   INTVEC(:), PTR :: int_buffer DEFAULT_NULL
   ! The integer buffer which is flushed to the file when full

   INT :: real_buffer_pos DEFAULT(1)
   ! The real buffer position marker

   REALVEC(:), PTR :: real_buffer DEFAULT_NULL
   ! The real buffer which is flushed to the file when full

   INT :: cpx_buffer_pos DEFAULT(1)
   ! The cpx buffer position marker

   CPXVEC(:), PTR :: cpx_buffer DEFAULT_NULL
   ! The cpx buffer which is flushed to the file when full

   end type



   type textfile_type

   STR(STR_SIZE) :: name DEFAULT("unknown")
   ! The name of the file

   STR(STR_SIZE) :: action DEFAULT("unknown")
   ! The action status of the file, "read" or "write"

   INT :: unit
   ! The unit number

   INT :: record
   ! The current record of the output

   INT :: io_status
   ! Set non-zero if there is an error condition

   BIN :: ignore_end_of_file DEFAULT(FALSE)
   ! Set to TRUE if the end of file is not to be regarded as an error.

   INT :: no_of_lines DEFAULT(-1)
   ! The number of lines in the file. This is only set if the end-of-file is
   ! encountered.

   INT :: n_fields DEFAULT(TEXTFILE_N_FIELDS)
   ! No of fields (i.e. columns) to use for outputting the variable

   INT :: int_width DEFAULT(TEXTFILE_INT_WIDTH)
   ! Width of an integer field

   INT :: real_width DEFAULT(TEXTFILE_REAL_WIDTH)
   ! Width of a real field

   INT :: real_precision DEFAULT(TEXTFILE_REAL_PRECISION)
   ! No. of decimal places to use for outputing a real variable

   STR(STR_SIZE) :: real_style DEFAULT(TEXTFILE_REAL_STYLE)
   ! Fortran style character for a real variable

   INT :: margin_width DEFAULT(TEXTFILE_MARGIN_WIDTH)
   ! Margin width

   BIN :: use_labels DEFAULT(TEXTFILE_USE_LABELS)
   ! Whether to use column and row labels on vectors and matrices

   STR(STR_SIZE) :: default_units DEFAULT(" ")
   ! Each REAL number is assumed to have "default_units", and are converted
   ! into generic (internal) units, usually A.U.

   STR(STR_SIZE) :: comment_chars DEFAULT(TEXTFILE_COMMENT_CHARS)
   ! The comment character symbols (concatenated) to be used in the textfile

   STR(STR_SIZE) :: quote_chars DEFAULT(TEXTFILE_QUOTE_CHARS)
   ! The quote character symbols (concatenated) to be used in the textfile

   BUFFER :: buffer
   ! The output buffer which holds one line of output to be flushed

   STRVEC(STR_SIZE,:), PTR :: internal DEFAULT_NULL
   ! Used to keep a record of of the entire file, internally

   TEXTFILE, PTR :: saved DEFAULT_NULL
   ! Used to keep a record of a previous file, say for input redirect

   end type



   type archive_type

   STR(STR_SIZE) :: root_name
   ! Archive root name (usually, the name of the job)

   STR(STR_SIZE) :: name
   ! Archive name (usually, the name of the entity in the archive)

   STR(STR_SIZE) :: genre
   ! Archive basis genre for opmatrix objects, or a qualifier for name

   STR(STR_SIZE) :: format
   ! Archive format (blank for binary)

   FILE, PTR :: file DEFAULT_NULL
   ! Binary file part

   TEXTFILE, PTR :: textfile DEFAULT_NULL
   ! Text file part

   end type



   type cif_type

   TEXTFILE, PTR :: file DEFAULT_NULL
   ! The actual CIF file to be processed

   STR(STR_SIZE) :: data_block_name DEFAULT(" ")
   ! The name of the data block

   INT :: start_of_data DEFAULT(1)
   ! The starting line of the data block

   INT :: end_of_data DEFAULT(0)
   ! The end line of the data block

   BIN :: data_block_found DEFAULT(FALSE)
   ! Set TRUE if a data block has been sucessfully found

   end type



   type rys_type

   INT :: nroots
   ! No. of rys roots

   REALVEC(:), PTR :: w DEFAULT_NULL
   ! The Rys weights

   REALVEC(:), PTR :: r DEFAULT_NULL
   ! The Rys roots

   end type



   type intvec__type

   INTVEC(:), PTR :: element DEFAULT_NULL
   ! Encapsulated ivec type

   end type



   type realvec__type

   REALVEC(:), PTR :: element DEFAULT_NULL
   ! Encapsulated vec type

   end type



   type realmat__type

   REALMAT(:,:), PTR :: element DEFAULT_NULL
   ! Encapsulated mat type

   end type



   type realmat3__type

   REALMAT3(:,:,:), PTR :: element DEFAULT_NULL
   ! Encapsulated mat3 type

   end type



   type realmat4__type

   REALMAT4(:,:,:,:), PTR :: element DEFAULT_NULL
   ! Encapsulated mat4 type

   end type



   type opvector_type

   INT :: n_bf
   ! No of spatial basis functions (i.e. AO's)

   REALVEC(:), PTR :: restricted DEFAULT_NULL
   ! Restricted spinorbital representation

   REALVEC(:), PTR :: alpha DEFAULT_NULL
   ! Alpha part of a DODS representation

   REALVEC(:), PTR :: beta DEFAULT_NULL
   ! Beta  part of a DODS representation

   REALVEC(:), PTR :: general DEFAULT_NULL
   ! General mixed alpha-beta matrix representation

   end type



   type opmatrix_type

   INT :: n_bf
   ! No of real spatial basis functions (i.e. AO's)

   REALMAT(:,:), PTR :: restricted DEFAULT_NULL
   ! Restricted spinorbital representation

   REALMAT(:,:), PTR :: alpha DEFAULT_NULL
   ! Alpha part of a DODS representation

   REALMAT(:,:), PTR :: beta DEFAULT_NULL
   ! Beta  part of a DODS representation

   REALMAT(:,:), PTR :: general DEFAULT_NULL
   ! General mixed alpha-beta matrix representation

   CPXMAT(:,:), PTR :: restricted_complex DEFAULT_NULL
   ! Complex restricted representation

   CPXMAT(:,:), PTR :: alpha_complex DEFAULT_NULL
   ! Complex alpha part of a DODS representation

   CPXMAT(:,:), PTR :: beta_complex DEFAULT_NULL
   ! Complex beta  part of a DODS representation

   CPXMAT(:,:), PTR :: general_complex DEFAULT_NULL
   ! Complex general mixed alpha-beta matrix representation

   REALVEC(:), PTR :: triangle DEFAULT_NULL
   ! Space saving for symmetric matrices: lower triangle only

   REALVEC(:), PTR :: square DEFAULT_NULL
   ! Space saving for hermitian matrices: lower triangle is real part, upper is imaginary

   end type



   type intvecinthash_type

   INT :: n_keys
   ! The number of keys

   INT :: n_size
   ! The size of the keys and values arrays.

   BIN :: reverse_search
   ! If TRUE, the search for values is performed from the largest key to the
   ! smallest.

   INTMAT(:,:), PTR :: keys DEFAULT_NULL
   ! The array of keys, stored contigously by columns.

   INTVEC(:), PTR :: values DEFAULT_NULL
   ! The array of values.

   end type



   type intvecintvechash_type

   INT :: n_keys
   ! The number of keys

   INT :: n_size
   ! The size of the keys and values arrays.

   BIN :: reverse_search
   ! If TRUE, the search for values is performed from the largest key to the
   ! smallest.

   INTMAT(:,:), PTR :: keys DEFAULT_NULL
   ! The array of keys, stored contigously by columns.

   INTMAT(:,:), PTR :: values DEFAULT_NULL
   ! The array of values.

   end type



   type reflection_type

   INT :: h
   ! Miller h index for the reflection

   INT :: k
   ! Miller k index for the reflection

   INT :: l
   ! Miller l index for the reflection

   REAL :: F_exp
   ! Experimental structure factor

   REAL :: F_sigma
   ! Experimental error in the structure factor

   CPX :: F_calc
   ! Calculated complex structure factor without including corrections

   REAL :: F_pred
   ! Calculated structure factor including scale and extinction corrections

   REAL :: I_exp
   ! Experimental intensity

   REAL :: I_sigma
   ! Experimental error in the intensity

   REAL :: I_pred
   ! Calculated intensity including scale and extinction corrections

   end type



   type diis_type

   INT :: keep DEFAULT(DIIS_KEEP)
   ! No. of parameter vectors to keep for use in extrapolation

   INT :: n_vec DEFAULT(0)
   ! The number of parameter vectors currently available for extrapolation

   INT :: new DEFAULT(0)
   ! The integer identifier for the new (current) parameter vector

   REAL :: error_length DEFAULT(ZERO)
   ! The length of the DIIS error vector

   ARCHIVE :: archive
   ! Archive used to store the DIIS parameter and error vectors

   REALVEC(:), PTR :: coeff DEFAULT_NULL
   ! The DIIS coefficients which combine/extrapolate the stored parameter
   ! vectors

   BIN :: in_core DEFAULT(FALSE)
   ! Whether to store data in memory instead of on disk
   REALMAT(:,:), PTR :: error_items DEFAULT_NULL
   REALMAT(:,:), PTR :: parameter_items DEFAULT_NULL
   REALMAT(:,:), PTR :: diis_matrix DEFAULT_NULL
   REALMAT(:,:), PTR :: constraint_matrix DEFAULT_NULL
   REALMAT(:,:), PTR :: density_matrix DEFAULT_NULL

   end type



   type gaussian_type

   INT :: l DEFAULT(0)
   ! l quantum number for the gaussian

   REAL :: ex DEFAULT(ZERO)
   ! Exponent for the gaussian

   REALVEC(3) :: pos DEFAULT(ZERO)
   ! Position of the gaussian
   end type



   type gaussian2_type

   GAUSSIAN :: a
   ! Shell "a" of the pair

   GAUSSIAN :: b
   ! Shell "b" of the pair

   end type



   type gaussian4_type

   GAUSSIAN :: a
   ! Shell "a" of the quartet

   GAUSSIAN :: b
   ! Shell "b" of the quartet

   GAUSSIAN :: c
   ! Shell "c" of the quartet

   GAUSSIAN :: d
   ! Shell "d" of the quartet

   end type



   type shell_type

   INT :: l DEFAULT(0)
   ! l quantum number for the gaussian shell

   INT :: n_comp DEFAULT(0)
   ! No. of components for the gaussian shell

   INT :: n_cc DEFAULT(0)
   ! No. of contractions for the gaussian shell

   REALVEC(:), PTR :: ex DEFAULT_NULL
   ! Exponent vector

   REALVEC(:), PTR :: cc DEFAULT_NULL
   ! Contraction coefficient vector

   end type



   type shell1_type

   INT :: l DEFAULT(0)
   ! l quantum number of the shell

   INT :: n_comp DEFAULT(0)
   ! No. of components for the shell

   INT :: n_cc DEFAULT(0)
   ! No. of contraction coefficients for the shell

   REALVEC(3) :: pos DEFAULT(ZERO)
   ! Position of the shell

   REALVEC(:), PTR :: ex DEFAULT_NULL
   ! Exponent vector

   REALVEC(:), PTR :: cc DEFAULT_NULL
   ! Contraction coefficient vector

   end type



   type shell2_type

   SHELL1 :: a
   ! Shell "a" of the pair

   SHELL1 :: b
   ! Shell "b" of the pair

   INT :: n_gaussian_pairs
   ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc

   INT :: l_max
   ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)

   INT :: l_min
   ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)

   INT :: l_sum
   ! Sum of the angular momenta on each shell, .a.l+.b.l

   REAL :: kappa_max
   ! Maximum of .a.cc * .b.cc * kappa_ab for the two electron integrals.

   REALVEC(:), PTR :: exponent_sum DEFAULT_NULL
   ! The sum of all gaussian pair exponents as a flattened array with shell "a"
   ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)

   REALVEC(:), PTR :: exponent_inv DEFAULT_NULL
   ! The inverse of the sum of the gaussian pair exponents, as a flattened
   ! array, 1/(.a.ex(:)+.b.ex(:))

   REALVEC(:), PTR :: a_exponent_inv DEFAULT_NULL
   ! The product of the exponent of shell "a" with the inverse of the sum of the
   ! gaussian pair exponents, as a flattened array, .a.ex(:)/(.a.ex(:)+.b.ex(:))

   REALVEC(:), PTR :: b_exponent_inv DEFAULT_NULL
   ! The product of the exponent of shell "b" with the inverse of the sum of the
   ! gaussian pair exponents, as a flattened array, .b.ex(:)/(.a.ex(:)+.b.ex(:))

   REALVEC(:), PTR :: cc_prefactor DEFAULT_NULL
   ! The contraction coefficient product with an exponential part for each
   ! gaussian pair as a flattened array with shell "a" gaussian components
   ! incrementing most rapidly. Useful for integral evaluation.

   REALVEC(:), PTR :: normalising_factors DEFAULT_NULL
   ! The product of the normalisation coefficients, as a flattened array,
   ! .a.l.normalising_factors * .b.l.normalising_factors.

   REALMAT(:,:), PTR :: pair_center DEFAULT_NULL
   ! The center of the product gaussian of gaussians .a and .b.

   REALMAT(:,:), PTR :: center_diff DEFAULT_NULL
   ! The difference between pair_center and the position of the gaussian of
   ! higher angular momentum out of .a and .b.

   end type



   type shellpair_type

   SHELL :: a
   ! Shell "a" of the pair

   SHELL :: b
   ! Shell "b" of the pair

   INT :: n_gaussian_pairs
   ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc

   INT :: l_max
   ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)

   INT :: l_min
   ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)

   INT :: l_sum
   ! Sum of the angular momenta on each shell

   REALVEC(:), PTR :: exponent_sum DEFAULT_NULL
   ! The sum of all gaussian pair exponents as a flattened array with shell "a"
   ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)

   REALVEC(:), PTR :: exponent_inv DEFAULT_NULL
   ! The inverse of the sum of the gaussian pair exponents, as a flattened
   ! array, 1/(.a.ex(:)+.b.ex(:))

   REALVEC(:), PTR :: a_exponent_inv DEFAULT_NULL
   ! The product of the exponent of shell "a" with the inverse of the sum of the
   ! gaussian pair exponents, as a flattened array, .a.ex(:)/(.a.ex(:)+.b.ex(:))

   REALVEC(:), PTR :: b_exponent_inv DEFAULT_NULL
   ! The product of the exponent of shell "b" with the inverse of the sum of the
   ! gaussian pair exponents, as a flattened array, .b.ex(:)/(.a.ex(:)+.b.ex(:))

   REALVEC(:), PTR :: cc_prefactor DEFAULT_NULL
   ! The contraction coefficient product with an exponential part for each
   ! gaussian pair as a flattened array with shell "a" gaussian components
   ! incrementing most rapidly. Useful for integral evaluation.

   REALVEC(:), PTR :: normalising_factors DEFAULT_NULL
   ! The product of the normalisation coefficients, as a flattened array,
   ! .a.l.normalising_factors * .b.l.normalising_factors.

   INTMAT3(:,:,:), PTR :: hrr_index_larger DEFAULT_NULL
   ! The mapping of the cartesian angular momenta to a single array, from
   ! max(.a.l,.b.l) to .a.l+.b.l.  In the HRR, this is the index for the
   ! component with the larger angular momentum.

   INTMAT3(:,:,:), PTR :: hrr_index_smaller DEFAULT_NULL
   ! The mapping of the cartesian angular momenta to a single array, from zero
   ! up to .a.l+.b.l.  In the HRR, this is the index for the component with the
   ! smaller angular momentum.

   INTMAT(:,:), PTR :: hrr_components DEFAULT_NULL
   ! Cartesian components of the angular momenta from zero up to .a.l+.b.l.

   INTVEC(:), PTR :: hrr_comp_to_use DEFAULT_NULL
   ! Which cartesian component of angular momentum to use for the HRR for the
   ! angular momenta between zero up to .a.l+.b.l.

   INTVEC(:), PTR :: form_3dints_x_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .l_max up to .a.l+.b.l.

   INTVEC(:), PTR :: form_3dints_y_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .l_max up to .a.l+.b.l.

   INTVEC(:), PTR :: form_3dints_z_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .l_max up to .a.l+.b.l.

   INTVEC(:), PTR :: form_3dints_yz_rms_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .l_max up to .a.l+.b.l.  This version is for the
   ! reduced multiplication scheme, where the y and z arrays have been collapsed
   ! into one product array.

   end type



   type shell4_type

   SHELL1 :: a
   ! Shell "a" of the quartet

   SHELL1 :: b
   ! Shell "b" of the quartet

   SHELL1 :: c
   ! Shell "c" of the quartet

   SHELL1 :: d
   ! Shell "d" of the quartet

   end type



   type shellquartet_type

   SHELLPAIR, PTR :: ab DEFAULT_NULL
   ! Pair "ab" of the quartet

   SHELLPAIR, PTR :: cd DEFAULT_NULL
   ! Pair "cd" of the quartet

   end type



   type shell1quartet_type

   SHELL, PTR :: a DEFAULT_NULL
   ! Shell "a" of the quartet

   SHELL, PTR :: b DEFAULT_NULL
   ! Shell "b" of the quartet

   SHELL, PTR :: c DEFAULT_NULL
   ! Shell "c" of the quartet

   SHELL, PTR :: d DEFAULT_NULL
   ! Shell "d" of the quartet

   REALVEC(3) :: pos_a
   ! Position of the "a" shell.

   REALVEC(3) :: pos_b
   ! Position of the "b" shell

   REALVEC(3) :: pos_c
   ! Position of the "c" shell

   REALVEC(3) :: pos_d
   ! Position of the "d" shell

   BIN :: ab_nullify
   ! Whether to nullify the precalculated .ab vectors or destroy them, i.e.
   ! whether they were pointer assigned or created/calculated.

   BIN :: cd_nullify
   ! Whether to nullify the precalculated .cd vectors or destroy them, i.e.
   ! whether they were pointer assigned or created/calculated.

   INT :: ab_n_gaussian_pairs
   ! The number of gaussian pair products in the shell pair, .a.n_cc*.b.n_cc

   INT :: ab_l_max
   ! Maximum of the angular momenta on each shell, max(.a.l,.b.l)

   INT :: ab_l_min
   ! Minimum of the angular momenta on each shell, min(.a.l,.b.l)

   INT :: ab_l_sum
   ! Sum of the angular momenta on each shell, .a.l+.b.l

   REAL :: ab_kappa_max
   ! Maximum of .a.cc * .b.cc * kappa_ab for the two electron integrals.

   REALVEC(:), PTR :: ab_exponent_sum DEFAULT_NULL
   ! The sum of all gaussian pair exponents as a flattened array with shell "a"
   ! exponents incrementing most rapidly, .a.ex(:)+.b.ex(:)

   REALVEC(:), PTR :: ab_cc_prefactor DEFAULT_NULL
   ! The contraction coefficient product with an exponential part for each
   ! gaussian pair as a flattened array with shell "a" gaussian components
   ! incrementing most rapidly. Useful for integral evaluation.

   REALVEC(:), PTR :: ab_normalising_factors DEFAULT_NULL
   ! The product of the normalisation coefficients, as a flattened array,
   ! .a.l.normalising_factors * .b.l.normalising_factors.

   REALMAT(:,:), PTR :: ab_pair_center DEFAULT_NULL
   ! The center of the product gaussian of gaussians .a and .b.

   REALMAT(:,:), PTR :: ab_center_diff DEFAULT_NULL
   ! The difference between ab_pair_center and the position of the gaussian of
   ! higher angular momentum out of .a and .b.

   INT :: cd_n_gaussian_pairs
   ! The number of gaussian pair products in the shell pair, .c.n_cc*.d.n_cc

   INT :: cd_l_max
   ! Maximum of the angular momenta on each shell, max(.c.l,.d.l)

   INT :: cd_l_min
   ! Minimum of the angular momenta on each shell, min(.c.l,.d.l)

   INT :: cd_l_sum
   ! Sum of the angular momenta on each shell, .c.l+.d.l

   REAL :: cd_kappa_max
   ! Maximum of .c.cc * .d.cc * kappa_cd for the two electron integrals.

   REALVEC(:), PTR :: cd_exponent_sum DEFAULT_NULL
   ! The sum of all gaussian pair exponents as a flattened array with shell "c"
   ! exponents incrementing most rapidly, .c.ex(:)+.d.ex(:)

   REALVEC(:), PTR :: cd_cc_prefactor DEFAULT_NULL
   ! The contraction coefficient product with an exponential part for each
   ! gaussian pair as a flattened array with shell "c" gaussian components
   ! incrementing most rapidly. Useful for integral evaluation.

   REALVEC(:), PTR :: cd_normalising_factors DEFAULT_NULL
   ! The product of the normalisation coefficients, as a flattened array,
   ! .c.l.normalising_factors * .d.l.normalising_factors.

   REALMAT(:,:), PTR :: cd_pair_center DEFAULT_NULL
   ! The center of the product gaussian of gaussians .c and .d.

   REALMAT(:,:), PTR :: cd_center_diff DEFAULT_NULL
   ! The difference between cd_pair_center and the position of the gaussian of
   ! higher angular momentum out of .c and .d.

   INTMAT3(:,:,:), PTR :: ab_hrr_index_larger DEFAULT_NULL
   ! The mapping of the cartesian angular momenta to a single array, from
   ! max(.a.l,.b.l) to .a.l+.b.l.  In the HRR, this is the index for the
   ! component with the larger angular momentum.

   INTMAT3(:,:,:), PTR :: ab_hrr_index_smaller DEFAULT_NULL
   ! The mapping of the cartesian angular momenta to a single array, from zero
   ! up to .a.l+.b.l.  In the HRR, this is the index for the component with the
   ! smaller angular momentum.

   INTMAT(:,:), PTR :: ab_hrr_components DEFAULT_NULL
   ! Cartesian components of the angular momenta from zero up to .a.l+.b.l.

   INTVEC(:), PTR :: ab_hrr_comp_to_use DEFAULT_NULL
   ! Which cartesian component of angular momentum to use for the HRR for the
   ! angular momenta between zero up to .a.l+.b.l.

   INTVEC(:), PTR :: ab_form_3dints_x_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .ab_l_max up to .a.l+.b.l.

   INTVEC(:), PTR :: ab_form_3dints_y_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .ab_l_max up to .a.l+.b.l.

   INTVEC(:), PTR :: ab_form_3dints_z_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .ab_l_max up to .a.l+.b.l.

   INTVEC(:), PTR :: ab_form_3dints_yz_rms_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .ab_l_max up to .a.l+.b.l.  This version is for the
   ! reduced multiplication scheme, where the y and z arrays have been collapsed
   ! into one product array.

   INTMAT3(:,:,:), PTR :: cd_hrr_index_larger DEFAULT_NULL
   ! The mapping of the cartesian angular momenta to a single array, from
   ! max(.c.l,.d.l) to .c.l+.d.l.  In the HRR, this is the index for the
   ! component with the larger angular momentum.

   INTMAT3(:,:,:), PTR :: cd_hrr_index_smaller DEFAULT_NULL
   ! The mapping of the cartesian angular momenta to a single array, from zero
   ! up to .c.l+.d.l.  In the HRR, this is the index for the component with the
   ! smaller angular momentum.

   INTMAT(:,:), PTR :: cd_hrr_components DEFAULT_NULL
   ! Cartesian components of the angular momenta from zero up to .c.l+.d.l.

   INTVEC(:), PTR :: cd_hrr_comp_to_use DEFAULT_NULL
   ! Which cartesian component of angular momentum to use for the HRR for the
   ! angular momenta between zero up to .c.l+.d.l.

   INTVEC(:), PTR :: cd_form_3dints_x_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .cd_l_max up to .c.l+.d.l.

   INTVEC(:), PTR :: cd_form_3dints_y_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .cd_l_max up to .c.l+.d.l.

   INTVEC(:), PTR :: cd_form_3dints_z_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .cd_l_max up to .c.l+.d.l.

   INTVEC(:), PTR :: cd_form_3dints_yz_rms_indices DEFAULT_NULL
   ! Which indices to use for combining the 2d ERIs to 3d ERIS, for the
   ! angular momenta from .cd_l_max up to .c.l+.d.l.  This version is for the
   ! reduced multiplication scheme, where the y and z arrays have been collapsed
   ! into one product array.

   REAL :: r2ab
   ! The distance between shells .a and .b.

   REAL :: r2cd
   ! The distance between shells .c and .d.

   end type



   type basis_type

   STR(STR_SIZE) :: label
   ! Unique label for the basis set

   INT :: n_shell
   ! No. of shells in the basis, equal to size(shell)

   INT :: n_bf
   ! No. of basis functions for the shell

   INT :: n_prim
   ! No. of primitives for the shell

   SHELLVEC(:), PTR :: shell DEFAULT_NULL
   ! The list of gaussian shells in the basis set

   end type



   type interpolator_type

   STR(STR_SIZE) :: interp_kind DEFAULT("linear")
   ! The kind of interpolation used (usually "linear"). Also allowed is
   ! "logarithmic"

   INT :: n_data DEFAULT(0)
   ! The number of "data_points" and "values" in the table

   REALVEC(:), PTR :: data_point DEFAULT_NULL
   ! The list of data points, from smallest to largest.

   REALVEC(:), PTR :: data_value DEFAULT_NULL
   ! The list of values corresponding to each data value.

   REAL :: spacing DEFAULT(ZERO)
   ! The spacing between data points, if using an even spaced grid.
   ! (this is the case if spacing in non-zero)

   BIN :: finalised DEFAULT(FALSE)
   ! Set TRUE if the object is ready for use

   end type



   type slatershell_type

   INT :: l DEFAULT(0)
   ! l quantum number 

   INT :: n_comp DEFAULT(0)
   ! The number of l-components, normally 2*l+1 for spherical type slater
   ! functions.

   INT :: n_orb DEFAULT(0)
   ! The number of generally contracted orbitals.

   INT :: n_prim DEFAULT(0)
   ! No. of exponents/contractions (i.e. the number of primitives for one
   ! *single* angular momentum shell component, e.g. p_x).

   INTVEC(:), PTR :: n DEFAULT_NULL
   ! The n quantum numbers.

   REALVEC(:), PTR :: z DEFAULT_NULL
   ! The exponent zeta for each slater function

   REALMAT(:,:), PTR :: c DEFAULT_NULL
   ! The contraction coefficient matrix -- this is a generally contracted slater
   ! shell. The size of .dim1 is "n_cc". The size of .dim2 is "n_orb".

   STRVEC(STR_SIZE,:), PTR :: orb_kind DEFAULT_NULL
   ! The kinds of the orbitals (1s, 2s, 2p, etc.), if available.
   ! It's length is "n_orb".

   INTVEC(:), PTR :: occupancy DEFAULT_NULL
   ! The occupany number of each contracted orbital, if available.
   ! It's length is "n_orb".

   end type



   type slaterbasis_type

   STR(STR_SIZE) :: label
   ! Unique label for the basis set

   STR(STR_SIZE) :: configuration
   ! A STR representation of the configuration.

   INT :: n_shell
   ! No. of shells in the basis, equal to size(shell)

   INT :: n_bf
   ! No. of basis functions for the shell

   INT :: n_prim
   ! No. of primitives for the shell

   SLATERSHELLVEC(:), PTR :: shell DEFAULT_NULL
   ! The list of gaussian shells in the basis set

   INTERPOLATOR, PTR :: interpolator DEFAULT_NULL
   ! An interpolator object, used for calculating the atomic density at a
   ! certain radius, from e.g. the coppensbasis

   end type



   type coppensorbital_type

   STR(STR_SIZE) :: orb_kind
   ! The kind of the orbital (1s, 2s, 2p, etc.)

   INT :: occupancy
   ! The number of electrons in the orbital.

   INT :: n_fun
   ! The number of contracted fitting functions

   INTVEC(:), PTR :: n DEFAULT_NULL
   ! The n quantum numbers

   REALVEC(:), PTR :: c DEFAULT_NULL
   ! The contraction coefficient

   REALVEC(:), PTR :: z DEFAULT_NULL
   ! The exponent zeta for each slater function

   end type



   type coppensbasis_type

   STR(STR_SIZE) :: label
   ! The basis label

   INT :: n_orb
   ! The number of orbitals in the basis

   INT :: n_prim
   ! The number of primitive functions in the basis

   COPPENSORBITALVEC(:), PTR :: orbital DEFAULT_NULL
   ! The list of fitted Slater atomic orbitals, Coppens style.

   INTERPOLATOR, PTR :: interpolator DEFAULT_NULL
   ! An interpolator object, used for calculating the atomic density at a
   ! certain radius, from e.g. the coppensbasis

   end type



   type atom_type

   STR(STR_SIZE) :: label DEFAULT("?")
   ! The label for the atom (not necessarily unique)

   INT :: atomic_number
   ! The atomic number

   REALVEC(3) :: pos
   ! Atom position

   STR(STR_SIZE) :: axis_system DEFAULT("cartesian")
   ! Specifies the coordinate axis system

   REAL :: U_iso DEFAULT(ZERO)
   ! The isotropic thermal smearing value for the atom

   REALMAT(3,3) :: thermal_tensor DEFAULT(ZERO)
   ! The thermal tensor for the atom

   STR(STR_SIZE) :: thermal_axis_system DEFAULT("cartesian")
   ! Specifies the thermal tensor coordinate system

   STR(STR_SIZE) :: basis_label DEFAULT(" ")
   ! The label of the basis set to match to.

   BASIS, PTR :: basis DEFAULT_NULL
   ! The basis for the atom

   SLATERBASIS, PTR :: slaterbasis DEFAULT_NULL
   ! The Slater function basis set for the atom. This includes occupancies and
   ! can be used for Hirshfeld surface plots and sum-of-spherical atoms densities.

   COPPENSBASIS, PTR :: coppensbasis DEFAULT_NULL
   ! The Coppens-style fitted relativistic orbital basis for the atom. This is
   ! used for Hirshfeld surface plots and sum-of-spherical atoms densities.

   OPMATRIX, PTR :: density_matrix DEFAULT_NULL
   ! The density matrix for the atom

   OPMATRIX, PTR :: natural_orbitals DEFAULT_NULL
   ! The natural orbitals for the atom

   OPVECTOR, PTR :: occupation_numbers DEFAULT_NULL
   ! The occupation numbers for the atom

   REAL :: energy
   ! The energy of the isolated atom

   INT :: group
   ! An integer which describes thr group to which the atom belongs

   INT :: sequence_number DEFAULT(0)
   ! The sequence number (unique within on molecule, only one chain allowed yet)

   STR(STR_SIZE) :: residue_atom_name DEFAULT("?")
   ! The unique name for the atom in its (protein-)residue

   STR(STR_SIZE) :: residue_name DEFAULT("UNK")
   ! The residue name (must be part of residue table for pdbfile as input)

   STR(STR_SIZE) :: mm_forcefield_name DEFAULT("?")
   ! The name of the forcefield (e.g. amber, sybyl,...)

   STR(STR_SIZE) :: mm_atom_type DEFAULT("?")
   ! The atom type, used to define the force field potential

   REAL :: mm_charge DEFAULT(ZERO)
   ! The atomic charge (relative to the force field!)

   REALVEC(3) :: restraining_position DEFAULT(ZERO)
   ! A position used for restrained geometry optimization

   REAL :: restraining_force_constant DEFAULT(ZERO)
   ! A force constant used for restrained geometry optimisations

   REAL :: site_occupancy DEFAULT(ONE)
   ! The crystallographic site occupancy.

   end type



   type plotgrid_type

   STR(STR_SIZE) :: plot_kind DEFAULT(" ")
   ! The type of plot calculation wanted

   INT :: orbital DEFAULT(0)
   ! The orbital to plot (if any)

   INT :: n_x DEFAULT(PLOTGRID_NX)
   ! The number of points on the x-axis

   INT :: n_y DEFAULT(PLOTGRID_NY)
   ! The number of points on the y-axis

   INT :: n_z DEFAULT(PLOTGRID_NZ)
   ! The number of points on the z-axis

   INT :: n_pt DEFAULT(0)
   ! The total no. of points in the plot

   ATOMVEC(:), PTR :: atom DEFAULT_NULL
   ! List of atoms which can be used to define grid aces and positions.

   INT :: centre_atom DEFAULT(0)
   ! Use this atom as the centre of the plot

   INT :: x_atom_1, x_atom_2
   ! These atoms define the x-axis of the plot

   INT :: y_atom_1, y_atom_2
   ! These atoms define the y-axis of the plot (made orthogonal to the x-axis)

   INT :: z_atom_1, z_atom_2
   ! These atoms define the z-axis of the plot.

   REAL :: del DEFAULT(ONE)
   ! The distance between axis points in the plot

   REALVEC(3) :: centre DEFAULT(ZERO)
   ! Centre of the plot

   REALVEC(3) :: origin DEFAULT(ZERO)
   ! Bottom left point of the plot

   REALVEC(3) :: x_axis DEFAULT(PLOTGRID_X_AXIS)
   ! x-axis of the plot

   REALVEC(3) :: y_axis DEFAULT(PLOTGRID_Y_AXIS)
   ! y-axis of the plot

   REALVEC(3) :: z_axis DEFAULT(PLOTGRID_Z_AXIS)
   ! z-axis of the plot

   REALVEC(3) :: width DEFAULT(PLOTGRID_WIDTH)
   ! The widths (in a.u.) of each axis of the plot

   REALVEC(3) :: offset DEFAULT(ZERO)
   ! The centre of the plot is offset by this amount

   BIN :: x_width_set DEFAULT(TRUE)
   ! If False the x_width, i.e. width(1), is to be calculated from the size of
   ! the inputted x_axis vector; or if a width has *not* been inputted.

   BIN :: y_width_set DEFAULT(TRUE)
   ! If False the y_width, i.e. width(2), is to be calculated from the size of
   ! the inputted y_axis vector; or if a width has *not* been inputted.

   BIN :: z_width_set DEFAULT(TRUE)
   ! If False the z_width, i.e. width(3), is to be calculated from the size of
   ! the inputted z_axis vector; or if a width has *not* been inputted.

   BIN :: x_axis_defined DEFAULT(FALSE)
   ! Flag set true if the user has defined the x_axis in the input

   BIN :: y_axis_defined DEFAULT(FALSE)
   ! Flag set true if the user has defined the y_axis in the input

   BIN :: z_axis_defined DEFAULT(FALSE)
   ! Flag set true if the user has defined the z_axis in the input

   REALVEC(3) :: box_centre
   ! The centre of the bounding box, defined as the centre of an optional
   ! inputted atom list.

   REALVEC(3) :: bounding_box
   ! The bounding box for the molecule, defined from an optional atom list.

   REAL :: box_scale_factor
   ! Scale factor for the bounding box.  Usually leave it at 1.

   REALMAT(3,3) :: box_axes
   ! The axes of the bounding box, defined as the principal axes of the shape
   ! tensor (essentially the unit weighted moment of inertia tensor). Defined
   ! from an optionally inputted atom list.

   REAL :: desired_separation DEFAULT(ZERO)
   ! The desired separation between grid points.

   end type



   type dftgrid_type

   STR(STR_SIZE) :: spherical_grid_kind DEFAULT(DFTGRID_SPHERICAL_GRID_KIND)
   ! Identifier for the kind of spherical grid

   STR(STR_SIZE) :: radial_grid_kind DEFAULT(DFTGRID_RADIAL_GRID_KIND)
   ! Type of radial grid

   INT :: spherical_grid_order DEFAULT(DFTGRID_SPHERICAL_GRID_ORDER)
   ! Order of the spherical grid

   INT :: radial_grid_order DEFAULT(DFTGRID_RADIAL_GRID_ORDER)
   ! Order of the radial grid

   INT :: n_spherical_pts DEFAULT(0)
   ! No of spherical grid points

   INT :: n_radial_pts DEFAULT(0)
   ! No of radial grid points

   INT :: n_pts DEFAULT(0)
   ! No of integration grid points

   REAL :: becke_m_partition_power DEFAULT(DFTGRID_BECKE_M_PARTITION_POWER)
   ! Used in smoothing the partition boundary

   REAL :: gauss_chebyshev_alpha DEFAULT(DFTGRID_GAUSS_CHEBYSHEV_ALPHA)
   ! Gauss-Chebychev radial grid parameters

   REAL :: gauss_chebyshev_m DEFAULT(DFTGRID_GAUSS_CHEBYSHEV_M)
   ! ?

   REAL :: euler_maclaurin_alpha DEFAULT(DFTGRID_EULER_MACLAURIN_ALPHA)
   ! Euler-Maclaurin radial grid parameters

   REAL :: euler_maclaurin_m DEFAULT(DFTGRID_EULER_MACLAURIN_M)
   !

   ARCHIVE :: archive
   ! Archive to store the generated grids

   BIN :: finalized DEFAULT(FALSE)
   ! Set to true if the DFTGRID has been "set" using set_grid_data

   REALMAT(:,:), PTR :: single_atom_points DEFAULT_NULL
   ! The dft grid points at the origin for a single atom.

   REALVEC(:), PTR :: single_atom_weights DEFAULT_NULL
   ! The dft grid weights at the origin for a single atom.

   end type



   type irrep_type

   STR(4) :: label
   ! Irrep label

   INT :: dimension
   ! Irrep dimension

   REALVEC(:), PTR :: character DEFAULT_NULL
   ! Characters for the irrep

   REALMAT3(:,:,:), PTR :: mat DEFAULT_NULL
   ! Representation matrices for the irrep

   end type



   type pointgroup_type

   STR(4) :: symbol
   ! symmetry symbol

   STR(4) :: ID_symbol
   ! group id symbol

   INT :: ID_number
   ! group id number

   INT :: axis_order
   ! principal axis order

   INT :: order
   ! order of the group

   INT :: n_irrep
   ! no of irreducible representations

   INT :: n_gen
   ! No. of generators

   BIN :: has_complex_irreps
   ! true if the group has complex irreps

   INTMAT(:,:), PTR :: table DEFAULT_NULL
   ! group multiplication table

   REALMAT3(:,:,:), PTR :: mat DEFAULT_NULL
   ! 3x3 representation matrices

   REALMAT3(:,:,:), PTR :: ptr DEFAULT_NULL
   ! 3x3 representation matrices for p functions, same as mat

   REALMAT3(:,:,:), PTR :: dtr DEFAULT_NULL
   ! 6x6 representation matrices for d functions

   REALMAT3(:,:,:), PTR :: ftr DEFAULT_NULL
   ! 10x10 representation matrices for f functions

   REALMAT3(:,:,:), PTR :: gtr DEFAULT_NULL
   ! 15x15 representation matrices for g functions

   INTVEC(:), PTR :: inverse DEFAULT_NULL
   ! Indices of inverse group elements

   IRREPVEC(:), PTR :: irrep DEFAULT_NULL
   ! List of irrrducible representations

   end type



   type unitcell_type

   REALVEC(3) :: angle
   ! The cell angles  (in radians)

   REALVEC(3) :: length
   ! The cell lengths (in bohr)

   REAL :: volume
   ! The cell volumes (bohr^3)

   REALMAT(3,3) :: direct_matrix
   ! Direct lattice cell matrix (bohr). The columns are vectors of the three
   ! cell axes.

   REALMAT(3,3) :: inverse_matrix
   ! Inverse direct lattice cell matrix (bohr^{-1})

   REALMAT(3,3) :: reciprocal_matrix
   ! Reciprocal lattice cell matrix (bohr^{-1}).

   REALMAT(3,3) :: direct_U_matrix
   ! Converts thermal tensors from crystal to cartesian systems.

   REALMAT(3,3) :: reciprocal_U_matrix
   ! Converts thermal tensors from cartesian to crystal systems.

   BIN :: info_made
   ! Set TRUE if all the above information is consistent.

   end type



   type spacegroup_type

   STR(STR_SIZE) :: IT_symbol
   ! International Table (Hermann-Maguin) symmetry symbol

   INT :: IT_group_number
   ! International tables group number

   STR(STR_SIZE) :: Hall_symbol
   ! Hall notation symbol

   STR(STR_SIZE) :: HM_symbol
   ! Hermann-Mauguin notation symbol

   STR(STR_SIZE) :: Schoenflies_symbol
   ! Hermann-Mauguin notation symbol

   STR(1) :: lattice_symbol
   ! Lattice symmetry symbol

   INT :: lattice_symbol_index
   ! Lattice symbol index number

   STR(STR_SIZE) :: lattice_type
   ! Lattice type

   BIN :: centrosymmetric
   ! True if center of symmetry present

   INTVEC(3) :: axis_order
   ! Order of each crystal axis

   STRVEC(len=1,3) :: axis_symbol
   ! Superscript rotation axis symbol

   INTVEC(3) :: axis_symbol_index
   ! Indicates the numberical index corresponding to the axis symbol x, y, z ...

   BINVEC(3) :: has_axis_bar
   ! True if bar for this axis

   BINVEC(3) :: has_translation
   ! True if translation subscript present for this axis

   STRMAT(len=1,3,3) :: translation_symbol
   ! Subscript translation symbols for each axis

   INTVEC(3) :: origin_shift
   ! Subscript translation symbols for each axis

   INT :: nL
   ! No. of translational lattice generators

   INT :: nR
   ! No. of rotation generators

   INT :: nG
   ! No. of generators

   INT :: n_seitz
   ! No. of Seitz matrices

   REALMAT3(:,:,:), PTR :: seitz DEFAULT_NULL
   ! The spacegroup Seitz matrices.

   INT :: n_unique
   ! No. of equivalent positions unrelated by traslation or inversion

   INTVEC(:), PTR :: unique_symop DEFAULT_NULL
   ! Index of unique symops not related by translation or inversion

   INTVEC(:), PTR :: map_to_unique DEFAULT_NULL
   ! Maps symop to a unique symop. Negative indicates inversion

   BIN :: analysed
   ! Set TRUE if the spacegroup symbol has been succesfully analysed

   end type



   type crystal_type

   STR(STR_SIZE) :: data_kind
   ! Kind of diffraction experiment used.

   SPACEGROUP :: spacegroup
   ! The crystal spacegroup

   UNITCELL :: unitcell
   ! The crystal unit cell

   INT :: n_fragment_atoms
   ! No. of atoms in the inputted cell fragment, used to calculate structure
   ! factors, or do wavefunction fitting.

   REALMAT(:,:), PTR :: fragment_geometry DEFAULT_NULL
   ! The geometry for a molecular fragment in the crystal (in the crystal
   ! coordiante system) used to calculate structure factors or do wavefunction
   ! fitting. IMPORTANT NOTE --- this is not to be confused with the
   ! "asymmetric_unit_geometry"; it may include symmetry non-unique atoms.

   INT :: n_fragment_cell_atoms
   ! No. of atoms in the whole unit cell which are generated from
   ! "fragment_geometry" by spacegroup symmetry operations. This will be the whole
   ! unit cell if the "fragment_geometry" includes all asymmetric unit cell.

   REALMAT(:,:), PTR :: fragment_cell_geometry DEFAULT_NULL
   ! The geometry of all atoms in the unit cell generated from
   ! "fragment_geometry" by the spacegroup symmetry operations. It will usually
   ! be the full "unit_cell_geometry", but may not be.

   INTVEC(:), PTR :: symop_for_fragment_cell_atom DEFAULT_NULL
   ! symop_for_unit_cell_atom :: INTVEC* DEFAULT_NULL
   ! "symop_for_fragment_cell_atom(a)" is the index of the spacegroup symmetry
   ! operation that generates atom position "fragment_cell_geometry(:,a)" from a
   ! unique atom position in "fragment_geometry".

   INTVEC(:), PTR :: atom_for_fragment_cell_atom DEFAULT_NULL
   ! fragment_atom_for :: INTVEC* DEFAULT_NULL
   ! "atom_for_fragment_cell_atom(a)" is the index of the unique fragment atom
   ! in "fragment_geometry" which generates atom position
   ! "fragment_cell_geometry(:,a)", using "symop_for_fragment_cell_atom(a)".

   INT :: n_unique_fragment_atoms
   ! n_unique_unit_cell_atoms :: INT
   ! No. of unique fragment atoms.

   INTVEC(:), PTR :: unique_fragment_atom DEFAULT_NULL
   ! unique_atom :: INTVEC* DEFAULT_NULL
   ! The list of symmetry-unique fragment atoms. This list may be a subset of
   ! the asymmetric unit -- but it usually will be the asymmetric unit.

   INTVEC(:), PTR :: unique_symop_for_fragment_atom DEFAULT_NULL
   ! unique_atom_symop_for :: INTVEC* DEFAULT_NULL
   ! "unique_symop_for_fragment_atom(a)" is the index of the spacegroup symmetry
   ! operation which maps the atom "unique_atom_for_fragment_atom(a)" onto the
   ! "a"-th atom position, fragment_geometry(:,a).

   INTVEC(:), PTR :: unique_atom_for_fragment_atom DEFAULT_NULL
   ! unique_atom_for :: INTVEC* DEFAULT_NULL
   ! "unique_atom_for_fragment_atom(a)" is the index of the symmetry-unique atom
   ! position in "fragment_geometry" which generates the position
   ! "fragment_geometry(:,a)".

   INT :: n_reduced_symops
   ! No. of symops needed to make the unit_cell_geometry from fragment_geometry
   ! OBSOLETE

   INTVEC(:), PTR :: reduced_symop DEFAULT_NULL
   ! Indices of the reduced symops in the spacegroup seitz list, i.e. those
   ! symops which make distinctly different fragment_geometries when transformed
   ! back to the unit cell. The unit symop is part of the reduced_symop list.
   ! OBSOLETE

   INT :: n_cluster_symops
   ! No. of cluster symops which generate distinctly different fragment geometries.
   ! OBSOLETE

   INTVEC(:), PTR :: cluster_symop DEFAULT_NULL
   ! Indices of the cluster symops in the spacegroup seitz list. These are the
   ! same as the reduced_symop's, except that the fragment_geometry and its
   ! symmetry transform are not mapped back to the unit cell
   ! OBSOLETE

   INT :: n_inverted_symops
   ! Number of symmetry operations related by inversion

   INTVEC(:), PTR :: inverted_symop DEFAULT_NULL
   ! Indices of the unique symops related by inversion

   INTVEC(:), PTR :: translated_symop DEFAULT_NULL
   ! Indices of the unique symops related by translation

   INT :: n_unique_SF_symops
   ! Number of unique symmetry operations not related by inversion or
   ! translation. This is used to save work in structure factor (SF)
   ! calculations.

   INTVEC(:), PTR :: unique_SF_symop DEFAULT_NULL
   ! Indices of the unique symops not related by inversion or translation.
   ! This is used to save work in structure factor (SF) calculations.

   REALVEC(:), PTR :: repetition_factor DEFAULT_NULL
   ! The partition factors for fragment_geometry Useful to get structure
   ! factor contributions from a small portion of the fragment.

   REAL :: Z
   ! The crystallographic Z factor for the molecular cell fragment in the
   ! unitcell

   BIN :: reduced_group_info_made
   ! Set TRUE if the reduced group information has been made

   INT :: n_asymmetric_unit_atoms
   ! No. of atoms in the asymmetric unit of the unit cell.

   REALMAT(:,:), PTR :: asymmetric_unit_geometry DEFAULT_NULL
   ! The asymmetric unit cell geometry. Usually inputted from a CIF file. This
   ! may or may not be the same as fragment_cell_geometry. It is NOT used for
   ! structure factor calculations, but for cluster generation.

   INT :: n_unit_cell_atoms
   ! Total no. of unit cell atoms.

   REALMAT(:,:), PTR :: unit_cell_geometry DEFAULT_NULL
   ! The full unit cell geometry, in the crystal coordiante system, generated
   ! from the asymmetric_unit_geometry.

   INTVEC(:), PTR :: symop_for_unit_cell_atom DEFAULT_NULL
   ! "symop_for_unit_cell_atom(a)" is the index of the spacegroup symmetry
   ! operation that generates atom position "unit_cell_geometry(:,a)" from a
   ! unique atom position in "asymmetric_unit_geometry".

   INTVEC(:), PTR :: atom_for_unit_cell_atom DEFAULT_NULL
   ! "atom_for_unit_cell_atom(a)" is the index of the unique unit cell atom
   ! in "asymmetric_unit_geometry" which generates atom position
   ! "unit_cell_geometry(:,a)", using "symop_for_unit_cell_atom(a)".

   REFLECTIONVEC(:), PTR :: reflections DEFAULT_NULL
   ! The structure factor information

   REAL :: scale_factor
   ! Scale factor to apply to the predicted structure factors

   REAL :: exp_scale_factor
   ! Fixed scale factor to apply to the experimental structure factors

   BIN :: optimise_scale
   ! True if an overall scaler factor is to be optimised, for use in calculating
   ! F_pred

   BIN :: synthesize_sigma_I
   ! True if artificial sigma(I) errors are to be gereated from poisson
   ! statistics and used in calculating agreement statistics

   BIN :: optimise_extinction
   ! True if extinction is to be optimised.

   BIN :: correct_dispersion
   ! True if dispersion is to be corrected

   REAL :: extinction_factor
   ! Secondary extinction factor

   REAL :: wavelength
   ! Experiment wavelength

   STR(STR_SIZE) :: thermal_smearing_model
   ! Thermal smearing model for ft integrals

   STR(STR_SIZE) :: partition_model
   ! Model for partitioning fragments of the molecule

   INT :: n_param
   ! No of fitting parameters used

   end type



   type cluster_type

   REAL :: radius DEFAULT(CLUSTER_RADIUS)
   ! The radius of the cluster. This number determines the maximum acceptable
   ! distance between an atom in fragment_geometry and its crystal transformed
   ! image. It defines the cluster.

   STR(STR_SIZE) :: add_criteria DEFAULT(CLUSTER_ADD_CRITERIA)
   ! The add criteria, i.e. whether to add atoms by whole clusters within a
   ! certain distance (radius) of the starting fragment, or by individual atoms
   ! within a certain distance of the starting fragment.

   BIN :: start_with_fragment DEFAULT(FALSE)
   ! Start building the cluster from the initial fragment geometry

   BIN :: defragment DEFAULT(CLUSTER_DEFRAGMENT)
   ! If TRUE, the cluster ends are defragmented, i.e. any atoms which are bonded
   ! at the ends of the cluster are included into the cluster.

   INT :: n_atoms
   ! The number of atoms in the cluster

   REALMAT(:,:), PTR :: geometry DEFAULT_NULL
   ! The (3 x .n_atoms) sized array of cluster atom positions

   CRYSTAL, PTR :: crystal DEFAULT_NULL
   ! The crystal information used to generate the cluster

   ATOMVEC(:), PTR :: asymmetric_cell_atom DEFAULT_NULL
   ! The atom list data associated with the *asymmetric* unit cell_geometry (see
   ! below) used to generate the cluster atom positions.

   INT :: n_fragment_atoms
   ! The number of fragment atoms used to build the cluster

   REALMAT(:,:), PTR :: fragment_geometry DEFAULT_NULL
   ! The (3 x .n_fragment_atoms) sized array of fragment atom positions used to
   ! generate the cluster.

   INT :: n_symop
   ! The number of symmetry operators used to generate the cluster

   INTMAT(:,:), PTR :: symop DEFAULT_NULL
   ! The (4 x .n_symop) sized list of symmetry operators used to generate the
   ! cluster. symop(1,q) is the index of the Seitz operator, while symop(2:4,q)
   ! is the translation vector applied (in the crystal axis coordinate system).

   REALVEC(3) :: fragment_width
   ! The width of the crystal fragment, to the nearest unit cell

   INTVEC(3) :: fragment_offset
   ! The center point of the fragment_geometry, to the nearest unit cell

   INTVEC(:), PTR :: symop_for_atom DEFAULT_NULL
   ! symop_for_atom(a) is the *index* "s" of the symmetry operation in .symop,
   ! .symop(:,s), used to generate the cluster atom "a", whose positon is given
   ! by .geometry(:,a).

   INTVEC(:), PTR :: parent_for_atom DEFAULT_NULL
   ! parent_for_atom(a) is the index of the unique asymmetric unit cell atoms in
   ! *used to generate* the cluster atom "a" using one of the symmetry
   ! operations in .symop (specifically the symop with index .symop_for_atom(a)).

   INTMAT(:,:), PTR :: atom_for_cell_atom DEFAULT_NULL
   ! atom_for_cell_atom(a,s) is the index of the cluster atom in .geometry which
   ! is *generated by* asymmetric cell atom "a", whose position is in
   ! .crystal.asymmetric_cell_geometry(:,a), by the symop with index "s",
   ! .symop(:,s). This is the inverse information array of parent_for_atom(a).

   REALVEC(:), PTR :: minimum_distance_to_atom DEFAULT_NULL
   ! minimum_distance_to_atom(a) is the minimum distance from the cluster
   ! atom "a", whose position is given in .geometry(:,a), to the crystal
   ! fragment, whose geometry is given in .fragment_geometry

   INTVEC(:), PTR :: closest_fragment_atom_to_atom DEFAULT_NULL
   ! closest_fragment_atom_to_atom(a) is the index of the atom in
   ! .fragment_geometry which is closest to the cluster atom "a", whose positon
   ! is .geometry(:,a).

   BINVEC(:), PTR :: is_fragment_atom DEFAULT_NULL
   ! is_fragment_atom(a) is TRUE if .geometry(:,a) is the position of a fragment
   ! atom, i.e. if "a" is the index of a fragm,ent atom.

   BINVEC(:), PTR :: symop_is_redundant DEFAULT_NULL
   ! symop_is_redundant(q) is TRUE if .symop(:,q) does not generate any new
   ! cluster atom (cluster atom positions are stored in .geometry). Instead,
   ! symops earlier in the .symop list are able to generate the atoms that symop
   ! "q" can generate.

   REALVEC(:), PTR :: partition_factor DEFAULT_NULL
   ! A list of partition factors which can be applied to a density matrix in
   ! order to partition it. This array can be generated automatically, or it can
   ! be explicitly inputted.

   BIN :: info_made DEFAULT(FALSE)
   ! Set to true if the routine make_info has been called

   end type



   type scfdata_type

   STR(STR_SIZE) :: scf_kind
   ! The kind of SCF calculation to perform

   STR(STR_SIZE) :: dft_exchange
   ! The DFT exchange functional to be used

   STR(STR_SIZE) :: dft_correlation
   ! The DFT correlation functional to be used

   BIN :: dft_non_local_exchange
   ! TRUE if the exchange functional is non local, otherwise false.

   BIN :: dft_non_local_correlation
   ! TRUE if the correlation functional is non local, otherwise false.

   STR(STR_SIZE) :: initial_density
   ! The kind of density matrix to start the SCF calc

   STR(STR_SIZE) :: initial_mos
   ! The kind of initial MO's to start the SCF calc

   REAL :: nuclear_energy
   ! Nuclear repulsion energy for the associated molecule

   REAL :: kinetic_energy
   ! The kinetic energy for the associated molecule

   REAL :: dft_energy_correction
   ! The DFT energy correction to the SCF energy for the associated molecule

   REAL :: energy
   ! The SCF energy for the associated molecule

   REAL :: old_energy
   ! The SCF energy from the previous SCF cycle

   REAL :: difference
   ! The change in the SCF energy between cycles

   REAL :: convergence
   ! A number which measures the convergnece of the SCF

   REAL :: diis_convergence
   ! A limit below which the DIIS is deemed converged

   INT :: diis_start_iteration DEFAULT(SCFDATA_DIIS_START_ITERATION)
   ! Which iteration of the SCF to start the DIIS

   BIN :: using_rough_convergence
   ! Whether to apply lower integral accuracy

   REAL :: rough_convergence
   ! How much to converge to before increasing integral accuracy

   REAL :: rough_diis_convergence
   ! How much to converge the DIIS error to before increasing integral accuracy

   INT :: iteration
   ! The interation count for the SCF procedure

   INT :: total_iterations
   ! The total interation count for the SCF procedure, which does not get reset
   ! after each lambda increment.

   INT :: lambda_iteration
   ! Lambda iteration count for the x-ray SCF procedure

   INT :: min_iterations
   ! The minimum number of SCF iterations to perform

   INT :: max_iterations
   ! The maximum number of SCF iterations to perform

   REAL :: lambda
   ! The initial lambda value to use in an x-ray SCF procedure

   REAL :: lambda_max
   ! The maximum lambda value to use in an x-ray SCF procedure

   REAL :: lambda_step
   ! The value to step the lambda value between lambda cycles in an x-ray SCF
   ! calc.

   REAL :: fit_value
   ! The value of E + lambda * chi2 in an x-ray SCF calc.

   REAL :: old_fit_value
   ! The value of E + lambda * chi2 of the previous iteration in an x-ray SCF
   ! calc.

   REAL :: F_chi2
   ! Chi^2 agreement statistic for an x-ray SCF calc

   REAL :: old_F_chi2
   ! Chi^2 agreement statistic for an x-ray SCF calc of the previous iteration

   REAL :: F_gof
   ! Goodness-of-fit agreement statistic for x-ray SCF calc

   REAL :: F_r_factor
   ! R-factor agreement statistic for x-ray SCF calc

   REAL :: F_weighted_r_factor
   ! The weighted r-factor agreement statistic

   BIN :: test
   ! Test flag. Set True if some test procedure is to be executed

   BIN :: direct
   ! True if using direct SCF

   BIN :: using_delta_build
   ! True if using an incremental (delta) fock build as advocated by Almlof.

   BIN :: using_fock_diis
   ! True if using DIIS extrapolation for fock matrices

   BIN :: using_MO_diis
   ! True if using DIIS extrapolation for molecular orbitals

   BIN :: using_damping
   ! True if using damping

   BIN :: using_level_shift
   ! True if using level shifting

   BIN :: using_camp_king
   ! True if using Camp-King converger

   INT :: camp_king_iters
   ! How many iterations the Camp-King converger took in SCF cycle

   BIN :: using_dynamic_damping
   ! True if using the Dynamic Damper

   REAL :: dynamic_damp_factor
   ! Damp factor used by the Dynamic Damper

   DIIS :: diis
   ! For diis extrapolation (usually Fock matrix DIIS extrapolation)

   REAL :: diis_error
   ! The DIIS error

   REAL :: old_diis_error
   ! The DIIS error of the previous iteration

   BIN :: using_diis_auto_start
   ! Set TRUE if starting DIIS automatically based on the size of
   ! the diis error

   INT :: damp_finish
   ! Iteration when density matrix damping is turned off

   REAL :: damp_factor
   ! The damping factor to use

   INT :: level_shift_finish
   ! Iteration when level shifting is turned off

   REAL :: level_shift
   ! Value to level shift the virtual orbitals

   BIN :: output
   ! True if output is wanted

   BIN :: nddo
   ! Neglect of diatomic differential overlap

   BIN :: nudo
   ! Neglect of unconnected differential overlap

   BIN :: pie
   ! Projective integral expansion method (PIE) developed by Mayer.
   ! See Mayer, CPL 332, 381 (2000).

   BIN :: using_bl_term
   ! Switch on/off the B.L term (complex SCF reqd.)

   BIN :: using_bs_term
   ! Switch on/off the B.S term

   BIN :: using_bs_t_term
   ! Switch on/off the (B.S) T_e term

   BIN :: using_aa_term
   ! Switch on/off the A.A diamagnetic term

   BIN :: using_1e_sl_term
   ! Switch on/off the 1 electron S.L term

   BIN :: using_1e_srxa_term
   ! Switch on/off the 1 electron diamagnetic term

   BIN :: using_1e_zora_term
   ! Switch on/off the 1 electron ZORA terms

   REAL :: sl_1e_factor
   ! Factor to apply to the 1-electron S.L terms

   BIN :: using_2e_sl_term
   ! Switch on/off the 2 electron S.L terms

   REAL :: sl_2e_factor
   ! Factor to apply to the 2-electron S.L terms

   REAL :: eri_limit
   ! Cutoff for the two electron integrals

   REAL :: old_eri_cutoff
   ! Previous iteration's cutoff for the two electron integrals, used for
   ! detecting if an incremental fock build is required.

   REALVEC(3) :: quantization_axis DEFAULT(ZERO)
   ! Quantization axis for GCHF, if using initial MO's as a guess

   BIN :: group
   ! Set TRUE if doing a noninteracting group SCF calculation

   BIN :: using_MO_gradient_update
   ! Set TRUE if using the gradient of the orbital coefficients to
   ! update the coefficients

   REAL :: MO_gradient_stepsize
   ! The stepisize to use for updating the orbitals

   REAL :: max_update_stepsize
   ! The maximum update stepsize to use for updating any SCF object, e.g. the orbitals

   end type



   type colour_type

   STR(STR_SIZE) :: name
   ! The standard colour name for this colour.

   INTVEC(3) :: RGB255
   ! The RGB triple for this colour as a triple of integers between 0 and 255.

   end type



   type colourfunction_type

   INT :: n_data
   ! The number of data values (and their associated colours) used to make
   ! the colour function

   REALVEC(:), PTR :: data DEFAULT_NULL
   ! The list of data values, from smallest to largest

   REALMAT(:,:), PTR :: RGB DEFAULT_NULL
   ! The RGB values corresponding to each data value. Their norm should be
   ! between zero and 3.

   BIN :: finalised
   ! Set TRUE if the object is ready for use

   end type



   type marchingcube_type

   REALMAT(3,0:7) :: vertex_pos
   ! A (3 x 0:7) dimensioned list of the actual coordinates of each cube vertex.
   ! The second index is the standard marching cubes vertex number.

   REALVEC(0:7) :: value_at_vertex
   ! A (0:7) dimensioned list of the values of the function at each cube vertex.

   REALMAT(3,0:7) :: vertex_gradient
   ! A (3 x 0:7) dimensioned list of the vertex gradients at each cube vertex.
   ! The second index is the standard marching cubes vertex number.

   REALMAT3(3,3,0:7) :: vertex_hessian
   ! A (3 x 3 x 0:7) dimensioned list of the hessian at each cube vertex.
   ! The third index is the standard marching cubes vertex number.

   REAL :: side_length
   ! The length of each side of the cube

   REAL :: iso_value
   ! The isovalue to be used for the isosurface

   REAL :: accuracy
   ! The function accuracy to which each isosurface point is determined

   INT :: case
   ! The case number of this marching cube, for lookup in edge_table and
   ! triangle_table

   INT :: interior_case
   ! The case number corresponding to a cube being wholly within a surface.
   ! Normally this is when the interior of the surface has larger values than
   ! the exterior, and in this case the default is 0.

   INT :: exterior_case
   ! The case number corresponding to a cube being wholly outside a surface.
   ! Normally this is when the interior of the surface has smaller values than
   ! the exterior, and in this case the default is 255.

   INT :: edge_bit_string
   ! The edge bit string for the marching cube. This is just the appropriate
   ! element of the edge_table i.e. if the bit is set, then that edge crosses
   ! the isosurface and must be interpolated.

   INT :: skip_bit_string
   ! A bit string which tells whether to skip processing certain cube edges.
   ! Can be used when certain edge vertex points are already done.

   INT :: cube_bit_string
   ! The grad bit string for the marching cube. This tells which *vertices* of
   ! the cube cross the isosurface and must be interpolated.

   INT :: n_edge
   ! The number of marching cube edges that need to be considered on this
   ! marching cube

   INT :: n_triangle
   ! The number of triangles formed on this marching cube

   INT :: n_pt
   ! The number of points up until this cube was analysed. Used to keep a
   ! list of unique indices to define triangle vertices and points.

   INTMAT(3,5) :: triangle_edge_index
   ! The (3 x .n_triangle) dimensioned list of 3-edges on the marching cube
   ! used to form triangles. This is just the appropriate row from the
   ! triangle_table. The maximum number of triangles is 5.

   INTMAT(3,5) :: triangle_vertex_index
   ! The (3,.n_triangle) dimensioned list of groups of 3 *unique* triangle
   ! vertex *indices*. These indices point to a unique list of points. The
   ! maximum number of triangles is 5.

   REALMAT(3,0:11) :: edge_vertex_pos
   ! A (3 x 0:11) dimensioned list of the interpolated triangle vertex
   ! coordinates on each edge of the cube (if that edge is needed).

   REALMAT(3,0:11) :: edge_vertex_gradient
   ! A (3 x 0:11) dimensioned list of the interpolated triangle vertex normals
   ! on each edge of the cube (if that edge is needed).

   REALMAT3(3,3,0:11) :: edge_vertex_hessian
   ! A (3 x 3 x 0:11) dimensioned list of the interpolated triangle vertex
   ! hessians on each edge of the cube (if that edge is needed).

   REALVEC(0:11) :: edge_mean_curvature
   ! A (0:11) dimensioned list of the (interpolated) mean curvatures on each
   ! edge of the cube that crosses the isosurface (if that edge is needed).

   REALVEC(0:11) :: edge_gaussian_curvature
   ! A (0:11) dimensioned list of the (interpolated) gaussian curvatures on each
   ! edge of the cube that crosses the isosurface (if that edge is needed).

   INTVEC(0:11) :: edge_vertex_index
   ! A (0:11) dimensioned list of the *unique* triangle vertex *indices* DEFAULT_NULL
   ! for each edge of the marching cube (if that edge is needed).

   INTVEC(12) :: vertex_edge_index
   ! A list of the edge indices for each unique triangle vertex index.
   ! Essentially, this is the reverse mapping of edge_vertex_index.

   end type



   type isosurface_type

   STR(STR_SIZE) :: iso_kind
   ! The kind of isosurface plot, if known. This helps in deciding which way the
   ! normals of the isosurface should point.

   STR(STR_SIZE) :: triangulation_method
   ! The method used to triangulate the isosurface.

   REAL :: iso_value
   ! The isovalue to be used for the isosurface

   PLOTGRID :: grid
   ! The isosurface plotgrid

   INT :: n_pt
   ! The number of isosurface points

   REALMAT(:,:), PTR :: point DEFAULT_NULL
   ! A (3 x n_pt) list of points on the isosurface

   INT :: n_face
   ! The number of triangulated faces on the isosurface

   INTMAT(:,:), PTR :: face DEFAULT_NULL
   ! A (3 x n_face) list of the triangular faces of the surface. Each face is
   ! represented by three integers which move in an anticlockwise direction when
   ! viwed from the outside.

   REALMAT(:,:), PTR :: point_gradient DEFAULT_NULL
   ! A (3 x n_pt) list of the function gradient vectors for every point on the
   ! isosurface

   REALVEC(:), PTR :: point_mean_curvature DEFAULT_NULL
   ! A (3 x n_pt) list of the mean surface curvatures for every point on the
   ! isosurface

   REALVEC(:), PTR :: point_gaussian_curvature DEFAULT_NULL
   ! A (3 x n_pt) list of the gaussian surface curvatures for every point on the
   ! isosurface

   REAL :: volume
   ! The best estimate of the interior volume of the isosurface (the average of
   ! .volume_min and .volume_max).

   REAL :: volume_min
   ! A lower bound to the interior volume of the isosurface.

   REAL :: volume_max
   ! An upper bound to the interior volume of the isosurface.

   INT :: n_skip
   ! The number of function evaluations skipped (recursive method only)

   INT :: level
   ! The current level to which the initial box has been divided, in the
   ! recursive marching cube algorithm.

   INT :: final_level
   ! The final level to which the initial box must be divided, in the
   ! recursive marching cube algorithm.

   INT :: scan_level
   ! The level to which the initial box must be divided, in the recursive
   ! marching cube algorithm, to achieve resolution of all important features.

   REAL :: del
   ! The current box side length in the recursive marching cubes algorithm.

   INT :: x
   ! The (partial) x-coordinate of the box in the recursive marching cubes
   ! algorithm.  This is a binary number, with each bit from the right
   ! representing a segment double the size to which the current box belongs.

   INT :: y
   ! The (partial) y-coordinate of the box in the recursive marching cubes
   ! algorithm.  This is a binary number, with each bit from the right
   ! representing a box double the size to which the current box belongs.

   INT :: z
   ! The (partial) z-coordinate of the box in the recursive marching cubes
   ! algorithm.  This is a binary number, with each bit from the right
   ! representing a box double the size to which the current box belongs.

   INTVECINTVECHASH, PTR :: hash DEFAULT_NULL
   ! A hash table storing marching cube edge_vertex_index information as a
   ! function of the cube coordinates, [.x,.y,.z]

   BIN :: big_interior
   ! Set TRUE if the interior of the isosurface is bigger than the exterior,
   ! i.e. if the point_normals are to be reversed on output. This switch could
   ! probably be determined automatically, assuming the botton left had corner
   ! of the plot (the first point) was "outside".

!  shift :: REALVEC* DEFAULT_NULL
!  ! A list of distances representing how far each point shifted from its
!  ! estimate to come to the isosurface.

!  adjoining_face :: INTMAT* DEFAULT_NULL
!  ! A (3 x n_face) list of the three adjoining faces for a particular face

!  adjoining_edge :: INTMAT* DEFAULT_NULL
!  ! A (3 x n_face) list of the adjoining edge for the adjoining faces for
!  ! a particular face

!  ok :: BINVEC* DEFAULT_NULL
!  ! A list of switches telling if each face is acceptably smooth.

!  ok_neighbours :: BINVEC* DEFAULT_NULL
!  ! A list of switched telling if all three neighbours of each face is acceptably smooth.

!  n_skip :: INT
!  ! The number of faces which are acceptapbly smooth, and can be skippped

!  smallness :: REAL
!  ! The maximum acceptable distance between triangulated points.
!  ! Used as a face smallness criteria.

!  flatness :: REAL
!  ! The maximum acceptable distance between a triangulated point and its ray origin
!  ! i.e. its "shift" (see above).  Used as a face flatness criteria.

!  accuracy :: REAL
!  ! The accuracy to which each isosurface point is determined

   STR(STR_SIZE) :: surface_property
   ! The name of a surface property to plot or calculate

   REALVEC(:), PTR :: surface_property_values DEFAULT_NULL
   ! The values of .surface_property as evaluated on the surface.

   BIN :: chop_surface_property_range
   ! Whether to use surface_property_cutoff_range, rather than scaling the
   ! smallest value to zero and the largest to one.

   REALVEC(2) :: surface_property_cutoff_range
   ! The cutoffs for values of .surface_property for colouring.  Values of the
   ! property outside this range will be chopped.

   REALVEC(3) :: surface_point
   ! A special point which lies on or near the isosurface, used for calculations
   ! of connected area, for example.

   REAL :: surface_property_lower_bound
   ! A lower bound to the property value, for calculating surface areas.

   REAL :: surface_property_upper_bound
   ! A upper bound to the property value, for calculating surface areas.

   COLOURFUNCTION, PTR :: colour DEFAULT_NULL
   ! A colourfunction used for colouring the isourface

   ATOMVEC(:), PTR :: atom DEFAULT_NULL
   ! A list of atoms associated with ths isosurface.

   BIN :: use_interpolator
   ! If set TRUE, then the routine used to calculate the isosurface may use
   ! interpolation tables, rather than exact values, if possible.

   end type



   type roby_type

   STR(STR_SIZE) :: roby_kind
   ! The kind of Roby calculation to perform

   REALVEC(:), PTR :: n1 DEFAULT_NULL
   ! Roby population for each atom-group

   REALMAT(:,:), PTR :: n2 DEFAULT_NULL
   ! Roby pair population for each atom-group pair
   ! NOTE: these are *not* shared populations

   REAL :: n_shared
   ! Roby multiple shared population for a specified atom-group

   REALMAT(:,:), PTR :: bond_index DEFAULT_NULL
   ! Gould Bond indices for each pair of atoms

   REALMAT(:,:), PTR :: percent_covalency DEFAULT_NULL
   ! The % covalency of the given bond pair

   REALVEC(:), PTR :: gould_charge DEFAULT_NULL
   ! Roby-Gould charges

   REALVEC(:), PTR :: cruickshank_charge DEFAULT_NULL
   ! Cruikshank-Avramedes charges

   REALVEC(:), PTR :: summed_n2 DEFAULT_NULL
   ! Summed atom-group pair populations

   REALVEC(:), PTR :: summed_n3 DEFAULT_NULL
   ! Summed atom-group triple populations

   INTVEC(:), PTR :: atom_list DEFAULT_NULL
   ! The indices of a list of roby atoms to be used in some way
   ! for population analysis

   INTVECVEC(:), PTR :: atom_group DEFAULT_NULL
   ! The indices of the roby atoms defining different spaces

   BIN :: analyse_all_atom_pairs
   ! If set TRUE, the bond index information is printed out ONLY for every
   ! atom pair which is considered "bonded". Otherwise all pairs of atoms are
   ! analysed. This is onlyeffective when an atom_list is defined.

   REAL :: bond_scale_factor
   ! Used to multiply the sum of the Bragg-Slater radii for two atoms,
   ! to determine a distance cutoff within which the atoms are regarded
   ! to be bonded

   REAL :: covalent_cutoff
   ! Angles (in radians) greater than this are ignored when calculating
   ! the covalent bond index

   REAL :: ionic_cutoff
   ! Angles (in radians) greater than this are ignored when calculating
   ! the ionic bond index

   REAL :: pi_on_2_cutoff
   ! Angles (in radians) greater than this are regareded as pi/2

   REAL :: zero_cutoff
   ! Angles (in radians) *less* than this are regareded as zero

   REAL :: occupied_ANO_cutoff
   ! Atomic natural orbitals with occupations less than this number
   ! are regarded as unoccupied. This number is used to define the Roby
   ! atomic projector

   BIN :: output_theta_info
   ! If set TRUE, the bond index information is printed out for every
   ! space V_theta. See the paper for details.

   INTVEC(:), PTR :: atom_a DEFAULT_NULL
   ! The indices of the atoms defining space V_A

   INTVEC(:), PTR :: atom_b DEFAULT_NULL
   ! The indices of the atoms defining space V_B

   INTVEC(:), PTR :: atom_ab DEFAULT_NULL
   ! The indices of the roby atoms defining spaces V_A and V_B

   REALMAT(:,:), PTR :: theta_C DEFAULT_NULL
   ! The matrix of the covalent theta orbitals

   REALVEC(:), PTR :: eval_C DEFAULT_NULL
   ! The array of the covalent theta eigenvalues

   REALVEC(:), PTR :: theta_angle DEFAULT_NULL
   ! The array of covalent theta angles

   INTVEC(:), PTR :: pair DEFAULT_NULL
   ! An array which pairs the positive eigenvalues in eval_C with the
   ! negative eigenvalues, thus definining each theta subspace

   REALMAT(:,:), PTR :: theta_I DEFAULT_NULL
   ! The matrix of the covalent theta orbitals

   REALVEC(:), PTR :: eval_I DEFAULT_NULL
   ! The matrix of the covalent theta eigenvalues

   REALVEC(:), PTR :: pop_C DEFAULT_NULL
   ! Covalent theta orbital popualtions

   REALVEC(:), PTR :: pop_I DEFAULT_NULL
   ! Covalent theta orbital popualtions

   REALVEC(:), PTR :: pop_A DEFAULT_NULL
   ! Atom "A" theta orbital popualtions

   REALVEC(:), PTR :: pop_B DEFAULT_NULL
   ! Atom "B" theta orbital popualtions

   REALVEC(:), PTR :: covalent_index DEFAULT_NULL
   ! The vector of each covalent theta bond index

   REALVEC(:), PTR :: ionic_index DEFAULT_NULL
   ! The vector of each covalent theta bond index

   REALMAT(:,:), PTR :: proportion_a DEFAULT_NULL
   ! The proportion to partition for atom A, between two atoms (A,B)
   ! using Gould's probabilistic scheme

   INT :: charge
   ! The total charge on the molecule

   INT :: mult
   ! The spin multiplicity of the molecule

   OPMATRIX, PTR :: rho DEFAULT_NULL
   ! The density matrix of the molecule

   REALMAT(:,:), PTR :: overlap_matrix DEFAULT_NULL
   ! The full molecular overlap matrix for Roby analysis

   ATOMVEC(:), PTR :: atom DEFAULT_NULL
   ! The actual list of atoms to be used in the Roby calculations
   ! Usually this will come from a MOL object

   end type



   type mol_type

   STR(STR_SIZE) :: name
   ! Name of molecule

   INT :: charge
   ! Electric charge of the molecule

   BIN :: minimal_io DEFAULT(TRUE)
   ! Outputs no data files unless requested

   INT :: mult
   ! Spin multiplicity of the molecule

   REALVEC(3) :: E_field
   ! Applied electric field in atomic units

   REALVEC(3) :: B_field
   ! Applied magnetic field in atomic units

   REALVEC(3) :: gauge_origin
   ! Global gauge origin for magnetic field

   ATOMVEC(:), PTR :: atom DEFAULT_NULL
   ! List of atoms in molecule

   STR(STR_SIZE) :: basis_set_kind
   ! A suffix string representing the name of the basis set class
   ! to be used for each atom

   BASISVEC(:), PTR :: basis DEFAULT_NULL
   ! List of basis sets used

   SLATERBASISVEC(:), PTR :: slaterbasis DEFAULT_NULL
   ! List of Slater basis sets used

   COPPENSBASISVEC(:), PTR :: coppensbasis DEFAULT_NULL
   ! List of coppens basis sets used

   BIN :: basis_info_made
   ! Set TRUE if the gaussian basis set info has been made.

   PLOTGRID, PTR :: grid DEFAULT_NULL
   ! Rectangular grid data, for plots

   ISOSURFACE, PTR :: isosurface DEFAULT_NULL
   ! An object used for creating triangluated meshes for isosurface plots.

   DFTGRID, PTR :: dftgrid DEFAULT_NULL
   ! DFT integration grid data

   CRYSTAL, PTR :: crystal DEFAULT_NULL
   ! Crystal data for the enclosing crystal

   CLUSTER, PTR :: cluster DEFAULT_NULL
   ! Crystal cluster data

   CIF, PTR :: cif DEFAULT_NULL
   ! Crystallographic Information file (CIF) object

   POINTGROUP, PTR :: pointgroup DEFAULT_NULL
   ! Pointgroup symmetry of the molecule

   MOL, PTR :: saved DEFAULT_NULL
   ! For saving self and temporarily using an alternative in its place

   INT :: n_e
   ! No of electrons

   INT :: n_a
   ! No of alpha electrons

   INT :: n_b
   ! No of beta electrons

   INT :: n_atom
   ! No of atoms

   INT :: n_atom_kind
   ! No of atoms of a different kind

   INT :: n_basis
   ! No of basis sets

   INT :: n_shell
   ! Total number of shells in the molecular basis set

   INT :: n_shell_pairs
   ! Total number of shell pairs in the molecular basis set

   INT :: n_bf
   ! Total number of basis functions in the molecular basis set

   INT :: n_prim
   ! Total number of primitives in the molecular basis set

   INT :: n_unique_shells
   ! Number of unique shells in the basis set.

   INTVEC(:), PTR :: atom_for_shell DEFAULT_NULL
   ! Atom index for molecular shell index array

   INTVEC(:), PTR :: atom_shell_for_shell DEFAULT_NULL
   ! Atom shell index for molecular shell index array

   INTVEC(:), PTR :: first_shell_for_atom DEFAULT_NULL
   ! First molecule shell index for an atom

   INTVEC(:), PTR :: last_shell_for_atom DEFAULT_NULL
   ! Last molecule shell index for an atom

   INTVEC(:), PTR :: basis_shell_for_shell DEFAULT_NULL
   ! Map a shell of the molecule to a shell of the basis set

   INTVEC(:), PTR :: first_basis_fn_for_shell DEFAULT_NULL
   ! First basis function for a given shell

   INTVEC(:), PTR :: last_basis_fn_for_shell DEFAULT_NULL
   ! Last basis function for a given shell

   INTVEC(:), PTR :: first_basis_fn_for_atom DEFAULT_NULL
   ! For the atom basis function limits

   INTVEC(:), PTR :: last_basis_fn_for_atom DEFAULT_NULL
   ! For the atom basis function limits

   SHELLPAIRVEC(:), PTR :: precomputed_basis_shellpair DEFAULT_NULL
   ! Precomputed stuff for shellpairs of the basis set, to speed up
   ! later calculations of gaussian integrals.

   INTVEC(:), PTR :: atom_kind DEFAULT_NULL
   ! The unique kind of each atom in .atom

   INTVEC(:), PTR :: unique_atom DEFAULT_NULL
   ! List of the unique atoms (of different kind)

   SCFDATA, PTR :: scfdata DEFAULT_NULL
   ! SCF data object

   OPVECTOR, PTR :: orbital_energies DEFAULT_NULL
   ! The orbital energies

   OPMATRIX, PTR :: molecular_orbitals DEFAULT_NULL
   ! The real molecular orbitals

   OPMATRIX, PTR :: density_matrix DEFAULT_NULL
   ! The real density matrix

   OPMATRIX, PTR :: natural_orbitals DEFAULT_NULL
   ! The natural orbitals

   OPVECTOR, PTR :: occupation_numbers DEFAULT_NULL
   ! The natural orbital occupation numbers

   OPMATRIX, PTR :: fock_matrix DEFAULT_NULL
   ! The real fock matrix

   OPMATRIX, PTR :: constraint_matrix DEFAULT_NULL
   ! The real constraint matrix

   OPMATRIX, PTR :: old_molecular_orbitals DEFAULT_NULL
   ! The real molecular orbitals from the previous SCF iteration

   OPMATRIX, PTR :: old_density_matrix DEFAULT_NULL
   ! The real density matrix from the previous SCF iteration

   OPMATRIX, PTR :: old_fock_matrix DEFAULT_NULL
   ! The real fock matrix from the previous SCF iteration

   OPMATRIX, PTR :: old_constraint_matrix DEFAULT_NULL
   ! The real constraint matrix from the previous SCF iteration

   INTVECVEC(:), PTR :: atom_group DEFAULT_NULL
   ! A list of atom group indices, for group property decomposition

   REALVEC(:), PTR :: atom_group_energy DEFAULT_NULL
   ! The SCF energy of each atom group

   INTVEC(:), PTR :: group_charges DEFAULT_NULL
   ! A list of charges for each group in the molecule, defined in atom_groups.

   BIN :: optimise_thermals
   ! Whether to optimise the thermal parameters

   ROBY, PTR :: roby DEFAULT_NULL
   ! A Roby data object

   STR(STR_SIZE) :: CIF_file_name DEFAULT(" ")
   ! The name of a CIF file to be used for input

   STR(STR_SIZE) :: CIF_data_block_name DEFAULT(" ")
   ! The name of a CIF file data block, to be used for processing

   STR(STR_SIZE) :: CX_file_name DEFAULT(" ")
   ! The name of the Crystal Explorer (CX) output file

   end type


end module
