# 1 "c-typeck.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "c-typeck.c"
# 32 "c-typeck.c"
# 1 "config.h" 1

# 1 "auto-host.h" 1
# 3 "config.h" 2




struct rtx_def;
typedef struct rtx_def *rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
union tree_node;
typedef union tree_node *tree;
# 1 "ansidecl.h" 1
# 14 "config.h" 2
# 1 "biarch64.h" 1
# 15 "config.h" 2
# 1 "i386.h" 1
# 53 "i386.h"
struct processor_costs {
  const int add;
  const int lea;
  const int shift_var;
  const int shift_const;
  const int mult_init;
  const int mult_bit;
  const int divide;
  int movsx;
  int movzx;
  const int large_insn;
  const int move_ratio;

  const int movzbl_load;
  const int int_load[3];


  const int int_store[3];

  const int fp_move;
  const int fp_load[3];

  const int fp_store[3];

  const int mmx_move;
  const int mmx_load[2];

  const int mmx_store[2];

  const int sse_move;
  const int sse_load[3];

  const int sse_store[3];

  const int mmxsse_to_integer;

  const int prefetch_block;
  const int simultaneous_prefetches;

};

extern const struct processor_costs *ix86_cost;



extern int target_flags;
# 211 "i386.h"
extern const int x86_use_leave, x86_push_memory, x86_zero_extend_with_and;
extern const int x86_use_bit_test, x86_cmove, x86_deep_branch;
extern const int x86_branch_hints, x86_unroll_strlen;
extern const int x86_double_with_add, x86_partial_reg_stall, x86_movx;
extern const int x86_use_loop, x86_use_fiop, x86_use_mov0;
extern const int x86_use_cltd, x86_read_modify_write;
extern const int x86_read_modify, x86_split_long_moves;
extern const int x86_promote_QImode, x86_single_stringop;
extern const int x86_himode_math, x86_qimode_math, x86_promote_qi_regs;
extern const int x86_promote_hi_regs, x86_integer_DFmode_moves;
extern const int x86_add_esp_4, x86_add_esp_8, x86_sub_esp_4, x86_sub_esp_8;
extern const int x86_partial_reg_dependency, x86_memory_mismatch_stall;
extern const int x86_accumulate_outgoing_args, x86_prologue_using_move;
extern const int x86_epilogue_using_move, x86_decompose_lea;
extern const int x86_arch_always_fancy_math_387;
extern int x86_prefetch_sse;
# 395 "i386.h"
enum processor_type
{
  PROCESSOR_I386,
  PROCESSOR_I486,
  PROCESSOR_PENTIUM,
  PROCESSOR_PENTIUMPRO,
  PROCESSOR_K6,
  PROCESSOR_ATHLON,
  PROCESSOR_PENTIUM4,
  PROCESSOR_max
};
enum fpmath_unit
{
  FPMATH_387 = 1,
  FPMATH_SSE = 2
};

extern enum processor_type ix86_cpu;
extern enum fpmath_unit ix86_fpmath;

extern int ix86_arch;
# 1202 "i386.h"
enum reg_class
{
  NO_REGS,
  AREG, DREG, CREG, BREG, SIREG, DIREG,
  AD_REGS,
  Q_REGS,
  NON_Q_REGS,
  INDEX_REGS,
  LEGACY_REGS,
  GENERAL_REGS,
  FP_TOP_REG, FP_SECOND_REG,
  FLOAT_REGS,
  SSE_REGS,
  MMX_REGS,
  FP_TOP_SSE_REGS,
  FP_SECOND_SSE_REGS,
  FLOAT_SSE_REGS,
  FLOAT_INT_REGS,
  INT_SSE_REGS,
  FLOAT_INT_SSE_REGS,
  ALL_REGS, LIM_REG_CLASSES
};
# 1667 "i386.h"
typedef struct ix86_args {
  int words;
  int nregs;
  int regno;
  int sse_words;
  int sse_nregs;
  int sse_regno;
  int maybe_vaarg;
} CUMULATIVE_ARGS;
# 2029 "i386.h"
enum ix86_builtins
{
  IX86_BUILTIN_ADDPS,
  IX86_BUILTIN_ADDSS,
  IX86_BUILTIN_DIVPS,
  IX86_BUILTIN_DIVSS,
  IX86_BUILTIN_MULPS,
  IX86_BUILTIN_MULSS,
  IX86_BUILTIN_SUBPS,
  IX86_BUILTIN_SUBSS,

  IX86_BUILTIN_CMPEQPS,
  IX86_BUILTIN_CMPLTPS,
  IX86_BUILTIN_CMPLEPS,
  IX86_BUILTIN_CMPGTPS,
  IX86_BUILTIN_CMPGEPS,
  IX86_BUILTIN_CMPNEQPS,
  IX86_BUILTIN_CMPNLTPS,
  IX86_BUILTIN_CMPNLEPS,
  IX86_BUILTIN_CMPNGTPS,
  IX86_BUILTIN_CMPNGEPS,
  IX86_BUILTIN_CMPORDPS,
  IX86_BUILTIN_CMPUNORDPS,
  IX86_BUILTIN_CMPNEPS,
  IX86_BUILTIN_CMPEQSS,
  IX86_BUILTIN_CMPLTSS,
  IX86_BUILTIN_CMPLESS,
  IX86_BUILTIN_CMPGTSS,
  IX86_BUILTIN_CMPGESS,
  IX86_BUILTIN_CMPNEQSS,
  IX86_BUILTIN_CMPNLTSS,
  IX86_BUILTIN_CMPNLESS,
  IX86_BUILTIN_CMPNGTSS,
  IX86_BUILTIN_CMPNGESS,
  IX86_BUILTIN_CMPORDSS,
  IX86_BUILTIN_CMPUNORDSS,
  IX86_BUILTIN_CMPNESS,

  IX86_BUILTIN_COMIEQSS,
  IX86_BUILTIN_COMILTSS,
  IX86_BUILTIN_COMILESS,
  IX86_BUILTIN_COMIGTSS,
  IX86_BUILTIN_COMIGESS,
  IX86_BUILTIN_COMINEQSS,
  IX86_BUILTIN_UCOMIEQSS,
  IX86_BUILTIN_UCOMILTSS,
  IX86_BUILTIN_UCOMILESS,
  IX86_BUILTIN_UCOMIGTSS,
  IX86_BUILTIN_UCOMIGESS,
  IX86_BUILTIN_UCOMINEQSS,

  IX86_BUILTIN_CVTPI2PS,
  IX86_BUILTIN_CVTPS2PI,
  IX86_BUILTIN_CVTSI2SS,
  IX86_BUILTIN_CVTSS2SI,
  IX86_BUILTIN_CVTTPS2PI,
  IX86_BUILTIN_CVTTSS2SI,

  IX86_BUILTIN_MAXPS,
  IX86_BUILTIN_MAXSS,
  IX86_BUILTIN_MINPS,
  IX86_BUILTIN_MINSS,

  IX86_BUILTIN_LOADAPS,
  IX86_BUILTIN_LOADUPS,
  IX86_BUILTIN_STOREAPS,
  IX86_BUILTIN_STOREUPS,
  IX86_BUILTIN_LOADSS,
  IX86_BUILTIN_STORESS,
  IX86_BUILTIN_MOVSS,

  IX86_BUILTIN_MOVHLPS,
  IX86_BUILTIN_MOVLHPS,
  IX86_BUILTIN_LOADHPS,
  IX86_BUILTIN_LOADLPS,
  IX86_BUILTIN_STOREHPS,
  IX86_BUILTIN_STORELPS,

  IX86_BUILTIN_MASKMOVQ,
  IX86_BUILTIN_MOVMSKPS,
  IX86_BUILTIN_PMOVMSKB,

  IX86_BUILTIN_MOVNTPS,
  IX86_BUILTIN_MOVNTQ,

  IX86_BUILTIN_PACKSSWB,
  IX86_BUILTIN_PACKSSDW,
  IX86_BUILTIN_PACKUSWB,

  IX86_BUILTIN_PADDB,
  IX86_BUILTIN_PADDW,
  IX86_BUILTIN_PADDD,
  IX86_BUILTIN_PADDSB,
  IX86_BUILTIN_PADDSW,
  IX86_BUILTIN_PADDUSB,
  IX86_BUILTIN_PADDUSW,
  IX86_BUILTIN_PSUBB,
  IX86_BUILTIN_PSUBW,
  IX86_BUILTIN_PSUBD,
  IX86_BUILTIN_PSUBSB,
  IX86_BUILTIN_PSUBSW,
  IX86_BUILTIN_PSUBUSB,
  IX86_BUILTIN_PSUBUSW,

  IX86_BUILTIN_PAND,
  IX86_BUILTIN_PANDN,
  IX86_BUILTIN_POR,
  IX86_BUILTIN_PXOR,

  IX86_BUILTIN_PAVGB,
  IX86_BUILTIN_PAVGW,

  IX86_BUILTIN_PCMPEQB,
  IX86_BUILTIN_PCMPEQW,
  IX86_BUILTIN_PCMPEQD,
  IX86_BUILTIN_PCMPGTB,
  IX86_BUILTIN_PCMPGTW,
  IX86_BUILTIN_PCMPGTD,

  IX86_BUILTIN_PEXTRW,
  IX86_BUILTIN_PINSRW,

  IX86_BUILTIN_PMADDWD,

  IX86_BUILTIN_PMAXSW,
  IX86_BUILTIN_PMAXUB,
  IX86_BUILTIN_PMINSW,
  IX86_BUILTIN_PMINUB,

  IX86_BUILTIN_PMULHUW,
  IX86_BUILTIN_PMULHW,
  IX86_BUILTIN_PMULLW,

  IX86_BUILTIN_PSADBW,
  IX86_BUILTIN_PSHUFW,

  IX86_BUILTIN_PSLLW,
  IX86_BUILTIN_PSLLD,
  IX86_BUILTIN_PSLLQ,
  IX86_BUILTIN_PSRAW,
  IX86_BUILTIN_PSRAD,
  IX86_BUILTIN_PSRLW,
  IX86_BUILTIN_PSRLD,
  IX86_BUILTIN_PSRLQ,
  IX86_BUILTIN_PSLLWI,
  IX86_BUILTIN_PSLLDI,
  IX86_BUILTIN_PSLLQI,
  IX86_BUILTIN_PSRAWI,
  IX86_BUILTIN_PSRADI,
  IX86_BUILTIN_PSRLWI,
  IX86_BUILTIN_PSRLDI,
  IX86_BUILTIN_PSRLQI,

  IX86_BUILTIN_PUNPCKHBW,
  IX86_BUILTIN_PUNPCKHWD,
  IX86_BUILTIN_PUNPCKHDQ,
  IX86_BUILTIN_PUNPCKLBW,
  IX86_BUILTIN_PUNPCKLWD,
  IX86_BUILTIN_PUNPCKLDQ,

  IX86_BUILTIN_SHUFPS,

  IX86_BUILTIN_RCPPS,
  IX86_BUILTIN_RCPSS,
  IX86_BUILTIN_RSQRTPS,
  IX86_BUILTIN_RSQRTSS,
  IX86_BUILTIN_SQRTPS,
  IX86_BUILTIN_SQRTSS,

  IX86_BUILTIN_UNPCKHPS,
  IX86_BUILTIN_UNPCKLPS,

  IX86_BUILTIN_ANDPS,
  IX86_BUILTIN_ANDNPS,
  IX86_BUILTIN_ORPS,
  IX86_BUILTIN_XORPS,

  IX86_BUILTIN_EMMS,
  IX86_BUILTIN_LDMXCSR,
  IX86_BUILTIN_STMXCSR,
  IX86_BUILTIN_SFENCE,


  IX86_BUILTIN_FEMMS,
  IX86_BUILTIN_PAVGUSB,
  IX86_BUILTIN_PF2ID,
  IX86_BUILTIN_PFACC,
  IX86_BUILTIN_PFADD,
  IX86_BUILTIN_PFCMPEQ,
  IX86_BUILTIN_PFCMPGE,
  IX86_BUILTIN_PFCMPGT,
  IX86_BUILTIN_PFMAX,
  IX86_BUILTIN_PFMIN,
  IX86_BUILTIN_PFMUL,
  IX86_BUILTIN_PFRCP,
  IX86_BUILTIN_PFRCPIT1,
  IX86_BUILTIN_PFRCPIT2,
  IX86_BUILTIN_PFRSQIT1,
  IX86_BUILTIN_PFRSQRT,
  IX86_BUILTIN_PFSUB,
  IX86_BUILTIN_PFSUBR,
  IX86_BUILTIN_PI2FD,
  IX86_BUILTIN_PMULHRW,


  IX86_BUILTIN_PF2IW,
  IX86_BUILTIN_PFNACC,
  IX86_BUILTIN_PFPNACC,
  IX86_BUILTIN_PI2FW,
  IX86_BUILTIN_PSWAPDSI,
  IX86_BUILTIN_PSWAPDSF,

  IX86_BUILTIN_SSE_ZERO,
  IX86_BUILTIN_MMX_ZERO,

  IX86_BUILTIN_MAX
};
# 2818 "i386.h"
extern int const dbx_register_map[53];
extern int const dbx64_register_map[53];
extern int const svr4_dbx_register_map[53];
# 3092 "i386.h"
enum cmodel {
  CM_32,
  CM_SMALL,
  CM_KERNEL,
  CM_MEDIUM,
  CM_LARGE,
  CM_SMALL_PIC
};





extern const char *ix86_debug_arg_string, *ix86_debug_addr_string;

enum asm_dialect {
  ASM_ATT,
  ASM_INTEL
};
extern const char *ix86_asm_string;
extern enum asm_dialect ix86_asm_dialect;

extern const char *ix86_cmodel_string;
extern enum cmodel ix86_cmodel;


extern const char *ix86_cpu_string;
extern const char *ix86_arch_string;
extern const char *ix86_fpmath_string;
extern const char *ix86_regparm_string;
extern const char *ix86_align_loops_string;
extern const char *ix86_align_jumps_string;
extern const char *ix86_align_funcs_string;
extern const char *ix86_preferred_stack_boundary_string;
extern const char *ix86_branch_cost_string;
extern int ix86_regparm;
extern int ix86_preferred_stack_boundary;
extern int ix86_branch_cost;
extern enum reg_class const regclass_map[53];
extern rtx ix86_compare_op0;
extern rtx ix86_compare_op1;
# 3149 "i386.h"
enum fp_cw_mode {FP_CW_STORED, FP_CW_UNINITIALIZED, FP_CW_ANY};
# 16 "config.h" 2
# 1 "att.h" 1
# 22 "att.h"
# 1 "unix.h" 1
# 23 "att.h" 2
# 17 "config.h" 2
# 1 "dbxelf.h" 1
# 18 "config.h" 2
# 1 "elfos.h" 1
# 19 "config.h" 2
# 1 "svr4.h" 1
# 20 "config.h" 2
# 1 "linux.h" 1
# 21 "config.h" 2
# 1 "x86-64.h" 1
# 22 "config.h" 2
# 1 "linux64.h" 1
# 23 "config.h" 2
# 1 "defaults.h" 1
# 24 "config.h" 2




# 1 "insn-constants.h" 1
# 29 "config.h" 2
# 1 "insn-flags.h" 1
# 599 "insn-flags.h"
struct rtx_def;
extern struct rtx_def *gen_cmpdi_ccno_1_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpdi_1_insn_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpqi_ext_3_insn (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpqi_ext_3_insn_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_fnstsw_1 (struct rtx_def *);
extern struct rtx_def *gen_x86_sahf_1 (struct rtx_def *);
extern struct rtx_def *gen_popsi1 (struct rtx_def *);
extern struct rtx_def *gen_movsi_insv_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pushdi2_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_popdi1 (struct rtx_def *);
extern struct rtx_def *gen_swapxf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_swaptf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendhisi2_and (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendsidi2_32 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendsidi2_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendhidi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendqidi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendsidi2_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendhidi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendqidi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendhisi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendqihi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendqisi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_truncdfsf2_3 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_truncdfsf2_sse_only (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncdi_nomemory (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncdi_memory (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncsfdi_sse (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncdfdi_sse (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncsi_nomemory (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncsi_memory (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncsfsi_sse (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncdfsi_sse (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_trunchi_nomemory (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_trunchi_memory (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_fnstcw_1 (struct rtx_def *);
extern struct rtx_def *gen_x86_fldcw_1 (struct rtx_def *);
extern struct rtx_def *gen_floathisf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floathidf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floathixf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floathitf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatsixf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatsitf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatdixf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatditf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addqi3_cc (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addsi_1_zext (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addqi_ext_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subdi3_carry_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subsi3_carry (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subsi3_carry_zext (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_udivqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divmodhi4 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_udivmoddi4 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_udivmodsi4 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_testsi_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_andqi_ext_0 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negsf2_memory (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negsf2_ifs (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negdf2_memory (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negdf2_ifs (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_abssf2_memory (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_abssf2_ifs (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_absdf2_memory (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_absdf2_ifs (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashldi3_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_shld_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrdi3_63_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrdi3_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_shrd_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrsi3_31 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_lshrdi3_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_setcc_2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_jump (struct rtx_def *);
extern struct rtx_def *gen_doloop_end_internal (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_blockage (void);
extern struct rtx_def *gen_return_internal (void);
extern struct rtx_def *gen_return_pop_internal (struct rtx_def *);
extern struct rtx_def *gen_return_indirect_internal (struct rtx_def *);
extern struct rtx_def *gen_nop (void);
extern struct rtx_def *gen_prologue_set_got (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_prologue_get_pc (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_eh_return_si (struct rtx_def *);
extern struct rtx_def *gen_eh_return_di (struct rtx_def *);
extern struct rtx_def *gen_leave (void);
extern struct rtx_def *gen_leave_rex64 (void);
extern struct rtx_def *gen_ffssi_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtsf2_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtsf2_1_sse_only (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtsf2_i387 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtdf2_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtdf2_1_sse_only (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtdf2_i387 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtxf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrttf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sindf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sinsf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sinxf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sintf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cosdf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cossf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cosxf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_costf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cld (void);
extern struct rtx_def *gen_strmovdi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovsi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovsi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovhi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovhi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovqi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovqi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_movdi_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_movsi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_movsi_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_movqi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_movqi_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetdi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetsi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetsi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsethi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsethi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetqi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetqi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_stosdi_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_stossi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_stossi_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_stosqi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rep_stosqi_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpstrqi_nz_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpstrqi_nz_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpstrqi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpstrqi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strlenqi_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strlenqi_rex_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_movdicc_0_m1_rex64 (struct rtx_def *);
extern struct rtx_def *gen_x86_movsicc_0_m1 (struct rtx_def *);
extern struct rtx_def *gen_pro_epilogue_adjust_stack_rex64 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movsfcc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movsfcc_eq (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movdfcc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movdfcc_eq (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_allocate_stack_worker_1 (struct rtx_def *);
extern struct rtx_def *gen_allocate_stack_worker_rex64 (struct rtx_def *);
extern struct rtx_def *gen_trap (void);
extern struct rtx_def *gen_movv4sf_internal (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv4si_internal (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv8qi_internal (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv4hi_internal (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv2si_internal (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv2sf_internal (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movti_internal (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movaps (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movups (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movmskps (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_pmovmskb (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_maskmovq (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_maskmovq_rex (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movntv4sf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movntdi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movhlps (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movlhps (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movhps (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movlps (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_loadss (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_movss (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_storess (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_shufps (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmaddv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmsubv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmmulv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmdivv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rcpv4sf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmrcpv4sf2 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rsqrtv4sf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmrsqrtv4sf2 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtv4sf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmsqrtv4sf2 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_andti3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_nandti3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_iorti3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_xorti3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_clrv4sf (struct rtx_def *);
extern struct rtx_def *gen_maskcmpv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_maskncmpv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmmaskcmpv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmmaskncmpv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_comi (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_ucomi (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_unpckhps (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sse_unpcklps (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_smaxv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmsmaxv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sminv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_vmsminv4sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cvtpi2ps (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cvtps2pi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cvttps2pi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cvtsi2ss (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cvtss2si (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cvttss2si (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addv2si3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ssaddv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ssaddv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_usaddv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_usaddv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subv2si3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sssubv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sssubv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ussubv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ussubv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_smulv4hi3_highpart (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_umulv4hi3_highpart (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_pmaddwd (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_iordi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_xordi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_clrdi (struct rtx_def *);
extern struct rtx_def *gen_mmx_anddi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_nanddi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_uavgv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_uavgv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_psadbw (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_pinsrw (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_pextrw (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_pshufw (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_eqv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_eqv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_eqv2si3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_gtv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_gtv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_gtv2si3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_umaxv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_smaxv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_uminv8qi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sminv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrv2si3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_lshrv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_lshrv2si3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_lshrdi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashlv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashlv2si3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_ashldi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_packsswb (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_packssdw (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_packuswb (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_punpckhbw (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_punpckhwd (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_punpckhdq (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_punpcklbw (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_punpcklwd (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mmx_punpckldq (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_emms (void);
extern struct rtx_def *gen_ldmxcsr (struct rtx_def *);
extern struct rtx_def *gen_stmxcsr (struct rtx_def *);
extern struct rtx_def *gen_addv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subrv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_gtv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_gev2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_eqv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfmaxv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfminv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulv2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_femms (void);
extern struct rtx_def *gen_pf2id (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pf2iw (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfacc (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfnacc (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfpnacc (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pi2fw (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatv2si2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pavgusb (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfrcpv2sf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfrcpit1v2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfrcpit2v2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfrsqrtv2sf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pfrsqit1v2sf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pmulhrwv4hi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pswapdv2si2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pswapdv2sf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpdi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpsi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmphi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpqi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpdi_1_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpsi_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpqi_ext_3 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpxf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmptf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpdf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpsf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movsi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movhi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movstricthi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movqi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_reload_outqi (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movstrictqi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movdi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movsf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movdf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movxf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movtf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendhisi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendqihi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendqisi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_zero_extendsidi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendsidi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendsfdf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendsfxf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extendsftf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extenddfxf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extenddftf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_truncdfsf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_truncxfsf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_trunctfsf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_truncxfdf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_trunctfdf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncxfdi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_trunctfdi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncdfdi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncsfdi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncxfsi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_trunctfsi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncdfsi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncsfsi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncxfhi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_trunctfhi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncdfhi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_fix_truncsfhi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatsisf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatdisf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatsidf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_floatdidf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_adddi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addxf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addtf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_adddf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_addsf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subdi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subxf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subtf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subdf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_subsf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_muldi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_umulqihi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulqihi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_umulditi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_umulsidi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulditi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulsidi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_umuldi3_highpart (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_umulsi3_highpart (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_smuldi3_highpart (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_smulsi3_highpart (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulxf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_multf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_muldf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mulsf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divxf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divtf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divdf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divsf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divmoddi4 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_divmodsi4 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_udivmodhi4 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_testsi_ccno_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_testqi_ccz_1 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_testqi_ext_ccno_0 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_anddi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_andsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_andhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_andqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_iordi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_iorsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_iorhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_iorqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_xordi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_xorsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_xorhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_xorqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_xorqi_cc_ext_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negdi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negsi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_neghi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negqi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negsf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negdf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negxf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_negtf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_abssf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_absdf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_absxf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_abstf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_one_cmpldi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_one_cmplsi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_one_cmplhi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_one_cmplqi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashldi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_shift_adj_1 (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_shift_adj_2 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashlsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashlhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashlqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrdi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_x86_shift_adj_3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ashrqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_lshrdi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_lshrsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_lshrhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_lshrqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotldi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotlsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotlhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotlqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotrdi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotrsi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotrhi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_rotrqi3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extv (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_extzv (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_insv (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_seq (struct rtx_def *);
extern struct rtx_def *gen_sne (struct rtx_def *);
extern struct rtx_def *gen_sgt (struct rtx_def *);
extern struct rtx_def *gen_sgtu (struct rtx_def *);
extern struct rtx_def *gen_slt (struct rtx_def *);
extern struct rtx_def *gen_sltu (struct rtx_def *);
extern struct rtx_def *gen_sge (struct rtx_def *);
extern struct rtx_def *gen_sgeu (struct rtx_def *);
extern struct rtx_def *gen_sle (struct rtx_def *);
extern struct rtx_def *gen_sleu (struct rtx_def *);
extern struct rtx_def *gen_sunordered (struct rtx_def *);
extern struct rtx_def *gen_sordered (struct rtx_def *);
extern struct rtx_def *gen_suneq (struct rtx_def *);
extern struct rtx_def *gen_sunge (struct rtx_def *);
extern struct rtx_def *gen_sungt (struct rtx_def *);
extern struct rtx_def *gen_sunle (struct rtx_def *);
extern struct rtx_def *gen_sunlt (struct rtx_def *);
extern struct rtx_def *gen_sltgt (struct rtx_def *);
extern struct rtx_def *gen_beq (struct rtx_def *);
extern struct rtx_def *gen_bne (struct rtx_def *);
extern struct rtx_def *gen_bgt (struct rtx_def *);
extern struct rtx_def *gen_bgtu (struct rtx_def *);
extern struct rtx_def *gen_blt (struct rtx_def *);
extern struct rtx_def *gen_bltu (struct rtx_def *);
extern struct rtx_def *gen_bge (struct rtx_def *);
extern struct rtx_def *gen_bgeu (struct rtx_def *);
extern struct rtx_def *gen_ble (struct rtx_def *);
extern struct rtx_def *gen_bleu (struct rtx_def *);
extern struct rtx_def *gen_bunordered (struct rtx_def *);
extern struct rtx_def *gen_bordered (struct rtx_def *);
extern struct rtx_def *gen_buneq (struct rtx_def *);
extern struct rtx_def *gen_bunge (struct rtx_def *);
extern struct rtx_def *gen_bungt (struct rtx_def *);
extern struct rtx_def *gen_bunle (struct rtx_def *);
extern struct rtx_def *gen_bunlt (struct rtx_def *);
extern struct rtx_def *gen_bltgt (struct rtx_def *);
extern struct rtx_def *gen_indirect_jump (struct rtx_def *);
extern struct rtx_def *gen_tablejump (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_doloop_end (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);

extern struct rtx_def *gen_call_pop (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);

extern struct rtx_def *gen_call (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_call_exp (struct rtx_def *, struct rtx_def *);

extern struct rtx_def *gen_call_value_pop (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);

extern struct rtx_def *gen_call_value (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_call_value_exp (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_untyped_call (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_return (void);
extern struct rtx_def *gen_prologue (void);
extern struct rtx_def *gen_epilogue (void);
extern struct rtx_def *gen_sibcall_epilogue (void);
extern struct rtx_def *gen_eh_return (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_ffssi2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtsf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sqrtdf2 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movstrsi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movstrdi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovdi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovsi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovsi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovhi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovhi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovqi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strmovqi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_clrstrsi (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_clrstrdi (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetdi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetsi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetsi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsethi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsethi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetqi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strsetqi_rex64 (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpstrsi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_cmpintqi (struct rtx_def *);
extern struct rtx_def *gen_strlensi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_strlendi (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movdicc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movsicc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movhicc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movsfcc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movdfcc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movxfcc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movtfcc (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_minsf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_mindf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_maxsf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_maxdf3 (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_pro_epilogue_adjust_stack (struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_allocate_stack_worker (struct rtx_def *);
extern struct rtx_def *gen_allocate_stack (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_builtin_setjmp_receiver (struct rtx_def *);
extern struct rtx_def *gen_conditional_trap (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movti (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv4sf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv4si (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv2si (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv4hi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv8qi (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_movv2sf (struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_sfence (void);
extern struct rtx_def *gen_sse_prologue_save (struct rtx_def *, struct rtx_def *, struct rtx_def *, struct rtx_def *);
extern struct rtx_def *gen_prefetch (struct rtx_def *, struct rtx_def *, struct rtx_def *);
# 30 "config.h" 2
# 33 "c-typeck.c" 2
# 1 "system.h" 1
# 33 "system.h"
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stdarg.h" 1 3 4
# 43 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 105 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stdarg.h" 3 4
typedef __gnuc_va_list va_list;
# 34 "system.h" 2
# 47 "system.h"
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 151 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 213 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 325 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 3 4
typedef int wchar_t;
# 48 "system.h" 2


# 1 "/usr/include/stdio.h" 1 3 4
# 28 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 296 "/usr/include/features.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 297 "/usr/include/features.h" 2 3 4
# 319 "/usr/include/features.h" 3 4
# 1 "/usr/include/gnu/stubs.h" 1 3 4
# 320 "/usr/include/features.h" 2 3 4
# 29 "/usr/include/stdio.h" 2 3 4





# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 35 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/bits/types.h" 1 3 4
# 28 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 29 "/usr/include/bits/types.h" 2 3 4


# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 32 "/usr/include/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;







typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
# 129 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/typesizes.h" 1 3 4
# 130 "/usr/include/bits/types.h" 2 3 4






__extension__ typedef unsigned long int __dev_t;
__extension__ typedef unsigned int __uid_t;
__extension__ typedef unsigned int __gid_t;
__extension__ typedef unsigned long int __ino_t;
__extension__ typedef unsigned long int __ino64_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef unsigned long int __nlink_t;
__extension__ typedef long int __off_t;
__extension__ typedef long int __off64_t;
__extension__ typedef int __pid_t;
__extension__ typedef struct { int __val[2]; } __fsid_t;
__extension__ typedef long int __clock_t;
__extension__ typedef unsigned long int __rlim_t;
__extension__ typedef unsigned long int __rlim64_t;
__extension__ typedef unsigned int __id_t;
__extension__ typedef long int __time_t;
__extension__ typedef unsigned int __useconds_t;
__extension__ typedef long int __suseconds_t;

__extension__ typedef int __daddr_t;
__extension__ typedef long int __swblk_t;
__extension__ typedef int __key_t;


__extension__ typedef int __clockid_t;


__extension__ typedef int __timer_t;


__extension__ typedef long int __blksize_t;




__extension__ typedef long int __blkcnt_t;
__extension__ typedef long int __blkcnt64_t;


__extension__ typedef unsigned long int __fsblkcnt_t;
__extension__ typedef unsigned long int __fsblkcnt64_t;


__extension__ typedef unsigned long int __fsfilcnt_t;
__extension__ typedef unsigned long int __fsfilcnt64_t;

__extension__ typedef long int __ssize_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


__extension__ typedef long int __intptr_t;


__extension__ typedef unsigned int __socklen_t;
# 37 "/usr/include/stdio.h" 2 3 4









typedef struct _IO_FILE FILE;





# 62 "/usr/include/stdio.h" 3 4
typedef struct _IO_FILE __FILE;
# 72 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/libio.h" 1 3 4
# 32 "/usr/include/libio.h" 3 4
# 1 "/usr/include/_G_config.h" 1 3 4
# 14 "/usr/include/_G_config.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 354 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 3 4
typedef unsigned int wint_t;
# 15 "/usr/include/_G_config.h" 2 3 4
# 24 "/usr/include/_G_config.h" 3 4
# 1 "/usr/include/wchar.h" 1 3 4
# 48 "/usr/include/wchar.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 49 "/usr/include/wchar.h" 2 3 4

# 1 "/usr/include/bits/wchar.h" 1 3 4
# 51 "/usr/include/wchar.h" 2 3 4
# 76 "/usr/include/wchar.h" 3 4
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 25 "/usr/include/_G_config.h" 2 3 4

typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
# 44 "/usr/include/_G_config.h" 3 4
# 1 "/usr/include/gconv.h" 1 3 4
# 28 "/usr/include/gconv.h" 3 4
# 1 "/usr/include/wchar.h" 1 3 4
# 48 "/usr/include/wchar.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 49 "/usr/include/wchar.h" 2 3 4
# 29 "/usr/include/gconv.h" 2 3 4


# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 32 "/usr/include/gconv.h" 2 3 4





enum
{
  __GCONV_OK = 0,
  __GCONV_NOCONV,
  __GCONV_NODB,
  __GCONV_NOMEM,

  __GCONV_EMPTY_INPUT,
  __GCONV_FULL_OUTPUT,
  __GCONV_ILLEGAL_INPUT,
  __GCONV_INCOMPLETE_INPUT,

  __GCONV_ILLEGAL_DESCRIPTOR,
  __GCONV_INTERNAL_ERROR
};



enum
{
  __GCONV_IS_LAST = 0x0001,
  __GCONV_IGNORE_ERRORS = 0x0002
};



struct __gconv_step;
struct __gconv_step_data;
struct __gconv_loaded_object;
struct __gconv_trans_data;



typedef int (*__gconv_fct) (struct __gconv_step *, struct __gconv_step_data *,
                            __const unsigned char **, __const unsigned char *,
                            unsigned char **, size_t *, int, int);


typedef wint_t (*__gconv_btowc_fct) (struct __gconv_step *, unsigned char);


typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);



typedef int (*__gconv_trans_fct) (struct __gconv_step *,
                                  struct __gconv_step_data *, void *,
                                  __const unsigned char *,
                                  __const unsigned char **,
                                  __const unsigned char *, unsigned char **,
                                  size_t *);


typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
                                          __const unsigned char *,
                                          unsigned char *, unsigned char *);


typedef int (*__gconv_trans_query_fct) (__const char *, __const char ***,
                                        size_t *);


typedef int (*__gconv_trans_init_fct) (void **, const char *);
typedef void (*__gconv_trans_end_fct) (void *);

struct __gconv_trans_data
{

  __gconv_trans_fct __trans_fct;
  __gconv_trans_context_fct __trans_context_fct;
  __gconv_trans_end_fct __trans_end_fct;
  void *__data;
  struct __gconv_trans_data *__next;
};



struct __gconv_step
{
  struct __gconv_loaded_object *__shlib_handle;
  __const char *__modname;

  int __counter;

  char *__from_name;
  char *__to_name;

  __gconv_fct __fct;
  __gconv_btowc_fct __btowc_fct;
  __gconv_init_fct __init_fct;
  __gconv_end_fct __end_fct;



  int __min_needed_from;
  int __max_needed_from;
  int __min_needed_to;
  int __max_needed_to;


  int __stateful;

  void *__data;
};



struct __gconv_step_data
{
  unsigned char *__outbuf;
  unsigned char *__outbufend;



  int __flags;



  int __invocation_counter;



  int __internal_use;

  __mbstate_t *__statep;
  __mbstate_t __state;



  struct __gconv_trans_data *__trans;
};



typedef struct __gconv_info
{
  size_t __nsteps;
  struct __gconv_step *__steps;
  __extension__ struct __gconv_step_data __data [];
} *__gconv_t;
# 45 "/usr/include/_G_config.h" 2 3 4
typedef union
{
  struct __gconv_info __cd;
  struct
  {
    struct __gconv_info __cd;
    struct __gconv_step_data __data;
  } __combined;
} _G_iconv_t;

typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));
# 33 "/usr/include/libio.h" 2 3 4
# 163 "/usr/include/libio.h" 3 4
struct _IO_jump_t; struct _IO_FILE;
# 173 "/usr/include/libio.h" 3 4
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 196 "/usr/include/libio.h" 3 4
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 264 "/usr/include/libio.h" 3 4
struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;



  int _flags2;

  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;
# 312 "/usr/include/libio.h" 3 4
  __off64_t _offset;





  void *__pad1;
  void *__pad2;

  int _mode;

  char _unused2[15 * sizeof (int) - 2 * sizeof (void *)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
# 351 "/usr/include/libio.h" 3 4
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
                                 size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);




typedef __io_read_fn cookie_read_function_t;
typedef __io_write_fn cookie_write_function_t;
typedef __io_seek_fn cookie_seek_function_t;
typedef __io_close_fn cookie_close_function_t;


typedef struct
{
  __io_read_fn *read;
  __io_write_fn *write;
  __io_seek_fn *seek;
  __io_close_fn *close;
} _IO_cookie_io_functions_t;
typedef _IO_cookie_io_functions_t cookie_io_functions_t;

struct _IO_cookie_file;


extern void _IO_cookie_init (struct _IO_cookie_file *__cfile, int __read_write,
                             void *__cookie, _IO_cookie_io_functions_t __fns);







extern int __underflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern int __uflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern int __overflow (_IO_FILE *, int) __attribute__ ((__nothrow__));
extern wint_t __wunderflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern wint_t __wuflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern wint_t __woverflow (_IO_FILE *, wint_t) __attribute__ ((__nothrow__));
# 441 "/usr/include/libio.h" 3 4
extern int _IO_getc (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_putc (int __c, _IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__));

extern int _IO_peekc_locked (_IO_FILE *__fp) __attribute__ ((__nothrow__));





extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__));
# 471 "/usr/include/libio.h" 3 4
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
                        __gnuc_va_list, int *__restrict) __attribute__ ((__nothrow__));
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
                         __gnuc_va_list) __attribute__ ((__nothrow__));
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) __attribute__ ((__nothrow__));
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) __attribute__ ((__nothrow__));

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) __attribute__ ((__nothrow__));
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) __attribute__ ((__nothrow__));

extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__));
# 73 "/usr/include/stdio.h" 2 3 4
# 86 "/usr/include/stdio.h" 3 4


typedef _G_fpos_t fpos_t;





typedef _G_fpos64_t fpos64_t;
# 138 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/stdio_lim.h" 1 3 4
# 139 "/usr/include/stdio.h" 2 3 4



extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;









extern int remove (__const char *__filename) __attribute__ ((__nothrow__));

extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));









extern FILE *tmpfile (void);
# 176 "/usr/include/stdio.h" 3 4
extern FILE *tmpfile64 (void);



extern char *tmpnam (char *__s) __attribute__ ((__nothrow__));





extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__));
# 198 "/usr/include/stdio.h" 3 4
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);

# 223 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 233 "/usr/include/stdio.h" 3 4
extern int fcloseall (void);









extern FILE *fopen (__const char *__restrict __filename,
                    __const char *__restrict __modes);




extern FILE *freopen (__const char *__restrict __filename,
                      __const char *__restrict __modes,
                      FILE *__restrict __stream);
# 264 "/usr/include/stdio.h" 3 4


extern FILE *fopen64 (__const char *__restrict __filename,
                      __const char *__restrict __modes);
extern FILE *freopen64 (__const char *__restrict __filename,
                        __const char *__restrict __modes,
                        FILE *__restrict __stream);




extern FILE *fdopen (int __fd, __const char *__modes) __attribute__ ((__nothrow__));





extern FILE *fopencookie (void *__restrict __magic_cookie,
                          __const char *__restrict __modes,
                          _IO_cookie_io_functions_t __io_funcs) __attribute__ ((__nothrow__));


extern FILE *fmemopen (void *__s, size_t __len, __const char *__modes) __attribute__ ((__nothrow__));




extern FILE *open_memstream (char **__restrict __bufloc,
                             size_t *__restrict __sizeloc) __attribute__ ((__nothrow__));






extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
                    int __modes, size_t __n) __attribute__ ((__nothrow__));





extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
                       size_t __size) __attribute__ ((__nothrow__));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__));








extern int fprintf (FILE *__restrict __stream,
                    __const char *__restrict __format, ...);




extern int printf (__const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
                    __const char *__restrict __format, ...) __attribute__ ((__nothrow__));





extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
                     __gnuc_va_list __arg);




extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
                     __gnuc_va_list __arg) __attribute__ ((__nothrow__));





extern int snprintf (char *__restrict __s, size_t __maxlen,
                     __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
                      __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));






extern int vasprintf (char **__restrict __ptr, __const char *__restrict __f,
                      __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 0)));
extern int __asprintf (char **__restrict __ptr,
                       __const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3)));
extern int asprintf (char **__restrict __ptr,
                     __const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3)));







extern int vdprintf (int __fd, __const char *__restrict __fmt,
                     __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, __const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));








extern int fscanf (FILE *__restrict __stream,
                   __const char *__restrict __format, ...);




extern int scanf (__const char *__restrict __format, ...);

extern int sscanf (__const char *__restrict __s,
                   __const char *__restrict __format, ...) __attribute__ ((__nothrow__));








extern int vfscanf (FILE *__restrict __s, __const char *__restrict __format,
                    __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0)));





extern int vscanf (__const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0)));


extern int vsscanf (__const char *__restrict __s,
                    __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__scanf__, 2, 0)));









extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);

# 456 "/usr/include/stdio.h" 3 4
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 467 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);

# 500 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);






extern char *gets (char *__s);

# 545 "/usr/include/stdio.h" 3 4
extern char *fgets_unlocked (char *__restrict __s, int __n,
                             FILE *__restrict __stream);
# 561 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
                               size_t *__restrict __n, int __delimiter,
                               FILE *__restrict __stream);
extern __ssize_t getdelim (char **__restrict __lineptr,
                             size_t *__restrict __n, int __delimiter,
                             FILE *__restrict __stream);







extern __ssize_t getline (char **__restrict __lineptr,
                            size_t *__restrict __n,
                            FILE *__restrict __stream);








extern int fputs (__const char *__restrict __s, FILE *__restrict __stream);





extern int puts (__const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
                     size_t __n, FILE *__restrict __stream);




extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
                      size_t __n, FILE *__restrict __s);

# 622 "/usr/include/stdio.h" 3 4
extern int fputs_unlocked (__const char *__restrict __s,
                           FILE *__restrict __stream);
# 633 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
                              size_t __n, FILE *__restrict __stream);
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
                               size_t __n, FILE *__restrict __stream);








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream);




extern void rewind (FILE *__stream);

# 669 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream);
# 688 "/usr/include/stdio.h" 3 4






extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, __const fpos_t *__pos);
# 711 "/usr/include/stdio.h" 3 4



extern int fseeko64 (FILE *__stream, __off64_t __off, int __whence);
extern __off64_t ftello64 (FILE *__stream);
extern int fgetpos64 (FILE *__restrict __stream, fpos64_t *__restrict __pos);
extern int fsetpos64 (FILE *__stream, __const fpos64_t *__pos);




extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__));

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__));




extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__));








extern void perror (__const char *__s);






# 1 "/usr/include/bits/sys_errlist.h" 1 3 4
# 27 "/usr/include/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern __const char *__const sys_errlist[];


extern int _sys_nerr;
extern __const char *__const _sys_errlist[];
# 750 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) __attribute__ ((__nothrow__));




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
# 769 "/usr/include/stdio.h" 3 4
extern FILE *popen (__const char *__command, __const char *__modes);





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) __attribute__ ((__nothrow__));





extern char *cuserid (char *__s);




struct obstack;


extern int obstack_printf (struct obstack *__restrict __obstack,
                           __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3)));
extern int obstack_vprintf (struct obstack *__restrict __obstack,
                            __const char *__restrict __format,
                            __gnuc_va_list __args)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 0)));







extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__));


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__));
# 833 "/usr/include/stdio.h" 3 4

# 51 "system.h" 2
# 110 "system.h"
# 1 "/usr/include/safe-ctype.h" 1 3 4
# 61 "/usr/include/safe-ctype.h" 3 4
enum {

  _sch_isblank = 0x0001,
  _sch_iscntrl = 0x0002,
  _sch_isdigit = 0x0004,
  _sch_islower = 0x0008,
  _sch_isprint = 0x0010,
  _sch_ispunct = 0x0020,
  _sch_isspace = 0x0040,
  _sch_isupper = 0x0080,
  _sch_isxdigit = 0x0100,


  _sch_isidst = 0x0200,
  _sch_isvsp = 0x0400,
  _sch_isnvsp = 0x0800,


  _sch_isalpha = _sch_isupper|_sch_islower,
  _sch_isalnum = _sch_isalpha|_sch_isdigit,
  _sch_isidnum = _sch_isidst|_sch_isdigit,
  _sch_isgraph = _sch_isalnum|_sch_ispunct,
  _sch_iscppsp = _sch_isvsp|_sch_isnvsp,
  _sch_isbasic = _sch_isprint|_sch_iscppsp

};


extern const unsigned short _sch_istable[256];
# 114 "/usr/include/safe-ctype.h" 3 4
extern const unsigned char _sch_toupper[256];
extern const unsigned char _sch_tolower[256];
# 111 "system.h" 2

# 1 "/usr/include/sys/types.h" 1 3 4
# 29 "/usr/include/sys/types.h" 3 4






typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;




typedef __loff_t loff_t;



typedef __ino_t ino_t;






typedef __ino64_t ino64_t;




typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;





typedef __off_t off_t;






typedef __off64_t off64_t;




typedef __pid_t pid_t;




typedef __id_t id_t;




typedef __ssize_t ssize_t;





typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;
# 133 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/time.h" 1 3 4
# 58 "/usr/include/time.h" 3 4


typedef __clock_t clock_t;



# 74 "/usr/include/time.h" 3 4


typedef __time_t time_t;



# 92 "/usr/include/time.h" 3 4
typedef __clockid_t clockid_t;
# 104 "/usr/include/time.h" 3 4
typedef __timer_t timer_t;
# 134 "/usr/include/sys/types.h" 2 3 4



typedef __useconds_t useconds_t;



typedef __suseconds_t suseconds_t;





# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 148 "/usr/include/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
# 191 "/usr/include/sys/types.h" 3 4
typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));


typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

typedef int register_t __attribute__ ((__mode__ (__word__)));
# 213 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 3 4
# 1 "/usr/include/bits/endian.h" 1 3 4
# 38 "/usr/include/endian.h" 2 3 4
# 214 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/select.h" 1 3 4
# 31 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/bits/select.h" 1 3 4
# 32 "/usr/include/sys/select.h" 2 3 4


# 1 "/usr/include/bits/sigset.h" 1 3 4
# 23 "/usr/include/bits/sigset.h" 3 4
typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
# 35 "/usr/include/sys/select.h" 2 3 4



typedef __sigset_t sigset_t;





# 1 "/usr/include/time.h" 1 3 4
# 118 "/usr/include/time.h" 3 4
struct timespec
  {
    __time_t tv_sec;
    long int tv_nsec;
  };
# 45 "/usr/include/sys/select.h" 2 3 4

# 1 "/usr/include/bits/time.h" 1 3 4
# 69 "/usr/include/bits/time.h" 3 4
struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
# 47 "/usr/include/sys/select.h" 2 3 4
# 55 "/usr/include/sys/select.h" 3 4
typedef long int __fd_mask;
# 67 "/usr/include/sys/select.h" 3 4
typedef struct
  {



    __fd_mask fds_bits[1024 / (8 * sizeof (__fd_mask))];





  } fd_set;






typedef __fd_mask fd_mask;
# 99 "/usr/include/sys/select.h" 3 4

# 109 "/usr/include/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
                   fd_set *__restrict __writefds,
                   fd_set *__restrict __exceptfds,
                   struct timeval *__restrict __timeout);
# 121 "/usr/include/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
                    fd_set *__restrict __writefds,
                    fd_set *__restrict __exceptfds,
                    const struct timespec *__restrict __timeout,
                    const __sigset_t *__restrict __sigmask);



# 217 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/sysmacros.h" 1 3 4
# 29 "/usr/include/sys/sysmacros.h" 3 4
__extension__
extern __inline unsigned int gnu_dev_major (unsigned long long int __dev)
     __attribute__ ((__nothrow__));
__extension__
extern __inline unsigned int gnu_dev_minor (unsigned long long int __dev)
     __attribute__ ((__nothrow__));
__extension__
extern __inline unsigned long long int gnu_dev_makedev (unsigned int __major,
                                                        unsigned int __minor)
     __attribute__ ((__nothrow__));


__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_major (unsigned long long int __dev)
{
  return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
}

__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_minor (unsigned long long int __dev)
{
  return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
}

__extension__ extern __inline unsigned long long int
__attribute__ ((__nothrow__)) gnu_dev_makedev (unsigned int __major, unsigned int __minor)
{
  return ((__minor & 0xff) | ((__major & 0xfff) << 8)
          | (((unsigned long long int) (__minor & ~0xff)) << 12)
          | (((unsigned long long int) (__major & ~0xfff)) << 32));
}
# 220 "/usr/include/sys/types.h" 2 3 4




typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 258 "/usr/include/sys/types.h" 3 4
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;





# 1 "/usr/include/bits/pthreadtypes.h" 1 3 4
# 23 "/usr/include/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/bits/sched.h" 1 3 4
# 83 "/usr/include/bits/sched.h" 3 4
struct __sched_param
  {
    int __sched_priority;
  };
# 24 "/usr/include/bits/pthreadtypes.h" 2 3 4


struct _pthread_fastlock
{
  long int __status;
  int __spinlock;

};



typedef struct _pthread_descr_struct *_pthread_descr;





typedef struct __pthread_attr_s
{
  int __detachstate;
  int __schedpolicy;
  struct __sched_param __schedparam;
  int __inheritsched;
  int __scope;
  size_t __guardsize;
  int __stackaddr_set;
  void *__stackaddr;
  size_t __stacksize;
} pthread_attr_t;





__extension__ typedef long long __pthread_cond_align_t;




typedef struct
{
  struct _pthread_fastlock __c_lock;
  _pthread_descr __c_waiting;
  char __padding[48 - sizeof (struct _pthread_fastlock)
                 - sizeof (_pthread_descr) - sizeof (__pthread_cond_align_t)];
  __pthread_cond_align_t __align;
} pthread_cond_t;



typedef struct
{
  int __dummy;
} pthread_condattr_t;


typedef unsigned int pthread_key_t;





typedef struct
{
  int __m_reserved;
  int __m_count;
  _pthread_descr __m_owner;
  int __m_kind;
  struct _pthread_fastlock __m_lock;
} pthread_mutex_t;



typedef struct
{
  int __mutexkind;
} pthread_mutexattr_t;



typedef int pthread_once_t;




typedef struct _pthread_rwlock_t
{
  struct _pthread_fastlock __rw_lock;
  int __rw_readers;
  _pthread_descr __rw_writer;
  _pthread_descr __rw_read_waiting;
  _pthread_descr __rw_write_waiting;
  int __rw_kind;
  int __rw_pshared;
} pthread_rwlock_t;



typedef struct
{
  int __lockkind;
  int __pshared;
} pthread_rwlockattr_t;




typedef volatile int pthread_spinlock_t;


typedef struct {
  struct _pthread_fastlock __ba_lock;
  int __ba_required;
  int __ba_present;
  _pthread_descr __ba_waiting;
} pthread_barrier_t;


typedef struct {
  int __pshared;
} pthread_barrierattr_t;





typedef unsigned long int pthread_t;
# 267 "/usr/include/sys/types.h" 2 3 4



# 113 "system.h" 2

# 1 "/usr/include/errno.h" 1 3 4
# 32 "/usr/include/errno.h" 3 4




# 1 "/usr/include/bits/errno.h" 1 3 4
# 25 "/usr/include/bits/errno.h" 3 4
# 1 "/usr/include/linux/errno.h" 1 3 4



# 1 "/usr/include/asm/errno.h" 1 3 4



# 1 "/usr/include/asm-x86_64/errno.h" 1 3 4



# 1 "/usr/include/asm-generic/errno.h" 1 3 4



# 1 "/usr/include/asm-generic/errno-base.h" 1 3 4
# 5 "/usr/include/asm-generic/errno.h" 2 3 4
# 5 "/usr/include/asm-x86_64/errno.h" 2 3 4
# 5 "/usr/include/asm/errno.h" 2 3 4
# 5 "/usr/include/linux/errno.h" 2 3 4
# 26 "/usr/include/bits/errno.h" 2 3 4
# 38 "/usr/include/bits/errno.h" 3 4
extern int *__errno_location (void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
# 37 "/usr/include/errno.h" 2 3 4
# 55 "/usr/include/errno.h" 3 4
extern char *program_invocation_name, *program_invocation_short_name;




# 69 "/usr/include/errno.h" 3 4
typedef int error_t;
# 115 "system.h" 2
# 125 "system.h"
# 1 "/usr/include/string.h" 1 3 4
# 28 "/usr/include/string.h" 3 4





# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 34 "/usr/include/string.h" 2 3 4




extern void *memcpy (void *__restrict __dest,
                     __const void *__restrict __src, size_t __n) __attribute__ ((__nothrow__));


extern void *memmove (void *__dest, __const void *__src, size_t __n)
     __attribute__ ((__nothrow__));






extern void *memccpy (void *__restrict __dest, __const void *__restrict __src,
                      int __c, size_t __n)
     __attribute__ ((__nothrow__));





extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__));


extern int memcmp (__const void *__s1, __const void *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern void *memchr (__const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern void *rawmemchr (__const void *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern void *memrchr (__const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern char *strcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__));

extern char *strncpy (char *__restrict __dest,
                      __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__));


extern char *strcat (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__));

extern char *strncat (char *__restrict __dest, __const char *__restrict __src,
                      size_t __n) __attribute__ ((__nothrow__));


extern int strcmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern int strncmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern int strcoll (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern size_t strxfrm (char *__restrict __dest,
                       __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__));






# 1 "/usr/include/xlocale.h" 1 3 4
# 28 "/usr/include/xlocale.h" 3 4
typedef struct __locale_struct
{

  struct locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
} *__locale_t;
# 115 "/usr/include/string.h" 2 3 4


extern int strcoll_l (__const char *__s1, __const char *__s2, __locale_t __l)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern size_t strxfrm_l (char *__dest, __const char *__src, size_t __n,
                         __locale_t __l) __attribute__ ((__nothrow__));




extern char *strdup (__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));






extern char *strndup (__const char *__string, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
# 160 "/usr/include/string.h" 3 4


extern char *strchr (__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern char *strrchr (__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern char *strchrnul (__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern size_t strcspn (__const char *__s, __const char *__reject)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern size_t strspn (__const char *__s, __const char *__accept)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern char *strpbrk (__const char *__s, __const char *__accept)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern char *strstr (__const char *__haystack, __const char *__needle)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));



extern char *strtok (char *__restrict __s, __const char *__restrict __delim)
     __attribute__ ((__nothrow__));




extern char *__strtok_r (char *__restrict __s,
                         __const char *__restrict __delim,
                         char **__restrict __save_ptr) __attribute__ ((__nothrow__));

extern char *strtok_r (char *__restrict __s, __const char *__restrict __delim,
                       char **__restrict __save_ptr) __attribute__ ((__nothrow__));




extern char *strcasestr (__const char *__haystack, __const char *__needle)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));






extern void *memmem (__const void *__haystack, size_t __haystacklen,
                     __const void *__needle, size_t __needlelen)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));



extern void *__mempcpy (void *__restrict __dest,
                        __const void *__restrict __src, size_t __n) __attribute__ ((__nothrow__));
extern void *mempcpy (void *__restrict __dest,
                      __const void *__restrict __src, size_t __n) __attribute__ ((__nothrow__));





extern size_t strlen (__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern size_t strnlen (__const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern char *strerror (int __errnum) __attribute__ ((__nothrow__));

# 268 "/usr/include/string.h" 3 4
extern char *strerror_r (int __errnum, char *__buf, size_t __buflen) __attribute__ ((__nothrow__));





extern void __bzero (void *__s, size_t __n) __attribute__ ((__nothrow__));



extern void bcopy (__const void *__src, void *__dest, size_t __n) __attribute__ ((__nothrow__));


extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__));


extern int bcmp (__const void *__s1, __const void *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern char *index (__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern char *rindex (__const char *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));



extern int ffs (int __i) __attribute__ ((__nothrow__)) __attribute__ ((__const__));




extern int ffsl (long int __l) __attribute__ ((__nothrow__)) __attribute__ ((__const__));

__extension__ extern int ffsll (long long int __ll)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));




extern int strcasecmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern int strncasecmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern int strcasecmp_l (__const char *__s1, __const char *__s2,
                         __locale_t __loc) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern int strncasecmp_l (__const char *__s1, __const char *__s2,
                          size_t __n, __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern char *strsep (char **__restrict __stringp,
                     __const char *__restrict __delim) __attribute__ ((__nothrow__));




extern int strverscmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern char *strsignal (int __sig) __attribute__ ((__nothrow__));


extern char *__stpcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__));
extern char *stpcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__));



extern char *__stpncpy (char *__restrict __dest,
                        __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__));
extern char *stpncpy (char *__restrict __dest,
                      __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__));


extern char *strfry (char *__string) __attribute__ ((__nothrow__));


extern void *memfrob (void *__s, size_t __n) __attribute__ ((__nothrow__));






extern char *basename (__const char *__filename) __attribute__ ((__nothrow__));
# 400 "/usr/include/string.h" 3 4

# 126 "system.h" 2
# 134 "system.h"
# 1 "/usr/include/stdlib.h" 1 3 4
# 33 "/usr/include/stdlib.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 34 "/usr/include/stdlib.h" 2 3 4








# 1 "/usr/include/bits/waitflags.h" 1 3 4
# 43 "/usr/include/stdlib.h" 2 3 4
# 1 "/usr/include/bits/waitstatus.h" 1 3 4
# 65 "/usr/include/bits/waitstatus.h" 3 4
union wait
  {
    int w_status;
    struct
      {

        unsigned int __w_termsig:7;
        unsigned int __w_coredump:1;
        unsigned int __w_retcode:8;
        unsigned int:16;







      } __wait_terminated;
    struct
      {

        unsigned int __w_stopval:8;
        unsigned int __w_stopsig:8;
        unsigned int:16;






      } __wait_stopped;
  };
# 44 "/usr/include/stdlib.h" 2 3 4
# 68 "/usr/include/stdlib.h" 3 4
typedef union
  {
    union wait *__uptr;
    int *__iptr;
  } __WAIT_STATUS __attribute__ ((__transparent_union__));
# 93 "/usr/include/stdlib.h" 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;







__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;


# 137 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__));




extern double atof (__const char *__nptr) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern int atoi (__const char *__nptr) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern long int atol (__const char *__nptr) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





__extension__ extern long long int atoll (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern double strtod (__const char *__restrict __nptr,
                      char **__restrict __endptr) __attribute__ ((__nothrow__));





extern float strtof (__const char *__restrict __nptr,
                     char **__restrict __endptr) __attribute__ ((__nothrow__));

extern long double strtold (__const char *__restrict __nptr,
                            char **__restrict __endptr) __attribute__ ((__nothrow__));





extern long int strtol (__const char *__restrict __nptr,
                        char **__restrict __endptr, int __base) __attribute__ ((__nothrow__));

extern unsigned long int strtoul (__const char *__restrict __nptr,
                                  char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__));




__extension__
extern long long int strtoq (__const char *__restrict __nptr,
                             char **__restrict __endptr, int __base) __attribute__ ((__nothrow__));

__extension__
extern unsigned long long int strtouq (__const char *__restrict __nptr,
                                       char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__));





__extension__
extern long long int strtoll (__const char *__restrict __nptr,
                              char **__restrict __endptr, int __base) __attribute__ ((__nothrow__));

__extension__
extern unsigned long long int strtoull (__const char *__restrict __nptr,
                                        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__));

# 229 "/usr/include/stdlib.h" 3 4
extern long int strtol_l (__const char *__restrict __nptr,
                          char **__restrict __endptr, int __base,
                          __locale_t __loc) __attribute__ ((__nothrow__));

extern unsigned long int strtoul_l (__const char *__restrict __nptr,
                                    char **__restrict __endptr,
                                    int __base, __locale_t __loc) __attribute__ ((__nothrow__));

__extension__
extern long long int strtoll_l (__const char *__restrict __nptr,
                                char **__restrict __endptr, int __base,
                                __locale_t __loc) __attribute__ ((__nothrow__));

__extension__
extern unsigned long long int strtoull_l (__const char *__restrict __nptr,
                                          char **__restrict __endptr,
                                          int __base, __locale_t __loc)
     __attribute__ ((__nothrow__));

extern double strtod_l (__const char *__restrict __nptr,
                        char **__restrict __endptr, __locale_t __loc)
     __attribute__ ((__nothrow__));

extern float strtof_l (__const char *__restrict __nptr,
                       char **__restrict __endptr, __locale_t __loc) __attribute__ ((__nothrow__));

extern long double strtold_l (__const char *__restrict __nptr,
                              char **__restrict __endptr,
                              __locale_t __loc) __attribute__ ((__nothrow__));






extern double __strtod_internal (__const char *__restrict __nptr,
                                 char **__restrict __endptr, int __group)
     __attribute__ ((__nothrow__));
extern float __strtof_internal (__const char *__restrict __nptr,
                                char **__restrict __endptr, int __group)
     __attribute__ ((__nothrow__));
extern long double __strtold_internal (__const char *__restrict __nptr,
                                       char **__restrict __endptr,
                                       int __group) __attribute__ ((__nothrow__));

extern long int __strtol_internal (__const char *__restrict __nptr,
                                   char **__restrict __endptr,
                                   int __base, int __group) __attribute__ ((__nothrow__));



extern unsigned long int __strtoul_internal (__const char *__restrict __nptr,
                                             char **__restrict __endptr,
                                             int __base, int __group) __attribute__ ((__nothrow__));




__extension__
extern long long int __strtoll_internal (__const char *__restrict __nptr,
                                         char **__restrict __endptr,
                                         int __base, int __group) __attribute__ ((__nothrow__));



__extension__
extern unsigned long long int __strtoull_internal (__const char *
                                                   __restrict __nptr,
                                                   char **__restrict __endptr,
                                                   int __base, int __group)
     __attribute__ ((__nothrow__));
# 408 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) __attribute__ ((__nothrow__));


extern long int a64l (__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));
# 423 "/usr/include/stdlib.h" 3 4
extern long int random (void) __attribute__ ((__nothrow__));


extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__));





extern char *initstate (unsigned int __seed, char *__statebuf,
                        size_t __statelen) __attribute__ ((__nothrow__));



extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
                     int32_t *__restrict __result) __attribute__ ((__nothrow__));

extern int srandom_r (unsigned int __seed, struct random_data *__buf) __attribute__ ((__nothrow__));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
                        size_t __statelen,
                        struct random_data *__restrict __buf) __attribute__ ((__nothrow__));

extern int setstate_r (char *__restrict __statebuf,
                       struct random_data *__restrict __buf) __attribute__ ((__nothrow__));






extern int rand (void) __attribute__ ((__nothrow__));

extern void srand (unsigned int __seed) __attribute__ ((__nothrow__));




extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__));







extern double drand48 (void) __attribute__ ((__nothrow__));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__));


extern long int lrand48 (void) __attribute__ ((__nothrow__));
extern long int nrand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__));


extern long int mrand48 (void) __attribute__ ((__nothrow__));
extern long int jrand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__));


extern void srand48 (long int __seedval) __attribute__ ((__nothrow__));
extern unsigned short int *seed48 (unsigned short int __seed16v[3]) __attribute__ ((__nothrow__));
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    unsigned long long int __a;
  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
                      double *__restrict __result) __attribute__ ((__nothrow__));
extern int erand48_r (unsigned short int __xsubi[3],
                      struct drand48_data *__restrict __buffer,
                      double *__restrict __result) __attribute__ ((__nothrow__));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
                      long int *__restrict __result) __attribute__ ((__nothrow__));
extern int nrand48_r (unsigned short int __xsubi[3],
                      struct drand48_data *__restrict __buffer,
                      long int *__restrict __result) __attribute__ ((__nothrow__));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
                      long int *__restrict __result) __attribute__ ((__nothrow__));
extern int jrand48_r (unsigned short int __xsubi[3],
                      struct drand48_data *__restrict __buffer,
                      long int *__restrict __result) __attribute__ ((__nothrow__));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__));

extern int seed48_r (unsigned short int __seed16v[3],
                     struct drand48_data *__buffer) __attribute__ ((__nothrow__));

extern int lcong48_r (unsigned short int __param[7],
                      struct drand48_data *__buffer) __attribute__ ((__nothrow__));









extern void *malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));

extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));







extern void *realloc (void *__ptr, size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));

extern void free (void *__ptr) __attribute__ ((__nothrow__));




extern void cfree (void *__ptr) __attribute__ ((__nothrow__));



# 1 "/usr/include/alloca.h" 1 3 4
# 25 "/usr/include/alloca.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 26 "/usr/include/alloca.h" 2 3 4







extern void *alloca (size_t __size) __attribute__ ((__nothrow__));






# 579 "/usr/include/stdlib.h" 2 3 4




extern void *valloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));




extern void abort (void) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__));





extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__));






extern void exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));






extern void _Exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));






extern char *getenv (__const char *__name) __attribute__ ((__nothrow__));




extern char *__secure_getenv (__const char *__name) __attribute__ ((__nothrow__));





extern int putenv (char *__string) __attribute__ ((__nothrow__));





extern int setenv (__const char *__name, __const char *__value, int __replace)
     __attribute__ ((__nothrow__));


extern int unsetenv (__const char *__name) __attribute__ ((__nothrow__));






extern int clearenv (void) __attribute__ ((__nothrow__));
# 663 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) __attribute__ ((__nothrow__));
# 674 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template);
# 683 "/usr/include/stdlib.h" 3 4
extern int mkstemp64 (char *__template);
# 693 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__));








extern int system (__const char *__command);







extern char *canonicalize_file_name (__const char *__name) __attribute__ ((__nothrow__));
# 720 "/usr/include/stdlib.h" 3 4
extern char *realpath (__const char *__restrict __name,
                       char *__restrict __resolved) __attribute__ ((__nothrow__));






typedef int (*__compar_fn_t) (__const void *, __const void *);


typedef __compar_fn_t comparison_fn_t;






extern void *bsearch (__const void *__key, __const void *__base,
                      size_t __nmemb, size_t __size, __compar_fn_t __compar);



extern void qsort (void *__base, size_t __nmemb, size_t __size,
                   __compar_fn_t __compar);



extern int abs (int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long int labs (long int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));



__extension__ extern long long int llabs (long long int __x)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));







extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));




__extension__ extern lldiv_t lldiv (long long int __numer,
                                    long long int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));

# 784 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
                   int *__restrict __sign) __attribute__ ((__nothrow__));




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
                   int *__restrict __sign) __attribute__ ((__nothrow__));




extern char *gcvt (double __value, int __ndigit, char *__buf) __attribute__ ((__nothrow__));




extern char *qecvt (long double __value, int __ndigit,
                    int *__restrict __decpt, int *__restrict __sign) __attribute__ ((__nothrow__));
extern char *qfcvt (long double __value, int __ndigit,
                    int *__restrict __decpt, int *__restrict __sign) __attribute__ ((__nothrow__));
extern char *qgcvt (long double __value, int __ndigit, char *__buf) __attribute__ ((__nothrow__));




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
                   int *__restrict __sign, char *__restrict __buf,
                   size_t __len) __attribute__ ((__nothrow__));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
                   int *__restrict __sign, char *__restrict __buf,
                   size_t __len) __attribute__ ((__nothrow__));

extern int qecvt_r (long double __value, int __ndigit,
                    int *__restrict __decpt, int *__restrict __sign,
                    char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__));
extern int qfcvt_r (long double __value, int __ndigit,
                    int *__restrict __decpt, int *__restrict __sign,
                    char *__restrict __buf, size_t __len) __attribute__ ((__nothrow__));







extern int mblen (__const char *__s, size_t __n) __attribute__ ((__nothrow__));


extern int mbtowc (wchar_t *__restrict __pwc,
                   __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));


extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__));



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
                        __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));

extern size_t wcstombs (char *__restrict __s,
                        __const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__));








extern int rpmatch (__const char *__response) __attribute__ ((__nothrow__));
# 866 "/usr/include/stdlib.h" 3 4
extern int getsubopt (char **__restrict __optionp,
                      char *__const *__restrict __tokens,
                      char **__restrict __valuep) __attribute__ ((__nothrow__));





extern void setkey (__const char *__key) __attribute__ ((__nothrow__));







extern int posix_openpt (int __oflag);







extern int grantpt (int __fd) __attribute__ ((__nothrow__));



extern int unlockpt (int __fd) __attribute__ ((__nothrow__));




extern char *ptsname (int __fd) __attribute__ ((__nothrow__));






extern int ptsname_r (int __fd, char *__buf, size_t __buflen) __attribute__ ((__nothrow__));


extern int getpt (void);






extern int getloadavg (double __loadavg[], int __nelem) __attribute__ ((__nothrow__));






# 135 "system.h" 2
# 167 "system.h"
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/limits.h" 1 3 4
# 11 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/limits.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/syslimits.h" 1 3 4






# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/limits.h" 1 3 4
# 122 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/limits.h" 3 4
# 1 "/usr/include/limits.h" 1 3 4
# 144 "/usr/include/limits.h" 3 4
# 1 "/usr/include/bits/posix1_lim.h" 1 3 4
# 153 "/usr/include/bits/posix1_lim.h" 3 4
# 1 "/usr/include/bits/local_lim.h" 1 3 4
# 36 "/usr/include/bits/local_lim.h" 3 4
# 1 "/usr/include/linux/limits.h" 1 3 4
# 37 "/usr/include/bits/local_lim.h" 2 3 4
# 154 "/usr/include/bits/posix1_lim.h" 2 3 4
# 145 "/usr/include/limits.h" 2 3 4



# 1 "/usr/include/bits/posix2_lim.h" 1 3 4
# 149 "/usr/include/limits.h" 2 3 4



# 1 "/usr/include/bits/xopen_lim.h" 1 3 4
# 34 "/usr/include/bits/xopen_lim.h" 3 4
# 1 "/usr/include/bits/stdio_lim.h" 1 3 4
# 35 "/usr/include/bits/xopen_lim.h" 2 3 4
# 153 "/usr/include/limits.h" 2 3 4
# 123 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/limits.h" 2 3 4
# 8 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/syslimits.h" 2 3 4
# 12 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/limits.h" 2 3 4
# 168 "system.h" 2



# 1 "hwint.h" 1
# 172 "system.h" 2
# 207 "system.h"
# 1 "/usr/include/time.h" 1 3 4
# 30 "/usr/include/time.h" 3 4








# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 39 "/usr/include/time.h" 2 3 4



# 1 "/usr/include/bits/time.h" 1 3 4
# 43 "/usr/include/time.h" 2 3 4
# 129 "/usr/include/time.h" 3 4


struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;


  long int tm_gmtoff;
  __const char *tm_zone;




};








struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };


struct sigevent;
# 178 "/usr/include/time.h" 3 4



extern clock_t clock (void) __attribute__ ((__nothrow__));


extern time_t time (time_t *__timer) __attribute__ ((__nothrow__));


extern double difftime (time_t __time1, time_t __time0)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern time_t mktime (struct tm *__tp) __attribute__ ((__nothrow__));





extern size_t strftime (char *__restrict __s, size_t __maxsize,
                        __const char *__restrict __format,
                        __const struct tm *__restrict __tp) __attribute__ ((__nothrow__));





extern char *strptime (__const char *__restrict __s,
                       __const char *__restrict __fmt, struct tm *__tp)
     __attribute__ ((__nothrow__));







extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
                          __const char *__restrict __format,
                          __const struct tm *__restrict __tp,
                          __locale_t __loc) __attribute__ ((__nothrow__));

extern char *strptime_l (__const char *__restrict __s,
                         __const char *__restrict __fmt, struct tm *__tp,
                         __locale_t __loc) __attribute__ ((__nothrow__));






extern struct tm *gmtime (__const time_t *__timer) __attribute__ ((__nothrow__));



extern struct tm *localtime (__const time_t *__timer) __attribute__ ((__nothrow__));





extern struct tm *gmtime_r (__const time_t *__restrict __timer,
                            struct tm *__restrict __tp) __attribute__ ((__nothrow__));



extern struct tm *localtime_r (__const time_t *__restrict __timer,
                               struct tm *__restrict __tp) __attribute__ ((__nothrow__));





extern char *asctime (__const struct tm *__tp) __attribute__ ((__nothrow__));


extern char *ctime (__const time_t *__timer) __attribute__ ((__nothrow__));







extern char *asctime_r (__const struct tm *__restrict __tp,
                        char *__restrict __buf) __attribute__ ((__nothrow__));


extern char *ctime_r (__const time_t *__restrict __timer,
                      char *__restrict __buf) __attribute__ ((__nothrow__));




extern char *__tzname[2];
extern int __daylight;
extern long int __timezone;




extern char *tzname[2];



extern void tzset (void) __attribute__ ((__nothrow__));



extern int daylight;
extern long int timezone;





extern int stime (__const time_t *__when) __attribute__ ((__nothrow__));
# 309 "/usr/include/time.h" 3 4
extern time_t timegm (struct tm *__tp) __attribute__ ((__nothrow__));


extern time_t timelocal (struct tm *__tp) __attribute__ ((__nothrow__));


extern int dysize (int __year) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
# 324 "/usr/include/time.h" 3 4
extern int nanosleep (__const struct timespec *__requested_time,
                      struct timespec *__remaining);



extern int clock_getres (clockid_t __clock_id, struct timespec *__res) __attribute__ ((__nothrow__));


extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp) __attribute__ ((__nothrow__));


extern int clock_settime (clockid_t __clock_id, __const struct timespec *__tp)
     __attribute__ ((__nothrow__));






extern int clock_nanosleep (clockid_t __clock_id, int __flags,
                            __const struct timespec *__req,
                            struct timespec *__rem);


extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) __attribute__ ((__nothrow__));




extern int timer_create (clockid_t __clock_id,
                         struct sigevent *__restrict __evp,
                         timer_t *__restrict __timerid) __attribute__ ((__nothrow__));


extern int timer_delete (timer_t __timerid) __attribute__ ((__nothrow__));


extern int timer_settime (timer_t __timerid, int __flags,
                          __const struct itimerspec *__restrict __value,
                          struct itimerspec *__restrict __ovalue) __attribute__ ((__nothrow__));


extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
     __attribute__ ((__nothrow__));


extern int timer_getoverrun (timer_t __timerid) __attribute__ ((__nothrow__));
# 386 "/usr/include/time.h" 3 4
extern int getdate_err;
# 395 "/usr/include/time.h" 3 4
extern struct tm *getdate (__const char *__string);
# 409 "/usr/include/time.h" 3 4
extern int getdate_r (__const char *__restrict __string,
                      struct tm *__restrict __resbufp);



# 208 "system.h" 2





# 1 "/usr/include/fcntl.h" 1 3 4
# 29 "/usr/include/fcntl.h" 3 4




# 1 "/usr/include/bits/fcntl.h" 1 3 4
# 26 "/usr/include/bits/fcntl.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 27 "/usr/include/bits/fcntl.h" 2 3 4
# 152 "/usr/include/bits/fcntl.h" 3 4
struct flock
  {
    short int l_type;
    short int l_whence;

    __off_t l_start;
    __off_t l_len;




    __pid_t l_pid;
  };


struct flock64
  {
    short int l_type;
    short int l_whence;
    __off64_t l_start;
    __off64_t l_len;
    __pid_t l_pid;
  };
# 197 "/usr/include/bits/fcntl.h" 3 4



extern ssize_t readahead (int __fd, __off64_t __offset, size_t __count)
    __attribute__ ((__nothrow__));


# 34 "/usr/include/fcntl.h" 2 3 4



# 1 "/usr/include/sys/stat.h" 1 3 4
# 103 "/usr/include/sys/stat.h" 3 4


# 1 "/usr/include/bits/stat.h" 1 3 4
# 43 "/usr/include/bits/stat.h" 3 4
struct stat
  {
    __dev_t st_dev;




    __ino_t st_ino;







    __nlink_t st_nlink;
    __mode_t st_mode;

    __uid_t st_uid;
    __gid_t st_gid;

    int pad0;

    __dev_t st_rdev;




    __off_t st_size;



    __blksize_t st_blksize;

    __blkcnt_t st_blocks;
# 88 "/usr/include/bits/stat.h" 3 4
    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
# 103 "/usr/include/bits/stat.h" 3 4
    long int __unused[3];
# 112 "/usr/include/bits/stat.h" 3 4
  };



struct stat64
  {
    __dev_t st_dev;

    __ino64_t st_ino;
    __nlink_t st_nlink;
    __mode_t st_mode;






    __uid_t st_uid;
    __gid_t st_gid;

    int pad0;
    __dev_t st_rdev;
    __off_t st_size;





    __blksize_t st_blksize;
    __blkcnt64_t st_blocks;







    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
# 164 "/usr/include/bits/stat.h" 3 4
    long int __unused[3];



  };
# 106 "/usr/include/sys/stat.h" 2 3 4
# 207 "/usr/include/sys/stat.h" 3 4
extern int stat (__const char *__restrict __file,
                 struct stat *__restrict __buf) __attribute__ ((__nothrow__));



extern int fstat (int __fd, struct stat *__buf) __attribute__ ((__nothrow__));
# 224 "/usr/include/sys/stat.h" 3 4
extern int stat64 (__const char *__restrict __file,
                   struct stat64 *__restrict __buf) __attribute__ ((__nothrow__));
extern int fstat64 (int __fd, struct stat64 *__buf) __attribute__ ((__nothrow__));






extern int lstat (__const char *__restrict __file,
                  struct stat *__restrict __buf) __attribute__ ((__nothrow__));
# 245 "/usr/include/sys/stat.h" 3 4
extern int lstat64 (__const char *__restrict __file,
                    struct stat64 *__restrict __buf) __attribute__ ((__nothrow__));





extern int chmod (__const char *__file, __mode_t __mode) __attribute__ ((__nothrow__));





extern int lchmod (__const char *__file, __mode_t __mode) __attribute__ ((__nothrow__));




extern int fchmod (int __fd, __mode_t __mode) __attribute__ ((__nothrow__));





extern __mode_t umask (__mode_t __mask) __attribute__ ((__nothrow__));




extern __mode_t getumask (void) __attribute__ ((__nothrow__));



extern int mkdir (__const char *__path, __mode_t __mode) __attribute__ ((__nothrow__));





extern int mknod (__const char *__path, __mode_t __mode, __dev_t __dev)
     __attribute__ ((__nothrow__));




extern int mkfifo (__const char *__path, __mode_t __mode) __attribute__ ((__nothrow__));
# 316 "/usr/include/sys/stat.h" 3 4
extern int __fxstat (int __ver, int __fildes, struct stat *__stat_buf) __attribute__ ((__nothrow__));
extern int __xstat (int __ver, __const char *__filename,
                    struct stat *__stat_buf) __attribute__ ((__nothrow__));
extern int __lxstat (int __ver, __const char *__filename,
                     struct stat *__stat_buf) __attribute__ ((__nothrow__));
# 338 "/usr/include/sys/stat.h" 3 4
extern int __fxstat64 (int __ver, int __fildes, struct stat64 *__stat_buf)
     __attribute__ ((__nothrow__));
extern int __xstat64 (int __ver, __const char *__filename,
                      struct stat64 *__stat_buf) __attribute__ ((__nothrow__));
extern int __lxstat64 (int __ver, __const char *__filename,
                       struct stat64 *__stat_buf) __attribute__ ((__nothrow__));

extern int __xmknod (int __ver, __const char *__path, __mode_t __mode,
                     __dev_t *__dev) __attribute__ ((__nothrow__));




extern __inline__ int
__attribute__ ((__nothrow__)) stat (__const char *__path, struct stat *__statbuf)
{
  return __xstat (1, __path, __statbuf);
}


extern __inline__ int
__attribute__ ((__nothrow__)) lstat (__const char *__path, struct stat *__statbuf)
{
  return __lxstat (1, __path, __statbuf);
}


extern __inline__ int
__attribute__ ((__nothrow__)) fstat (int __fd, struct stat *__statbuf)
{
  return __fxstat (1, __fd, __statbuf);
}


extern __inline__ int
__attribute__ ((__nothrow__)) mknod (__const char *__path, __mode_t __mode, __dev_t __dev)
{
  return __xmknod (0, __path, __mode, &__dev);
}





extern __inline__ int
__attribute__ ((__nothrow__)) stat64 (__const char *__path, struct stat64 *__statbuf)
{
  return __xstat64 (1, __path, __statbuf);
}


extern __inline__ int
__attribute__ ((__nothrow__)) lstat64 (__const char *__path, struct stat64 *__statbuf)
{
  return __lxstat64 (1, __path, __statbuf);
}


extern __inline__ int
__attribute__ ((__nothrow__)) fstat64 (int __fd, struct stat64 *__statbuf)
{
  return __fxstat64 (1, __fd, __statbuf);
}





# 38 "/usr/include/fcntl.h" 2 3 4
# 63 "/usr/include/fcntl.h" 3 4
extern int fcntl (int __fd, int __cmd, ...);
# 72 "/usr/include/fcntl.h" 3 4
extern int open (__const char *__file, int __oflag, ...);
# 81 "/usr/include/fcntl.h" 3 4
extern int open64 (__const char *__file, int __oflag, ...);
# 90 "/usr/include/fcntl.h" 3 4
extern int creat (__const char *__file, __mode_t __mode);
# 100 "/usr/include/fcntl.h" 3 4
extern int creat64 (__const char *__file, __mode_t __mode);
# 119 "/usr/include/fcntl.h" 3 4
extern int lockf (int __fd, int __cmd, __off_t __len);
# 128 "/usr/include/fcntl.h" 3 4
extern int lockf64 (int __fd, int __cmd, __off64_t __len);







extern int posix_fadvise (int __fd, __off_t __offset, __off_t __len,
                          int __advise) __attribute__ ((__nothrow__));
# 148 "/usr/include/fcntl.h" 3 4
extern int posix_fadvise64 (int __fd, __off64_t __offset, __off64_t __len,
                            int __advise) __attribute__ ((__nothrow__));
# 158 "/usr/include/fcntl.h" 3 4
extern int posix_fallocate (int __fd, __off_t __offset, __off_t __len);
# 169 "/usr/include/fcntl.h" 3 4
extern int posix_fallocate64 (int __fd, __off64_t __offset, __off64_t __len);




# 214 "system.h" 2
# 312 "system.h"
# 1 "/usr/include/malloc.h" 1 3 4
# 46 "/usr/include/malloc.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 47 "/usr/include/malloc.h" 2 3 4
# 120 "/usr/include/malloc.h" 3 4
extern void * malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));


extern void * calloc (size_t __nmemb, size_t __size) __attribute__ ((__nothrow__))
       __attribute__ ((__malloc__));



extern void * realloc (void * __ptr, size_t __size) __attribute__ ((__nothrow__))

       __attribute__ ((__malloc__));


extern void free (void * __ptr) __attribute__ ((__nothrow__));


extern void cfree (void * __ptr) __attribute__ ((__nothrow__));


extern void * memalign (size_t __alignment, size_t __size) __attribute__ ((__nothrow__));


extern void * valloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));



extern void * pvalloc (size_t __size) __attribute__ ((__nothrow__))
       __attribute__ ((__malloc__));



extern void * (*__morecore) (ptrdiff_t __size);


extern void * __default_morecore (ptrdiff_t __size) __attribute__ ((__nothrow__))
       __attribute__ ((__malloc__));



struct mallinfo {
  int arena;
  int ordblks;
  int smblks;
  int hblks;
  int hblkhd;
  int usmblks;
  int fsmblks;
  int uordblks;
  int fordblks;
  int keepcost;
};


extern struct mallinfo mallinfo (void) __attribute__ ((__nothrow__));
# 197 "/usr/include/malloc.h" 3 4
extern int mallopt (int __param, int __val) __attribute__ ((__nothrow__));



extern int malloc_trim (size_t __pad) __attribute__ ((__nothrow__));



extern size_t malloc_usable_size (void * __ptr) __attribute__ ((__nothrow__));


extern void malloc_stats (void) __attribute__ ((__nothrow__));


extern void * malloc_get_state (void) __attribute__ ((__nothrow__));



extern int malloc_set_state (void * __ptr) __attribute__ ((__nothrow__));




extern void (*__malloc_initialize_hook) (void);

extern void (*__free_hook) (void * __ptr, __const void *);

extern void * (*__malloc_hook) (size_t __size, __const void *);

extern void * (*__realloc_hook) (void * __ptr, size_t __size, __const void *);


extern void * (*__memalign_hook) (size_t __alignment, size_t __size, __const void *);


extern void (*__after_morecore_hook) (void);


extern void __malloc_check_init (void) __attribute__ ((__nothrow__));
# 313 "system.h" 2
# 493 "system.h"
# 1 "libiberty.h" 1
# 46 "libiberty.h"
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 47 "libiberty.h" 2
# 58 "libiberty.h"
extern char **buildargv (const char *) __attribute__ ((__malloc__));



extern void freeargv (char **);




extern char **dupargv (char **) __attribute__ ((__malloc__));
# 81 "libiberty.h"
extern char *basename (const char *);
# 90 "libiberty.h"
extern const char *lbasename (const char *);





extern char *concat (const char *, ...) __attribute__ ((__malloc__));
# 105 "libiberty.h"
extern char *reconcat (char *, const char *, ...) __attribute__ ((__malloc__));





extern unsigned long concat_length (const char *, ...);






extern char *concat_copy (char *, const char *, ...);






extern char *concat_copy2 (const char *, ...);



extern char *libiberty_concat_ptr;
# 141 "libiberty.h"
extern int fdmatch (int fd1, int fd2);




extern char * getpwd (void);



extern long get_run_time (void);



extern char *choose_temp_base (void) __attribute__ ((__malloc__));



extern char *make_temp_file (const char *) __attribute__ ((__malloc__));



extern const char *spaces (int count);




extern int errno_max (void);




extern const char *strerrno (int);



extern int strtoerrno (const char *);



extern char *xstrerror (int);




extern int signo_max (void);
# 197 "libiberty.h"
extern const char *strsigno (int);



extern int strtosigno (const char *);



extern int xatexit (void (*fn) (void));



extern void xexit (int status) __attribute__ ((__noreturn__));



extern void xmalloc_set_program_name (const char *);


extern void xmalloc_failed (size_t) __attribute__ ((__noreturn__));





extern void * xmalloc (size_t) __attribute__ ((__malloc__));





extern void * xrealloc (void *, size_t);




extern void * xcalloc (size_t, size_t) __attribute__ ((__malloc__));



extern char *xstrdup (const char *) __attribute__ ((__malloc__));



extern void * xmemdup (const void *, size_t, size_t) __attribute__ ((__malloc__));


extern void specqsort (void *, int, int, int (*)() );





extern const char _hex_value[256];
extern void hex_init (void);
# 267 "libiberty.h"
extern int pexecute (const char *, char * const *, const char *, const char *, char **, char **, int);




extern int pwait (int, int *, int);




extern int asprintf (char **, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)));




extern int vasprintf (char **, const char *, va_list)
  __attribute__ ((__format__ (__printf__, 2, 0)));
# 494 "system.h" 2
# 1 "symcat.h" 1
# 495 "system.h" 2
# 624 "system.h"
        
# 34 "c-typeck.c" 2
# 1 "rtl.h" 1
# 25 "rtl.h"
struct function;

# 1 "machmode.h" 1
# 29 "machmode.h"
enum machine_mode {
# 1 "machmode.def" 1
# 74 "machmode.def"
VOIDmode,

BImode,
QImode,
HImode,
SImode,
DImode,
TImode,
OImode,




PQImode,
PHImode,
PSImode,
PDImode,

QFmode,
HFmode,
TQFmode,
SFmode,
DFmode,
XFmode,
TFmode,


QCmode,
HCmode,
SCmode,
DCmode,
XCmode,
TCmode,

CQImode,
CHImode,
CSImode,
CDImode,
CTImode,
COImode,







V2QImode,
V2HImode,
V2SImode,
V2DImode,

V4QImode,
V4HImode,
V4SImode,
V4DImode,

V8QImode,
V8HImode,
V8SImode,
V8DImode,

V16QImode,

V2SFmode,
V2DFmode,

V4SFmode,
V4DFmode,

V8SFmode,
V8DFmode,
V16SFmode,



BLKmode,
# 159 "machmode.def"
CCmode,


CCGCmode, CCGOCmode, CCNOmode, CCZmode, CCFPmode, CCFPUmode,
# 31 "machmode.h" 2
MAX_MACHINE_MODE };
# 41 "machmode.h"
extern const char * const mode_name[(int) MAX_MACHINE_MODE];


enum mode_class { MODE_RANDOM, MODE_INT, MODE_FLOAT, MODE_PARTIAL_INT, MODE_CC,
                  MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT,
                  MODE_VECTOR_INT, MODE_VECTOR_FLOAT,
                  MAX_MODE_CLASS};




extern const enum mode_class mode_class[(int) MAX_MACHINE_MODE];
# 80 "machmode.h"
extern const unsigned char mode_size[(int) MAX_MACHINE_MODE];




extern const unsigned char mode_unit_size[(int) MAX_MACHINE_MODE];
# 96 "machmode.h"
extern const unsigned short mode_bitsize[(int) MAX_MACHINE_MODE];
# 106 "machmode.h"
extern const unsigned long long mode_mask_array[(int) MAX_MACHINE_MODE];



extern const enum machine_mode inner_mode_array[(int) MAX_MACHINE_MODE];
# 123 "machmode.h"
extern const unsigned char mode_wider_mode[(int) MAX_MACHINE_MODE];






extern enum machine_mode mode_for_size (unsigned int, enum mode_class, int);




extern enum machine_mode smallest_mode_for_size
                                (unsigned int, enum mode_class);





extern enum machine_mode int_mode_for_mode (enum machine_mode);



extern enum machine_mode get_best_mode (int, int, unsigned int, enum machine_mode, int);




extern unsigned get_mode_alignment (enum machine_mode);





extern const enum machine_mode class_narrowest_mode[(int) MAX_MODE_CLASS];





extern enum machine_mode byte_mode;
extern enum machine_mode word_mode;
extern enum machine_mode ptr_mode;
# 28 "rtl.h" 2
# 41 "rtl.h"
enum rtx_code {


# 1 "rtl.def" 1
# 70 "rtl.def"
UNKNOWN ,



NIL ,




INCLUDE ,






EXPR_LIST ,



INSN_LIST ,
# 129 "rtl.def"
MATCH_OPERAND ,






MATCH_SCRATCH ,




MATCH_DUP ,







MATCH_OPERATOR ,
# 158 "rtl.def"
MATCH_PARALLEL ,




MATCH_OP_DUP ,




MATCH_PAR_DUP ,




MATCH_INSN ,
# 192 "rtl.def"
DEFINE_INSN ,







DEFINE_PEEPHOLE ,
# 211 "rtl.def"
DEFINE_SPLIT ,
# 239 "rtl.def"
DEFINE_INSN_AND_SPLIT ,



DEFINE_PEEPHOLE2 ,



DEFINE_COMBINE ,
# 260 "rtl.def"
DEFINE_EXPAND ,
# 276 "rtl.def"
DEFINE_DELAY ,
# 317 "rtl.def"
DEFINE_FUNCTION_UNIT ,


DEFINE_ASM_ATTRIBUTES ,
# 333 "rtl.def"
DEFINE_COND_EXEC ,





SEQUENCE ,


ADDRESS ,
# 353 "rtl.def"
DEFINE_ATTR ,


ATTR ,







SET_ATTR ,
# 379 "rtl.def"
SET_ATTR_ALTERNATIVE ,




EQ_ATTR ,
# 394 "rtl.def"
ATTR_FLAG ,
# 407 "rtl.def"
INSN ,



JUMP_INSN ,






CALL_INSN ,


BARRIER ,
# 430 "rtl.def"
CODE_LABEL ,






NOTE ,
# 450 "rtl.def"
COND_EXEC ,


PARALLEL ,







ASM_INPUT ,
# 475 "rtl.def"
ASM_OPERANDS ,
# 486 "rtl.def"
UNSPEC ,


UNSPEC_VOLATILE ,



ADDR_VEC ,
# 518 "rtl.def"
ADDR_DIFF_VEC ,
# 529 "rtl.def"
PREFETCH ,
# 541 "rtl.def"
SET ,




USE ,




CLOBBER ,





CALL ,



RETURN ,





TRAP_IF ,




RESX ,






CONST_INT ,






CONST_DOUBLE ,


CONST_VECTOR ,


CONST_STRING ,





CONST ,



PC ,


VALUE ,
# 614 "rtl.def"
REG ,






SCRATCH ,
# 631 "rtl.def"
SUBREG ,
# 644 "rtl.def"
STRICT_LOW_PART ,





CONCAT ,




MEM ,





LABEL_REF ,





SYMBOL_REF ,






CC0 ,
# 683 "rtl.def"
ADDRESSOF ,
# 701 "rtl.def"
QUEUED ,
# 713 "rtl.def"
IF_THEN_ELSE ,
# 722 "rtl.def"
COND ,


COMPARE ,


PLUS ,


MINUS ,


NEG ,

MULT ,


DIV ,

MOD ,


UDIV ,
UMOD ,


AND ,

IOR ,

XOR ,

NOT ,




ASHIFT ,
ROTATE ,
ASHIFTRT ,
LSHIFTRT ,
ROTATERT ,





SMIN ,
SMAX ,
UMIN ,
UMAX ,
# 781 "rtl.def"
PRE_DEC ,
PRE_INC ,
POST_DEC ,
POST_INC ,
# 798 "rtl.def"
PRE_MODIFY ,
POST_MODIFY ,



NE ,
EQ ,
GE ,
GT ,
LE ,
LT ,
GEU ,
GTU ,
LEU ,
LTU ,


UNORDERED ,
ORDERED ,


UNEQ ,
UNGE ,
UNGT ,
UNLE ,
UNLT ,


LTGT ,




SIGN_EXTEND ,


ZERO_EXTEND ,


TRUNCATE ,


FLOAT_EXTEND ,
FLOAT_TRUNCATE ,


FLOAT ,







FIX ,


UNSIGNED_FLOAT ,




UNSIGNED_FIX ,


ABS ,


SQRT ,




FFS ,
# 882 "rtl.def"
SIGN_EXTRACT ,


ZERO_EXTRACT ,




HIGH ,



LO_SUM ,
# 907 "rtl.def"
RANGE_INFO ,
# 922 "rtl.def"
RANGE_REG ,





RANGE_VAR ,



RANGE_LIVE ,




CONSTANT_P_RTX ,
# 958 "rtl.def"
CALL_PLACEHOLDER ,






VEC_MERGE ,





VEC_SELECT ,




VEC_CONCAT ,





VEC_DUPLICATE ,


SS_PLUS ,


US_PLUS ,


SS_MINUS ,


US_MINUS ,


SS_TRUNCATE ,


US_TRUNCATE ,
# 1014 "rtl.def"
PHI ,
# 45 "rtl.h" 2


  LAST_AND_UNUSED_RTX_CODE};






extern const unsigned char rtx_length[((int) LAST_AND_UNUSED_RTX_CODE)];


extern const char * const rtx_name[((int) LAST_AND_UNUSED_RTX_CODE)];


extern const char * const rtx_format[((int) LAST_AND_UNUSED_RTX_CODE)];


extern const char rtx_class[((int) LAST_AND_UNUSED_RTX_CODE)];




typedef struct
{

  unsigned min_align: 8;

  unsigned base_after_vec: 1;
  unsigned min_after_vec: 1;

  unsigned max_after_vec: 1;

  unsigned min_after_base: 1;

  unsigned max_after_base: 1;


  unsigned offset_unsigned: 1;
  unsigned : 2;
  unsigned scale : 8;
} addr_diff_vec_flags;





typedef struct
{
  long long alias;
  tree expr;
  rtx offset;
  rtx size;
  unsigned int align;
} mem_attrs;



typedef union rtunion_def
{
  long long rtwint;
  int rtint;
  unsigned int rtuint;
  const char *rtstr;
  rtx rtx;
  rtvec rtvec;
  enum machine_mode rttype;
  addr_diff_vec_flags rt_addr_diff_vec_flags;
  struct cselib_val_struct *rt_cselib;
  struct bitmap_head_def *rtbit;
  tree rttree;
  struct basic_block_def *bb;
  mem_attrs *rtmem;
} rtunion;



struct rtx_def
{

  enum rtx_code code: 16;


  enum machine_mode mode : 8;






  unsigned int jump : 1;


  unsigned int call : 1;
# 149 "rtl.h"
  unsigned int unchanging : 1;







  unsigned int volatil : 1;
# 175 "rtl.h"
  unsigned int in_struct : 1;






  unsigned int used : 1;




  unsigned integrated : 1;
# 196 "rtl.h"
  unsigned frame_related : 1;




  rtunion fld[1];
};
# 222 "rtl.h"
struct rtvec_def {
  int num_elem;
  rtx elem[1];
};
# 451 "rtl.h"
enum reg_note
{



  REG_DEAD = 1,


  REG_INC,
# 470 "rtl.h"
  REG_EQUIV,




  REG_EQUAL,





  REG_WAS_0,





  REG_RETVAL,




  REG_LIBCALL,






  REG_NONNEG,



  REG_NO_CONFLICT,


  REG_UNUSED,
# 515 "rtl.h"
  REG_CC_SETTER, REG_CC_USER,




  REG_LABEL,





  REG_DEP_ANTI, REG_DEP_OUTPUT,





  REG_BR_PROB,




  REG_EXEC_COUNT,



  REG_NOALIAS,



  REG_SAVE_AREA,





  REG_BR_PRED,




  REG_FRAME_RELATED_EXPR,




  REG_EH_CONTEXT,





  REG_EH_REGION,


  REG_SAVE_NOTE,





  REG_MAYBE_DEAD,


  REG_NORETURN,



  REG_NON_LOCAL_GOTO,



  REG_SETJMP,


  REG_ALWAYS_RETURN,



  REG_VTABLE_REF
};
# 607 "rtl.h"
extern const char * const reg_note_name[];
# 663 "rtl.h"
enum insn_note
{

  NOTE_INSN_BIAS = -100,



  NOTE_INSN_DELETED,



  NOTE_INSN_BLOCK_BEG,
  NOTE_INSN_BLOCK_END,


  NOTE_INSN_LOOP_BEG,
  NOTE_INSN_LOOP_END,


  NOTE_INSN_LOOP_CONT,

  NOTE_INSN_LOOP_VTOP,





  NOTE_INSN_LOOP_END_TOP_COND,






  NOTE_INSN_FUNCTION_END,


  NOTE_INSN_PROLOGUE_END,


  NOTE_INSN_EPILOGUE_BEG,


  NOTE_INSN_DELETED_LABEL,




  NOTE_INSN_FUNCTION_BEG,



  NOTE_INSN_EH_REGION_BEG,
  NOTE_INSN_EH_REGION_END,




  NOTE_INSN_REPEATED_LINE_NUMBER,



  NOTE_INSN_RANGE_BEG,
  NOTE_INSN_RANGE_END,


  NOTE_INSN_LIVE,


  NOTE_INSN_BASIC_BLOCK,



  NOTE_INSN_EXPECTED_VALUE,

  NOTE_INSN_MAX
};



extern const char * const note_insn_name[NOTE_INSN_MAX - NOTE_INSN_BIAS];
# 840 "rtl.h"
extern unsigned int subreg_lsb (rtx);
extern unsigned int subreg_regno_offset (unsigned int, enum machine_mode, unsigned int, enum machine_mode);



extern unsigned int subreg_regno (rtx);
# 1211 "rtl.h"
extern int rtx_equal_function_value_matters;


extern int generating_concat_p;




extern int ceil_log2 (unsigned long long);




extern rtx expand_builtin_expect_jump (tree, rtx, rtx);


extern void set_stack_check_libfunc (rtx);
extern long long trunc_int_for_mode (long long, enum machine_mode);

extern rtx plus_constant_wide (rtx, long long);
extern rtx plus_constant_for_output_wide (rtx, long long);
extern void optimize_save_area_alloca (rtx);


extern rtx gen_rtx (enum rtx_code, enum machine_mode, ...);

extern rtvec gen_rtvec (int, ...);
extern rtx copy_insn_1 (rtx);
extern rtx copy_insn (rtx);
extern rtx gen_int_mode (long long, enum machine_mode);



extern rtx rtx_alloc (enum rtx_code);
extern rtvec rtvec_alloc (int);
extern rtx copy_rtx (rtx);


extern rtx copy_rtx_if_shared (rtx);


extern rtx copy_most_rtx (rtx, rtx);
extern rtx shallow_copy_rtx (rtx);
extern int rtx_equal_p (rtx, rtx);


extern rtvec gen_rtvec_v (int, rtx *);
extern rtx gen_reg_rtx (enum machine_mode);
extern rtx gen_label_rtx (void);
extern int subreg_hard_regno (rtx, int);
extern rtx gen_lowpart_common (enum machine_mode, rtx);
extern rtx gen_lowpart (enum machine_mode, rtx);


extern rtx gen_lowpart_if_possible (enum machine_mode, rtx);


extern rtx gen_highpart (enum machine_mode, rtx);
extern rtx gen_highpart_mode (enum machine_mode, enum machine_mode, rtx);

extern rtx gen_realpart (enum machine_mode, rtx);
extern rtx gen_imagpart (enum machine_mode, rtx);
extern rtx operand_subword (rtx, unsigned int, int, enum machine_mode);

extern rtx constant_subword (rtx, int, enum machine_mode);



extern rtx operand_subword_force (rtx, unsigned int, enum machine_mode);

extern int subreg_lowpart_p (rtx);
extern unsigned int subreg_lowpart_offset (enum machine_mode, enum machine_mode);

extern unsigned int subreg_highpart_offset (enum machine_mode, enum machine_mode);

extern rtx make_safe_from (rtx, rtx);
extern rtx convert_memory_address (enum machine_mode, rtx);
extern rtx get_insns (void);
extern const char *get_insn_name (int);
extern rtx get_last_insn (void);
extern rtx get_last_insn_anywhere (void);
extern void start_sequence (void);
extern void push_to_sequence (rtx);
extern void end_sequence (void);
extern void push_to_full_sequence (rtx, rtx);
extern void end_full_sequence (rtx*, rtx*);
extern rtx gen_sequence (void);


extern rtx immed_double_const (long long, long long, enum machine_mode);
extern rtx mem_for_const_double (rtx);
extern rtx force_const_mem (enum machine_mode, rtx);


extern rtx get_pool_constant (rtx);
extern rtx get_pool_constant_mark (rtx, _Bool *);
extern enum machine_mode get_pool_mode (rtx);
extern rtx get_pool_constant_for_function (struct function *, rtx);
extern enum machine_mode get_pool_mode_for_function (struct function *, rtx);
extern int get_pool_offset (rtx);
extern rtx simplify_subtraction (rtx);


extern rtx assign_stack_local (enum machine_mode, long long, int);

extern rtx assign_stack_temp (enum machine_mode, long long, int);

extern rtx assign_stack_temp_for_type (enum machine_mode, long long, int, tree);

extern rtx assign_temp (tree, int, int, int);

extern rtx emit_insn_before (rtx, rtx);
extern rtx emit_jump_insn_before (rtx, rtx);
extern rtx emit_call_insn_before (rtx, rtx);
extern rtx emit_barrier_before (rtx);
extern rtx emit_label_before (rtx, rtx);
extern rtx emit_note_before (int, rtx);
extern rtx emit_insn_after (rtx, rtx);
extern rtx emit_jump_insn_after (rtx, rtx);
extern rtx emit_barrier_after (rtx);
extern rtx emit_label_after (rtx, rtx);
extern rtx emit_note_after (int, rtx);
extern rtx emit_line_note_after (const char *, int, rtx);
extern rtx emit_insn (rtx);
extern rtx emit_insns (rtx);
extern rtx emit_insns_before (rtx, rtx);
extern rtx emit_insns_after (rtx, rtx);
extern rtx emit_jump_insn (rtx);
extern rtx emit_call_insn (rtx);
extern rtx emit_label (rtx);
extern rtx emit_barrier (void);
extern rtx emit_line_note (const char *, int);
extern rtx emit_note (const char *, int);
extern rtx emit_line_note_force (const char *, int);
extern rtx make_insn_raw (rtx);
extern rtx previous_insn (rtx);
extern rtx next_insn (rtx);
extern rtx prev_nonnote_insn (rtx);
extern rtx next_nonnote_insn (rtx);
extern rtx prev_real_insn (rtx);
extern rtx next_real_insn (rtx);
extern rtx prev_active_insn (rtx);
extern rtx next_active_insn (rtx);
extern int active_insn_p (rtx);
extern rtx prev_label (rtx);
extern rtx next_label (rtx);
extern rtx next_cc0_user (rtx);
extern rtx prev_cc0_setter (rtx);


extern rtx next_nondeleted_insn (rtx);
extern enum rtx_code reverse_condition (enum rtx_code);
extern enum rtx_code reverse_condition_maybe_unordered (enum rtx_code);
extern enum rtx_code swap_condition (enum rtx_code);
extern enum rtx_code unsigned_condition (enum rtx_code);
extern enum rtx_code signed_condition (enum rtx_code);
extern void mark_jump_label (rtx, rtx, int);
extern void cleanup_barriers (void);


extern _Bool squeeze_notes (rtx *, rtx *);
extern rtx delete_related_insns (rtx);
extern void delete_jump (rtx);
extern void delete_barrier (rtx);
extern rtx get_label_before (rtx);
extern rtx get_label_after (rtx);
extern rtx follow_jumps (rtx);


extern rtx *find_constant_term_loc (rtx *);


extern rtx try_split (rtx, rtx, int);
extern int split_branch_probability;


extern rtx split_insns (rtx, rtx);


extern rtx simplify_unary_operation (enum rtx_code, enum machine_mode, rtx, enum machine_mode);


extern rtx simplify_binary_operation (enum rtx_code, enum machine_mode, rtx, rtx);


extern rtx simplify_ternary_operation (enum rtx_code, enum machine_mode, enum machine_mode, rtx, rtx, rtx);



extern rtx simplify_relational_operation (enum rtx_code, enum machine_mode, rtx, rtx);


extern rtx simplify_gen_binary (enum rtx_code, enum machine_mode, rtx, rtx);


extern rtx simplify_gen_unary (enum rtx_code, enum machine_mode, rtx, enum machine_mode);


extern rtx simplify_gen_ternary (enum rtx_code, enum machine_mode, enum machine_mode, rtx, rtx, rtx);



extern rtx simplify_gen_relational (enum rtx_code, enum machine_mode, enum machine_mode, rtx, rtx);



extern rtx simplify_subreg (enum machine_mode, rtx, enum machine_mode, unsigned int);



extern rtx simplify_gen_subreg (enum machine_mode, rtx, enum machine_mode, unsigned int);



extern rtx simplify_replace_rtx (rtx, rtx, rtx);
extern rtx simplify_rtx (rtx);
extern rtx avoid_constant_pool_reference (rtx);


extern rtx gen_mem_addressof (rtx, tree);


extern enum machine_mode choose_hard_reg_mode (unsigned int, unsigned int);



extern rtx set_unique_reg_note (rtx, enum reg_note, rtx);
# 1448 "rtl.h"
extern int rtx_addr_can_trap_p (rtx);
extern int rtx_unstable_p (rtx);
extern int rtx_varies_p (rtx, int);
extern int rtx_addr_varies_p (rtx, int);
extern long long get_integer_term (rtx);
extern rtx get_related_value (rtx);
extern rtx get_jump_table_offset (rtx, rtx *);
extern int reg_mentioned_p (rtx, rtx);
extern int count_occurrences (rtx, rtx, int);
extern int reg_referenced_p (rtx, rtx);
extern int reg_used_between_p (rtx, rtx, rtx);
extern int reg_referenced_between_p (rtx, rtx, rtx);
extern int reg_set_between_p (rtx, rtx, rtx);
extern int regs_set_between_p (rtx, rtx, rtx);
extern int commutative_operand_precedence (rtx);
extern int swap_commutative_operands_p (rtx, rtx);
extern int modified_between_p (rtx, rtx, rtx);
extern int no_labels_between_p (rtx, rtx);
extern int no_jumps_between_p (rtx, rtx);
extern int modified_in_p (rtx, rtx);
extern int insn_dependent_p (rtx, rtx);
extern int reg_set_p (rtx, rtx);
extern rtx single_set_2 (rtx, rtx);
extern int multiple_sets (rtx);
extern int set_noop_p (rtx);
extern int noop_move_p (rtx);
extern rtx find_last_value (rtx, rtx *, rtx, int);
extern int refers_to_regno_p (unsigned int, unsigned int, rtx, rtx *);

extern int reg_overlap_mentioned_p (rtx, rtx);
extern rtx set_of (rtx, rtx);
extern void note_stores (rtx, void (*) (rtx, rtx, void *), void *);


extern void note_uses (rtx *, void (*) (rtx *, void *), void *);


extern rtx reg_set_last (rtx, rtx);
extern int dead_or_set_p (rtx, rtx);
extern int dead_or_set_regno_p (rtx, unsigned int);
extern rtx find_reg_note (rtx, enum reg_note, rtx);
extern rtx find_regno_note (rtx, enum reg_note, unsigned int);

extern rtx find_reg_equal_equiv_note (rtx);
extern int find_reg_fusage (rtx, enum rtx_code, rtx);
extern int find_regno_fusage (rtx, enum rtx_code, unsigned int);

extern int pure_call_p (rtx);
extern void remove_note (rtx, rtx);
extern int side_effects_p (rtx);
extern int volatile_refs_p (rtx);
extern int volatile_insn_p (rtx);
extern int may_trap_p (rtx);
extern int inequality_comparisons_p (rtx);
extern rtx replace_rtx (rtx, rtx, rtx);
extern rtx replace_regs (rtx, rtx *, unsigned int, int);

extern int computed_jump_p (rtx);
typedef int (*rtx_function) (rtx *, void *);
extern int for_each_rtx (rtx *, rtx_function, void *);
extern rtx regno_use_in (unsigned int, rtx);
extern int auto_inc_p (rtx);
extern int in_expr_list_p (rtx, rtx);
extern void remove_node_from_expr_list (rtx, rtx *);
extern int insns_safe_to_move_p (rtx, rtx, rtx *);
extern int loc_mentioned_in_p (rtx *, rtx);
extern rtx find_first_parameter_load (rtx, rtx);



extern rtx find_use_as_address (rtx, rtx, long long);
void init_EXPR_INSN_LIST_cache (void);
void free_EXPR_LIST_list (rtx *);
void free_INSN_LIST_list (rtx *);
void free_EXPR_LIST_node (rtx);
void free_INSN_LIST_node (rtx);
rtx alloc_INSN_LIST (rtx, rtx);
rtx alloc_EXPR_LIST (int, rtx, rtx);







extern int max_parallel;


extern void free_reg_info (void);


extern int asm_noperands (rtx);
extern const char *decode_asm_operands (rtx, rtx *, rtx **, const char **, enum machine_mode *);



extern enum reg_class reg_preferred_class (int);
extern enum reg_class reg_alternate_class (int);

extern rtx get_first_nonparm_insn (void);

extern void split_all_insns (int);
extern void split_all_insns_noflow (void);


extern rtx const_int_rtx[64 * 2 + 1];





extern rtx const_true_rtx;

extern rtx const_tiny_rtx[3][(int) MAX_MACHINE_MODE];
# 1584 "rtl.h"
enum global_rtl_index
{
  GR_PC,
  GR_CC0,
  GR_STACK_POINTER,
  GR_FRAME_POINTER,
# 1599 "rtl.h"
  GR_HARD_FRAME_POINTER,





  GR_ARG_POINTER,


  GR_VIRTUAL_INCOMING_ARGS,
  GR_VIRTUAL_STACK_ARGS,
  GR_VIRTUAL_STACK_DYNAMIC,
  GR_VIRTUAL_OUTGOING_ARGS,
  GR_VIRTUAL_CFA,

  GR_MAX
};


extern rtx global_rtl[GR_MAX];
# 1632 "rtl.h"
extern rtx pic_offset_table_rtx;
extern rtx struct_value_rtx;
extern rtx struct_value_incoming_rtx;
extern rtx static_chain_rtx;
extern rtx static_chain_incoming_rtx;
extern rtx return_address_pointer_rtx;




# 1 "genrtl.h" 1





extern rtx gen_rtx_fmt_s (enum rtx_code, enum machine_mode mode, const char *arg0);

extern rtx gen_rtx_fmt_ee (enum rtx_code, enum machine_mode mode, rtx arg0, rtx arg1);

extern rtx gen_rtx_fmt_ue (enum rtx_code, enum machine_mode mode, rtx arg0, rtx arg1);

extern rtx gen_rtx_fmt_iss (enum rtx_code, enum machine_mode mode, int arg0, const char *arg1, const char *arg2);


extern rtx gen_rtx_fmt_is (enum rtx_code, enum machine_mode mode, int arg0, const char *arg1);

extern rtx gen_rtx_fmt_i (enum rtx_code, enum machine_mode mode, int arg0);

extern rtx gen_rtx_fmt_isE (enum rtx_code, enum machine_mode mode, int arg0, const char *arg1, rtvec arg2);


extern rtx gen_rtx_fmt_iE (enum rtx_code, enum machine_mode mode, int arg0, rtvec arg1);

extern rtx gen_rtx_fmt_Ess (enum rtx_code, enum machine_mode mode, rtvec arg0, const char *arg1, const char *arg2);


extern rtx gen_rtx_fmt_sEss (enum rtx_code, enum machine_mode mode, const char *arg0, rtvec arg1, const char *arg2, const char *arg3);


extern rtx gen_rtx_fmt_eE (enum rtx_code, enum machine_mode mode, rtx arg0, rtvec arg1);

extern rtx gen_rtx_fmt_E (enum rtx_code, enum machine_mode mode, rtvec arg0);

extern rtx gen_rtx_fmt_e (enum rtx_code, enum machine_mode mode, rtx arg0);

extern rtx gen_rtx_fmt_sse (enum rtx_code, enum machine_mode mode, const char *arg0, const char *arg1, rtx arg2);


extern rtx gen_rtx_fmt_ss (enum rtx_code, enum machine_mode mode, const char *arg0, const char *arg1);

extern rtx gen_rtx_fmt_sE (enum rtx_code, enum machine_mode mode, const char *arg0, rtvec arg1);

extern rtx gen_rtx_fmt_iuueiee (enum rtx_code, enum machine_mode mode, int arg0, rtx arg1, rtx arg2, rtx arg3, int arg4, rtx arg5, rtx arg6);



extern rtx gen_rtx_fmt_iuueiee0 (enum rtx_code, enum machine_mode mode, int arg0, rtx arg1, rtx arg2, rtx arg3, int arg4, rtx arg5, rtx arg6);



extern rtx gen_rtx_fmt_iuueieee (enum rtx_code, enum machine_mode mode, int arg0, rtx arg1, rtx arg2, rtx arg3, int arg4, rtx arg5, rtx arg6, rtx arg7);



extern rtx gen_rtx_fmt_iuu (enum rtx_code, enum machine_mode mode, int arg0, rtx arg1, rtx arg2);

extern rtx gen_rtx_fmt_iuu00iss (enum rtx_code, enum machine_mode mode, int arg0, rtx arg1, rtx arg2, int arg3, const char *arg4, const char *arg5);



extern rtx gen_rtx_fmt_ssiEEsi (enum rtx_code, enum machine_mode mode, const char *arg0, const char *arg1, int arg2, rtvec arg3, rtvec arg4, const char *arg5, int arg6);



extern rtx gen_rtx_fmt_Ei (enum rtx_code, enum machine_mode mode, rtvec arg0, int arg1);

extern rtx gen_rtx_fmt_eEee0 (enum rtx_code, enum machine_mode mode, rtx arg0, rtvec arg1, rtx arg2, rtx arg3);


extern rtx gen_rtx_fmt_eee (enum rtx_code, enum machine_mode mode, rtx arg0, rtx arg1, rtx arg2);

extern rtx gen_rtx_fmt_ (enum rtx_code, enum machine_mode mode);
extern rtx gen_rtx_fmt_w (enum rtx_code, enum machine_mode mode, long long arg0);

extern rtx gen_rtx_fmt_0www (enum rtx_code, enum machine_mode mode, long long arg0, long long arg1, long long arg2);



extern rtx gen_rtx_fmt_0 (enum rtx_code, enum machine_mode mode);
extern rtx gen_rtx_fmt_i0 (enum rtx_code, enum machine_mode mode, int arg0);

extern rtx gen_rtx_fmt_ei (enum rtx_code, enum machine_mode mode, rtx arg0, int arg1);

extern rtx gen_rtx_fmt_e0 (enum rtx_code, enum machine_mode mode, rtx arg0);

extern rtx gen_rtx_fmt_u00 (enum rtx_code, enum machine_mode mode, rtx arg0);

extern rtx gen_rtx_fmt_eit (enum rtx_code, enum machine_mode mode, rtx arg0, int arg1, union tree_node *arg2);


extern rtx gen_rtx_fmt_eeeee (enum rtx_code, enum machine_mode mode, rtx arg0, rtx arg1, rtx arg2, rtx arg3, rtx arg4);


extern rtx gen_rtx_fmt_Ee (enum rtx_code, enum machine_mode mode, rtvec arg0, rtx arg1);

extern rtx gen_rtx_fmt_uuEiiiiiibbii (enum rtx_code, enum machine_mode mode, rtx arg0, rtx arg1, rtvec arg2, int arg3, int arg4, int arg5, int arg6, int arg7, int arg8, struct bitmap_head_def *arg9, struct bitmap_head_def *arg10, int arg11, int arg12);






extern rtx gen_rtx_fmt_iiiiiiiitt (enum rtx_code, enum machine_mode mode, int arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7, union tree_node *arg8, union tree_node *arg9);





extern rtx gen_rtx_fmt_eti (enum rtx_code, enum machine_mode mode, rtx arg0, union tree_node *arg1, int arg2);


extern rtx gen_rtx_fmt_bi (enum rtx_code, enum machine_mode mode, struct bitmap_head_def *arg0, int arg1);


extern rtx gen_rtx_fmt_uuuu (enum rtx_code, enum machine_mode mode, rtx arg0, rtx arg1, rtx arg2, rtx arg3);
# 1643 "rtl.h" 2







extern rtx gen_rtx_CONST_DOUBLE (enum machine_mode, long long, long long);

extern rtx gen_rtx_CONST_INT (enum machine_mode, long long);
extern rtx gen_raw_REG (enum machine_mode, int);
extern rtx gen_rtx_REG (enum machine_mode, int);
extern rtx gen_rtx_SUBREG (enum machine_mode, rtx, int);
extern rtx gen_rtx_MEM (enum machine_mode, rtx);

extern rtx gen_lowpart_SUBREG (enum machine_mode, rtx);
# 1728 "rtl.h"
extern rtx find_next_ref (rtx, rtx);

extern rtx output_constant_def (tree, int);
extern rtx immed_real_const (tree);
# 1741 "rtl.h"
extern int flow2_completed;




extern int reload_completed;




extern int reload_in_progress;







extern int cse_not_expected;



extern int no_new_pseudos;





extern int rtx_to_tree_code (enum rtx_code);


struct obstack;
extern void gcc_obstack_init (struct obstack *);


struct cse_basic_block_data;
# 1787 "rtl.h"
extern int rtx_cost (rtx, enum rtx_code);
extern int address_cost (rtx, enum machine_mode);
extern void delete_trivially_dead_insns (rtx, int, int);

extern int cse_main (rtx, int, int, FILE *);

extern void cse_end_of_basic_block (rtx, struct cse_basic_block_data *, int, int, int);




extern int comparison_dominates_p (enum rtx_code, enum rtx_code);
extern int condjump_p (rtx);
extern int any_condjump_p (rtx);
extern int any_uncondjump_p (rtx);
extern int safe_to_remove_jump_p (rtx);
extern rtx pc_set (rtx);
extern rtx condjump_label (rtx);
extern int simplejump_p (rtx);
extern int returnjump_p (rtx);
extern int onlyjump_p (rtx);
extern int only_sets_cc0_p (rtx);
extern int sets_cc0_p (rtx);
extern int invert_jump_1 (rtx, rtx);
extern int invert_jump (rtx, rtx, int);
extern int rtx_renumbered_equal_p (rtx, rtx);
extern int true_regnum (rtx);
extern int redirect_jump_1 (rtx, rtx);
extern int redirect_jump (rtx, rtx, int);
extern void rebuild_jump_labels (rtx);
extern enum rtx_code reversed_comparison_code (rtx, rtx);
extern enum rtx_code reversed_comparison_code_parts (enum rtx_code, rtx, rtx, rtx);

extern void delete_for_peephole (rtx, rtx);
extern int condjump_in_parallel_p (rtx);
extern void never_reached_warning (rtx, rtx);
extern void purge_line_number_notes (rtx);
extern void copy_loop_headers (rtx);


extern int max_reg_num (void);
extern int max_label_num (void);
extern int get_first_label_num (void);
extern void delete_insns_since (rtx);
extern void mark_reg_pointer (rtx, int);
extern void mark_user_reg (rtx);
extern void reset_used_flags (rtx);
extern void reorder_insns (rtx, rtx, rtx);
extern void reorder_insns_nobb (rtx, rtx, rtx);
extern int get_max_uid (void);
extern int in_sequence_p (void);
extern void force_next_line_note (void);
extern void clear_emit_caches (void);
extern void init_emit (void);
extern void init_emit_once (int);
extern void push_topmost_sequence (void);
extern void pop_topmost_sequence (void);
extern int subreg_realpart_p (rtx);
extern void reverse_comparison (rtx);
extern void set_new_first_and_last_insn (rtx, rtx);
extern void set_new_first_and_last_label_num (int, int);
extern void set_new_last_label_num (int);
extern void unshare_all_rtl_again (rtx);
extern void set_last_insn (rtx);
extern void link_cc0_insns (rtx);
extern void add_insn (rtx);
extern void add_insn_before (rtx, rtx);
extern void add_insn_after (rtx, rtx);
extern void remove_insn (rtx);
extern void reorder_insns_with_line_notes (rtx, rtx, rtx);
extern void emit_insn_after_with_line_notes (rtx, rtx, rtx);
extern enum rtx_code classify_insn (rtx);
extern rtx emit (rtx);



int force_line_numbers (void);
void restore_line_number_status (int old_value);
extern void renumber_insns (FILE *);
extern void remove_unnecessary_notes (void);
extern rtx delete_insn (rtx);
extern void delete_insn_chain (rtx, rtx);


extern int combine_instructions (rtx, unsigned int);
extern unsigned int extended_count (rtx, enum machine_mode, int);
extern rtx remove_death (unsigned int, rtx);

extern void dump_combine_stats (FILE *);
extern void dump_combine_total_stats (FILE *);




extern void schedule_insns (FILE *);
extern void schedule_ebbs (FILE *);

extern void fix_sched_param (const char *, const char *);


extern const char *print_rtx_head;
extern void debug_rtx (rtx);
extern void debug_rtx_list (rtx, int);
extern void debug_rtx_range (rtx, rtx);
extern rtx debug_rtx_find (rtx, int);

extern void print_mem_expr (FILE *, tree);
extern void print_rtl (FILE *, rtx);
extern void print_simple_rtl (FILE *, rtx);
extern int print_rtl_single (FILE *, rtx);
extern void print_inline_rtx (FILE *, rtx, int);



extern void init_loop (void);
extern rtx libcall_other_reg (rtx, rtx);

extern void loop_optimize (rtx, FILE *, int);

extern void record_excess_regs (rtx, rtx, rtx *);


extern void reposition_prologue_and_epilogue_notes (rtx);
extern void thread_prologue_and_epilogue_insns (rtx);
extern int prologue_epilogue_contains (rtx);
extern int sibcall_epilogue_contains (rtx);
extern void preserve_rtl_expr_result (rtx);
extern void mark_temp_addr_taken (rtx);
extern void update_temp_slot_address (rtx, rtx);
extern void purge_addressof (rtx);
extern void purge_hard_subreg_sets (rtx);


extern void set_file_and_line_for_stmt (const char *, int);
extern void expand_null_return (void);
extern void emit_jump (rtx);
extern int preserve_subexpressions_p (void);


extern void move_by_pieces (rtx, rtx, unsigned long long, unsigned int);




extern void recompute_reg_usage (rtx, int);
extern int initialize_uninitialized_subregs (void);

extern void print_rtl_with_bb (FILE *, rtx);
extern void dump_flow_info (FILE *);



extern void init_expmed (void);
extern void expand_inc (rtx, rtx);
extern void expand_dec (rtx, rtx);
extern rtx expand_mult_highpart (enum machine_mode, rtx, unsigned long long, rtx, int, int);





extern int gcse_main (rtx, FILE *);



extern void mark_elimination (int, int);

extern int global_alloc (FILE *);
extern void dump_global_regs (FILE *);




extern void build_insn_chain (rtx);


extern int reg_classes_intersect_p (enum reg_class, enum reg_class);
extern int reg_class_subset_p (enum reg_class, enum reg_class);
extern void globalize_reg (int);
extern void init_regs (void);
extern void init_reg_sets (void);
extern void regset_release_memory (void);
extern void regclass_init (void);
extern void regclass (rtx, int, FILE *);
extern void reg_scan (rtx, unsigned int, int);
extern void reg_scan_update (rtx, rtx, unsigned int);
extern void fix_register (const char *, int, int);

extern void delete_null_pointer_checks (rtx);



extern void regmove_optimize (rtx, int, FILE *);

extern void combine_stack_adjustments (void);



extern void dbr_schedule (rtx, FILE *);




extern void dump_local_alloc (FILE *);

extern int local_alloc (void);
extern int function_invariant_p (rtx);


extern void init_branch_prob (const char *);
extern void branch_prob (void);
extern void end_branch_prob (void);
extern void output_func_start_profiler (void);



extern void reg_to_stack (rtx, FILE *);



extern int add_double (unsigned long long, long long, unsigned long long, long long, unsigned long long *, long long *);



extern int neg_double (unsigned long long, long long, unsigned long long *, long long *);


extern int mul_double (unsigned long long, long long, unsigned long long, long long, unsigned long long *, long long *);




extern void lshift_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *, int);



extern void rshift_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *, int);



extern void lrotate_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *);



extern void rrotate_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *);





enum libcall_type
{
  LCT_NORMAL = 0,
  LCT_CONST = 1,
  LCT_PURE = 2,
  LCT_CONST_MAKE_BLOCK = 3,
  LCT_PURE_MAKE_BLOCK = 4,
  LCT_NORETURN = 5,
  LCT_THROW = 6,
  LCT_ALWAYS_RETURN = 7,
  LCT_RETURNS_TWICE = 8
};

extern void emit_library_call (rtx, enum libcall_type, enum machine_mode, int, ...);


extern rtx emit_library_call_value (rtx, rtx, enum libcall_type, enum machine_mode, int, ...);




extern int set_dominates_use (int, int, int, rtx, rtx);


extern int in_data_section (void);
extern void init_varasm_once (void);


extern void init_rtl (void);
extern void traverse_md_constants (int (*) (void **, void *), void *);

struct md_constant { char *name, *value; };


extern int read_skip_spaces (FILE *);
extern rtx read_rtx (FILE *);


extern const char *read_rtx_filename;
extern int read_rtx_lineno;







extern void fancy_abort (const char *, int, const char *)
    __attribute__ ((__noreturn__));



extern void clear_reg_alias_info (rtx);
extern rtx canon_rtx (rtx);
extern int true_dependence (rtx, enum machine_mode, rtx, int (*)(rtx, int));

extern rtx get_addr (rtx);
extern int canon_true_dependence (rtx, enum machine_mode, rtx, rtx, int (*)(rtx, int));

extern int read_dependence (rtx, rtx);
extern int anti_dependence (rtx, rtx);
extern int output_dependence (rtx, rtx);
extern void mark_constant_function (void);
extern void init_alias_once (void);
extern void init_alias_analysis (void);
extern void end_alias_analysis (void);
extern rtx addr_side_effect_eval (rtx, int, int);


typedef enum {
  sibcall_use_normal = 1,
  sibcall_use_tail_recursion,
  sibcall_use_sibcall
} sibcall_use_t;

extern void optimize_sibling_and_tail_recursive_calls (void);
extern void replace_call_placeholder (rtx, sibcall_use_t);


extern int stack_regs_mentioned (rtx insn);



extern rtx stack_limit_rtx;


extern void regrename_optimize (void);
extern void copyprop_hardreg_forward (void);


extern void if_convert (int);


extern void invert_br_probabilities (rtx);
extern _Bool expensive_function_p (int);
# 35 "c-typeck.c" 2
# 1 "tree.h" 1
# 22 "tree.h"
# 1 "machmode.h" 1
# 23 "tree.h" 2
# 1 "version.h" 1


extern const char *const version_string;
# 24 "tree.h" 2





enum tree_code {
# 1 "tree.def" 1
# 46 "tree.def"
ERROR_MARK,





IDENTIFIER_NODE,






TREE_LIST,


TREE_VEC,
# 84 "tree.def"
BLOCK,
# 129 "tree.def"
VOID_TYPE,
# 139 "tree.def"
INTEGER_TYPE,



REAL_TYPE,



COMPLEX_TYPE,



VECTOR_TYPE,
# 164 "tree.def"
ENUMERAL_TYPE,



BOOLEAN_TYPE,



CHAR_TYPE,



POINTER_TYPE,





OFFSET_TYPE,



REFERENCE_TYPE,






METHOD_TYPE,


FILE_TYPE,
# 209 "tree.def"
ARRAY_TYPE,





SET_TYPE,
# 225 "tree.def"
RECORD_TYPE,





UNION_TYPE,





QUAL_UNION_TYPE,







FUNCTION_TYPE,





LANG_TYPE,
# 262 "tree.def"
INTEGER_CST,


REAL_CST,




COMPLEX_CST,


VECTOR_CST,



STRING_CST,
# 336 "tree.def"
FUNCTION_DECL,
LABEL_DECL,
CONST_DECL,
TYPE_DECL,
VAR_DECL,
PARM_DECL,
RESULT_DECL,
FIELD_DECL,



NAMESPACE_DECL,






COMPONENT_REF,
# 363 "tree.def"
BIT_FIELD_REF,


INDIRECT_REF,


BUFFER_REF,



ARRAY_REF,




ARRAY_RANGE_REF,






VTABLE_REF,
# 409 "tree.def"
CONSTRUCTOR,
# 419 "tree.def"
COMPOUND_EXPR,


MODIFY_EXPR,



INIT_EXPR,






TARGET_EXPR,
# 443 "tree.def"
COND_EXPR,
# 466 "tree.def"
BIND_EXPR,




CALL_EXPR,




METHOD_CALL_EXPR,
# 490 "tree.def"
WITH_CLEANUP_EXPR,
# 507 "tree.def"
CLEANUP_POINT_EXPR,
# 556 "tree.def"
PLACEHOLDER_EXPR,





WITH_RECORD_EXPR,


PLUS_EXPR,
MINUS_EXPR,
MULT_EXPR,


TRUNC_DIV_EXPR,


CEIL_DIV_EXPR,


FLOOR_DIV_EXPR,


ROUND_DIV_EXPR,


TRUNC_MOD_EXPR,
CEIL_MOD_EXPR,
FLOOR_MOD_EXPR,
ROUND_MOD_EXPR,


RDIV_EXPR,



EXACT_DIV_EXPR,






FIX_TRUNC_EXPR,
FIX_CEIL_EXPR,
FIX_FLOOR_EXPR,
FIX_ROUND_EXPR,


FLOAT_EXPR,


NEGATE_EXPR,

MIN_EXPR,
MAX_EXPR,





ABS_EXPR,

FFS_EXPR,
# 628 "tree.def"
LSHIFT_EXPR,
RSHIFT_EXPR,
LROTATE_EXPR,
RROTATE_EXPR,


BIT_IOR_EXPR,
BIT_XOR_EXPR,
BIT_AND_EXPR,
BIT_ANDTC_EXPR,
BIT_NOT_EXPR,
# 649 "tree.def"
TRUTH_ANDIF_EXPR,
TRUTH_ORIF_EXPR,
TRUTH_AND_EXPR,
TRUTH_OR_EXPR,
TRUTH_XOR_EXPR,
TRUTH_NOT_EXPR,







LT_EXPR,
LE_EXPR,
GT_EXPR,
GE_EXPR,
EQ_EXPR,
NE_EXPR,


UNORDERED_EXPR,
ORDERED_EXPR,


UNLT_EXPR,
UNLE_EXPR,
UNGT_EXPR,
UNGE_EXPR,
UNEQ_EXPR,


IN_EXPR,
SET_LE_EXPR,
CARD_EXPR,
RANGE_EXPR,




CONVERT_EXPR,


NOP_EXPR,


NON_LVALUE_EXPR,
# 707 "tree.def"
VIEW_CONVERT_EXPR,





SAVE_EXPR,
# 722 "tree.def"
UNSAVE_EXPR,
# 731 "tree.def"
RTL_EXPR,



ADDR_EXPR,


REFERENCE_EXPR,



ENTRY_VALUE_EXPR,



FDESC_EXPR,



COMPLEX_EXPR,


CONJ_EXPR,



REALPART_EXPR,
IMAGPART_EXPR,




PREDECREMENT_EXPR,
PREINCREMENT_EXPR,
POSTDECREMENT_EXPR,
POSTINCREMENT_EXPR,


VA_ARG_EXPR,






TRY_CATCH_EXPR,
# 790 "tree.def"
TRY_FINALLY_EXPR,






GOTO_SUBROUTINE_EXPR,







LABEL_EXPR,



GOTO_EXPR,






RETURN_EXPR,



EXIT_EXPR,




LOOP_EXPR,




LABELED_BLOCK_EXPR,




EXIT_BLOCK_EXPR,
# 844 "tree.def"
EXPR_WITH_FILE_LOCATION,





SWITCH_EXPR,


EXC_PTR_EXPR,
# 31 "tree.h" 2

  LAST_AND_UNUSED_TREE_CODE

};
# 49 "tree.h"
extern char tree_code_type[256];
# 60 "tree.h"
extern int tree_code_length[256];




extern const char *tree_code_name[256];



enum built_in_class
{
  NOT_BUILT_IN = 0,
  BUILT_IN_FRONTEND,
  BUILT_IN_MD,
  BUILT_IN_NORMAL
};


extern const char *const built_in_class_names[4];





enum built_in_function
{
# 1 "builtins.def" 1
# 145 "builtins.def"
BUILT_IN_ALLOCA,




BUILT_IN_ABS,


BUILT_IN_LABS,



BUILT_IN_FABS,


BUILT_IN_FABSF,


BUILT_IN_FABSL,



BUILT_IN_LLABS,


BUILT_IN_IMAXABS,


BUILT_IN_CONJ,


BUILT_IN_CONJF,


BUILT_IN_CONJL,


BUILT_IN_CREAL,


BUILT_IN_CREALF,


BUILT_IN_CREALL,


BUILT_IN_CIMAG,


BUILT_IN_CIMAGF,


BUILT_IN_CIMAGL,



BUILT_IN_DIV,
BUILT_IN_LDIV,
BUILT_IN_FFLOOR,
BUILT_IN_FCEIL,
BUILT_IN_FMOD,
BUILT_IN_FREM,




BUILT_IN_BZERO,





BUILT_IN_BCMP,






BUILT_IN_FFS,


BUILT_IN_INDEX,


BUILT_IN_RINDEX,



BUILT_IN_MEMCPY,


BUILT_IN_MEMCMP,


BUILT_IN_MEMSET,



BUILT_IN_STRCAT,


BUILT_IN_STRNCAT,


BUILT_IN_STRCPY,


BUILT_IN_STRNCPY,


BUILT_IN_STRCMP,


BUILT_IN_STRNCMP,


BUILT_IN_STRLEN,


BUILT_IN_STRSTR,


BUILT_IN_STRPBRK,


BUILT_IN_STRSPN,


BUILT_IN_STRCSPN,


BUILT_IN_STRCHR,


BUILT_IN_STRRCHR,



BUILT_IN_SQRT,


BUILT_IN_SIN,


BUILT_IN_COS,


BUILT_IN_SQRTF,


BUILT_IN_SINF,


BUILT_IN_COSF,


BUILT_IN_SQRTL,


BUILT_IN_SINL,


BUILT_IN_COSL,



BUILT_IN_GETEXP,
BUILT_IN_GETMAN,

BUILT_IN_SAVEREGS,


BUILT_IN_CLASSIFY_TYPE,


BUILT_IN_NEXT_ARG,


BUILT_IN_ARGS_INFO,


BUILT_IN_CONSTANT_P,


BUILT_IN_FRAME_ADDRESS,


BUILT_IN_RETURN_ADDRESS,


BUILT_IN_AGGREGATE_INCOMING_ADDRESS,


BUILT_IN_APPLY_ARGS,


BUILT_IN_APPLY,


BUILT_IN_RETURN,


BUILT_IN_SETJMP,


BUILT_IN_LONGJMP,


BUILT_IN_TRAP,


BUILT_IN_PREFETCH,




BUILT_IN_PUTCHAR,


BUILT_IN_PUTS,


BUILT_IN_PRINTF,


BUILT_IN_FPUTC,






BUILT_IN_FPUTS,





BUILT_IN_FWRITE,


BUILT_IN_FPRINTF,





BUILT_IN_PUTCHAR_UNLOCKED,


BUILT_IN_PUTS_UNLOCKED,


BUILT_IN_PRINTF_UNLOCKED,


BUILT_IN_FPUTC_UNLOCKED,






BUILT_IN_FPUTS_UNLOCKED,





BUILT_IN_FWRITE_UNLOCKED,


BUILT_IN_FPRINTF_UNLOCKED,




BUILT_IN_ISGREATER,


BUILT_IN_ISGREATEREQUAL,


BUILT_IN_ISLESS,


BUILT_IN_ISLESSEQUAL,


BUILT_IN_ISLESSGREATER,


BUILT_IN_ISUNORDERED,




BUILT_IN_UNWIND_INIT,


BUILT_IN_DWARF_CFA,


BUILT_IN_DWARF_FP_REGNUM,


BUILT_IN_INIT_DWARF_REG_SIZES,


BUILT_IN_FROB_RETURN_ADDR,


BUILT_IN_EXTRACT_RETURN_ADDR,


BUILT_IN_EH_RETURN,


BUILT_IN_EH_RETURN_DATA_REGNO,



BUILT_IN_VARARGS_START,


BUILT_IN_STDARG_START,


BUILT_IN_VA_END,


BUILT_IN_VA_COPY,


BUILT_IN_EXPECT,




BUILT_IN_NEW,
BUILT_IN_VEC_NEW,
BUILT_IN_DELETE,
BUILT_IN_VEC_DELETE,
# 87 "tree.h" 2


  END_BUILTINS
};



extern const char *const built_in_names[(int) END_BUILTINS];


extern tree built_in_decls[(int) END_BUILTINS];
# 121 "tree.h"
struct tree_common
{
  tree chain;
  tree type;

  enum tree_code code : 8;

  unsigned side_effects_flag : 1;
  unsigned constant_flag : 1;
  unsigned addressable_flag : 1;
  unsigned volatile_flag : 1;
  unsigned readonly_flag : 1;
  unsigned unsigned_flag : 1;
  unsigned asm_written_flag: 1;
  unsigned unused_0 : 1;

  unsigned used_flag : 1;
  unsigned nothrow_flag : 1;
  unsigned static_flag : 1;
  unsigned public_flag : 1;
  unsigned private_flag : 1;
  unsigned protected_flag : 1;
  unsigned bounded_flag : 1;
  unsigned deprecated_flag : 1;

  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned unused_1 : 1;
};
# 332 "tree.h"
# 1 "tree-check.h" 1
# 333 "tree.h" 2
# 705 "tree.h"
struct tree_int_cst
{
  struct tree_common common;
  rtx rtl;




  struct {
    unsigned long long low;
    long long high;
  } int_cst;
};
# 732 "tree.h"
# 1 "real.h" 1
# 107 "real.h"
typedef struct {
  long long r[(19 + sizeof (long long))/(sizeof (long long))];
} realvaluetype;
# 131 "real.h"
extern unsigned int significand_size (enum machine_mode);
# 144 "real.h"
extern void earith (realvaluetype *, int, realvaluetype *, realvaluetype *);

extern realvaluetype etrunci (realvaluetype);
extern realvaluetype etruncui (realvaluetype);
extern realvaluetype ereal_negate (realvaluetype);
extern long long efixi (realvaluetype);
extern unsigned long long efixui (realvaluetype);
extern void ereal_from_int (realvaluetype *, long long, long long, enum machine_mode);


extern void ereal_from_uint (realvaluetype *, unsigned long long, unsigned long long, enum machine_mode);



extern void ereal_to_int (long long *, long long *, realvaluetype);

extern realvaluetype ereal_ldexp (realvaluetype, int);

extern void etartdouble (realvaluetype, long *);
extern void etarldouble (realvaluetype, long *);
extern void etardouble (realvaluetype, long *);
extern long etarsingle (realvaluetype);
extern void ereal_to_decimal (realvaluetype, char *);
extern int ereal_cmp (realvaluetype, realvaluetype);
extern int ereal_isneg (realvaluetype);
extern realvaluetype ereal_unto_float (long);
extern realvaluetype ereal_unto_double (long *);
extern realvaluetype ereal_from_float (long long);
extern realvaluetype ereal_from_double (long long *);
# 406 "real.h"
extern realvaluetype real_value_truncate (enum machine_mode, realvaluetype);
# 437 "real.h"
extern realvaluetype dconst0;
extern realvaluetype dconst1;
extern realvaluetype dconst2;
extern realvaluetype dconstm1;




union real_extract
{
  realvaluetype d;
  long long i[sizeof (realvaluetype) / sizeof (long long)];
};




union tree_node;
realvaluetype real_value_from_int_cst (union tree_node *, union tree_node *);
# 466 "real.h"
extern struct rtx_def *immed_real_const_1 (realvaluetype, enum machine_mode);
# 478 "real.h"
extern int exact_real_inverse (enum machine_mode, realvaluetype *);
extern int target_isnan (realvaluetype);
extern int target_isinf (realvaluetype);
extern int target_negative (realvaluetype);
extern void debug_real (realvaluetype);
extern realvaluetype ereal_atof (const char *, enum machine_mode);
# 733 "tree.h" 2

struct tree_real_cst
{
  struct tree_common common;
  rtx rtl;
  realvaluetype real_cst;
};





struct tree_string
{
  struct tree_common common;
  rtx rtl;
  int length;
  const char *pointer;
};





struct tree_complex
{
  struct tree_common common;
  rtx rtl;
  tree real;
  tree imag;
};




struct tree_vector
{
  struct tree_common common;
  rtx rtl;
  tree elements;
};

# 1 "hashtable.h" 1
# 21 "hashtable.h"
# 1 "obstack.h" 1
# 161 "obstack.h"
struct _obstack_chunk
{
  char *limit;
  struct _obstack_chunk *prev;
  char contents[4];
};

struct obstack
{
  long chunk_size;
  struct _obstack_chunk *chunk;
  char *object_base;
  char *next_free;
  char *chunk_limit;
  long int temp;
  int alignment_mask;




  struct _obstack_chunk *(*chunkfun) (void *, long);
  void (*freefun) (void *, struct _obstack_chunk *);
  void *extra_arg;





  unsigned use_extra_arg:1;
  unsigned maybe_empty_object:1;



  unsigned alloc_failed:1;


};




extern void _obstack_newchunk (struct obstack *, int);
extern void _obstack_free (struct obstack *, void *);
extern int _obstack_begin (struct obstack *, int, int,
                            void *(*) (long), void (*) (void *));
extern int _obstack_begin_1 (struct obstack *, int, int,
                             void *(*) (void *, long),
                             void (*) (void *, void *), void *);
extern int _obstack_memory_used (struct obstack *);
# 223 "obstack.h"
void obstack_init (struct obstack *obstack);

void * obstack_alloc (struct obstack *obstack, int size);

void * obstack_copy (struct obstack *obstack, void *address, int size);
void * obstack_copy0 (struct obstack *obstack, void *address, int size);

void obstack_free (struct obstack *obstack, void *block);

void obstack_blank (struct obstack *obstack, int size);

void obstack_grow (struct obstack *obstack, void *data, int size);
void obstack_grow0 (struct obstack *obstack, void *data, int size);

void obstack_1grow (struct obstack *obstack, int data_char);
void obstack_ptr_grow (struct obstack *obstack, void *data);
void obstack_int_grow (struct obstack *obstack, int data);

void * obstack_finish (struct obstack *obstack);

int obstack_object_size (struct obstack *obstack);

int obstack_room (struct obstack *obstack);
void obstack_make_room (struct obstack *obstack, int size);
void obstack_1grow_fast (struct obstack *obstack, int data_char);
void obstack_ptr_grow_fast (struct obstack *obstack, void *data);
void obstack_int_grow_fast (struct obstack *obstack, int data);
void obstack_blank_fast (struct obstack *obstack, int size);

void * obstack_base (struct obstack *obstack);
void * obstack_next_free (struct obstack *obstack);
int obstack_alignment_mask (struct obstack *obstack);
int obstack_chunk_size (struct obstack *obstack);
int obstack_memory_used (struct obstack *obstack);
# 267 "obstack.h"
extern void (*obstack_alloc_failed_handler) (void);





extern int obstack_exit_failure;
# 22 "hashtable.h" 2



typedef struct ht_identifier ht_identifier;
struct ht_identifier
{
  unsigned int len;
  const unsigned char *str;
};
# 39 "hashtable.h"
struct cpp_reader;
typedef struct ht hash_table;
typedef struct ht_identifier *hashnode;

enum ht_lookup_option {HT_NO_INSERT = 0, HT_ALLOC, HT_ALLOCED};


struct ht
{

  struct obstack stack;

  hashnode *entries;

  hashnode (*alloc_node) (hash_table *);

  unsigned int nslots;
  unsigned int nelements;


  struct cpp_reader *pfile;


  unsigned int searches;
  unsigned int collisions;
};

extern void gcc_obstack_init (struct obstack *);


extern hash_table *ht_create (unsigned int order);


extern void ht_destroy (hash_table *);

extern hashnode ht_lookup (hash_table *, const unsigned char *, unsigned int, enum ht_lookup_option);





typedef int (*ht_cb) (struct cpp_reader *, hashnode, const void *);
extern void ht_forall (hash_table *, ht_cb, const void *);


extern void ht_dump_statistics (hash_table *);



extern double approx_sqrt (double);
# 776 "tree.h" 2
# 791 "tree.h"
struct tree_identifier
{
  struct tree_common common;
  struct ht_identifier id;
};





struct tree_list
{
  struct tree_common common;
  tree purpose;
  tree value;
};







struct tree_vec
{
  struct tree_common common;
  int length;
  tree a[1];
};
# 879 "tree.h"
struct tree_exp
{
  struct tree_common common;
  int complexity;
  tree operands[1];
};
# 931 "tree.h"
struct tree_block
{
  struct tree_common common;

  unsigned handler_block_flag : 1;
  unsigned abstract_flag : 1;
  unsigned block_num : 30;

  tree vars;
  tree subblocks;
  tree supercontext;
  tree abstract_origin;
  tree fragment_origin;
  tree fragment_chain;
};
# 1208 "tree.h"
struct tree_type
{
  struct tree_common common;
  tree values;
  tree size;
  tree size_unit;
  tree attributes;
  unsigned int uid;

  unsigned int precision : 9;
  enum machine_mode mode : 7;

  unsigned string_flag : 1;
  unsigned no_force_blk_flag : 1;
  unsigned needs_constructing_flag : 1;
  unsigned transparent_union_flag : 1;
  unsigned packed_flag : 1;
  unsigned restrict_flag : 1;
  unsigned pointer_depth : 2;

  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned user_align : 1;

  unsigned int align;
  tree pointer_to;
  tree reference_to;
  union {int address; char *pointer; } symtab;
  tree name;
  tree minval;
  tree maxval;
  tree next_variant;
  tree main_variant;
  tree binfo;
  tree context;
  long long alias_set;

  struct lang_type *lang_specific;
};
# 1754 "tree.h"
struct function;

struct tree_decl
{
  struct tree_common common;
  const char *filename;
  int linenum;
  unsigned int uid;
  tree size;
  enum machine_mode mode : 8;

  unsigned external_flag : 1;
  unsigned nonlocal_flag : 1;
  unsigned regdecl_flag : 1;
  unsigned inline_flag : 1;
  unsigned bit_field_flag : 1;
  unsigned virtual_flag : 1;
  unsigned ignored_flag : 1;
  unsigned abstract_flag : 1;

  unsigned in_system_header_flag : 1;
  unsigned common_flag : 1;
  unsigned defer_output : 1;
  unsigned transparent_union : 1;
  unsigned static_ctor_flag : 1;
  unsigned static_dtor_flag : 1;
  unsigned artificial_flag : 1;
  unsigned weak_flag : 1;

  unsigned non_addr_const_p : 1;
  unsigned no_instrument_function_entry_exit : 1;
  unsigned comdat_flag : 1;
  unsigned malloc_flag : 1;
  unsigned no_limit_stack : 1;
  enum built_in_class built_in_class : 2;
  unsigned pure_flag : 1;

  unsigned pointer_depth : 2;
  unsigned non_addressable : 1;
  unsigned user_align : 1;
  unsigned uninlinable : 1;


  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned lang_flag_7 : 1;

  union {


    enum built_in_function f;


    long long i;


    struct {unsigned int align : 24; unsigned int off_align : 8;} a;
  } u1;

  tree size_unit;
  tree name;
  tree context;
  tree arguments;
  tree result;
  tree initial;
  tree abstract_origin;
  tree assembler_name;
  tree section_name;
  tree attributes;
  rtx rtl;
  rtx live_range_rtl;






  union {
    struct function *f;
    rtx r;
    tree t;
    int i;
  } u2;


  tree saved_tree;



  tree inlined_fns;

  tree vindex;
  long long pointer_alias_set;

  struct lang_decl *lang_specific;
};





union tree_node
{
  struct tree_common common;
  struct tree_int_cst int_cst;
  struct tree_real_cst real_cst;
  struct tree_vector vector;
  struct tree_string string;
  struct tree_complex complex;
  struct tree_identifier identifier;
  struct tree_decl decl;
  struct tree_type type;
  struct tree_list list;
  struct tree_vec vec;
  struct tree_exp exp;
  struct tree_block block;
 };



enum tree_index
{
  TI_ERROR_MARK,
  TI_INTQI_TYPE,
  TI_INTHI_TYPE,
  TI_INTSI_TYPE,
  TI_INTDI_TYPE,
  TI_INTTI_TYPE,

  TI_UINTQI_TYPE,
  TI_UINTHI_TYPE,
  TI_UINTSI_TYPE,
  TI_UINTDI_TYPE,
  TI_UINTTI_TYPE,

  TI_INTEGER_ZERO,
  TI_INTEGER_ONE,
  TI_INTEGER_MINUS_ONE,
  TI_NULL_POINTER,

  TI_SIZE_ZERO,
  TI_SIZE_ONE,

  TI_BITSIZE_ZERO,
  TI_BITSIZE_ONE,
  TI_BITSIZE_UNIT,

  TI_COMPLEX_INTEGER_TYPE,
  TI_COMPLEX_FLOAT_TYPE,
  TI_COMPLEX_DOUBLE_TYPE,
  TI_COMPLEX_LONG_DOUBLE_TYPE,

  TI_FLOAT_TYPE,
  TI_DOUBLE_TYPE,
  TI_LONG_DOUBLE_TYPE,

  TI_VOID_TYPE,
  TI_PTR_TYPE,
  TI_CONST_PTR_TYPE,
  TI_PTRDIFF_TYPE,
  TI_VA_LIST_TYPE,

  TI_VOID_LIST_NODE,

  TI_UV4SF_TYPE,
  TI_UV4SI_TYPE,
  TI_UV8HI_TYPE,
  TI_UV8QI_TYPE,
  TI_UV4HI_TYPE,
  TI_UV2SI_TYPE,
  TI_UV2SF_TYPE,
  TI_UV16QI_TYPE,

  TI_V4SF_TYPE,
  TI_V16SF_TYPE,
  TI_V4SI_TYPE,
  TI_V8HI_TYPE,
  TI_V8QI_TYPE,
  TI_V4HI_TYPE,
  TI_V2SI_TYPE,
  TI_V2SF_TYPE,
  TI_V16QI_TYPE,

  TI_MAIN_IDENTIFIER,

  TI_MAX
};

extern tree global_trees[TI_MAX];
# 2020 "tree.h"
enum integer_type_kind
{
  itk_char,
  itk_signed_char,
  itk_unsigned_char,
  itk_short,
  itk_unsigned_short,
  itk_int,
  itk_unsigned_int,
  itk_long,
  itk_unsigned_long,
  itk_long_long,
  itk_unsigned_long_long,
  itk_none
};

typedef enum integer_type_kind integer_type_kind;



extern tree integer_types[itk_none];
# 2059 "tree.h"
extern double approx_sqrt (double);

extern char *permalloc (int);
extern char *expralloc (int);




extern size_t tree_size (tree);





extern tree make_node (enum tree_code);
extern tree make_lang_type (enum tree_code);
extern tree (*make_lang_type_fn) (enum tree_code);





extern tree copy_node (tree);



extern tree copy_list (tree);



extern tree make_tree_vec (int);




extern tree get_identifier (const char *);




extern tree get_identifier_with_length (const char *, unsigned int);





extern tree maybe_get_identifier (const char *);






extern tree build (enum tree_code, tree, ...);
extern tree build_nt (enum tree_code, ...);

extern tree build_int_2_wide (unsigned long long, long long);
extern tree build_vector (tree, tree);
extern tree build_real (tree, realvaluetype);
extern tree build_real_from_int_cst (tree, tree);
extern tree build_complex (tree, tree, tree);
extern tree build_string (int, const char *);
extern tree build1 (enum tree_code, tree, tree);
extern tree build_tree_list (tree, tree);
extern tree build_decl (enum tree_code, tree, tree);
extern tree build_block (tree, tree, tree, tree, tree);
extern tree build_expr_wfl (tree, const char *, int, int);



extern tree make_signed_type (int);
extern tree make_unsigned_type (int);
extern void initialize_sizetypes (void);
extern void set_sizetype (tree);
extern tree signed_or_unsigned_type (int, tree);
extern void fixup_unsigned_type (tree);
extern tree build_pointer_type (tree);
extern tree build_reference_type (tree);
extern tree build_type_no_quals (tree);
extern tree build_index_type (tree);
extern tree build_index_2_type (tree, tree);
extern tree build_array_type (tree, tree);
extern tree build_function_type (tree, tree);
extern tree build_method_type (tree, tree);
extern tree build_offset_type (tree, tree);
extern tree build_complex_type (tree);
extern tree array_type_nelts (tree);

extern tree value_member (tree, tree);
extern tree purpose_member (tree, tree);
extern tree binfo_member (tree, tree);
extern unsigned int attribute_hash_list (tree);
extern int attribute_list_equal (tree, tree);
extern int attribute_list_contained (tree, tree);
extern int tree_int_cst_equal (tree, tree);
extern int tree_int_cst_lt (tree, tree);
extern int tree_int_cst_compare (tree, tree);
extern int host_integerp (tree, int);
extern long long tree_low_cst (tree, int);
extern int tree_int_cst_msb (tree);
extern int tree_int_cst_sgn (tree);
extern int tree_expr_nonnegative_p (tree);
extern int rtl_expr_nonnegative_p (rtx);
extern int index_type_equal (tree, tree);
extern tree get_inner_array_type (tree);





extern tree make_tree (tree, rtx);







extern tree build_type_attribute_variant (tree, tree);
extern tree build_decl_attribute_variant (tree, tree);


struct attribute_spec
{


  const char *const name;

  const int min_length;


  const int max_length;







  const _Bool decl_required;


  const _Bool type_required;




  const _Bool function_type_required;
# 2221 "tree.h"
  tree (*const handler) (tree *node, tree name, tree args, int flags, _Bool *no_add_attrs);

};

extern const struct attribute_spec default_target_attribute_table[];



enum attribute_flags
{



  ATTR_FLAG_DECL_NEXT = 1,



  ATTR_FLAG_FUNCTION_NEXT = 2,



  ATTR_FLAG_ARRAY_NEXT = 4,


  ATTR_FLAG_TYPE_IN_PLACE = 8,



  ATTR_FLAG_BUILT_IN = 16
};



extern tree merge_decl_attributes (tree, tree);
extern tree merge_type_attributes (tree, tree);
extern int default_comp_type_attributes (tree, tree);
extern void default_set_default_type_attributes (tree);
extern void default_insert_attributes (tree, tree *);
extern _Bool default_function_attribute_inlinable_p (tree);
extern _Bool default_ms_bitfield_layout_p (tree);



extern void split_specs_attrs (tree, tree *, tree *);



extern tree strip_attrs (tree);



extern int valid_machine_attribute (tree, tree, tree, tree);




extern int is_attribute_p (const char *, tree);




extern tree lookup_attribute (const char *, tree);



extern tree merge_attributes (tree, tree);
# 2298 "tree.h"
extern tree get_qualified_type (tree, int);




extern tree build_qualified_type (tree, int);
# 2317 "tree.h"
extern tree build_type_copy (tree);





extern void layout_type (tree);
# 2332 "tree.h"
typedef struct record_layout_info_s
{

  tree t;


  tree offset;

  unsigned int offset_align;

  tree bitpos;

  unsigned int record_align;

  unsigned int unpacked_align;


  unsigned int unpadded_align;

  tree prev_field;


  tree pending_statics;
  int packed_maybe_necessary;
} *record_layout_info;

extern void set_lang_adjust_rli (void (*) (record_layout_info));

extern record_layout_info start_record_layout (tree);
extern tree bit_from_pos (tree, tree);
extern tree byte_from_pos (tree, tree);
extern void pos_from_byte (tree *, tree *, unsigned int, tree);

extern void pos_from_bit (tree *, tree *, unsigned int, tree);

extern void normalize_offset (tree *, tree *, unsigned int);

extern tree rli_size_unit_so_far (record_layout_info);
extern tree rli_size_so_far (record_layout_info);
extern void normalize_rli (record_layout_info);
extern void place_field (record_layout_info, tree);
extern void compute_record_mode (tree);
extern void finish_record_layout (record_layout_info);






extern tree type_hash_canon (unsigned int, tree);
# 2391 "tree.h"
extern void layout_decl (tree, unsigned);






extern enum machine_mode mode_for_size_tree (tree, enum mode_class, int);




extern tree non_lvalue (tree);
extern tree pedantic_non_lvalue (tree);

extern tree convert (tree, tree);
extern unsigned int expr_align (tree);
extern tree size_in_bytes (tree);
extern long long int_size_in_bytes (tree);
extern tree bit_position (tree);
extern long long int_bit_position (tree);
extern tree byte_position (tree);
extern long long int_byte_position (tree);




enum size_type_kind
{
  SIZETYPE,
  SSIZETYPE,
  USIZETYPE,
  BITSIZETYPE,
  SBITSIZETYPE,
  UBITSIZETYPE,
  TYPE_KIND_LAST};

extern tree sizetype_tab[(int) TYPE_KIND_LAST];
# 2437 "tree.h"
extern tree size_binop (enum tree_code, tree, tree);
extern tree size_diffop (tree, tree);
extern tree size_int_wide (long long, enum size_type_kind);

extern tree size_int_type_wide (long long, tree);







extern tree round_up (tree, int);
extern tree round_down (tree, int);
extern tree get_pending_sizes (void);
extern int is_pending_size (tree);
extern void put_pending_size (tree);
extern void put_pending_sizes (tree);
# 2464 "tree.h"
extern unsigned int maximum_field_alignment;


extern unsigned int set_alignment;





extern tree chainon (tree, tree);



extern tree tree_cons (tree, tree, tree);



extern tree tree_last (tree);



extern tree nreverse (tree);




extern int list_length (tree);



extern int fields_length (tree);



extern int integer_zerop (tree);



extern int integer_onep (tree);




extern int integer_all_onesp (tree);




extern int integer_pow2p (tree);




extern int staticp (tree);




extern int lvalue_or_else (tree, const char *);





extern tree save_expr (tree);




extern int first_rtl_op (enum tree_code);





extern tree unsave_expr (tree);




extern void unsave_expr_1 (tree);



extern tree unsave_expr_now (tree);






extern void (*lang_unsave) (tree *);
extern void (*lang_unsave_expr_now) (tree);




extern int unsafe_for_reeval (tree);



extern int (*lang_unsafe_for_reeval) (tree);







extern int contains_placeholder_p (tree);




extern int has_cleanups (tree);






extern tree substitute_in_expr (tree, tree, tree);







extern tree variable_size (tree);





extern tree stabilize_reference (tree);





extern tree stabilize_reference_1 (tree);






extern tree get_unwidened (tree, tree);






extern tree get_narrower (tree, int *);






extern tree type_for_mode (enum machine_mode, int);






extern tree type_for_size (unsigned, int);






extern tree unsigned_type (tree);






extern tree signed_type (tree);





extern tree maybe_build_cleanup (tree);





extern tree get_inner_reference (tree, long long *, long long *, tree *, enum machine_mode *, int *, int *);






extern int handled_component_p (tree);




extern tree get_containing_scope (tree);



extern tree decl_function_context (tree);



extern tree decl_type_context (tree);






extern const char *function_cannot_inline_p (tree);


extern int real_zerop (tree);





extern const char *input_filename;


extern int lineno;




extern int pedantic_lvalues;




extern int immediate_size_expand;



extern tree current_function_decl;


extern tree current_function_func_begin_label;



extern int all_types_permanent;
# 2729 "tree.h"
extern const char *(*decl_printable_name) (tree, int);




extern void (*incomplete_decl_finalize_hook) (tree);



extern tree builtin_function (const char *, tree, int, enum built_in_class, const char *);




extern char *perm_calloc (int, long);
extern void clean_symbol_name (char *);
extern tree get_file_function_name_long (const char *);
extern tree get_set_constructor_bits (tree, char *, int);
extern tree get_set_constructor_bytes (tree, unsigned char *, int);

extern tree get_callee_fndecl (tree);
extern void set_decl_assembler_name (tree);
extern int type_num_arguments (tree);



extern int in_control_zone_p (void);
extern void expand_fixups (rtx);
extern tree expand_start_stmt_expr (int);
extern tree expand_end_stmt_expr (tree);
extern void expand_expr_stmt (tree);
extern void expand_expr_stmt_value (tree, int, int);
extern int warn_if_unused_value (tree);
extern void expand_decl_init (tree);
extern void clear_last_expr (void);
extern void expand_label (tree);
extern void expand_goto (tree);
extern void expand_asm (tree);
extern void expand_start_cond (tree, int);
extern void expand_end_cond (void);
extern void expand_start_else (void);
extern void expand_start_elseif (tree);
extern struct nesting *expand_start_loop (int);
extern struct nesting *expand_start_loop_continue_elsewhere (int);
extern struct nesting *expand_start_null_loop (void);
extern void expand_loop_continue_here (void);
extern void expand_end_loop (void);
extern void expand_end_null_loop (void);
extern int expand_continue_loop (struct nesting *);
extern int expand_exit_loop (struct nesting *);
extern int expand_exit_loop_if_false (struct nesting *, tree);

extern int expand_exit_loop_top_cond (struct nesting *, tree);

extern int expand_exit_something (void);

extern void expand_return (tree);
extern int optimize_tail_recursion (tree, rtx);
extern void expand_start_bindings_and_block (int, tree);


extern void expand_end_bindings (tree, int, int);
extern void warn_about_unused_variables (tree);
extern void start_cleanup_deferral (void);
extern void end_cleanup_deferral (void);
extern int is_body_block (tree);

extern int conditional_context (void);
extern struct nesting * current_nesting_level (void);
extern tree last_cleanup_this_contour (void);
extern void expand_start_case (int, tree, tree, const char *);

extern void expand_end_case_type (tree, tree);

extern int add_case_node (tree, tree, tree, tree *);

extern int pushcase (tree, tree (*) (tree, tree), tree, tree *);


extern int pushcase_range (tree, tree, tree (*) (tree, tree), tree, tree *);


extern void using_eh_for_cleanups (void);
extern int stmt_loop_nest_empty (void);
# 2822 "tree.h"
extern tree fold (tree);

extern int force_fit_type (tree, int);
extern int add_double (unsigned long long, long long, unsigned long long, long long, unsigned long long *, long long *);



extern int neg_double (unsigned long long, long long, unsigned long long *, long long *);


extern int mul_double (unsigned long long, long long, unsigned long long, long long, unsigned long long *, long long *);




extern void lshift_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *, int);



extern void rshift_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *, int);



extern void lrotate_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *);



extern void rrotate_double (unsigned long long, long long, long long, unsigned int, unsigned long long *, long long *);



extern int operand_equal_p (tree, tree, int);
extern tree invert_truthvalue (tree);





extern tree (*lang_type_promotes_to) (tree);
extern tree fold_builtin (tree);




extern void copy_lang_decl (tree);


extern int yyparse (void);



extern void pushlevel (int);





extern tree poplevel (int, int, int);

extern void set_block (tree);




extern tree pushdecl (tree);

extern tree getdecls (void);

extern tree gettags (void);

extern tree build_range_type (tree, tree, tree);


extern void record_component_aliases (tree);
extern long long get_alias_set (tree);
extern int alias_sets_conflict_p (long long, long long);

extern int readonly_fields_p (tree);
extern int objects_must_conflict_p (tree, tree);






extern void (*lang_set_decl_assembler_name) (tree);

struct obstack;


extern int really_constant_p (tree);
extern int int_fits_type_p (tree, tree);
extern int tree_log2 (tree);
extern int tree_floor_log2 (tree);
extern void preserve_data (void);
extern int object_permanent_p (tree);
extern int type_precision (tree);
extern int simple_cst_equal (tree, tree);
extern int compare_tree_int (tree, unsigned long long);

extern int type_list_equal (tree, tree);
extern int chain_member (tree, tree);
extern int chain_member_purpose (tree, tree);
extern int chain_member_value (tree, tree);
extern tree listify (tree);
extern tree type_hash_lookup (unsigned int, tree);
extern void type_hash_add (unsigned int, tree);
extern unsigned int type_hash_list (tree);
extern int simple_cst_list_equal (tree, tree);
extern void dump_tree_statistics (void);
extern void print_obstack_statistics (const char *, struct obstack *);


extern void print_obstack_name (char *, FILE *, const char *);


extern void expand_function_end (const char *, int, int);
extern void expand_function_start (tree, int);
extern void expand_pending_sizes (tree);

extern int real_onep (tree);
extern int real_twop (tree);
extern void gcc_obstack_init (struct obstack *);
extern void init_obstacks (void);
extern void build_common_tree_nodes (int);
extern void build_common_tree_nodes_2 (int);
extern void mark_tree_hashtable (void *);


extern void setjmp_protect_args (void);
extern void setjmp_protect (tree);
extern void expand_main_function (void);
extern void mark_varargs (void);
extern void init_dummy_function_start (void);
extern void expand_dummy_function_end (void);
extern void init_function_for_compilation (void);
extern void init_function_start (tree, const char *, int);
extern void assign_parms (tree);
extern void put_var_into_stack (tree);
extern void flush_addressof (tree);
extern void uninitialized_vars_warning (tree);
extern void setjmp_args_warning (void);
extern void mark_all_temps_used (void);
extern void init_temp_slots (void);
extern void combine_temp_slots (void);
extern void free_temp_slots (void);
extern void pop_temp_slots (void);
extern void push_temp_slots (void);
extern void preserve_temp_slots (rtx);
extern void preserve_rtl_expr_temps (tree);
extern int aggregate_value_p (tree);
extern void free_temps_for_rtl_expr (tree);
extern void instantiate_virtual_regs (tree, rtx);
extern void unshare_all_rtl (tree, rtx);
extern int max_parm_reg_num (void);
extern void push_function_context (void);
extern void pop_function_context (void);
extern void push_function_context_to (tree);
extern void pop_function_context_from (tree);
extern void ggc_mark_struct_function (struct function *);



extern void print_rtl (FILE *, rtx);



extern void debug_tree (tree);

extern void print_node (FILE *, const char *, tree, int);

extern void print_node_brief (FILE *, const char *, tree, int);

extern void indent_to (FILE *, int);



extern int apply_args_register_offset (int);
extern rtx expand_builtin_return_addr
        (enum built_in_function, int, rtx);
extern void check_max_integer_computation_mode (tree);


extern void start_sequence_for_rtl_expr (tree);
extern rtx emit_line_note (const char *, int);



extern int setjmp_call_p (tree);
# 3022 "tree.h"
extern tree decl_attributes (tree *, tree, int);






extern void insert_default_attributes (tree);


extern const struct attribute_spec *format_attribute_table;


extern const struct attribute_spec *lang_attribute_table;


extern int lang_attribute_common;



extern int mark_addressable (tree);
extern void incomplete_type_error (tree, tree);
extern tree truthvalue_conversion (tree);
extern int global_bindings_p (void);
extern void insert_block (tree);


extern void save_for_inline (tree);
extern void set_decl_abstract_flags (tree, int);
extern void output_inline_function (tree);
extern void set_decl_origin_self (tree);


extern void fixup_signed_type (tree);
extern void internal_reference_types (void);


extern void make_decl_rtl (tree, const char *);
extern void make_decl_one_only (tree);
extern int supports_one_only (void);
extern void variable_section (tree, int);


extern int div_and_round_double (enum tree_code, int, unsigned long long, long long, unsigned long long, long long, unsigned long long *, long long *, unsigned long long *, long long *);
# 3076 "tree.h"
extern void emit_nop (void);
extern void expand_computed_goto (tree);
extern _Bool parse_output_constraint (const char **, int, int, int, _Bool *, _Bool *, _Bool *);


extern void expand_asm_operands (tree, tree, tree, tree, int, const char *, int);

extern int any_pending_cleanups (int);
extern void init_stmt (void);
extern void init_stmt_for_function (void);
extern int drop_through_at_end_p (void);
extern void expand_start_target_temps (void);
extern void expand_end_target_temps (void);
extern void expand_elseif (tree);
extern void save_stack_pointer (void);
extern void expand_decl (tree);
extern int expand_decl_cleanup (tree, tree);
extern int expand_decl_cleanup_eh (tree, tree, int);
extern void expand_anon_union_decl (tree, tree, tree);
extern void move_cleanups_up (void);
extern void expand_start_case_dummy (void);
extern void expand_end_case_dummy (void);
extern tree case_index_expr_type (void);
extern long long all_cases_count (tree, int *);
extern void check_for_full_enumeration_handling (tree);
extern void declare_nonlocal_label (tree);



extern tree get_file_function_name (int);





extern char *dwarf2out_cfi_label (void);



extern void dwarf2out_def_cfa (const char *, unsigned, long);



extern void dwarf2out_window_save (const char *);




extern void dwarf2out_args_size (const char *, long);



extern void dwarf2out_reg_save (const char *, unsigned, long);



extern void dwarf2out_return_save (const char *, long);



extern void dwarf2out_return_reg (const char *, unsigned);



typedef tree (*walk_tree_fn) (tree *, int *, void *);





enum tree_dump_index
{
  TDI_all,
  TDI_class,
  TDI_original,
  TDI_optimized,
  TDI_inlined,

  TDI_end
};







typedef struct dump_info *dump_info_p;

extern int dump_flag (dump_info_p, int, tree);
extern int dump_enabled_p (enum tree_dump_index);
extern FILE *dump_begin (enum tree_dump_index, int *);
extern void dump_end (enum tree_dump_index, FILE *);
extern void dump_node (tree, int, FILE *);
extern int dump_switch_p (const char *);
extern const char *dump_flag_name (enum tree_dump_index);
# 3180 "tree.h"
extern void fancy_abort (const char *, int, const char *)
    __attribute__ ((__noreturn__));
# 36 "c-typeck.c" 2
# 1 "c-tree.h" 1
# 25 "c-tree.h"
# 1 "c-common.h" 1
# 25 "c-common.h"
# 1 "splay-tree.h" 1
# 37 "splay-tree.h"
# 1 "/usr/include/ansidecl.h" 1 3 4
# 38 "splay-tree.h" 2
# 49 "splay-tree.h"
typedef unsigned long int splay_tree_key;
typedef unsigned long int splay_tree_value;



typedef struct splay_tree_node_s *splay_tree_node;



typedef int (*splay_tree_compare_fn) (splay_tree_key, splay_tree_key);



typedef void (*splay_tree_delete_key_fn) (splay_tree_key);



typedef void (*splay_tree_delete_value_fn) (splay_tree_value);


typedef int (*splay_tree_foreach_fn) (splay_tree_node, void*);





typedef void *(*splay_tree_allocate_fn) (int, void *);





typedef void (*splay_tree_deallocate_fn) (void *, void *);


struct splay_tree_node_s
{

  splay_tree_key key;


  splay_tree_value value;


  splay_tree_node left;
  splay_tree_node right;
};


typedef struct splay_tree_s
{

  splay_tree_node root;


  splay_tree_compare_fn comp;


  splay_tree_delete_key_fn delete_key;


  splay_tree_delete_value_fn delete_value;


  splay_tree_allocate_fn allocate;
  splay_tree_deallocate_fn deallocate;
  void *allocate_data;

} *splay_tree;

extern splay_tree splay_tree_new (splay_tree_compare_fn, splay_tree_delete_key_fn, splay_tree_delete_value_fn);


extern splay_tree splay_tree_new_with_allocator
                                        (splay_tree_compare_fn, splay_tree_delete_key_fn, splay_tree_delete_value_fn, splay_tree_allocate_fn, splay_tree_deallocate_fn, void *);





extern void splay_tree_delete (splay_tree);
extern splay_tree_node splay_tree_insert
                                        (splay_tree, splay_tree_key, splay_tree_value);


extern void splay_tree_remove (splay_tree, splay_tree_key);

extern splay_tree_node splay_tree_lookup
                                        (splay_tree, splay_tree_key);

extern splay_tree_node splay_tree_predecessor
                                        (splay_tree, splay_tree_key);

extern splay_tree_node splay_tree_successor
                                        (splay_tree, splay_tree_key);

extern splay_tree_node splay_tree_max
                                        (splay_tree);
extern splay_tree_node splay_tree_min
                                        (splay_tree);
extern int splay_tree_foreach (splay_tree, splay_tree_foreach_fn, void*);


extern int splay_tree_compare_ints (splay_tree_key, splay_tree_key);

extern int splay_tree_compare_pointers (splay_tree_key, splay_tree_key);
# 26 "c-common.h" 2
# 1 "cpplib.h" 1
# 28 "cpplib.h"
# 1 "line-map.h" 1
# 31 "line-map.h"
enum lc_reason {LC_ENTER = 0, LC_LEAVE, LC_RENAME};
# 41 "line-map.h"
struct line_map
{
  const char *to_file;
  unsigned int to_line;
  unsigned int from_line;
  int included_from;
  enum lc_reason reason : 8;
  unsigned char sysp;
};


struct line_maps
{
  struct line_map *maps;
  unsigned int allocated;
  unsigned int used;




  int last_listed;


  unsigned int depth;


  _Bool trace_includes;
};


extern void init_line_maps
  (struct line_maps *);


extern void free_line_maps
  (struct line_maps *);
# 87 "line-map.h"
extern const struct line_map *add_line_map
  (struct line_maps *, enum lc_reason, unsigned int sysp, unsigned int from_line, const char *to_file, unsigned int to_line);




extern const struct line_map *lookup_line
  (struct line_maps *, unsigned int);




extern void print_containing_files
  (struct line_maps *, const struct line_map *);
# 29 "cpplib.h" 2







typedef struct cpp_reader cpp_reader;

typedef struct cpp_buffer cpp_buffer;
typedef struct cpp_options cpp_options;
typedef struct cpp_token cpp_token;
typedef struct cpp_string cpp_string;
typedef struct cpp_hashnode cpp_hashnode;
typedef struct cpp_macro cpp_macro;
typedef struct cpp_callbacks cpp_callbacks;

struct answer;
struct file_name_map_list;
# 143 "cpplib.h"
enum cpp_ttype
{
  CPP_EQ = 0, CPP_NOT, CPP_GREATER, CPP_LESS, CPP_PLUS, CPP_MINUS, CPP_MULT, CPP_DIV, CPP_MOD, CPP_AND, CPP_OR, CPP_XOR, CPP_RSHIFT, CPP_LSHIFT, CPP_MIN, CPP_MAX, CPP_COMPL, CPP_AND_AND, CPP_OR_OR, CPP_QUERY, CPP_COLON, CPP_COMMA, CPP_OPEN_PAREN, CPP_CLOSE_PAREN, CPP_EQ_EQ, CPP_NOT_EQ, CPP_GREATER_EQ, CPP_LESS_EQ, CPP_PLUS_EQ, CPP_MINUS_EQ, CPP_MULT_EQ, CPP_DIV_EQ, CPP_MOD_EQ, CPP_AND_EQ, CPP_OR_EQ, CPP_XOR_EQ, CPP_RSHIFT_EQ, CPP_LSHIFT_EQ, CPP_MIN_EQ, CPP_MAX_EQ, CPP_HASH, CPP_PASTE, CPP_OPEN_SQUARE, CPP_CLOSE_SQUARE, CPP_OPEN_BRACE, CPP_CLOSE_BRACE, CPP_SEMICOLON, CPP_ELLIPSIS, CPP_PLUS_PLUS, CPP_MINUS_MINUS, CPP_DEREF, CPP_DOT, CPP_SCOPE, CPP_DEREF_STAR, CPP_DOT_STAR, CPP_ATSIGN, CPP_NAME, CPP_NUMBER, CPP_CHAR, CPP_WCHAR, CPP_OTHER, CPP_STRING, CPP_WSTRING, CPP_HEADER_NAME, CPP_COMMENT, CPP_MACRO_ARG, CPP_PADDING, CPP_EOF,
  N_TTYPES
};




enum c_lang {CLK_GNUC89 = 0, CLK_GNUC99, CLK_STDC89, CLK_STDC94, CLK_STDC99,
             CLK_GNUCXX, CLK_CXX98, CLK_OBJC, CLK_OBJCXX, CLK_ASM};


struct cpp_string
{
  unsigned int len;
  const unsigned char *text;
};
# 173 "cpplib.h"
struct cpp_token
{
  unsigned int line;
  unsigned short col;
  enum cpp_ttype type : 8;
  unsigned char flags;

  union
  {
    cpp_hashnode *node;
    const cpp_token *source;
    struct cpp_string str;
    unsigned int arg_no;
    unsigned char c;
  } val;
};



typedef int cppchar_t;
# 201 "cpplib.h"
enum { dump_none = 0, dump_only, dump_names, dump_definitions };



struct cpp_options
{

  const char *in_fname;
  const char *out_fname;


  unsigned int tabstop;


  struct cpp_pending *pending;



  const char *deps_file;


  struct search_path *quote_include;
  struct search_path *bracket_include;



  struct file_name_map_list *map_list;



  const char *include_prefix;
  unsigned int include_prefix_len;


  const char *user_label_prefix;


  enum c_lang lang;


  unsigned char verbose;


  unsigned char signed_char;


  unsigned char cplusplus;


  unsigned char cplusplus_comments;


  unsigned char objc;


  unsigned char discard_comments;


  unsigned char trigraphs;


  unsigned char digraphs;


  unsigned char extended_numbers;




  unsigned char print_deps;


  unsigned char deps_phony_targets;



  unsigned char print_deps_missing_files;


  unsigned char print_deps_append;


  unsigned char print_include_names;


  unsigned char pedantic_errors;


  unsigned char inhibit_warnings;


  unsigned char warn_system_headers;



  unsigned char inhibit_errors;


  unsigned char warn_comments;


  unsigned char warn_trigraphs;


  unsigned char warn_import;



  unsigned char warn_traditional;


  unsigned char warnings_are_errors;



  unsigned char no_output;



  unsigned char remap;


  unsigned char no_line_commands;



  unsigned char ignore_srcdir;


  unsigned char dollars_in_ident;


  unsigned char warn_undef;


  unsigned char c99;


  unsigned char pedantic;



  unsigned char preprocessed;


  unsigned char no_standard_includes;


  unsigned char no_standard_cplusplus_includes;


  unsigned char dump_macros;


  unsigned char dump_includes;


  unsigned char show_column;


  unsigned char operator_names;




  unsigned char help_only;
};


struct cpp_callbacks
{

  void (*line_change) (cpp_reader *, const cpp_token *, int);
  void (*file_change) (cpp_reader *, const struct line_map *);
  void (*include) (cpp_reader *, unsigned int, const unsigned char *, const cpp_token *);

  void (*define) (cpp_reader *, unsigned int, cpp_hashnode *);
  void (*undef) (cpp_reader *, unsigned int, cpp_hashnode *);
  void (*ident) (cpp_reader *, unsigned int, const cpp_string *);
  void (*def_pragma) (cpp_reader *, unsigned int);
};






extern const char *progname;
# 410 "cpplib.h"
enum node_type
{
  NT_VOID = 0,
  NT_MACRO,
  NT_ASSERTION
};



enum builtin_type
{
  BT_SPECLINE = 0,
  BT_DATE,
  BT_FILE,
  BT_BASE_FILE,
  BT_INCLUDE_LEVEL,
  BT_TIME,
  BT_STDC,
  BT_PRAGMA
};
# 439 "cpplib.h"
struct cpp_hashnode
{
  struct ht_identifier ident;
  unsigned short arg_index;
  unsigned char directive_index;
  unsigned char rid_code;
  enum node_type type : 8;
  unsigned char flags;

  union
  {
    cpp_macro *macro;
    struct answer *answers;
    enum cpp_ttype operator;
    enum builtin_type builtin;
  } value;
};


extern cpp_reader *cpp_create_reader (enum c_lang);






extern cpp_options *cpp_get_options (cpp_reader *);
extern const struct line_maps *cpp_get_line_maps (cpp_reader *);
extern cpp_callbacks *cpp_get_callbacks (cpp_reader *);
extern void cpp_set_callbacks (cpp_reader *, cpp_callbacks *);
# 478 "cpplib.h"
extern int cpp_handle_options (cpp_reader *, int, char **);
extern int cpp_handle_option (cpp_reader *, int, char **, int);
extern void cpp_post_options (cpp_reader *);
# 492 "cpplib.h"
extern const char *cpp_read_main_file (cpp_reader *, const char *, struct ht *);







extern void cpp_finish_options (cpp_reader *);




extern int cpp_destroy (cpp_reader *);


extern unsigned int cpp_errors (cpp_reader *);

extern unsigned int cpp_token_len (const cpp_token *);
extern unsigned char *cpp_token_as_text (cpp_reader *, const cpp_token *);

extern unsigned char *cpp_spell_token (cpp_reader *, const cpp_token *, unsigned char *);

extern void cpp_register_pragma (cpp_reader *, const char *, const char *, void (*) (cpp_reader *));



extern void cpp_finish (cpp_reader *);
extern int cpp_avoid_paste (cpp_reader *, const cpp_token *, const cpp_token *);

extern const cpp_token *cpp_get_token (cpp_reader *);
extern const unsigned char *cpp_macro_definition (cpp_reader *, const cpp_hashnode *);

extern void _cpp_backup_tokens (cpp_reader *, unsigned int);


extern long long
cpp_interpret_charconst (cpp_reader *, const cpp_token *, int, int, unsigned int *);


extern void cpp_define (cpp_reader *, const char *);
extern void cpp_assert (cpp_reader *, const char *);
extern void cpp_undef (cpp_reader *, const char *);
extern void cpp_unassert (cpp_reader *, const char *);

extern cpp_buffer *cpp_push_buffer (cpp_reader *, const unsigned char *, size_t, int, int);


extern int cpp_defined (cpp_reader *, const unsigned char *, int);







extern void cpp_ice (cpp_reader *, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
extern void cpp_fatal (cpp_reader *, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
extern void cpp_error (cpp_reader *, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
extern void cpp_warning (cpp_reader *, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
extern void cpp_pedwarn (cpp_reader *, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
extern void cpp_notice (cpp_reader *, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 2, 3)));
extern void cpp_error_with_line (cpp_reader *, int, int, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 4, 5)));
extern void cpp_warning_with_line (cpp_reader *, int, int, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 4, 5)));
extern void cpp_pedwarn_with_line (cpp_reader *, int, int, const char *msgid, ...)
  __attribute__ ((__format__ (__printf__, 4, 5)));
extern void cpp_error_from_errno (cpp_reader *, const char *);
extern void cpp_notice_from_errno (cpp_reader *, const char *);


extern int cpp_ideq (const cpp_token *, const char *);

extern void cpp_output_line (cpp_reader *, FILE *);
extern void cpp_output_token (const cpp_token *, FILE *);
extern const char *cpp_type2name (enum cpp_ttype);
extern unsigned int cpp_parse_escape (cpp_reader *, const unsigned char **, const unsigned char *, unsigned long long, int);
# 584 "cpplib.h"
extern cpp_hashnode *cpp_lookup (cpp_reader *, const unsigned char *, unsigned int);



typedef int (*cpp_cb) (cpp_reader *, cpp_hashnode *, void *);
extern void cpp_forall_identifiers (cpp_reader *, cpp_cb, void *);



extern void cpp_scan_nooutput (cpp_reader *);
extern int cpp_sys_macro_p (cpp_reader *);
extern unsigned char *cpp_quote_string (unsigned char *, const unsigned char *, unsigned int);




extern int cpp_included (cpp_reader *, const char *);
extern void cpp_make_system_header (cpp_reader *, int, int);
# 27 "c-common.h" 2
# 51 "c-common.h"
enum rid
{


  RID_STATIC = 0,
  RID_UNSIGNED, RID_LONG, RID_CONST, RID_EXTERN,
  RID_REGISTER, RID_TYPEDEF, RID_SHORT, RID_INLINE,
  RID_VOLATILE, RID_SIGNED, RID_AUTO, RID_RESTRICT,


  RID_BOUNDED, RID_UNBOUNDED, RID_COMPLEX,


  RID_FRIEND, RID_VIRTUAL, RID_EXPLICIT, RID_EXPORT, RID_MUTABLE,


  RID_IN, RID_OUT, RID_INOUT, RID_BYCOPY, RID_BYREF, RID_ONEWAY,


  RID_INT, RID_CHAR, RID_FLOAT, RID_DOUBLE, RID_VOID,
  RID_ENUM, RID_STRUCT, RID_UNION, RID_IF, RID_ELSE,
  RID_WHILE, RID_DO, RID_FOR, RID_SWITCH, RID_CASE,
  RID_DEFAULT, RID_BREAK, RID_CONTINUE, RID_RETURN, RID_GOTO,
  RID_SIZEOF,


  RID_ASM, RID_TYPEOF, RID_ALIGNOF, RID_ATTRIBUTE, RID_VA_ARG,
  RID_EXTENSION, RID_IMAGPART, RID_REALPART, RID_LABEL, RID_PTRBASE,
  RID_PTREXTENT, RID_PTRVALUE, RID_CHOOSE_EXPR, RID_TYPES_COMPATIBLE_P,


  RID_FUNCTION_NAME, RID_PRETTY_FUNCTION_NAME, RID_C99_FUNCTION_NAME,


  RID_BOOL, RID_WCHAR, RID_CLASS,
  RID_PUBLIC, RID_PRIVATE, RID_PROTECTED,
  RID_TEMPLATE, RID_NULL, RID_CATCH,
  RID_DELETE, RID_FALSE, RID_NAMESPACE,
  RID_NEW, RID_OPERATOR, RID_THIS,
  RID_THROW, RID_TRUE, RID_TRY,
  RID_TYPENAME, RID_TYPEID, RID_USING,


  RID_CONSTCAST, RID_DYNCAST, RID_REINTCAST, RID_STATCAST,


  RID_AND, RID_AND_EQ, RID_NOT, RID_NOT_EQ,
  RID_OR, RID_OR_EQ, RID_XOR, RID_XOR_EQ,
  RID_BITAND, RID_BITOR, RID_COMPL,


  RID_ID, RID_AT_ENCODE, RID_AT_END,
  RID_AT_CLASS, RID_AT_ALIAS, RID_AT_DEFS,
  RID_AT_PRIVATE, RID_AT_PROTECTED, RID_AT_PUBLIC,
  RID_AT_PROTOCOL, RID_AT_SELECTOR, RID_AT_INTERFACE,
  RID_AT_IMPLEMENTATION,

  RID_MAX,

  RID_FIRST_MODIFIER = RID_STATIC,
  RID_LAST_MODIFIER = RID_ONEWAY,

  RID_FIRST_AT = RID_AT_ENCODE,
  RID_LAST_AT = RID_AT_IMPLEMENTATION,
  RID_FIRST_PQ = RID_IN,
  RID_LAST_PQ = RID_ONEWAY
};
# 129 "c-common.h"
extern tree *ridpointers;



enum c_tree_index
{
    CTI_WCHAR_TYPE,
    CTI_SIGNED_WCHAR_TYPE,
    CTI_UNSIGNED_WCHAR_TYPE,
    CTI_WINT_TYPE,
    CTI_C_SIZE_TYPE,



    CTI_SIGNED_SIZE_TYPE,
    CTI_UNSIGNED_PTRDIFF_TYPE,
    CTI_INTMAX_TYPE,
    CTI_UINTMAX_TYPE,
    CTI_WIDEST_INT_LIT_TYPE,
    CTI_WIDEST_UINT_LIT_TYPE,

    CTI_CHAR_ARRAY_TYPE,
    CTI_WCHAR_ARRAY_TYPE,
    CTI_INT_ARRAY_TYPE,
    CTI_STRING_TYPE,
    CTI_CONST_STRING_TYPE,


    CTI_BOOLEAN_TYPE,
    CTI_BOOLEAN_TRUE,
    CTI_BOOLEAN_FALSE,

    CTI_C_BOOL_TYPE,
    CTI_C_BOOL_TRUE,
    CTI_C_BOOL_FALSE,
    CTI_DEFAULT_FUNCTION_TYPE,

    CTI_G77_INTEGER_TYPE,
    CTI_G77_UINTEGER_TYPE,
    CTI_G77_LONGINT_TYPE,
    CTI_G77_ULONGINT_TYPE,


    CTI_FUNCTION_NAME_DECL,
    CTI_PRETTY_FUNCTION_NAME_DECL,
    CTI_C99_FUNCTION_NAME_DECL,
    CTI_SAVED_FUNCTION_NAME_DECLS,

    CTI_VOID_ZERO,

    CTI_MAX
};





struct c_common_identifier
{
  struct tree_common common;
  struct cpp_hashnode node;
};
# 234 "c-common.h"
extern tree c_global_trees[CTI_MAX];
# 244 "c-common.h"
typedef enum c_language_kind
{
  clk_c,

  clk_cplusplus,
  clk_objective_c
}
c_language_kind;



struct stmt_tree_s {

  tree x_last_stmt;


  tree x_last_expr_type;

  const char *x_last_expr_filename;
# 277 "c-common.h"
  int stmts_are_full_exprs_p;
};

typedef struct stmt_tree_s *stmt_tree;




struct language_function {


  struct stmt_tree_s x_stmt_tree;

  tree x_scope_stmt_stack;
};
# 320 "c-common.h"
extern int (*lang_statement_code_p) (enum tree_code);
extern void (*lang_expand_stmt) (tree);
extern void (*lang_expand_decl_stmt) (tree);
extern void (*lang_expand_function_end) (void);



extern int (*lang_missing_noreturn_ok_p) (tree);


extern stmt_tree current_stmt_tree (void);
extern tree *current_scope_stmt_stack (void);
extern void begin_stmt_tree (tree *);
extern tree add_stmt (tree);
extern void add_decl_stmt (tree);
extern tree add_scope_stmt (int, int);
extern void finish_stmt_tree (tree *);

extern int statement_code_p (enum tree_code);
extern tree walk_stmt_tree (tree *, walk_tree_fn, void *);


extern void prep_stmt (tree);
extern void expand_stmt (tree);
extern void mark_stmt_tree (void *);
extern void shadow_warning (const char *, tree, tree);

extern tree c_begin_if_stmt (void);
extern tree c_begin_while_stmt (void);
extern void c_finish_while_stmt_cond (tree, tree);







struct c_lang_decl {
  unsigned declared_inline : 1;
};
# 368 "c-common.h"
extern void c_mark_lang_decl (struct c_lang_decl *);




extern c_language_kind c_language;




extern int flag_const_strings;



extern int flag_short_double;



extern int flag_short_wchar;



extern int warn_format;



extern int warn_format_y2k;



extern int warn_format_extra_args;



extern int warn_format_nonliteral;



extern int warn_format_security;



extern int warn_sequence_point;



extern int warn_missing_format_attribute;




extern int warn_pointer_arith;


extern int warn_div_by_zero;



extern int flag_traditional;



extern int flag_isoc94;



extern int flag_isoc99;



extern int flag_hosted;




extern int flag_noniso_default_format_attributes;



extern int flag_no_builtin;




extern int flag_no_nonansi_builtin;



extern int warn_parentheses;



extern int warn_conversion;




extern int warn_long_long;
# 492 "c-common.h"
extern tree (*make_fname_decl) (tree, int);

extern tree identifier_global_value (tree);
extern void record_builtin_type (enum rid, const char *, tree);

extern tree build_void_list_node (void);
extern void start_fname_decls (void);
extern void finish_fname_decls (void);
extern const char *fname_as_string (int);
extern tree fname_decl (unsigned, tree);
extern const char *fname_string (unsigned);

extern void check_function_format (int *, tree, tree);
extern void set_Wformat (int);
extern tree handle_format_attribute (tree *, tree, tree, int, _Bool *);

extern tree handle_format_arg_attribute (tree *, tree, tree, int, _Bool *);

extern void c_common_insert_default_attributes (tree);
extern void c_apply_type_quals_to_decl (int, tree);
extern tree c_sizeof (tree);
extern tree c_alignof (tree);
extern tree c_alignof_expr (tree);


extern void binary_op_error (enum tree_code);
extern tree c_expand_expr_stmt (tree);
extern void c_expand_start_cond (tree, int, tree);
extern void c_finish_then (void);
extern void c_expand_start_else (void);
extern void c_finish_else (void);
extern void c_expand_end_cond (void);

extern tree check_case_value (tree);

extern tree combine_strings (tree);
extern void constant_expression_warning (tree);
extern tree convert_and_check (tree, tree);
extern void overflow_warning (tree);
extern void unsigned_conversion_warning (tree, tree);


extern char *get_directive_line (void);





extern tree shorten_compare (tree *, tree *, tree *, enum tree_code *);

extern tree pointer_int_sum (enum tree_code, tree, tree);
extern unsigned int min_precision (tree, int);


extern tree c_build_qualified_type (tree, int);



extern void c_common_nodes_and_builtins (void);

extern void disable_builtin_function (const char *);

extern tree build_va_arg (tree, tree);

extern void c_common_init_options (enum c_language_kind);
extern void c_common_post_options (void);
extern const char *c_common_init (const char *);
extern void c_common_finish (void);
extern long long c_common_get_alias_set (tree);
extern _Bool c_promoting_integer_type_p (tree);
extern int self_promoting_args_p (tree);
extern tree simple_type_promotes_to (tree);
extern tree strip_array_types (tree);
# 739 "c-common.h"
enum c_tree_code {
  C_DUMMY_TREE_CODE = LAST_AND_UNUSED_TREE_CODE,
# 1 "c-common.def" 1
# 29 "c-common.def"
SRCLOC,

SIZEOF_EXPR,
ARROW_EXPR,
ALIGNOF_EXPR,



EXPR_STMT,



COMPOUND_STMT,



DECL_STMT,



IF_STMT,



FOR_STMT,



WHILE_STMT,



DO_STMT,



RETURN_STMT,


BREAK_STMT,


CONTINUE_STMT,



SWITCH_STMT,


GOTO_STMT,



LABEL_STMT,


ASM_STMT,







SCOPE_STMT,



FILE_STMT,





CASE_LABEL,



STMT_EXPR,






COMPOUND_LITERAL_EXPR,




CLEANUP_STMT,
# 742 "c-common.h" 2
  LAST_C_TREE_CODE
};



extern void add_c_tree_codes (void);
extern void genrtl_do_pushlevel (void);
extern void genrtl_goto_stmt (tree);
extern void genrtl_expr_stmt (tree);
extern void genrtl_expr_stmt_value (tree, int, int);
extern void genrtl_decl_stmt (tree);
extern void genrtl_if_stmt (tree);
extern void genrtl_while_stmt (tree);
extern void genrtl_do_stmt (tree);
extern void genrtl_return_stmt (tree);
extern void genrtl_for_stmt (tree);
extern void genrtl_break_stmt (void);
extern void genrtl_continue_stmt (void);
extern void genrtl_scope_stmt (tree);
extern void genrtl_switch_stmt (tree);
extern void genrtl_case_label (tree);
extern void genrtl_compound_stmt (tree);
extern void genrtl_asm_stmt (tree, tree, tree, tree, tree, int);


extern void genrtl_decl_cleanup (tree);
extern int stmts_are_full_exprs_p (void);
extern int anon_aggr_type_p (tree);
# 787 "c-common.h"
extern void emit_local_var (tree);
extern void make_rtl_for_local_static (tree);
extern tree expand_cond (tree);
extern tree c_expand_return (tree);
extern tree do_case (tree, tree);
extern tree build_stmt (enum tree_code, ...);
extern tree build_case_label (tree, tree, tree);
extern tree build_continue_stmt (void);
extern tree build_break_stmt (void);
extern tree build_return_stmt (tree);







extern void c_expand_asm_operands (tree, tree, tree, tree, int, const char *, int);




extern tree build_unary_op (enum tree_code, tree, int);

extern tree build_binary_op (enum tree_code, tree, tree, int);

extern int lvalue_p (tree);
extern tree default_conversion (tree);




extern tree common_type (tree, tree);

extern tree expand_tree_builtin (tree, tree, tree);

extern tree decl_constant_value (tree);


extern tree boolean_increment (enum tree_code, tree);




extern void extract_interface_info (void);

extern void mark_c_language_function (struct language_function *);

extern int case_compare (splay_tree_key, splay_tree_key);


extern tree c_add_case_label (splay_tree, tree, tree, tree);



extern tree build_function_call (tree, tree);

extern tree finish_label_address_expr (tree);



extern tree lookup_label (tree);
# 857 "c-common.h"
extern int c_safe_from_p (rtx, tree);

extern int c_staticp (tree);

extern int c_unsafe_for_reeval (tree);



struct c_fileinfo
{
  int time;
  short interface_only;
  short interface_unknown;
};

struct c_fileinfo *get_fileinfo (const char *);
extern void dump_time_statistics (void);
# 26 "c-tree.h" 2
# 37 "c-tree.h"
struct lang_identifier
{
  struct c_common_identifier ignore;
  tree global_value, local_value, label_value, implicit_decl;
  tree error_locus, limbo_value;
};



struct lang_decl
{
  struct c_lang_decl base;



  tree pending_sizes;
};
# 110 "c-tree.h"
struct lang_type
{
  int len;
  tree elts[1];
};
# 151 "c-tree.h"
extern tree lookup_interface (tree);
extern tree is_class_name (tree);
extern void maybe_objc_check_decl (tree);
extern void finish_file (void);
extern int maybe_objc_comptypes (tree, tree, int);
extern tree maybe_building_objc_message_expr (void);
extern int recognize_objc_keyword (void);
extern tree lookup_objc_ivar (tree);



extern void c_parse_init (void);
extern void c_set_yydebug (int);
extern int yyparse_1 (void);


extern void gen_aux_info_record (tree, int, int, int);


extern void c_init_decl_processing (void);
extern void c_print_identifier (FILE *, tree, int);
extern tree build_array_declarator (tree, tree, int, int);
extern tree build_enumerator (tree, tree);
extern int c_decode_option (int, char **);
extern void c_mark_varargs (void);
extern void check_for_loop_decls (void);
extern void clear_parm_order (void);
extern int complete_array_type (tree, tree, int);
extern void declare_parm_level (int);
extern tree define_label (const char *, int, tree);

extern void finish_decl (tree, tree, tree);
extern tree finish_enum (tree, tree, tree);
extern void finish_function (int, int);
extern tree finish_struct (tree, tree, tree);
extern tree get_parm_info (int);
extern tree grokfield (const char *, int, tree, tree, tree);
extern tree groktypename (tree);
extern tree groktypename_in_parm_context (tree);
extern tree implicitly_declare (tree);
extern void implicit_decl_warning (tree);
extern int in_parm_level_p (void);
extern void keep_next_level (void);
extern int kept_level_p (void);
extern tree lookup_name (tree);
extern tree lookup_name_current_level (tree);
extern void parmlist_tags_warning (void);
extern void pending_xref_error (void);
extern void mark_c_function_context (struct function *);
extern void push_c_function_context (struct function *);
extern void pop_c_function_context (struct function *);
extern void pop_label_level (void);
extern void push_label_level (void);
extern void push_parm_decl (tree);
extern tree pushdecl_top_level (tree);
extern void pushtag (tree, tree);
extern tree set_array_declarator_type (tree, tree, int);
extern tree shadow_label (tree);
extern void shadow_tag (tree);
extern void shadow_tag_warned (tree, int);
extern tree start_enum (tree);
extern int start_function (tree, tree, tree);
extern tree start_decl (tree, tree, int, tree);

extern tree start_struct (enum tree_code, tree);
extern void store_parm_decls (void);
extern tree xref_tag (enum tree_code, tree);
extern tree c_begin_compound_stmt (void);
extern void c_expand_deferred_function (tree);
extern void c_expand_decl_stmt (tree);



extern int c_disregard_inline_limits (tree);
extern int c_cannot_inline_tree_fn (tree *);
extern const char *c_objc_common_init (const char *);
extern int c_missing_noreturn_ok_p (tree);
extern void c_objc_common_finish_file (void);
extern int defer_fn (tree);







extern tree require_complete_type (tree);
extern int comptypes (tree, tree);
extern tree c_sizeof_nowarn (tree);
extern tree c_size_in_bytes (tree);
extern tree build_component_ref (tree, tree);
extern tree build_indirect_ref (tree, const char *);
extern tree build_array_ref (tree, tree);
extern tree build_external_ref (tree, int);
extern tree parser_build_binary_op (enum tree_code, tree, tree);

extern void readonly_warning (tree, const char *);
extern tree build_conditional_expr (tree, tree, tree);
extern tree build_compound_expr (tree);
extern tree c_cast_expr (tree, tree);
extern tree build_c_cast (tree, tree);
extern tree build_modify_expr (tree, enum tree_code, tree);

extern void store_init_value (tree, tree);
extern void error_init (const char *);
extern void pedwarn_init (const char *);
extern void start_init (tree, tree, int);
extern void finish_init (void);
extern void really_start_incremental_init (tree);
extern void push_init_level (int);
extern tree pop_init_level (int);
extern void set_init_index (tree, tree);
extern void set_init_label (tree);
extern void process_init_element (tree);
extern tree build_compound_literal (tree, tree);
extern void pedwarn_c99 (const char *, ...)
                                                        __attribute__ ((__format__ (__printf__, 1, 2)));
extern tree c_start_case (tree);
extern void c_finish_case (void);
extern tree simple_asm_stmt (tree);
extern tree build_asm_stmt (tree, tree, tree, tree, tree);

extern tree c_convert_parm_for_inlining (tree, tree, tree);




extern int current_function_returns_value;




extern int current_function_returns_null;




extern int current_function_returns_abnormally;




extern int skip_evaluation;



extern int dollars_in_ident;




extern int flag_cond_mismatch;



extern int flag_no_asm;



extern int warn_implicit;



extern int warn_strict_prototypes;




extern int warn_redundant_decls;







extern int warn_nested_externs;




extern int warn_cast_qual;





extern int warn_bad_function_cast;



extern int warn_traditional;



extern int warn_char_subscripts;



extern int warn_main;



extern int flag_allow_single_precision;



extern int warn_missing_braces;



extern int warn_sign_compare;



extern int warn_float_equal;



extern int warn_multichar;



extern int system_header_p;


extern int mesg_implicit_function_declaration;


extern void finish_incomplete_decl (tree);

extern tree static_ctors;
extern tree static_dtors;
# 37 "c-typeck.c" 2
# 1 "tm_p.h" 1
# 1 "i386-protos.h" 1
# 23 "i386-protos.h"
extern void override_options (void);
extern void optimization_options (int, int);

extern int ix86_can_use_return_insn_p (void);
extern int ix86_frame_pointer_required (void);
extern void ix86_setup_frame_addresses (void);

extern void ix86_asm_file_end (FILE *);
extern void load_pic_register (void);
extern long long ix86_initial_elimination_offset (int, int);
extern void ix86_expand_prologue (void);
extern void ix86_expand_epilogue (int);

extern void ix86_output_addr_vec_elt (FILE *, int);
extern void ix86_output_addr_diff_elt (FILE *, int, int);


extern int ix86_aligned_p (rtx);

extern int standard_80387_constant_p (rtx);
extern int standard_sse_constant_p (rtx);
extern int symbolic_reference_mentioned_p (rtx);

extern int x86_64_general_operand (rtx, enum machine_mode);
extern int x86_64_szext_general_operand (rtx, enum machine_mode);
extern int x86_64_nonmemory_operand (rtx, enum machine_mode);
extern int x86_64_szext_nonmemory_operand (rtx, enum machine_mode);
extern int x86_64_immediate_operand (rtx, enum machine_mode);
extern int x86_64_zext_immediate_operand (rtx, enum machine_mode);
extern int const_int_1_operand (rtx, enum machine_mode);
extern int symbolic_operand (rtx, enum machine_mode);
extern int pic_symbolic_operand (rtx, enum machine_mode);
extern int call_insn_operand (rtx, enum machine_mode);
extern int constant_call_address_operand (rtx, enum machine_mode);
extern int const0_operand (rtx, enum machine_mode);
extern int const1_operand (rtx, enum machine_mode);
extern int const248_operand (rtx, enum machine_mode);
extern int incdec_operand (rtx, enum machine_mode);
extern int reg_no_sp_operand (rtx, enum machine_mode);
extern int mmx_reg_operand (rtx, enum machine_mode);
extern int general_no_elim_operand (rtx, enum machine_mode);
extern int nonmemory_no_elim_operand (rtx, enum machine_mode);
extern int q_regs_operand (rtx, enum machine_mode);
extern int non_q_regs_operand (rtx, enum machine_mode);
extern int sse_comparison_operator (rtx, enum machine_mode);
extern int fcmov_comparison_operator (rtx, enum machine_mode);
extern int cmp_fp_expander_operand (rtx, enum machine_mode);
extern int ix86_comparison_operator (rtx, enum machine_mode);
extern int ext_register_operand (rtx, enum machine_mode);
extern int binary_fp_operator (rtx, enum machine_mode);
extern int mult_operator (rtx, enum machine_mode);
extern int div_operator (rtx, enum machine_mode);
extern int arith_or_logical_operator (rtx, enum machine_mode);
extern int promotable_binary_operator (rtx, enum machine_mode);
extern int memory_displacement_operand (rtx, enum machine_mode);
extern int cmpsi_operand (rtx, enum machine_mode);
extern int long_memory_operand (rtx, enum machine_mode);
extern int aligned_operand (rtx, enum machine_mode);
extern enum machine_mode ix86_cc_mode (enum rtx_code, rtx, rtx);

extern int ix86_expand_movstr (rtx, rtx, rtx, rtx);
extern int ix86_expand_clrstr (rtx, rtx, rtx);
extern int ix86_expand_strlen (rtx, rtx, rtx, rtx);

extern int legitimate_pic_address_disp_p (rtx);
extern int legitimate_address_p (enum machine_mode, rtx, int);
extern rtx legitimize_pic_address (rtx, rtx);
extern rtx legitimize_address (rtx, rtx, enum machine_mode);

extern void print_reg (rtx, int, FILE*);
extern void print_operand (FILE*, rtx, int);
extern void print_operand_address (FILE*, rtx);

extern void split_di (rtx[], int, rtx[], rtx[]);
extern void split_ti (rtx[], int, rtx[], rtx[]);

extern const char *output_387_binary_op (rtx, rtx*);
extern const char *output_fix_trunc (rtx, rtx*);
extern const char *output_fp_compare (rtx, rtx*, int, int);

extern void i386_dwarf_output_addr_const (FILE*, rtx);
extern rtx i386_simplify_dwarf_addr (rtx);

extern void ix86_expand_clear (rtx);
extern void ix86_expand_move (enum machine_mode, rtx[]);
extern void ix86_expand_vector_move (enum machine_mode, rtx[]);
extern void ix86_expand_binary_operator (enum rtx_code, enum machine_mode, rtx[]);

extern int ix86_binary_operator_ok (enum rtx_code, enum machine_mode, rtx[]);

extern void ix86_expand_unary_operator (enum rtx_code, enum machine_mode, rtx[]);

extern int ix86_unary_operator_ok (enum rtx_code, enum machine_mode, rtx[]);

extern int ix86_match_ccmode (rtx, enum machine_mode);
extern rtx ix86_expand_compare (enum rtx_code, rtx *, rtx *);
extern int ix86_use_fcomi_compare (enum rtx_code);
extern void ix86_expand_branch (enum rtx_code, rtx);
extern int ix86_expand_setcc (enum rtx_code, rtx);
extern int ix86_expand_int_movcc (rtx[]);
extern int ix86_expand_fp_movcc (rtx[]);
extern void x86_initialize_trampoline (rtx, rtx, rtx);
extern rtx ix86_zero_extend_to_Pmode (rtx);
extern void ix86_split_long_move (rtx[]);
extern void ix86_split_ashldi (rtx *, rtx);
extern void ix86_split_ashrdi (rtx *, rtx);
extern void ix86_split_lshrdi (rtx *, rtx);
extern int ix86_address_cost (rtx);
extern rtx ix86_find_base_term (rtx);

extern rtx assign_386_stack_local (enum machine_mode, int);
extern int ix86_attr_length_immediate_default (rtx, int);
extern int ix86_attr_length_address_default (rtx);

extern enum machine_mode ix86_fp_compare_mode (enum rtx_code);

extern int x86_64_sign_extended_value (rtx);
extern int x86_64_zero_extended_value (rtx);
extern rtx ix86_libcall_value (enum machine_mode);
extern _Bool ix86_function_value_regno_p (int);
extern _Bool ix86_function_arg_regno_p (int);
extern int ix86_function_arg_boundary (enum machine_mode, tree);
extern int ix86_return_in_memory (tree);
extern void ix86_va_start (int, tree, rtx);
extern rtx ix86_va_arg (tree, tree);
extern void ix86_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode, tree, int *, int);



extern rtx ix86_force_to_memory (enum machine_mode, rtx);
extern void ix86_free_from_memory (enum machine_mode);
extern void ix86_split_fp_branch (enum rtx_code code, rtx, rtx, rtx, rtx, rtx);

extern int ix86_hard_regno_mode_ok (int, enum machine_mode);
extern int ix86_register_move_cost (enum machine_mode, enum reg_class, enum reg_class);

extern int ix86_secondary_memory_needed (enum reg_class, enum reg_class, enum machine_mode, int);


extern enum reg_class ix86_preferred_reload_class (rtx, enum reg_class);

extern int ix86_memory_move_cost (enum machine_mode, enum reg_class, int);

extern void ix86_set_move_mem_attrs (rtx, rtx, rtx, rtx, rtx);
extern void emit_i387_cw_initialization (rtx, rtx);
extern _Bool ix86_fp_jump_nontrivial_p (enum rtx_code);
extern void x86_order_regs_for_local_alloc (void);



extern void init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx);
extern rtx function_arg (CUMULATIVE_ARGS *, enum machine_mode, tree, int);
extern void function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode, tree, int);

extern rtx ix86_function_value (tree);
extern void ix86_init_builtins (void);
extern rtx ix86_expand_builtin (tree, rtx, rtx, enum machine_mode, int);





extern int ix86_return_pops_args (tree, tree, int);
extern tree ix86_build_va_list (void);

extern int ix86_data_alignment (tree, int);
extern int ix86_local_alignment (tree, int);
extern int ix86_constant_alignment (tree, int);
extern tree ix86_handle_dll_attribute (tree *, tree, tree, int, _Bool *);
extern tree ix86_handle_shared_attribute (tree *, tree, tree, int, _Bool *);

extern unsigned int i386_pe_section_type_flags (tree, const char *, int);

extern void i386_pe_asm_named_section (const char *, unsigned int);
extern void x86_output_mi_thunk (FILE *, int, tree);
extern int x86_field_alignment (tree, int);
# 2 "tm_p.h" 2
# 1 "tm-preds.h" 1







extern int x86_64_immediate_operand (rtx, enum machine_mode);
extern int x86_64_nonmemory_operand (rtx, enum machine_mode);
extern int x86_64_movabs_operand (rtx, enum machine_mode);
extern int x86_64_szext_nonmemory_operand (rtx, enum machine_mode);
extern int x86_64_general_operand (rtx, enum machine_mode);
extern int x86_64_szext_general_operand (rtx, enum machine_mode);
extern int x86_64_zext_immediate_operand (rtx, enum machine_mode);
extern int shiftdi_operand (rtx, enum machine_mode);
extern int const_int_1_operand (rtx, enum machine_mode);
extern int const_int_1_31_operand (rtx, enum machine_mode);
extern int symbolic_operand (rtx, enum machine_mode);
extern int aligned_operand (rtx, enum machine_mode);
extern int pic_symbolic_operand (rtx, enum machine_mode);
extern int call_insn_operand (rtx, enum machine_mode);
extern int constant_call_address_operand (rtx, enum machine_mode);
extern int const0_operand (rtx, enum machine_mode);
extern int const1_operand (rtx, enum machine_mode);
extern int const248_operand (rtx, enum machine_mode);
extern int incdec_operand (rtx, enum machine_mode);
extern int mmx_reg_operand (rtx, enum machine_mode);
extern int reg_no_sp_operand (rtx, enum machine_mode);
extern int general_no_elim_operand (rtx, enum machine_mode);
extern int nonmemory_no_elim_operand (rtx, enum machine_mode);
extern int q_regs_operand (rtx, enum machine_mode);
extern int non_q_regs_operand (rtx, enum machine_mode);
extern int fcmov_comparison_operator (rtx, enum machine_mode);
extern int sse_comparison_operator (rtx, enum machine_mode);
extern int ix86_comparison_operator (rtx, enum machine_mode);
extern int cmp_fp_expander_operand (rtx, enum machine_mode);
extern int ext_register_operand (rtx, enum machine_mode);
extern int binary_fp_operator (rtx, enum machine_mode);
extern int mult_operator (rtx, enum machine_mode);
extern int div_operator (rtx, enum machine_mode);
extern int arith_or_logical_operator (rtx, enum machine_mode);
extern int promotable_binary_operator (rtx, enum machine_mode);
extern int memory_displacement_operand (rtx, enum machine_mode);
extern int cmpsi_operand (rtx, enum machine_mode);
extern int long_memory_operand (rtx, enum machine_mode);
# 3 "tm_p.h" 2
# 38 "c-typeck.c" 2
# 1 "flags.h" 1
# 26 "flags.h"
extern const char *main_input_filename;

enum debug_info_type
{
  NO_DEBUG,
  DBX_DEBUG,
  SDB_DEBUG,
  DWARF_DEBUG,
  DWARF2_DEBUG,
  XCOFF_DEBUG,
  VMS_DEBUG,
  VMS_AND_DWARF2_DEBUG

};


extern enum debug_info_type write_symbols;

enum debug_info_level
{
  DINFO_LEVEL_NONE,
  DINFO_LEVEL_TERSE,
  DINFO_LEVEL_NORMAL,
  DINFO_LEVEL_VERBOSE
};


extern enum debug_info_level debug_info_level;



extern int use_gnu_debug_info_extensions;



extern int optimize;



extern int optimize_size;




extern int quiet_flag;



extern int time_report;




extern int mem_report;



extern int inhibit_warnings;



extern int warn_system_headers;



extern int extra_warnings;





extern void set_Wunused (int setting);

extern int warn_unused_function;
extern int warn_unused_label;
extern int warn_unused_parameter;
extern int warn_unused_variable;
extern int warn_unused_value;



extern int warn_notreached;



extern int warn_inline;



extern int warn_uninitialized;







extern int warn_unknown_pragmas;



extern int warn_shadow;



extern int warn_switch;




extern int warn_return_type;



extern int warn_missing_noreturn;





extern int warn_cast_align;





extern int warn_larger_than;
extern long long larger_than_size;




extern int warn_aggregate_return;



extern int warn_packed;



extern int warn_padded;



extern int warn_disabled_optimization;




extern int warn_deprecated_decl;



extern int profile_flag;



extern int profile_arc_flag;



extern int flag_test_coverage;



extern int flag_branch_probabilities;



extern int flag_reorder_blocks;



extern int flag_rename_registers;




extern int pedantic;




extern int in_system_header;




extern int flag_print_asm_name;





extern int flag_signed_char;



extern int flag_short_enums;





extern int flag_caller_saves;



extern int flag_pcc_struct_return;




extern int flag_force_mem;




extern int flag_force_addr;




extern int flag_defer_pop;




extern int flag_float_store;



extern int flag_strength_reduce;






extern int flag_unroll_loops;




extern int flag_unroll_all_loops;




extern int flag_move_all_movables;



extern int flag_prefetch_loop_arrays;




extern int flag_reduce_all_givs;




extern int flag_cse_follow_jumps;




extern int flag_cse_skip_blocks;



extern int flag_expensive_optimizations;




extern int flag_writable_strings;





extern int flag_no_function_cse;




extern int flag_omit_frame_pointer;



extern int flag_no_peephole;



extern int flag_volatile;



extern int flag_volatile_global;



extern int flag_volatile_static;



extern int flag_optimize_sibling_calls;




extern int flag_errno_math;






extern int flag_unsafe_math_optimizations;





extern int flag_trapping_math;





extern int flag_complex_divide_method;



extern int flag_rerun_loop_opt;




extern int flag_inline_functions;





extern int flag_keep_inline_functions;







extern int flag_no_inline;




extern int flag_really_no_inline;



extern int flag_syntax_only;



extern int flag_gen_aux_info;



extern int flag_shared_data;






extern int flag_schedule_insns;
extern int flag_schedule_insns_after_reload;
# 418 "flags.h"
extern int flag_schedule_interblock;
extern int flag_schedule_speculative;
extern int flag_schedule_speculative_load;
extern int flag_schedule_speculative_load_dangerous;



extern int flag_branch_on_count_reg;





extern int flag_single_precision_constant;



extern int flag_delayed_branch;




extern int flag_dump_unnumbered;





extern int flag_pretend_float;




extern int flag_pedantic_errors;




extern int flag_pic;




extern int flag_exceptions;



extern int flag_unwind_tables;



extern int flag_asynchronous_unwind_tables;




extern int flag_no_common;





extern int flag_inhibit_size_directive;




extern int flag_function_sections;



extern int flag_data_sections;
# 498 "flags.h"
extern int flag_verbose_asm;
# 507 "flags.h"
extern int flag_debug_asm;

extern int flag_dump_rtl_in_asm;



extern int flag_gnu_linker;


extern int flag_pack_struct;
# 525 "flags.h"
extern int flag_argument_noalias;





extern int flag_strict_aliasing;



extern int flag_stack_check;


extern int flag_regmove;


extern int flag_instrument_function_entry_exit;


extern int flag_peephole2;


extern int flag_guess_branch_prob;






extern int flag_bounded_pointers;







extern int flag_bounds_check;




extern int flag_merge_constants;




extern int flag_renumber_insns;







extern int frame_pointer_needed;



extern int flag_trapv;


extern int g_switch_value;
extern int g_switch_set;






extern int align_loops;
extern int align_loops_log;
extern int align_loops_max_skip;
extern int align_jumps;
extern int align_jumps_log;
extern int align_jumps_max_skip;
extern int align_labels;
extern int align_labels_log;
extern int align_labels_max_skip;
extern int align_functions;
extern int align_functions_log;


extern int dump_for_graph;


enum graph_dump_types
{
  no_graph = 0,
  vcg
};
extern enum graph_dump_types graph_dump_format;





extern int flag_no_ident;



extern int flag_gcse_lm;



extern int flag_gcse_sm;




extern int flag_eliminate_dwarf2_dups;



extern int flag_detailed_statistics;


extern int flag_non_call_exceptions;
# 39 "c-typeck.c" 2
# 1 "output.h" 1
# 24 "output.h"
extern void compute_alignments (void);


extern void init_final (const char *);



extern void end_final (const char *);



extern void app_enable (void);



extern void app_disable (void);




extern int dbr_sequence_length (void);


extern void init_insn_lengths (void);




extern int get_attr_length (rtx);



extern void shorten_branches (rtx);






extern void final_start_function (rtx, FILE *, int);




extern void final_end_function (void);


extern void final (rtx, FILE *, int, int);




extern rtx final_scan_insn (rtx, FILE *, int, int, int);



extern rtx alter_subreg (rtx *);



extern void output_operand_lossage (const char *, ...) __attribute__ ((__format__ (__printf__, 1, 2)));



extern void output_asm_insn (const char *, rtx *);




extern int insn_current_reference_address (rtx);



extern int label_to_alignment (rtx);


extern void output_asm_label (rtx);



extern void output_address (rtx);




extern void output_addr_const (FILE *, rtx);



extern void asm_fprintf (FILE *file, const char *p, ...);



extern void split_double (rtx, rtx *, rtx *);


extern int leaf_function_p (void);




extern int final_forward_branch_p (rtx);



extern int only_leaf_regs_used (void);



extern void leaf_renumber_regs_insn (rtx);


extern const char *get_insn_template (int, rtx);



extern int add_weak (const char *, const char *);


extern void allocate_for_life_analysis (void);
extern int regno_uninitialized (unsigned int);
extern int regno_clobbered_at_setjmp (int);
extern void find_basic_blocks (rtx, int, FILE *);
extern _Bool cleanup_cfg (int);
extern void check_function_return_warnings (void);





extern void text_section (void);


extern void data_section (void);


extern void force_data_section (void);



extern void readonly_data_section (void);


extern int in_text_section (void);
# 178 "output.h"
extern void bss_section (void);



extern void const_section (void);



extern void init_section (void);



extern void fini_section (void);
# 218 "output.h"
extern void named_section (tree, const char *, int);


extern void function_section (tree);


extern void mergeable_string_section (tree, unsigned long long, unsigned int);



extern void mergeable_constant_section (enum machine_mode, unsigned long long, unsigned int);




extern void declare_weak (tree);

extern void merge_weak (tree, tree);



extern void weak_finish (void);
# 248 "output.h"
extern int decode_reg_name (const char *);




extern void make_var_volatile (tree);


extern void assemble_constant_align (tree);

extern void assemble_alias (tree, tree);



extern void assemble_asm (tree);





extern void assemble_start_function (tree, const char *);



extern void assemble_end_function (tree, const char *);
# 283 "output.h"
extern void assemble_variable (tree, int, int, int);




extern void assemble_external (tree);



extern void assemble_zeros (int);


extern void assemble_align (int);
extern void assemble_eh_align (int);


extern void assemble_string (const char *, int);



extern void assemble_external_libcall (rtx);



extern void assemble_global (const char *);


extern void assemble_label (const char *);
extern void assemble_eh_label (const char *);






extern void assemble_name (FILE *, const char *);
# 327 "output.h"
extern const char *integer_asm_op (int, int);




extern void assemble_integer_with_op (const char *, rtx);


extern _Bool default_assemble_integer (rtx, unsigned int, int);





extern _Bool assemble_integer (rtx, unsigned, unsigned, int);
# 351 "output.h"
extern void assemble_real (realvaluetype, enum machine_mode, unsigned);
# 360 "output.h"
extern void clear_const_double_mem (void);


extern void defer_addressed_constants (void);



extern void output_deferred_addressed_constants (void);


extern int get_pool_size (void);







extern void output_constant_pool (const char *, tree);
# 389 "output.h"
extern tree initializer_constant_valid_p (tree, tree);
# 399 "output.h"
extern void output_constant (tree, long long, unsigned int);
# 410 "output.h"
extern rtx final_sequence;
# 423 "output.h"
extern FILE *asm_out_file;



extern const char *first_global_object_name;


extern const char *weak_global_object_name;






extern int current_function_is_leaf;




extern int current_function_nothrow;





extern int current_function_sp_is_unchanging;





extern int current_function_uses_only_leaf_regs;




extern FILE *rtl_dump_file;



extern struct rtx_def *current_insn_predicate;


extern struct rtx_def *current_output_insn;
# 480 "output.h"
extern const char *user_label_prefix;
# 491 "output.h"
extern void default_function_pro_epilogue (FILE *, long long);


extern void default_exception_section (void);


extern void default_eh_frame_section (void);


extern void no_asm_to_stream (FILE *);
# 517 "output.h"
extern unsigned int get_named_section_flags (const char *);
extern _Bool set_named_section_flags (const char *, unsigned int);
extern void named_section_flags (const char *, unsigned int);
extern _Bool named_section_first_declaration (const char *);

union tree_node;
extern unsigned int default_section_type_flags (union tree_node *, const char *, int);


extern void default_no_named_section (const char *, unsigned int);
extern void default_elf_asm_named_section (const char *, unsigned int);
extern void default_coff_asm_named_section (const char *, unsigned int);

extern void default_pe_asm_named_section (const char *, unsigned int);

extern void default_stabs_asm_out_destructor (struct rtx_def *, int);
extern void default_named_section_asm_out_destructor (struct rtx_def *, int);

extern void default_dtor_section_asm_out_destructor (struct rtx_def *, int);

extern void default_stabs_asm_out_constructor (struct rtx_def *, int);
extern void default_named_section_asm_out_constructor (struct rtx_def *, int);

extern void default_ctor_section_asm_out_constructor (struct rtx_def *, int);



extern void assemble_vtable_entry (struct rtx_def *, long long);
extern void assemble_vtable_inherit (struct rtx_def *, struct rtx_def *);
# 40 "c-typeck.c" 2
# 1 "expr.h" 1
# 52 "expr.h"
enum expand_modifier {EXPAND_NORMAL, EXPAND_SUM, EXPAND_CONST_ADDRESS,
                        EXPAND_INITIALIZER, EXPAND_WRITE};
# 69 "expr.h"
struct args_size
{
  long long constant;
  tree var;
};
# 119 "expr.h"
enum direction {none, upward, downward};
# 256 "expr.h"
enum optab_methods
{
  OPTAB_DIRECT,
  OPTAB_LIB,
  OPTAB_WIDEN,
  OPTAB_LIB_WIDEN,
  OPTAB_MUST_WIDEN
};




extern rtx expand_simple_binop (enum machine_mode, enum rtx_code, rtx, rtx, rtx, int, enum optab_methods);

extern rtx expand_simple_unop (enum machine_mode, enum rtx_code, rtx, rtx, int);




extern int have_insn_for (enum rtx_code, enum machine_mode);


extern void emit_libcall_block (rtx, rtx, rtx, rtx);





extern rtx gen_add2_insn (rtx, rtx);
extern rtx gen_add3_insn (rtx, rtx, rtx);
extern rtx gen_sub2_insn (rtx, rtx);
extern rtx gen_sub3_insn (rtx, rtx, rtx);
extern rtx gen_move_insn (rtx, rtx);
extern int have_add2_insn (rtx, rtx);
extern int have_sub2_insn (rtx, rtx);



extern void emit_cmp_and_jump_insns (rtx, rtx, enum rtx_code, rtx, enum machine_mode, int, rtx);



extern void emit_indirect_jump (rtx);
# 316 "expr.h"
extern rtx negate_rtx (enum machine_mode, rtx);


extern rtx expand_and (enum machine_mode, rtx, rtx, rtx);


extern rtx emit_store_flag (rtx, enum rtx_code, rtx, rtx, enum machine_mode, int, int);



extern rtx emit_store_flag_force (rtx, enum rtx_code, rtx, rtx, enum machine_mode, int, int);






extern rtx canonicalize_condition (rtx, rtx, int, rtx *, rtx);



extern rtx get_condition (rtx, rtx *);


extern rtx gen_cond_trap (enum rtx_code, rtx, rtx, rtx);


extern rtx expand_builtin (tree, rtx, rtx, enum machine_mode, int);
extern void std_expand_builtin_va_start (int, tree, rtx);
extern rtx std_expand_builtin_va_arg (tree, tree);
extern rtx expand_builtin_va_arg (tree, tree);
extern void default_init_builtins (void);
extern rtx default_expand_builtin (tree, rtx, rtx, enum machine_mode, int);

extern void expand_builtin_setjmp_setup (rtx, rtx);
extern void expand_builtin_setjmp_receiver (rtx);
extern void expand_builtin_longjmp (rtx, rtx);
extern rtx expand_builtin_saveregs (void);
extern void expand_builtin_trap (void);
extern long long get_varargs_alias_set (void);
extern long long get_frame_alias_set (void);
extern void record_base_value (unsigned int, rtx, int);
extern void record_alias_subset (long long, long long);

extern long long new_alias_set (void);
extern int can_address_p (tree);





extern void init_expr_once (void);


extern void init_expr (void);



extern void init_stor_layout_once (void);


extern void finish_expr_for_function (void);



extern rtx protect_from_queue (rtx, int);


extern void emit_queue (void);


extern int queued_subexp_p (rtx);



extern void convert_move (rtx, rtx, int);


extern rtx convert_to_mode (enum machine_mode, rtx, int);


extern rtx convert_modes (enum machine_mode, enum machine_mode, rtx, int);



extern rtx emit_block_move (rtx, rtx, rtx);



extern void move_block_to_reg (int, rtx, int, enum machine_mode);



extern void move_block_from_reg (int, rtx, int, int);



extern void emit_group_load (rtx, rtx, int);



extern void emit_group_store (rtx, rtx, int);



extern rtx copy_blkmode_from_reg (rtx,rtx,tree);



extern void use_reg (rtx *, rtx);



extern void use_regs (rtx *, int, int);


extern void use_group_regs (rtx *, rtx);



extern rtx clear_storage (rtx, rtx);






extern int can_store_by_pieces (unsigned long long, rtx (*) (void *, long long, enum machine_mode), void *, unsigned int);
# 452 "expr.h"
extern void store_by_pieces (rtx, unsigned long long, rtx (*) (void *, long long, enum machine_mode), void *, unsigned int);





extern rtx emit_move_insn (rtx, rtx);


extern rtx emit_move_insn_1 (rtx, rtx);



extern rtx push_block (rtx, int, int);



extern void emit_push_insn (rtx, enum machine_mode, tree, rtx, unsigned int, int, rtx, int, rtx, rtx, int, rtx);




extern rtx expand_assignment (tree, tree, int, int);





extern rtx store_expr (tree, rtx, int);





extern rtx force_operand (rtx, rtx);
# 495 "expr.h"
extern tree find_placeholder (tree, tree *);




extern rtx expand_expr (tree, rtx, enum machine_mode, enum expand_modifier);




extern void init_pending_stack_adjust (void);



extern void clear_pending_stack_adjust (void);


extern void do_pending_stack_adjust (void);




extern tree string_constant (tree, tree *);


extern void jumpifnot (tree, rtx);


extern void jumpif (tree, rtx);



extern void do_jump (tree, rtx, rtx);



extern rtx compare_from_rtx (rtx, rtx, enum rtx_code, int, enum machine_mode, rtx);

extern void do_compare_rtx_and_jump (rtx, rtx, enum rtx_code, int, enum machine_mode, rtx, rtx, rtx);




extern int try_casesi (tree, tree, tree, tree, rtx, rtx);
extern int try_tablejump (tree, tree, tree, tree, rtx, rtx);



extern unsigned int case_values_threshold (void);





extern rtx expr_size (tree);

extern rtx lookup_static_chain (tree);



extern rtx fix_lexical_addr (rtx, tree);


extern rtx trampoline_address (tree);



extern rtx hard_function_value (tree, tree, int);

extern rtx prepare_call_address (rtx, tree, rtx *, int, int);

extern rtx expand_call (tree, rtx, int);

extern rtx expand_shift (enum tree_code, enum machine_mode, rtx, tree, rtx, int);

extern rtx expand_divmod (int, enum tree_code, enum machine_mode, rtx, rtx, rtx, int);

extern void locate_and_pad_parm (enum machine_mode, tree, int, tree, struct args_size *, struct args_size *, struct args_size *, struct args_size *);




extern rtx expand_inline_function (tree, tree, rtx, int, tree, rtx);


extern rtx label_rtx (tree);



extern rtx promoted_input_arg (unsigned int, enum machine_mode *, int *);





extern rtx eliminate_constant_term (rtx, rtx *);



extern rtx memory_address (enum machine_mode, rtx);


extern rtx memory_address_noforce (enum machine_mode, rtx);


extern void set_mem_alias_set (rtx, long long);


extern void set_mem_align (rtx, unsigned int);


extern void set_mem_expr (rtx, tree);


extern void set_mem_offset (rtx, rtx);





extern rtx change_address (rtx, enum machine_mode, rtx);
# 636 "expr.h"
extern rtx adjust_address_1 (rtx, enum machine_mode, long long, int, int);

extern rtx adjust_automodify_address_1 (rtx, enum machine_mode, rtx, long long, int);





extern rtx offset_address (rtx, rtx, long long);





extern rtx replace_equiv_address (rtx, rtx);


extern rtx replace_equiv_address_nv (rtx, rtx);



extern rtx widen_memory_access (rtx, enum machine_mode, long long);



extern rtx validize_mem (rtx);





extern void maybe_set_unchanging (rtx, tree);




extern void set_mem_attributes (rtx, tree, int);



extern rtx assemble_trampoline_template (void);



extern rtx stabilize (rtx);



extern rtx copy_all_regs (rtx);


extern rtx copy_to_reg (rtx);


extern rtx copy_addr_to_reg (rtx);


extern rtx copy_to_mode_reg (enum machine_mode, rtx);


extern rtx copy_to_suggested_reg (rtx, rtx, enum machine_mode);



extern rtx force_reg (enum machine_mode, rtx);


extern rtx force_not_mem (rtx);



extern enum machine_mode promote_mode (tree, enum machine_mode, int *, int);




extern void adjust_stack (rtx);


extern void anti_adjust_stack (rtx);


enum save_level {SAVE_BLOCK, SAVE_FUNCTION, SAVE_NONLOCAL};


extern void emit_stack_save (enum save_level, rtx *, rtx);


extern void emit_stack_restore (enum save_level, rtx, rtx);



extern rtx allocate_dynamic_stack_space (rtx, rtx, int);






extern void probe_stack_range (long long, rtx);



extern rtx hard_libcall_value (enum machine_mode);



extern rtx round_push (rtx);





enum extraction_pattern { EP_insv, EP_extv, EP_extzv };
extern enum machine_mode
mode_for_extraction (enum extraction_pattern, int);

extern rtx store_bit_field (rtx, unsigned long long, unsigned long long, enum machine_mode, rtx, long long);


extern rtx extract_bit_field (rtx, unsigned long long, unsigned long long, int, rtx, enum machine_mode, enum machine_mode, long long);



extern rtx expand_mult (enum machine_mode, rtx, rtx, rtx, int);
extern rtx expand_mult_add (rtx, rtx, rtx, rtx,enum machine_mode, int);
extern rtx expand_mult_highpart_adjust (enum machine_mode, rtx, rtx, rtx, rtx, int);

extern rtx assemble_static_space (int);




extern rtx (*lang_expand_expr) (union tree_node *, rtx, enum machine_mode, enum expand_modifier modifier);



extern int safe_from_p (rtx, tree, int);



extern void init_optabs (void);
extern void init_all_optabs (void);


extern rtx init_one_libfunc (const char *);

extern void do_jump_by_parts_equality_rtx (rtx, rtx, rtx);
extern void do_jump_by_parts_greater_rtx (enum machine_mode, int, rtx, rtx, rtx, rtx);




extern void mark_seen_cases (tree, unsigned char *, long long, int);
# 41 "c-typeck.c" 2
# 1 "toplev.h" 1
# 29 "toplev.h"
extern int toplev_main (int, char **);
extern int read_integral_parameter (const char *, const char *, const int);

extern int count_error (int);
extern void strip_off_ending (char *, int);
extern void print_time (const char *, long);
extern const char *trim_filename (const char *);
extern void internal_error (const char *, ...)
                                               __attribute__ ((__noreturn__));
extern void fatal_io_error (const char *, ...)
                                               __attribute__ ((__noreturn__));
extern void _fatal_insn_not_found (struct rtx_def *, const char *, int, const char *)


                                               __attribute__ ((__noreturn__));
extern void _fatal_insn (const char *, struct rtx_def *, const char *, int, const char *)



                                              __attribute__ ((__noreturn__));
# 58 "toplev.h"
extern void warning (const char *, ...);
extern void error (const char *, ...);
extern void fatal_error (const char *, ...)
                                               __attribute__ ((__noreturn__));
extern void pedwarn (const char *, ...);
extern void pedwarn_with_file_and_line (const char *, int, const char *, ...);

extern void warning_with_file_and_line (const char *, int, const char *, ...);

extern void error_with_file_and_line (const char *, int, const char *, ...);

extern void sorry (const char *, ...);
extern void report_error_function (const char *);

extern void rest_of_decl_compilation (union tree_node *, const char *, int, int);

extern void rest_of_type_compilation (union tree_node *, int);
extern void rest_of_compilation (union tree_node *);

extern void pedwarn_with_decl (union tree_node *, const char *, ...);

extern void warning_with_decl (union tree_node *, const char *, ...);

extern void error_with_decl (union tree_node *, const char *, ...);


extern void announce_function (union tree_node *);

extern void error_for_asm (struct rtx_def *, const char *, ...);

extern void warning_for_asm (struct rtx_def *, const char *, ...);

extern void warn_deprecated_use (union tree_node *);
extern int do_float_handler (void (*) (void *), void *);


extern void output_quoted_string (FILE *, const char *);
extern void output_file_directive (FILE *, const char *);

extern void do_abort (void) __attribute__ ((__noreturn__));
extern void botch (const char *)
  __attribute__ ((__noreturn__));




extern void fnotice (FILE *, const char *, ...)
                                               __attribute__ ((__format__ (__printf__, 2, 3)));


extern int wrapup_global_declarations (union tree_node **, int);
extern void check_global_declarations (union tree_node **, int);

extern const char *progname;
extern const char *dump_base_name;


extern struct ht *ident_hash;




extern void set_fast_math_flags (void);
extern void set_no_fast_math_flags (void);
# 130 "toplev.h"
extern int exact_log2_wide (unsigned long long);
extern int floor_log2_wide (unsigned long long);
# 42 "c-typeck.c" 2
# 1 "intl.h" 1
# 23 "intl.h"
# 1 "/usr/include/locale.h" 1 3 4
# 29 "/usr/include/locale.h" 3 4
# 1 "/usr/lib64/gcc-lib/x86_64-suse-linux/3.3.4/include/stddef.h" 1 3 4
# 30 "/usr/include/locale.h" 2 3 4
# 1 "/usr/include/bits/locale.h" 1 3 4
# 27 "/usr/include/bits/locale.h" 3 4
enum
{
  __LC_CTYPE = 0,
  __LC_NUMERIC = 1,
  __LC_TIME = 2,
  __LC_COLLATE = 3,
  __LC_MONETARY = 4,
  __LC_MESSAGES = 5,
  __LC_ALL = 6,
  __LC_PAPER = 7,
  __LC_NAME = 8,
  __LC_ADDRESS = 9,
  __LC_TELEPHONE = 10,
  __LC_MEASUREMENT = 11,
  __LC_IDENTIFICATION = 12
};
# 31 "/usr/include/locale.h" 2 3 4


# 52 "/usr/include/locale.h" 3 4
struct lconv
{


  char *decimal_point;
  char *thousands_sep;





  char *grouping;





  char *int_curr_symbol;
  char *currency_symbol;
  char *mon_decimal_point;
  char *mon_thousands_sep;
  char *mon_grouping;
  char *positive_sign;
  char *negative_sign;
  char int_frac_digits;
  char frac_digits;

  char p_cs_precedes;

  char p_sep_by_space;

  char n_cs_precedes;

  char n_sep_by_space;






  char p_sign_posn;
  char n_sign_posn;


  char int_p_cs_precedes;

  char int_p_sep_by_space;

  char int_n_cs_precedes;

  char int_n_sep_by_space;






  char int_p_sign_posn;
  char int_n_sign_posn;
# 119 "/usr/include/locale.h" 3 4
};





extern char *setlocale (int __category, __const char *__locale) __attribute__ ((__nothrow__));


extern struct lconv *localeconv (void) __attribute__ ((__nothrow__));


# 148 "/usr/include/locale.h" 3 4
typedef __locale_t locale_t;





extern __locale_t newlocale (int __category_mask, __const char *__locale,
                             __locale_t __base) __attribute__ ((__nothrow__));
# 189 "/usr/include/locale.h" 3 4
extern __locale_t duplocale (__locale_t __dataset) __attribute__ ((__nothrow__));



extern void freelocale (__locale_t __dataset) __attribute__ ((__nothrow__));






extern __locale_t uselocale (__locale_t __dataset) __attribute__ ((__nothrow__));








# 24 "intl.h" 2
# 43 "c-typeck.c" 2
# 1 "ggc.h" 1
# 21 "ggc.h"
# 1 "varray.h" 1
# 36 "varray.h"
struct const_equiv_data {
# 50 "varray.h"
  struct rtx_def *rtx;



  unsigned age;
};


typedef union varray_data_tag {
  char c[1];
  unsigned char uc[1];
  short s[1];
  unsigned short us[1];
  int i[1];
  unsigned int u[1];
  long l[1];
  unsigned long ul[1];
  long long hint[1];
  unsigned long long uhint[1];
  void * generic[1];
  char *cptr[1];
  struct rtx_def *rtx[1];
  struct rtvec_def *rtvec[1];
  union tree_node *tree[1];
  struct bitmap_head_def *bitmap[1];
  struct sched_info_tag *sched[1];
  struct reg_info_def *reg[1];
  struct const_equiv_data const_equiv[1];
  struct basic_block_def *bb[1];
  struct elt_list *te[1];
} varray_data;


typedef struct varray_head_tag {
  size_t num_elements;
  size_t elements_used;

  size_t element_size;
  const char *name;
  varray_data data;
} *varray_type;



extern varray_type varray_init (size_t, size_t, const char *);
# 165 "varray.h"
extern varray_type varray_grow (varray_type, size_t);
# 22 "ggc.h" 2
# 30 "ggc.h"
struct eh_status;
struct emit_status;
struct expr_status;
struct hash_table;
struct label_node;
struct rtx_def;
struct rtvec_def;
struct stmt_status;
union tree_node;
struct varasm_status;


extern const char empty_string[];
extern const char digit_vector[];



extern varray_type ggc_pending_trees;


extern void ggc_add_root (void *base, int nelt, int size, void (*)(void *));

extern void ggc_add_rtx_root (struct rtx_def **, int nelt);
extern void ggc_add_tree_root (union tree_node **, int nelt);

extern void ggc_add_rtx_varray_root (struct varray_head_tag **, int nelt);

extern void ggc_add_tree_varray_root (struct varray_head_tag **, int nelt);

extern void ggc_add_tree_hash_table_root (struct hash_table **, int nelt);

extern void ggc_del_root (void *base);



typedef int (*ggc_htab_marked_p) (const void *);
typedef void (*ggc_htab_mark) (const void *);




extern void ggc_add_deletable_htab (void *, ggc_htab_marked_p, ggc_htab_mark);




extern void ggc_mark_rtx_varray (struct varray_head_tag *);
extern void ggc_mark_tree_varray (struct varray_head_tag *);
extern void ggc_mark_tree_hash_table (struct hash_table *);
extern void ggc_mark_roots (void);

extern void ggc_mark_rtx_children (struct rtx_def *);
extern void ggc_mark_rtvec_children (struct rtvec_def *);
# 127 "ggc.h"
extern void init_ggc (void);
extern void init_stringpool (void);



extern void ggc_push_context (void);



extern void ggc_pop_context (void);




extern void *ggc_alloc (size_t);

extern void *ggc_alloc_cleared (size_t);
# 158 "ggc.h"
extern const char *ggc_alloc_string (const char *contents, int length);







extern void ggc_collect (void);






extern int ggc_set_mark (const void *);




extern int ggc_marked_p (const void *);





extern void lang_mark_tree (union tree_node *);





extern void (*lang_mark_false_label_stack) (struct label_node *);



void mark_eh_status (struct eh_status *);
void mark_emit_status (struct emit_status *);
void mark_expr_status (struct expr_status *);
void mark_stmt_status (struct stmt_status *);
void mark_varasm_status (struct varasm_status *);
void mark_optab (void *);





typedef struct ggc_statistics
{

  unsigned num_trees[256];


  size_t size_trees[256];

  unsigned num_rtxs[256];


  size_t size_rtxs[256];

  size_t total_size_trees;

  size_t total_size_rtxs;

  unsigned total_num_trees;

  unsigned total_num_rtxs;
} ggc_statistics;


extern size_t ggc_get_size (const void *);



extern void ggc_print_common_statistics (FILE *, ggc_statistics *);


extern void ggc_print_statistics (void);
extern void stringpool_statistics (void);
# 44 "c-typeck.c" 2
# 1 "target.h" 1
# 47 "target.h"
struct gcc_target
{

  struct asm_out
  {

    const char *open_paren, *close_paren;


    const char *byte_op;
    struct asm_int_op
    {
      const char *hi;
      const char *si;
      const char *di;
      const char *ti;
    } aligned_op, unaligned_op;






    _Bool (* integer) (rtx x, unsigned int size, int aligned_p);


    void (* function_prologue) (FILE *, long long);


    void (* function_end_prologue) (FILE *);


    void (* function_begin_epilogue) (FILE *);


    void (* function_epilogue) (FILE *, long long);



    void (* named_section) (const char *, unsigned int);


    void (* exception_section) (void);


    void (* eh_frame_section) (void);


    void (* constructor) (rtx, int);


    void (* destructor) (rtx, int);
  } asm_out;


  struct sched
  {



    int (* adjust_cost) (rtx insn, rtx link, rtx def_insn, int cost);



    int (* adjust_priority) (rtx, int);




    int (* issue_rate) (void);



    int (* variable_issue) (FILE *, int, rtx, int);


    void (* md_init) (FILE *, int, int);


    void (* md_finish) (FILE *, int);



    int (* reorder) (FILE *, int, rtx *, int *, int);
    int (* reorder2) (FILE *, int, rtx *, int *, int);






    rtx (* cycle_display) (int clock, rtx last);
  } sched;


  tree (* merge_decl_attributes) (tree, tree);


  tree (* merge_type_attributes) (tree, tree);


  const struct attribute_spec *attribute_table;




  int (* comp_type_attributes) (tree type1, tree type2);


  void (* set_default_type_attributes) (tree type);


  void (* insert_attributes) (tree decl, tree *attributes);



  _Bool (* function_attribute_inlinable_p) (tree fndecl);



  _Bool (* ms_bitfield_layout_p) (tree record_type);


  void (* init_builtins) (void);


  rtx (* expand_builtin) (tree exp, rtx target, rtx subtarget, enum machine_mode mode, int ignore);





  unsigned int (* section_type_flags) (tree, const char *, int);


  _Bool have_named_sections;



  _Bool have_ctors_dtors;



  _Bool (* cannot_modify_jumps_p) (void);
};

extern struct gcc_target targetm;
# 45 "c-typeck.c" 2



static int missing_braces_mentioned;


static int undeclared_variable_notice;

static tree qualify_type (tree, tree);
static int comp_target_types (tree, tree);
static int function_types_compatible_p (tree, tree);
static int type_lists_compatible_p (tree, tree);
static tree decl_constant_value_for_broken_optimization (tree);
static tree default_function_array_conversion (tree);
static tree lookup_field (tree, tree);
static tree convert_arguments (tree, tree, tree, tree);
static tree pointer_diff (tree, tree);
static tree unary_complex_lvalue (enum tree_code, tree, int);
static void pedantic_lvalue_warning (enum tree_code);
static tree internal_build_compound_expr (tree, int);
static tree convert_for_assignment (tree, tree, const char *, tree, tree, int);

static void warn_for_assignment (const char *, const char *, tree, int);

static tree valid_compound_expr_initializer (tree, tree);
static void push_string (const char *);
static void push_member_name (tree);
static void push_array_bounds (int);
static int spelling_length (void);
static char *print_spelling (char *);
static void warning_init (const char *);
static tree digest_init (tree, tree, int, int);
static void output_init_element (tree, tree, tree, int);
static void output_pending_init_elements (int);
static int set_designator (int);
static void push_range_stack (tree);
static void add_pending_init (tree, tree);
static void set_nonincremental_init (void);
static void set_nonincremental_init_from_string (tree);
static tree find_init_member (tree);




tree
require_complete_type (value)
     tree value;
{
  tree type = ((value)->common.type);

  if (value == global_trees[TI_ERROR_MARK] || type == global_trees[TI_ERROR_MARK])
    return global_trees[TI_ERROR_MARK];


  if ((((type)->type.size) != (tree) ((void *)0)))
    return value;

  incomplete_type_error (value, type);
  return global_trees[TI_ERROR_MARK];
}





void
incomplete_type_error (value, type)
     tree value;
     tree type;
{
  const char *type_code_string;


  if (((enum tree_code) (type)->common.code) == ERROR_MARK)
    return;

  if (value != 0 && (((enum tree_code) (value)->common.code) == VAR_DECL
                     || ((enum tree_code) (value)->common.code) == PARM_DECL))
    error ("`%s' has an incomplete type",
           ((const char *) (((value)->decl.name))->identifier.id.str));
  else
    {
    retry:


      switch (((enum tree_code) (type)->common.code))
        {
        case RECORD_TYPE:
          type_code_string = "struct";
          break;

        case UNION_TYPE:
          type_code_string = "union";
          break;

        case ENUMERAL_TYPE:
          type_code_string = "enum";
          break;

        case VOID_TYPE:
          error ("invalid use of void expression");
          return;

        case ARRAY_TYPE:
          if (((type)->type.values))
            {
              if (((((type)->type.values))->type.maxval) == ((void *)0))
                {
                  error ("invalid use of flexible array member");
                  return;
                }
              type = ((type)->common.type);
              goto retry;
            }
          error ("invalid use of array with unspecified bounds");
          return;

        default:
          fancy_abort ("c-typeck.c", 163, __FUNCTION__);
        }

      if (((enum tree_code) (((type)->type.name))->common.code) == IDENTIFIER_NODE)
        error ("invalid use of undefined type `%s %s'",
               type_code_string, ((const char *) (((type)->type.name))->identifier.id.str));
      else

        error ("invalid use of incomplete typedef `%s'",
               ((const char *) (((((type)->type.name))->decl.name))->identifier.id.str));
    }
}




static tree
qualify_type (type, like)
     tree type, like;
{
  return c_build_qualified_type (type,
                                 ((((type)->common.readonly_flag) * 0x1) | (((type)->common.volatile_flag) * 0x2) | (((type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (type)->common.code) == RECORD_TYPE && ((type)->common.type)) * 0x8)) | ((((like)->common.readonly_flag) * 0x1) | (((like)->common.volatile_flag) * 0x2) | (((like)->type.restrict_flag) * 0x4) | ((((enum tree_code) (like)->common.code) == RECORD_TYPE && ((like)->common.type)) * 0x8)));
}
# 195 "c-typeck.c"
tree
common_type (t1, t2)
     tree t1, t2;
{
  enum tree_code code1;
  enum tree_code code2;
  tree attributes;



  if (t1 == t2) return t1;


  if (t1 == global_trees[TI_ERROR_MARK])
    return t2;
  if (t2 == global_trees[TI_ERROR_MARK])
    return t1;


  attributes = (*targetm.merge_type_attributes) (t1, t2);



  if (((enum tree_code) (t1)->common.code) == ENUMERAL_TYPE)
    t1 = type_for_size (((t1)->type.precision), 1);
  if (((enum tree_code) (t2)->common.code) == ENUMERAL_TYPE)
    t2 = type_for_size (((t2)->type.precision), 1);

  code1 = ((enum tree_code) (t1)->common.code);
  code2 = ((enum tree_code) (t2)->common.code);




  if (code1 == COMPLEX_TYPE || code2 == COMPLEX_TYPE)
    {
      tree subtype1 = code1 == COMPLEX_TYPE ? ((t1)->common.type) : t1;
      tree subtype2 = code2 == COMPLEX_TYPE ? ((t2)->common.type) : t2;
      tree subtype = common_type (subtype1, subtype2);

      if (code1 == COMPLEX_TYPE && ((t1)->common.type) == subtype)
        return build_type_attribute_variant (t1, attributes);
      else if (code2 == COMPLEX_TYPE && ((t2)->common.type) == subtype)
        return build_type_attribute_variant (t2, attributes);
      else
        return build_type_attribute_variant (build_complex_type (subtype),
                                             attributes);
    }

  switch (code1)
    {
    case INTEGER_TYPE:
    case REAL_TYPE:


      if (code1 == REAL_TYPE && code2 != REAL_TYPE)
        return build_type_attribute_variant (t1, attributes);

      if (code2 == REAL_TYPE && code1 != REAL_TYPE)
        return build_type_attribute_variant (t2, attributes);



      if (((t1)->type.precision) > ((t2)->type.precision))
        return build_type_attribute_variant (t1, attributes);
      else if (((t2)->type.precision) > ((t1)->type.precision))
        return build_type_attribute_variant (t2, attributes);



      if (((t1)->type.main_variant) == integer_types[itk_unsigned_long]
          || ((t2)->type.main_variant) == integer_types[itk_unsigned_long])
        return build_type_attribute_variant (integer_types[itk_unsigned_long],
                                             attributes);

      if (((t1)->type.main_variant) == integer_types[itk_long]
          || ((t2)->type.main_variant) == integer_types[itk_long])
        {


          if (((t1)->common.unsigned_flag) || ((t2)->common.unsigned_flag))
             t1 = integer_types[itk_unsigned_long];
          else
             t1 = integer_types[itk_long];
          return build_type_attribute_variant (t1, attributes);
        }


      if (((t1)->type.main_variant) == global_trees[TI_LONG_DOUBLE_TYPE]
          || ((t2)->type.main_variant) == global_trees[TI_LONG_DOUBLE_TYPE])
        return build_type_attribute_variant (global_trees[TI_LONG_DOUBLE_TYPE],
                                             attributes);



      if (((t1)->common.unsigned_flag))
        return build_type_attribute_variant (t1, attributes);
      else
        return build_type_attribute_variant (t2, attributes);

    case POINTER_TYPE:





      {
        tree pointed_to_1 = ((t1)->common.type);
        tree pointed_to_2 = ((t2)->common.type);
        tree target = common_type (((pointed_to_1)->type.main_variant),
                                   ((pointed_to_2)->type.main_variant));
        t1 = build_pointer_type (c_build_qualified_type
                                 (target,
                                  ((((pointed_to_1)->common.readonly_flag) * 0x1) | (((pointed_to_1)->common.volatile_flag) * 0x2) | (((pointed_to_1)->type.restrict_flag) * 0x4) | ((((enum tree_code) (pointed_to_1)->common.code) == RECORD_TYPE && ((pointed_to_1)->common.type)) * 0x8)) |
                                  ((((pointed_to_2)->common.readonly_flag) * 0x1) | (((pointed_to_2)->common.volatile_flag) * 0x2) | (((pointed_to_2)->type.restrict_flag) * 0x4) | ((((enum tree_code) (pointed_to_2)->common.code) == RECORD_TYPE && ((pointed_to_2)->common.type)) * 0x8))));
        return build_type_attribute_variant (t1, attributes);
      }





    case ARRAY_TYPE:
      {
        tree elt = common_type (((t1)->common.type), ((t2)->common.type));

        if (elt == ((t1)->common.type) && ((t1)->type.values))
          return build_type_attribute_variant (t1, attributes);
        if (elt == ((t2)->common.type) && ((t2)->type.values))
          return build_type_attribute_variant (t2, attributes);

        t1 = build_array_type (elt, ((((t1)->type.values) ? t1 : t2)->type.values));
        return build_type_attribute_variant (t1, attributes);
      }

    case FUNCTION_TYPE:


      {
        tree valtype = common_type (((t1)->common.type), ((t2)->common.type));
        tree p1 = ((t1)->type.values);
        tree p2 = ((t2)->type.values);
        int len;
        tree newargs, n;
        int i;


        if (valtype == ((t1)->common.type) && ! ((t2)->type.values))
          return build_type_attribute_variant (t1, attributes);
        if (valtype == ((t2)->common.type) && ! ((t1)->type.values))
          return build_type_attribute_variant (t2, attributes);


        if (((t1)->type.values) == 0)
         {
           t1 = build_function_type (valtype, ((t2)->type.values));
           return build_type_attribute_variant (t1, attributes);
         }
        if (((t2)->type.values) == 0)
         {
           t1 = build_function_type (valtype, ((t1)->type.values));
           return build_type_attribute_variant (t1, attributes);
         }




        pushlevel (0);
        declare_parm_level (1);

        len = list_length (p1);
        newargs = 0;

        for (i = 0; i < len; i++)
          newargs = tree_cons ((tree) ((void *)0), (tree) ((void *)0), newargs);

        n = newargs;

        for (; p1;
             p1 = ((p1)->common.chain), p2 = ((p2)->common.chain), n = ((n)->common.chain))
          {


            if (((p1)->list.value) == 0)
              {
                ((n)->list.value) = ((p2)->list.value);
                goto parm_done;
              }
            if (((p2)->list.value) == 0)
              {
                ((n)->list.value) = ((p1)->list.value);
                goto parm_done;
              }




            if (((enum tree_code) (((p1)->list.value))->common.code) == UNION_TYPE
                && ((p1)->list.value) != ((p2)->list.value))
              {
                tree memb;
                for (memb = ((((p1)->list.value))->type.values);
                     memb; memb = ((memb)->common.chain))
                  if (comptypes (((memb)->common.type), ((p2)->list.value)))
                    {
                      ((n)->list.value) = ((p2)->list.value);
                      if (pedantic)
                        pedwarn ("function types not truly compatible in ISO C");
                      goto parm_done;
                    }
              }
            if (((enum tree_code) (((p2)->list.value))->common.code) == UNION_TYPE
                && ((p2)->list.value) != ((p1)->list.value))
              {
                tree memb;
                for (memb = ((((p2)->list.value))->type.values);
                     memb; memb = ((memb)->common.chain))
                  if (comptypes (((memb)->common.type), ((p1)->list.value)))
                    {
                      ((n)->list.value) = ((p1)->list.value);
                      if (pedantic)
                        pedwarn ("function types not truly compatible in ISO C");
                      goto parm_done;
                    }
              }
            ((n)->list.value) = common_type (((p1)->list.value), ((p2)->list.value));
          parm_done: ;
          }

        poplevel (0, 0, 0);

        t1 = build_function_type (valtype, newargs);

      }

    default:
      return build_type_attribute_variant (t1, attributes);
    }

}





int
comptypes (type1, type2)
     tree type1, type2;
{
  tree t1 = type1;
  tree t2 = type2;
  int attrval, val;



  if (t1 == t2 || !t1 || !t2
      || ((enum tree_code) (t1)->common.code) == ERROR_MARK || ((enum tree_code) (t2)->common.code) == ERROR_MARK)
    return 1;



  if (((enum tree_code) (t1)->common.code) == INTEGER_TYPE && ((t1)->type.no_force_blk_flag)
      && ((t1)->type.values) != 0)
    t1 = ((t1)->type.values);

  if (((enum tree_code) (t2)->common.code) == INTEGER_TYPE && ((t2)->type.no_force_blk_flag)
      && ((t2)->type.values) != 0)
    t2 = ((t2)->type.values);




  if (((enum tree_code) (t1)->common.code) == ENUMERAL_TYPE)
    t1 = type_for_size (((t1)->type.precision), ((t1)->common.unsigned_flag));
  if (((enum tree_code) (t2)->common.code) == ENUMERAL_TYPE)
    t2 = type_for_size (((t2)->type.precision), ((t2)->common.unsigned_flag));

  if (t1 == t2)
    return 1;



  if (((enum tree_code) (t1)->common.code) != ((enum tree_code) (t2)->common.code)) return 0;



  if (((((t1)->common.readonly_flag) * 0x1) | (((t1)->common.volatile_flag) * 0x2) | (((t1)->type.restrict_flag) * 0x4) | ((((enum tree_code) (t1)->common.code) == RECORD_TYPE && ((t1)->common.type)) * 0x8)) != ((((t2)->common.readonly_flag) * 0x1) | (((t2)->common.volatile_flag) * 0x2) | (((t2)->type.restrict_flag) * 0x4) | ((((enum tree_code) (t2)->common.code) == RECORD_TYPE && ((t2)->common.type)) * 0x8)))
    return 0;





  if (((t1)->type.main_variant) == ((t2)->type.main_variant))
    return 1;


  if (! (attrval = (*targetm.comp_type_attributes) (t1, t2)))
     return 0;


  val = 0;

  switch (((enum tree_code) (t1)->common.code))
    {
    case POINTER_TYPE:
      val = (((t1)->common.type) == ((t2)->common.type)
              ? 1 : comptypes (((t1)->common.type), ((t2)->common.type)));
      break;

    case FUNCTION_TYPE:
      val = function_types_compatible_p (t1, t2);
      break;

    case ARRAY_TYPE:
      {
        tree d1 = ((t1)->type.values);
        tree d2 = ((t2)->type.values);
        _Bool d1_variable, d2_variable;
        _Bool d1_zero, d2_zero;
        val = 1;


        if (((t1)->common.type) != ((t2)->common.type)
            && 0 == (val = comptypes (((t1)->common.type), ((t2)->common.type))))
          return 0;


        if (d1 == 0 || d2 == 0 || d1 == d2)
          break;

        d1_zero = ! ((d1)->type.maxval);
        d2_zero = ! ((d2)->type.maxval);

        d1_variable = (! d1_zero
                       && (((enum tree_code) (((d1)->type.minval))->common.code) != INTEGER_CST
                           || ((enum tree_code) (((d1)->type.maxval))->common.code) != INTEGER_CST));
        d2_variable = (! d2_zero
                       && (((enum tree_code) (((d2)->type.minval))->common.code) != INTEGER_CST
                           || ((enum tree_code) (((d2)->type.maxval))->common.code) != INTEGER_CST));

        if (d1_variable || d2_variable)
          break;
        if (d1_zero && d2_zero)
          break;
        if (d1_zero || d2_zero
            || ! tree_int_cst_equal (((d1)->type.minval), ((d2)->type.minval))
            || ! tree_int_cst_equal (((d1)->type.maxval), ((d2)->type.maxval)))
          val = 0;

        break;
      }

    case RECORD_TYPE:
      if (maybe_objc_comptypes (t1, t2, 0) == 1)
        val = 1;
      break;

    default:
      break;
    }
  return attrval == 2 && val == 1 ? 2 : val;
}




static int
comp_target_types (ttl, ttr)
     tree ttl, ttr;
{
  int val;


  if ((val = maybe_objc_comptypes (ttl, ttr, 1)) >= 0)
    return val;

  val = comptypes (((((ttl)->common.type))->type.main_variant),
                   ((((ttr)->common.type))->type.main_variant));

  if (val == 2 && pedantic)
    pedwarn ("types are not quite compatible");
  return val;
}
# 589 "c-typeck.c"
static int
function_types_compatible_p (f1, f2)
     tree f1, f2;
{
  tree args1, args2;

  int val = 1;
  int val1;

  if (!(((f1)->common.type) == ((f2)->common.type)
        || (val = comptypes (((f1)->common.type), ((f2)->common.type)))))
    return 0;

  args1 = ((f1)->type.values);
  args2 = ((f2)->type.values);




  if (args1 == 0)
    {
      if (!self_promoting_args_p (args2))
        return 0;



      if (((f1)->type.binfo)
          && 1 != type_lists_compatible_p (args2, ((f1)->type.binfo)))
        val = 2;
      return val;
    }
  if (args2 == 0)
    {
      if (!self_promoting_args_p (args1))
        return 0;
      if (((f2)->type.binfo)
          && 1 != type_lists_compatible_p (args1, ((f2)->type.binfo)))
        val = 2;
      return val;
    }


  val1 = type_lists_compatible_p (args1, args2);
  return val1 != 1 ? val1 : val;
}





static int
type_lists_compatible_p (args1, args2)
     tree args1, args2;
{

  int val = 1;
  int newval = 0;

  while (1)
    {
      if (args1 == 0 && args2 == 0)
        return val;


      if (args1 == 0 || args2 == 0)
        return 0;




      if (((args1)->list.value) == 0)
        {
          if (simple_type_promotes_to (((args2)->list.value)) != (tree) ((void *)0))
            return 0;
        }
      else if (((args2)->list.value) == 0)
        {
          if (simple_type_promotes_to (((args1)->list.value)) != (tree) ((void *)0))
            return 0;
        }
      else if (! (newval = comptypes (((((args1)->list.value))->type.main_variant),
                                      ((((args2)->list.value))->type.main_variant))))
        {


          if (((enum tree_code) (((args1)->list.value))->common.code) == UNION_TYPE
              && (((((args1)->list.value))->type.name) == 0
                  || ((((args1)->list.value))->type.transparent_union_flag))
              && ((enum tree_code) (((((args1)->list.value))->type.size))->common.code) == INTEGER_CST
              && tree_int_cst_equal (((((args1)->list.value))->type.size),
                                     ((((args2)->list.value))->type.size)))
            {
              tree memb;
              for (memb = ((((args1)->list.value))->type.values);
                   memb; memb = ((memb)->common.chain))
                if (comptypes (((memb)->common.type), ((args2)->list.value)))
                  break;
              if (memb == 0)
                return 0;
            }
          else if (((enum tree_code) (((args2)->list.value))->common.code) == UNION_TYPE
                   && (((((args2)->list.value))->type.name) == 0
                       || ((((args2)->list.value))->type.transparent_union_flag))
                   && ((enum tree_code) (((((args2)->list.value))->type.size))->common.code) == INTEGER_CST
                   && tree_int_cst_equal (((((args2)->list.value))->type.size),
                                          ((((args1)->list.value))->type.size)))
            {
              tree memb;
              for (memb = ((((args2)->list.value))->type.values);
                   memb; memb = ((memb)->common.chain))
                if (comptypes (((memb)->common.type), ((args1)->list.value)))
                  break;
              if (memb == 0)
                return 0;
            }
          else
            return 0;
        }


      if (newval > val)
        val = newval;

      args1 = ((args1)->common.chain);
      args2 = ((args2)->common.chain);
    }
}



tree
c_sizeof (type)
     tree type;
{
  enum tree_code code = ((enum tree_code) (type)->common.code);
  tree size;

  if (code == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
        pedwarn ("sizeof applied to a function type");
      size = global_trees[TI_SIZE_ONE];
    }
  else if (code == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
        pedwarn ("sizeof applied to a void type");
      size = global_trees[TI_SIZE_ONE];
    }
  else if (code == ERROR_MARK)
    size = global_trees[TI_SIZE_ONE];
  else if (!(((type)->type.size) != (tree) ((void *)0)))
    {
      error ("sizeof applied to an incomplete type");
      size = global_trees[TI_SIZE_ZERO];
    }
  else

    size = size_binop (CEIL_DIV_EXPR, ((type)->type.size_unit),
                       size_int_wide ((long long) (((integer_types[itk_char])->type.precision) / 8), SIZETYPE));






  return fold (build1 (NOP_EXPR, c_global_trees[CTI_C_SIZE_TYPE], size));
}

tree
c_sizeof_nowarn (type)
     tree type;
{
  enum tree_code code = ((enum tree_code) (type)->common.code);
  tree size;

  if (code == FUNCTION_TYPE || code == VOID_TYPE || code == ERROR_MARK)
    size = global_trees[TI_SIZE_ONE];
  else if (!(((type)->type.size) != (tree) ((void *)0)))
    size = global_trees[TI_SIZE_ZERO];
  else

    size = size_binop (CEIL_DIV_EXPR, ((type)->type.size_unit),
                       size_int_wide ((long long) (((integer_types[itk_char])->type.precision) / 8), SIZETYPE));






  return fold (build1 (NOP_EXPR, c_global_trees[CTI_C_SIZE_TYPE], size));
}



tree
c_size_in_bytes (type)
     tree type;
{
  enum tree_code code = ((enum tree_code) (type)->common.code);

  if (code == FUNCTION_TYPE || code == VOID_TYPE || code == ERROR_MARK)
    return global_trees[TI_SIZE_ONE];

  if (!((((type)->type.size) != (tree) ((void *)0)) || (((enum tree_code) (type)->common.code) == VOID_TYPE)))
    {
      error ("arithmetic on pointer to an incomplete type");
      return global_trees[TI_SIZE_ONE];
    }


  return size_binop (CEIL_DIV_EXPR, ((type)->type.size_unit),
                     size_int_wide ((long long) (((integer_types[itk_char])->type.precision) / 8), SIZETYPE));

}



tree
decl_constant_value (decl)
     tree decl;
{
  if (

      current_function_decl != 0
      && ! ((decl)->common.volatile_flag)
      && ((decl)->common.readonly_flag)
      && ((decl)->decl.initial) != 0
      && ((enum tree_code) (((decl)->decl.initial))->common.code) != ERROR_MARK



      && ((((decl)->decl.initial))->common.constant_flag)

      && ((enum tree_code) (((decl)->decl.initial))->common.code) != CONSTRUCTOR)
    return ((decl)->decl.initial);
  return decl;
}
# 836 "c-typeck.c"
static tree
decl_constant_value_for_broken_optimization (decl)
     tree decl;
{
  if (pedantic || ((decl)->decl.mode) == BLKmode)
    return decl;
  else
    return decl_constant_value (decl);
}






static tree
default_function_array_conversion (exp)
     tree exp;
{
  tree orig_exp;
  tree type = ((exp)->common.type);
  enum tree_code code = ((enum tree_code) (type)->common.code);
  int not_lvalue = 0;






  orig_exp = exp;
  while (((enum tree_code) (exp)->common.code) == NON_LVALUE_EXPR
         || (((enum tree_code) (exp)->common.code) == NOP_EXPR
             && ((((exp)->exp.operands[0]))->common.type) == ((exp)->common.type)))
    {
      if (((enum tree_code) (exp)->common.code) == NON_LVALUE_EXPR)
        not_lvalue = 1;
      exp = ((exp)->exp.operands[0]);
    }


  if (((tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == '<' || (tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == '1' || (tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == '2' || (tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == 'e'))
    (((exp)->exp.complexity) = (int) (((enum tree_code) ((orig_exp)->exp.complexity))));

  if (code == FUNCTION_TYPE)
    {
      return build_unary_op (ADDR_EXPR, exp, 0);
    }
  if (code == ARRAY_TYPE)
    {
      tree adr;
      tree restype = ((type)->common.type);
      tree ptrtype;
      int constp = 0;
      int volatilep = 0;
      int lvalue_array_p;

      if (tree_code_type[(int) (((enum tree_code) (exp)->common.code))] == 'r' || (tree_code_type[(int) (((enum tree_code) (exp)->common.code))] == 'd'))
        {
          constp = ((exp)->common.readonly_flag);
          volatilep = ((exp)->common.volatile_flag);
        }

      if (((((type)->common.readonly_flag) * 0x1) | (((type)->common.volatile_flag) * 0x2) | (((type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (type)->common.code) == RECORD_TYPE && ((type)->common.type)) * 0x8)) || constp || volatilep)
        restype
          = c_build_qualified_type (restype,
                                    ((((type)->common.readonly_flag) * 0x1) | (((type)->common.volatile_flag) * 0x2) | (((type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (type)->common.code) == RECORD_TYPE && ((type)->common.type)) * 0x8))
                                    | (constp * 0x1)
                                    | (volatilep * 0x2));

      if (((enum tree_code) (exp)->common.code) == INDIRECT_REF)
        return convert (((restype)->type.pointer_to),
                        ((exp)->exp.operands[0]));

      if (((enum tree_code) (exp)->common.code) == COMPOUND_EXPR)
        {
          tree op1 = default_conversion (((exp)->exp.operands[1]));
          return build (COMPOUND_EXPR, ((op1)->common.type),
                        ((exp)->exp.operands[0]), op1);
        }

      lvalue_array_p = !not_lvalue && lvalue_p (exp);
      if (!flag_isoc99 && !lvalue_array_p)
        {




          return exp;
        }

      ptrtype = build_pointer_type (restype);

      if (((enum tree_code) (exp)->common.code) == VAR_DECL)
        {




          adr = build1 (ADDR_EXPR, ptrtype, exp);
          if (mark_addressable (exp) == 0)
            return global_trees[TI_ERROR_MARK];
          ((adr)->common.constant_flag) = staticp (exp);
          ((adr)->common.side_effects_flag) = 0;
          return adr;
        }


      adr = build_unary_op (ADDR_EXPR, exp, 1);
      return convert (ptrtype, adr);
    }
  return exp;
}






tree
default_conversion (exp)
     tree exp;
{
  tree orig_exp;
  tree type = ((exp)->common.type);
  enum tree_code code = ((enum tree_code) (type)->common.code);

  if (code == FUNCTION_TYPE || code == ARRAY_TYPE)
    return default_function_array_conversion (exp);


  if (((enum tree_code) (exp)->common.code) == CONST_DECL)
    exp = ((exp)->decl.initial);




  else if (optimize && ((enum tree_code) (exp)->common.code) == VAR_DECL && code != ARRAY_TYPE)
    {
      exp = decl_constant_value_for_broken_optimization (exp);
      type = ((exp)->common.type);
    }






  orig_exp = exp;
  while (((enum tree_code) (exp)->common.code) == NON_LVALUE_EXPR
         || (((enum tree_code) (exp)->common.code) == NOP_EXPR
             && ((((exp)->exp.operands[0]))->common.type) == ((exp)->common.type)))
    exp = ((exp)->exp.operands[0]);


  if (((tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == '<' || (tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == '1' || (tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == '2' || (tree_code_type[(int) (((enum tree_code) (exp)->common.code))]) == 'e'))
    (((exp)->exp.complexity) = (int) (((enum tree_code) ((orig_exp)->exp.complexity))));



  if (code == ENUMERAL_TYPE)
    {
      type = type_for_size (((((type)->type.precision)) > (((integer_types[itk_int])->type.precision)) ? (((type)->type.precision)) : (((integer_types[itk_int])->type.precision))),

                            ((flag_traditional
                              || (((type)->type.precision)
                                  >= ((integer_types[itk_int])->type.precision)))
                             && ((type)->common.unsigned_flag)));

      return convert (type, exp);
    }

  if (((enum tree_code) (exp)->common.code) == COMPONENT_REF
      && ((((((exp)->exp.operands[1])))->decl.lang_flag_4) == 1)


      && 0 > compare_tree_int (((((exp)->exp.operands[1]))->decl.size),
                               ((integer_types[itk_int])->type.precision)))
    return convert (flag_traditional && ((type)->common.unsigned_flag)
                    ? integer_types[itk_unsigned_int] : integer_types[itk_int],
                    exp);

  if (c_promoting_integer_type_p (type))
    {


      if (((type)->common.unsigned_flag)
          && (flag_traditional
              || ((type)->type.precision) == ((integer_types[itk_int])->type.precision)))
        return convert (integer_types[itk_unsigned_int], exp);

      return convert (integer_types[itk_int], exp);
    }

  if (flag_traditional && !flag_allow_single_precision
      && ((type)->type.main_variant) == global_trees[TI_FLOAT_TYPE])
    return convert (global_trees[TI_DOUBLE_TYPE], exp);

  if (code == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return global_trees[TI_ERROR_MARK];
    }
  return exp;
}
# 1050 "c-typeck.c"
static tree
lookup_field (decl, component)
     tree decl, component;
{
  tree type = ((decl)->common.type);
  tree field;






  if (((type)->type.lang_specific))
    {
      int bot, top, half;
      tree *field_array = &((type)->type.lang_specific)->elts[0];

      field = ((type)->type.values);
      bot = 0;
      top = ((type)->type.lang_specific)->len;
      while (top - bot > 1)
        {
          half = (top - bot + 1) >> 1;
          field = field_array[bot+half];

          if (((field)->decl.name) == (tree) ((void *)0))
            {

              while (((field_array[bot])->decl.name) == (tree) ((void *)0))
                {
                  field = field_array[bot++];
                  if (((enum tree_code) (((field)->common.type))->common.code) == RECORD_TYPE
                      || ((enum tree_code) (((field)->common.type))->common.code) == UNION_TYPE)
                    {
                      tree anon = lookup_field (field, component);

                      if (anon)
                        return tree_cons ((tree) ((void *)0), field, anon);
                    }
                }


              if (bot > top)
                return (tree) ((void *)0);


              continue;
            }

          if (((field)->decl.name) == component)
            break;
          if (((field)->decl.name) < component)
            bot += half;
          else
            top = bot + half;
        }

      if (((field_array[bot])->decl.name) == component)
        field = field_array[bot];
      else if (((field)->decl.name) != component)
        return (tree) ((void *)0);
    }
  else
    {
      for (field = ((type)->type.values); field; field = ((field)->common.chain))
        {
          if (((field)->decl.name) == (tree) ((void *)0)
              && (((enum tree_code) (((field)->common.type))->common.code) == RECORD_TYPE
                  || ((enum tree_code) (((field)->common.type))->common.code) == UNION_TYPE))
            {
              tree anon = lookup_field (field, component);

              if (anon)
                return tree_cons ((tree) ((void *)0), field, anon);
            }

          if (((field)->decl.name) == component)
            break;
        }

      if (field == (tree) ((void *)0))
        return (tree) ((void *)0);
    }

  return tree_cons ((tree) ((void *)0), field, (tree) ((void *)0));
}




tree
build_component_ref (datum, component)
     tree datum, component;
{
  tree type = ((datum)->common.type);
  enum tree_code code = ((enum tree_code) (type)->common.code);
  tree field = ((void *)0);
  tree ref;
# 1157 "c-typeck.c"
  switch (((enum tree_code) (datum)->common.code))
    {
    case COMPOUND_EXPR:
      {
        tree value = build_component_ref (((datum)->exp.operands[1]), component);
        return build (COMPOUND_EXPR, ((value)->common.type),
                      ((datum)->exp.operands[0]), pedantic_non_lvalue (value));
      }
    default:
      break;
    }



  if (code == RECORD_TYPE || code == UNION_TYPE)
    {
      if (!(((type)->type.size) != (tree) ((void *)0)))
        {
          incomplete_type_error ((tree) ((void *)0), type);
          return global_trees[TI_ERROR_MARK];
        }

      field = lookup_field (datum, component);

      if (!field)
        {
          error ("%s has no member named `%s'",
                 code == RECORD_TYPE ? "structure" : "union",
                 ((const char *) (component)->identifier.id.str));
          return global_trees[TI_ERROR_MARK];
        }






      for (; field; field = ((field)->common.chain))
        {
          tree subdatum = ((field)->list.value);

          if (((subdatum)->common.type) == global_trees[TI_ERROR_MARK])
            return global_trees[TI_ERROR_MARK];

          ref = build (COMPONENT_REF, ((subdatum)->common.type), datum, subdatum);
          if (((datum)->common.readonly_flag) || ((subdatum)->common.readonly_flag))
            ((ref)->common.readonly_flag) = 1;
          if (((datum)->common.volatile_flag) || ((subdatum)->common.volatile_flag))
            ((ref)->common.volatile_flag) = 1;

          if (((subdatum)->common.deprecated_flag))
            warn_deprecated_use (subdatum);

          datum = ref;
        }

      return ref;
    }
  else if (code != ERROR_MARK)
    error ("request for member `%s' in something not a structure or union",
            ((const char *) (component)->identifier.id.str));

  return global_trees[TI_ERROR_MARK];
}





tree
build_indirect_ref (ptr, errorstring)
     tree ptr;
     const char *errorstring;
{
  tree pointer = default_conversion (ptr);
  tree type = ((pointer)->common.type);

  if (((enum tree_code) (type)->common.code) == POINTER_TYPE)
    {
      if (((enum tree_code) (pointer)->common.code) == ADDR_EXPR
          && !flag_volatile
          && (((((pointer)->exp.operands[0]))->common.type)
              == ((type)->common.type)))
        return ((pointer)->exp.operands[0]);
      else
        {
          tree t = ((type)->common.type);
          tree ref = build1 (INDIRECT_REF, ((t)->type.main_variant), pointer);

          if (!((((t)->type.size) != (tree) ((void *)0)) || (((enum tree_code) (t)->common.code) == VOID_TYPE)) && ((enum tree_code) (t)->common.code) != ARRAY_TYPE)
            {
              error ("dereferencing pointer to incomplete type");
              return global_trees[TI_ERROR_MARK];
            }
          if ((((enum tree_code) (t)->common.code) == VOID_TYPE) && skip_evaluation == 0)
            warning ("dereferencing `void *' pointer");
# 1261 "c-typeck.c"
          ((ref)->common.readonly_flag) = ((t)->common.readonly_flag);
          ((ref)->common.side_effects_flag)
            = ((t)->common.volatile_flag) || ((pointer)->common.side_effects_flag) || flag_volatile;
          ((ref)->common.volatile_flag) = ((t)->common.volatile_flag);
          return ref;
        }
    }
  else if (((enum tree_code) (pointer)->common.code) != ERROR_MARK)
    error ("invalid type argument of `%s'", errorstring);
  return global_trees[TI_ERROR_MARK];
}
# 1282 "c-typeck.c"
tree
build_array_ref (array, index)
     tree array, index;
{
  if (index == 0)
    {
      error ("subscript missing in array reference");
      return global_trees[TI_ERROR_MARK];
    }

  if (((array)->common.type) == global_trees[TI_ERROR_MARK]
      || ((index)->common.type) == global_trees[TI_ERROR_MARK])
    return global_trees[TI_ERROR_MARK];

  if (((enum tree_code) (((array)->common.type))->common.code) == ARRAY_TYPE
      && ((enum tree_code) (array)->common.code) != INDIRECT_REF)
    {
      tree rval, type;







      if (warn_char_subscripts
          && ((((index)->common.type))->type.main_variant) == integer_types[itk_char])
        warning ("array subscript has type `char'");


      index = default_conversion (index);


      if (((enum tree_code) (((index)->common.type))->common.code) != INTEGER_TYPE)
        {
          error ("array subscript is not an integer");
          return global_trees[TI_ERROR_MARK];
        }





      if (((enum tree_code) (index)->common.code) != INTEGER_CST
          || ((((((((array)->common.type))->common.type))->type.size) != (tree) ((void *)0))
              && ((enum tree_code) (((((((array)->common.type))->common.type))->type.size))->common.code) != INTEGER_CST))
        {
          if (mark_addressable (array) == 0)
            return global_trees[TI_ERROR_MARK];
        }




      if (((enum tree_code) (index)->common.code) == INTEGER_CST
          && ((((array)->common.type))->type.values)
          && ! int_fits_type_p (index, ((((array)->common.type))->type.values)))
        {
          if (mark_addressable (array) == 0)
            return global_trees[TI_ERROR_MARK];
        }

      if (pedantic)
        {
          tree foo = array;
          while (((enum tree_code) (foo)->common.code) == COMPONENT_REF)
            foo = ((foo)->exp.operands[0]);
          if (((enum tree_code) (foo)->common.code) == VAR_DECL && ((foo)->decl.regdecl_flag))
            pedwarn ("ISO C forbids subscripting `register' array");
          else if (! flag_isoc99 && ! lvalue_p (foo))
            pedwarn ("ISO C89 forbids subscripting non-lvalue array");
        }

      type = ((((((array)->common.type))->common.type))->type.main_variant);
      rval = build (ARRAY_REF, type, array, index);


      ((rval)->common.readonly_flag)
        |= (((((((array)->common.type))->common.type))->common.readonly_flag)
            | ((array)->common.readonly_flag));
      ((rval)->common.side_effects_flag)
        |= (((((((array)->common.type))->common.type))->common.volatile_flag)
            | ((array)->common.side_effects_flag));
      ((rval)->common.volatile_flag)
        |= (((((((array)->common.type))->common.type))->common.volatile_flag)




            | ((array)->common.volatile_flag));
      return require_complete_type (fold (rval));
    }

  {
    tree ar = default_conversion (array);
    tree ind = default_conversion (index);




    if (warn_char_subscripts
        && ((enum tree_code) (((index)->common.type))->common.code) == INTEGER_TYPE
        && ((((index)->common.type))->type.main_variant) == integer_types[itk_char])
      warning ("subscript has type `char'");


    if (((enum tree_code) (((ar)->common.type))->common.code) == INTEGER_TYPE)
      {
        tree temp = ar;
        ar = ind;
        ind = temp;
      }

    if (ar == global_trees[TI_ERROR_MARK])
      return ar;

    if (((enum tree_code) (((ar)->common.type))->common.code) != POINTER_TYPE
        || ((enum tree_code) (((((ar)->common.type))->common.type))->common.code) == FUNCTION_TYPE)
      {
        error ("subscripted value is neither array nor pointer");
        return global_trees[TI_ERROR_MARK];
      }
    if (((enum tree_code) (((ind)->common.type))->common.code) != INTEGER_TYPE)
      {
        error ("array subscript is not an integer");
        return global_trees[TI_ERROR_MARK];
      }

    return build_indirect_ref (build_binary_op (PLUS_EXPR, ar, ind, 0),
                               "array indexing");
  }
}



tree
build_external_ref (id, fun)
     tree id;
     int fun;
{
  tree ref;
  tree decl = lookup_name (id);
  tree objc_ivar = lookup_objc_ivar (id);

  if (decl && ((decl)->common.deprecated_flag))
    warn_deprecated_use (decl);

  if (!decl || decl == global_trees[TI_ERROR_MARK] || ((decl)->decl.lang_flag_3))
    {
      if (objc_ivar)
        ref = objc_ivar;
      else if (fun)
        {
          if (!decl || decl == global_trees[TI_ERROR_MARK])

            ref = implicitly_declare (id);
          else
            {



              implicit_decl_warning (id);


              ((decl)->decl.lang_flag_3) = 0;
              ref = decl;
            }
        }
      else
        {


          if (current_function_decl == 0)
            error ("`%s' undeclared here (not in a function)",
                   ((const char *) (id)->identifier.id.str));
          else
            {
              if ((((struct lang_identifier *) (id))->global_value) != global_trees[TI_ERROR_MARK]
                  || (((struct lang_identifier *) (id))->error_locus) != current_function_decl)
                {
                  error ("`%s' undeclared (first use in this function)",
                         ((const char *) (id)->identifier.id.str));

                  if (! undeclared_variable_notice)
                    {
                      error ("(Each undeclared identifier is reported only once");
                      error ("for each function it appears in.)");
                      undeclared_variable_notice = 1;
                    }
                }
              (((struct lang_identifier *) (id))->global_value) = global_trees[TI_ERROR_MARK];
              (((struct lang_identifier *) (id))->error_locus) = current_function_decl;
            }
          return global_trees[TI_ERROR_MARK];
        }
    }
  else
    {

      if (!objc_ivar)
        ref = decl;
      else if (decl != objc_ivar && (((struct lang_identifier *) (id))->local_value))
        {
          warning ("local declaration of `%s' hides instance variable",
                   ((const char *) (id)->identifier.id.str));
          ref = decl;
        }
      else
        ref = objc_ivar;
    }

  if (((ref)->common.type) == global_trees[TI_ERROR_MARK])
    return global_trees[TI_ERROR_MARK];

  assemble_external (ref);
  ((ref)->common.used_flag) = 1;

  if (((enum tree_code) (ref)->common.code) == CONST_DECL)
    {
      ref = ((ref)->decl.initial);
      ((ref)->common.constant_flag) = 1;
    }

  return ref;
}






tree
build_function_call (function, params)
     tree function, params;
{
  tree fntype, fundecl = 0;
  tree coerced_params;
  tree name = (tree) ((void *)0), assembler_name = (tree) ((void *)0), result;


  while ((((enum tree_code) (function)->common.code) == NOP_EXPR || ((enum tree_code) (function)->common.code) == CONVERT_EXPR || ((enum tree_code) (function)->common.code) == NON_LVALUE_EXPR) && ((function)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((function)->common.type) == ((((function)->exp.operands[0]))->common.type))) (function) = ((function)->exp.operands[0]);


  if (((enum tree_code) (function)->common.code) == FUNCTION_DECL)
    {
      name = ((function)->decl.name);
      assembler_name = ((((function)->decl.assembler_name != (tree) ((void *)0)) ? (void) 0 : (*lang_set_decl_assembler_name) (function)), (function)->decl.assembler_name);




      fntype = build_qualified_type ((((function)->common.type)), ((((function)->common.readonly_flag)) ? 0x1 : 0) | ((((function)->common.volatile_flag)) ? 0x2 : 0));


      fundecl = function;
      function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
    }
  else
    function = default_conversion (function);

  fntype = ((function)->common.type);

  if (((enum tree_code) (fntype)->common.code) == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];

  if (!(((enum tree_code) (fntype)->common.code) == POINTER_TYPE
        && ((enum tree_code) (((fntype)->common.type))->common.code) == FUNCTION_TYPE))
    {
      error ("called object is not a function");
      return global_trees[TI_ERROR_MARK];
    }

  if (fundecl && ((fundecl)->common.volatile_flag))
    current_function_returns_abnormally = 1;


  fntype = ((fntype)->common.type);




  coerced_params
    = convert_arguments (((fntype)->type.values), params, name, fundecl);



  if (warn_format)
    check_function_format (((void *)0), ((fntype)->type.attributes), coerced_params);





  if (((enum tree_code) (function)->common.code) == ADDR_EXPR
      && ((enum tree_code) (((function)->exp.operands[0]))->common.code) == FUNCTION_DECL
      && (((((function)->exp.operands[0]))->decl.built_in_class) != NOT_BUILT_IN))
    {
      result = expand_tree_builtin (((function)->exp.operands[0]),
                                    params, coerced_params);
      if (result)
        return result;
    }

  result = build (CALL_EXPR, ((fntype)->common.type),
                  function, coerced_params, (tree) ((void *)0));
  ((result)->common.side_effects_flag) = 1;
  result = fold (result);

  if ((((enum tree_code) (((result)->common.type))->common.code) == VOID_TYPE))
    return result;
  return require_complete_type (result);
}
# 1613 "c-typeck.c"
static tree
convert_arguments (typelist, values, name, fundecl)
     tree typelist, values, name, fundecl;
{
  tree typetail, valtail;
  tree result = ((void *)0);
  int parmnum;




  for (valtail = values, typetail = typelist, parmnum = 0;
       valtail;
       valtail = ((valtail)->common.chain), parmnum++)
    {
      tree type = typetail ? ((typetail)->list.value) : 0;
      tree val = ((valtail)->list.value);

      if (type == global_trees[TI_VOID_TYPE])
        {
          if (name)
            error ("too many arguments to function `%s'",
                   ((const char *) (name)->identifier.id.str));
          else
            error ("too many arguments to function");
          break;
        }




      if (((enum tree_code) (val)->common.code) == NON_LVALUE_EXPR)
        val = ((val)->exp.operands[0]);

      val = default_function_array_conversion (val);

      val = require_complete_type (val);

      if (type != 0)
        {

          tree parmval;

          if (!(((type)->type.size) != (tree) ((void *)0)))
            {
              error ("type of formal parameter %d is incomplete", parmnum + 1);
              parmval = val;
            }
          else
            {


              if (warn_conversion || warn_traditional)
                {
                  int formal_prec = ((type)->type.precision);

                  if ((((enum tree_code) (type)->common.code) == INTEGER_TYPE || ((enum tree_code) (type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (type)->common.code) == CHAR_TYPE)
                      && ((enum tree_code) (((val)->common.type))->common.code) == REAL_TYPE)
                    warn_for_assignment ("%s as integer rather than floating due to prototype", (char *) 0, name, parmnum + 1);
                  if ((((enum tree_code) (type)->common.code) == INTEGER_TYPE || ((enum tree_code) (type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (type)->common.code) == CHAR_TYPE)
                      && ((enum tree_code) (((val)->common.type))->common.code) == COMPLEX_TYPE)
                    warn_for_assignment ("%s as integer rather than complex due to prototype", (char *) 0, name, parmnum + 1);
                  else if (((enum tree_code) (type)->common.code) == COMPLEX_TYPE
                           && ((enum tree_code) (((val)->common.type))->common.code) == REAL_TYPE)
                    warn_for_assignment ("%s as complex rather than floating due to prototype", (char *) 0, name, parmnum + 1);
                  else if (((enum tree_code) (type)->common.code) == REAL_TYPE
                           && (((enum tree_code) (((val)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == CHAR_TYPE))
                    warn_for_assignment ("%s as floating rather than integer due to prototype", (char *) 0, name, parmnum + 1);
                  else if (((enum tree_code) (type)->common.code) == COMPLEX_TYPE
                           && (((enum tree_code) (((val)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == CHAR_TYPE))
                    warn_for_assignment ("%s as complex rather than integer due to prototype", (char *) 0, name, parmnum + 1);
                  else if (((enum tree_code) (type)->common.code) == REAL_TYPE
                           && ((enum tree_code) (((val)->common.type))->common.code) == COMPLEX_TYPE)
                    warn_for_assignment ("%s as floating rather than complex due to prototype", (char *) 0, name, parmnum + 1);



                  else if (((enum tree_code) (type)->common.code) == REAL_TYPE
                           && ((enum tree_code) (((val)->common.type))->common.code) == REAL_TYPE)
                    {


                      if (formal_prec == ((global_trees[TI_FLOAT_TYPE])->type.precision))
                        warn_for_assignment ("%s as `float' rather than `double' due to prototype", (char *) 0, name, parmnum + 1);
                    }



                  else if (warn_conversion && (((enum tree_code) (type)->common.code) == INTEGER_TYPE || ((enum tree_code) (type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (type)->common.code) == CHAR_TYPE)
                           && (((enum tree_code) (((val)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((val)->common.type))->common.code) == CHAR_TYPE))
                    {
                      tree would_have_been = default_conversion (val);
                      tree type1 = ((would_have_been)->common.type);

                      if (((enum tree_code) (type)->common.code) == ENUMERAL_TYPE
                          && (((type)->type.main_variant)
                              == ((((val)->common.type))->type.main_variant)))


                        ;
                      else if (formal_prec != ((type1)->type.precision))
                        warn_for_assignment ("%s with different width due to prototype", (char *) 0, name, parmnum + 1);
                      else if (((type)->common.unsigned_flag) == ((type1)->common.unsigned_flag))
                        ;



                      else if (((enum tree_code) (type)->common.code) == ENUMERAL_TYPE)
                        ;
                      else if (((enum tree_code) (val)->common.code) == INTEGER_CST
                               && int_fits_type_p (val, type))


                        ;

                      else if (((enum tree_code) (val)->common.code) == NOP_EXPR
                               && ((enum tree_code) (((val)->exp.operands[0]))->common.code) == INTEGER_CST
                               && int_fits_type_p (((val)->exp.operands[0]), type))
                        ;
# 1744 "c-typeck.c"
                      else if (((((val)->common.type))->type.precision) < ((type)->type.precision)
                               && ((((val)->common.type))->common.unsigned_flag))
                        ;
                      else if (((type)->common.unsigned_flag))
                        warn_for_assignment ("%s as unsigned due to prototype", (char *) 0, name, parmnum + 1);
                      else
                        warn_for_assignment ("%s as signed due to prototype", (char *) 0, name, parmnum + 1);
                    }
                }

              parmval = convert_for_assignment (type, val,
                                                (char *) 0,
                                                fundecl, name, parmnum + 1);

              if ((!(target_flags & 0x02000000))
                  && (((enum tree_code) (type)->common.code) == INTEGER_TYPE || ((enum tree_code) (type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (type)->common.code) == CHAR_TYPE)
                  && (((type)->type.precision) < ((integer_types[itk_int])->type.precision)))
                parmval = default_conversion (parmval);
            }
          result = tree_cons ((tree) ((void *)0), parmval, result);
        }
      else if (((enum tree_code) (((val)->common.type))->common.code) == REAL_TYPE
               && (((((val)->common.type))->type.precision)
                   < ((global_trees[TI_DOUBLE_TYPE])->type.precision)))

        result = tree_cons ((tree) ((void *)0), convert (global_trees[TI_DOUBLE_TYPE], val), result);
      else

        result = tree_cons ((tree) ((void *)0), default_conversion (val), result);

      if (typetail)
        typetail = ((typetail)->common.chain);
    }

  if (typetail != 0 && ((typetail)->list.value) != global_trees[TI_VOID_TYPE])
    {
      if (name)
        error ("too few arguments to function `%s'",
               ((const char *) (name)->identifier.id.str));
      else
        error ("too few arguments to function");
    }

  return nreverse (result);
}







tree
parser_build_binary_op (code, arg1, arg2)
     enum tree_code code;
     tree arg1, arg2;
{
  tree result = build_binary_op (code, arg1, arg2, 1);

  char class;
  char class1 = tree_code_type[(int) (((enum tree_code) (arg1)->common.code))];
  char class2 = tree_code_type[(int) (((enum tree_code) (arg2)->common.code))];
  enum tree_code code1 = ERROR_MARK;
  enum tree_code code2 = ERROR_MARK;

  if (((enum tree_code) (result)->common.code) == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];

  if (((class1) == '<' || (class1) == '1' || (class1) == '2' || (class1) == 'e'))
    code1 = ((enum tree_code) ((arg1)->exp.complexity));
  if (((class2) == '<' || (class2) == '1' || (class2) == '2' || (class2) == 'e'))
    code2 = ((enum tree_code) ((arg2)->exp.complexity));




  if (warn_parentheses)
    {
      if (code == LSHIFT_EXPR || code == RSHIFT_EXPR)
        {
          if (code1 == PLUS_EXPR || code1 == MINUS_EXPR
              || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
            warning ("suggest parentheses around + or - inside shift");
        }

      if (code == TRUTH_ORIF_EXPR)
        {
          if (code1 == TRUTH_ANDIF_EXPR
              || code2 == TRUTH_ANDIF_EXPR)
            warning ("suggest parentheses around && within ||");
        }

      if (code == BIT_IOR_EXPR)
        {
          if (code1 == BIT_AND_EXPR || code1 == BIT_XOR_EXPR
              || code1 == PLUS_EXPR || code1 == MINUS_EXPR
              || code2 == BIT_AND_EXPR || code2 == BIT_XOR_EXPR
              || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
            warning ("suggest parentheses around arithmetic in operand of |");

          if (tree_code_type[(int) (code1)] == '<' || tree_code_type[(int) (code2)] == '<')
            warning ("suggest parentheses around comparison in operand of |");
        }

      if (code == BIT_XOR_EXPR)
        {
          if (code1 == BIT_AND_EXPR
              || code1 == PLUS_EXPR || code1 == MINUS_EXPR
              || code2 == BIT_AND_EXPR
              || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
            warning ("suggest parentheses around arithmetic in operand of ^");

          if (tree_code_type[(int) (code1)] == '<' || tree_code_type[(int) (code2)] == '<')
            warning ("suggest parentheses around comparison in operand of ^");
        }

      if (code == BIT_AND_EXPR)
        {
          if (code1 == PLUS_EXPR || code1 == MINUS_EXPR
              || code2 == PLUS_EXPR || code2 == MINUS_EXPR)
            warning ("suggest parentheses around + or - in operand of &");

          if (tree_code_type[(int) (code1)] == '<' || tree_code_type[(int) (code2)] == '<')
            warning ("suggest parentheses around comparison in operand of &");
        }
    }


  if (tree_code_type[(int) (code)] == '<' && extra_warnings
      && (tree_code_type[(int) (code1)] == '<' || tree_code_type[(int) (code2)] == '<'))
    warning ("comparisons like X<=Y<=Z do not have their mathematical meaning");

  unsigned_conversion_warning (result, arg1);
  unsigned_conversion_warning (result, arg2);
  overflow_warning (result);

  class = tree_code_type[(int) (((enum tree_code) (result)->common.code))];



  if (((class) == '<' || (class) == '1' || (class) == '2' || (class) == 'e'))
    (((result)->exp.complexity) = (int) (code));
  else
    {
      int flag = ((result)->common.constant_flag);




      result = build1 (NON_LVALUE_EXPR, ((result)->common.type), result);
      (((result)->exp.complexity) = (int) (code));
      ((result)->common.constant_flag) = flag;
    }

  return result;
}
# 1916 "c-typeck.c"
tree
build_binary_op (code, orig_op0, orig_op1, convert_p)
     enum tree_code code;
     tree orig_op0, orig_op1;
     int convert_p;
{
  tree type0, type1;
  enum tree_code code0, code1;
  tree op0, op1;




  enum tree_code resultcode = code;



  tree result_type = ((void *)0);




  int converted = 0;



  tree build_type = 0;



  tree final_type = 0;







  int shorten = 0;




  int short_compare = 0;



  int short_shift = 0;


  int common = 0;

  if (convert_p)
    {
      op0 = default_conversion (orig_op0);
      op1 = default_conversion (orig_op1);
    }
  else
    {
      op0 = orig_op0;
      op1 = orig_op1;
    }

  type0 = ((op0)->common.type);
  type1 = ((op1)->common.type);



  code0 = ((enum tree_code) (type0)->common.code);
  code1 = ((enum tree_code) (type1)->common.code);


  while ((((enum tree_code) (op0)->common.code) == NOP_EXPR || ((enum tree_code) (op0)->common.code) == CONVERT_EXPR || ((enum tree_code) (op0)->common.code) == NON_LVALUE_EXPR) && ((op0)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((op0)->common.type) == ((((op0)->exp.operands[0]))->common.type))) (op0) = ((op0)->exp.operands[0]);
  while ((((enum tree_code) (op1)->common.code) == NOP_EXPR || ((enum tree_code) (op1)->common.code) == CONVERT_EXPR || ((enum tree_code) (op1)->common.code) == NON_LVALUE_EXPR) && ((op1)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((op1)->common.type) == ((((op1)->exp.operands[0]))->common.type))) (op1) = ((op1)->exp.operands[0]);




  if (code0 == ERROR_MARK || code1 == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];

  switch (code)
    {
    case PLUS_EXPR:

      if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
        return pointer_int_sum (PLUS_EXPR, op0, op1);
      else if (code1 == POINTER_TYPE && code0 == INTEGER_TYPE)
        return pointer_int_sum (PLUS_EXPR, op1, op0);
      else
        common = 1;
      break;

    case MINUS_EXPR:


      if (code0 == POINTER_TYPE && code1 == POINTER_TYPE
          && comp_target_types (type0, type1))
        return pointer_diff (op0, op1);

      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
        return pointer_int_sum (MINUS_EXPR, op0, op1);
      else
        common = 1;
      break;

    case MULT_EXPR:
      common = 1;
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:


      if (warn_div_by_zero && skip_evaluation == 0 && integer_zerop (op1))
        warning ("division by zero");

      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
           || code0 == COMPLEX_TYPE)
          && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
              || code1 == COMPLEX_TYPE))
        {
          if (!(code0 == INTEGER_TYPE && code1 == INTEGER_TYPE))
            resultcode = RDIV_EXPR;
          else





            shorten = (((((orig_op0)->common.type))->common.unsigned_flag)
                       || (((enum tree_code) (op1)->common.code) == INTEGER_CST
                           && ! integer_all_onesp (op1)));
          common = 1;
        }
      break;

    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        shorten = -1;
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      if (warn_div_by_zero && skip_evaluation == 0 && integer_zerop (op1))
        warning ("division by zero");

      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        {




          shorten = (((((orig_op0)->common.type))->common.unsigned_flag)
                     || (((enum tree_code) (op1)->common.code) == INTEGER_CST
                         && ! integer_all_onesp (op1)));
          common = 1;
        }
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == POINTER_TYPE
           || code0 == REAL_TYPE || code0 == COMPLEX_TYPE)
          && (code1 == INTEGER_TYPE || code1 == POINTER_TYPE
              || code1 == REAL_TYPE || code1 == COMPLEX_TYPE))
        {



          result_type = integer_types[itk_int];
          op0 = truthvalue_conversion (op0);
          op1 = truthvalue_conversion (op1);
          converted = 1;
        }
      break;





    case RSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        {
          if (((enum tree_code) (op1)->common.code) == INTEGER_CST && skip_evaluation == 0)
            {
              if (tree_int_cst_sgn (op1) < 0)
                warning ("right shift count is negative");
              else
                {
                  if (! integer_zerop (op1))
                    short_shift = 1;

                  if (compare_tree_int (op1, ((type0)->type.precision)) >= 0)
                    warning ("right shift count >= width of type");
                }
            }



          result_type = type0;


          if (! flag_traditional)
            {
              if (((((op1)->common.type))->type.main_variant) != integer_types[itk_int])
                op1 = convert (integer_types[itk_int], op1);

              converted = 1;
            }
        }
      break;

    case LSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        {
          if (((enum tree_code) (op1)->common.code) == INTEGER_CST && skip_evaluation == 0)
            {
              if (tree_int_cst_sgn (op1) < 0)
                warning ("left shift count is negative");

              else if (compare_tree_int (op1, ((type0)->type.precision)) >= 0)
                warning ("left shift count >= width of type");
            }



          result_type = type0;


          if (! flag_traditional)
            {
              if (((((op1)->common.type))->type.main_variant) != integer_types[itk_int])
                op1 = convert (integer_types[itk_int], op1);

              converted = 1;
            }
        }
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
        {
          if (((enum tree_code) (op1)->common.code) == INTEGER_CST && skip_evaluation == 0)
            {
              if (tree_int_cst_sgn (op1) < 0)
                warning ("shift count is negative");
              else if (compare_tree_int (op1, ((type0)->type.precision)) >= 0)
                warning ("shift count >= width of type");
            }



          result_type = type0;


          if (! flag_traditional)
            {
              if (((((op1)->common.type))->type.main_variant) != integer_types[itk_int])
                op1 = convert (integer_types[itk_int], op1);

              converted = 1;
            }
        }
      break;

    case EQ_EXPR:
    case NE_EXPR:
      if (warn_float_equal && (code0 == REAL_TYPE || code1 == REAL_TYPE))
        warning ("comparing floating point with == or != is unsafe");


      build_type = integer_types[itk_int];
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE
           || code0 == COMPLEX_TYPE)
          && (code1 == INTEGER_TYPE || code1 == REAL_TYPE
              || code1 == COMPLEX_TYPE))
        short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
        {
          tree tt0 = ((type0)->common.type);
          tree tt1 = ((type1)->common.type);



          if (comp_target_types (type0, type1))
            result_type = common_type (type0, type1);
          else if ((((enum tree_code) (tt0)->common.code) == VOID_TYPE))
            {


              if (pedantic && (!integer_zerop (op0) || op0 != orig_op0)
                  && ((enum tree_code) (tt1)->common.code) == FUNCTION_TYPE)
                pedwarn ("ISO C forbids comparison of `void *' with function pointer");
            }
          else if ((((enum tree_code) (tt1)->common.code) == VOID_TYPE))
            {
              if (pedantic && (!integer_zerop (op1) || op1 != orig_op1)
                  && ((enum tree_code) (tt0)->common.code) == FUNCTION_TYPE)
                pedwarn ("ISO C forbids comparison of `void *' with function pointer");
            }
          else
            pedwarn ("comparison of distinct pointer types lacks a cast");

          if (result_type == (tree) ((void *)0))
            result_type = global_trees[TI_PTR_TYPE];
        }
      else if (code0 == POINTER_TYPE && ((enum tree_code) (op1)->common.code) == INTEGER_CST
               && integer_zerop (op1))
        result_type = type0;
      else if (code1 == POINTER_TYPE && ((enum tree_code) (op0)->common.code) == INTEGER_CST
               && integer_zerop (op0))
        result_type = type1;
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
        {
          result_type = type0;
          if (! flag_traditional)
            pedwarn ("comparison between pointer and integer");
        }
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
        {
          result_type = type1;
          if (! flag_traditional)
            pedwarn ("comparison between pointer and integer");
        }
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
          && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
        shorten = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
        {
          if (comp_target_types (type0, type1))
            {
              result_type = common_type (type0, type1);
              if (pedantic
                  && ((enum tree_code) (((type0)->common.type))->common.code) == FUNCTION_TYPE)
                pedwarn ("ISO C forbids ordered comparisons of pointers to functions");
            }
          else
            {
              result_type = global_trees[TI_PTR_TYPE];
              pedwarn ("comparison of distinct pointer types lacks a cast");
            }
        }
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      build_type = integer_types[itk_int];
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
          && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
        short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
        {
          if (comp_target_types (type0, type1))
            {
              result_type = common_type (type0, type1);
              if (!(((((type0)->common.type))->type.size) != (tree) ((void *)0))
                  != !(((((type1)->common.type))->type.size) != (tree) ((void *)0)))
                pedwarn ("comparison of complete and incomplete pointers");
              else if (pedantic
                       && ((enum tree_code) (((type0)->common.type))->common.code) == FUNCTION_TYPE)
                pedwarn ("ISO C forbids ordered comparisons of pointers to functions");
            }
          else
            {
              result_type = global_trees[TI_PTR_TYPE];
              pedwarn ("comparison of distinct pointer types lacks a cast");
            }
        }
      else if (code0 == POINTER_TYPE && ((enum tree_code) (op1)->common.code) == INTEGER_CST
               && integer_zerop (op1))
        {
          result_type = type0;
          if (pedantic || extra_warnings)
            pedwarn ("ordered comparison of pointer with integer zero");
        }
      else if (code1 == POINTER_TYPE && ((enum tree_code) (op0)->common.code) == INTEGER_CST
               && integer_zerop (op0))
        {
          result_type = type1;
          if (pedantic)
            pedwarn ("ordered comparison of pointer with integer zero");
        }
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
        {
          result_type = type0;
          if (! flag_traditional)
            pedwarn ("comparison between pointer and integer");
        }
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
        {
          result_type = type1;
          if (! flag_traditional)
            pedwarn ("comparison between pointer and integer");
        }
      break;

    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
      build_type = integer_types[itk_int];
      if (code0 != REAL_TYPE || code1 != REAL_TYPE)
        {
          error ("unordered comparison on non-floating point argument");
          return global_trees[TI_ERROR_MARK];
        }
      common = 1;
      break;

    default:
      break;
    }

  if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE || code0 == COMPLEX_TYPE)
      &&
      (code1 == INTEGER_TYPE || code1 == REAL_TYPE || code1 == COMPLEX_TYPE))
    {
      int none_complex = (code0 != COMPLEX_TYPE && code1 != COMPLEX_TYPE);

      if (shorten || common || short_compare)
        result_type = common_type (type0, type1);
# 2369 "c-typeck.c"
      if (shorten && none_complex)
        {
          int unsigned0, unsigned1;
          tree arg0 = get_narrower (op0, &unsigned0);
          tree arg1 = get_narrower (op1, &unsigned1);

          int uns = ((result_type)->common.unsigned_flag);
          tree type;

          final_type = result_type;




          if ((((((op0)->common.type))->type.precision)
               == ((((arg0)->common.type))->type.precision))
              && ((op0)->common.type) != final_type)
            unsigned0 = ((((op0)->common.type))->common.unsigned_flag);
          if ((((((op1)->common.type))->type.precision)
               == ((((arg1)->common.type))->type.precision))
              && ((op1)->common.type) != final_type)
            unsigned1 = ((((op1)->common.type))->common.unsigned_flag);





          if (shorten == -1)
            uns = unsigned0;
# 2406 "c-typeck.c"
          if ((((((arg0)->common.type))->type.precision)
               < ((result_type)->type.precision))
              && (((((arg1)->common.type))->type.precision)
                  == ((((arg0)->common.type))->type.precision))
              && unsigned0 == unsigned1
              && (unsigned0 || !uns))
            result_type
              = signed_or_unsigned_type (unsigned0,
                                         common_type (((arg0)->common.type), ((arg1)->common.type)));
          else if (((enum tree_code) (arg0)->common.code) == INTEGER_CST
                   && (unsigned1 || !uns)
                   && (((((arg1)->common.type))->type.precision)
                       < ((result_type)->type.precision))
                   && (type = signed_or_unsigned_type (unsigned1,
                                                       ((arg1)->common.type)),
                       int_fits_type_p (arg0, type)))
            result_type = type;
          else if (((enum tree_code) (arg1)->common.code) == INTEGER_CST
                   && (unsigned0 || !uns)
                   && (((((arg0)->common.type))->type.precision)
                       < ((result_type)->type.precision))
                   && (type = signed_or_unsigned_type (unsigned0,
                                                       ((arg0)->common.type)),
                       int_fits_type_p (arg1, type)))
            result_type = type;
        }



      if (short_shift)
        {
          int unsigned_arg;
          tree arg0 = get_narrower (op0, &unsigned_arg);

          final_type = result_type;

          if (arg0 == op0 && final_type == ((op0)->common.type))
            unsigned_arg = ((((op0)->common.type))->common.unsigned_flag);

          if (((((arg0)->common.type))->type.precision) < ((result_type)->type.precision)


              && compare_tree_int (op1, ((((arg0)->common.type))->type.precision)) < 0

              && (!((final_type)->common.unsigned_flag) || unsigned_arg))
            {

              result_type
                = signed_or_unsigned_type (unsigned_arg, ((arg0)->common.type));

              if (((op0)->common.type) != result_type)
                op0 = convert (result_type, op0);
              converted = 1;
            }
        }




      if (short_compare)
        {




          tree xop0 = op0, xop1 = op1, xresult_type = result_type;
          enum tree_code xresultcode = resultcode;
          tree val
            = shorten_compare (&xop0, &xop1, &xresult_type, &xresultcode);

          if (val != 0)
            return val;

          op0 = xop0, op1 = xop1;
          converted = 1;
          resultcode = xresultcode;

          if ((warn_sign_compare < 0 ? extra_warnings : warn_sign_compare != 0)
              && skip_evaluation == 0)
            {
              int op0_signed = ! ((((orig_op0)->common.type))->common.unsigned_flag);
              int op1_signed = ! ((((orig_op1)->common.type))->common.unsigned_flag);
              int unsignedp0, unsignedp1;
              tree primop0 = get_narrower (op0, &unsignedp0);
              tree primop1 = get_narrower (op1, &unsignedp1);

              xop0 = orig_op0;
              xop1 = orig_op1;
              while ((((enum tree_code) (xop0)->common.code) == NOP_EXPR || ((enum tree_code) (xop0)->common.code) == CONVERT_EXPR || ((enum tree_code) (xop0)->common.code) == NON_LVALUE_EXPR) && ((xop0)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((xop0)->common.type) == ((((xop0)->exp.operands[0]))->common.type))) (xop0) = ((xop0)->exp.operands[0]);
              while ((((enum tree_code) (xop1)->common.code) == NOP_EXPR || ((enum tree_code) (xop1)->common.code) == CONVERT_EXPR || ((enum tree_code) (xop1)->common.code) == NON_LVALUE_EXPR) && ((xop1)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((xop1)->common.type) == ((((xop1)->exp.operands[0]))->common.type))) (xop1) = ((xop1)->exp.operands[0]);
# 2506 "c-typeck.c"
              if (! ((result_type)->common.unsigned_flag))
                        ;

              else if (op0_signed == op1_signed)
                        ;
              else
                {
                  tree sop, uop;

                  if (op0_signed)
                    sop = xop0, uop = xop1;
                  else
                    sop = xop1, uop = xop0;






                  if (tree_expr_nonnegative_p (sop))
                            ;



                  else if (((enum tree_code) (uop)->common.code) == INTEGER_CST
                           && (resultcode == EQ_EXPR || resultcode == NE_EXPR)
                           && int_fits_type_p (uop, signed_type (result_type)))
                            ;



                  else if (((enum tree_code) (uop)->common.code) == INTEGER_CST
                           && ((enum tree_code) (((uop)->common.type))->common.code) == ENUMERAL_TYPE
                           && int_fits_type_p (((((uop)->common.type))->type.maxval),
                                               signed_type (result_type)))
                            ;
                  else
                    warning ("comparison between signed and unsigned");
                }
# 2554 "c-typeck.c"
              if ((((enum tree_code) (primop0)->common.code) == BIT_NOT_EXPR)
                  != (((enum tree_code) (primop1)->common.code) == BIT_NOT_EXPR))
                {
                  if (((enum tree_code) (primop0)->common.code) == BIT_NOT_EXPR)
                    primop0 = get_narrower (((primop0)->exp.operands[0]),
                                            &unsignedp0);
                  else
                    primop1 = get_narrower (((primop1)->exp.operands[0]),
                                            &unsignedp1);

                  if (host_integerp (primop0, 0) || host_integerp (primop1, 0))
                    {
                      tree primop;
                      long long constant, mask;
                      int unsignedp, bits;

                      if (host_integerp (primop0, 0))
                        {
                          primop = primop1;
                          unsignedp = unsignedp1;
                          constant = tree_low_cst (primop0, 0);
                        }
                      else
                        {
                          primop = primop0;
                          unsignedp = unsignedp0;
                          constant = tree_low_cst (primop1, 0);
                        }

                      bits = ((((primop)->common.type))->type.precision);
                      if (bits < ((result_type)->type.precision)
                          && bits < (8 * 8) && unsignedp)
                        {
                          mask = (~ (long long) 0) << bits;
                          if ((mask & constant) != mask)
                            warning ("comparison of promoted ~unsigned with constant");
                        }
                    }
                  else if (unsignedp0 && unsignedp1
                           && (((((primop0)->common.type))->type.precision)
                               < ((result_type)->type.precision))
                           && (((((primop1)->common.type))->type.precision)
                               < ((result_type)->type.precision)))
                    warning ("comparison of promoted ~unsigned with unsigned");
                }
            }
        }
    }







  if (!result_type)
    {
      binary_op_error (code);
      return global_trees[TI_ERROR_MARK];
    }

  if (! converted)
    {
      if (((op0)->common.type) != result_type)
        op0 = convert (result_type, op0);
      if (((op1)->common.type) != result_type)
        op1 = convert (result_type, op1);
    }

  if (build_type == (tree) ((void *)0))
    build_type = result_type;

  {
    tree result = build (resultcode, build_type, op0, op1);
    tree folded;

    folded = fold (result);
    if (folded == result)
      ((folded)->common.constant_flag) = ((op0)->common.constant_flag) & ((op1)->common.constant_flag);
    if (final_type != 0)
      return convert (final_type, folded);
    return folded;
  }
}




static tree
pointer_diff (op0, op1)
     tree op0, op1;
{
  tree result, folded;
  tree restype = global_trees[TI_PTRDIFF_TYPE];

  tree target_type = ((((op0)->common.type))->common.type);
  tree con0, con1, lit0, lit1;
  tree orig_op1 = op1;

  if (pedantic || warn_pointer_arith)
    {
      if (((enum tree_code) (target_type)->common.code) == VOID_TYPE)
        pedwarn ("pointer of type `void *' used in subtraction");
      if (((enum tree_code) (target_type)->common.code) == FUNCTION_TYPE)
        pedwarn ("pointer to a function used in subtraction");
    }
# 2669 "c-typeck.c"
  con0 = ((enum tree_code) (op0)->common.code) == NOP_EXPR ? ((op0)->exp.operands[0]) : op0;
  con1 = ((enum tree_code) (op1)->common.code) == NOP_EXPR ? ((op1)->exp.operands[0]) : op1;

  if (((enum tree_code) (con0)->common.code) == PLUS_EXPR)
    {
      lit0 = ((con0)->exp.operands[1]);
      con0 = ((con0)->exp.operands[0]);
    }
  else
    lit0 = global_trees[TI_INTEGER_ZERO];

  if (((enum tree_code) (con1)->common.code) == PLUS_EXPR)
    {
      lit1 = ((con1)->exp.operands[1]);
      con1 = ((con1)->exp.operands[0]);
    }
  else
    lit1 = global_trees[TI_INTEGER_ZERO];

  if (operand_equal_p (con0, con1, 0))
    {
      op0 = lit0;
      op1 = lit1;
    }







  op0 = build_binary_op (MINUS_EXPR, convert (restype, op0),
                         convert (restype, op1), 0);

  if (!((((((((orig_op1)->common.type))->common.type))->type.size) != (tree) ((void *)0)) || (((enum tree_code) (((((orig_op1)->common.type))->common.type))->common.code) == VOID_TYPE)))
    error ("arithmetic on pointer to an incomplete type");


  op1 = c_size_in_bytes (target_type);



  result = build (EXACT_DIV_EXPR, restype, op0, convert (restype, op1));

  folded = fold (result);
  if (folded == result)
    ((folded)->common.constant_flag) = ((op0)->common.constant_flag) & ((op1)->common.constant_flag);
  return folded;
}
# 2728 "c-typeck.c"
tree
build_unary_op (code, xarg, flag)
     enum tree_code code;
     tree xarg;
     int flag;
{

  tree arg = xarg;
  tree argtype = 0;
  enum tree_code typecode = ((enum tree_code) (((arg)->common.type))->common.code);
  tree val;
  int noconvert = flag;

  if (typecode == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];
  if (typecode == ENUMERAL_TYPE || typecode == BOOLEAN_TYPE)
    typecode = INTEGER_TYPE;

  switch (code)
    {
    case CONVERT_EXPR:



      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
            || typecode == COMPLEX_TYPE))
        {
          error ("wrong type argument to unary plus");
          return global_trees[TI_ERROR_MARK];
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case NEGATE_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
            || typecode == COMPLEX_TYPE))
        {
          error ("wrong type argument to unary minus");
          return global_trees[TI_ERROR_MARK];
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case BIT_NOT_EXPR:
      if (typecode == COMPLEX_TYPE)
        {
          code = CONJ_EXPR;
          if (pedantic)
            pedwarn ("ISO C does not support `~' for complex conjugation");
          if (!noconvert)
            arg = default_conversion (arg);
        }
      else if (typecode != INTEGER_TYPE)
        {
          error ("wrong type argument to bit-complement");
          return global_trees[TI_ERROR_MARK];
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case ABS_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
            || typecode == COMPLEX_TYPE))
        {
          error ("wrong type argument to abs");
          return global_trees[TI_ERROR_MARK];
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case CONJ_EXPR:

      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE
            || typecode == COMPLEX_TYPE))
        {
          error ("wrong type argument to conjugation");
          return global_trees[TI_ERROR_MARK];
        }
      else if (!noconvert)
        arg = default_conversion (arg);
      break;

    case TRUTH_NOT_EXPR:
      if (typecode != INTEGER_TYPE
          && typecode != REAL_TYPE && typecode != POINTER_TYPE
          && typecode != COMPLEX_TYPE

          && typecode != ARRAY_TYPE && typecode != FUNCTION_TYPE)
        {
          error ("wrong type argument to unary exclamation mark");
          return global_trees[TI_ERROR_MARK];
        }
      arg = truthvalue_conversion (arg);
      return invert_truthvalue (arg);

    case NOP_EXPR:
      break;

    case REALPART_EXPR:
      if (((enum tree_code) (arg)->common.code) == COMPLEX_CST)
        return ((arg)->complex.real);
      else if (((enum tree_code) (((arg)->common.type))->common.code) == COMPLEX_TYPE)
        return fold (build1 (REALPART_EXPR, ((((arg)->common.type))->common.type), arg));
      else
        return arg;

    case IMAGPART_EXPR:
      if (((enum tree_code) (arg)->common.code) == COMPLEX_CST)
        return ((arg)->complex.imag);
      else if (((enum tree_code) (((arg)->common.type))->common.code) == COMPLEX_TYPE)
        return fold (build1 (IMAGPART_EXPR, ((((arg)->common.type))->common.type), arg));
      else
        return convert (((arg)->common.type), global_trees[TI_INTEGER_ZERO]);

    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:



      val = unary_complex_lvalue (code, arg, 0);
      if (val != 0)
        return val;



      if (typecode == COMPLEX_TYPE)
        {
          tree real, imag;

          if (pedantic)
            pedwarn ("ISO C does not support `++' and `--' on complex types");

          arg = stabilize_reference (arg);
          real = build_unary_op (REALPART_EXPR, arg, 1);
          imag = build_unary_op (IMAGPART_EXPR, arg, 1);
          return build (COMPLEX_EXPR, ((arg)->common.type),
                        build_unary_op (code, real, 1), imag);
        }



      if (typecode != POINTER_TYPE
          && typecode != INTEGER_TYPE && typecode != REAL_TYPE)
        {
          if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
            error ("wrong type argument to increment");
          else
            error ("wrong type argument to decrement");

          return global_trees[TI_ERROR_MARK];
        }

      {
        tree inc;
        tree result_type = ((arg)->common.type);

        arg = get_unwidened (arg, 0);
        argtype = ((arg)->common.type);



        if (typecode == POINTER_TYPE)
          {


            if (!((((((result_type)->common.type))->type.size) != (tree) ((void *)0)) || (((enum tree_code) (((result_type)->common.type))->common.code) == VOID_TYPE)))
              {
                if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
                  error ("increment of pointer to unknown structure");
                else
                  error ("decrement of pointer to unknown structure");
              }
            else if ((pedantic || warn_pointer_arith)
                     && (((enum tree_code) (((result_type)->common.type))->common.code) == FUNCTION_TYPE
                         || ((enum tree_code) (((result_type)->common.type))->common.code) == VOID_TYPE))
              {
                if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
                  pedwarn ("wrong type argument to increment");
                else
                  pedwarn ("wrong type argument to decrement");
              }

            inc = c_size_in_bytes (((result_type)->common.type));
          }
        else
          inc = global_trees[TI_INTEGER_ONE];

        inc = convert (argtype, inc);



        while (1)
          switch (((enum tree_code) (arg)->common.code))
            {
            case NOP_EXPR:
            case CONVERT_EXPR:
            case FLOAT_EXPR:
            case FIX_TRUNC_EXPR:
            case FIX_FLOOR_EXPR:
            case FIX_ROUND_EXPR:
            case FIX_CEIL_EXPR:
              pedantic_lvalue_warning (CONVERT_EXPR);



              if ((((enum tree_code) (((arg)->common.type))->common.code)
                   == ((enum tree_code) (((((arg)->exp.operands[0]))->common.type))->common.code))
                  && (((((arg)->common.type))->type.mode)
                      == ((((((arg)->exp.operands[0]))->common.type))->type.mode)))
                arg = ((arg)->exp.operands[0]);
              else
                {
                  tree incremented, modify, value;
                  if (((enum tree_code) (((arg)->common.type))->common.code) == BOOLEAN_TYPE)
                    value = boolean_increment (code, arg);
                  else
                    {
                      arg = stabilize_reference (arg);
                      if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
                        value = arg;
                      else
                        value = save_expr (arg);
                      incremented = build (((code == PREINCREMENT_EXPR
                                             || code == POSTINCREMENT_EXPR)
                                            ? PLUS_EXPR : MINUS_EXPR),
                                           argtype, value, inc);
                      ((incremented)->common.side_effects_flag) = 1;
                      modify = build_modify_expr (arg, NOP_EXPR, incremented);
                      value = build (COMPOUND_EXPR, ((arg)->common.type), modify, value);
                    }
                  ((value)->common.used_flag) = 1;
                  return value;
                }
              break;

            default:
              goto give_up;
            }
      give_up:


        if (!lvalue_or_else (arg, ((code == PREINCREMENT_EXPR
                                    || code == POSTINCREMENT_EXPR)
                                   ? "invalid lvalue in increment"
                                   : "invalid lvalue in decrement")))
          return global_trees[TI_ERROR_MARK];


        if (((arg)->common.readonly_flag))
          readonly_warning (arg,
                            ((code == PREINCREMENT_EXPR
                              || code == POSTINCREMENT_EXPR)
                             ? "increment" : "decrement"));

        if (((enum tree_code) (((arg)->common.type))->common.code) == BOOLEAN_TYPE)
          val = boolean_increment (code, arg);
        else
          val = build (code, ((arg)->common.type), arg, inc);
        ((val)->common.side_effects_flag) = 1;
        val = convert (result_type, val);
        if (((enum tree_code) (val)->common.code) != code)
          ((val)->common.static_flag) = 1;
        return val;
      }

    case ADDR_EXPR:



      if (((enum tree_code) (arg)->common.code) == INDIRECT_REF)
        {

          if (lvalue_p (((arg)->exp.operands[0])))
            return non_lvalue (((arg)->exp.operands[0]));
          return ((arg)->exp.operands[0]);
        }


      if (((enum tree_code) (arg)->common.code) == ARRAY_REF)
        {
          if (mark_addressable (((arg)->exp.operands[0])) == 0)
            return global_trees[TI_ERROR_MARK];
          return build_binary_op (PLUS_EXPR, ((arg)->exp.operands[0]),
                                  ((arg)->exp.operands[1]), 1);
        }



      val = unary_complex_lvalue (code, arg, flag);
      if (val != 0)
        return val;
# 3050 "c-typeck.c"
      else if (typecode != FUNCTION_TYPE && !flag
               && !lvalue_or_else (arg, "invalid lvalue in unary `&'"))
        return global_trees[TI_ERROR_MARK];


      argtype = ((arg)->common.type);





      if (((tree_code_type[(int) (((enum tree_code) (arg)->common.code))] == 'd') || tree_code_type[(int) (((enum tree_code) (arg)->common.code))] == 'r')
          && (((arg)->common.readonly_flag) || ((arg)->common.volatile_flag)))
          argtype = c_build_qualified_type ((argtype), ((((arg)->common.readonly_flag)) ? 0x1 : 0) | ((((arg)->common.volatile_flag)) ? 0x2 : 0));



      argtype = build_pointer_type (argtype);

      if (mark_addressable (arg) == 0)
        return global_trees[TI_ERROR_MARK];

      {
        tree addr;

        if (((enum tree_code) (arg)->common.code) == COMPONENT_REF)
          {
            tree field = ((arg)->exp.operands[1]);

            addr = build_unary_op (ADDR_EXPR, ((arg)->exp.operands[0]), flag);

            if (((((field))->decl.lang_flag_4) == 1))
              {
                error ("attempt to take address of bit-field structure member `%s'",
                       ((const char *) (((field)->decl.name))->identifier.id.str));
                return global_trees[TI_ERROR_MARK];
              }

            addr = fold (build (PLUS_EXPR, argtype,
                                convert (argtype, addr),
                                convert (argtype, byte_position (field))));
          }
        else
          addr = build1 (code, argtype, arg);



        if (staticp (arg)
            && ! (((enum tree_code) (arg)->common.code) == FUNCTION_DECL
                  && ((arg)->decl.context) != 0))
          ((addr)->common.constant_flag) = 1;
        return addr;
      }

    default:
      break;
    }

  if (argtype == 0)
    argtype = ((arg)->common.type);
  return fold (build1 (code, argtype, arg));
}
# 3146 "c-typeck.c"
int
lvalue_p (ref)
     tree ref;
{
  enum tree_code code = ((enum tree_code) (ref)->common.code);

  switch (code)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
      return lvalue_p (((ref)->exp.operands[0]));

    case COMPOUND_LITERAL_EXPR:
    case STRING_CST:
      return 1;

    case INDIRECT_REF:
    case ARRAY_REF:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case ERROR_MARK:
      return (((enum tree_code) (((ref)->common.type))->common.code) != FUNCTION_TYPE
              && ((enum tree_code) (((ref)->common.type))->common.code) != METHOD_TYPE);

    case BIND_EXPR:
    case RTL_EXPR:
      return ((enum tree_code) (((ref)->common.type))->common.code) == ARRAY_TYPE;

    default:
      return 0;
    }
}




int
lvalue_or_else (ref, msgid)
     tree ref;
     const char *msgid;
{
  int win = lvalue_p (ref);

  if (! win)
    error ("%s", msgid);

  return win;
}
# 3205 "c-typeck.c"
static tree
unary_complex_lvalue (code, arg, flag)
     enum tree_code code;
     tree arg;
     int flag;
{

  if (((enum tree_code) (arg)->common.code) == COMPOUND_EXPR)
    {
      tree real_result = build_unary_op (code, ((arg)->exp.operands[1]), 0);



      if (((enum tree_code) (((arg)->common.type))->common.code) != FUNCTION_TYPE && !flag)
        pedantic_lvalue_warning (COMPOUND_EXPR);

      return build (COMPOUND_EXPR, ((real_result)->common.type),
                    ((arg)->exp.operands[0]), real_result);
    }


  if (((enum tree_code) (arg)->common.code) == COND_EXPR)
    {
      if (!flag)
        pedantic_lvalue_warning (COND_EXPR);
      if (((enum tree_code) (((arg)->common.type))->common.code) != FUNCTION_TYPE && !flag)
        pedantic_lvalue_warning (COMPOUND_EXPR);

      return (build_conditional_expr
              (((arg)->exp.operands[0]),
               build_unary_op (code, ((arg)->exp.operands[1]), flag),
               build_unary_op (code, ((arg)->exp.operands[2]), flag)));
    }

  return 0;
}




static void
pedantic_lvalue_warning (code)
     enum tree_code code;
{
  if (pedantic)
    switch (code)
      {
      case COND_EXPR:
        pedwarn ("ISO C forbids use of conditional expressions as lvalues");
        break;
      case COMPOUND_EXPR:
        pedwarn ("ISO C forbids use of compound expressions as lvalues");
        break;
      default:
        pedwarn ("ISO C forbids use of cast expressions as lvalues");
        break;
      }
}



void
readonly_warning (arg, msgid)
     tree arg;
     const char *msgid;
{
  if (((enum tree_code) (arg)->common.code) == COMPONENT_REF)
    {
      if (((((((arg)->exp.operands[0]))->common.type))->common.readonly_flag))
        readonly_warning (((arg)->exp.operands[0]), msgid);
      else
        pedwarn ("%s of read-only member `%s'", (msgid),
                 ((const char *) (((((arg)->exp.operands[1]))->decl.name))->identifier.id.str));
    }
  else if (((enum tree_code) (arg)->common.code) == VAR_DECL)
    pedwarn ("%s of read-only variable `%s'", (msgid),
             ((const char *) (((arg)->decl.name))->identifier.id.str));
  else
    pedwarn ("%s of read-only location", (msgid));
}





int
mark_addressable (exp)
     tree exp;
{
  tree x = exp;
  while (1)
    switch (((enum tree_code) (x)->common.code))
      {
      case COMPONENT_REF:
        if (((((((x)->exp.operands[1])))->decl.lang_flag_4) == 1))
          {
            error ("cannot take address of bit-field `%s'",
                   ((const char *) (((((x)->exp.operands[1]))->decl.name))->identifier.id.str));
            return 0;
          }



      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
        x = ((x)->exp.operands[0]);
        break;

      case COMPOUND_LITERAL_EXPR:
      case CONSTRUCTOR:
        ((x)->common.addressable_flag) = 1;
        return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
        if (((x)->decl.regdecl_flag) && !((x)->common.addressable_flag)
            && ((x)->decl.nonlocal_flag))
          {
            if (((x)->common.public_flag))
              {
                error ("global register variable `%s' used in nested function",
                       ((const char *) (((x)->decl.name))->identifier.id.str));
                return 0;
              }
            pedwarn ("register variable `%s' used in nested function",
                     ((const char *) (((x)->decl.name))->identifier.id.str));
          }
        else if (((x)->decl.regdecl_flag) && !((x)->common.addressable_flag))
          {
            if (((x)->common.public_flag))
              {
                error ("address of global register variable `%s' requested",
                       ((const char *) (((x)->decl.name))->identifier.id.str));
                return 0;
              }






            else if (((((x)->common.type))->common.lang_flag_2))
              {
                error ("cannot put object with volatile field into register");
                return 0;
              }

            pedwarn ("address of register variable `%s' requested",
                     ((const char *) (((x)->decl.name))->identifier.id.str));
          }
        put_var_into_stack (x);


      case FUNCTION_DECL:
        ((x)->common.addressable_flag) = 1;





      default:
        return 1;
    }
}



tree
build_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  tree type1;
  tree type2;
  enum tree_code code1;
  enum tree_code code2;
  tree result_type = ((void *)0);
  tree orig_op1 = op1, orig_op2 = op2;

  ifexp = truthvalue_conversion (default_conversion (ifexp));
# 3408 "c-typeck.c"
  if (((enum tree_code) (((op1)->common.type))->common.code) != VOID_TYPE)
    op1 = default_conversion (op1);
  if (((enum tree_code) (((op2)->common.type))->common.code) != VOID_TYPE)
    op2 = default_conversion (op2);

  if (((enum tree_code) (ifexp)->common.code) == ERROR_MARK
      || ((enum tree_code) (((op1)->common.type))->common.code) == ERROR_MARK
      || ((enum tree_code) (((op2)->common.type))->common.code) == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];

  type1 = ((op1)->common.type);
  code1 = ((enum tree_code) (type1)->common.code);
  type2 = ((op2)->common.type);
  code2 = ((enum tree_code) (type2)->common.code);



  if (((type1)->type.main_variant) == ((type2)->type.main_variant))
    {
      if (type1 == type2)
        result_type = type1;
      else
        result_type = ((type1)->type.main_variant);
    }
  else if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE
            || code1 == COMPLEX_TYPE)
           && (code2 == INTEGER_TYPE || code2 == REAL_TYPE
               || code2 == COMPLEX_TYPE))
    {
      result_type = common_type (type1, type2);






      if ((warn_sign_compare < 0 ? extra_warnings : warn_sign_compare)
          && !skip_evaluation)
        {
          int unsigned_op1 = ((((orig_op1)->common.type))->common.unsigned_flag);
          int unsigned_op2 = ((((orig_op2)->common.type))->common.unsigned_flag);

          if (unsigned_op1 ^ unsigned_op2)
            {



              if (! ((result_type)->common.unsigned_flag))
                        ;



              else if ((unsigned_op2 && tree_expr_nonnegative_p (op1))
                       || (unsigned_op1 && tree_expr_nonnegative_p (op2)))
                        ;
              else
                warning ("signed and unsigned type in conditional expression");
            }
        }
    }
  else if (code1 == VOID_TYPE || code2 == VOID_TYPE)
    {
      if (pedantic && (code1 != VOID_TYPE || code2 != VOID_TYPE))
        pedwarn ("ISO C forbids conditional expr with only one void side");
      result_type = global_trees[TI_VOID_TYPE];
    }
  else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
    {
      if (comp_target_types (type1, type2))
        result_type = common_type (type1, type2);
      else if (integer_zerop (op1) && ((type1)->common.type) == global_trees[TI_VOID_TYPE]
               && ((enum tree_code) (orig_op1)->common.code) != NOP_EXPR)
        result_type = qualify_type (type2, type1);
      else if (integer_zerop (op2) && ((type2)->common.type) == global_trees[TI_VOID_TYPE]
               && ((enum tree_code) (orig_op2)->common.code) != NOP_EXPR)
        result_type = qualify_type (type1, type2);
      else if ((((enum tree_code) (((type1)->common.type))->common.code) == VOID_TYPE))
        {
          if (pedantic && ((enum tree_code) (((type2)->common.type))->common.code) == FUNCTION_TYPE)
            pedwarn ("ISO C forbids conditional expr between `void *' and function pointer");
          result_type = build_pointer_type (qualify_type (((type1)->common.type),
                                                          ((type2)->common.type)));
        }
      else if ((((enum tree_code) (((type2)->common.type))->common.code) == VOID_TYPE))
        {
          if (pedantic && ((enum tree_code) (((type1)->common.type))->common.code) == FUNCTION_TYPE)
            pedwarn ("ISO C forbids conditional expr between `void *' and function pointer");
          result_type = build_pointer_type (qualify_type (((type2)->common.type),
                                                          ((type1)->common.type)));
        }
      else
        {
          pedwarn ("pointer type mismatch in conditional expression");
          result_type = build_pointer_type (global_trees[TI_VOID_TYPE]);
        }
    }
  else if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
    {
      if (! integer_zerop (op2))
        pedwarn ("pointer/integer type mismatch in conditional expression");
      else
        {
          op2 = global_trees[TI_NULL_POINTER];
        }
      result_type = type1;
    }
  else if (code2 == POINTER_TYPE && code1 == INTEGER_TYPE)
    {
      if (!integer_zerop (op1))
        pedwarn ("pointer/integer type mismatch in conditional expression");
      else
        {
          op1 = global_trees[TI_NULL_POINTER];
        }
      result_type = type2;
    }

  if (!result_type)
    {
      if (flag_cond_mismatch)
        result_type = global_trees[TI_VOID_TYPE];
      else
        {
          error ("type mismatch in conditional expression");
          return global_trees[TI_ERROR_MARK];
        }
    }


  result_type
    = build_qualified_type ((result_type), ((((op1)->common.readonly_flag) || ((op2)->common.readonly_flag)) ? 0x1 : 0) | ((((op1)->common.volatile_flag) || ((op2)->common.volatile_flag)) ? 0x2 : 0));



  if (result_type != ((op1)->common.type))
    op1 = convert_and_check (result_type, op1);
  if (result_type != ((op2)->common.type))
    op2 = convert_and_check (result_type, op2);

  if (((enum tree_code) (ifexp)->common.code) == INTEGER_CST)
    return pedantic_non_lvalue (integer_zerop (ifexp) ? op2 : op1);

  return fold (build (COND_EXPR, result_type, ifexp, op1, op2));
}




tree
build_compound_expr (list)
     tree list;
{
  return internal_build_compound_expr (list, 1);
}

static tree
internal_build_compound_expr (list, first_p)
     tree list;
     int first_p;
{
  tree rest;

  if (((list)->common.chain) == 0)
    {


      if (!first_p)
        ((list)->list.value)
          = default_function_array_conversion (((list)->list.value));
# 3587 "c-typeck.c"
      if (!first_p && integer_zerop (((list)->list.value)))
        return non_lvalue (((list)->list.value));
      return ((list)->list.value);
    }

  rest = internal_build_compound_expr (((list)->common.chain), 0);

  if (! ((((list)->list.value))->common.side_effects_flag))
    {



      if ((extra_warnings || warn_unused_value)
           && ! (((enum tree_code) (((list)->list.value))->common.code) == CONVERT_EXPR
                && (((enum tree_code) (((((list)->list.value))->common.type))->common.code) == VOID_TYPE)))
        warning ("left-hand operand of comma expression has no effect");



      if (! pedantic)
        return rest;
    }





  else if (warn_unused_value)
    warn_if_unused_value (((list)->list.value));

  return build (COMPOUND_EXPR, ((rest)->common.type), ((list)->list.value), rest);
}



tree
build_c_cast (type, expr)
     tree type;
     tree expr;
{
  tree value = expr;

  if (type == global_trees[TI_ERROR_MARK] || expr == global_trees[TI_ERROR_MARK])
    return global_trees[TI_ERROR_MARK];
  type = ((type)->type.main_variant);







  if (((enum tree_code) (type)->common.code) == ARRAY_TYPE)
    {
      error ("cast specifies array type");
      return global_trees[TI_ERROR_MARK];
    }

  if (((enum tree_code) (type)->common.code) == FUNCTION_TYPE)
    {
      error ("cast specifies function type");
      return global_trees[TI_ERROR_MARK];
    }

  if (type == ((((value)->common.type))->type.main_variant))
    {
      if (pedantic)
        {
          if (((enum tree_code) (type)->common.code) == RECORD_TYPE
              || ((enum tree_code) (type)->common.code) == UNION_TYPE)
            pedwarn ("ISO C forbids casting nonscalar to the same type");
        }
    }
  else if (((enum tree_code) (type)->common.code) == UNION_TYPE)
    {
      tree field;
      value = default_function_array_conversion (value);

      for (field = ((type)->type.values); field; field = ((field)->common.chain))
        if (comptypes (((((field)->common.type))->type.main_variant),
                       ((((value)->common.type))->type.main_variant)))
          break;

      if (field)
        {
          const char *name;
          tree t;

          if (pedantic)
            pedwarn ("ISO C forbids casts to union type");
          if (((type)->type.name) != 0)
            {
              if (((enum tree_code) (((type)->type.name))->common.code) == IDENTIFIER_NODE)
                name = ((const char *) (((type)->type.name))->identifier.id.str);
              else
                name = ((const char *) (((((type)->type.name))->decl.name))->identifier.id.str);
            }
          else
            name = "";
          t = digest_init (type, build (CONSTRUCTOR, type, (tree) ((void *)0),
                                        build_tree_list (field, value)),
                           0, 0);
          ((t)->common.constant_flag) = ((value)->common.constant_flag);
          return t;
        }
      error ("cast to union type from type not present in union");
      return global_trees[TI_ERROR_MARK];
    }
  else
    {
      tree otype, ovalue;



      if (type == global_trees[TI_VOID_TYPE])
        return build1 (CONVERT_EXPR, type, value);



      value = default_function_array_conversion (value);
      otype = ((value)->common.type);



      if (warn_cast_qual
          && ((enum tree_code) (type)->common.code) == POINTER_TYPE
          && ((enum tree_code) (otype)->common.code) == POINTER_TYPE)
        {
          tree in_type = type;
          tree in_otype = otype;
          int added = 0;
          int discarded = 0;





          do
            {
              in_otype = ((in_otype)->common.type);
              in_type = ((in_type)->common.type);





              if (((enum tree_code) (in_otype)->common.code) == FUNCTION_TYPE
                  && ((enum tree_code) (in_type)->common.code) == FUNCTION_TYPE)
                added |= (((((in_type)->common.readonly_flag) * 0x1) | (((in_type)->common.volatile_flag) * 0x2) | (((in_type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (in_type)->common.code) == RECORD_TYPE && ((in_type)->common.type)) * 0x8)) & ~((((in_otype)->common.readonly_flag) * 0x1) | (((in_otype)->common.volatile_flag) * 0x2) | (((in_otype)->type.restrict_flag) * 0x4) | ((((enum tree_code) (in_otype)->common.code) == RECORD_TYPE && ((in_otype)->common.type)) * 0x8)));
              else
                discarded |= (((((in_otype)->common.readonly_flag) * 0x1) | (((in_otype)->common.volatile_flag) * 0x2) | (((in_otype)->type.restrict_flag) * 0x4) | ((((enum tree_code) (in_otype)->common.code) == RECORD_TYPE && ((in_otype)->common.type)) * 0x8)) & ~((((in_type)->common.readonly_flag) * 0x1) | (((in_type)->common.volatile_flag) * 0x2) | (((in_type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (in_type)->common.code) == RECORD_TYPE && ((in_type)->common.type)) * 0x8)));
            }
          while (((enum tree_code) (in_type)->common.code) == POINTER_TYPE
                 && ((enum tree_code) (in_otype)->common.code) == POINTER_TYPE);

          if (added)
            warning ("cast adds new qualifiers to function type");

          if (discarded)


            warning ("cast discards qualifiers from pointer target type");
        }


      if (0 && warn_cast_align
          && ((enum tree_code) (type)->common.code) == POINTER_TYPE
          && ((enum tree_code) (otype)->common.code) == POINTER_TYPE
          && ((enum tree_code) (((otype)->common.type))->common.code) != VOID_TYPE
          && ((enum tree_code) (((otype)->common.type))->common.code) != FUNCTION_TYPE


          && !((((enum tree_code) (((otype)->common.type))->common.code) == UNION_TYPE
                || ((enum tree_code) (((otype)->common.type))->common.code) == RECORD_TYPE)
               && ((((otype)->common.type))->type.mode) == VOIDmode)
          && ((((type)->common.type))->type.align) > ((((otype)->common.type))->type.align))
        warning ("cast increases required alignment of target type");

      if (((enum tree_code) (type)->common.code) == INTEGER_TYPE
          && ((enum tree_code) (otype)->common.code) == POINTER_TYPE
          && ((type)->type.precision) != ((otype)->type.precision)
          && !((value)->common.constant_flag))
        warning ("cast from pointer to integer of different size");

      if (warn_bad_function_cast
          && ((enum tree_code) (value)->common.code) == CALL_EXPR
          && ((enum tree_code) (type)->common.code) != ((enum tree_code) (otype)->common.code))
        warning ("cast does not match function type");

      if (((enum tree_code) (type)->common.code) == POINTER_TYPE
          && ((enum tree_code) (otype)->common.code) == INTEGER_TYPE
          && ((type)->type.precision) != ((otype)->type.precision)

          && !((value)->common.constant_flag))
        warning ("cast to pointer from integer of different size");

      ovalue = value;
      value = convert (type, value);


      if (((enum tree_code) (value)->common.code) == INTEGER_CST)
        {
          ((value)->common.public_flag) = ((ovalue)->common.public_flag);
          ((value)->common.static_flag) = ((ovalue)->common.static_flag);
        }
    }


  if (pedantic && ((enum tree_code) (value)->common.code) == INTEGER_CST
      && ((enum tree_code) (expr)->common.code) == INTEGER_CST
      && ((enum tree_code) (((expr)->common.type))->common.code) != INTEGER_TYPE)
    value = non_lvalue (value);


  if (value == expr && pedantic)
    value = non_lvalue (value);

  return value;
}


tree
c_cast_expr (type, expr)
     tree type, expr;
{
  int saved_wsp = warn_strict_prototypes;



  if (((enum tree_code) (expr)->common.code) == INTEGER_CST)
    warn_strict_prototypes = 0;
  type = groktypename (type);
  warn_strict_prototypes = saved_wsp;

  return build_c_cast (type, expr);
}







tree
build_modify_expr (lhs, modifycode, rhs)
     tree lhs, rhs;
     enum tree_code modifycode;
{
  tree result;
  tree newrhs;
  tree lhstype = ((lhs)->common.type);
  tree olhstype = lhstype;


  lhs = require_complete_type (lhs);


  if (((enum tree_code) (lhs)->common.code) == ERROR_MARK || ((enum tree_code) (rhs)->common.code) == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];




  if (((enum tree_code) (rhs)->common.code) == NON_LVALUE_EXPR)
    rhs = ((rhs)->exp.operands[0]);

  newrhs = rhs;



  switch (((enum tree_code) (lhs)->common.code))
    {

    case COMPOUND_EXPR:
      pedantic_lvalue_warning (COMPOUND_EXPR);
      newrhs = build_modify_expr (((lhs)->exp.operands[1]), modifycode, rhs);
      if (((enum tree_code) (newrhs)->common.code) == ERROR_MARK)
        return global_trees[TI_ERROR_MARK];
      return build (COMPOUND_EXPR, lhstype,
                    ((lhs)->exp.operands[0]), newrhs);


    case COND_EXPR:
      pedantic_lvalue_warning (COND_EXPR);
      rhs = save_expr (rhs);
      {



        tree cond
          = build_conditional_expr (((lhs)->exp.operands[0]),
                                    build_modify_expr (((lhs)->exp.operands[1]),
                                                       modifycode, rhs),
                                    build_modify_expr (((lhs)->exp.operands[2]),
                                                       modifycode, rhs));
        if (((enum tree_code) (cond)->common.code) == ERROR_MARK)
          return cond;


        return build (COMPOUND_EXPR, ((lhs)->common.type),

                      convert (global_trees[TI_VOID_TYPE], rhs), cond);
      }
    default:
      break;
    }




  if (modifycode != NOP_EXPR)
    {
      lhs = stabilize_reference (lhs);
      newrhs = build_binary_op (modifycode, lhs, rhs, 1);
    }
# 3910 "c-typeck.c"
  switch (((enum tree_code) (lhs)->common.code))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      newrhs = default_function_array_conversion (newrhs);
      {
        tree inner_lhs = ((lhs)->exp.operands[0]);
        tree result;
        result = build_modify_expr (inner_lhs, NOP_EXPR,
                                    convert (((inner_lhs)->common.type),
                                             convert (lhstype, newrhs)));
        if (((enum tree_code) (result)->common.code) == ERROR_MARK)
          return result;
        pedantic_lvalue_warning (CONVERT_EXPR);
        return convert (((lhs)->common.type), result);
      }

    default:
      break;
    }




  if (!lvalue_or_else (lhs, "invalid lvalue in assignment"))
    return global_trees[TI_ERROR_MARK];



  if (((lhs)->common.readonly_flag) || ((lhstype)->common.readonly_flag)
      || ((((enum tree_code) (lhstype)->common.code) == RECORD_TYPE
           || ((enum tree_code) (lhstype)->common.code) == UNION_TYPE)
          && ((lhstype)->common.lang_flag_1)))
    readonly_warning (lhs, "assignment");






  if (((enum tree_code) (lhs)->common.code) == COMPONENT_REF
      && (((enum tree_code) (lhstype)->common.code) == INTEGER_TYPE
          || ((enum tree_code) (lhstype)->common.code) == BOOLEAN_TYPE
          || ((enum tree_code) (lhstype)->common.code) == REAL_TYPE
          || ((enum tree_code) (lhstype)->common.code) == ENUMERAL_TYPE))
    lhstype = ((get_unwidened (lhs, 0))->common.type);




  if (lhstype != ((lhs)->common.type))
    {
      lhs = copy_node (lhs);
      ((lhs)->common.type) = lhstype;
    }



  newrhs = convert_for_assignment (lhstype, newrhs, ("assignment"),
                                   (tree) ((void *)0), (tree) ((void *)0), 0);
  if (((enum tree_code) (newrhs)->common.code) == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];



  result = build (MODIFY_EXPR, lhstype, lhs, newrhs);
  ((result)->common.side_effects_flag) = 1;






  if (olhstype == ((result)->common.type))
    return result;
  return convert_for_assignment (olhstype, result, ("assignment"),
                                 (tree) ((void *)0), (tree) ((void *)0), 0);
}
# 4007 "c-typeck.c"
static tree
convert_for_assignment (type, rhs, errtype, fundecl, funname, parmnum)
     tree type, rhs;
     const char *errtype;
     tree fundecl, funname;
     int parmnum;
{
  enum tree_code codel = ((enum tree_code) (type)->common.code);
  tree rhstype;
  enum tree_code coder;




  if (((enum tree_code) (rhs)->common.code) == NON_LVALUE_EXPR)
    rhs = ((rhs)->exp.operands[0]);

  if (((enum tree_code) (((rhs)->common.type))->common.code) == ARRAY_TYPE
      || ((enum tree_code) (((rhs)->common.type))->common.code) == FUNCTION_TYPE)
    rhs = default_conversion (rhs);
  else if (optimize && ((enum tree_code) (rhs)->common.code) == VAR_DECL)
    rhs = decl_constant_value_for_broken_optimization (rhs);

  rhstype = ((rhs)->common.type);
  coder = ((enum tree_code) (rhstype)->common.code);

  if (coder == ERROR_MARK)
    return global_trees[TI_ERROR_MARK];

  if (((type)->type.main_variant) == ((rhstype)->type.main_variant))
    {
      overflow_warning (rhs);


      maybe_objc_comptypes (type, rhstype, 0);
      return rhs;
    }

  if (coder == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return global_trees[TI_ERROR_MARK];
    }



  if (codel == REFERENCE_TYPE
      && comptypes (((type)->common.type), ((rhs)->common.type)) == 1)
    {
      if (mark_addressable (rhs) == 0)
        return global_trees[TI_ERROR_MARK];
      rhs = build1 (ADDR_EXPR, build_pointer_type (((rhs)->common.type)), rhs);






      if (((((rhs)->common.type))->common.type) != ((type)->common.type))
        rhs = build1 (NOP_EXPR, build_pointer_type (((type)->common.type)), rhs);

      rhs = build1 (NOP_EXPR, type, rhs);
      return rhs;
    }

  else if ((codel == INTEGER_TYPE || codel == REAL_TYPE
            || codel == ENUMERAL_TYPE || codel == COMPLEX_TYPE
            || codel == BOOLEAN_TYPE)
           && (coder == INTEGER_TYPE || coder == REAL_TYPE
               || coder == ENUMERAL_TYPE || coder == COMPLEX_TYPE
               || coder == BOOLEAN_TYPE))
    return convert_and_check (type, rhs);



  else if (codel == UNION_TYPE && ((type)->type.transparent_union_flag) && ! errtype)
    {
      tree memb_types;
      tree marginal_memb_type = 0;

      for (memb_types = ((type)->type.values); memb_types;
           memb_types = ((memb_types)->common.chain))
        {
          tree memb_type = ((memb_types)->common.type);

          if (comptypes (((memb_type)->type.main_variant),
                         ((rhstype)->type.main_variant)))
            break;

          if (((enum tree_code) (memb_type)->common.code) != POINTER_TYPE)
            continue;

          if (coder == POINTER_TYPE)
            {
              tree ttl = ((memb_type)->common.type);
              tree ttr = ((rhstype)->common.type);





              if ((((enum tree_code) (ttl)->common.code) == VOID_TYPE) || (((enum tree_code) (ttr)->common.code) == VOID_TYPE)
                  || comp_target_types (memb_type, rhstype))
                {

                  if (((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)) == ((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8))
                      || ((((enum tree_code) (ttr)->common.code) == FUNCTION_TYPE
                           && ((enum tree_code) (ttl)->common.code) == FUNCTION_TYPE)
                          ? ((((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)) | ((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8)))
                             == ((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8)))
                          : ((((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)) | ((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8)))
                             == ((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)))))
                    break;


                  if (! marginal_memb_type)
                    marginal_memb_type = memb_type;
                }
            }


          if (integer_zerop (rhs)
              || (((enum tree_code) (rhs)->common.code) == NOP_EXPR
                  && integer_zerop (((rhs)->exp.operands[0]))))
            {
              rhs = global_trees[TI_NULL_POINTER];
              break;
            }
        }

      if (memb_types || marginal_memb_type)
        {
          if (! memb_types)
            {


              tree ttl = ((marginal_memb_type)->common.type);
              tree ttr = ((rhstype)->common.type);



              if (((enum tree_code) (ttr)->common.code) == FUNCTION_TYPE
                  && ((enum tree_code) (ttl)->common.code) == FUNCTION_TYPE)
                {





                  if (((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)) & ~((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8)))
                    warn_for_assignment ("%s makes qualified function pointer from unqualified",
                                         errtype, funname, parmnum);
                }
              else if (((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8)) & ~((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)))
                warn_for_assignment ("%s discards qualifiers from pointer target type",
                                     errtype, funname,
                                     parmnum);
            }

          if (pedantic && ! ((fundecl)->decl.in_system_header_flag))
            pedwarn ("ISO C prohibits argument conversion to union type");

          return build1 (NOP_EXPR, type, rhs);
        }
    }


  else if ((codel == POINTER_TYPE || codel == REFERENCE_TYPE)
           && (coder == POINTER_TYPE || coder == REFERENCE_TYPE))
    {
      tree ttl = ((type)->common.type);
      tree ttr = ((rhstype)->common.type);




      if ((((enum tree_code) (ttl)->common.code) == VOID_TYPE) || (((enum tree_code) (ttr)->common.code) == VOID_TYPE)
          || comp_target_types (type, rhstype)
          || (unsigned_type (((ttl)->type.main_variant))
              == unsigned_type (((ttr)->type.main_variant))))
        {
          if (pedantic
              && (((((enum tree_code) (ttl)->common.code) == VOID_TYPE) && ((enum tree_code) (ttr)->common.code) == FUNCTION_TYPE)
                  ||
                  ((((enum tree_code) (ttr)->common.code) == VOID_TYPE)


                   && (!integer_zerop (rhs) || ((enum tree_code) (rhs)->common.code) == NOP_EXPR)
                   && ((enum tree_code) (ttl)->common.code) == FUNCTION_TYPE)))
            warn_for_assignment ("ISO C forbids %s between function pointer and `void *'",
                                 errtype, funname, parmnum);


          else if (((enum tree_code) (ttr)->common.code) != FUNCTION_TYPE
                   && ((enum tree_code) (ttl)->common.code) != FUNCTION_TYPE)
            {
              if (((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8)) & ~((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)))
                warn_for_assignment ("%s discards qualifiers from pointer target type",
                                     errtype, funname, parmnum);


              else if ((((enum tree_code) (ttl)->common.code) == VOID_TYPE) || (((enum tree_code) (ttr)->common.code) == VOID_TYPE)
                       || comp_target_types (type, rhstype))
                ;

              else if (pedantic)
                warn_for_assignment ("pointer targets in %s differ in signedness",
                                     errtype, funname, parmnum);
            }
          else if (((enum tree_code) (ttl)->common.code) == FUNCTION_TYPE
                   && ((enum tree_code) (ttr)->common.code) == FUNCTION_TYPE)
            {




              if (((((ttl)->common.readonly_flag) * 0x1) | (((ttl)->common.volatile_flag) * 0x2) | (((ttl)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttl)->common.code) == RECORD_TYPE && ((ttl)->common.type)) * 0x8)) & ~((((ttr)->common.readonly_flag) * 0x1) | (((ttr)->common.volatile_flag) * 0x2) | (((ttr)->type.restrict_flag) * 0x4) | ((((enum tree_code) (ttr)->common.code) == RECORD_TYPE && ((ttr)->common.type)) * 0x8)))
                warn_for_assignment ("%s makes qualified function pointer from unqualified",
                                     errtype, funname, parmnum);
            }
        }
      else
        warn_for_assignment ("%s from incompatible pointer type",
                             errtype, funname, parmnum);
      return convert (type, rhs);
    }
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {



      if (! (((enum tree_code) (rhs)->common.code) == INTEGER_CST && integer_zerop (rhs))
          &&
          ! (((enum tree_code) (rhs)->common.code) == NOP_EXPR
             && ((enum tree_code) (((rhs)->common.type))->common.code) == INTEGER_TYPE
             && ((enum tree_code) (((rhs)->exp.operands[0]))->common.code) == INTEGER_CST
             && integer_zerop (((rhs)->exp.operands[0]))))
        {
          warn_for_assignment ("%s makes pointer from integer without a cast",
                               errtype, funname, parmnum);
          return convert (type, rhs);
        }
      return global_trees[TI_NULL_POINTER];
    }
  else if (codel == INTEGER_TYPE && coder == POINTER_TYPE)
    {
      warn_for_assignment ("%s makes integer from pointer without a cast",
                           errtype, funname, parmnum);
      return convert (type, rhs);
    }
  else if (codel == BOOLEAN_TYPE && coder == POINTER_TYPE)
    return convert (type, rhs);

  if (!errtype)
    {
      if (funname)
        {
          tree selector = maybe_building_objc_message_expr ();

          if (selector && parmnum > 2)
            error ("incompatible type for argument %d of `%s'",
                   parmnum - 2, ((const char *) (selector)->identifier.id.str));
          else
            error ("incompatible type for argument %d of `%s'",
                   parmnum, ((const char *) (funname)->identifier.id.str));
        }
      else
        error ("incompatible type for argument %d of indirect function call",
               parmnum);
    }
  else
    error ("incompatible types in %s", errtype);

  return global_trees[TI_ERROR_MARK];
}



tree
c_convert_parm_for_inlining (parm, value, fn)
     tree parm, value, fn;
{
  tree ret, type;



  if (! value || ((((fn)->common.type))->type.values))
    return value;

  type = ((parm)->common.type);
  ret = convert_for_assignment (type, value,
                                (char *) 0 , fn,
                                ((fn)->decl.name), 0);
  if ((!(target_flags & 0x02000000))
      && (((enum tree_code) (type)->common.code) == INTEGER_TYPE || ((enum tree_code) (type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (type)->common.code) == CHAR_TYPE)
      && (((type)->type.precision) < ((integer_types[itk_int])->type.precision)))
    ret = default_conversion (ret);
  return ret;
}







static void
warn_for_assignment (msgid, opname, function, argnum)
     const char *msgid;
     const char *opname;
     tree function;
     int argnum;
{



  if (opname == 0)
    {
      tree selector = maybe_building_objc_message_expr ();
      char * new_opname;

      if (selector && argnum > 2)
        {
          function = selector;
          argnum -= 2;
        }
      if (function)
        {

          const char *const argstring = ("passing arg %d of `%s'");
          new_opname = (char *) __builtin_alloca (((function)->identifier.id.len) + strlen (argstring) + 1 + 25 + 1);


          sprintf (new_opname, argstring, argnum,
                   ((const char *) (function)->identifier.id.str));
        }
      else
        {

          const char *const argnofun = ("passing arg %d of pointer to function");
          new_opname = (char *) __builtin_alloca (strlen (argnofun) + 1 + 25 + 1);
          sprintf (new_opname, argnofun, argnum);
        }
      opname = new_opname;
    }
  pedwarn (msgid, opname);
}







static tree
valid_compound_expr_initializer (value, endtype)
     tree value;
     tree endtype;
{
  if (((enum tree_code) (value)->common.code) == COMPOUND_EXPR)
    {
      if (valid_compound_expr_initializer (((value)->exp.operands[0]), endtype)
          == global_trees[TI_ERROR_MARK])
        return global_trees[TI_ERROR_MARK];
      return valid_compound_expr_initializer (((value)->exp.operands[1]),
                                              endtype);
    }
  else if (! ((value)->common.constant_flag)
           && ! initializer_constant_valid_p (value, endtype))
    return global_trees[TI_ERROR_MARK];
  else
    return value;
}






void
store_init_value (decl, init)
     tree decl, init;
{
  tree value, type;



  type = ((decl)->common.type);
  if (((enum tree_code) (type)->common.code) == ERROR_MARK)
    return;



  value = digest_init (type, init, ((decl)->common.static_flag),
                       ((decl)->common.static_flag) || (pedantic && !flag_isoc99));
# 4434 "c-typeck.c"
  if (warn_traditional && !in_system_header
      && (((enum tree_code) (((decl)->common.type))->common.code) == ARRAY_TYPE || ((enum tree_code) (((decl)->common.type))->common.code) == RECORD_TYPE || ((enum tree_code) (((decl)->common.type))->common.code) == UNION_TYPE || ((enum tree_code) (((decl)->common.type))->common.code) == QUAL_UNION_TYPE || ((enum tree_code) (((decl)->common.type))->common.code) == SET_TYPE) && ! ((decl)->common.static_flag))
    warning ("traditional C rejects automatic aggregate initialization");

  ((decl)->decl.initial) = value;


  while ((((enum tree_code) (value)->common.code) == NOP_EXPR || ((enum tree_code) (value)->common.code) == CONVERT_EXPR || ((enum tree_code) (value)->common.code) == NON_LVALUE_EXPR) && ((value)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((value)->common.type) == ((((value)->exp.operands[0]))->common.type))) (value) = ((value)->exp.operands[0]);
  constant_expression_warning (value);


  if (((enum tree_code) (type)->common.code) == ARRAY_TYPE
      && ((type)->type.values) == 0
      && value != global_trees[TI_ERROR_MARK])
    {
      tree inside_init = init;

      if (((enum tree_code) (init)->common.code) == NON_LVALUE_EXPR)
        inside_init = ((init)->exp.operands[0]);
      inside_init = fold (inside_init);

      if (((enum tree_code) (inside_init)->common.code) == COMPOUND_LITERAL_EXPR)
        {
          tree decl = ((((((inside_init))->exp.operands[0])))->exp.operands[0]);

          if (((((decl)->common.type))->type.values))
            {



              ((type)->type.values) = ((((decl)->common.type))->type.values);
              layout_type (type);
              layout_decl (decl, 0);
            }
        }
    }
}






struct spelling
{
  int kind;
  union
    {
      int i;
      const char *s;
    } u;
};





static struct spelling *spelling;
static struct spelling *spelling_base;
static int spelling_size;
# 4537 "c-typeck.c"
static void
push_string (string)
     const char *string;
{
  { int depth = (spelling - spelling_base); if (depth >= spelling_size) { spelling_size += 10; if (spelling_base == 0) spelling_base = (struct spelling *) xmalloc (spelling_size * sizeof (struct spelling)); else spelling_base = (struct spelling *) xrealloc (spelling_base, spelling_size * sizeof (struct spelling)); (spelling = spelling_base + (depth)); } spelling->kind = (1); spelling->u.s = (string); spelling++; };
}



static void
push_member_name (decl)
     tree decl;

{
  const char *const string
    = ((decl)->decl.name) ? ((const char *) (((decl)->decl.name))->identifier.id.str) : "<anonymous>";
  { int depth = (spelling - spelling_base); if (depth >= spelling_size) { spelling_size += 10; if (spelling_base == 0) spelling_base = (struct spelling *) xmalloc (spelling_size * sizeof (struct spelling)); else spelling_base = (struct spelling *) xrealloc (spelling_base, spelling_size * sizeof (struct spelling)); (spelling = spelling_base + (depth)); } spelling->kind = (2); spelling->u.s = (string); spelling++; };
}



static void
push_array_bounds (bounds)
     int bounds;
{
  { int depth = (spelling - spelling_base); if (depth >= spelling_size) { spelling_size += 10; if (spelling_base == 0) spelling_base = (struct spelling *) xmalloc (spelling_size * sizeof (struct spelling)); else spelling_base = (struct spelling *) xrealloc (spelling_base, spelling_size * sizeof (struct spelling)); (spelling = spelling_base + (depth)); } spelling->kind = (3); spelling->u.i = (bounds); spelling++; };
}



static int
spelling_length ()
{
  int size = 0;
  struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    {
      if (p->kind == 3)
        size += 25;
      else
        size += strlen (p->u.s) + 1;
    }

  return size;
}



static char *
print_spelling (buffer)
     char *buffer;
{
  char *d = buffer;
  struct spelling *p;

  for (p = spelling_base; p < spelling; p++)
    if (p->kind == 3)
      {
        sprintf (d, "[%d]", p->u.i);
        d += strlen (d);
      }
    else
      {
        const char *s;
        if (p->kind == 2)
          *d++ = '.';
        for (s = p->u.s; (*d = *s++); d++)
          ;
      }
  *d++ = '\0';
  return buffer;
}





void
error_init (msgid)
     const char *msgid;
{
  char *ofwhat;

  error ("%s", (msgid));
  ofwhat = print_spelling ((char *) __builtin_alloca (spelling_length () + 1));
  if (*ofwhat)
    error ("(near initialization for `%s')", ofwhat);
}





void
pedwarn_init (msgid)
     const char *msgid;
{
  char *ofwhat;

  pedwarn ("%s", (msgid));
  ofwhat = print_spelling ((char *) __builtin_alloca (spelling_length () + 1));
  if (*ofwhat)
    pedwarn ("(near initialization for `%s')", ofwhat);
}





static void
warning_init (msgid)
     const char *msgid;
{
  char *ofwhat;

  warning ("%s", (msgid));
  ofwhat = print_spelling ((char *) __builtin_alloca (spelling_length () + 1));
  if (*ofwhat)
    warning ("(near initialization for `%s')", ofwhat);
}
# 4666 "c-typeck.c"
static tree
digest_init (type, init, require_constant, constructor_constant)
     tree type, init;
     int require_constant, constructor_constant;
{
  enum tree_code code = ((enum tree_code) (type)->common.code);
  tree inside_init = init;

  if (type == global_trees[TI_ERROR_MARK]
      || init == global_trees[TI_ERROR_MARK]
      || ((init)->common.type) == global_trees[TI_ERROR_MARK])
    return global_trees[TI_ERROR_MARK];




  if (((enum tree_code) (init)->common.code) == NON_LVALUE_EXPR)
    inside_init = ((init)->exp.operands[0]);

  inside_init = fold (inside_init);




  if (code == ARRAY_TYPE)
    {
      tree typ1 = ((((type)->common.type))->type.main_variant);
      if ((typ1 == integer_types[itk_char]
           || typ1 == integer_types[itk_signed_char]
           || typ1 == integer_types[itk_unsigned_char]
           || typ1 == c_global_trees[CTI_UNSIGNED_WCHAR_TYPE]
           || typ1 == c_global_trees[CTI_SIGNED_WCHAR_TYPE])
          && ((inside_init && ((enum tree_code) (inside_init)->common.code) == STRING_CST)))
        {
          if (comptypes (((((inside_init)->common.type))->type.main_variant),
                         ((type)->type.main_variant)))
            return inside_init;

          if ((((((((inside_init)->common.type))->common.type))->type.main_variant)
               != integer_types[itk_char])
              && ((typ1)->type.precision) == ((integer_types[itk_char])->type.precision))
            {
              error_init ("char-array initialized from wide string");
              return global_trees[TI_ERROR_MARK];
            }
          if ((((((((inside_init)->common.type))->common.type))->type.main_variant)
               == integer_types[itk_char])
              && ((typ1)->type.precision) != ((integer_types[itk_char])->type.precision))
            {
              error_init ("int-array initialized from non-wide string");
              return global_trees[TI_ERROR_MARK];
            }

          ((inside_init)->common.type) = type;
          if (((type)->type.values) != 0
              && ((type)->type.size) != 0
              && ((enum tree_code) (((type)->type.size))->common.code) == INTEGER_CST



              && 0 > compare_tree_int (((type)->type.size_unit),
                                       ((inside_init)->string.length)
                                       - ((((typ1)->type.precision)
                                           != ((integer_types[itk_char])->type.precision))
                                          ? (((c_global_trees[CTI_WCHAR_TYPE])->type.precision)
                                             / 8)
                                          : 1)))
            pedwarn_init ("initializer-string for array of chars is too long");

          return inside_init;
        }
    }




  if (inside_init && ((inside_init)->common.type) != 0
      && (comptypes (((((inside_init)->common.type))->type.main_variant),
                     ((type)->type.main_variant))
          || (code == ARRAY_TYPE
              && comptypes (((inside_init)->common.type), type))
          || (code == VECTOR_TYPE
              && comptypes (((inside_init)->common.type), type))
          || (code == POINTER_TYPE
              && (((enum tree_code) (((inside_init)->common.type))->common.code) == ARRAY_TYPE
                  || ((enum tree_code) (((inside_init)->common.type))->common.code) == FUNCTION_TYPE)
              && comptypes (((((inside_init)->common.type))->common.type),
                            ((type)->common.type)))))
    {
      if (code == POINTER_TYPE)
        inside_init = default_function_array_conversion (inside_init);

      if (require_constant && !flag_isoc99
          && ((enum tree_code) (inside_init)->common.code) == COMPOUND_LITERAL_EXPR)
        {



          tree decl = ((((((inside_init))->exp.operands[0])))->exp.operands[0]);
          inside_init = ((decl)->decl.initial);
        }

      if (code == ARRAY_TYPE && ((enum tree_code) (inside_init)->common.code) != STRING_CST
          && ((enum tree_code) (inside_init)->common.code) != CONSTRUCTOR)
        {
          error_init ("array initialized from non-constant array expression");
          return global_trees[TI_ERROR_MARK];
        }

      if (optimize && ((enum tree_code) (inside_init)->common.code) == VAR_DECL)
        inside_init = decl_constant_value_for_broken_optimization (inside_init);




      if (require_constant && pedantic
          && ((enum tree_code) (inside_init)->common.code) == COMPOUND_EXPR)
        {
          inside_init
            = valid_compound_expr_initializer (inside_init,
                                               ((inside_init)->common.type));
          if (inside_init == global_trees[TI_ERROR_MARK])
            error_init ("initializer element is not constant");
          else
            pedwarn_init ("initializer element is not constant");
          if (flag_pedantic_errors)
            inside_init = global_trees[TI_ERROR_MARK];
        }
      else if (require_constant
               && (!((inside_init)->common.constant_flag)







                   || !initializer_constant_valid_p (inside_init,
                                                     ((inside_init)->common.type))))
        {
          error_init ("initializer element is not constant");
          inside_init = global_trees[TI_ERROR_MARK];
        }

      return inside_init;
    }



  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE || code == BOOLEAN_TYPE || code == COMPLEX_TYPE)
    {



      inside_init
        = convert_for_assignment (type, init, ("initialization"),
                                  (tree) ((void *)0), (tree) ((void *)0), 0);

      if (require_constant && ! ((inside_init)->common.constant_flag))
        {
          error_init ("initializer element is not constant");
          inside_init = global_trees[TI_ERROR_MARK];
        }
      else if (require_constant
               && initializer_constant_valid_p (inside_init, ((inside_init)->common.type)) == 0)
        {
          error_init ("initializer element is not computable at load time");
          inside_init = global_trees[TI_ERROR_MARK];
        }

      return inside_init;
    }



  if ((((type)->type.size) != (tree) ((void *)0)) && ((enum tree_code) (((type)->type.size))->common.code) != INTEGER_CST)
    {
      error_init ("variable-sized object may not be initialized");
      return global_trees[TI_ERROR_MARK];
    }



  if (flag_traditional)
    {
      tree top = 0, prev = 0, otype = type;
      while (((enum tree_code) (type)->common.code) == RECORD_TYPE
             || ((enum tree_code) (type)->common.code) == ARRAY_TYPE
             || ((enum tree_code) (type)->common.code) == QUAL_UNION_TYPE
             || ((enum tree_code) (type)->common.code) == UNION_TYPE)
        {
          tree temp = build (CONSTRUCTOR, type, (tree) ((void *)0), (tree) ((void *)0));
          if (prev == 0)
            top = temp;
          else
            ((prev)->exp.operands[1]) = build_tree_list ((tree) ((void *)0), temp);
          prev = temp;
          if (((enum tree_code) (type)->common.code) == ARRAY_TYPE)
            type = ((type)->common.type);
          else if (((type)->type.values))
            type = ((((type)->type.values))->common.type);
          else
            {
              error_init ("invalid initializer");
              return global_trees[TI_ERROR_MARK];
            }
        }

      if (otype != type)
        {
          ((prev)->exp.operands[1])
            = build_tree_list ((tree) ((void *)0),
                               digest_init (type, init, require_constant,
                                            constructor_constant));
          return top;
        }
      else
        return global_trees[TI_ERROR_MARK];
    }
  error_init ("invalid initializer");
  return global_trees[TI_ERROR_MARK];
}





static tree constructor_type;



static tree constructor_fields;



static tree constructor_index;


static tree constructor_max_index;


static tree constructor_unfilled_fields;



static tree constructor_unfilled_index;



static tree constructor_bit_index;




static tree constructor_elements;



static int constructor_incremental;


static int constructor_constant;


static int constructor_simple;


static int constructor_erroneous;


static int constructor_subconstants_deferred;




struct init_node
{
  struct init_node *left, *right;
  struct init_node *parent;
  int balance;
  tree purpose;
  tree value;
};






static struct init_node *constructor_pending_elts;


static int constructor_depth;


int constructor_no_implicit = 0;

static int require_constant_value;
static int require_constant_elements;




static tree constructor_decl;


static const char *constructor_asmspec;


static int constructor_top_level;


static int constructor_designated;


static int designator_depth;


static int designator_errorneous;






struct constructor_range_stack;

struct constructor_stack
{
  struct constructor_stack *next;
  tree type;
  tree fields;
  tree index;
  tree max_index;
  tree unfilled_index;
  tree unfilled_fields;
  tree bit_index;
  tree elements;
  struct init_node *pending_elts;
  int offset;
  int depth;


  tree replacement_value;
  struct constructor_range_stack *range_stack;
  char constant;
  char simple;
  char implicit;
  char erroneous;
  char outer;
  char incremental;
  char designated;
};

struct constructor_stack *constructor_stack;




struct constructor_range_stack
{
  struct constructor_range_stack *next, *prev;
  struct constructor_stack *stack;
  tree range_start;
  tree index;
  tree range_end;
  tree fields;
};

struct constructor_range_stack *constructor_range_stack;





struct initializer_stack
{
  struct initializer_stack *next;
  tree decl;
  const char *asmspec;
  struct constructor_stack *constructor_stack;
  struct constructor_range_stack *constructor_range_stack;
  tree elements;
  struct spelling *spelling;
  struct spelling *spelling_base;
  int spelling_size;
  char top_level;
  char require_constant_value;
  char require_constant_elements;
  char deferred;
};

struct initializer_stack *initializer_stack;



void
start_init (decl, asmspec_tree, top_level)
     tree decl;
     tree asmspec_tree;
     int top_level;
{
  const char *locus;
  struct initializer_stack *p
    = (struct initializer_stack *) xmalloc (sizeof (struct initializer_stack));
  const char *asmspec = 0;

  if (asmspec_tree)
    asmspec = ((asmspec_tree)->string.pointer);

  p->decl = constructor_decl;
  p->asmspec = constructor_asmspec;
  p->require_constant_value = require_constant_value;
  p->require_constant_elements = require_constant_elements;
  p->constructor_stack = constructor_stack;
  p->constructor_range_stack = constructor_range_stack;
  p->elements = constructor_elements;
  p->spelling = spelling;
  p->spelling_base = spelling_base;
  p->spelling_size = spelling_size;
  p->deferred = constructor_subconstants_deferred;
  p->top_level = constructor_top_level;
  p->next = initializer_stack;
  initializer_stack = p;

  constructor_decl = decl;
  constructor_asmspec = asmspec;
  constructor_subconstants_deferred = 0;
  constructor_designated = 0;
  constructor_top_level = top_level;

  if (decl != 0)
    {
      require_constant_value = ((decl)->common.static_flag);
      require_constant_elements
        = ((((decl)->common.static_flag) || (pedantic && !flag_isoc99))


           && (((enum tree_code) (((decl)->common.type))->common.code) == ARRAY_TYPE
               || ((enum tree_code) (((decl)->common.type))->common.code) == RECORD_TYPE
               || ((enum tree_code) (((decl)->common.type))->common.code) == UNION_TYPE
               || ((enum tree_code) (((decl)->common.type))->common.code) == QUAL_UNION_TYPE));
      locus = ((const char *) (((decl)->decl.name))->identifier.id.str);
    }
  else
    {
      require_constant_value = 0;
      require_constant_elements = 0;
      locus = "(anonymous)";
    }

  constructor_stack = 0;
  constructor_range_stack = 0;

  missing_braces_mentioned = 0;

  spelling_base = 0;
  spelling_size = 0;
  (spelling = spelling_base + (0));

  if (locus)
    push_string (locus);
}

void
finish_init ()
{
  struct initializer_stack *p = initializer_stack;




  if (constructor_subconstants_deferred)
    output_deferred_addressed_constants ();


  while (constructor_stack)
    {
      struct constructor_stack *q = constructor_stack;
      constructor_stack = q->next;
      free (q);
    }

  if (constructor_range_stack)
    fancy_abort ("c-typeck.c", 5151, __FUNCTION__);


  constructor_decl = p->decl;
  constructor_asmspec = p->asmspec;
  require_constant_value = p->require_constant_value;
  require_constant_elements = p->require_constant_elements;
  constructor_stack = p->constructor_stack;
  constructor_range_stack = p->constructor_range_stack;
  constructor_elements = p->elements;
  spelling = p->spelling;
  spelling_base = p->spelling_base;
  spelling_size = p->spelling_size;
  constructor_subconstants_deferred = p->deferred;
  constructor_top_level = p->top_level;
  initializer_stack = p->next;
  free (p);
}
# 5177 "c-typeck.c"
void
really_start_incremental_init (type)
     tree type;
{
  struct constructor_stack *p
    = (struct constructor_stack *) xmalloc (sizeof (struct constructor_stack));

  if (type == 0)
    type = ((constructor_decl)->common.type);

  p->type = constructor_type;
  p->fields = constructor_fields;
  p->index = constructor_index;
  p->max_index = constructor_max_index;
  p->unfilled_index = constructor_unfilled_index;
  p->unfilled_fields = constructor_unfilled_fields;
  p->bit_index = constructor_bit_index;
  p->elements = constructor_elements;
  p->constant = constructor_constant;
  p->simple = constructor_simple;
  p->erroneous = constructor_erroneous;
  p->pending_elts = constructor_pending_elts;
  p->depth = constructor_depth;
  p->replacement_value = 0;
  p->implicit = 0;
  p->range_stack = 0;
  p->outer = 0;
  p->incremental = constructor_incremental;
  p->designated = constructor_designated;
  p->next = 0;
  constructor_stack = p;

  constructor_constant = 1;
  constructor_simple = 1;
  constructor_depth = (spelling - spelling_base);
  constructor_elements = 0;
  constructor_pending_elts = 0;
  constructor_type = type;
  constructor_incremental = 1;
  constructor_designated = 0;
  designator_depth = 0;
  designator_errorneous = 0;

  if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
      || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
    {
      constructor_fields = ((constructor_type)->type.values);

      while (constructor_fields != 0 && ((((constructor_fields))->decl.lang_flag_4) == 1)
             && ((constructor_fields)->decl.name) == 0)
        constructor_fields = ((constructor_fields)->common.chain);

      constructor_unfilled_fields = constructor_fields;
      constructor_bit_index = global_trees[TI_BITSIZE_ZERO];
    }
  else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    {
      if (((constructor_type)->type.values))
        {
          constructor_max_index
            = ((((constructor_type)->type.values))->type.maxval);


          if (constructor_max_index == (tree) ((void *)0)
              && ((constructor_type)->type.size))
            constructor_max_index = build_int_2_wide ((unsigned long long) (-1), (long long) (-1));




          if (constructor_max_index
              && ((enum tree_code) (constructor_max_index)->common.code) != INTEGER_CST)
            constructor_max_index = build_int_2_wide ((unsigned long long) (-1), (long long) (-1));

          constructor_index
            = convert (sizetype_tab[(int) BITSIZETYPE],
                       ((((constructor_type)->type.values))->type.minval));
        }
      else
        constructor_index = global_trees[TI_BITSIZE_ZERO];

      constructor_unfilled_index = constructor_index;
    }
  else if (((enum tree_code) (constructor_type)->common.code) == VECTOR_TYPE)
    {

      constructor_max_index =
        build_int_2_wide ((unsigned long long) ((((mode_unit_size[(int) (((constructor_type)->type.mode))]) == 0) ? 0 : ((mode_size[(int) (((constructor_type)->type.mode))]) / (mode_unit_size[(int) (((constructor_type)->type.mode))]))) - 1), (long long) (0));
      constructor_index = convert (sizetype_tab[(int) BITSIZETYPE], global_trees[TI_INTEGER_ZERO]);
      constructor_unfilled_index = constructor_index;
    }
  else
    {

      constructor_fields = constructor_type;
      constructor_unfilled_fields = constructor_type;
    }
}






void
push_init_level (implicit)
     int implicit;
{
  struct constructor_stack *p;
  tree value = (tree) ((void *)0);



  while (constructor_stack->implicit)
    {
      if ((((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
           || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
          && constructor_fields == 0)
        process_init_element (pop_init_level (1));
      else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE
               && tree_int_cst_lt (constructor_max_index, constructor_index))
        process_init_element (pop_init_level (1));
      else
        break;
    }



  if (implicit)
    {
      if ((((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
           || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
          && constructor_fields)
        value = find_init_member (constructor_fields);
      else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
        value = find_init_member (constructor_index);
    }

  p = (struct constructor_stack *) xmalloc (sizeof (struct constructor_stack));
  p->type = constructor_type;
  p->fields = constructor_fields;
  p->index = constructor_index;
  p->max_index = constructor_max_index;
  p->unfilled_index = constructor_unfilled_index;
  p->unfilled_fields = constructor_unfilled_fields;
  p->bit_index = constructor_bit_index;
  p->elements = constructor_elements;
  p->constant = constructor_constant;
  p->simple = constructor_simple;
  p->erroneous = constructor_erroneous;
  p->pending_elts = constructor_pending_elts;
  p->depth = constructor_depth;
  p->replacement_value = 0;
  p->implicit = implicit;
  p->outer = 0;
  p->incremental = constructor_incremental;
  p->designated = constructor_designated;
  p->next = constructor_stack;
  p->range_stack = 0;
  constructor_stack = p;

  constructor_constant = 1;
  constructor_simple = 1;
  constructor_depth = (spelling - spelling_base);
  constructor_elements = 0;
  constructor_incremental = 1;
  constructor_designated = 0;
  constructor_pending_elts = 0;
  if (!implicit)
    {
      p->range_stack = constructor_range_stack;
      constructor_range_stack = 0;
      designator_depth = 0;
      designator_errorneous = 0;
    }



  if (constructor_type == 0)
    ;
  else if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
           || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
    {

      if (constructor_fields == 0)
        constructor_type = 0;
      else
        {
          constructor_type = ((constructor_fields)->common.type);
          push_member_name (constructor_fields);
          constructor_depth++;
        }
    }
  else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    {
      constructor_type = ((constructor_type)->common.type);
      push_array_bounds (tree_low_cst (constructor_index, 0));
      constructor_depth++;
    }

  if (constructor_type == 0)
    {
      error_init ("extra brace group at end of initializer");
      constructor_fields = 0;
      constructor_unfilled_fields = 0;
      return;
    }

  if (value && ((enum tree_code) (value)->common.code) == CONSTRUCTOR)
    {
      constructor_constant = ((value)->common.constant_flag);
      constructor_simple = ((value)->common.static_flag);
      constructor_elements = ((value)->exp.operands[1]);
      if (constructor_elements
          && (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
              || ((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE))
        set_nonincremental_init ();
    }

  if (implicit == 1 && warn_missing_braces && !missing_braces_mentioned)
    {
      missing_braces_mentioned = 1;
      warning_init ("missing braces around initializer");
    }

  if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
           || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
    {
      constructor_fields = ((constructor_type)->type.values);

      while (constructor_fields != 0 && ((((constructor_fields))->decl.lang_flag_4) == 1)
             && ((constructor_fields)->decl.name) == 0)
        constructor_fields = ((constructor_fields)->common.chain);

      constructor_unfilled_fields = constructor_fields;
      constructor_bit_index = global_trees[TI_BITSIZE_ZERO];
    }
  else if (((enum tree_code) (constructor_type)->common.code) == VECTOR_TYPE)
    {

      constructor_max_index =
        build_int_2_wide ((unsigned long long) ((((mode_unit_size[(int) (((constructor_type)->type.mode))]) == 0) ? 0 : ((mode_size[(int) (((constructor_type)->type.mode))]) / (mode_unit_size[(int) (((constructor_type)->type.mode))]))) - 1), (long long) (0));
      constructor_index = convert (sizetype_tab[(int) BITSIZETYPE], global_trees[TI_INTEGER_ZERO]);
      constructor_unfilled_index = constructor_index;
    }
  else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    {
      if (((constructor_type)->type.values))
        {
          constructor_max_index
            = ((((constructor_type)->type.values))->type.maxval);


          if (constructor_max_index == (tree) ((void *)0)
              && ((constructor_type)->type.size))
            constructor_max_index = build_int_2_wide ((unsigned long long) (-1), (long long) (-1));




          if (constructor_max_index
              && ((enum tree_code) (constructor_max_index)->common.code) != INTEGER_CST)
            constructor_max_index = build_int_2_wide ((unsigned long long) (-1), (long long) (-1));

          constructor_index
            = convert (sizetype_tab[(int) BITSIZETYPE],
                       ((((constructor_type)->type.values))->type.minval));
        }
      else
        constructor_index = global_trees[TI_BITSIZE_ZERO];

      constructor_unfilled_index = constructor_index;
      if (value && ((enum tree_code) (value)->common.code) == STRING_CST)
        {



          set_nonincremental_init_from_string (value);
        }
    }
  else
    {
      warning_init ("braces around scalar initializer");
      constructor_fields = constructor_type;
      constructor_unfilled_fields = constructor_type;
    }
}
# 5473 "c-typeck.c"
tree
pop_init_level (implicit)
     int implicit;
{
  struct constructor_stack *p;
  tree constructor = 0;

  if (implicit == 0)
    {


      while (constructor_stack->implicit)
        process_init_element (pop_init_level (1));

      if (constructor_range_stack)
        fancy_abort ("c-typeck.c", 5488, __FUNCTION__);
    }

  p = constructor_stack;



  if (constructor_type && constructor_fields
      && ((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE
      && ((constructor_type)->type.values)
      && ! ((((constructor_type)->type.values))->type.maxval))
    {


      if (integer_zerop (constructor_unfilled_index))
        constructor_type = (tree) ((void *)0);
      else if (! ((constructor_type)->type.size))
        {
          if (constructor_depth > 2)
            error_init ("initialization of flexible array member in a nested context");
          else if (pedantic)
            pedwarn_init ("initialization of a flexible array member");




          if (((constructor_fields)->common.chain) != (tree) ((void *)0))
            constructor_type = (tree) ((void *)0);
        }
      else


        fancy_abort ("c-typeck.c", 5520, __FUNCTION__);
    }


  if (extra_warnings
      && constructor_type
      && ((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
      && constructor_unfilled_fields)
    {

        while (constructor_unfilled_fields
               && (! ((constructor_unfilled_fields)->decl.size)
                   || integer_zerop (((constructor_unfilled_fields)->decl.size))))
          constructor_unfilled_fields = ((constructor_unfilled_fields)->common.chain);



        if (constructor_unfilled_fields && !constructor_designated)
          {
            push_member_name (constructor_unfilled_fields);
            warning_init ("missing initializer");
            (spelling = spelling_base + (constructor_depth));
          }
    }


  constructor_incremental = 1;
  output_pending_init_elements (1);


  if (p->replacement_value)


    constructor = p->replacement_value;
  else if (constructor_type == 0)
    ;
  else if (((enum tree_code) (constructor_type)->common.code) != RECORD_TYPE
           && ((enum tree_code) (constructor_type)->common.code) != UNION_TYPE
           && ((enum tree_code) (constructor_type)->common.code) != ARRAY_TYPE
           && ((enum tree_code) (constructor_type)->common.code) != VECTOR_TYPE)
    {


      if (constructor_elements == 0)
        {
          if (!constructor_erroneous)
            error_init ("empty scalar initializer");
          constructor = global_trees[TI_ERROR_MARK];
        }
      else if (((constructor_elements)->common.chain) != 0)
        {
          error_init ("extra elements in scalar initializer");
          constructor = ((constructor_elements)->list.value);
        }
      else
        constructor = ((constructor_elements)->list.value);
    }
  else
    {
      if (constructor_erroneous)
        constructor = global_trees[TI_ERROR_MARK];
      else
        {
          constructor = build (CONSTRUCTOR, constructor_type, (tree) ((void *)0),
                               nreverse (constructor_elements));
          if (constructor_constant)
            ((constructor)->common.constant_flag) = 1;
          if (constructor_constant && constructor_simple)
            ((constructor)->common.static_flag) = 1;
        }
    }

  constructor_type = p->type;
  constructor_fields = p->fields;
  constructor_index = p->index;
  constructor_max_index = p->max_index;
  constructor_unfilled_index = p->unfilled_index;
  constructor_unfilled_fields = p->unfilled_fields;
  constructor_bit_index = p->bit_index;
  constructor_elements = p->elements;
  constructor_constant = p->constant;
  constructor_simple = p->simple;
  constructor_erroneous = p->erroneous;
  constructor_incremental = p->incremental;
  constructor_designated = p->designated;
  constructor_pending_elts = p->pending_elts;
  constructor_depth = p->depth;
  if (!p->implicit)
    constructor_range_stack = p->range_stack;
  (spelling = spelling_base + (constructor_depth));

  constructor_stack = p->next;
  free (p);

  if (constructor == 0)
    {
      if (constructor_stack == 0)
        return global_trees[TI_ERROR_MARK];
      return (tree) ((void *)0);
    }
  return constructor;
}




static int
set_designator (array)
     int array;
{
  tree subtype;
  enum tree_code subcode;



  if (constructor_type == 0)
    return 1;


  if (designator_errorneous)
    return 1;

  if (!designator_depth)
    {
      if (constructor_range_stack)
        fancy_abort ("c-typeck.c", 5645, __FUNCTION__);



      while (constructor_stack->implicit)
        process_init_element (pop_init_level (1));
      constructor_designated = 1;
      return 0;
    }

  if (constructor_no_implicit)
    {
      error_init ("initialization designators may not nest");
      return 1;
    }

  if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
      || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
    {
      subtype = ((constructor_fields)->common.type);
      if (subtype != global_trees[TI_ERROR_MARK])
        subtype = ((subtype)->type.main_variant);
    }
  else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    {
      subtype = ((((constructor_type)->common.type))->type.main_variant);
    }
  else
    fancy_abort ("c-typeck.c", 5673, __FUNCTION__);

  subcode = ((enum tree_code) (subtype)->common.code);
  if (array && subcode != ARRAY_TYPE)
    {
      error_init ("array index in non-array initializer");
      return 1;
    }
  else if (!array && subcode != RECORD_TYPE && subcode != UNION_TYPE)
    {
      error_init ("field name not in record or union initializer");
      return 1;
    }

  constructor_designated = 1;
  push_init_level (2);
  return 0;
}





static void
push_range_stack (range_end)
     tree range_end;
{
  struct constructor_range_stack *p;

  p = (struct constructor_range_stack *)
      ggc_alloc (sizeof (struct constructor_range_stack));
  p->prev = constructor_range_stack;
  p->next = 0;
  p->fields = constructor_fields;
  p->range_start = constructor_index;
  p->index = constructor_index;
  p->stack = constructor_stack;
  p->range_end = range_end;
  if (constructor_range_stack)
    constructor_range_stack->next = p;
  constructor_range_stack = p;
}





void
set_init_index (first, last)
     tree first, last;
{
  if (set_designator (1))
    return;

  designator_errorneous = 1;

  while ((((enum tree_code) (first)->common.code) == NOP_EXPR
          || ((enum tree_code) (first)->common.code) == CONVERT_EXPR
          || ((enum tree_code) (first)->common.code) == NON_LVALUE_EXPR)
         && (((((first)->common.type))->type.mode)
             == ((((((first)->exp.operands[0]))->common.type))->type.mode)))
    first = ((first)->exp.operands[0]);

  if (last)
    while ((((enum tree_code) (last)->common.code) == NOP_EXPR
            || ((enum tree_code) (last)->common.code) == CONVERT_EXPR
            || ((enum tree_code) (last)->common.code) == NON_LVALUE_EXPR)
           && (((((last)->common.type))->type.mode)
               == ((((((last)->exp.operands[0]))->common.type))->type.mode)))
      last = ((last)->exp.operands[0]);

  if (((enum tree_code) (first)->common.code) != INTEGER_CST)
    error_init ("nonconstant array index in initializer");
  else if (last != 0 && ((enum tree_code) (last)->common.code) != INTEGER_CST)
    error_init ("nonconstant array index in initializer");
  else if (((enum tree_code) (constructor_type)->common.code) != ARRAY_TYPE)
    error_init ("array index in non-array initializer");
  else if (constructor_max_index
           && tree_int_cst_lt (constructor_max_index, first))
    error_init ("array index in initializer exceeds array bounds");
  else
    {
      constructor_index = convert (sizetype_tab[(int) BITSIZETYPE], first);

      if (last)
        {
          if (tree_int_cst_equal (first, last))
            last = 0;
          else if (tree_int_cst_lt (last, first))
            {
              error_init ("empty index range in initializer");
              last = 0;
            }
          else
            {
              last = convert (sizetype_tab[(int) BITSIZETYPE], last);
              if (constructor_max_index != 0
                  && tree_int_cst_lt (constructor_max_index, last))
                {
                  error_init ("array index range in initializer exceeds array bounds");
                  last = 0;
                }
            }
        }

      designator_depth++;
      designator_errorneous = 0;
      if (constructor_range_stack || last)
        push_range_stack (last);
    }
}



void
set_init_label (fieldname)
     tree fieldname;
{
  tree tail;

  if (set_designator (0))
    return;

  designator_errorneous = 1;

  if (((enum tree_code) (constructor_type)->common.code) != RECORD_TYPE
      && ((enum tree_code) (constructor_type)->common.code) != UNION_TYPE)
    {
      error_init ("field name not in record or union initializer");
      return;
    }

  for (tail = ((constructor_type)->type.values); tail;
       tail = ((tail)->common.chain))
    {
      if (((tail)->decl.name) == fieldname)
        break;
    }

  if (tail == 0)
    error ("unknown field `%s' specified in initializer",
           ((const char *) (fieldname)->identifier.id.str));
  else
    {
      constructor_fields = tail;
      designator_depth++;
      designator_errorneous = 0;
      if (constructor_range_stack)
        push_range_stack ((tree) ((void *)0));
    }
}





static void
add_pending_init (purpose, value)
     tree purpose, value;
{
  struct init_node *p, **q, *r;

  q = &constructor_pending_elts;
  p = 0;

  if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    {
      while (*q != 0)
        {
          p = *q;
          if (tree_int_cst_lt (purpose, p->purpose))
            q = &p->left;
          else if (tree_int_cst_lt (p->purpose, purpose))
            q = &p->right;
          else
            {
              if (((p->value)->common.side_effects_flag))
                warning_init ("initialized field with side-effects overwritten");
              p->value = value;
              return;
            }
        }
    }
  else
    {
      tree bitpos;

      bitpos = bit_position (purpose);
      while (*q != ((void *)0))
        {
          p = *q;
          if (tree_int_cst_lt (bitpos, bit_position (p->purpose)))
            q = &p->left;
          else if (p->purpose != purpose)
            q = &p->right;
          else
            {
              if (((p->value)->common.side_effects_flag))
                warning_init ("initialized field with side-effects overwritten");
              p->value = value;
              return;
            }
        }
    }

  r = (struct init_node *) ggc_alloc (sizeof (struct init_node));
  r->purpose = purpose;
  r->value = value;

  *q = r;
  r->parent = p;
  r->left = 0;
  r->right = 0;
  r->balance = 0;

  while (p)
    {
      struct init_node *s;

      if (r == p->left)
        {
          if (p->balance == 0)
            p->balance = -1;
          else if (p->balance < 0)
            {
              if (r->balance < 0)
                {

                  p->left = r->right;
                  if (p->left)
                    p->left->parent = p;
                  r->right = p;

                  p->balance = 0;
                  r->balance = 0;

                  s = p->parent;
                  p->parent = r;
                  r->parent = s;
                  if (s)
                    {
                      if (s->left == p)
                        s->left = r;
                      else
                        s->right = r;
                    }
                  else
                    constructor_pending_elts = r;
                }
              else
                {

                  struct init_node *t = r->right;

                  r->right = t->left;
                  if (r->right)
                    r->right->parent = r;
                  t->left = r;

                  p->left = t->right;
                  if (p->left)
                    p->left->parent = p;
                  t->right = p;

                  p->balance = t->balance < 0;
                  r->balance = -(t->balance > 0);
                  t->balance = 0;

                  s = p->parent;
                  p->parent = t;
                  r->parent = t;
                  t->parent = s;
                  if (s)
                    {
                      if (s->left == p)
                        s->left = t;
                      else
                        s->right = t;
                    }
                  else
                    constructor_pending_elts = t;
                }
              break;
            }
          else
            {

              p->balance = 0;
              break;
            }
        }
      else
        {
          if (p->balance == 0)

            p->balance++;
          else if (p->balance > 0)
            {
              if (r->balance > 0)
                {

                  p->right = r->left;
                  if (p->right)
                    p->right->parent = p;
                  r->left = p;

                  p->balance = 0;
                  r->balance = 0;

                  s = p->parent;
                  p->parent = r;
                  r->parent = s;
                  if (s)
                    {
                      if (s->left == p)
                        s->left = r;
                      else
                        s->right = r;
                    }
                  else
                    constructor_pending_elts = r;
                }
              else
                {

                  struct init_node *t = r->left;

                  r->left = t->right;
                  if (r->left)
                    r->left->parent = r;
                  t->right = r;

                  p->right = t->left;
                  if (p->right)
                    p->right->parent = p;
                  t->left = p;

                  r->balance = (t->balance < 0);
                  p->balance = -(t->balance > 0);
                  t->balance = 0;

                  s = p->parent;
                  p->parent = t;
                  r->parent = t;
                  t->parent = s;
                  if (s)
                    {
                      if (s->left == p)
                        s->left = t;
                      else
                        s->right = t;
                    }
                  else
                    constructor_pending_elts = t;
                }
              break;
            }
          else
            {

              p->balance = 0;
              break;
            }
        }

      r = p;
      p = p->parent;
    }
}



static void
set_nonincremental_init ()
{
  tree chain;

  if (((enum tree_code) (constructor_type)->common.code) != RECORD_TYPE
      && ((enum tree_code) (constructor_type)->common.code) != ARRAY_TYPE)
    return;

  for (chain = constructor_elements; chain; chain = ((chain)->common.chain))
    add_pending_init (((chain)->list.purpose), ((chain)->list.value));
  constructor_elements = 0;
  if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE)
    {
      constructor_unfilled_fields = ((constructor_type)->type.values);

      while (constructor_unfilled_fields != 0
             && ((((constructor_unfilled_fields))->decl.lang_flag_4) == 1)
             && ((constructor_unfilled_fields)->decl.name) == 0)
        constructor_unfilled_fields = ((constructor_unfilled_fields)->common.chain);

    }
  else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    {
      if (((constructor_type)->type.values))
        constructor_unfilled_index
            = convert (sizetype_tab[(int) BITSIZETYPE],
                       ((((constructor_type)->type.values))->type.minval));
      else
        constructor_unfilled_index = global_trees[TI_BITSIZE_ZERO];
    }
  constructor_incremental = 0;
}



static void
set_nonincremental_init_from_string (str)
     tree str;
{
  tree value, purpose, type;
  long long val[2];
  const char *p, *end;
  int byte, wchar_bytes, charwidth, bitpos;

  if (((enum tree_code) (constructor_type)->common.code) != ARRAY_TYPE)
    fancy_abort ("c-typeck.c", 6091, __FUNCTION__);

  if (((((((str)->common.type))->common.type))->type.precision)
      == ((integer_types[itk_char])->type.precision))
    wchar_bytes = 1;
  else if (((((((str)->common.type))->common.type))->type.precision)
           == ((c_global_trees[CTI_WCHAR_TYPE])->type.precision))
    wchar_bytes = ((c_global_trees[CTI_WCHAR_TYPE])->type.precision) / 8;
  else
    fancy_abort ("c-typeck.c", 6100, __FUNCTION__);

  charwidth = ((integer_types[itk_char])->type.precision);
  type = ((constructor_type)->common.type);
  p = ((str)->string.pointer);
  end = p + ((str)->string.length);

  for (purpose = global_trees[TI_BITSIZE_ZERO];
       p < end && !tree_int_cst_lt (constructor_max_index, purpose);
       purpose = size_binop (PLUS_EXPR, purpose, global_trees[TI_BITSIZE_ONE]))
    {
      if (wchar_bytes == 1)
        {
          val[1] = (unsigned char) *p++;
          val[0] = 0;
        }
      else
        {
          val[0] = 0;
          val[1] = 0;
          for (byte = 0; byte < wchar_bytes; byte++)
            {
              if (0)
                bitpos = (wchar_bytes - byte - 1) * charwidth;
              else
                bitpos = byte * charwidth;
              val[bitpos < (8 * 8)]
                |= ((unsigned long long) ((unsigned char) *p++))
                   << (bitpos % (8 * 8));
            }
        }

      if (!((type)->common.unsigned_flag))
        {
          bitpos = ((wchar_bytes - 1) * charwidth) + 8;
          if (bitpos < (8 * 8))
            {
              if (val[1] & (((long long) 1) << (bitpos - 1)))
                {
                  val[1] |= ((long long) -1) << bitpos;
                  val[0] = -1;
                }
            }
          else if (bitpos == (8 * 8))
            {
              if (val[1] < 0)
                val[0] = -1;
            }
          else if (val[0] & (((long long) 1)
                             << (bitpos - 1 - (8 * 8))))
            val[0] |= ((long long) -1)
                      << (bitpos - (8 * 8));
        }

      value = build_int_2_wide ((unsigned long long) (val[1]), (long long) (val[0]));
      ((value)->common.type) = type;
      add_pending_init (purpose, value);
    }

  constructor_incremental = 0;
}




static tree
find_init_member (field)
     tree field;
{
  struct init_node *p;

  if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    {
      if (constructor_incremental
          && tree_int_cst_lt (field, constructor_unfilled_index))
        set_nonincremental_init ();

      p = constructor_pending_elts;
      while (p)
        {
          if (tree_int_cst_lt (field, p->purpose))
            p = p->left;
          else if (tree_int_cst_lt (p->purpose, field))
            p = p->right;
          else
            return p->value;
        }
    }
  else if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE)
    {
      tree bitpos = bit_position (field);

      if (constructor_incremental
          && (!constructor_unfilled_fields
              || tree_int_cst_lt (bitpos,
                                  bit_position (constructor_unfilled_fields))))
        set_nonincremental_init ();

      p = constructor_pending_elts;
      while (p)
        {
          if (field == p->purpose)
            return p->value;
          else if (tree_int_cst_lt (bitpos, bit_position (p->purpose)))
            p = p->left;
          else
            p = p->right;
        }
    }
  else if (((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
    {
      if (constructor_elements
          && ((constructor_elements)->list.purpose) == field)
        return ((constructor_elements)->list.value);
    }
  return 0;
}
# 6228 "c-typeck.c"
static void
output_init_element (value, type, field, pending)
     tree value, type, field;
     int pending;
{
  if (((enum tree_code) (((value)->common.type))->common.code) == FUNCTION_TYPE
      || (((enum tree_code) (((value)->common.type))->common.code) == ARRAY_TYPE
          && !(((enum tree_code) (value)->common.code) == STRING_CST
               && ((enum tree_code) (type)->common.code) == ARRAY_TYPE
               && ((enum tree_code) (((type)->common.type))->common.code) == INTEGER_TYPE)
          && !comptypes (((((value)->common.type))->type.main_variant),
                         ((type)->type.main_variant))))
    value = default_conversion (value);

  if (((enum tree_code) (value)->common.code) == COMPOUND_LITERAL_EXPR
      && require_constant_value && !flag_isoc99 && pending)
    {



      tree decl = ((((((value))->exp.operands[0])))->exp.operands[0]);
      value = ((decl)->decl.initial);
    }

  if (value == global_trees[TI_ERROR_MARK])
    constructor_erroneous = 1;
  else if (!((value)->common.constant_flag))
    constructor_constant = 0;
  else if (initializer_constant_valid_p (value, ((value)->common.type)) == 0
           || ((((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
                || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
               && ((((field))->decl.lang_flag_4) == 1)
               && ((enum tree_code) (value)->common.code) != INTEGER_CST))
    constructor_simple = 0;

  if (require_constant_value && ! ((value)->common.constant_flag))
    {
      error_init ("initializer element is not constant");
      value = global_trees[TI_ERROR_MARK];
    }
  else if (require_constant_elements
           && initializer_constant_valid_p (value, ((value)->common.type)) == 0)
    pedwarn ("initializer element is not computable at load time");



  if (field
      && (((field)->common.type) == global_trees[TI_ERROR_MARK]
          || ((((((field)->common.type))->type.size) != (tree) ((void *)0))
              && integer_zerop (((((field)->common.type))->type.size))
              && (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE
                  || ((field)->common.chain)))))
    return;

  value = digest_init (type, value, require_constant_value,
                       require_constant_elements);
  if (value == global_trees[TI_ERROR_MARK])
    {
      constructor_erroneous = 1;
      return;
    }



  if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE
      && (!constructor_incremental
          || !tree_int_cst_equal (field, constructor_unfilled_index)))
    {
      if (constructor_incremental
          && tree_int_cst_lt (field, constructor_unfilled_index))
        set_nonincremental_init ();

      add_pending_init (field, value);
      return;
    }
  else if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
           && (!constructor_incremental
               || field != constructor_unfilled_fields))
    {



      if (constructor_incremental)
        {
          if (!constructor_unfilled_fields)
            set_nonincremental_init ();
          else
            {
              tree bitpos, unfillpos;

              bitpos = bit_position (field);
              unfillpos = bit_position (constructor_unfilled_fields);

              if (tree_int_cst_lt (bitpos, unfillpos))
                set_nonincremental_init ();
            }
        }

      add_pending_init (field, value);
      return;
    }
  else if (((enum tree_code) (constructor_type)->common.code) == UNION_TYPE
           && constructor_elements)
    {
      if (((((constructor_elements)->list.value))->common.side_effects_flag))
        warning_init ("initialized field with side-effects overwritten");


      constructor_elements = 0;
    }




  if (field && ((enum tree_code) (field)->common.code) == INTEGER_CST)
    field = copy_node (field);
  constructor_elements
    = tree_cons (field, value, constructor_elements);


  if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    constructor_unfilled_index
      = size_binop (PLUS_EXPR, constructor_unfilled_index,
                    global_trees[TI_BITSIZE_ONE]);
  else if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE)
    {
      constructor_unfilled_fields
        = ((constructor_unfilled_fields)->common.chain);


      while (constructor_unfilled_fields != 0
             && ((((constructor_unfilled_fields))->decl.lang_flag_4) == 1)
             && ((constructor_unfilled_fields)->decl.name) == 0)
        constructor_unfilled_fields =
          ((constructor_unfilled_fields)->common.chain);
    }
  else if (((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
    constructor_unfilled_fields = 0;


  if (pending)
    output_pending_init_elements (0);
}
# 6383 "c-typeck.c"
static void
output_pending_init_elements (all)
     int all;
{
  struct init_node *elt = constructor_pending_elts;
  tree next;

 retry:






  next = 0;
  while (elt)
    {
      if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
        {
          if (tree_int_cst_equal (elt->purpose,
                                  constructor_unfilled_index))
            output_init_element (elt->value,
                                 ((constructor_type)->common.type),
                                 constructor_unfilled_index, 0);
          else if (tree_int_cst_lt (constructor_unfilled_index,
                                    elt->purpose))
            {

              if (elt->left)
                elt = elt->left;
              else
                {


                  next = elt->purpose;
                  break;
                }
            }
          else
            {

              if (elt->right)
                elt = elt->right;
              else
                {


                  while (elt->parent && elt->parent->right == elt)
                    elt = elt->parent;
                  elt = elt->parent;
                  if (elt && tree_int_cst_lt (constructor_unfilled_index,
                                              elt->purpose))
                    {
                      next = elt->purpose;
                      break;
                    }
                }
            }
        }
      else if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
               || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
        {
          tree ctor_unfilled_bitpos, elt_bitpos;


          if (constructor_unfilled_fields == 0)
            break;

          ctor_unfilled_bitpos = bit_position (constructor_unfilled_fields);
          elt_bitpos = bit_position (elt->purpose);


          if (tree_int_cst_equal (elt_bitpos, ctor_unfilled_bitpos))
            {
              constructor_unfilled_fields = elt->purpose;
              output_init_element (elt->value, ((elt->purpose)->common.type),
                                   elt->purpose, 0);
            }
          else if (tree_int_cst_lt (ctor_unfilled_bitpos, elt_bitpos))
            {

              if (elt->left)
                elt = elt->left;
              else
                {


                  next = elt->purpose;
                  break;
                }
            }
          else
            {

              if (elt->right)
                elt = elt->right;
              else
                {


                  while (elt->parent && elt->parent->right == elt)
                    elt = elt->parent;
                  elt = elt->parent;
                  if (elt
                      && (tree_int_cst_lt (ctor_unfilled_bitpos,
                                           bit_position (elt->purpose))))
                    {
                      next = elt->purpose;
                      break;
                    }
                }
            }
        }
    }



  if (! (all && next != 0))
    return;



  if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
      || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
    constructor_unfilled_fields = next;
  else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
    constructor_unfilled_index = next;



  goto retry;
}
# 6524 "c-typeck.c"
void
process_init_element (value)
     tree value;
{
  tree orig_value = value;
  int string_flag = value != 0 && ((enum tree_code) (value)->common.code) == STRING_CST;

  designator_depth = 0;
  designator_errorneous = 0;



  if (string_flag
      && constructor_type
      && ((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE
      && ((enum tree_code) (((constructor_type)->common.type))->common.code) == INTEGER_TYPE
      && integer_zerop (constructor_unfilled_index))
    {
      if (constructor_stack->replacement_value)
        error_init ("excess elements in char array initializer");
      constructor_stack->replacement_value = value;
      return;
    }

  if (constructor_stack->replacement_value != 0)
    {
      error_init ("excess elements in struct initializer");
      return;
    }



  if (constructor_type == 0)
    return;



  while (constructor_stack->implicit)
    {
      if ((((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE
           || ((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
          && constructor_fields == 0)
        process_init_element (pop_init_level (1));
      else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE
               && (constructor_max_index == 0
                   || tree_int_cst_lt (constructor_max_index,
                                       constructor_index)))
        process_init_element (pop_init_level (1));
      else
        break;
    }


  if (constructor_range_stack)
    {


      if (((enum tree_code) (value)->common.code) != COMPOUND_LITERAL_EXPR
          || !require_constant_value
          || flag_isoc99)
        value = save_expr (value);
    }

  while (1)
    {
      if (((enum tree_code) (constructor_type)->common.code) == RECORD_TYPE)
        {
          tree fieldtype;
          enum tree_code fieldcode;

          if (constructor_fields == 0)
            {
              pedwarn_init ("excess elements in struct initializer");
              break;
            }

          fieldtype = ((constructor_fields)->common.type);
          if (fieldtype != global_trees[TI_ERROR_MARK])
            fieldtype = ((fieldtype)->type.main_variant);
          fieldcode = ((enum tree_code) (fieldtype)->common.code);


          if (fieldcode == ARRAY_TYPE
              && !require_constant_value
              && ((fieldtype)->type.size) == (tree) ((void *)0)
              && ((constructor_fields)->common.chain) == (tree) ((void *)0))
            {
              error_init ("non-static initialization of a flexible array member");
              break;
            }


          if (value != 0
              && fieldcode == ARRAY_TYPE
              && ((enum tree_code) (((fieldtype)->common.type))->common.code) == INTEGER_TYPE
              && string_flag)
            value = orig_value;


          else if (value != 0 && !constructor_no_implicit
                   && value != global_trees[TI_ERROR_MARK]
                   && ((((value)->common.type))->type.main_variant) != fieldtype
                   && (fieldcode == RECORD_TYPE || fieldcode == ARRAY_TYPE
                       || fieldcode == UNION_TYPE))
            {
              push_init_level (1);
              continue;
            }

          if (value)
            {
              push_member_name (constructor_fields);
              output_init_element (value, fieldtype, constructor_fields, 1);
              (spelling = spelling_base + (constructor_depth));
            }
          else


            {

              if (((constructor_fields)->decl.size))
                constructor_bit_index
                  = size_binop (PLUS_EXPR,
                                bit_position (constructor_fields),
                                ((constructor_fields)->decl.size));

              constructor_unfilled_fields = ((constructor_fields)->common.chain);

              while (constructor_unfilled_fields != 0
                     && ((((constructor_unfilled_fields))->decl.lang_flag_4) == 1)
                     && ((constructor_unfilled_fields)->decl.name) == 0)
                constructor_unfilled_fields =
                  ((constructor_unfilled_fields)->common.chain);
            }

          constructor_fields = ((constructor_fields)->common.chain);

          while (constructor_fields != 0
                 && ((((constructor_fields))->decl.lang_flag_4) == 1)
                 && ((constructor_fields)->decl.name) == 0)
            constructor_fields = ((constructor_fields)->common.chain);
        }
      else if (((enum tree_code) (constructor_type)->common.code) == UNION_TYPE)
        {
          tree fieldtype;
          enum tree_code fieldcode;

          if (constructor_fields == 0)
            {
              pedwarn_init ("excess elements in union initializer");
              break;
            }

          fieldtype = ((constructor_fields)->common.type);
          if (fieldtype != global_trees[TI_ERROR_MARK])
            fieldtype = ((fieldtype)->type.main_variant);
          fieldcode = ((enum tree_code) (fieldtype)->common.code);
# 6692 "c-typeck.c"
          if (warn_traditional && !in_system_header && !constructor_designated
              && !(value && (integer_zerop (value) || real_zerop (value))))
            warning ("traditional C rejects initialization of unions");


          if (value != 0
              && fieldcode == ARRAY_TYPE
              && ((enum tree_code) (((fieldtype)->common.type))->common.code) == INTEGER_TYPE
              && string_flag)
            value = orig_value;


          else if (value != 0 && !constructor_no_implicit
                   && value != global_trees[TI_ERROR_MARK]
                   && ((((value)->common.type))->type.main_variant) != fieldtype
                   && (fieldcode == RECORD_TYPE || fieldcode == ARRAY_TYPE
                       || fieldcode == UNION_TYPE))
            {
              push_init_level (1);
              continue;
            }

          if (value)
            {
              push_member_name (constructor_fields);
              output_init_element (value, fieldtype, constructor_fields, 1);
              (spelling = spelling_base + (constructor_depth));
            }
          else


            {
              constructor_bit_index = ((constructor_fields)->decl.size);
              constructor_unfilled_fields = ((constructor_fields)->common.chain);
            }

          constructor_fields = 0;
        }
      else if (((enum tree_code) (constructor_type)->common.code) == ARRAY_TYPE)
        {
          tree elttype = ((((constructor_type)->common.type))->type.main_variant);
          enum tree_code eltcode = ((enum tree_code) (elttype)->common.code);


          if (value != 0
              && eltcode == ARRAY_TYPE
              && ((enum tree_code) (((elttype)->common.type))->common.code) == INTEGER_TYPE
              && string_flag)
            value = orig_value;


          else if (value != 0 && !constructor_no_implicit
                   && value != global_trees[TI_ERROR_MARK]
                   && ((((value)->common.type))->type.main_variant) != elttype
                   && (eltcode == RECORD_TYPE || eltcode == ARRAY_TYPE
                       || eltcode == UNION_TYPE))
            {
              push_init_level (1);
              continue;
            }

          if (constructor_max_index != 0
              && (tree_int_cst_lt (constructor_max_index, constructor_index)
                  || integer_all_onesp (constructor_max_index)))
            {
              pedwarn_init ("excess elements in array initializer");
              break;
            }


          if (value)
            {
              push_array_bounds (tree_low_cst (constructor_index, 0));
              output_init_element (value, elttype, constructor_index, 1);
              (spelling = spelling_base + (constructor_depth));
            }

          constructor_index
            = size_binop (PLUS_EXPR, constructor_index, global_trees[TI_BITSIZE_ONE]);

          if (! value)



            constructor_unfilled_index = constructor_index;
        }
      else if (((enum tree_code) (constructor_type)->common.code) == VECTOR_TYPE)
        {
          tree elttype = ((((constructor_type)->common.type))->type.main_variant);



          if (tree_int_cst_lt (constructor_max_index, constructor_index))
            {
              pedwarn_init ("excess elements in vector initializer");
              break;
            }


          if (value)
            output_init_element (value, elttype, constructor_index, 1);

          constructor_index
            = size_binop (PLUS_EXPR, constructor_index, global_trees[TI_BITSIZE_ONE]);

          if (! value)



            constructor_unfilled_index = constructor_index;
        }



      else if (constructor_fields == 0)
        {
          pedwarn_init ("excess elements in scalar initializer");
          break;
        }
      else
        {
          if (value)
            output_init_element (value, constructor_type, (tree) ((void *)0), 1);
          constructor_fields = 0;
        }



      if (constructor_range_stack)
        {
          struct constructor_range_stack *p, *range_stack;
          int finish = 0;

          range_stack = constructor_range_stack;
          constructor_range_stack = 0;
          while (constructor_stack != range_stack->stack)
            {
              if (!constructor_stack->implicit)
                fancy_abort ("c-typeck.c", 6830, __FUNCTION__);
              process_init_element (pop_init_level (1));
            }
          for (p = range_stack;
               !p->range_end || tree_int_cst_equal (p->index, p->range_end);
               p = p->prev)
            {
              if (!constructor_stack->implicit)
                fancy_abort ("c-typeck.c", 6838, __FUNCTION__);
              process_init_element (pop_init_level (1));
            }

          p->index = size_binop (PLUS_EXPR, p->index, global_trees[TI_BITSIZE_ONE]);
          if (tree_int_cst_equal (p->index, p->range_end) && !p->prev)
            finish = 1;

          while (1)
            {
              constructor_index = p->index;
              constructor_fields = p->fields;
              if (finish && p->range_end && p->index == p->range_start)
                {
                  finish = 0;
                  p->prev = 0;
                }
              p = p->next;
              if (!p)
                break;
              push_init_level (2);
              p->stack = constructor_stack;
              if (p->range_end && tree_int_cst_equal (p->index, p->range_end))
                p->index = p->range_start;
            }

          if (!finish)
            constructor_range_stack = range_stack;
          continue;
        }

      break;
    }

  constructor_range_stack = 0;
}


tree
simple_asm_stmt (expr)
     tree expr;
{
  while ((((enum tree_code) (expr)->common.code) == NOP_EXPR || ((enum tree_code) (expr)->common.code) == CONVERT_EXPR || ((enum tree_code) (expr)->common.code) == NON_LVALUE_EXPR) && ((expr)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((expr)->common.type))->type.mode) == ((((((expr)->exp.operands[0]))->common.type))->type.mode))) (expr) = ((expr)->exp.operands[0]);

  if (((enum tree_code) (expr)->common.code) == ADDR_EXPR)
    expr = ((expr)->exp.operands[0]);

  if (((enum tree_code) (expr)->common.code) == STRING_CST)
    {
      tree stmt;

      if (((expr)->common.chain))
        expr = combine_strings (expr);
      stmt = add_stmt (build_stmt (ASM_STMT, (tree) ((void *)0), expr,
                                   (tree) ((void *)0), (tree) ((void *)0),
                                   (tree) ((void *)0)));
      (((stmt)->common.lang_flag_0)) = 1;
      return stmt;
    }

  error ("argument of `asm' is not a constant string");
  return (tree) ((void *)0);
}




tree
build_asm_stmt (cv_qualifier, string, outputs, inputs, clobbers)
     tree cv_qualifier;
     tree string;
     tree outputs;
     tree inputs;
     tree clobbers;
{
  tree tail;

  if (((string)->common.chain))
    string = combine_strings (string);
  if (((enum tree_code) (string)->common.code) != STRING_CST)
    {
      error ("asm template is not a string constant");
      return (tree) ((void *)0);
    }

  if (cv_qualifier != (tree) ((void *)0)
      && cv_qualifier != ridpointers[(int) RID_VOLATILE])
    {
      warning ("%s qualifier ignored on asm",
               ((const char *) (cv_qualifier)->identifier.id.str));
      cv_qualifier = (tree) ((void *)0);
    }



  for (tail = outputs; tail; tail = ((tail)->common.chain))
    {
      tree output = ((tail)->list.value);

      while ((((enum tree_code) (output)->common.code) == NOP_EXPR || ((enum tree_code) (output)->common.code) == CONVERT_EXPR || ((enum tree_code) (output)->common.code) == NON_LVALUE_EXPR) && ((output)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((output)->common.type))->type.mode) == ((((((output)->exp.operands[0]))->common.type))->type.mode))) (output) = ((output)->exp.operands[0]);
      ((tail)->list.value) = output;



      while (((enum tree_code) (output)->common.code) == NOP_EXPR
             || ((enum tree_code) (output)->common.code) == CONVERT_EXPR
             || ((enum tree_code) (output)->common.code) == FLOAT_EXPR
             || ((enum tree_code) (output)->common.code) == FIX_TRUNC_EXPR
             || ((enum tree_code) (output)->common.code) == FIX_FLOOR_EXPR
             || ((enum tree_code) (output)->common.code) == FIX_ROUND_EXPR
             || ((enum tree_code) (output)->common.code) == FIX_CEIL_EXPR)
        output = ((output)->exp.operands[0]);

      lvalue_or_else (((tail)->list.value), "invalid lvalue in asm statement");
    }


  for (tail = outputs; tail; tail = ((tail)->common.chain))
    {
      tree output = ((tail)->list.value);
      while ((((enum tree_code) (output)->common.code) == NOP_EXPR || ((enum tree_code) (output)->common.code) == CONVERT_EXPR || ((enum tree_code) (output)->common.code) == NON_LVALUE_EXPR) && ((output)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((output)->common.type))->type.mode) == ((((((output)->exp.operands[0]))->common.type))->type.mode))) (output) = ((output)->exp.operands[0]);
      ((tail)->list.value) = output;
    }




  for (tail = inputs; tail; tail = ((tail)->common.chain))
    ((tail)->list.value) = default_function_array_conversion (((tail)->list.value));

  return add_stmt (build_stmt (ASM_STMT, cv_qualifier, string,
                               outputs, inputs, clobbers));
}







void
c_expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line)
     tree string, outputs, inputs, clobbers;
     int vol;
     const char *filename;
     int line;
{
  int noutputs = list_length (outputs);
  int i;

  tree *o = (tree *) __builtin_alloca (noutputs * sizeof (tree));
  tree tail;


  for (i = 0, tail = outputs; tail; tail = ((tail)->common.chain), i++)
    o[i] = ((tail)->list.value);



  expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line);


  for (i = 0, tail = outputs; tail; tail = ((tail)->common.chain), i++)
    {
      if (o[i] != ((tail)->list.value))
        {
          expand_expr (build_modify_expr (o[i], NOP_EXPR, ((tail)->list.value)),
                       (rtx) 0, VOIDmode, EXPAND_NORMAL);
          free_temp_slots ();



          ((tail)->list.value) = o[i];
        }


      else
        {
          tree type = ((o[i])->common.type);
          if (((o[i])->common.readonly_flag)
              || ((type)->common.readonly_flag)
              || ((((enum tree_code) (type)->common.code) == RECORD_TYPE
                   || ((enum tree_code) (type)->common.code) == UNION_TYPE)
                  && ((type)->common.lang_flag_1)))
            readonly_warning (o[i], "modification by `asm'");
        }
    }


  emit_queue ();
}





tree
c_expand_return (retval)
     tree retval;
{
  tree valtype = ((((current_function_decl)->common.type))->common.type);

  if (((current_function_decl)->common.volatile_flag))
    warning ("function declared `noreturn' has a `return' statement");

  if (!retval)
    {
      current_function_returns_null = 1;
      if ((warn_return_type || flag_isoc99)
          && valtype != 0 && ((enum tree_code) (valtype)->common.code) != VOID_TYPE)
        pedwarn_c99 ("`return' with no value, in function returning non-void");
    }
  else if (valtype == 0 || ((enum tree_code) (valtype)->common.code) == VOID_TYPE)
    {
      current_function_returns_null = 1;
      if (pedantic || ((enum tree_code) (((retval)->common.type))->common.code) != VOID_TYPE)
        pedwarn ("`return' with a value, in function returning void");
    }
  else
    {
      tree t = convert_for_assignment (valtype, retval, ("return"),
                                       (tree) ((void *)0), (tree) ((void *)0), 0);
      tree res = ((current_function_decl)->decl.result);
      tree inner;

      current_function_returns_value = 1;
      if (t == global_trees[TI_ERROR_MARK])
        return (tree) ((void *)0);

      inner = t = convert (((res)->common.type), t);



      while (1)
        {
          switch (((enum tree_code) (inner)->common.code))
            {
            case NOP_EXPR: case NON_LVALUE_EXPR: case CONVERT_EXPR:
            case PLUS_EXPR:
              inner = ((inner)->exp.operands[0]);
              continue;

            case MINUS_EXPR:



              {
                tree op1 = ((inner)->exp.operands[1]);

                while (! (((enum tree_code) (((op1)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((op1)->common.type))->common.code) == REFERENCE_TYPE)
                       && (((enum tree_code) (op1)->common.code) == NOP_EXPR
                           || ((enum tree_code) (op1)->common.code) == NON_LVALUE_EXPR
                           || ((enum tree_code) (op1)->common.code) == CONVERT_EXPR))
                  op1 = ((op1)->exp.operands[0]);

                if ((((enum tree_code) (((op1)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((op1)->common.type))->common.code) == REFERENCE_TYPE))
                  break;

                inner = ((inner)->exp.operands[0]);
                continue;
              }

            case ADDR_EXPR:
              inner = ((inner)->exp.operands[0]);

              while (tree_code_type[(int) (((enum tree_code) (inner)->common.code))] == 'r')
                inner = ((inner)->exp.operands[0]);

              if (((enum tree_code) (inner)->common.code) == VAR_DECL
                  && ! ((inner)->decl.external_flag)
                  && ! ((inner)->common.static_flag)
                  && ((inner)->decl.context) == current_function_decl)
                warning ("function returns address of local variable");
              break;

            default:
              break;
            }

          break;
        }

      retval = build (MODIFY_EXPR, ((res)->common.type), res, t);
    }

 return add_stmt (build_return_stmt (retval));
}

struct c_switch {

  tree switch_stmt;





  splay_tree cases;

  struct c_switch *next;
};







static struct c_switch *switch_stack;




tree
c_start_case (exp)
     tree exp;
{
  enum tree_code code;
  tree type, orig_type = global_trees[TI_ERROR_MARK];
  struct c_switch *cs;

  if (exp != global_trees[TI_ERROR_MARK])
    {
      code = ((enum tree_code) (((exp)->common.type))->common.code);
      orig_type = ((exp)->common.type);

      if (! (((enum tree_code) (orig_type)->common.code) == INTEGER_TYPE || ((enum tree_code) (orig_type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (orig_type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (orig_type)->common.code) == CHAR_TYPE)
          && code != ERROR_MARK)
        {
          error ("switch quantity not an integer");
          exp = global_trees[TI_INTEGER_ZERO];
        }
      else
        {
          type = ((((exp)->common.type))->type.main_variant);

          if (warn_traditional && !in_system_header
              && (type == integer_types[itk_long]
                  || type == integer_types[itk_unsigned_long]))
            warning ("`long' switch expression not converted to `int' in ISO C");

          exp = default_conversion (exp);
          type = ((exp)->common.type);
        }
    }


  cs = (struct c_switch *) xmalloc (sizeof (*cs));
  cs->switch_stmt = build_stmt (SWITCH_STMT, exp, (tree) ((void *)0), orig_type);
  cs->cases = splay_tree_new (case_compare, ((void *)0), ((void *)0));
  cs->next = switch_stack;
  switch_stack = cs;

  return add_stmt (switch_stack->switch_stmt);
}



tree
do_case (low_value, high_value)
     tree low_value;
     tree high_value;
{
  tree label = (tree) ((void *)0);

  if (switch_stack)
    {
      label = c_add_case_label (switch_stack->cases,
                                (((switch_stack->switch_stmt))->exp.operands[0]),
                                low_value, high_value);
      if (label == global_trees[TI_ERROR_MARK])
        label = (tree) ((void *)0);
    }
  else if (low_value)
    error ("case label not within a switch statement");
  else
    error ("`default' label not within a switch statement");

  return label;
}



void
c_finish_case ()
{
  struct c_switch *cs = switch_stack;

  do { (((cs->switch_stmt))->exp.operands[1]) = ((cs->switch_stmt)->common.chain); ((cs->switch_stmt)->common.chain) = (tree) ((void *)0); (current_stmt_tree ()->x_last_stmt) = cs->switch_stmt; } while (0);


  switch_stack = switch_stack->next;
  splay_tree_delete (cs->cases);
  free (cs);
}
