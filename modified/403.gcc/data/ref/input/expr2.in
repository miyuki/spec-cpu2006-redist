# 1 "expr.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "expr.c"
# 22 "expr.c"
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
# 23 "expr.c" 2
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
        
# 24 "expr.c" 2
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
# 25 "expr.c" 2
# 1 "rtl.h" 1
# 25 "rtl.h"
struct function;

# 1 "machmode.h" 1
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
# 26 "expr.c" 2
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
# 27 "expr.c" 2

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
# 29 "expr.c" 2
# 1 "regs.h" 1
# 23 "regs.h"
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
# 24 "regs.h" 2
# 42 "regs.h"
extern int max_regno;


typedef struct reg_info_def
{
  int first_uid;
  int last_uid;
  int last_note_uid;


  int sets;


  int refs;
  int freq;
  int deaths;
  int live_length;
  int calls_crossed;
  int basic_block;
  char changes_mode;

} reg_info;

extern varray_type reg_n_info;
# 157 "regs.h"
extern short *reg_renumber;




extern char regs_ever_live[53];



extern const char * reg_names[53];






extern enum machine_mode reg_raw_mode[53];
# 196 "regs.h"
extern rtx regs_may_share;




extern int caller_save_needed;
# 230 "regs.h"
extern void allocate_reg_info (size_t, int, int);
# 30 "expr.c" 2
# 1 "hard-reg-set.h" 1
# 41 "hard-reg-set.h"
typedef unsigned long long HARD_REG_ELT_TYPE;
# 395 "hard-reg-set.h"
extern char fixed_regs[53];



extern HARD_REG_ELT_TYPE fixed_reg_set;






extern char call_used_regs[53];



extern HARD_REG_ELT_TYPE call_used_reg_set;


extern HARD_REG_ELT_TYPE losing_caller_save_reg_set;







extern char call_fixed_regs[53];



extern HARD_REG_ELT_TYPE call_fixed_reg_set;






extern char global_regs[53];
# 441 "hard-reg-set.h"
extern HARD_REG_ELT_TYPE regs_invalidated_by_call;




extern int reg_alloc_order[53];



extern int inv_reg_alloc_order[53];




extern HARD_REG_ELT_TYPE reg_class_contents[((int) LIM_REG_CLASSES)];



extern unsigned int reg_class_size[((int) LIM_REG_CLASSES)];



extern enum reg_class reg_class_superclasses[((int) LIM_REG_CLASSES)][((int) LIM_REG_CLASSES)];



extern enum reg_class reg_class_subclasses[((int) LIM_REG_CLASSES)][((int) LIM_REG_CLASSES)];




extern enum reg_class reg_class_subunion[((int) LIM_REG_CLASSES)][((int) LIM_REG_CLASSES)];




extern enum reg_class reg_class_superunion[((int) LIM_REG_CLASSES)][((int) LIM_REG_CLASSES)];



extern int n_non_fixed_regs;



extern const char * reg_names[53];
# 31 "expr.c" 2
# 1 "except.h" 1
# 29 "except.h"
struct function;

struct inline_remap;



struct eh_status;


struct eh_region;


extern int doing_eh (int);




extern void expand_eh_region_start (void);



extern void expand_eh_region_end_cleanup (tree);



extern void expand_start_all_catch (void);




extern void expand_start_catch (tree);


extern void expand_end_catch (void);


extern void expand_end_all_catch (void);




extern void expand_eh_region_end_allowed (tree, tree);



extern void expand_eh_region_end_must_not_throw (tree);




extern void expand_eh_region_end_throw (tree);




extern void expand_eh_region_end_fixup (tree);



extern void begin_protect_partials (void);




extern void add_partial_entry (tree);



extern void end_protect_partials (void);



extern void for_each_eh_label (void (*) (rtx));


extern _Bool can_throw_internal (rtx);
extern _Bool can_throw_external (rtx);


extern _Bool nothrow_function_p (void);



extern void finish_eh_generation (void);

extern void init_eh (void);
extern void init_eh_for_function (void);

extern rtx reachable_handlers (rtx);
extern void maybe_remove_eh_handler (rtx);

extern void convert_from_eh_region_ranges (void);
extern void convert_to_eh_region_ranges (void);
extern void find_exception_handler_labels (void);
extern _Bool current_function_has_exception_handlers (void);
extern void output_function_exception_table (void);

extern void expand_builtin_unwind_init (void);
extern rtx expand_builtin_eh_return_data_regno (tree);
extern rtx expand_builtin_extract_return_addr (tree);
extern void expand_builtin_init_dwarf_reg_sizes (tree);
extern rtx expand_builtin_frob_return_addr (tree);
extern rtx expand_builtin_dwarf_fp_regnum (void);
extern void expand_builtin_eh_return (tree, tree);
extern void expand_eh_return (void);
extern rtx get_exception_pointer (struct function *);
extern int duplicate_eh_regions (struct function *, struct inline_remap *);


extern void sjlj_emit_function_exit_after (rtx);
# 147 "except.h"
extern tree (*lang_protect_cleanup_actions) (void);


extern int (*lang_eh_type_covers) (tree a, tree b);


extern tree (*lang_eh_runtime_type) (tree);
# 32 "expr.c" 2
# 1 "function.h" 1
# 22 "function.h"
struct var_refs_queue
{
  rtx modified;
  enum machine_mode promoted_mode;
  int unsignedp;
  struct var_refs_queue *next;
};






struct sequence_stack
{

  rtx first, last;
  tree sequence_rtl_expr;
  struct sequence_stack *next;
};

extern struct sequence_stack *sequence_stack;



struct simple_obstack_stack
{
  struct obstack *obstack;
  struct simple_obstack_stack *next;
};

struct emit_status
{


  int x_reg_rtx_no;


  int x_first_label_num;






  rtx x_first_insn;
  rtx x_last_insn;




  tree sequence_rtl_expr;





  struct sequence_stack *sequence_stack;



  int x_cur_insn_uid;



  int x_last_linenum;
  const char *x_last_filename;





  int regno_pointer_align_length;




  unsigned char *regno_pointer_align;



  tree *regno_decl;



  rtx *x_regno_reg_rtx;
};
# 119 "function.h"
struct expr_status
{


  int x_pending_stack_adjust;
# 140 "function.h"
  int x_inhibit_defer_pop;





  int x_stack_pointer_delta;




  rtx x_saveregs_value;


  rtx x_apply_args_value;


  rtx x_forced_labels;


  rtx x_pending_chain;
};
# 174 "function.h"
struct function
{
  struct eh_status *eh;
  struct stmt_status *stmt;
  struct expr_status *expr;
  struct emit_status *emit;
  struct varasm_status *varasm;




  const char *name;


  tree decl;


  struct function *outer;




  int pops_args;




  int args_size;




  int pretend_args_size;



  int outgoing_args_size;



  rtx arg_offset_rtx;



  CUMULATIVE_ARGS args_info;





  rtx return_rtx;


  rtx internal_arg_pointer;



  const char *cannot_inline;



  struct initial_value_struct *hard_reg_initial_vals;


  int x_function_call_count;




  tree x_nonlocal_labels;





  rtx x_nonlocal_goto_handler_slots;



  rtx x_nonlocal_goto_handler_labels;




  rtx x_nonlocal_goto_stack_level;





  rtx x_cleanup_label;




  rtx x_return_label;



  rtx x_save_expr_regs;



  rtx x_stack_slot_list;


  tree x_rtl_expr_chain;



  rtx x_tail_recursion_label;


  rtx x_tail_recursion_reentry;





  rtx x_arg_pointer_save_area;




  rtx x_clobber_return_insn;




  long long x_frame_offset;




  tree x_context_display;
# 317 "function.h"
  tree x_trampoline_list;


  rtx x_parm_birth_insn;



  rtx x_last_parm_insn;



  unsigned int x_max_parm_reg;





  rtx *x_parm_reg_stack_loc;


  struct temp_slot *x_temp_slots;


  int x_temp_slot_level;


  int x_var_temp_slot_level;





  int x_target_temp_slot_level;



  struct var_refs_queue *fixup_var_refs_queue;


  int inlinable;
  int no_debugging_symbols;

  void *original_arg_vector;
  tree original_decl_initial;


  rtx inl_last_parm_insn;

  int inl_max_label_num;


  int profile_label_no;




  struct machine_function *machine;

  int stack_alignment_needed;

  int preferred_stack_boundary;


  struct language_function *language;





  rtx epilogue_delay_list;





  unsigned int returns_struct : 1;



  unsigned int returns_pcc_struct : 1;


  unsigned int returns_pointer : 1;


  unsigned int needs_context : 1;


  unsigned int calls_setjmp : 1;


  unsigned int calls_longjmp : 1;



  unsigned int calls_alloca : 1;


  unsigned int calls_eh_return : 1;



  unsigned int has_nonlocal_label : 1;



  unsigned int has_nonlocal_goto : 1;


  unsigned int contains_functions : 1;


  unsigned int has_computed_jump : 1;




  unsigned int is_thunk : 1;



  unsigned int instrument_entry_exit : 1;


  unsigned int profile : 1;



  unsigned int limit_stack : 1;



  unsigned int varargs : 1;



  unsigned int stdarg : 1;





  unsigned int x_whole_function_mode_p : 1;
# 468 "function.h"
  unsigned int x_dont_save_pending_sizes_p : 1;


  unsigned int uses_const_pool : 1;


  unsigned int uses_pic_offset_table : 1;


  unsigned int uses_eh_lsda : 1;


  unsigned int arg_pointer_save_area_init : 1;
};


extern struct function *cfun;


extern int virtuals_instantiated;
# 549 "function.h"
extern tree inline_function_decl;



struct function *find_function_data (tree);


extern void identify_blocks (void);



extern void reorder_blocks (void);


extern void number_blocks (tree);




extern long long get_frame_size (void);

extern long long get_func_frame_size (struct function *);



extern void (*init_machine_status) (struct function *);
extern void (*free_machine_status) (struct function *);



extern void (*mark_machine_status) (struct function *);


extern void (*init_lang_status) (struct function *);
extern void (*mark_lang_status) (struct function *);
extern void (*save_lang_status) (struct function *);
extern void (*restore_lang_status) (struct function *);
extern void (*free_lang_status) (struct function *);


extern void restore_emit_status (struct function *);
extern void free_after_parsing (struct function *);
extern void free_after_compilation (struct function *);

extern void init_varasm_status (struct function *);
extern void free_varasm_status (struct function *);
extern void free_emit_status (struct function *);
extern void free_stmt_status (struct function *);
extern void free_eh_status (struct function *);
extern void free_expr_status (struct function *);

extern rtx get_first_block_beg (void);


extern void diddle_return_value (void (*)(rtx, void*), void*);
extern void clobber_return_register (void);
extern void use_return_register (void);


extern rtx get_arg_pointer_save_area (struct function *);

extern void init_virtual_regs (struct emit_status *);


extern void init_function_once (void);
# 33 "expr.c" 2
# 1 "insn-config.h" 1
# 34 "expr.c" 2
# 1 "insn-attr.h" 1
# 10 "insn-attr.h"
enum attr_cpu {CPU_I386, CPU_I486, CPU_PENTIUM, CPU_PENTIUMPRO, CPU_K6, CPU_ATHLON, CPU_PENTIUM4};
extern enum attr_cpu get_attr_cpu (void);


enum attr_type {TYPE_OTHER, TYPE_MULTI, TYPE_ALU1, TYPE_NEGNOT, TYPE_ALU, TYPE_ICMP, TYPE_TEST, TYPE_IMOV, TYPE_IMOVX, TYPE_LEA, TYPE_INCDEC, TYPE_ISHIFT, TYPE_IMUL, TYPE_IDIV, TYPE_IBR, TYPE_SETCC, TYPE_PUSH, TYPE_POP, TYPE_CALL, TYPE_CALLV, TYPE_ICMOV, TYPE_FMOV, TYPE_FOP, TYPE_FOP1, TYPE_FSGN, TYPE_FMUL, TYPE_FDIV, TYPE_FPSPC, TYPE_FCMOV, TYPE_FCMP, TYPE_FXCH, TYPE_STR, TYPE_CLD, TYPE_SSE, TYPE_MMX, TYPE_FISTP};
extern enum attr_type get_attr_type (rtx);


enum attr_mode {MODE_UNKNOWN, MODE_NONE, MODE_QI, MODE_HI, MODE_SI, MODE_DI, MODE_UNKNOWNFP, MODE_SF, MODE_DF, MODE_XF, MODE_TI};
extern enum attr_mode get_attr_mode (rtx);


extern int get_attr_i387 (rtx);

extern int get_attr_length_immediate (rtx);

extern int get_attr_length_address (rtx);

extern int get_attr_prefix_data16 (rtx);

extern int get_attr_prefix_rep (rtx);

extern int get_attr_prefix_0f (rtx);

extern int get_attr_modrm (rtx);

extern int get_attr_length (rtx);
extern void shorten_branches (rtx);
extern int insn_default_length (rtx);
extern int insn_variable_length_p (rtx);
extern int insn_current_length (rtx);

# 1 "insn-addr.h" 1
# 26 "insn-addr.h"
extern varray_type insn_addresses_;
extern int insn_current_address;
# 43 "insn-attr.h" 2


enum attr_memory {MEMORY_NONE, MEMORY_LOAD, MEMORY_STORE, MEMORY_BOTH, MEMORY_UNKNOWN};
extern enum attr_memory get_attr_memory (rtx);


enum attr_imm_disp {IMM_DISP_FALSE, IMM_DISP_TRUE, IMM_DISP_UNKNOWN};
extern enum attr_imm_disp get_attr_imm_disp (rtx);


enum attr_fp_int_src {FP_INT_SRC_FALSE, FP_INT_SRC_TRUE};
extern enum attr_fp_int_src get_attr_fp_int_src (rtx);


enum attr_pent_prefix {PENT_PREFIX_FALSE, PENT_PREFIX_TRUE};
extern enum attr_pent_prefix get_attr_pent_prefix (rtx);


enum attr_pent_pair {PENT_PAIR_UV, PENT_PAIR_PU, PENT_PAIR_PV, PENT_PAIR_NP};
extern enum attr_pent_pair get_attr_pent_pair (rtx);


enum attr_ppro_uops {PPRO_UOPS_ONE, PPRO_UOPS_FEW, PPRO_UOPS_MANY};
extern enum attr_ppro_uops get_attr_ppro_uops (rtx);


enum attr_athlon_decode {ATHLON_DECODE_DIRECT, ATHLON_DECODE_VECTOR};
extern enum attr_athlon_decode get_attr_athlon_decode (rtx);


enum attr_athlon_fpunits {ATHLON_FPUNITS_NONE, ATHLON_FPUNITS_STORE, ATHLON_FPUNITS_MUL, ATHLON_FPUNITS_ADD, ATHLON_FPUNITS_MULADD, ATHLON_FPUNITS_ANY};
extern enum attr_athlon_fpunits get_attr_athlon_fpunits (rtx);



extern int result_ready_cost (rtx);
extern int function_units_used (rtx);

extern const struct function_unit_desc
{
  const char *const name;
  const int bitmask;
  const int multiplicity;
  const int simultaneity;
  const int default_cost;
  const int max_issue_delay;
  int (*const ready_cost_function) (rtx);
  int (*const conflict_cost_function) (rtx, rtx);
  const int max_blockage;
  unsigned int (*const blockage_range_function) (rtx);
  int (*const blockage_function) (rtx, rtx);
} function_units[];
# 35 "expr.c" 2

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



rtx emit_conditional_move (rtx, enum rtx_code, rtx, rtx, enum machine_mode, rtx, rtx, enum machine_mode, int);




int can_conditionally_move_p (enum machine_mode mode);
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
# 37 "expr.c" 2
# 1 "optabs.h" 1
# 24 "optabs.h"
# 1 "insn-codes.h" 1






enum insn_code {
  CODE_FOR_cmpdi_ccno_1_rex64 = 0,
  CODE_FOR_cmpdi_1_insn_rex64 = 2,
  CODE_FOR_cmpqi_ext_3_insn = 15,
  CODE_FOR_cmpqi_ext_3_insn_rex64 = 16,
  CODE_FOR_x86_fnstsw_1 = 30,
  CODE_FOR_x86_sahf_1 = 31,
  CODE_FOR_popsi1 = 42,
  CODE_FOR_movsi_insv_1 = 73,
  CODE_FOR_pushdi2_rex64 = 77,
  CODE_FOR_popdi1 = 80,
  CODE_FOR_swapxf = 105,
  CODE_FOR_swaptf = 106,
  CODE_FOR_zero_extendhisi2_and = 107,
  CODE_FOR_zero_extendsidi2_32 = 115,
  CODE_FOR_zero_extendsidi2_rex64 = 116,
  CODE_FOR_zero_extendhidi2 = 117,
  CODE_FOR_zero_extendqidi2 = 118,
  CODE_FOR_extendsidi2_rex64 = 120,
  CODE_FOR_extendhidi2 = 121,
  CODE_FOR_extendqidi2 = 122,
  CODE_FOR_extendhisi2 = 123,
  CODE_FOR_extendqihi2 = 125,
  CODE_FOR_extendqisi2 = 126,
  CODE_FOR_truncdfsf2_3 = 142,
  CODE_FOR_truncdfsf2_sse_only = 143,
  CODE_FOR_fix_truncdi_nomemory = 153,
  CODE_FOR_fix_truncdi_memory = 154,
  CODE_FOR_fix_truncsfdi_sse = 155,
  CODE_FOR_fix_truncdfdi_sse = 156,
  CODE_FOR_fix_truncsi_nomemory = 158,
  CODE_FOR_fix_truncsi_memory = 159,
  CODE_FOR_fix_truncsfsi_sse = 160,
  CODE_FOR_fix_truncdfsi_sse = 161,
  CODE_FOR_fix_trunchi_nomemory = 163,
  CODE_FOR_fix_trunchi_memory = 164,
  CODE_FOR_x86_fnstcw_1 = 165,
  CODE_FOR_x86_fldcw_1 = 166,
  CODE_FOR_floathisf2 = 167,
  CODE_FOR_floathidf2 = 173,
  CODE_FOR_floathixf2 = 179,
  CODE_FOR_floathitf2 = 180,
  CODE_FOR_floatsixf2 = 181,
  CODE_FOR_floatsitf2 = 182,
  CODE_FOR_floatdixf2 = 183,
  CODE_FOR_floatditf2 = 184,
  CODE_FOR_addqi3_cc = 191,
  CODE_FOR_addsi_1_zext = 208,
  CODE_FOR_addqi_ext_1 = 227,
  CODE_FOR_subdi3_carry_rex64 = 231,
  CODE_FOR_subsi3_carry = 235,
  CODE_FOR_subsi3_carry_zext = 236,
  CODE_FOR_divqi3 = 266,
  CODE_FOR_udivqi3 = 267,
  CODE_FOR_divmodhi4 = 274,
  CODE_FOR_udivmoddi4 = 275,
  CODE_FOR_udivmodsi4 = 277,
  CODE_FOR_testsi_1 = 281,
  CODE_FOR_andqi_ext_0 = 302,
  CODE_FOR_negsf2_memory = 355,
  CODE_FOR_negsf2_ifs = 356,
  CODE_FOR_negdf2_memory = 358,
  CODE_FOR_negdf2_ifs = 359,
  CODE_FOR_abssf2_memory = 374,
  CODE_FOR_abssf2_ifs = 375,
  CODE_FOR_absdf2_memory = 377,
  CODE_FOR_absdf2_ifs = 378,
  CODE_FOR_ashldi3_1 = 405,
  CODE_FOR_x86_shld_1 = 407,
  CODE_FOR_ashrdi3_63_rex64 = 418,
  CODE_FOR_ashrdi3_1 = 423,
  CODE_FOR_x86_shrd_1 = 425,
  CODE_FOR_ashrsi3_31 = 426,
  CODE_FOR_lshrdi3_1 = 448,
  CODE_FOR_setcc_2 = 487,
  CODE_FOR_jump = 502,
  CODE_FOR_doloop_end_internal = 507,
  CODE_FOR_blockage = 513,
  CODE_FOR_return_internal = 514,
  CODE_FOR_return_pop_internal = 515,
  CODE_FOR_return_indirect_internal = 516,
  CODE_FOR_nop = 517,
  CODE_FOR_prologue_set_got = 518,
  CODE_FOR_prologue_get_pc = 519,
  CODE_FOR_eh_return_si = 520,
  CODE_FOR_eh_return_di = 521,
  CODE_FOR_leave = 522,
  CODE_FOR_leave_rex64 = 523,
  CODE_FOR_ffssi_1 = 524,
  CODE_FOR_sqrtsf2_1 = 559,
  CODE_FOR_sqrtsf2_1_sse_only = 560,
  CODE_FOR_sqrtsf2_i387 = 561,
  CODE_FOR_sqrtdf2_1 = 562,
  CODE_FOR_sqrtdf2_1_sse_only = 563,
  CODE_FOR_sqrtdf2_i387 = 564,
  CODE_FOR_sqrtxf2 = 566,
  CODE_FOR_sqrttf2 = 567,
  CODE_FOR_sindf2 = 572,
  CODE_FOR_sinsf2 = 573,
  CODE_FOR_sinxf2 = 575,
  CODE_FOR_sintf2 = 576,
  CODE_FOR_cosdf2 = 577,
  CODE_FOR_cossf2 = 578,
  CODE_FOR_cosxf2 = 580,
  CODE_FOR_costf2 = 581,
  CODE_FOR_cld = 582,
  CODE_FOR_strmovdi_rex_1 = 583,
  CODE_FOR_strmovsi_1 = 584,
  CODE_FOR_strmovsi_rex_1 = 585,
  CODE_FOR_strmovhi_1 = 586,
  CODE_FOR_strmovhi_rex_1 = 587,
  CODE_FOR_strmovqi_1 = 588,
  CODE_FOR_strmovqi_rex_1 = 589,
  CODE_FOR_rep_movdi_rex64 = 590,
  CODE_FOR_rep_movsi = 591,
  CODE_FOR_rep_movsi_rex64 = 592,
  CODE_FOR_rep_movqi = 593,
  CODE_FOR_rep_movqi_rex64 = 594,
  CODE_FOR_strsetdi_rex_1 = 595,
  CODE_FOR_strsetsi_1 = 596,
  CODE_FOR_strsetsi_rex_1 = 597,
  CODE_FOR_strsethi_1 = 598,
  CODE_FOR_strsethi_rex_1 = 599,
  CODE_FOR_strsetqi_1 = 600,
  CODE_FOR_strsetqi_rex_1 = 601,
  CODE_FOR_rep_stosdi_rex64 = 602,
  CODE_FOR_rep_stossi = 603,
  CODE_FOR_rep_stossi_rex64 = 604,
  CODE_FOR_rep_stosqi = 605,
  CODE_FOR_rep_stosqi_rex64 = 606,
  CODE_FOR_cmpstrqi_nz_1 = 607,
  CODE_FOR_cmpstrqi_nz_rex_1 = 608,
  CODE_FOR_cmpstrqi_1 = 609,
  CODE_FOR_cmpstrqi_rex_1 = 610,
  CODE_FOR_strlenqi_1 = 611,
  CODE_FOR_strlenqi_rex_1 = 612,
  CODE_FOR_x86_movdicc_0_m1_rex64 = 613,
  CODE_FOR_x86_movsicc_0_m1 = 615,
  CODE_FOR_pro_epilogue_adjust_stack_rex64 = 636,
  CODE_FOR_sse_movsfcc = 637,
  CODE_FOR_sse_movsfcc_eq = 638,
  CODE_FOR_sse_movdfcc = 639,
  CODE_FOR_sse_movdfcc_eq = 640,
  CODE_FOR_allocate_stack_worker_1 = 649,
  CODE_FOR_allocate_stack_worker_rex64 = 650,
  CODE_FOR_trap = 657,
  CODE_FOR_movv4sf_internal = 659,
  CODE_FOR_movv4si_internal = 660,
  CODE_FOR_movv8qi_internal = 661,
  CODE_FOR_movv4hi_internal = 662,
  CODE_FOR_movv2si_internal = 663,
  CODE_FOR_movv2sf_internal = 664,
  CODE_FOR_movti_internal = 672,
  CODE_FOR_sse_movaps = 674,
  CODE_FOR_sse_movups = 675,
  CODE_FOR_sse_movmskps = 676,
  CODE_FOR_mmx_pmovmskb = 677,
  CODE_FOR_mmx_maskmovq = 678,
  CODE_FOR_mmx_maskmovq_rex = 679,
  CODE_FOR_sse_movntv4sf = 680,
  CODE_FOR_sse_movntdi = 681,
  CODE_FOR_sse_movhlps = 682,
  CODE_FOR_sse_movlhps = 683,
  CODE_FOR_sse_movhps = 684,
  CODE_FOR_sse_movlps = 685,
  CODE_FOR_sse_loadss = 686,
  CODE_FOR_sse_movss = 687,
  CODE_FOR_sse_storess = 688,
  CODE_FOR_sse_shufps = 689,
  CODE_FOR_addv4sf3 = 690,
  CODE_FOR_vmaddv4sf3 = 691,
  CODE_FOR_subv4sf3 = 692,
  CODE_FOR_vmsubv4sf3 = 693,
  CODE_FOR_mulv4sf3 = 694,
  CODE_FOR_vmmulv4sf3 = 695,
  CODE_FOR_divv4sf3 = 696,
  CODE_FOR_vmdivv4sf3 = 697,
  CODE_FOR_rcpv4sf2 = 698,
  CODE_FOR_vmrcpv4sf2 = 699,
  CODE_FOR_rsqrtv4sf2 = 700,
  CODE_FOR_vmrsqrtv4sf2 = 701,
  CODE_FOR_sqrtv4sf2 = 702,
  CODE_FOR_vmsqrtv4sf2 = 703,
  CODE_FOR_sse_andti3 = 708,
  CODE_FOR_sse_nandti3 = 712,
  CODE_FOR_sse_iorti3 = 718,
  CODE_FOR_sse_xorti3 = 724,
  CODE_FOR_sse_clrv4sf = 726,
  CODE_FOR_maskcmpv4sf3 = 727,
  CODE_FOR_maskncmpv4sf3 = 728,
  CODE_FOR_vmmaskcmpv4sf3 = 729,
  CODE_FOR_vmmaskncmpv4sf3 = 730,
  CODE_FOR_sse_comi = 731,
  CODE_FOR_sse_ucomi = 732,
  CODE_FOR_sse_unpckhps = 733,
  CODE_FOR_sse_unpcklps = 734,
  CODE_FOR_smaxv4sf3 = 735,
  CODE_FOR_vmsmaxv4sf3 = 736,
  CODE_FOR_sminv4sf3 = 737,
  CODE_FOR_vmsminv4sf3 = 738,
  CODE_FOR_cvtpi2ps = 739,
  CODE_FOR_cvtps2pi = 740,
  CODE_FOR_cvttps2pi = 741,
  CODE_FOR_cvtsi2ss = 742,
  CODE_FOR_cvtss2si = 743,
  CODE_FOR_cvttss2si = 744,
  CODE_FOR_addv8qi3 = 745,
  CODE_FOR_addv4hi3 = 746,
  CODE_FOR_addv2si3 = 747,
  CODE_FOR_ssaddv8qi3 = 748,
  CODE_FOR_ssaddv4hi3 = 749,
  CODE_FOR_usaddv8qi3 = 750,
  CODE_FOR_usaddv4hi3 = 751,
  CODE_FOR_subv8qi3 = 752,
  CODE_FOR_subv4hi3 = 753,
  CODE_FOR_subv2si3 = 754,
  CODE_FOR_sssubv8qi3 = 755,
  CODE_FOR_sssubv4hi3 = 756,
  CODE_FOR_ussubv8qi3 = 757,
  CODE_FOR_ussubv4hi3 = 758,
  CODE_FOR_mulv4hi3 = 759,
  CODE_FOR_smulv4hi3_highpart = 760,
  CODE_FOR_umulv4hi3_highpart = 761,
  CODE_FOR_mmx_pmaddwd = 762,
  CODE_FOR_mmx_iordi3 = 763,
  CODE_FOR_mmx_xordi3 = 764,
  CODE_FOR_mmx_clrdi = 765,
  CODE_FOR_mmx_anddi3 = 766,
  CODE_FOR_mmx_nanddi3 = 767,
  CODE_FOR_mmx_uavgv8qi3 = 768,
  CODE_FOR_mmx_uavgv4hi3 = 769,
  CODE_FOR_mmx_psadbw = 770,
  CODE_FOR_mmx_pinsrw = 771,
  CODE_FOR_mmx_pextrw = 772,
  CODE_FOR_mmx_pshufw = 773,
  CODE_FOR_eqv8qi3 = 774,
  CODE_FOR_eqv4hi3 = 775,
  CODE_FOR_eqv2si3 = 776,
  CODE_FOR_gtv8qi3 = 777,
  CODE_FOR_gtv4hi3 = 778,
  CODE_FOR_gtv2si3 = 779,
  CODE_FOR_umaxv8qi3 = 780,
  CODE_FOR_smaxv4hi3 = 781,
  CODE_FOR_uminv8qi3 = 782,
  CODE_FOR_sminv4hi3 = 783,
  CODE_FOR_ashrv4hi3 = 784,
  CODE_FOR_ashrv2si3 = 785,
  CODE_FOR_lshrv4hi3 = 786,
  CODE_FOR_lshrv2si3 = 787,
  CODE_FOR_mmx_lshrdi3 = 788,
  CODE_FOR_ashlv4hi3 = 789,
  CODE_FOR_ashlv2si3 = 790,
  CODE_FOR_mmx_ashldi3 = 791,
  CODE_FOR_mmx_packsswb = 792,
  CODE_FOR_mmx_packssdw = 793,
  CODE_FOR_mmx_packuswb = 794,
  CODE_FOR_mmx_punpckhbw = 795,
  CODE_FOR_mmx_punpckhwd = 796,
  CODE_FOR_mmx_punpckhdq = 797,
  CODE_FOR_mmx_punpcklbw = 798,
  CODE_FOR_mmx_punpcklwd = 799,
  CODE_FOR_mmx_punpckldq = 800,
  CODE_FOR_emms = 801,
  CODE_FOR_ldmxcsr = 802,
  CODE_FOR_stmxcsr = 803,
  CODE_FOR_addv2sf3 = 806,
  CODE_FOR_subv2sf3 = 807,
  CODE_FOR_subrv2sf3 = 808,
  CODE_FOR_gtv2sf3 = 809,
  CODE_FOR_gev2sf3 = 810,
  CODE_FOR_eqv2sf3 = 811,
  CODE_FOR_pfmaxv2sf3 = 812,
  CODE_FOR_pfminv2sf3 = 813,
  CODE_FOR_mulv2sf3 = 814,
  CODE_FOR_femms = 815,
  CODE_FOR_pf2id = 816,
  CODE_FOR_pf2iw = 817,
  CODE_FOR_pfacc = 818,
  CODE_FOR_pfnacc = 819,
  CODE_FOR_pfpnacc = 820,
  CODE_FOR_pi2fw = 821,
  CODE_FOR_floatv2si2 = 822,
  CODE_FOR_pavgusb = 823,
  CODE_FOR_pfrcpv2sf2 = 824,
  CODE_FOR_pfrcpit1v2sf3 = 825,
  CODE_FOR_pfrcpit2v2sf3 = 826,
  CODE_FOR_pfrsqrtv2sf2 = 827,
  CODE_FOR_pfrsqit1v2sf3 = 828,
  CODE_FOR_pmulhrwv4hi3 = 829,
  CODE_FOR_pswapdv2si2 = 830,
  CODE_FOR_pswapdv2sf2 = 831,
  CODE_FOR_cmpdi = 834,
  CODE_FOR_cmpsi = 835,
  CODE_FOR_cmphi = 836,
  CODE_FOR_cmpqi = 837,
  CODE_FOR_cmpdi_1_rex64 = 838,
  CODE_FOR_cmpsi_1 = 839,
  CODE_FOR_cmpqi_ext_3 = 840,
  CODE_FOR_cmpxf = 841,
  CODE_FOR_cmptf = 842,
  CODE_FOR_cmpdf = 843,
  CODE_FOR_cmpsf = 844,
  CODE_FOR_movsi = 846,
  CODE_FOR_movhi = 847,
  CODE_FOR_movstricthi = 848,
  CODE_FOR_movqi = 849,
  CODE_FOR_reload_outqi = 850,
  CODE_FOR_movstrictqi = 851,
  CODE_FOR_movdi = 852,
  CODE_FOR_movsf = 861,
  CODE_FOR_movdf = 865,
  CODE_FOR_movxf = 870,
  CODE_FOR_movtf = 871,
  CODE_FOR_zero_extendhisi2 = 878,
  CODE_FOR_zero_extendqihi2 = 880,
  CODE_FOR_zero_extendqisi2 = 884,
  CODE_FOR_zero_extendsidi2 = 888,
  CODE_FOR_extendsidi2 = 892,
  CODE_FOR_extendsfdf2 = 904,
  CODE_FOR_extendsfxf2 = 905,
  CODE_FOR_extendsftf2 = 906,
  CODE_FOR_extenddfxf2 = 907,
  CODE_FOR_extenddftf2 = 908,
  CODE_FOR_truncdfsf2 = 909,
  CODE_FOR_truncxfsf2 = 913,
  CODE_FOR_trunctfsf2 = 916,
  CODE_FOR_truncxfdf2 = 919,
  CODE_FOR_trunctfdf2 = 922,
  CODE_FOR_fix_truncxfdi2 = 925,
  CODE_FOR_fix_trunctfdi2 = 926,
  CODE_FOR_fix_truncdfdi2 = 927,
  CODE_FOR_fix_truncsfdi2 = 928,
  CODE_FOR_fix_truncxfsi2 = 932,
  CODE_FOR_fix_trunctfsi2 = 933,
  CODE_FOR_fix_truncdfsi2 = 934,
  CODE_FOR_fix_truncsfsi2 = 935,
  CODE_FOR_fix_truncxfhi2 = 939,
  CODE_FOR_fix_trunctfhi2 = 940,
  CODE_FOR_fix_truncdfhi2 = 941,
  CODE_FOR_fix_truncsfhi2 = 942,
  CODE_FOR_floatsisf2 = 946,
  CODE_FOR_floatdisf2 = 947,
  CODE_FOR_floatsidf2 = 948,
  CODE_FOR_floatdidf2 = 949,
  CODE_FOR_adddi3 = 951,
  CODE_FOR_addsi3 = 953,
  CODE_FOR_addhi3 = 963,
  CODE_FOR_addqi3 = 964,
  CODE_FOR_addxf3 = 965,
  CODE_FOR_addtf3 = 966,
  CODE_FOR_adddf3 = 967,
  CODE_FOR_addsf3 = 968,
  CODE_FOR_subdi3 = 969,
  CODE_FOR_subsi3 = 971,
  CODE_FOR_subhi3 = 972,
  CODE_FOR_subqi3 = 973,
  CODE_FOR_subxf3 = 974,
  CODE_FOR_subtf3 = 975,
  CODE_FOR_subdf3 = 976,
  CODE_FOR_subsf3 = 977,
  CODE_FOR_muldi3 = 978,
  CODE_FOR_mulsi3 = 979,
  CODE_FOR_mulhi3 = 980,
  CODE_FOR_mulqi3 = 981,
  CODE_FOR_umulqihi3 = 982,
  CODE_FOR_mulqihi3 = 983,
  CODE_FOR_umulditi3 = 984,
  CODE_FOR_umulsidi3 = 985,
  CODE_FOR_mulditi3 = 986,
  CODE_FOR_mulsidi3 = 987,
  CODE_FOR_umuldi3_highpart = 988,
  CODE_FOR_umulsi3_highpart = 989,
  CODE_FOR_smuldi3_highpart = 990,
  CODE_FOR_smulsi3_highpart = 991,
  CODE_FOR_mulxf3 = 992,
  CODE_FOR_multf3 = 993,
  CODE_FOR_muldf3 = 994,
  CODE_FOR_mulsf3 = 995,
  CODE_FOR_divxf3 = 996,
  CODE_FOR_divtf3 = 997,
  CODE_FOR_divdf3 = 998,
  CODE_FOR_divsf3 = 999,
  CODE_FOR_divmoddi4 = 1000,
  CODE_FOR_divmodsi4 = 1002,
  CODE_FOR_udivmodhi4 = 1006,
  CODE_FOR_testsi_ccno_1 = 1007,
  CODE_FOR_testqi_ccz_1 = 1008,
  CODE_FOR_testqi_ext_ccno_0 = 1009,
  CODE_FOR_anddi3 = 1011,
  CODE_FOR_andsi3 = 1012,
  CODE_FOR_andhi3 = 1016,
  CODE_FOR_andqi3 = 1017,
  CODE_FOR_iordi3 = 1018,
  CODE_FOR_iorsi3 = 1019,
  CODE_FOR_iorhi3 = 1020,
  CODE_FOR_iorqi3 = 1021,
  CODE_FOR_xordi3 = 1022,
  CODE_FOR_xorsi3 = 1023,
  CODE_FOR_xorhi3 = 1024,
  CODE_FOR_xorqi3 = 1025,
  CODE_FOR_xorqi_cc_ext_1 = 1026,
  CODE_FOR_negdi2 = 1027,
  CODE_FOR_negsi2 = 1029,
  CODE_FOR_neghi2 = 1030,
  CODE_FOR_negqi2 = 1031,
  CODE_FOR_negsf2 = 1032,
  CODE_FOR_negdf2 = 1039,
  CODE_FOR_negxf2 = 1046,
  CODE_FOR_negtf2 = 1047,
  CODE_FOR_abssf2 = 1052,
  CODE_FOR_absdf2 = 1059,
  CODE_FOR_absxf2 = 1065,
  CODE_FOR_abstf2 = 1066,
  CODE_FOR_one_cmpldi2 = 1071,
  CODE_FOR_one_cmplsi2 = 1073,
  CODE_FOR_one_cmplhi2 = 1076,
  CODE_FOR_one_cmplqi2 = 1078,
  CODE_FOR_ashldi3 = 1080,
  CODE_FOR_x86_shift_adj_1 = 1084,
  CODE_FOR_x86_shift_adj_2 = 1085,
  CODE_FOR_ashlsi3 = 1086,
  CODE_FOR_ashlhi3 = 1089,
  CODE_FOR_ashlqi3 = 1090,
  CODE_FOR_ashrdi3 = 1091,
  CODE_FOR_x86_shift_adj_3 = 1094,
  CODE_FOR_ashrsi3 = 1095,
  CODE_FOR_ashrhi3 = 1096,
  CODE_FOR_ashrqi3 = 1097,
  CODE_FOR_lshrdi3 = 1098,
  CODE_FOR_lshrsi3 = 1101,
  CODE_FOR_lshrhi3 = 1102,
  CODE_FOR_lshrqi3 = 1103,
  CODE_FOR_rotldi3 = 1104,
  CODE_FOR_rotlsi3 = 1105,
  CODE_FOR_rotlhi3 = 1106,
  CODE_FOR_rotlqi3 = 1107,
  CODE_FOR_rotrdi3 = 1108,
  CODE_FOR_rotrsi3 = 1109,
  CODE_FOR_rotrhi3 = 1110,
  CODE_FOR_rotrqi3 = 1111,
  CODE_FOR_extv = 1112,
  CODE_FOR_extzv = 1113,
  CODE_FOR_insv = 1114,
  CODE_FOR_seq = 1115,
  CODE_FOR_sne = 1116,
  CODE_FOR_sgt = 1117,
  CODE_FOR_sgtu = 1118,
  CODE_FOR_slt = 1119,
  CODE_FOR_sltu = 1120,
  CODE_FOR_sge = 1121,
  CODE_FOR_sgeu = 1122,
  CODE_FOR_sle = 1123,
  CODE_FOR_sleu = 1124,
  CODE_FOR_sunordered = 1125,
  CODE_FOR_sordered = 1126,
  CODE_FOR_suneq = 1127,
  CODE_FOR_sunge = 1128,
  CODE_FOR_sungt = 1129,
  CODE_FOR_sunle = 1130,
  CODE_FOR_sunlt = 1131,
  CODE_FOR_sltgt = 1132,
  CODE_FOR_beq = 1137,
  CODE_FOR_bne = 1138,
  CODE_FOR_bgt = 1139,
  CODE_FOR_bgtu = 1140,
  CODE_FOR_blt = 1141,
  CODE_FOR_bltu = 1142,
  CODE_FOR_bge = 1143,
  CODE_FOR_bgeu = 1144,
  CODE_FOR_ble = 1145,
  CODE_FOR_bleu = 1146,
  CODE_FOR_bunordered = 1147,
  CODE_FOR_bordered = 1148,
  CODE_FOR_buneq = 1149,
  CODE_FOR_bunge = 1150,
  CODE_FOR_bungt = 1151,
  CODE_FOR_bunle = 1152,
  CODE_FOR_bunlt = 1153,
  CODE_FOR_bltgt = 1154,
  CODE_FOR_indirect_jump = 1159,
  CODE_FOR_tablejump = 1160,
  CODE_FOR_doloop_end = 1161,
  CODE_FOR_call_pop = 1166,
  CODE_FOR_call = 1167,
  CODE_FOR_call_exp = 1168,
  CODE_FOR_call_value_pop = 1169,
  CODE_FOR_call_value = 1170,
  CODE_FOR_call_value_exp = 1171,
  CODE_FOR_untyped_call = 1172,
  CODE_FOR_return = 1173,
  CODE_FOR_prologue = 1174,
  CODE_FOR_epilogue = 1175,
  CODE_FOR_sibcall_epilogue = 1176,
  CODE_FOR_eh_return = 1177,
  CODE_FOR_ffssi2 = 1180,
  CODE_FOR_sqrtsf2 = 1183,
  CODE_FOR_sqrtdf2 = 1184,
  CODE_FOR_movstrsi = 1185,
  CODE_FOR_movstrdi = 1186,
  CODE_FOR_strmovdi_rex64 = 1187,
  CODE_FOR_strmovsi = 1188,
  CODE_FOR_strmovsi_rex64 = 1189,
  CODE_FOR_strmovhi = 1190,
  CODE_FOR_strmovhi_rex64 = 1191,
  CODE_FOR_strmovqi = 1192,
  CODE_FOR_strmovqi_rex64 = 1193,
  CODE_FOR_clrstrsi = 1194,
  CODE_FOR_clrstrdi = 1195,
  CODE_FOR_strsetdi_rex64 = 1196,
  CODE_FOR_strsetsi = 1197,
  CODE_FOR_strsetsi_rex64 = 1198,
  CODE_FOR_strsethi = 1199,
  CODE_FOR_strsethi_rex64 = 1200,
  CODE_FOR_strsetqi = 1201,
  CODE_FOR_strsetqi_rex64 = 1202,
  CODE_FOR_cmpstrsi = 1203,
  CODE_FOR_cmpintqi = 1204,
  CODE_FOR_strlensi = 1205,
  CODE_FOR_strlendi = 1206,
  CODE_FOR_movdicc = 1209,
  CODE_FOR_movsicc = 1210,
  CODE_FOR_movhicc = 1211,
  CODE_FOR_movsfcc = 1212,
  CODE_FOR_movdfcc = 1213,
  CODE_FOR_movxfcc = 1215,
  CODE_FOR_movtfcc = 1216,
  CODE_FOR_minsf3 = 1217,
  CODE_FOR_mindf3 = 1220,
  CODE_FOR_maxsf3 = 1223,
  CODE_FOR_maxdf3 = 1226,
  CODE_FOR_pro_epilogue_adjust_stack = 1229,
  CODE_FOR_allocate_stack_worker = 1233,
  CODE_FOR_allocate_stack = 1234,
  CODE_FOR_builtin_setjmp_receiver = 1235,
  CODE_FOR_conditional_trap = 1298,
  CODE_FOR_movti = 1299,
  CODE_FOR_movv4sf = 1300,
  CODE_FOR_movv4si = 1301,
  CODE_FOR_movv2si = 1302,
  CODE_FOR_movv4hi = 1303,
  CODE_FOR_movv8qi = 1304,
  CODE_FOR_movv2sf = 1305,
  CODE_FOR_sfence = 1314,
  CODE_FOR_sse_prologue_save = 1315,
  CODE_FOR_prefetch = 1316,
CODE_FOR_nothing
};
# 25 "optabs.h" 2
# 41 "optabs.h"
typedef struct optab
{
  enum rtx_code code;
  struct {
    enum insn_code insn_code;
    rtx libfunc;
  } handlers [(int) MAX_MACHINE_MODE];
} * optab;






enum optab_index
{
  OTI_add,
  OTI_addv,
  OTI_sub,
  OTI_subv,


  OTI_smul,
  OTI_smulv,

  OTI_smul_highpart,
  OTI_umul_highpart,

  OTI_smul_widen,
  OTI_umul_widen,


  OTI_sdiv,
  OTI_sdivv,

  OTI_sdivmod,
  OTI_udiv,
  OTI_udivmod,

  OTI_smod,
  OTI_umod,

  OTI_ftrunc,


  OTI_and,

  OTI_ior,

  OTI_xor,


  OTI_ashl,

  OTI_lshr,

  OTI_ashr,

  OTI_rotl,

  OTI_rotr,

  OTI_smin,

  OTI_smax,

  OTI_umin,

  OTI_umax,


  OTI_mov,

  OTI_movstrict,



  OTI_neg,
  OTI_negv,

  OTI_abs,
  OTI_absv,

  OTI_one_cmpl,

  OTI_ffs,

  OTI_sqrt,

  OTI_sin,

  OTI_cos,


  OTI_cmp,

  OTI_ucmp,

  OTI_tst,


  OTI_strlen,


  OTI_cbranch,
  OTI_cmov,
  OTI_cstore,


  OTI_push,

  OTI_MAX
};

extern optab optab_table[OTI_MAX];
# 213 "optabs.h"
extern enum insn_code extendtab[MAX_MACHINE_MODE][MAX_MACHINE_MODE][2];


extern enum insn_code fixtab[(int) MAX_MACHINE_MODE][(int) MAX_MACHINE_MODE][2];
extern enum insn_code fixtrunctab[(int) MAX_MACHINE_MODE][(int) MAX_MACHINE_MODE][2];
extern enum insn_code floattab[(int) MAX_MACHINE_MODE][(int) MAX_MACHINE_MODE][2];




extern enum insn_code reload_in_optab[(int) MAX_MACHINE_MODE];
extern enum insn_code reload_out_optab[(int) MAX_MACHINE_MODE];


extern optab code_to_optab[((int) LAST_AND_UNUSED_RTX_CODE) + 1];


typedef rtx (*rtxfun) (rtx);




extern rtxfun bcc_gen_fctn[((int) LAST_AND_UNUSED_RTX_CODE)];





extern enum insn_code setcc_gen_code[((int) LAST_AND_UNUSED_RTX_CODE)];





extern enum insn_code movcc_gen_code[(int) MAX_MACHINE_MODE];



extern enum insn_code movstr_optab[(int) MAX_MACHINE_MODE];


extern enum insn_code clrstr_optab[(int) MAX_MACHINE_MODE];




extern rtx expand_binop (enum machine_mode, optab, rtx, rtx, rtx, int, enum optab_methods);



extern rtx sign_expand_binop (enum machine_mode, optab, optab, rtx, rtx, rtx, int, enum optab_methods);



extern int expand_twoval_binop (optab, rtx, rtx, rtx, rtx, int);


extern rtx expand_unop (enum machine_mode, optab, rtx, rtx, int);


extern rtx expand_abs (enum machine_mode, rtx, rtx, int, int);


extern rtx expand_complex_abs (enum machine_mode, rtx, rtx, int);



extern void emit_unop_insn (int, rtx, rtx, enum rtx_code);



extern rtx emit_no_conflict_block (rtx, rtx, rtx, rtx, rtx);


extern void emit_clr_insn (rtx);


extern void emit_0_to_1_insn (rtx);


extern void emit_cmp_insn (rtx, rtx, enum rtx_code, rtx, enum machine_mode, int);




enum can_compare_purpose
{
  ccp_jump,
  ccp_cmov,
  ccp_store_flag
};



extern int can_compare_p (enum rtx_code, enum machine_mode, enum can_compare_purpose);


extern rtx prepare_operand (int, rtx, int, enum machine_mode, enum machine_mode, int);



extern enum insn_code can_extend_p (enum machine_mode, enum machine_mode, int);




extern rtx gen_extend_insn (rtx, rtx, enum machine_mode, enum machine_mode, int);




extern void init_fixtab (void);
extern void init_floattab (void);


extern void expand_float (rtx, rtx, int);


extern void expand_fix (rtx, rtx, int);
# 38 "expr.c" 2
# 1 "libfuncs.h" 1
# 25 "libfuncs.h"
enum libfunc_index
{
  LTI_extendsfdf2,
  LTI_extendsfxf2,
  LTI_extendsftf2,
  LTI_extenddfxf2,
  LTI_extenddftf2,

  LTI_truncdfsf2,
  LTI_truncxfsf2,
  LTI_trunctfsf2,
  LTI_truncxfdf2,
  LTI_trunctfdf2,

  LTI_abort,
  LTI_memcpy,
  LTI_memmove,
  LTI_bcopy,
  LTI_memcmp,
  LTI_bcmp,
  LTI_memset,
  LTI_bzero,

  LTI_unwind_resume,
  LTI_eh_personality,
  LTI_setjmp,
  LTI_longjmp,
  LTI_unwind_sjlj_register,
  LTI_unwind_sjlj_unregister,

  LTI_eqhf2,
  LTI_nehf2,
  LTI_gthf2,
  LTI_gehf2,
  LTI_lthf2,
  LTI_lehf2,
  LTI_unordhf2,

  LTI_eqsf2,
  LTI_nesf2,
  LTI_gtsf2,
  LTI_gesf2,
  LTI_ltsf2,
  LTI_lesf2,
  LTI_unordsf2,

  LTI_eqdf2,
  LTI_nedf2,
  LTI_gtdf2,
  LTI_gedf2,
  LTI_ltdf2,
  LTI_ledf2,
  LTI_unorddf2,

  LTI_eqxf2,
  LTI_nexf2,
  LTI_gtxf2,
  LTI_gexf2,
  LTI_ltxf2,
  LTI_lexf2,
  LTI_unordxf2,

  LTI_eqtf2,
  LTI_netf2,
  LTI_gttf2,
  LTI_getf2,
  LTI_lttf2,
  LTI_letf2,
  LTI_unordtf2,

  LTI_floatsisf,
  LTI_floatdisf,
  LTI_floattisf,

  LTI_floatsidf,
  LTI_floatdidf,
  LTI_floattidf,

  LTI_floatsixf,
  LTI_floatdixf,
  LTI_floattixf,

  LTI_floatsitf,
  LTI_floatditf,
  LTI_floattitf,

  LTI_fixsfsi,
  LTI_fixsfdi,
  LTI_fixsfti,

  LTI_fixdfsi,
  LTI_fixdfdi,
  LTI_fixdfti,

  LTI_fixxfsi,
  LTI_fixxfdi,
  LTI_fixxfti,

  LTI_fixtfsi,
  LTI_fixtfdi,
  LTI_fixtfti,

  LTI_fixunssfsi,
  LTI_fixunssfdi,
  LTI_fixunssfti,

  LTI_fixunsdfsi,
  LTI_fixunsdfdi,
  LTI_fixunsdfti,

  LTI_fixunsxfsi,
  LTI_fixunsxfdi,
  LTI_fixunsxfti,

  LTI_fixunstfsi,
  LTI_fixunstfdi,
  LTI_fixunstfti,

  LTI_profile_function_entry,
  LTI_profile_function_exit,

  LTI_MAX
};



extern rtx libfunc_table[LTI_MAX];
# 39 "expr.c" 2
# 1 "recog.h" 1
# 28 "recog.h"
enum op_type {
  OP_IN,
  OP_OUT,
  OP_INOUT
};

struct operand_alternative
{


  const char *constraint;


  enum reg_class class;



  unsigned int reject;


  int matches;



  int matched;


  unsigned int earlyclobber:1;

  unsigned int memory_ok:1;

  unsigned int offmem_ok:1;

  unsigned int nonoffmem_ok:1;

  unsigned int decmem_ok:1;

  unsigned int incmem_ok:1;

  unsigned int is_address:1;


  unsigned int anything_ok:1;
};


extern void init_recog (void);
extern void init_recog_no_volatile (void);
extern int recog_memoized_1 (rtx);
extern int check_asm_operands (rtx);
extern int asm_operand_ok (rtx, const char *);
extern int validate_change (rtx, rtx *, rtx, int);
extern int insn_invalid_p (rtx);
extern int apply_change_group (void);
extern int num_validated_changes (void);
extern void cancel_changes (int);
extern int constrain_operands (int);
extern int constrain_operands_cached (int);
extern int memory_address_p (enum machine_mode, rtx);
extern int strict_memory_address_p (enum machine_mode, rtx);
extern int validate_replace_rtx_subexp (rtx, rtx, rtx, rtx *);
extern int validate_replace_rtx (rtx, rtx, rtx);
extern void validate_replace_rtx_group (rtx, rtx, rtx);
extern int validate_replace_src (rtx, rtx, rtx);



extern int reg_fits_class_p (rtx, enum reg_class, int, enum machine_mode);

extern rtx *find_single_use (rtx, rtx, rtx *);

extern int general_operand (rtx, enum machine_mode);
extern int address_operand (rtx, enum machine_mode);
extern int register_operand (rtx, enum machine_mode);
extern int pmode_register_operand (rtx, enum machine_mode);
extern int scratch_operand (rtx, enum machine_mode);
extern int immediate_operand (rtx, enum machine_mode);
extern int const_int_operand (rtx, enum machine_mode);
extern int const_double_operand (rtx, enum machine_mode);
extern int nonimmediate_operand (rtx, enum machine_mode);
extern int nonmemory_operand (rtx, enum machine_mode);
extern int push_operand (rtx, enum machine_mode);
extern int pop_operand (rtx, enum machine_mode);
extern int memory_operand (rtx, enum machine_mode);
extern int indirect_operand (rtx, enum machine_mode);
extern int mode_independent_operand (rtx, enum machine_mode);
extern int comparison_operator (rtx, enum machine_mode);

extern int offsettable_memref_p (rtx);
extern int offsettable_nonstrict_memref_p (rtx);
extern int offsettable_address_p (int, enum machine_mode, rtx);
extern int mode_dependent_address_p (rtx);

extern int recog (rtx, rtx, int *);
extern void add_clobbers (rtx, int);
extern int added_clobbers_hard_reg_p (int);
extern void insn_extract (rtx);
extern void extract_insn (rtx);
extern void extract_constrain_insn_cached (rtx);
extern void extract_insn_cached (rtx);
extern void preprocess_constraints (void);
extern rtx peep2_next_insn (int);
extern int peep2_regno_dead_p (int, int);
extern int peep2_reg_dead_p (int, rtx);

extern rtx peep2_find_free_register (int, int, const char *, enum machine_mode, HARD_REG_ELT_TYPE *);



extern void peephole2_optimize (FILE *);
extern rtx peephole2_insns (rtx, rtx, int *);


extern int volatile_ok;



extern int which_alternative;



struct recog_data
{







  rtx operand[30];


  rtx *operand_loc[30];


  const char *constraints[30];


  enum machine_mode operand_mode[30];


  enum op_type operand_type[30];



  rtx *dup_loc[4];



  char dup_num[4];
# 191 "recog.h"
  char n_operands;


  char n_dups;


  char n_alternatives;


  rtx insn;
};

extern struct recog_data recog_data;



extern struct operand_alternative recog_op_alt[30][30];




typedef int (*insn_operand_predicate_fn) (rtx, enum machine_mode);
typedef const char * (*insn_output_fn) (rtx *, rtx);
typedef rtx (*insn_gen_fn) (rtx, ...);

struct insn_operand_data
{
  const insn_operand_predicate_fn predicate;

  const char *const constraint;

  const enum machine_mode mode : 16;

  const char strict_low;

  const char eliminable;
};
# 236 "recog.h"
struct insn_data
{
  const char *const name;
  const void * output;
  const insn_gen_fn genfun;
  const struct insn_operand_data *const operand;

  const char n_operands;
  const char n_dups;
  const char n_alternatives;
  const char output_format;
};

extern const struct insn_data insn_data[];
# 40 "expr.c" 2
# 1 "reload.h" 1
# 47 "reload.h"
extern int memory_move_secondary_cost (enum machine_mode, enum reg_class, int);
# 76 "reload.h"
enum reload_type
{
  RELOAD_FOR_INPUT, RELOAD_FOR_OUTPUT, RELOAD_FOR_INSN,
  RELOAD_FOR_INPUT_ADDRESS, RELOAD_FOR_INPADDR_ADDRESS,
  RELOAD_FOR_OUTPUT_ADDRESS, RELOAD_FOR_OUTADDR_ADDRESS,
  RELOAD_FOR_OPERAND_ADDRESS, RELOAD_FOR_OPADDR_ADDR,
  RELOAD_OTHER, RELOAD_FOR_OTHER_ADDRESS
};



struct reload
{

  rtx in;


  rtx out;


  enum reg_class class;


  enum machine_mode inmode;

  enum machine_mode outmode;


  enum machine_mode mode;


  unsigned int nregs;




  int inc;




  rtx in_reg;
  rtx out_reg;


  int regno;




  rtx reg_rtx;




  int opnum;



  int secondary_in_reload;


  int secondary_out_reload;



  enum insn_code secondary_in_icode;

  enum insn_code secondary_out_icode;




  enum reload_type when_needed;



  unsigned int optional:1;

  unsigned int nocombine:1;

  unsigned int secondary_p:1;


  unsigned int nongroup:1;
};

extern struct reload rld[(2 * 30 * (2 + 1))];
extern int n_reloads;


extern rtx *reg_equiv_constant;
extern rtx *reg_equiv_memory_loc;
extern rtx *reg_equiv_address;
extern rtx *reg_equiv_mem;



extern int n_earlyclobbers;
extern rtx reload_earlyclobbers[30];


extern int reload_n_operands;



extern int reload_first_uid;





extern char indirect_symref_ok;


extern char double_reg_address_ok;

extern int num_not_at_initial_offset;

struct needs
{

  short regs[2][((int) LIM_REG_CLASSES)];
  short groups[((int) LIM_REG_CLASSES)];
};
# 260 "reload.h"
extern rtx get_secondary_mem (rtx, enum machine_mode, int, enum reload_type);



extern void clear_secondary_mem (void);



extern void transfer_replacements (int, int);





extern int remove_address_replacements (rtx in_rtx);




extern int operands_match_p (rtx, rtx);


extern int safe_from_earlyclobber (rtx, rtx);




extern int find_reloads (rtx, int, int, int, short *);





extern rtx form_sum (rtx, rtx);



extern void subst_reloads (rtx);




extern void copy_replacements (rtx, rtx);


extern void move_replacements (rtx *x, rtx *y);



extern rtx find_replacement (rtx *);




extern int refers_to_regno_for_reload_p (unsigned int, unsigned int, rtx, rtx *);



extern int reg_overlap_mentioned_for_reload_p (rtx, rtx);



extern int refers_to_mem_for_reload_p (rtx);



extern rtx find_equiv_reg (rtx, rtx, enum reg_class, int, short *, int, enum machine_mode);



extern int regno_clobbered_p (unsigned int, rtx, enum machine_mode, int);



extern int earlyclobber_operand_p (rtx);


extern int push_reload (rtx, rtx, rtx *, rtx *, enum reg_class, enum machine_mode, enum machine_mode, int, int, int, enum reload_type);





extern void reload_cse_regs (rtx);
extern int reloads_conflict (int, int);


extern void init_reload (void);


extern int reload (rtx, int);



extern void mark_home_live (int);



extern rtx eliminate_regs (rtx, enum machine_mode, rtx);




extern rtx gen_reload (rtx, rtx, int, enum reload_type);


extern void deallocate_reload_reg (int r);




extern void init_caller_save (void);


extern void init_save_areas (void);


extern void setup_save_areas (void);


extern void save_call_clobbered_regs (void);


extern void cleanup_subreg_operands (rtx);


extern void debug_reload_to_stream (FILE *);
extern void debug_reload (void);
# 41 "expr.c" 2
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
# 42 "expr.c" 2
# 1 "typeclass.h" 1


enum type_class
{
  no_type_class = -1,
  void_type_class, integer_type_class, char_type_class,
  enumeral_type_class, boolean_type_class,
  pointer_type_class, reference_type_class, offset_type_class,
  real_type_class, complex_type_class,
  function_type_class, method_type_class,
  record_type_class, union_type_class,
  array_type_class, string_type_class, set_type_class, file_type_class,
  lang_type_class
};
# 43 "expr.c" 2
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
# 44 "expr.c" 2
# 1 "ggc.h" 1
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
# 45 "expr.c" 2
# 1 "langhooks.h" 1
# 25 "langhooks.h"
typedef void (*lang_print_tree_hook) (FILE *, tree, int indent);




struct lang_hooks_for_tree_inlining
{
  union tree_node *(*walk_subtrees) (union tree_node **, int *, union tree_node *(*) (union tree_node **, int *, void *), void *, void *);




  int (*cannot_inline_tree_fn) (union tree_node **);
  int (*disregard_inline_limits) (union tree_node *);
  union tree_node *(*add_pending_fn_decls) (void *, union tree_node *);

  int (*tree_chain_matters_p) (union tree_node *);
  int (*auto_var_in_fn_p) (union tree_node *, union tree_node *);
  union tree_node *(*copy_res_decl_for_inlining) (union tree_node *, union tree_node *, union tree_node *, void *, int *, void *);




  int (*anon_aggr_type_p) (union tree_node *);
  int (*start_inlining) (union tree_node *);
  void (*end_inlining) (union tree_node *);
  union tree_node *(*convert_parm_for_inlining) (union tree_node *, union tree_node *, union tree_node *);


};



struct lang_hooks_for_tree_dump
{


  int (*dump_tree) (void *, tree);


  int (*type_quals) (tree);
};



struct lang_hooks
{

  const char *name;



  size_t identifier_size;



  void (*init_options) (void);
# 91 "langhooks.h"
  int (*decode_option) (int, char **);
# 100 "langhooks.h"
  void (*post_options) (void);







  const char * (*init) (const char *);


  void (*finish) (void);


  void (*clear_binding_stack) (void);



  long long (*get_alias_set) (tree);




  tree (*expand_constant) (tree);
# 132 "langhooks.h"
  int (*safe_from_p) (rtx, tree);


  int (*staticp) (tree);


  _Bool honor_readonly;



  void (*print_statistics) (void);



  lang_print_tree_hook print_xnode;



  lang_print_tree_hook print_decl;
  lang_print_tree_hook print_type;
  lang_print_tree_hook print_identifier;




  void (*set_yydebug) (int);

  struct lang_hooks_for_tree_inlining tree_inlining;

  struct lang_hooks_for_tree_dump tree_dump;



};


extern const struct lang_hooks lang_hooks;
# 46 "expr.c" 2
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
# 47 "expr.c" 2
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
# 48 "expr.c" 2
# 82 "expr.c"
int cse_not_expected;


static tree placeholder_list = 0;



struct move_by_pieces
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  rtx from;
  rtx from_addr;
  int autinc_from;
  int explicit_inc_from;
  unsigned long long len;
  long long offset;
  int reverse;
};




struct store_by_pieces
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  unsigned long long len;
  long long offset;
  rtx (*constfun) (void *, long long, enum machine_mode);
  void * constfundata;
  int reverse;
};

extern struct obstack permanent_obstack;

static rtx enqueue_insn (rtx, rtx);
static unsigned long long move_by_pieces_ninsns
                                (unsigned long long, unsigned int);

static void move_by_pieces_1 (rtx (*) (rtx, ...), enum machine_mode, struct move_by_pieces *);

static rtx clear_by_pieces_1 (void *, long long, enum machine_mode);

static void clear_by_pieces (rtx, unsigned long long, unsigned int);

static void store_by_pieces_1 (struct store_by_pieces *, unsigned int);

static void store_by_pieces_2 (rtx (*) (rtx, ...), enum machine_mode, struct store_by_pieces *);


static rtx get_subtarget (rtx);
static int is_zeros_p (tree);
static int mostly_zeros_p (tree);
static void store_constructor_field (rtx, unsigned long long, long long, enum machine_mode, tree, tree, int, int);


static void store_constructor (tree, rtx, int, long long);
static rtx store_field (rtx, long long, long long, enum machine_mode, tree, enum machine_mode, int, tree, int);



static rtx var_rtx (tree);
static long long highest_pow2_factor (tree);
static long long highest_pow2_factor_for_type (tree, tree);
static int is_aligning_offset (tree, tree);
static rtx expand_increment (tree, int, int);
static void do_jump_by_parts_greater (tree, int, rtx, rtx);
static void do_jump_by_parts_equality (tree, rtx, rtx);
static void do_compare_and_jump (tree, enum rtx_code, enum rtx_code, rtx, rtx);

static rtx do_store_flag (tree, rtx, enum machine_mode, int);

static void emit_single_push_insn (enum machine_mode, rtx, tree);

static void do_tablejump (rtx, enum machine_mode, rtx, rtx, rtx);





static char direct_load[(int) MAX_MACHINE_MODE];
static char direct_store[(int) MAX_MACHINE_MODE];
# 190 "expr.c"
enum insn_code movstr_optab[(int) MAX_MACHINE_MODE];


enum insn_code clrstr_optab[(int) MAX_MACHINE_MODE];
# 204 "expr.c"
void
init_expr_once ()
{
  rtx insn, pat;
  enum machine_mode mode;
  int num_clobbers;
  rtx mem, mem1;

  start_sequence ();




  mem = gen_rtx_MEM (VOIDmode, (global_rtl[GR_STACK_POINTER]));
  mem1 = gen_rtx_MEM (VOIDmode, (global_rtl[GR_FRAME_POINTER]));

  insn = emit_insn (gen_rtx_fmt_ee (SET, (0), ((rtx) 0), ((rtx) 0)));
  pat = (((insn)->fld[3]).rtx);

  for (mode = VOIDmode; (int) mode < (int) MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      int regno;
      rtx reg;

      direct_load[(int) mode] = direct_store[(int) mode] = 0;
      ((mem)->mode = (enum machine_mode) (mode));
      ((mem1)->mode = (enum machine_mode) (mode));




      if (mode != VOIDmode && mode != BLKmode)
        for (regno = 0; regno < 53
             && (direct_load[(int) mode] == 0 || direct_store[(int) mode] == 0);
             regno++)
          {
            if (! ix86_hard_regno_mode_ok ((regno), (mode)))
              continue;

            reg = gen_rtx_REG (mode, regno);

            (((pat)->fld[1]).rtx) = mem;
            (((pat)->fld[0]).rtx) = reg;
            if (recog (pat, insn, &num_clobbers) >= 0)
              direct_load[(int) mode] = 1;

            (((pat)->fld[1]).rtx) = mem1;
            (((pat)->fld[0]).rtx) = reg;
            if (recog (pat, insn, &num_clobbers) >= 0)
              direct_load[(int) mode] = 1;

            (((pat)->fld[1]).rtx) = reg;
            (((pat)->fld[0]).rtx) = mem;
            if (recog (pat, insn, &num_clobbers) >= 0)
              direct_store[(int) mode] = 1;

            (((pat)->fld[1]).rtx) = reg;
            (((pat)->fld[0]).rtx) = mem1;
            if (recog (pat, insn, &num_clobbers) >= 0)
              direct_store[(int) mode] = 1;
          }
    }

  end_sequence ();
}



void
init_expr ()
{
  cfun->expr = (struct expr_status *) xmalloc (sizeof (struct expr_status));

  (cfun->expr->x_pending_chain) = 0;
  (cfun->expr->x_pending_stack_adjust) = 0;
  (cfun->expr->x_stack_pointer_delta) = 0;
  (cfun->expr->x_inhibit_defer_pop) = 0;
  (cfun->expr->x_saveregs_value) = 0;
  (cfun->expr->x_apply_args_value) = 0;
  (cfun->expr->x_forced_labels) = 0;
}

void
mark_expr_status (p)
     struct expr_status *p;
{
  if (p == ((void *)0))
    return;

  do { rtx r__ = (p->x_saveregs_value); if (((r__) != ((void *)0) && ! ggc_set_mark (r__))) ggc_mark_rtx_children (r__); } while (0);
  do { rtx r__ = (p->x_apply_args_value); if (((r__) != ((void *)0) && ! ggc_set_mark (r__))) ggc_mark_rtx_children (r__); } while (0);
  do { rtx r__ = (p->x_forced_labels); if (((r__) != ((void *)0) && ! ggc_set_mark (r__))) ggc_mark_rtx_children (r__); } while (0);
}

void
free_expr_status (f)
     struct function *f;
{
  free (f->expr);
  f->expr = ((void *)0);
}



void
finish_expr_for_function ()
{
  if ((cfun->expr->x_pending_chain))
    fancy_abort ("expr.c", 313, __FUNCTION__);
}
# 326 "expr.c"
static rtx
enqueue_insn (var, body)
     rtx var, body;
{
  (cfun->expr->x_pending_chain) = gen_rtx_fmt_eeeee (QUEUED, (((enum machine_mode) (var)->mode)), (var), ((rtx) 0), ((rtx) 0), (body), ((cfun->expr->x_pending_chain)));

  return (cfun->expr->x_pending_chain);
}
# 350 "expr.c"
rtx
protect_from_queue (x, modify)
     rtx x;
     int modify;
{
  enum rtx_code code = ((enum rtx_code) (x)->code);







  if (code != QUEUED)
    {





      if (code == MEM && ((enum machine_mode) (x)->mode) != BLKmode
          && ((enum rtx_code) ((((x)->fld[0]).rtx))->code) == QUEUED && !modify)
        {
          rtx y = (((x)->fld[0]).rtx);
          rtx new = replace_equiv_address_nv (x, (((y)->fld[0]).rtx));

          if ((((y)->fld[1]).rtx))
            {
              rtx temp = gen_reg_rtx (((enum machine_mode) (x)->mode));

              emit_insn_before (gen_move_insn (temp, new),
                                (((y)->fld[1]).rtx));
              return temp;
            }



          return replace_equiv_address (new, copy_to_reg ((((new)->fld[0]).rtx)));
        }



      if (code == MEM)
        {
          rtx tem = protect_from_queue ((((x)->fld[0]).rtx), 0);
          if (tem != (((x)->fld[0]).rtx))
            {
              x = copy_rtx (x);
              (((x)->fld[0]).rtx) = tem;
            }
        }
      else if (code == PLUS || code == MULT)
        {
          rtx new0 = protect_from_queue ((((x)->fld[0]).rtx), 0);
          rtx new1 = protect_from_queue ((((x)->fld[1]).rtx), 0);
          if (new0 != (((x)->fld[0]).rtx) || new1 != (((x)->fld[1]).rtx))
            {
              x = copy_rtx (x);
              (((x)->fld[0]).rtx) = new0;
              (((x)->fld[1]).rtx) = new1;
            }
        }
      return x;
    }



  if ((((x)->fld[1]).rtx) == 0)
    return copy_to_reg ((((x)->fld[0]).rtx));


  if ((((x)->fld[2]).rtx) != 0)
    return (((x)->fld[2]).rtx);


  (((x)->fld[2]).rtx) = gen_reg_rtx (((enum machine_mode) ((((x)->fld[0]).rtx))->mode));
  emit_insn_before (gen_move_insn ((((x)->fld[2]).rtx), (((x)->fld[0]).rtx)),
                    (((x)->fld[1]).rtx));
  return (((x)->fld[2]).rtx);
}






int
queued_subexp_p (x)
     rtx x;
{
  enum rtx_code code = ((enum rtx_code) (x)->code);
  switch (code)
    {
    case QUEUED:
      return 1;
    case MEM:
      return queued_subexp_p ((((x)->fld[0]).rtx));
    case MULT:
    case PLUS:
    case MINUS:
      return (queued_subexp_p ((((x)->fld[0]).rtx))
              || queued_subexp_p ((((x)->fld[1]).rtx)));
    default:
      return 0;
    }
}



void
emit_queue ()
{
  rtx p;
  while ((p = (cfun->expr->x_pending_chain)))
    {
      rtx body = (((p)->fld[3]).rtx);

      if (((enum rtx_code) (body)->code) == SEQUENCE)
        {
          (((p)->fld[1]).rtx) = ((((((((p)->fld[3]).rtx))->fld[0]).rtvec))->elem[0]);
          emit_insn ((((p)->fld[3]).rtx));
        }
      else
        (((p)->fld[1]).rtx) = emit_insn ((((p)->fld[3]).rtx));
      (cfun->expr->x_pending_chain) = (((p)->fld[4]).rtx);
    }
}






void
convert_move (to, from, unsignedp)
     rtx to, from;
     int unsignedp;
{
  enum machine_mode to_mode = ((enum machine_mode) (to)->mode);
  enum machine_mode from_mode = ((enum machine_mode) (from)->mode);
  int to_real = (mode_class[(int) (to_mode)]) == MODE_FLOAT;
  int from_real = (mode_class[(int) (from_mode)]) == MODE_FLOAT;
  enum insn_code code;
  rtx libcall;


  enum rtx_code equiv_code = (unsignedp ? ZERO_EXTEND : SIGN_EXTEND);

  to = protect_from_queue (to, 1);
  from = protect_from_queue (from, 0);

  if (to_real != from_real)
    fancy_abort ("expr.c", 502, __FUNCTION__);





  if (((enum rtx_code) (from)->code) == SUBREG && ((from)->in_struct)
      && ((mode_size[(int) (((enum machine_mode) ((((from)->fld[0]).rtx))->mode))])
          >= (mode_size[(int) (to_mode)]))
      && ((from)->unchanging) == unsignedp)
    from = gen_lowpart (to_mode, from), from_mode = to_mode;

  if (((enum rtx_code) (to)->code) == SUBREG && ((to)->in_struct))
    fancy_abort ("expr.c", 515, __FUNCTION__);

  if (to_mode == from_mode
      || (from_mode == VOIDmode && (((enum rtx_code) (from)->code) == LABEL_REF || ((enum rtx_code) (from)->code) == SYMBOL_REF || ((enum rtx_code) (from)->code) == CONST_INT || ((enum rtx_code) (from)->code) == CONST_DOUBLE || ((enum rtx_code) (from)->code) == CONST || ((enum rtx_code) (from)->code) == HIGH || ((enum rtx_code) (from)->code) == CONST_VECTOR || ((enum rtx_code) (from)->code) == CONSTANT_P_RTX)))
    {
      emit_move_insn (to, from);
      return;
    }

  if (((mode_class[(int) (to_mode)]) == MODE_VECTOR_INT || (mode_class[(int) (to_mode)]) == MODE_VECTOR_FLOAT) || ((mode_class[(int) (from_mode)]) == MODE_VECTOR_INT || (mode_class[(int) (from_mode)]) == MODE_VECTOR_FLOAT))
    {
      if ((mode_bitsize[(int) (from_mode)]) != (mode_bitsize[(int) (to_mode)]))
        fancy_abort ("expr.c", 527, __FUNCTION__);

      if (((mode_class[(int) (to_mode)]) == MODE_VECTOR_INT || (mode_class[(int) (to_mode)]) == MODE_VECTOR_FLOAT))
        from = simplify_gen_subreg (to_mode, from, ((enum machine_mode) (from)->mode), 0);
      else
        to = simplify_gen_subreg (from_mode, to, ((enum machine_mode) (to)->mode), 0);

      emit_move_insn (to, from);
      return;
    }

  if (to_real != from_real)
    fancy_abort ("expr.c", 539, __FUNCTION__);

  if (to_real)
    {
      rtx value, insns;

      if ((mode_bitsize[(int) (from_mode)]) < (mode_bitsize[(int) (to_mode)]))
        {

          if ((code = can_extend_p (to_mode, from_mode, 0))
              != CODE_FOR_nothing)
            {
              emit_unop_insn (code, to, from, UNKNOWN);
              return;
            }
        }
# 665 "expr.c"
      if (((target_flags & 0x00000001) || ((target_flags & 0x00040000) != 0)) && from_mode == DFmode && to_mode == SFmode)
        {
          emit_unop_insn (CODE_FOR_truncdfsf2, to, from, UNKNOWN);
          return;
        }


      if ((!(target_flags & 0x02000000) && (target_flags & 0x00000001)) && from_mode == XFmode && to_mode == SFmode)
        {
          emit_unop_insn (CODE_FOR_truncxfsf2, to, from, UNKNOWN);
          return;
        }


      if (((target_flags & 0x00000001)) && from_mode == TFmode && to_mode == SFmode)
        {
          emit_unop_insn (CODE_FOR_trunctfsf2, to, from, UNKNOWN);
          return;
        }


      if ((!(target_flags & 0x02000000) && (target_flags & 0x00000001)) && from_mode == XFmode && to_mode == DFmode)
        {
          emit_unop_insn (CODE_FOR_truncxfdf2, to, from, UNKNOWN);
          return;
        }


      if (((target_flags & 0x00000001)) && from_mode == TFmode && to_mode == DFmode)
        {
          emit_unop_insn (CODE_FOR_trunctfdf2, to, from, UNKNOWN);
          return;
        }


      libcall = (rtx) 0;
      switch (from_mode)
        {
        case SFmode:
          switch (to_mode)
            {
            case DFmode:
              libcall = (libfunc_table[LTI_extendsfdf2]);
              break;

            case XFmode:
              libcall = (libfunc_table[LTI_extendsfxf2]);
              break;

            case TFmode:
              libcall = (libfunc_table[LTI_extendsftf2]);
              break;

            default:
              break;
            }
          break;

        case DFmode:
          switch (to_mode)
            {
            case SFmode:
              libcall = (libfunc_table[LTI_truncdfsf2]);
              break;

            case XFmode:
              libcall = (libfunc_table[LTI_extenddfxf2]);
              break;

            case TFmode:
              libcall = (libfunc_table[LTI_extenddftf2]);
              break;

            default:
              break;
            }
          break;

        case XFmode:
          switch (to_mode)
            {
            case SFmode:
              libcall = (libfunc_table[LTI_truncxfsf2]);
              break;

            case DFmode:
              libcall = (libfunc_table[LTI_truncxfdf2]);
              break;

            default:
              break;
            }
          break;

        case TFmode:
          switch (to_mode)
            {
            case SFmode:
              libcall = (libfunc_table[LTI_trunctfsf2]);
              break;

            case DFmode:
              libcall = (libfunc_table[LTI_trunctfdf2]);
              break;

            default:
              break;
            }
          break;

        default:
          break;
        }

      if (libcall == (rtx) 0)

        fancy_abort ("expr.c", 781, __FUNCTION__);

      start_sequence ();
      value = emit_library_call_value (libcall, (rtx) 0, LCT_CONST, to_mode,
                                       1, from, from_mode);
      insns = get_insns ();
      end_sequence ();
      emit_libcall_block (insns, to, value, gen_rtx_fmt_e (FLOAT_TRUNCATE, (to_mode), (from)));

      return;
    }




  if ((mode_bitsize[(int) (from_mode)]) < (mode_bitsize[(int) (to_mode)])
      && (mode_bitsize[(int) (to_mode)]) > ((target_flags & 0x02000000) ? 64 : 32))
    {
      rtx insns;
      rtx lowpart;
      rtx fill_value;
      rtx lowfrom;
      int i;
      enum machine_mode lowpart_mode;
      int nwords = ((((mode_size[(int) (to_mode)])) + (((target_flags & 0x02000000) ? 8 : 4)) - 1) / (((target_flags & 0x02000000) ? 8 : 4)));


      if ((code = can_extend_p (to_mode, from_mode, unsignedp))
          != CODE_FOR_nothing)
        {




          if (optimize > 0 && ((enum rtx_code) (from)->code) == SUBREG)
            from = force_reg (from_mode, from);
          emit_unop_insn (code, to, from, equiv_code);
          return;
        }

      else if ((mode_bitsize[(int) (from_mode)]) < ((target_flags & 0x02000000) ? 64 : 32)
               && ((code = can_extend_p (to_mode, word_mode, unsignedp))
                   != CODE_FOR_nothing))
        {
          if (((enum rtx_code) (to)->code) == REG)
            emit_insn (gen_rtx_fmt_e (CLOBBER, (VOIDmode), (to)));
          convert_move (gen_lowpart (word_mode, to), from, unsignedp);
          emit_unop_insn (code, to,
                          gen_lowpart (word_mode, to), equiv_code);
          return;
        }


      start_sequence ();




      if (reg_overlap_mentioned_p (to, from))
        from = force_reg (from_mode, from);


      if ((mode_bitsize[(int) (from_mode)]) < ((target_flags & 0x02000000) ? 64 : 32))
        lowpart_mode = word_mode;
      else
        lowpart_mode = from_mode;

      lowfrom = convert_to_mode (lowpart_mode, from, unsignedp);

      lowpart = gen_lowpart (lowpart_mode, to);
      emit_move_insn (lowpart, lowfrom);


      if (unsignedp)
        fill_value = (const_int_rtx[64]);
      else
        {

          if (1
              && insn_data[(int) CODE_FOR_slt].operand[0].mode == word_mode
              && 1 == -1)
            {
              emit_cmp_insn (lowfrom, (const_int_rtx[64]), NE, (rtx) 0,
                             lowpart_mode, 0);
              fill_value = gen_reg_rtx (word_mode);
              emit_insn (gen_slt (fill_value));
            }
          else

            {
              fill_value
                = expand_shift (RSHIFT_EXPR, lowpart_mode, lowfrom,
                                size_int_wide ((long long) ((mode_bitsize[(int) (lowpart_mode)]) - 1), SIZETYPE),
                                (rtx) 0, 0);
              fill_value = convert_to_mode (word_mode, fill_value, 1);
            }
        }


      for (i = (mode_size[(int) (lowpart_mode)]) / ((target_flags & 0x02000000) ? 8 : 4); i < nwords; i++)
        {
          int index = (0 ? nwords - i - 1 : i);
          rtx subword = operand_subword (to, index, 1, to_mode);

          if (subword == 0)
            fancy_abort ("expr.c", 886, __FUNCTION__);

          if (fill_value != subword)
            emit_move_insn (subword, fill_value);
        }

      insns = get_insns ();
      end_sequence ();

      emit_no_conflict_block (insns, to, from, (rtx) 0,
                              gen_rtx_fmt_e (equiv_code, to_mode, copy_rtx (from)));
      return;
    }


  if ((mode_bitsize[(int) (from_mode)]) > ((target_flags & 0x02000000) ? 64 : 32)
      && (mode_bitsize[(int) (to_mode)]) <= ((target_flags & 0x02000000) ? 64 : 32))
    {
      if (!((((enum rtx_code) (from)->code) == MEM
             && ! ((from)->volatil)
             && direct_load[(int) to_mode]
             && ! mode_dependent_address_p ((((from)->fld[0]).rtx)))
            || ((enum rtx_code) (from)->code) == REG
            || ((enum rtx_code) (from)->code) == SUBREG))
        from = force_reg (from_mode, from);
      convert_move (to, gen_lowpart (word_mode, from), 0);
      return;
    }


  if (to_mode == PQImode)
    {
      if (from_mode != QImode)
        from = convert_to_mode (QImode, from, unsignedp);
# 928 "expr.c"
      fancy_abort ("expr.c", 928, __FUNCTION__);
    }

  if (from_mode == PQImode)
    {
      if (to_mode != QImode)
        {
          from = convert_to_mode (QImode, from, unsignedp);
          from_mode = QImode;
        }
      else
        {







          fancy_abort ("expr.c", 947, __FUNCTION__);
        }
    }

  if (to_mode == PSImode)
    {
      if (from_mode != SImode)
        from = convert_to_mode (SImode, from, unsignedp);
# 963 "expr.c"
      fancy_abort ("expr.c", 963, __FUNCTION__);
    }

  if (from_mode == PSImode)
    {
      if (to_mode != SImode)
        {
          from = convert_to_mode (SImode, from, unsignedp);
          from_mode = SImode;
        }
      else
        {
# 989 "expr.c"
          fancy_abort ("expr.c", 989, __FUNCTION__);
        }
    }

  if (to_mode == PDImode)
    {
      if (from_mode != DImode)
        from = convert_to_mode (DImode, from, unsignedp);
# 1005 "expr.c"
      fancy_abort ("expr.c", 1005, __FUNCTION__);
    }

  if (from_mode == PDImode)
    {
      if (to_mode != DImode)
        {
          from = convert_to_mode (DImode, from, unsignedp);
          from_mode = DImode;
        }
      else
        {







          fancy_abort ("expr.c", 1024, __FUNCTION__);
        }
    }





  if ((mode_bitsize[(int) (to_mode)]) < (mode_bitsize[(int) (from_mode)])
      && 1)

    {
      if (!((((enum rtx_code) (from)->code) == MEM
             && ! ((from)->volatil)
             && direct_load[(int) to_mode]
             && ! mode_dependent_address_p ((((from)->fld[0]).rtx)))
            || ((enum rtx_code) (from)->code) == REG
            || ((enum rtx_code) (from)->code) == SUBREG))
        from = force_reg (from_mode, from);
      if (((enum rtx_code) (from)->code) == REG && (((from)->fld[0]).rtuint) < 53
          && ! ix86_hard_regno_mode_ok (((((from)->fld[0]).rtuint)), (to_mode)))
        from = copy_to_reg (from);
      emit_move_insn (to, gen_lowpart (to_mode, from));
      return;
    }


  if ((mode_bitsize[(int) (to_mode)]) > (mode_bitsize[(int) (from_mode)]))
    {

      if ((code = can_extend_p (to_mode, from_mode, unsignedp))
          != CODE_FOR_nothing)
        {
          if (flag_force_mem)
            from = force_not_mem (from);

          emit_unop_insn (code, to, from, equiv_code);
          return;
        }
      else
        {
          enum machine_mode intermediate;
          rtx tmp;
          tree shift_amount;


          for (intermediate = from_mode; intermediate != VOIDmode;
               intermediate = ((enum machine_mode)mode_wider_mode[(int) (intermediate)]))
            if (((can_extend_p (to_mode, intermediate, unsignedp)
                  != CODE_FOR_nothing)
                 || ((mode_size[(int) (to_mode)]) < (mode_size[(int) (intermediate)])
                     && 1))

                && (can_extend_p (intermediate, from_mode, unsignedp)
                    != CODE_FOR_nothing))
              {
                convert_move (to, convert_to_mode (intermediate, from,
                                                   unsignedp), unsignedp);
                return;
              }



          shift_amount = build_int_2_wide ((unsigned long long) ((mode_bitsize[(int) (to_mode)]) - (mode_bitsize[(int) (from_mode)])), (long long) (0));

          from = gen_lowpart (to_mode, force_reg (from_mode, from));
          tmp = expand_shift (LSHIFT_EXPR, to_mode, from, shift_amount,
                              to, unsignedp);
          tmp = expand_shift (RSHIFT_EXPR, to_mode, tmp, shift_amount,
                              to, unsignedp);
          if (tmp != to)
            emit_move_insn (to, tmp);
          return;
        }
    }



  if (from_mode == DImode && to_mode == SImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == DImode && to_mode == HImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == DImode && to_mode == QImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == SImode && to_mode == HImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == SImode && to_mode == QImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == HImode && to_mode == QImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == TImode && to_mode == DImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == TImode && to_mode == SImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == TImode && to_mode == HImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }

  if (from_mode == TImode && to_mode == QImode)
    {







      convert_move (to, force_reg (from_mode, from), unsignedp);
      return;
    }




  if ((mode_bitsize[(int) (to_mode)]) < (mode_bitsize[(int) (from_mode)]))
    {
      rtx temp = force_reg (to_mode, gen_lowpart (to_mode, from));
      emit_move_insn (to, temp);
      return;
    }


  fancy_abort ("expr.c", 1243, __FUNCTION__);
}
# 1256 "expr.c"
rtx
convert_to_mode (mode, x, unsignedp)
     enum machine_mode mode;
     rtx x;
     int unsignedp;
{
  return convert_modes (mode, VOIDmode, x, unsignedp);
}
# 1278 "expr.c"
rtx
convert_modes (mode, oldmode, x, unsignedp)
     enum machine_mode mode, oldmode;
     rtx x;
     int unsignedp;
{
  rtx temp;




  if (((enum rtx_code) (x)->code) == SUBREG && ((x)->in_struct)
      && (mode_size[(int) (((enum machine_mode) ((((x)->fld[0]).rtx))->mode))]) >= (mode_size[(int) (mode)])
      && ((x)->unchanging) == unsignedp)
    x = gen_lowpart (mode, x);

  if (((enum machine_mode) (x)->mode) != VOIDmode)
    oldmode = ((enum machine_mode) (x)->mode);

  if (mode == oldmode)
    return x;







  if (unsignedp && (mode_class[(int) (mode)]) == MODE_INT
      && (mode_bitsize[(int) (mode)]) == 2 * (8 * 8)
      && ((enum rtx_code) (x)->code) == CONST_INT && (((x)->fld[0]).rtwint) < 0)
    {
      long long val = (((x)->fld[0]).rtwint);

      if (oldmode != VOIDmode
          && (8 * 8) > (mode_bitsize[(int) (oldmode)]))
        {
          int width = (mode_bitsize[(int) (oldmode)]);


          val &= ((long long) 1 << width) - 1;
        }

      return immed_double_const (val, (long long) 0, mode);
    }






  if ((((enum rtx_code) (x)->code) == CONST_INT
       && (mode_bitsize[(int) (mode)]) <= (8 * 8))
      || ((mode_class[(int) (mode)]) == MODE_INT
          && (mode_class[(int) (oldmode)]) == MODE_INT
          && (((enum rtx_code) (x)->code) == CONST_DOUBLE
              || ((mode_size[(int) (mode)]) <= (mode_size[(int) (oldmode)])
                  && ((((enum rtx_code) (x)->code) == MEM && ! ((x)->volatil)
                       && direct_load[(int) mode])
                      || (((enum rtx_code) (x)->code) == REG
                          && 1))))))

    {



      if (((enum rtx_code) (x)->code) == CONST_INT && oldmode != VOIDmode
          && (mode_size[(int) (mode)]) > (mode_size[(int) (oldmode)]))
        {
          long long val = (((x)->fld[0]).rtwint);
          int width = (mode_bitsize[(int) (oldmode)]);



          val &= ((long long) 1 << width) - 1;
          if (! unsignedp
              && (val & ((long long) 1 << (width - 1))))
            val |= (long long) (-1) << width;

          return gen_rtx_CONST_INT (VOIDmode, (long long) (trunc_int_for_mode (val, mode)));
        }

      return gen_lowpart (mode, x);
    }

  temp = gen_reg_rtx (mode);
  convert_move (temp, x, unsignedp);
  return temp;
}
# 1388 "expr.c"
void
move_by_pieces (to, from, len, align)
     rtx to, from;
     unsigned long long len;
     unsigned int align;
{
  struct move_by_pieces data;
  rtx to_addr, from_addr = (((from)->fld[0]).rtx);
  unsigned int max_size = ((target_flags & 0x02000000) ? 8 : 4) + 1;
  enum machine_mode mode = VOIDmode, tmode;
  enum insn_code icode;

  data.offset = 0;
  data.from_addr = from_addr;
  if (to)
    {
      to_addr = (((to)->fld[0]).rtx);
      data.to = to;
      data.autinc_to
        = (((enum rtx_code) (to_addr)->code) == PRE_INC || ((enum rtx_code) (to_addr)->code) == PRE_DEC
           || ((enum rtx_code) (to_addr)->code) == POST_INC || ((enum rtx_code) (to_addr)->code) == POST_DEC);
      data.reverse
        = (((enum rtx_code) (to_addr)->code) == PRE_DEC || ((enum rtx_code) (to_addr)->code) == POST_DEC);
    }
  else
    {
      to_addr = (rtx) 0;
      data.to = (rtx) 0;
      data.autinc_to = 1;

      data.reverse = 1;



    }
  data.to_addr = to_addr;
  data.from = from;
  data.autinc_from
    = (((enum rtx_code) (from_addr)->code) == PRE_INC || ((enum rtx_code) (from_addr)->code) == PRE_DEC
       || ((enum rtx_code) (from_addr)->code) == POST_INC
       || ((enum rtx_code) (from_addr)->code) == POST_DEC);

  data.explicit_inc_from = 0;
  data.explicit_inc_to = 0;
  if (data.reverse) data.offset = len;
  data.len = len;




  if (!(data.autinc_from && data.autinc_to)
      && move_by_pieces_ninsns (len, align) > 2)
    {

      for (tmode = class_narrowest_mode[(int) (MODE_INT)];
           tmode != VOIDmode; tmode = ((enum machine_mode)mode_wider_mode[(int) (tmode)]))
        if ((mode_size[(int) (tmode)]) < max_size)
          mode = tmode;

      if (0 && data.reverse && ! data.autinc_from)
        {
          data.from_addr = copy_addr_to_reg (plus_constant_wide ((from_addr), (long long) (len)));
          data.autinc_from = 1;
          data.explicit_inc_from = -1;
        }
      if (0 && ! data.autinc_from)
        {
          data.from_addr = copy_addr_to_reg (from_addr);
          data.autinc_from = 1;
          data.explicit_inc_from = 1;
        }
      if (!data.autinc_from && (((enum rtx_code) (from_addr)->code) == LABEL_REF || ((enum rtx_code) (from_addr)->code) == SYMBOL_REF || ((enum rtx_code) (from_addr)->code) == CONST_INT || ((enum rtx_code) (from_addr)->code) == CONST_DOUBLE || ((enum rtx_code) (from_addr)->code) == CONST || ((enum rtx_code) (from_addr)->code) == HIGH || ((enum rtx_code) (from_addr)->code) == CONST_VECTOR || ((enum rtx_code) (from_addr)->code) == CONSTANT_P_RTX))
        data.from_addr = copy_addr_to_reg (from_addr);
      if (0 && data.reverse && ! data.autinc_to)
        {
          data.to_addr = copy_addr_to_reg (plus_constant_wide ((to_addr), (long long) (len)));
          data.autinc_to = 1;
          data.explicit_inc_to = -1;
        }
      if (0 && ! data.reverse && ! data.autinc_to)
        {
          data.to_addr = copy_addr_to_reg (to_addr);
          data.autinc_to = 1;
          data.explicit_inc_to = 1;
        }
      if (!data.autinc_to && (((enum rtx_code) (to_addr)->code) == LABEL_REF || ((enum rtx_code) (to_addr)->code) == SYMBOL_REF || ((enum rtx_code) (to_addr)->code) == CONST_INT || ((enum rtx_code) (to_addr)->code) == CONST_DOUBLE || ((enum rtx_code) (to_addr)->code) == CONST || ((enum rtx_code) (to_addr)->code) == HIGH || ((enum rtx_code) (to_addr)->code) == CONST_VECTOR || ((enum rtx_code) (to_addr)->code) == CONSTANT_P_RTX))
        data.to_addr = copy_addr_to_reg (to_addr);
    }

  if (! 0
      || align > 16 * 8 || align >= 128)
    align = 16 * 8;




  while (max_size > 1)
    {
      for (tmode = class_narrowest_mode[(int) (MODE_INT)];
           tmode != VOIDmode; tmode = ((enum machine_mode)mode_wider_mode[(int) (tmode)]))
        if ((mode_size[(int) (tmode)]) < max_size)
          mode = tmode;

      if (mode == VOIDmode)
        break;

      icode = (optab_table[OTI_mov])->handlers[(int) mode].insn_code;
      if (icode != CODE_FOR_nothing && align >= get_mode_alignment (mode))
        move_by_pieces_1 ((*insn_data[(int) (icode)].genfun), mode, &data);

      max_size = (mode_size[(int) (mode)]);
    }


  if (data.len > 0)
    fancy_abort ("expr.c", 1503, __FUNCTION__);
}




static unsigned long long
move_by_pieces_ninsns (l, align)
     unsigned long long l;
     unsigned int align;
{
  unsigned long long n_insns = 0;
  unsigned long long max_size = 16 + 1;

  if (! 0
      || align > 16 * 8 || align >= 128)
    align = 16 * 8;

  while (max_size > 1)
    {
      enum machine_mode mode = VOIDmode, tmode;
      enum insn_code icode;

      for (tmode = class_narrowest_mode[(int) (MODE_INT)];
           tmode != VOIDmode; tmode = ((enum machine_mode)mode_wider_mode[(int) (tmode)]))
        if ((mode_size[(int) (tmode)]) < max_size)
          mode = tmode;

      if (mode == VOIDmode)
        break;

      icode = (optab_table[OTI_mov])->handlers[(int) mode].insn_code;
      if (icode != CODE_FOR_nothing && align >= get_mode_alignment (mode))
        n_insns += l / (mode_size[(int) (mode)]), l %= (mode_size[(int) (mode)]);

      max_size = (mode_size[(int) (mode)]);
    }

  if (l)
    fancy_abort ("expr.c", 1542, __FUNCTION__);
  return n_insns;
}





static void
move_by_pieces_1 (genfun, mode, data)
     rtx (*genfun) (rtx, ...);
     enum machine_mode mode;
     struct move_by_pieces *data;
{
  unsigned int size = (mode_size[(int) (mode)]);
  rtx to1 = (rtx) 0, from1;

  while (data->len >= size)
    {
      if (data->reverse)
        data->offset -= size;

      if (data->to)
        {
          if (data->autinc_to)
            to1 = adjust_automodify_address_1 (data->to, mode, data->to_addr, data->offset, 1);

          else
            to1 = adjust_address_1 (data->to, mode, data->offset, 1, 1);
        }

      if (data->autinc_from)
        from1 = adjust_automodify_address_1 (data->from, mode, data->from_addr, data->offset, 1);

      else
        from1 = adjust_address_1 (data->from, mode, data->offset, 1, 1);

      if (0 && data->explicit_inc_to < 0)
        emit_insn (gen_add2_insn (data->to_addr,
                                  gen_rtx_CONST_INT (VOIDmode, (long long) (-(long long)size))));
      if (0 && data->explicit_inc_from < 0)
        emit_insn (gen_add2_insn (data->from_addr,
                                  gen_rtx_CONST_INT (VOIDmode, (long long) (-(long long)size))));

      if (data->to)
        emit_insn ((*genfun) (to1, from1));
      else
        {

          emit_single_push_insn (mode, from1, ((void *)0));



        }

      if (0 && data->explicit_inc_to > 0)
        emit_insn (gen_add2_insn (data->to_addr, gen_rtx_CONST_INT (VOIDmode, (long long) (size))));
      if (0 && data->explicit_inc_from > 0)
        emit_insn (gen_add2_insn (data->from_addr, gen_rtx_CONST_INT (VOIDmode, (long long) (size))));

      if (! data->reverse)
        data->offset += size;

      data->len -= size;
    }
}
# 1621 "expr.c"
rtx
emit_block_move (x, y, size)
     rtx x, y;
     rtx size;
{
  rtx retval = 0;

  static tree fn;
  tree call_expr, arg_list;

  unsigned int align = ((((((x)->fld[1]).rtmem) != 0 ? (((x)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (x)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (x)->mode)) : 8))) < (((((y)->fld[1]).rtmem) != 0 ? (((y)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (y)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (y)->mode)) : 8))) ? (((((x)->fld[1]).rtmem) != 0 ? (((x)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (x)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (x)->mode)) : 8))) : (((((y)->fld[1]).rtmem) != 0 ? (((y)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (y)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (y)->mode)) : 8))));

  if (((enum machine_mode) (x)->mode) != BLKmode)
    fancy_abort ("expr.c", 1634, __FUNCTION__);

  if (((enum machine_mode) (y)->mode) != BLKmode)
    fancy_abort ("expr.c", 1637, __FUNCTION__);

  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);
  size = protect_from_queue (size, 0);

  if (((enum rtx_code) (x)->code) != MEM)
    fancy_abort ("expr.c", 1644, __FUNCTION__);
  if (((enum rtx_code) (y)->code) != MEM)
    fancy_abort ("expr.c", 1646, __FUNCTION__);
  if (size == 0)
    fancy_abort ("expr.c", 1648, __FUNCTION__);

  if (((enum rtx_code) (size)->code) == CONST_INT && (move_by_pieces_ninsns ((((size)->fld[0]).rtwint), align) < (unsigned int) (optimize_size ? 3 : ix86_cost->move_ratio)))
    move_by_pieces (x, y, (((size)->fld[0]).rtwint), align);
  else
    {




      rtx opalign = gen_rtx_CONST_INT (VOIDmode, (long long) (align / 8));
      enum machine_mode mode;


      volatile_ok = 1;

      for (mode = class_narrowest_mode[(int) (MODE_INT)]; mode != VOIDmode;
           mode = ((enum machine_mode)mode_wider_mode[(int) (mode)]))
        {
          enum insn_code code = movstr_optab[(int) mode];
          insn_operand_predicate_fn pred;

          if (code != CODE_FOR_nothing




              && ((((enum rtx_code) (size)->code) == CONST_INT
                   && ((unsigned long long) (((size)->fld[0]).rtwint)
                       <= (mode_mask_array[(int) (mode)] >> 1)))
                  || (mode_bitsize[(int) (mode)]) >= ((target_flags & 0x02000000) ? 64 : 32))
              && ((pred = insn_data[(int) code].operand[0].predicate) == 0
                  || (*pred) (x, BLKmode))
              && ((pred = insn_data[(int) code].operand[1].predicate) == 0
                  || (*pred) (y, BLKmode))
              && ((pred = insn_data[(int) code].operand[3].predicate) == 0
                  || (*pred) (opalign, VOIDmode)))
            {
              rtx op2;
              rtx last = get_last_insn ();
              rtx pat;

              op2 = convert_to_mode (mode, size, 1);
              pred = insn_data[(int) code].operand[2].predicate;
              if (pred != 0 && ! (*pred) (op2, mode))
                op2 = copy_to_mode_reg (mode, op2);

              pat = (*insn_data[(int) ((int) code)].genfun) (x, y, op2, opalign);
              if (pat)
                {
                  emit_insn (pat);
                  volatile_ok = 0;
                  return 0;
                }
              else
                delete_insns_since (last);
            }
        }

      volatile_ok = 0;
# 1729 "expr.c"
      x = copy_to_mode_reg (((target_flags & 0x02000000) ? DImode : SImode), (((x)->fld[0]).rtx));
      y = copy_to_mode_reg (((target_flags & 0x02000000) ? DImode : SImode), (((y)->fld[0]).rtx));


      size = copy_to_mode_reg (((sizetype_tab[(int) SIZETYPE])->type.mode), size);
# 1752 "expr.c"
      if (fn == (tree) ((void *)0))
        {
          tree fntype;



          fn = get_identifier ("memcpy");
          fntype = build_pointer_type (global_trees[TI_VOID_TYPE]);
          fntype = build_function_type (fntype, (tree) ((void *)0));
          fn = build_decl (FUNCTION_DECL, fn, fntype);
          ggc_add_tree_root (&fn, 1);
          ((fn)->decl.external_flag) = 1;
          ((fn)->common.public_flag) = 1;
          ((fn)->decl.artificial_flag) = 1;
          ((fn)->common.nothrow_flag) = 1;
          make_decl_rtl (fn, ((void *)0));
          assemble_external (fn);
        }





      arg_list
        = build_tree_list ((tree) ((void *)0),
                           make_tree (build_pointer_type (global_trees[TI_VOID_TYPE]), x));
      ((arg_list)->common.chain)
        = build_tree_list ((tree) ((void *)0),
                           make_tree (build_pointer_type (global_trees[TI_VOID_TYPE]), y));
      ((((arg_list)->common.chain))->common.chain)
         = build_tree_list ((tree) ((void *)0), make_tree (sizetype_tab[(int) SIZETYPE], size));
      ((((((arg_list)->common.chain))->common.chain))->common.chain) = (tree) ((void *)0);


      call_expr = build1 (ADDR_EXPR, build_pointer_type (((fn)->common.type)), fn);
      call_expr = build (CALL_EXPR, ((((fn)->common.type))->common.type),
                         call_expr, arg_list, (tree) ((void *)0));
      ((call_expr)->common.side_effects_flag) = 1;

      retval = expand_expr (call_expr, (rtx) 0, VOIDmode, 0);
# 1803 "expr.c"
      if (((x)->unchanging))
        emit_insn (gen_rtx_fmt_e (CLOBBER, (VOIDmode), (x)));
    }

  return retval;
}




void
move_block_to_reg (regno, x, nregs, mode)
     int regno;
     rtx x;
     int nregs;
     enum machine_mode mode;
{
  int i;





  if (nregs == 0)
    return;

  if ((((enum rtx_code) (x)->code) == LABEL_REF || ((enum rtx_code) (x)->code) == SYMBOL_REF || ((enum rtx_code) (x)->code) == CONST_INT || ((enum rtx_code) (x)->code) == CONST_DOUBLE || ((enum rtx_code) (x)->code) == CONST || ((enum rtx_code) (x)->code) == HIGH || ((enum rtx_code) (x)->code) == CONST_VECTOR || ((enum rtx_code) (x)->code) == CONSTANT_P_RTX) && ! 1)
    x = validize_mem (force_const_mem (mode, x));
# 1849 "expr.c"
  for (i = 0; i < nregs; i++)
    emit_move_insn (gen_rtx_REG (word_mode, regno + i),
                    operand_subword_force (x, i, mode));
}





void
move_block_from_reg (regno, x, nregs, size)
     int regno;
     rtx x;
     int nregs;
     int size;
{
  int i;




  enum machine_mode mode;

  if (nregs == 0)
    return;



  if (size <= ((target_flags & 0x02000000) ? 8 : 4)
      && (mode = mode_for_size (size * 8, MODE_INT, 0)) != BLKmode
      && !0)
    {
      emit_move_insn (adjust_address_1 (x, mode, 0, 1, 1), gen_rtx_REG (mode, regno));
      return;
    }




  if (size < ((target_flags & 0x02000000) ? 8 : 4)
      && 0
      && !0)
    {
      rtx tem = operand_subword (x, 0, 1, BLKmode);
      rtx shift;

      if (tem == 0)
        fancy_abort ("expr.c", 1896, __FUNCTION__);

      shift = expand_shift (LSHIFT_EXPR, word_mode,
                            gen_rtx_REG (word_mode, regno),
                            build_int_2_wide ((unsigned long long) ((((target_flags & 0x02000000) ? 8 : 4) - size) * 8), (long long) (0)), (rtx) 0, 0);

      emit_move_insn (tem, shift);
      return;
    }
# 1923 "expr.c"
  for (i = 0; i < nregs; i++)
    {
      rtx tem = operand_subword (x, i, 1, BLKmode);

      if (tem == 0)
        fancy_abort ("expr.c", 1928, __FUNCTION__);

      emit_move_insn (tem, gen_rtx_REG (word_mode, regno + i));
    }
}
# 1944 "expr.c"
void
emit_group_load (dst, orig_src, ssize)
     rtx dst, orig_src;
     int ssize;
{
  rtx *tmps, src;
  int start, i;

  if (((enum rtx_code) (dst)->code) != PARALLEL)
    fancy_abort ("expr.c", 1953, __FUNCTION__);



  if (((((((((dst)->fld[0]).rtvec))->elem[0]))->fld[0]).rtx))
    start = 0;
  else
    start = 1;

  tmps = (rtx *) __builtin_alloca (sizeof (rtx) * (((((dst)->fld[0]).rtvec))->num_elem));


  for (i = start; i < (((((dst)->fld[0]).rtvec))->num_elem); i++)
    {
      enum machine_mode mode = ((enum machine_mode) (((((((((dst)->fld[0]).rtvec))->elem[i]))->fld[0]).rtx))->mode);
      long long bytepos = (((((((((((dst)->fld[0]).rtvec))->elem[i]))->fld[1]).rtx))->fld[0]).rtwint);
      unsigned int bytelen = (mode_size[(int) (mode)]);
      int shift = 0;


      if (ssize >= 0 && bytepos + (long long) bytelen > ssize)
        {
          shift = (bytelen - (ssize - bytepos)) * 8;
          bytelen = ssize - bytepos;
          if (bytelen <= 0)
            fancy_abort ("expr.c", 1978, __FUNCTION__);
        }




      src = orig_src;
      if (((enum rtx_code) (orig_src)->code) != MEM
          && (!(((enum rtx_code) (orig_src)->code) == LABEL_REF || ((enum rtx_code) (orig_src)->code) == SYMBOL_REF || ((enum rtx_code) (orig_src)->code) == CONST_INT || ((enum rtx_code) (orig_src)->code) == CONST_DOUBLE || ((enum rtx_code) (orig_src)->code) == CONST || ((enum rtx_code) (orig_src)->code) == HIGH || ((enum rtx_code) (orig_src)->code) == CONST_VECTOR || ((enum rtx_code) (orig_src)->code) == CONSTANT_P_RTX)
              || (((enum machine_mode) (orig_src)->mode) != mode
                  && ((enum machine_mode) (orig_src)->mode) != VOIDmode)))
        {
          if (((enum machine_mode) (orig_src)->mode) == VOIDmode)
            src = gen_reg_rtx (mode);
          else
            src = gen_reg_rtx (((enum machine_mode) (orig_src)->mode));

          emit_move_insn (src, orig_src);
        }


      if (((enum rtx_code) (src)->code) == MEM
          && ((((src)->fld[1]).rtmem) != 0 ? (((src)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (src)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (src)->mode)) : 8)) >= get_mode_alignment (mode)
          && bytepos * 8 % get_mode_alignment (mode) == 0
          && bytelen == (mode_size[(int) (mode)]))
        {
          tmps[i] = gen_reg_rtx (mode);
          emit_move_insn (tmps[i], adjust_address_1 (src, mode, bytepos, 1, 1));
        }
      else if (((enum rtx_code) (src)->code) == CONCAT)
        {
          if ((bytepos == 0
               && bytelen == (mode_size[(int) (((enum machine_mode) ((((src)->fld[0]).rtx))->mode))]))
              || (bytepos == (long long) (mode_size[(int) (((enum machine_mode) ((((src)->fld[0]).rtx))->mode))])
                  && bytelen == (mode_size[(int) (((enum machine_mode) ((((src)->fld[1]).rtx))->mode))])))
            {
              tmps[i] = (((src)->fld[bytepos != 0]).rtx);
              if (! (((enum rtx_code) (tmps[i])->code) == LABEL_REF || ((enum rtx_code) (tmps[i])->code) == SYMBOL_REF || ((enum rtx_code) (tmps[i])->code) == CONST_INT || ((enum rtx_code) (tmps[i])->code) == CONST_DOUBLE || ((enum rtx_code) (tmps[i])->code) == CONST || ((enum rtx_code) (tmps[i])->code) == HIGH || ((enum rtx_code) (tmps[i])->code) == CONST_VECTOR || ((enum rtx_code) (tmps[i])->code) == CONSTANT_P_RTX)
                  && (((enum rtx_code) (tmps[i])->code) != REG || ((enum machine_mode) (tmps[i])->mode) != mode))
                tmps[i] = extract_bit_field (tmps[i], bytelen * 8,
                                             0, 1, (rtx) 0, mode, mode, ssize);
            }
          else if (bytepos == 0)
            {
              rtx mem = assign_stack_temp (((enum machine_mode) (src)->mode),
                                           (mode_size[(int) (((enum machine_mode) (src)->mode))]), 0);
              emit_move_insn (mem, src);
              tmps[i] = adjust_address_1 (mem, mode, 0, 1, 1);
            }
          else
            fancy_abort ("expr.c", 2028, __FUNCTION__);
        }
      else if ((((enum rtx_code) (src)->code) == LABEL_REF || ((enum rtx_code) (src)->code) == SYMBOL_REF || ((enum rtx_code) (src)->code) == CONST_INT || ((enum rtx_code) (src)->code) == CONST_DOUBLE || ((enum rtx_code) (src)->code) == CONST || ((enum rtx_code) (src)->code) == HIGH || ((enum rtx_code) (src)->code) == CONST_VECTOR || ((enum rtx_code) (src)->code) == CONSTANT_P_RTX)
               || (((enum rtx_code) (src)->code) == REG && ((enum machine_mode) (src)->mode) == mode))
        tmps[i] = src;
      else
        tmps[i] = extract_bit_field (src, bytelen * 8,
                                     bytepos * 8, 1, (rtx) 0,
                                     mode, mode, ssize);

      if (0 && shift)
        expand_binop (mode, (optab_table[OTI_ashl]), tmps[i], gen_rtx_CONST_INT (VOIDmode, (long long) (shift)),
                      tmps[i], 0, OPTAB_WIDEN);
    }

  emit_queue ();


  for (i = start; i < (((((dst)->fld[0]).rtvec))->num_elem); i++)
    emit_move_insn (((((((((dst)->fld[0]).rtvec))->elem[i]))->fld[0]).rtx), tmps[i]);
}





void
emit_group_store (orig_dst, src, ssize)
     rtx orig_dst, src;
     int ssize;
{
  rtx *tmps, dst;
  int start, i;

  if (((enum rtx_code) (src)->code) != PARALLEL)
    fancy_abort ("expr.c", 2063, __FUNCTION__);



  if (((((((((src)->fld[0]).rtvec))->elem[0]))->fld[0]).rtx))
    start = 0;
  else
    start = 1;

  tmps = (rtx *) __builtin_alloca (sizeof (rtx) * (((((src)->fld[0]).rtvec))->num_elem));


  for (i = start; i < (((((src)->fld[0]).rtvec))->num_elem); i++)
    {
      rtx reg = ((((((((src)->fld[0]).rtvec))->elem[i]))->fld[0]).rtx);
      tmps[i] = gen_reg_rtx (((enum machine_mode) (reg)->mode));
      emit_move_insn (tmps[i], reg);
    }
  emit_queue ();



  dst = orig_dst;
  if (((enum rtx_code) (dst)->code) == PARALLEL)
    {
      rtx temp;




      if (rtx_equal_p (dst, src))
        return;





      temp = assign_stack_temp (((enum machine_mode) (dst)->mode), ssize, 0);
      emit_group_store (temp, src, ssize);
      emit_group_load (dst, temp, ssize);
      return;
    }
  else if (((enum rtx_code) (dst)->code) != MEM && ((enum rtx_code) (dst)->code) != CONCAT)
    {
      dst = gen_reg_rtx (((enum machine_mode) (orig_dst)->mode));

      emit_move_insn (dst, (const_int_rtx[64]));
    }


  for (i = start; i < (((((src)->fld[0]).rtvec))->num_elem); i++)
    {
      long long bytepos = (((((((((((src)->fld[0]).rtvec))->elem[i]))->fld[1]).rtx))->fld[0]).rtwint);
      enum machine_mode mode = ((enum machine_mode) (tmps[i])->mode);
      unsigned int bytelen = (mode_size[(int) (mode)]);
      rtx dest = dst;


      if (ssize >= 0 && bytepos + (long long) bytelen > ssize)
        {
          if (0)
            {
              int shift = (bytelen - (ssize - bytepos)) * 8;
              expand_binop (mode, (optab_table[OTI_ashr]), tmps[i], gen_rtx_CONST_INT (VOIDmode, (long long) (shift)),
                            tmps[i], 0, OPTAB_WIDEN);
            }
          bytelen = ssize - bytepos;
        }

      if (((enum rtx_code) (dst)->code) == CONCAT)
        {
          if (bytepos + bytelen <= (mode_size[(int) (((enum machine_mode) ((((dst)->fld[0]).rtx))->mode))]))
            dest = (((dst)->fld[0]).rtx);
          else if (bytepos >= (mode_size[(int) (((enum machine_mode) ((((dst)->fld[0]).rtx))->mode))]))
            {
              bytepos -= (mode_size[(int) (((enum machine_mode) ((((dst)->fld[0]).rtx))->mode))]);
              dest = (((dst)->fld[1]).rtx);
            }
          else
            fancy_abort ("expr.c", 2142, __FUNCTION__);
        }


      if (((enum rtx_code) (dest)->code) == MEM
          && ((((dest)->fld[1]).rtmem) != 0 ? (((dest)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (dest)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (dest)->mode)) : 8)) >= get_mode_alignment (mode)
          && bytepos * 8 % get_mode_alignment (mode) == 0
          && bytelen == (mode_size[(int) (mode)]))
        emit_move_insn (adjust_address_1 (dest, mode, bytepos, 1, 1), tmps[i]);
      else
        store_bit_field (dest, bytelen * 8, bytepos * 8,
                         mode, tmps[i], ssize);
    }

  emit_queue ();


  if (((enum rtx_code) (dst)->code) == REG)
    emit_move_insn (orig_dst, dst);
}
# 2172 "expr.c"
rtx
copy_blkmode_from_reg (tgtblk, srcreg, type)
     rtx tgtblk;
     rtx srcreg;
     tree type;
{
  unsigned long long bytes = int_size_in_bytes (type);
  rtx src = ((void *)0), dst = ((void *)0);
  unsigned long long bitsize = ((((type)->type.align)) < (((target_flags & 0x02000000) ? 64 : 32)) ? (((type)->type.align)) : (((target_flags & 0x02000000) ? 64 : 32)));
  unsigned long long bitpos, xbitpos, big_endian_correction = 0;

  if (tgtblk == 0)
    {
      tgtblk = assign_temp (build_qualified_type (type,
                                                  (((((type)->common.readonly_flag) * 0x1) | (((type)->common.volatile_flag) * 0x2) | (((type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (type)->common.code) == RECORD_TYPE && ((type)->common.type)) * 0x8))
                                                   | 0x1)),
                            0, 1, 1);
      preserve_temp_slots (tgtblk);
    }







  if (((enum machine_mode) (srcreg)->mode) != BLKmode
      && (mode_size[(int) (((enum machine_mode) (srcreg)->mode))]) < ((target_flags & 0x02000000) ? 8 : 4))
    {
      if (0)
        srcreg = simplify_gen_subreg (word_mode, srcreg, ((enum machine_mode) (srcreg)->mode), 0);
      else
        srcreg = convert_to_mode (word_mode, srcreg, ((type)->common.unsigned_flag));
    }





  if (0
      && !0
      && bytes % ((target_flags & 0x02000000) ? 8 : 4))
    big_endian_correction
      = (((target_flags & 0x02000000) ? 64 : 32) - ((bytes % ((target_flags & 0x02000000) ? 8 : 4)) * 8));






  for (bitpos = 0, xbitpos = big_endian_correction;
       bitpos < bytes * 8;
       bitpos += bitsize, xbitpos += bitsize)
    {



      if (xbitpos % ((target_flags & 0x02000000) ? 64 : 32) == 0
          || xbitpos == big_endian_correction)
        src = operand_subword_force (srcreg, xbitpos / ((target_flags & 0x02000000) ? 64 : 32),
                                     ((enum machine_mode) (srcreg)->mode));



      if (bitpos % ((target_flags & 0x02000000) ? 64 : 32) == 0)
        dst = operand_subword (tgtblk, bitpos / ((target_flags & 0x02000000) ? 64 : 32), 1, BLKmode);



      store_bit_field (dst, bitsize, bitpos % ((target_flags & 0x02000000) ? 64 : 32), word_mode,
                       extract_bit_field (src, bitsize,
                                          xbitpos % ((target_flags & 0x02000000) ? 64 : 32), 1,
                                          (rtx) 0, word_mode, word_mode,
                                          ((target_flags & 0x02000000) ? 64 : 32)),
                       ((target_flags & 0x02000000) ? 64 : 32));
    }

  return tgtblk;
}




void
use_reg (call_fusage, reg)
     rtx *call_fusage, reg;
{
  if (((enum rtx_code) (reg)->code) != REG
      || (((reg)->fld[0]).rtuint) >= 53)
    fancy_abort ("expr.c", 2261, __FUNCTION__);

  *call_fusage
    = gen_rtx_fmt_ee (EXPR_LIST, (VOIDmode), (gen_rtx_fmt_e (USE, (VOIDmode), (reg))), (*call_fusage));

}




void
use_regs (call_fusage, regno, nregs)
     rtx *call_fusage;
     int regno;
     int nregs;
{
  int i;

  if (regno + nregs > 53)
    fancy_abort ("expr.c", 2280, __FUNCTION__);

  for (i = 0; i < nregs; i++)
    use_reg (call_fusage, gen_rtx_REG (reg_raw_mode[regno + i], regno + i));
}





void
use_group_regs (call_fusage, regs)
     rtx *call_fusage;
     rtx regs;
{
  int i;

  for (i = 0; i < (((((regs)->fld[0]).rtvec))->num_elem); i++)
    {
      rtx reg = ((((((((regs)->fld[0]).rtvec))->elem[i]))->fld[0]).rtx);




      if (reg != 0 && ((enum rtx_code) (reg)->code) == REG)
        use_reg (call_fusage, reg);
    }
}


int
can_store_by_pieces (len, constfun, constfundata, align)
     unsigned long long len;
     rtx (*constfun) (void *, long long, enum machine_mode);
     void * constfundata;
     unsigned int align;
{
  unsigned long long max_size, l;
  long long offset = 0;
  enum machine_mode mode, tmode;
  enum insn_code icode;
  int reverse;
  rtx cst;

  if (! (move_by_pieces_ninsns (len, align) < (unsigned int) (optimize_size ? 3 : ix86_cost->move_ratio)))
    return 0;

  if (! 0
      || align > 16 * 8 || align >= 128)
    align = 16 * 8;




  for (reverse = 0;
       reverse <= (0 || 0);
       reverse++)
    {
      l = len;
      mode = VOIDmode;
      max_size = ((target_flags & 0x02000000) ? 8 : 4) + 1;
      while (max_size > 1)
        {
          for (tmode = class_narrowest_mode[(int) (MODE_INT)];
               tmode != VOIDmode; tmode = ((enum machine_mode)mode_wider_mode[(int) (tmode)]))
            if ((mode_size[(int) (tmode)]) < max_size)
              mode = tmode;

          if (mode == VOIDmode)
            break;

          icode = (optab_table[OTI_mov])->handlers[(int) mode].insn_code;
          if (icode != CODE_FOR_nothing
              && align >= get_mode_alignment (mode))
            {
              unsigned int size = (mode_size[(int) (mode)]);

              while (l >= size)
                {
                  if (reverse)
                    offset -= size;

                  cst = (*constfun) (constfundata, offset, mode);
                  if (!1)
                    return 0;

                  if (!reverse)
                    offset += size;

                  l -= size;
                }
            }

          max_size = (mode_size[(int) (mode)]);
        }


      if (l != 0)
        fancy_abort ("expr.c", 2378, __FUNCTION__);
    }

  return 1;
}






void
store_by_pieces (to, len, constfun, constfundata, align)
     rtx to;
     unsigned long long len;
     rtx (*constfun) (void *, long long, enum machine_mode);
     void * constfundata;
     unsigned int align;
{
  struct store_by_pieces data;

  if (! (move_by_pieces_ninsns (len, align) < (unsigned int) (optimize_size ? 3 : ix86_cost->move_ratio)))
    fancy_abort ("expr.c", 2400, __FUNCTION__);
  to = protect_from_queue (to, 1);
  data.constfun = constfun;
  data.constfundata = constfundata;
  data.len = len;
  data.to = to;
  store_by_pieces_1 (&data, align);
}





static void
clear_by_pieces (to, len, align)
     rtx to;
     unsigned long long len;
     unsigned int align;
{
  struct store_by_pieces data;

  data.constfun = clear_by_pieces_1;
  data.constfundata = ((void *)0);
  data.len = len;
  data.to = to;
  store_by_pieces_1 (&data, align);
}




static rtx
clear_by_pieces_1 (data, offset, mode)
     void * data __attribute__ ((__unused__));
     long long offset __attribute__ ((__unused__));
     enum machine_mode mode __attribute__ ((__unused__));
{
  return (const_int_rtx[64]);
}






static void
store_by_pieces_1 (data, align)
     struct store_by_pieces *data;
     unsigned int align;
{
  rtx to_addr = (((data->to)->fld[0]).rtx);
  unsigned long long max_size = ((target_flags & 0x02000000) ? 8 : 4) + 1;
  enum machine_mode mode = VOIDmode, tmode;
  enum insn_code icode;

  data->offset = 0;
  data->to_addr = to_addr;
  data->autinc_to
    = (((enum rtx_code) (to_addr)->code) == PRE_INC || ((enum rtx_code) (to_addr)->code) == PRE_DEC
       || ((enum rtx_code) (to_addr)->code) == POST_INC || ((enum rtx_code) (to_addr)->code) == POST_DEC);

  data->explicit_inc_to = 0;
  data->reverse
    = (((enum rtx_code) (to_addr)->code) == PRE_DEC || ((enum rtx_code) (to_addr)->code) == POST_DEC);
  if (data->reverse)
    data->offset = data->len;




  if (!data->autinc_to
      && move_by_pieces_ninsns (data->len, align) > 2)
    {

      for (tmode = class_narrowest_mode[(int) (MODE_INT)];
           tmode != VOIDmode; tmode = ((enum machine_mode)mode_wider_mode[(int) (tmode)]))
        if ((mode_size[(int) (tmode)]) < max_size)
          mode = tmode;

      if (0 && data->reverse && ! data->autinc_to)
        {
          data->to_addr = copy_addr_to_reg (plus_constant_wide ((to_addr), (long long) (data->len)));
          data->autinc_to = 1;
          data->explicit_inc_to = -1;
        }

      if (0 && ! data->reverse
          && ! data->autinc_to)
        {
          data->to_addr = copy_addr_to_reg (to_addr);
          data->autinc_to = 1;
          data->explicit_inc_to = 1;
        }

      if ( !data->autinc_to && (((enum rtx_code) (to_addr)->code) == LABEL_REF || ((enum rtx_code) (to_addr)->code) == SYMBOL_REF || ((enum rtx_code) (to_addr)->code) == CONST_INT || ((enum rtx_code) (to_addr)->code) == CONST_DOUBLE || ((enum rtx_code) (to_addr)->code) == CONST || ((enum rtx_code) (to_addr)->code) == HIGH || ((enum rtx_code) (to_addr)->code) == CONST_VECTOR || ((enum rtx_code) (to_addr)->code) == CONSTANT_P_RTX))
        data->to_addr = copy_addr_to_reg (to_addr);
    }

  if (! 0
      || align > 16 * 8 || align >= 128)
    align = 16 * 8;




  while (max_size > 1)
    {
      for (tmode = class_narrowest_mode[(int) (MODE_INT)];
           tmode != VOIDmode; tmode = ((enum machine_mode)mode_wider_mode[(int) (tmode)]))
        if ((mode_size[(int) (tmode)]) < max_size)
          mode = tmode;

      if (mode == VOIDmode)
        break;

      icode = (optab_table[OTI_mov])->handlers[(int) mode].insn_code;
      if (icode != CODE_FOR_nothing && align >= get_mode_alignment (mode))
        store_by_pieces_2 ((*insn_data[(int) (icode)].genfun), mode, data);

      max_size = (mode_size[(int) (mode)]);
    }


  if (data->len != 0)
    fancy_abort ("expr.c", 2524, __FUNCTION__);
}





static void
store_by_pieces_2 (genfun, mode, data)
     rtx (*genfun) (rtx, ...);
     enum machine_mode mode;
     struct store_by_pieces *data;
{
  unsigned int size = (mode_size[(int) (mode)]);
  rtx to1, cst;

  while (data->len >= size)
    {
      if (data->reverse)
        data->offset -= size;

      if (data->autinc_to)
        to1 = adjust_automodify_address_1 (data->to, mode, data->to_addr, data->offset, 1);

      else
        to1 = adjust_address_1 (data->to, mode, data->offset, 1, 1);

      if (0 && data->explicit_inc_to < 0)
        emit_insn (gen_add2_insn (data->to_addr,
                                  gen_rtx_CONST_INT (VOIDmode, (long long) (-(long long) size))));

      cst = (*data->constfun) (data->constfundata, data->offset, mode);
      emit_insn ((*genfun) (to1, cst));

      if (0 && data->explicit_inc_to > 0)
        emit_insn (gen_add2_insn (data->to_addr, gen_rtx_CONST_INT (VOIDmode, (long long) (size))));

      if (! data->reverse)
        data->offset += size;

      data->len -= size;
    }
}




rtx
clear_storage (object, size)
     rtx object;
     rtx size;
{

  static tree fn;
  tree call_expr, arg_list;

  rtx retval = 0;
  unsigned int align = (((enum rtx_code) (object)->code) == MEM ? ((((object)->fld[1]).rtmem) != 0 ? (((object)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (object)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (object)->mode)) : 8))
                        : get_mode_alignment (((enum machine_mode) (object)->mode)));



  if (((enum machine_mode) (object)->mode) != BLKmode
      && ((enum rtx_code) (size)->code) == CONST_INT
      && (mode_size[(int) (((enum machine_mode) (object)->mode))]) == (unsigned int) (((size)->fld[0]).rtwint))
    emit_move_insn (object, (const_tiny_rtx[0][(int) (((enum machine_mode) (object)->mode))]));
  else
    {
      object = protect_from_queue (object, 1);
      size = protect_from_queue (size, 0);

      if (((enum rtx_code) (size)->code) == CONST_INT
          && (move_by_pieces_ninsns ((((size)->fld[0]).rtwint), align) < (unsigned int) (optimize_size ? 3 : ix86_cost->move_ratio)))
        clear_by_pieces (object, (((size)->fld[0]).rtwint), align);
      else
        {




          rtx opalign = gen_rtx_CONST_INT (VOIDmode, (long long) (align / 8));
          enum machine_mode mode;

          for (mode = class_narrowest_mode[(int) (MODE_INT)]; mode != VOIDmode;
               mode = ((enum machine_mode)mode_wider_mode[(int) (mode)]))
            {
              enum insn_code code = clrstr_optab[(int) mode];
              insn_operand_predicate_fn pred;

              if (code != CODE_FOR_nothing




                  && ((((enum rtx_code) (size)->code) == CONST_INT
                       && ((unsigned long long) (((size)->fld[0]).rtwint)
                           <= (mode_mask_array[(int) (mode)] >> 1)))
                      || (mode_bitsize[(int) (mode)]) >= ((target_flags & 0x02000000) ? 64 : 32))
                  && ((pred = insn_data[(int) code].operand[0].predicate) == 0
                      || (*pred) (object, BLKmode))
                  && ((pred = insn_data[(int) code].operand[2].predicate) == 0
                      || (*pred) (opalign, VOIDmode)))
                {
                  rtx op1;
                  rtx last = get_last_insn ();
                  rtx pat;

                  op1 = convert_to_mode (mode, size, 1);
                  pred = insn_data[(int) code].operand[1].predicate;
                  if (pred != 0 && ! (*pred) (op1, mode))
                    op1 = copy_to_mode_reg (mode, op1);

                  pat = (*insn_data[(int) ((int) code)].genfun) (object, op1, opalign);
                  if (pat)
                    {
                      emit_insn (pat);
                      return 0;
                    }
                  else
                    delete_insns_since (last);
                }
            }
# 2667 "expr.c"
          object = copy_to_mode_reg (((target_flags & 0x02000000) ? DImode : SImode), (((object)->fld[0]).rtx));


          size = copy_to_mode_reg (((sizetype_tab[(int) SIZETYPE])->type.mode), size);
# 2690 "expr.c"
          if (fn == (tree) ((void *)0))
            {
              tree fntype;



              fn = get_identifier ("memset");
              fntype = build_pointer_type (global_trees[TI_VOID_TYPE]);
              fntype = build_function_type (fntype, (tree) ((void *)0));
              fn = build_decl (FUNCTION_DECL, fn, fntype);
              ggc_add_tree_root (&fn, 1);
              ((fn)->decl.external_flag) = 1;
              ((fn)->common.public_flag) = 1;
              ((fn)->decl.artificial_flag) = 1;
              ((fn)->common.nothrow_flag) = 1;
              make_decl_rtl (fn, ((void *)0));
              assemble_external (fn);
            }






          arg_list
            = build_tree_list ((tree) ((void *)0),
                               make_tree (build_pointer_type (global_trees[TI_VOID_TYPE]),
                                          object));
          ((arg_list)->common.chain)
            = build_tree_list ((tree) ((void *)0),
                               make_tree (integer_types[itk_int], (const_int_rtx[64])));
          ((((arg_list)->common.chain))->common.chain)
            = build_tree_list ((tree) ((void *)0), make_tree (sizetype_tab[(int) SIZETYPE], size));
          ((((((arg_list)->common.chain))->common.chain))->common.chain) = (tree) ((void *)0);


          call_expr = build1 (ADDR_EXPR,
                              build_pointer_type (((fn)->common.type)), fn);
          call_expr = build (CALL_EXPR, ((((fn)->common.type))->common.type),
                             call_expr, arg_list, (tree) ((void *)0));
          ((call_expr)->common.side_effects_flag) = 1;

          retval = expand_expr (call_expr, (rtx) 0, VOIDmode, 0);
# 2742 "expr.c"
          if (((object)->unchanging))
            emit_insn (gen_rtx_fmt_e (CLOBBER, (VOIDmode), (object)));
        }
    }

  return retval;
}
# 2757 "expr.c"
rtx
emit_move_insn (x, y)
     rtx x, y;
{
  enum machine_mode mode = ((enum machine_mode) (x)->mode);
  rtx y_cst = (rtx) 0;
  rtx last_insn;

  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);

  if (mode == BLKmode || (((enum machine_mode) (y)->mode) != mode && ((enum machine_mode) (y)->mode) != VOIDmode))
    fancy_abort ("expr.c", 2769, __FUNCTION__);


  if (((enum rtx_code) (y)->code) == CONSTANT_P_RTX)
    ;
  else if ((((enum rtx_code) (y)->code) == LABEL_REF || ((enum rtx_code) (y)->code) == SYMBOL_REF || ((enum rtx_code) (y)->code) == CONST_INT || ((enum rtx_code) (y)->code) == CONST_DOUBLE || ((enum rtx_code) (y)->code) == CONST || ((enum rtx_code) (y)->code) == HIGH || ((enum rtx_code) (y)->code) == CONST_VECTOR || ((enum rtx_code) (y)->code) == CONSTANT_P_RTX) && ! 1)
    {
      y_cst = y;
      y = force_const_mem (mode, y);
    }



  if (((enum rtx_code) (x)->code) == MEM
      && ((! memory_address_p (((enum machine_mode) (x)->mode), (((x)->fld[0]).rtx))
           && ! push_operand (x, ((enum machine_mode) (x)->mode)))
          || (flag_force_addr
              && (((enum rtx_code) ((((x)->fld[0]).rtx))->code) == LABEL_REF || ((enum rtx_code) ((((x)->fld[0]).rtx))->code) == SYMBOL_REF || ((enum rtx_code) ((((x)->fld[0]).rtx))->code) == CONST_INT || ((enum rtx_code) ((((x)->fld[0]).rtx))->code) == CONST || ((enum rtx_code) ((((x)->fld[0]).rtx))->code) == CONST_DOUBLE))))
    x = validize_mem (x);

  if (((enum rtx_code) (y)->code) == MEM
      && (! memory_address_p (((enum machine_mode) (y)->mode), (((y)->fld[0]).rtx))
          || (flag_force_addr
              && (((enum rtx_code) ((((y)->fld[0]).rtx))->code) == LABEL_REF || ((enum rtx_code) ((((y)->fld[0]).rtx))->code) == SYMBOL_REF || ((enum rtx_code) ((((y)->fld[0]).rtx))->code) == CONST_INT || ((enum rtx_code) ((((y)->fld[0]).rtx))->code) == CONST || ((enum rtx_code) ((((y)->fld[0]).rtx))->code) == CONST_DOUBLE))))
    y = validize_mem (y);

  if (mode == BLKmode)
    fancy_abort ("expr.c", 2796, __FUNCTION__);

  last_insn = emit_move_insn_1 (x, y);

  if (y_cst && ((enum rtx_code) (x)->code) == REG)
    set_unique_reg_note (last_insn, REG_EQUAL, y_cst);

  return last_insn;
}





rtx
emit_move_insn_1 (x, y)
     rtx x, y;
{
  enum machine_mode mode = ((enum machine_mode) (x)->mode);
  enum machine_mode submode;
  enum mode_class class = (mode_class[(int) (mode)]);

  if ((unsigned int) mode >= (unsigned int) MAX_MACHINE_MODE)
    fancy_abort ("expr.c", 2819, __FUNCTION__);

  if ((optab_table[OTI_mov])->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    return
      emit_insn ((*insn_data[(int) ((optab_table[OTI_mov])->handlers[(int) mode].insn_code)].genfun) (x, y));


  else if ((class == MODE_COMPLEX_FLOAT || class == MODE_COMPLEX_INT)
           && BLKmode != (submode = mode_for_size (((mode_unit_size[(int) (mode)])
                                                    * 8),
                                                   (class == MODE_COMPLEX_INT
                                                    ? MODE_INT : MODE_FLOAT),
                                                   0))
           && ((optab_table[OTI_mov])->handlers[(int) submode].insn_code
               != CODE_FOR_nothing))
    {

      int stack = push_operand (x, ((enum machine_mode) (x)->mode));




      if (stack
          && (((target_flags & 0x02000000) ? ((((mode_size[(int) (submode)])) + 7) & (-8)) : ((((mode_size[(int) (submode)])) + 1) & (-2)))
              != (mode_size[(int) (submode)])))
        {
          rtx temp;
          long long offset1, offset2;



          temp = expand_binop (((target_flags & 0x02000000) ? DImode : SImode),

                               (optab_table[OTI_sub]),



                               (global_rtl[GR_STACK_POINTER]),
                               gen_rtx_CONST_INT (VOIDmode, (long long) (((target_flags & 0x02000000) ? ((((mode_size[(int) (((enum machine_mode) (x)->mode))])) + 7) & (-8)) : ((((mode_size[(int) (((enum machine_mode) (x)->mode))])) + 1) & (-2))))),


                               (global_rtl[GR_STACK_POINTER]), 0, OPTAB_LIB_WIDEN);

          if (temp != (global_rtl[GR_STACK_POINTER]))
            emit_move_insn ((global_rtl[GR_STACK_POINTER]), temp);


          offset1 = 0;
          offset2 = (mode_size[(int) (submode)]);






          emit_move_insn (change_address (x, submode,
                                          gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_STACK_POINTER])), (gen_rtx_CONST_INT (VOIDmode, (long long) (offset1))))),


                          gen_realpart (submode, y));
          emit_move_insn (change_address (x, submode,
                                          gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_STACK_POINTER])), (gen_rtx_CONST_INT (VOIDmode, (long long) (offset2))))),


                          gen_imagpart (submode, y));
        }
      else






      if (stack)
        {



          emit_insn ((*insn_data[(int) ((optab_table[OTI_mov])->handlers[(int) submode].insn_code)].genfun)
                     (gen_rtx_MEM (submode, (((x)->fld[0]).rtx)),
                      gen_imagpart (submode, y)));
          emit_insn ((*insn_data[(int) ((optab_table[OTI_mov])->handlers[(int) submode].insn_code)].genfun)
                     (gen_rtx_MEM (submode, (((x)->fld[0]).rtx)),
                      gen_realpart (submode, y)));
# 2911 "expr.c"
        }
      else
        {
          rtx realpart_x, realpart_y;
          rtx imagpart_x, imagpart_y;
# 2926 "expr.c"
          if ((mode_bitsize[(int) (mode)]) < 2 * ((target_flags & 0x02000000) ? 64 : 32)
              && (reload_in_progress | reload_completed) == 0)
            {
              int packed_dest_p
                = ((((enum rtx_code) (x)->code) == REG) && (((x)->fld[0]).rtuint) < 53);
              int packed_src_p
                = ((((enum rtx_code) (y)->code) == REG) && (((y)->fld[0]).rtuint) < 53);

              if (packed_dest_p || packed_src_p)
                {
                  enum mode_class reg_class = ((class == MODE_COMPLEX_FLOAT)
                                               ? MODE_FLOAT : MODE_INT);

                  enum machine_mode reg_mode
                    = mode_for_size ((mode_bitsize[(int) (mode)]), reg_class, 1);

                  if (reg_mode != BLKmode)
                    {
                      rtx mem = assign_stack_temp (reg_mode,
                                                   (mode_size[(int) (mode)]), 0);
                      rtx cmem = adjust_address_1 (mem, mode, 0, 1, 1);

                      cfun->cannot_inline
                        = ("function using short complex types cannot be inline");

                      if (packed_dest_p)
                        {
                          rtx sreg = gen_rtx_SUBREG (reg_mode, x, 0);

                          emit_move_insn_1 (cmem, y);
                          return emit_move_insn_1 (sreg, mem);
                        }
                      else
                        {
                          rtx sreg = gen_rtx_SUBREG (reg_mode, y, 0);

                          emit_move_insn_1 (mem, sreg);
                          return emit_move_insn_1 (x, cmem);
                        }
                    }
                }
            }

          realpart_x = gen_realpart (submode, x);
          realpart_y = gen_realpart (submode, y);
          imagpart_x = gen_imagpart (submode, x);
          imagpart_y = gen_imagpart (submode, y);





          if (x != y
              && ! (reload_in_progress || reload_completed)
              && (((enum rtx_code) (realpart_x)->code) == SUBREG
                  || ((enum rtx_code) (imagpart_x)->code) == SUBREG))
            emit_insn (gen_rtx_fmt_e (CLOBBER, (VOIDmode), (x)));

          emit_insn ((*insn_data[(int) ((optab_table[OTI_mov])->handlers[(int) submode].insn_code)].genfun)
                     (realpart_x, realpart_y));
          emit_insn ((*insn_data[(int) ((optab_table[OTI_mov])->handlers[(int) submode].insn_code)].genfun)
                     (imagpart_x, imagpart_y));
        }

      return get_last_insn ();
    }




  else if ((mode_size[(int) (mode)]) > ((target_flags & 0x02000000) ? 8 : 4))
    {
      rtx last_insn = 0;
      rtx seq, inner;
      int need_clobber;
      int i;





      if (push_operand (x, ((enum machine_mode) (x)->mode)))
        {
          rtx temp;
          enum rtx_code code;



          temp = expand_binop (((target_flags & 0x02000000) ? DImode : SImode),

                               (optab_table[OTI_sub]),



                               (global_rtl[GR_STACK_POINTER]),
                               gen_rtx_CONST_INT (VOIDmode, (long long) (((target_flags & 0x02000000) ? ((((mode_size[(int) (((enum machine_mode) (x)->mode))])) + 7) & (-8)) : ((((mode_size[(int) (((enum machine_mode) (x)->mode))])) + 1) & (-2))))),


                               (global_rtl[GR_STACK_POINTER]), 0, OPTAB_LIB_WIDEN);

          if (temp != (global_rtl[GR_STACK_POINTER]))
            emit_move_insn ((global_rtl[GR_STACK_POINTER]), temp);

          code = ((enum rtx_code) ((((x)->fld[0]).rtx))->code);


          if (code == POST_INC)
            temp = gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_STACK_POINTER])), (gen_rtx_CONST_INT (VOIDmode, (long long) (-((long long) (mode_size[(int) (((enum machine_mode) (x)->mode))]))))));


          else if (code == POST_DEC)
            temp = gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_STACK_POINTER])), (gen_rtx_CONST_INT (VOIDmode, (long long) ((mode_size[(int) (((enum machine_mode) (x)->mode))])))));

          else
            temp = (global_rtl[GR_STACK_POINTER]);

          x = change_address (x, VOIDmode, temp);
        }




      if (reload_in_progress && ((enum rtx_code) (x)->code) == MEM
          && (inner = find_replacement (&(((x)->fld[0]).rtx))) != (((x)->fld[0]).rtx))
        x = replace_equiv_address_nv (x, inner);
      if (reload_in_progress && ((enum rtx_code) (y)->code) == MEM
          && (inner = find_replacement (&(((y)->fld[0]).rtx))) != (((y)->fld[0]).rtx))
        y = replace_equiv_address_nv (y, inner);

      start_sequence ();

      need_clobber = 0;
      for (i = 0;
           i < ((mode_size[(int) (mode)]) + (((target_flags & 0x02000000) ? 8 : 4) - 1)) / ((target_flags & 0x02000000) ? 8 : 4);
           i++)
        {
          rtx xpart = operand_subword (x, i, 1, mode);
          rtx ypart = operand_subword (y, i, 1, mode);




          if (ypart == 0 && (((enum rtx_code) (y)->code) == LABEL_REF || ((enum rtx_code) (y)->code) == SYMBOL_REF || ((enum rtx_code) (y)->code) == CONST_INT || ((enum rtx_code) (y)->code) == CONST_DOUBLE || ((enum rtx_code) (y)->code) == CONST || ((enum rtx_code) (y)->code) == HIGH || ((enum rtx_code) (y)->code) == CONST_VECTOR || ((enum rtx_code) (y)->code) == CONSTANT_P_RTX))
            {
              y = force_const_mem (mode, y);
              ypart = operand_subword (y, i, 1, mode);
            }
          else if (ypart == 0)
            ypart = operand_subword_force (y, i, mode);

          if (xpart == 0 || ypart == 0)
            fancy_abort ("expr.c", 3077, __FUNCTION__);

          need_clobber |= (((enum rtx_code) (xpart)->code) == SUBREG);

          last_insn = emit_move_insn (xpart, ypart);
        }

      seq = gen_sequence ();
      end_sequence ();





      if (x != y
          && ! (reload_in_progress || reload_completed)
          && need_clobber != 0)
        emit_insn (gen_rtx_fmt_e (CLOBBER, (VOIDmode), (x)));

      emit_insn (seq);

      return last_insn;
    }
  else
    fancy_abort ("expr.c", 3101, __FUNCTION__);
}
# 3115 "expr.c"
rtx
push_block (size, extra, below)
     rtx size;
     int extra, below;
{
  rtx temp;

  size = convert_modes (((target_flags & 0x02000000) ? DImode : SImode), ptr_mode, size, 1);
  if ((((enum rtx_code) (size)->code) == LABEL_REF || ((enum rtx_code) (size)->code) == SYMBOL_REF || ((enum rtx_code) (size)->code) == CONST_INT || ((enum rtx_code) (size)->code) == CONST_DOUBLE || ((enum rtx_code) (size)->code) == CONST || ((enum rtx_code) (size)->code) == HIGH || ((enum rtx_code) (size)->code) == CONST_VECTOR || ((enum rtx_code) (size)->code) == CONSTANT_P_RTX))
    anti_adjust_stack (plus_constant_wide ((size), (long long) (extra)));
  else if (((enum rtx_code) (size)->code) == REG && extra == 0)
    anti_adjust_stack (size);
  else
    {
      temp = copy_to_mode_reg (((target_flags & 0x02000000) ? DImode : SImode), size);
      if (extra != 0)
        temp = expand_binop (((target_flags & 0x02000000) ? DImode : SImode), (optab_table[OTI_add]), temp, gen_rtx_CONST_INT (VOIDmode, (long long) (extra)),
                             temp, 0, OPTAB_LIB_WIDEN);
      anti_adjust_stack (temp);
    }




  if (1)

    {
      temp = (global_rtl[GR_VIRTUAL_OUTGOING_ARGS]);
      if (extra != 0 && below)
        temp = plus_constant_wide ((temp), (long long) (extra));
    }
  else
    {
      if (((enum rtx_code) (size)->code) == CONST_INT)
        temp = plus_constant_wide (((global_rtl[GR_VIRTUAL_OUTGOING_ARGS])), (long long) (-(((size)->fld[0]).rtwint) - (below ? 0 : extra)));

      else if (extra != 0 && !below)
        temp = gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_VIRTUAL_OUTGOING_ARGS])), (negate_rtx (((target_flags & 0x02000000) ? DImode : SImode), plus_constant_wide ((size), (long long) (extra)))));

      else
        temp = gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_VIRTUAL_OUTGOING_ARGS])), (negate_rtx (((target_flags & 0x02000000) ? DImode : SImode), size)));

    }

  return memory_address (class_narrowest_mode[(int) (MODE_INT)], temp);
}





static void
emit_single_push_insn (mode, x, type)
     rtx x;
     enum machine_mode mode;
     tree type;
{
  rtx dest_addr;
  unsigned rounded_size = ((target_flags & 0x02000000) ? ((((mode_size[(int) (mode)])) + 7) & (-8)) : ((((mode_size[(int) (mode)])) + 1) & (-2)));
  rtx dest;
  enum insn_code icode;
  insn_operand_predicate_fn pred;

  (cfun->expr->x_stack_pointer_delta) += ((target_flags & 0x02000000) ? ((((mode_size[(int) (mode)])) + 7) & (-8)) : ((((mode_size[(int) (mode)])) + 1) & (-2)));


  icode = (optab_table[OTI_push])->handlers[(int) mode].insn_code;
  if (icode != CODE_FOR_nothing)
    {
      if (((pred = insn_data[(int) icode].operand[0].predicate)
           && !((*pred) (x, mode))))
        x = force_reg (mode, x);
      emit_insn ((*insn_data[(int) (icode)].genfun) (x));
      return;
    }
  if ((mode_size[(int) (mode)]) == rounded_size)
    dest_addr = gen_rtx_fmt_e (PRE_DEC, ((target_flags & 0x02000000) ? DImode : SImode), (global_rtl[GR_STACK_POINTER]));
  else
    {

      dest_addr = gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_STACK_POINTER])), (gen_rtx_CONST_INT (VOIDmode, (long long) (-(long long) rounded_size))));





      dest_addr = gen_rtx_fmt_ee (PRE_MODIFY, (((target_flags & 0x02000000) ? DImode : SImode)), ((global_rtl[GR_STACK_POINTER])), (dest_addr));
    }

  dest = gen_rtx_MEM (mode, dest_addr);

  if (type != 0)
    {
      set_mem_attributes (dest, type, 1);

      if (flag_optimize_sibling_calls)




        set_mem_alias_set (dest, 0);
    }
  emit_move_insn (dest, x);
}
# 3253 "expr.c"
void
emit_push_insn (x, mode, type, size, align, partial, reg, extra,
                args_addr, args_so_far, reg_parm_stack_space,
                alignment_pad)
     rtx x;
     enum machine_mode mode;
     tree type;
     rtx size;
     unsigned int align;
     int partial;
     rtx reg;
     int extra;
     rtx args_addr;
     rtx args_so_far;
     int reg_parm_stack_space;
     rtx alignment_pad;
{
  rtx xinner;
  enum direction stack_direction

    = downward;







  enum direction where_pad = (! 0 ? upward : (((mode) == BLKmode ? ((type) && ((enum tree_code) (((type)->type.size))->common.code) == INTEGER_CST && int_size_in_bytes (type) < (((target_flags & 0x02000000) ? 64 : 32) / 8)) : (mode_bitsize[(int) (mode)]) < ((target_flags & 0x02000000) ? 64 : 32)) ? downward : upward));



  if (PRE_DEC == POST_DEC)
    if (where_pad != none)
      where_pad = (where_pad == downward ? upward : downward);

  xinner = x = protect_from_queue (x, 0);

  if (mode == BLKmode)
    {


      rtx temp;
      int used = partial * ((target_flags & 0x02000000) ? 8 : 4);
      int offset = used % (((target_flags & 0x02000000) ? 64 : 32) / 8);
      int skip;

      if (size == 0)
        fancy_abort ("expr.c", 3301, __FUNCTION__);

      used -= offset;




      if (partial != 0)
        xinner = adjust_address_1 (xinner, BLKmode, used, 1, 1);





      skip = (reg_parm_stack_space == 0) ? 0 : used;





      if (args_addr == 0
          && ((!(target_flags & 0x00000800)) && !(target_flags & 0x00001000))
          && ((enum rtx_code) (size)->code) == CONST_INT
          && skip == 0
          && ((move_by_pieces_ninsns ((unsigned) (((size)->fld[0]).rtwint) - used, align) < (unsigned int) (optimize_size ? 3 : ix86_cost->move_ratio)))



          && ((! 0)
              || align >= 128
              || (((target_flags & 0x02000000) ? (((align / 8) + 7) & (-8)) : (((align / 8) + 1) & (-2)))
                  == (align / 8)))
          && ((target_flags & 0x02000000) ? ((((((size)->fld[0]).rtwint)) + 7) & (-8)) : ((((((size)->fld[0]).rtwint)) + 1) & (-2))) == (((size)->fld[0]).rtwint))
        {



          if (extra && args_addr == 0
              && where_pad != none && where_pad != stack_direction)
            anti_adjust_stack (gen_rtx_CONST_INT (VOIDmode, (long long) (extra)));

          move_by_pieces (((void *)0), xinner, (((size)->fld[0]).rtwint) - used, align);
        }
      else

        {
          rtx target;





          if (partial != 0)
            {
              if (((enum rtx_code) (size)->code) == CONST_INT)
                size = gen_rtx_CONST_INT (VOIDmode, (long long) ((((size)->fld[0]).rtwint) - used));
              else
                size = expand_binop (((enum machine_mode) (size)->mode), (optab_table[OTI_sub]), size,
                                     gen_rtx_CONST_INT (VOIDmode, (long long) (used)), (rtx) 0, 0,
                                     OPTAB_LIB_WIDEN);
            }




          if (! args_addr)
            {
              temp = push_block (size, extra, where_pad == downward);
              extra = 0;
            }
          else if (((enum rtx_code) (args_so_far)->code) == CONST_INT)
            temp = memory_address (BLKmode,
                                   plus_constant_wide ((args_addr), (long long) (skip + (((args_so_far)->fld[0]).rtwint))));

          else
            temp = memory_address (BLKmode,
                                   plus_constant_wide ((gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), (args_addr), (args_so_far))), (long long) (skip)));



          target = gen_rtx_MEM (BLKmode, temp);

          if (type != 0)
            {
              set_mem_attributes (target, type, 1);




              set_mem_alias_set (target, 0);
            }
          else
            set_mem_align (target, align);


          if (((enum rtx_code) (size)->code) == CONST_INT
              && (move_by_pieces_ninsns ((unsigned) (((size)->fld[0]).rtwint), align) < (unsigned int) (optimize_size ? 3 : ix86_cost->move_ratio)))
            {
              move_by_pieces (target, xinner, (((size)->fld[0]).rtwint), align);
              goto ret;
            }
          else
            {
              rtx opalign = gen_rtx_CONST_INT (VOIDmode, (long long) (align / 8));
              enum machine_mode mode;

              for (mode = class_narrowest_mode[(int) (MODE_INT)];
                   mode != VOIDmode;
                   mode = ((enum machine_mode)mode_wider_mode[(int) (mode)]))
                {
                  enum insn_code code = movstr_optab[(int) mode];
                  insn_operand_predicate_fn pred;

                  if (code != CODE_FOR_nothing
                      && ((((enum rtx_code) (size)->code) == CONST_INT
                           && ((unsigned long long) (((size)->fld[0]).rtwint)
                               <= (mode_mask_array[(int) (mode)] >> 1)))
                          || (mode_bitsize[(int) (mode)]) >= ((target_flags & 0x02000000) ? 64 : 32))
                      && (!(pred = insn_data[(int) code].operand[0].predicate)
                          || ((*pred) (target, BLKmode)))
                      && (!(pred = insn_data[(int) code].operand[1].predicate)
                          || ((*pred) (xinner, BLKmode)))
                      && (!(pred = insn_data[(int) code].operand[3].predicate)
                          || ((*pred) (opalign, VOIDmode))))
                    {
                      rtx op2 = convert_to_mode (mode, size, 1);
                      rtx last = get_last_insn ();
                      rtx pat;

                      pred = insn_data[(int) code].operand[2].predicate;
                      if (pred != 0 && ! (*pred) (op2, mode))
                        op2 = copy_to_mode_reg (mode, op2);

                      pat = (*insn_data[(int) ((int) code)].genfun) (target, xinner,
                                                  op2, opalign);
                      if (pat)
                        {
                          emit_insn (pat);
                          goto ret;
                        }
                      else
                        delete_insns_since (last);
                    }
                }
            }

          if (!(target_flags & 0x00001000))
            {




              if (reg_mentioned_p ((global_rtl[GR_VIRTUAL_STACK_DYNAMIC]), temp)
                  || reg_mentioned_p ((global_rtl[GR_VIRTUAL_OUTGOING_ARGS]), temp))
                temp = copy_to_reg (temp);
            }



          ((cfun->expr->x_inhibit_defer_pop) += 1);

          emit_library_call ((libfunc_table[LTI_memcpy]), LCT_NORMAL,
                             VOIDmode, 3, temp, ((target_flags & 0x02000000) ? DImode : SImode), (((xinner)->fld[0]).rtx), ((target_flags & 0x02000000) ? DImode : SImode),
                             convert_to_mode (((sizetype_tab[(int) SIZETYPE])->type.mode),
                                              size, ((sizetype_tab[(int) SIZETYPE])->common.unsigned_flag)),
                             ((sizetype_tab[(int) SIZETYPE])->type.mode));
# 3475 "expr.c"
          ((cfun->expr->x_inhibit_defer_pop) -= 1);
        }
    }
  else if (partial > 0)
    {


      int size = (mode_size[(int) (mode)]) / ((target_flags & 0x02000000) ? 8 : 4);
      int i;
      int not_stack;


      int offset = partial % (((target_flags & 0x02000000) ? 64 : 32) / ((target_flags & 0x02000000) ? 64 : 32));
      int args_offset = (((args_so_far)->fld[0]).rtwint);
      int skip;




      if (extra && args_addr == 0
          && where_pad != none && where_pad != stack_direction)
        anti_adjust_stack (gen_rtx_CONST_INT (VOIDmode, (long long) (extra)));




      if (args_addr == 0)
        offset = 0;



      not_stack = partial - offset;





      skip = (reg_parm_stack_space == 0) ? 0 : not_stack;

      if ((((enum rtx_code) (x)->code) == LABEL_REF || ((enum rtx_code) (x)->code) == SYMBOL_REF || ((enum rtx_code) (x)->code) == CONST_INT || ((enum rtx_code) (x)->code) == CONST_DOUBLE || ((enum rtx_code) (x)->code) == CONST || ((enum rtx_code) (x)->code) == HIGH || ((enum rtx_code) (x)->code) == CONST_VECTOR || ((enum rtx_code) (x)->code) == CONSTANT_P_RTX) && ! 1)
        x = validize_mem (force_const_mem (mode, x));



      if ((((enum rtx_code) (x)->code) == REG && (((x)->fld[0]).rtuint) < 53
           && (mode_class[(int) (((enum machine_mode) (x)->mode))]) != MODE_INT))
        x = copy_to_reg (x);







      for (i = size - 1; i >= not_stack; i--)

        if (i >= not_stack + offset)
          emit_push_insn (operand_subword_force (x, i, mode),
                          word_mode, (tree) ((void *)0), (rtx) 0, align, 0, (rtx) 0,
                          0, args_addr,
                          gen_rtx_CONST_INT (VOIDmode, (long long) (args_offset + ((i - not_stack + skip) * ((target_flags & 0x02000000) ? 8 : 4)))),

                          reg_parm_stack_space, alignment_pad);
    }
  else
    {
      rtx addr;
      rtx target = (rtx) 0;
      rtx dest;




      if (extra && args_addr == 0
          && where_pad != none && where_pad != stack_direction)
        anti_adjust_stack (gen_rtx_CONST_INT (VOIDmode, (long long) (extra)));


      if (args_addr == 0 && ((!(target_flags & 0x00000800)) && !(target_flags & 0x00001000)))
        emit_single_push_insn (mode, x, type);
      else

        {
          if (((enum rtx_code) (args_so_far)->code) == CONST_INT)
            addr
              = memory_address (mode,
                                plus_constant_wide ((args_addr), (long long) ((((args_so_far)->fld[0]).rtwint))));

          else
            addr = memory_address (mode, gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), (args_addr), (args_so_far)));

          target = addr;
          dest = gen_rtx_MEM (mode, addr);
          if (type != 0)
            {
              set_mem_attributes (dest, type, 1);




              set_mem_alias_set (dest, 0);
            }

          emit_move_insn (dest, x);
        }

    }

 ret:



  if (partial > 0 && reg != 0)
    {


      if (((enum rtx_code) (reg)->code) == PARALLEL)
        emit_group_load (reg, x, -1);
      else
        move_block_to_reg ((((reg)->fld[0]).rtuint), x, partial, mode);
    }

  if (extra && args_addr == 0 && where_pad == stack_direction)
    anti_adjust_stack (gen_rtx_CONST_INT (VOIDmode, (long long) (extra)));

  if (alignment_pad && args_addr == 0)
    anti_adjust_stack (alignment_pad);
}




static rtx
get_subtarget (x)
     rtx x;
{
  return ((x == 0

           || ((enum rtx_code) (x)->code) != REG

           || ((x)->unchanging)

           || (((x)->fld[0]).rtuint) < 53


           || preserve_subexpressions_p ())
          ? 0 : x);
}
# 3635 "expr.c"
rtx
expand_assignment (to, from, want_value, suggest_reg)
     tree to, from;
     int want_value;
     int suggest_reg __attribute__ ((__unused__));
{
  rtx to_rtx = 0;
  rtx result;



  if (((enum tree_code) (to)->common.code) == ERROR_MARK)
    {
      result = expand_expr (from, (rtx) 0, VOIDmode, 0);
      return want_value ? result : (rtx) 0;
    }







  if (((enum tree_code) (to)->common.code) == COMPONENT_REF || ((enum tree_code) (to)->common.code) == BIT_FIELD_REF
      || ((enum tree_code) (to)->common.code) == ARRAY_REF || ((enum tree_code) (to)->common.code) == ARRAY_RANGE_REF)
    {
      enum machine_mode mode1;
      long long bitsize, bitpos;
      rtx orig_to_rtx;
      tree offset;
      int unsignedp;
      int volatilep = 0;
      tree tem;

      push_temp_slots ();
      tem = get_inner_reference (to, &bitsize, &bitpos, &offset, &mode1,
                                 &unsignedp, &volatilep);




      if (mode1 == VOIDmode && want_value)
        tem = stabilize_reference (tem);

      orig_to_rtx = to_rtx = expand_expr (tem, (rtx) 0, VOIDmode, 0);

      if (offset != 0)
        {
          rtx offset_rtx = expand_expr (offset, (rtx) 0, VOIDmode, EXPAND_SUM);

          if (((enum rtx_code) (to_rtx)->code) != MEM)
            fancy_abort ("expr.c", 3686, __FUNCTION__);





          if (((enum machine_mode) (offset_rtx)->mode) != ptr_mode)
            offset_rtx = convert_to_mode (ptr_mode, offset_rtx, 0);




          if (((enum rtx_code) (to_rtx)->code) == MEM
              && ((enum machine_mode) (to_rtx)->mode) == BLKmode
              && ((enum machine_mode) ((((to_rtx)->fld[0]).rtx))->mode) != VOIDmode
              && bitsize > 0
              && (bitpos % bitsize) == 0
              && (bitsize % get_mode_alignment (mode1)) == 0
              && ((((to_rtx)->fld[1]).rtmem) != 0 ? (((to_rtx)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (to_rtx)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (to_rtx)->mode)) : 8)) == get_mode_alignment (mode1))
            {
              to_rtx = adjust_address_1 (to_rtx, mode1, bitpos / 8, 1, 1);
              bitpos = 0;
            }

          to_rtx = offset_address (to_rtx, offset_rtx,
                                   highest_pow2_factor_for_type (((to)->common.type),
                                                                 offset));
        }

      if (((enum rtx_code) (to_rtx)->code) == MEM)
        {
          tree old_expr = ((((to_rtx)->fld[1]).rtmem) == 0 ? 0 : (((to_rtx)->fld[1]).rtmem)->expr);



          to_rtx = shallow_copy_rtx (to_rtx);

          set_mem_attributes (to_rtx, to, 0);






          if (((((to_rtx)->fld[1]).rtmem) == 0 ? 0 : (((to_rtx)->fld[1]).rtmem)->expr) != old_expr && ((((to_rtx)->fld[1]).rtmem) == 0 ? 0 : (((to_rtx)->fld[1]).rtmem)->offset)
              && (bitpos / 8) != 0)
            set_mem_offset (to_rtx, gen_rtx_CONST_INT (VOIDmode, (long long) ((((((((to_rtx)->fld[1]).rtmem) == 0 ? 0 : (((to_rtx)->fld[1]).rtmem)->offset))->fld[0]).rtwint) - (bitpos / 8))));

        }



      if (volatilep && ((enum rtx_code) (to_rtx)->code) == MEM)
        {
          if (to_rtx == orig_to_rtx)
            to_rtx = copy_rtx (to_rtx);
          ((to_rtx)->volatil) = 1;
        }

      if (((enum tree_code) (to)->common.code) == COMPONENT_REF
          && ((((to)->exp.operands[1]))->common.readonly_flag))
        {
          if (to_rtx == orig_to_rtx)
            to_rtx = copy_rtx (to_rtx);
          ((to_rtx)->unchanging) = 1;
        }

      if (((enum rtx_code) (to_rtx)->code) == MEM && ! can_address_p (to))
        {
          if (to_rtx == orig_to_rtx)
            to_rtx = copy_rtx (to_rtx);
          ((to_rtx)->jump) = 1;
        }

      result = store_field (to_rtx, bitsize, bitpos, mode1, from,
                            (want_value

                             ? ((enum machine_mode)
                                ((((to)->common.type))->type.mode))
                             : VOIDmode),
                            unsignedp, ((tem)->common.type), get_alias_set (to));

      preserve_temp_slots (result);
      free_temp_slots ();
      pop_temp_slots ();



      return (want_value ? convert_modes (((((to)->common.type))->type.mode),
                                          ((((from)->common.type))->type.mode),
                                          result,
                                          ((((to)->common.type))->common.unsigned_flag))
              : (rtx) 0);
    }
# 3791 "expr.c"
  if (((enum tree_code) (from)->common.code) == CALL_EXPR && ! aggregate_value_p (from)
      && ((enum tree_code) (((((from)->common.type))->type.size))->common.code) == INTEGER_CST
      && ! ((((enum tree_code) (to)->common.code) == VAR_DECL || ((enum tree_code) (to)->common.code) == PARM_DECL)
            && ((enum rtx_code) (((to)->decl.rtl ? (to)->decl.rtl : (make_decl_rtl (to, ((void *)0)), (to)->decl.rtl)))->code) == REG))
    {
      rtx value;

      push_temp_slots ();
      value = expand_expr (from, (rtx) 0, VOIDmode, 0);
      if (to_rtx == 0)
        to_rtx = expand_expr (to, (rtx) 0, VOIDmode, EXPAND_WRITE);



      if (((enum rtx_code) (to_rtx)->code) == PARALLEL)
        emit_group_load (to_rtx, value, int_size_in_bytes (((from)->common.type)));
      else if (((enum machine_mode) (to_rtx)->mode) == BLKmode)
        emit_block_move (to_rtx, value, expr_size (from));
      else
        {





          emit_move_insn (to_rtx, value);
        }
      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return want_value ? to_rtx : (rtx) 0;
    }




  if (to_rtx == 0)
    to_rtx = expand_expr (to, (rtx) 0, VOIDmode, EXPAND_WRITE);


  if (((enum tree_code) (to)->common.code) == RESULT_DECL
      && (((enum rtx_code) (to_rtx)->code) == REG || ((enum rtx_code) (to_rtx)->code) == PARALLEL))
    {
      rtx temp;

      push_temp_slots ();
      temp = expand_expr (from, 0, ((enum machine_mode) (to_rtx)->mode), 0);

      if (((enum rtx_code) (to_rtx)->code) == PARALLEL)
        emit_group_load (to_rtx, temp, int_size_in_bytes (((from)->common.type)));
      else
        emit_move_insn (to_rtx, temp);

      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return want_value ? to_rtx : (rtx) 0;
    }




  if (((enum tree_code) (to)->common.code) == RESULT_DECL && ((enum tree_code) (from)->common.code) == INDIRECT_REF
      && (cfun->returns_struct)
      && !(cfun->returns_pcc_struct))
    {
      rtx from_rtx, size;

      push_temp_slots ();
      size = expr_size (from);
      from_rtx = expand_expr (from, (rtx) 0, VOIDmode, 0);


      emit_library_call ((libfunc_table[LTI_memmove]), LCT_NORMAL,
                         VOIDmode, 3, (((to_rtx)->fld[0]).rtx), ((target_flags & 0x02000000) ? DImode : SImode),
                         (((from_rtx)->fld[0]).rtx), ((target_flags & 0x02000000) ? DImode : SImode),
                         convert_to_mode (((sizetype_tab[(int) SIZETYPE])->type.mode),
                                          size, ((sizetype_tab[(int) SIZETYPE])->common.unsigned_flag)),
                         ((sizetype_tab[(int) SIZETYPE])->type.mode));
# 3879 "expr.c"
      preserve_temp_slots (to_rtx);
      free_temp_slots ();
      pop_temp_slots ();
      return want_value ? to_rtx : (rtx) 0;
    }



  push_temp_slots ();
  result = store_expr (from, to_rtx, want_value);
  preserve_temp_slots (result);
  free_temp_slots ();
  pop_temp_slots ();
  return want_value ? result : (rtx) 0;
}
# 3918 "expr.c"
rtx
store_expr (exp, target, want_value)
     tree exp;
     rtx target;
     int want_value;
{
  rtx temp;
  int dont_return_target = 0;
  int dont_store_target = 0;

  if (((enum tree_code) (exp)->common.code) == COMPOUND_EXPR)
    {


      expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode, 0);
      emit_queue ();
      return store_expr (((exp)->exp.operands[1]), target, want_value);
    }
  else if (((enum tree_code) (exp)->common.code) == COND_EXPR && ((enum machine_mode) (target)->mode) == BLKmode)
    {





      rtx lab1 = gen_label_rtx (), lab2 = gen_label_rtx ();

      emit_queue ();
      target = protect_from_queue (target, 1);

      do_pending_stack_adjust ();
      ((cfun->expr->x_inhibit_defer_pop) += 1);
      jumpifnot (((exp)->exp.operands[0]), lab1);
      start_cleanup_deferral ();
      store_expr (((exp)->exp.operands[1]), target, 0);
      end_cleanup_deferral ();
      emit_queue ();
      emit_jump_insn (gen_jump (lab2));
      emit_barrier ();
      emit_label (lab1);
      start_cleanup_deferral ();
      store_expr (((exp)->exp.operands[2]), target, 0);
      end_cleanup_deferral ();
      emit_queue ();
      emit_label (lab2);
      ((cfun->expr->x_inhibit_defer_pop) -= 1);

      return want_value ? target : (rtx) 0;
    }
  else if (queued_subexp_p (target))


    {
      if (((enum machine_mode) (target)->mode) != BLKmode && ((enum machine_mode) (target)->mode) != VOIDmode)
        {

          temp = gen_reg_rtx (((enum machine_mode) (target)->mode));
          temp = expand_expr (exp, temp, ((enum machine_mode) (target)->mode), 0);
        }
      else
        temp = expand_expr (exp, (rtx) 0, ((enum machine_mode) (target)->mode), 0);




      if (! ((target)->volatil) && want_value)
        dont_return_target = 1;
    }
  else if (want_value && ((enum rtx_code) (target)->code) == MEM && ! ((target)->volatil)
           && ((enum machine_mode) (target)->mode) != BLKmode)






    {
      temp = expand_expr (exp, target, ((enum machine_mode) (target)->mode), 0);
      if (((enum machine_mode) (temp)->mode) != BLKmode && ((enum machine_mode) (temp)->mode) != VOIDmode)
        {


          if (temp == target
              || (rtx_equal_p (temp, target)
                  && ! side_effects_p (temp) && ! side_effects_p (target)))
            dont_store_target = 1;
          temp = copy_to_reg (temp);
        }
      dont_return_target = 1;
    }
  else if (((enum rtx_code) (target)->code) == SUBREG && ((target)->in_struct))




    {
      rtx inner_target = 0;







      if (! want_value && (((enum tree_code) (((exp)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((exp)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((exp)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((exp)->common.type))->common.code) == CHAR_TYPE)
          && ((((exp)->common.type))->common.type) == 0)
        {
          if (((((exp)->common.type))->common.unsigned_flag)
              != ((target)->unchanging))
            exp
              = convert
                (signed_or_unsigned_type (((target)->unchanging),
                                          ((exp)->common.type)),
                 exp);

          exp = convert (type_for_mode (((enum machine_mode) ((((target)->fld[0]).rtx))->mode),
                                        ((target)->unchanging)),
                         exp);

          inner_target = (((target)->fld[0]).rtx);
        }

      temp = expand_expr (exp, inner_target, VOIDmode, 0);




      if (((enum rtx_code) (temp)->code) == MEM && want_value
          && (((temp)->volatil)
              || reg_mentioned_p ((((target)->fld[0]).rtx), (((temp)->fld[0]).rtx))))
        temp = copy_to_reg (temp);



      if ((((enum rtx_code) (temp)->code) == LABEL_REF || ((enum rtx_code) (temp)->code) == SYMBOL_REF || ((enum rtx_code) (temp)->code) == CONST_INT || ((enum rtx_code) (temp)->code) == CONST_DOUBLE || ((enum rtx_code) (temp)->code) == CONST || ((enum rtx_code) (temp)->code) == HIGH || ((enum rtx_code) (temp)->code) == CONST_VECTOR || ((enum rtx_code) (temp)->code) == CONSTANT_P_RTX) && ((enum machine_mode) (temp)->mode) == VOIDmode)
        {
          temp = convert_modes (((enum machine_mode) (target)->mode), ((((exp)->common.type))->type.mode),
                                temp, ((target)->unchanging));
          temp = convert_modes (((enum machine_mode) ((((target)->fld[0]).rtx))->mode),
                                ((enum machine_mode) (target)->mode), temp,
                                ((target)->unchanging));
        }

      convert_move ((((target)->fld[0]).rtx), temp,
                    ((target)->unchanging));





      if (want_value && ((enum machine_mode) (temp)->mode) != ((enum machine_mode) (target)->mode))
        {
          if (((enum machine_mode) (temp)->mode) != VOIDmode)
            {
              temp = gen_lowpart_SUBREG (((enum machine_mode) (target)->mode), temp);
              ((temp)->in_struct) = 1;
              ((temp)->unchanging)
                = ((target)->unchanging);
            }
          else
            temp = convert_modes (((enum machine_mode) (target)->mode),
                                  ((enum machine_mode) ((((target)->fld[0]).rtx))->mode),
                                  temp, ((target)->unchanging));
        }

      return want_value ? temp : (rtx) 0;
    }
  else
    {
      temp = expand_expr (exp, target, ((enum machine_mode) (target)->mode), 0);







      if (!(target && ((enum rtx_code) (target)->code) == REG
            && (((target)->fld[0]).rtuint) < 53)
          && !(((enum rtx_code) (target)->code) == MEM && ((target)->volatil))
          && ! rtx_equal_p (temp, target)
          && ((((enum rtx_code) (temp)->code) == LABEL_REF || ((enum rtx_code) (temp)->code) == SYMBOL_REF || ((enum rtx_code) (temp)->code) == CONST_INT || ((enum rtx_code) (temp)->code) == CONST_DOUBLE || ((enum rtx_code) (temp)->code) == CONST || ((enum rtx_code) (temp)->code) == HIGH || ((enum rtx_code) (temp)->code) == CONST_VECTOR || ((enum rtx_code) (temp)->code) == CONSTANT_P_RTX) || want_value))
        dont_return_target = 1;
    }





  if ((((enum rtx_code) (temp)->code) == LABEL_REF || ((enum rtx_code) (temp)->code) == SYMBOL_REF || ((enum rtx_code) (temp)->code) == CONST_INT || ((enum rtx_code) (temp)->code) == CONST_DOUBLE || ((enum rtx_code) (temp)->code) == CONST || ((enum rtx_code) (temp)->code) == HIGH || ((enum rtx_code) (temp)->code) == CONST_VECTOR || ((enum rtx_code) (temp)->code) == CONSTANT_P_RTX) && ((enum machine_mode) (temp)->mode) == VOIDmode
      && ((enum tree_code) (exp)->common.code) != ERROR_MARK
      && ((enum machine_mode) (target)->mode) != ((((exp)->common.type))->type.mode))
    temp = convert_modes (((enum machine_mode) (target)->mode), ((((exp)->common.type))->type.mode),
                          temp, ((((exp)->common.type))->common.unsigned_flag));
# 4127 "expr.c"
  if ((! rtx_equal_p (temp, target)
       || (temp != target && (side_effects_p (temp)
                              || side_effects_p (target))))
      && ((enum tree_code) (exp)->common.code) != ERROR_MARK
      && ! dont_store_target



      && (tree_code_type[(int) (((enum tree_code) (exp)->common.code))] != 'd'
          || target != (((exp)->decl.rtl != ((void *)0)) ? ((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)) : ((void *)0))))
    {
      target = protect_from_queue (target, 1);
      if (((enum machine_mode) (temp)->mode) != ((enum machine_mode) (target)->mode)
          && ((enum machine_mode) (temp)->mode) != VOIDmode)
        {
          int unsignedp = ((((exp)->common.type))->common.unsigned_flag);
          if (dont_return_target)
            {



              temp = convert_to_mode (((enum machine_mode) (target)->mode), temp, unsignedp);
              emit_move_insn (target, temp);
            }
          else
            convert_move (target, temp, unsignedp);
        }

      else if (((enum machine_mode) (temp)->mode) == BLKmode && ((enum tree_code) (exp)->common.code) == STRING_CST)
        {




          rtx size = expr_size (exp);

          if (((enum rtx_code) (size)->code) == CONST_INT
              && (((size)->fld[0]).rtwint) < ((exp)->string.length))
            emit_block_move (target, temp, size);
          else
            {

              tree copy_size
                = size_binop (MIN_EXPR,
                              make_tree (sizetype_tab[(int) SIZETYPE], size),
                              size_int_wide ((long long) (((exp)->string.length)), SIZETYPE));
              rtx copy_size_rtx = expand_expr (copy_size, (rtx) 0,
                                               VOIDmode, 0);
              rtx label = 0;


              copy_size_rtx = convert_to_mode (ptr_mode, copy_size_rtx, 0);
              emit_block_move (target, temp, copy_size_rtx);



              if (((enum rtx_code) (copy_size_rtx)->code) == CONST_INT)
                {
                  size = plus_constant_wide ((size), (long long) (-(((copy_size_rtx)->fld[0]).rtwint)));
                  target = adjust_address_1 (target, BLKmode, (((copy_size_rtx)->fld[0]).rtwint), 1, 1);

                }
              else
                {
                  size = expand_binop (((sizetype_tab[(int) SIZETYPE])->type.mode), (optab_table[OTI_sub]), size,
                                       copy_size_rtx, (rtx) 0, 0,
                                       OPTAB_LIB_WIDEN);







                  target = offset_address (target, copy_size_rtx,
                                           highest_pow2_factor (copy_size));
                  label = gen_label_rtx ();
                  emit_cmp_and_jump_insns (size, (const_int_rtx[64]), LT, (rtx) 0,
                                           ((enum machine_mode) (size)->mode), 0, label);
                }

              if (size != (const_int_rtx[64]))
                clear_storage (target, size);

              if (label)
                emit_label (label);
            }
        }


      else if (((enum rtx_code) (target)->code) == PARALLEL)
        emit_group_load (target, temp, int_size_in_bytes (((exp)->common.type)));
      else if (((enum machine_mode) (temp)->mode) == BLKmode)
        emit_block_move (target, temp, expr_size (exp));
      else
        emit_move_insn (target, temp);
    }


  if (! want_value)
    return (rtx) 0;



  else if (dont_return_target && ((enum rtx_code) (temp)->code) != MEM)
    return temp;


  else if (want_value && ((enum machine_mode) (target)->mode) != BLKmode
           && ! (((enum rtx_code) (target)->code) == REG
                 && (((target)->fld[0]).rtuint) < 53))
    return copy_to_reg (target);

  else
    return target;
}



static int
is_zeros_p (exp)
     tree exp;
{
  tree elt;

  switch (((enum tree_code) (exp)->common.code))
    {
    case CONVERT_EXPR:
    case NOP_EXPR:
    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      return is_zeros_p (((exp)->exp.operands[0]));

    case INTEGER_CST:
      return integer_zerop (exp);

    case COMPLEX_CST:
      return
        is_zeros_p (((exp)->complex.real)) && is_zeros_p (((exp)->complex.imag));

    case REAL_CST:
      return (!memcmp ((char *) &(((exp)->real_cst.real_cst)), (char *) &(dconst0), sizeof (realvaluetype)));

    case VECTOR_CST:
      for (elt = ((exp)->vector.elements); elt;
           elt = ((elt)->common.chain))
        if (!is_zeros_p (((elt)->list.value)))
          return 0;

      return 1;

    case CONSTRUCTOR:
      if (((exp)->common.type) && ((enum tree_code) (((exp)->common.type))->common.code) == SET_TYPE)
        return (((exp))->exp.operands[1]) == (tree) ((void *)0);
      for (elt = (((exp))->exp.operands[1]); elt; elt = ((elt)->common.chain))
        if (! is_zeros_p (((elt)->list.value)))
          return 0;

      return 1;

    default:
      return 0;
    }
}



static int
mostly_zeros_p (exp)
     tree exp;
{
  if (((enum tree_code) (exp)->common.code) == CONSTRUCTOR)
    {
      int elts = 0, zeros = 0;
      tree elt = (((exp))->exp.operands[1]);
      if (((exp)->common.type) && ((enum tree_code) (((exp)->common.type))->common.code) == SET_TYPE)
        {

          return elt == (tree) ((void *)0);
        }
      for (; elt; elt = ((elt)->common.chain))
        {





          if (mostly_zeros_p (((elt)->list.value)))
            zeros++;
          elts++;
        }

      return 4 * zeros >= 3 * elts;
    }

  return is_zeros_p (exp);
}
# 4336 "expr.c"
static void
store_constructor_field (target, bitsize, bitpos, mode, exp, type, cleared,
                         alias_set)
     rtx target;
     unsigned long long bitsize;
     long long bitpos;
     enum machine_mode mode;
     tree exp, type;
     int cleared;
     int alias_set;
{
  if (((enum tree_code) (exp)->common.code) == CONSTRUCTOR
      && bitpos % 8 == 0



      && (bitpos == 0 || ((enum rtx_code) (target)->code) == MEM))
    {
      if (((enum rtx_code) (target)->code) == MEM)
        target
          = adjust_address_1 (target, ((enum machine_mode) (target)->mode) == BLKmode || 0 != (bitpos % get_mode_alignment (((enum machine_mode) (target)->mode))) ? BLKmode : VOIDmode, bitpos / 8, 1, 1);







      if (((enum rtx_code) (target)->code) == MEM && ! ((target)->jump)
          && ((((target)->fld[1]).rtmem) == 0 ? 0 : (((target)->fld[1]).rtmem)->alias) != 0)
        {
          target = copy_rtx (target);
          set_mem_alias_set (target, alias_set);
        }

      store_constructor (exp, target, cleared, bitsize / 8);
    }
  else
    store_field (target, bitsize, bitpos, mode, exp, VOIDmode, 0, type,
                 alias_set);
}
# 4386 "expr.c"
static void
store_constructor (exp, target, cleared, size)
     tree exp;
     rtx target;
     int cleared;
     long long size;
{
  tree type = ((exp)->common.type);




  if (((enum tree_code) (type)->common.code) == RECORD_TYPE || ((enum tree_code) (type)->common.code) == UNION_TYPE
      || ((enum tree_code) (type)->common.code) == QUAL_UNION_TYPE)
    {
      tree elt;


      if ((((enum tree_code) (type)->common.code) == UNION_TYPE
           || ((enum tree_code) (type)->common.code) == QUAL_UNION_TYPE)
          && ! cleared
          && ! (((exp))->exp.operands[1]))

        {
          clear_storage (target, expr_size (exp));
          cleared = 1;
        }





      else if (! cleared && ((enum rtx_code) (target)->code) == REG && ((exp)->common.static_flag)
               && (mode_size[(int) (((enum machine_mode) (target)->mode))]) <= ((target_flags & 0x02000000) ? 8 : 4))
        {
          emit_move_insn (target, (const_tiny_rtx[0][(int) (((enum machine_mode) (target)->mode))]));
          cleared = 1;
        }






      else if (! cleared && size > 0
               && ((list_length ((((exp))->exp.operands[1]))
                    != fields_length (type))
                   || mostly_zeros_p (exp))
               && (((enum rtx_code) (target)->code) != REG
                   || ((long long) (mode_size[(int) (((enum machine_mode) (target)->mode))])
                       == size)))
        {
          clear_storage (target, gen_rtx_CONST_INT (VOIDmode, (long long) (size)));
          cleared = 1;
        }

      if (! cleared)
        emit_insn (gen_rtx_fmt_e (CLOBBER, (VOIDmode), (target)));




      for (elt = (((exp))->exp.operands[1]); elt; elt = ((elt)->common.chain))
        {
          tree field = ((elt)->list.purpose);
          tree value = ((elt)->list.value);
          enum machine_mode mode;
          long long bitsize;
          long long bitpos = 0;
          int unsignedp;
          tree offset;
          rtx to_rtx = target;




          if (field == 0)
            continue;

          if (cleared && is_zeros_p (value))
            continue;

          if (host_integerp (((field)->decl.size), 1))
            bitsize = tree_low_cst (((field)->decl.size), 1);
          else
            bitsize = -1;

          unsignedp = ((field)->common.unsigned_flag);
          mode = ((field)->decl.mode);
          if (((field)->decl.bit_field_flag))
            mode = VOIDmode;

          offset = ((field)->decl.arguments);
          if (host_integerp (offset, 0)
              && host_integerp (bit_position (field), 0))
            {
              bitpos = int_bit_position (field);
              offset = 0;
            }
          else
            bitpos = tree_low_cst (((field)->decl.u2.t), 0);

          if (offset)
            {
              rtx offset_rtx;

              if (contains_placeholder_p (offset))
                offset = build (WITH_RECORD_EXPR, sizetype_tab[(int) SIZETYPE],
                                offset, make_tree (((exp)->common.type), target));

              offset_rtx = expand_expr (offset, (rtx) 0, VOIDmode, 0);
              if (((enum rtx_code) (to_rtx)->code) != MEM)
                fancy_abort ("expr.c", 4498, __FUNCTION__);





              if (((enum machine_mode) (offset_rtx)->mode) != ptr_mode)
                offset_rtx = convert_to_mode (ptr_mode, offset_rtx, 0);


              to_rtx = offset_address (to_rtx, offset_rtx,
                                       highest_pow2_factor (offset));
            }

          if (((field)->common.readonly_flag))
            {
              if (((enum rtx_code) (to_rtx)->code) == MEM)
                to_rtx = copy_rtx (to_rtx);

              ((to_rtx)->unchanging) = 1;
            }
# 4550 "expr.c"
          if (((enum rtx_code) (to_rtx)->code) == MEM && !((to_rtx)->jump)
              && ((field)->decl.non_addressable))
            {
              to_rtx = copy_rtx (to_rtx);
              ((to_rtx)->jump) = 1;
            }

          store_constructor_field (to_rtx, bitsize, bitpos, mode,
                                   value, type, cleared,
                                   get_alias_set (((field)->common.type)));
        }
    }
  else if (((enum tree_code) (type)->common.code) == ARRAY_TYPE
           || ((enum tree_code) (type)->common.code) == VECTOR_TYPE)
    {
      tree elt;
      int i;
      int need_to_clear;
      tree domain = ((type)->type.values);
      tree elttype = ((type)->common.type);
      int const_bounds_p;
      long long minelt = 0;
      long long maxelt = 0;



      if (((enum tree_code) (type)->common.code) == VECTOR_TYPE)
        {



          domain = ((type)->type.values);
          domain = ((((((domain)->type.values))->common.type))->type.values);
        }

      const_bounds_p = (((domain)->type.minval)
                        && ((domain)->type.maxval)
                        && host_integerp (((domain)->type.minval), 0)
                        && host_integerp (((domain)->type.maxval), 0));


      if (const_bounds_p)
        {
          minelt = tree_low_cst (((domain)->type.minval), 0);
          maxelt = tree_low_cst (((domain)->type.maxval), 0);
        }




      if (cleared || (((enum rtx_code) (target)->code) == REG && ((exp)->common.static_flag)))
        need_to_clear = 1;
      else
        {
          long long count = 0, zero_count = 0;
          need_to_clear = ! const_bounds_p;




          for (elt = (((exp))->exp.operands[1]);
               elt != (tree) ((void *)0) && ! need_to_clear;
               elt = ((elt)->common.chain))
            {
              tree index = ((elt)->list.purpose);
              long long this_node_count;

              if (index != (tree) ((void *)0) && ((enum tree_code) (index)->common.code) == RANGE_EXPR)
                {
                  tree lo_index = ((index)->exp.operands[0]);
                  tree hi_index = ((index)->exp.operands[1]);

                  if (! host_integerp (lo_index, 1)
                      || ! host_integerp (hi_index, 1))
                    {
                      need_to_clear = 1;
                      break;
                    }

                  this_node_count = (tree_low_cst (hi_index, 1)
                                     - tree_low_cst (lo_index, 1) + 1);
                }
              else
                this_node_count = 1;

              count += this_node_count;
              if (mostly_zeros_p (((elt)->list.value)))
                zero_count += this_node_count;
            }



          if (! need_to_clear
              && (count < maxelt - minelt + 1 || 4 * zero_count >= 3 * count))
            need_to_clear = 1;
        }

      if (need_to_clear && size > 0)
        {
          if (! cleared)
            {
              if ((((enum rtx_code) (target)->code) == REG))
                emit_move_insn (target, (const_tiny_rtx[0][(int) (((enum machine_mode) (target)->mode))]));
              else
                clear_storage (target, gen_rtx_CONST_INT (VOIDmode, (long long) (size)));
            }
          cleared = 1;
        }
      else if ((((enum rtx_code) (target)->code) == REG))

        emit_insn (gen_rtx_fmt_e (CLOBBER, (VOIDmode), (target)));




      for (elt = (((exp))->exp.operands[1]), i = 0;
           elt;
           elt = ((elt)->common.chain), i++)
        {
          enum machine_mode mode;
          long long bitsize;
          long long bitpos;
          int unsignedp;
          tree value = ((elt)->list.value);
          tree index = ((elt)->list.purpose);
          rtx xtarget = target;

          if (cleared && is_zeros_p (value))
            continue;

          unsignedp = ((elttype)->common.unsigned_flag);
          mode = ((elttype)->type.mode);
          if (mode == BLKmode)
            bitsize = (host_integerp (((elttype)->type.size), 1)
                       ? tree_low_cst (((elttype)->type.size), 1)
                       : -1);
          else
            bitsize = (mode_bitsize[(int) (mode)]);

          if (index != (tree) ((void *)0) && ((enum tree_code) (index)->common.code) == RANGE_EXPR)
            {
              tree lo_index = ((index)->exp.operands[0]);
              tree hi_index = ((index)->exp.operands[1]);
              rtx index_r, pos_rtx, hi_r, loop_top, loop_end;
              struct nesting *loop;
              long long lo, hi, count;
              tree position;


              if (const_bounds_p
                  && host_integerp (lo_index, 0)
                  && host_integerp (hi_index, 0)
                  && (lo = tree_low_cst (lo_index, 0),
                      hi = tree_low_cst (hi_index, 0),
                      count = hi - lo + 1,
                      (((enum rtx_code) (target)->code) != MEM
                       || count <= 2
                       || (host_integerp (((elttype)->type.size), 1)
                           && (tree_low_cst (((elttype)->type.size), 1) * count
                               <= 40 * 8)))))
                {
                  lo -= minelt; hi -= minelt;
                  for (; lo <= hi; lo++)
                    {
                      bitpos = lo * tree_low_cst (((elttype)->type.size), 0);

                      if (((enum rtx_code) (target)->code) == MEM
                          && !((target)->jump)
                          && ((enum tree_code) (type)->common.code) == ARRAY_TYPE
                          && ((type)->type.transparent_union_flag))
                        {
                          target = copy_rtx (target);
                          ((target)->jump) = 1;
                        }

                      store_constructor_field
                        (target, bitsize, bitpos, mode, value, type, cleared,
                         get_alias_set (elttype));
                    }
                }
              else
                {
                  hi_r = expand_expr (hi_index, (rtx) 0, VOIDmode, 0);
                  loop_top = gen_label_rtx ();
                  loop_end = gen_label_rtx ();

                  unsignedp = ((domain)->common.unsigned_flag);

                  index = build_decl (VAR_DECL, (tree) ((void *)0), domain);

                  index_r
                    = gen_reg_rtx (promote_mode (domain, ((index)->decl.mode),
                                                 &unsignedp, 0));
                  ((index)->decl.rtl = (index_r));
                  if (((enum tree_code) (value)->common.code) == SAVE_EXPR
                      && (*(rtx *) &(value)->exp.operands[2]) == 0)
                    {


                      expand_expr (value, (const_int_rtx[64]), VOIDmode, 0);
                      emit_queue ();
                    }
                  store_expr (lo_index, index_r, 0);
                  loop = expand_start_loop (0);


                  position
                    = convert (sizetype_tab[(int) SSIZETYPE],
                               fold (build (MINUS_EXPR, ((index)->common.type),
                                            index, ((domain)->type.minval))));
                  position = size_binop (MULT_EXPR, position,
                                         convert (sizetype_tab[(int) SSIZETYPE],
                                                  ((elttype)->type.size_unit)));

                  pos_rtx = expand_expr (position, 0, VOIDmode, 0);
                  xtarget = offset_address (target, pos_rtx,
                                            highest_pow2_factor (position));
                  xtarget = adjust_address_1 (xtarget, mode, 0, 1, 1);
                  if (((enum tree_code) (value)->common.code) == CONSTRUCTOR)
                    store_constructor (value, xtarget, cleared,
                                       bitsize / 8);
                  else
                    store_expr (value, xtarget, 0);

                  expand_exit_loop_if_false (loop,
                                             build (LT_EXPR, integer_types[itk_int],
                                                    index, hi_index));

                  expand_increment (build (PREINCREMENT_EXPR,
                                           ((index)->common.type),
                                           index, global_trees[TI_INTEGER_ONE]), 0, 0);
                  expand_end_loop ();
                  emit_label (loop_end);
                }
            }
          else if ((index != 0 && ! host_integerp (index, 0))
                   || ! host_integerp (((elttype)->type.size), 1))
            {
              tree position;

              if (index == 0)
                index = size_int_wide ((long long) (1), SSIZETYPE);

              if (minelt)
                index = convert (sizetype_tab[(int) SSIZETYPE],
                                 fold (build (MINUS_EXPR, index,
                                              ((domain)->type.minval))));

              position = size_binop (MULT_EXPR, index,
                                     convert (sizetype_tab[(int) SSIZETYPE],
                                              ((elttype)->type.size_unit)));
              xtarget = offset_address (target,
                                        expand_expr (position, 0, VOIDmode, 0),
                                        highest_pow2_factor (position));
              xtarget = adjust_address_1 (xtarget, mode, 0, 1, 1);
              store_expr (value, xtarget, 0);
            }
          else
            {
              if (index != 0)
                bitpos = ((tree_low_cst (index, 0) - minelt)
                          * tree_low_cst (((elttype)->type.size), 1));
              else
                bitpos = (i * tree_low_cst (((elttype)->type.size), 1));

              if (((enum rtx_code) (target)->code) == MEM && !((target)->jump)
                  && ((enum tree_code) (type)->common.code) == ARRAY_TYPE
                  && ((type)->type.transparent_union_flag))
                {
                  target = copy_rtx (target);
                  ((target)->jump) = 1;
                }

              store_constructor_field (target, bitsize, bitpos, mode, value,
                                       type, cleared, get_alias_set (elttype));

            }
        }
    }


  else if (((enum tree_code) (type)->common.code) == SET_TYPE)
    {
      tree elt = (((exp))->exp.operands[1]);
      unsigned long long nbytes = int_size_in_bytes (type), nbits;
      tree domain = ((type)->type.values);
      tree domain_min, domain_max, bitlength;
# 4849 "expr.c"
      if (elt == (tree) ((void *)0) && size > 0)
        {
          if (!cleared)
            clear_storage (target, gen_rtx_CONST_INT (VOIDmode, (long long) (size)));
          return;
        }

      domain_min = convert (sizetype_tab[(int) SIZETYPE], ((domain)->type.minval));
      domain_max = convert (sizetype_tab[(int) SIZETYPE], ((domain)->type.maxval));
      bitlength = size_binop (PLUS_EXPR,
                              size_diffop (domain_max, domain_min),
                              size_int_wide ((long long) (1), SSIZETYPE));

      nbits = tree_low_cst (bitlength, 1);




      if (((enum machine_mode) (target)->mode) != BLKmode || nbits <= 2 * ((target_flags & 0x02000000) ? 64 : 32)
          || (nbytes <= 32 && ((elt)->common.chain) != (tree) ((void *)0)))
        {
          unsigned int set_word_size = ((((exp)->common.type))->type.align);
          enum machine_mode mode = mode_for_size (set_word_size, MODE_INT, 1);
          char *bit_buffer = (char *) __builtin_alloca (nbits);
          long long word = 0;
          unsigned int bit_pos = 0;
          unsigned int ibit = 0;
          unsigned int offset = 0;

          elt = get_set_constructor_bits (exp, bit_buffer, nbits);
          for (;;)
            {
              if (bit_buffer[ibit])
                {
                  if (0)
                    word |= (1 << (set_word_size - 1 - bit_pos));
                  else
                    word |= 1 << bit_pos;
                }

              bit_pos++; ibit++;
              if (bit_pos >= set_word_size || ibit == nbits)
                {
                  if (word != 0 || ! cleared)
                    {
                      rtx datum = gen_rtx_CONST_INT (VOIDmode, (long long) (word));
                      rtx to_rtx;




                      if (((enum rtx_code) (target)->code) == MEM)
                        to_rtx = adjust_address_1 (target, mode, offset, 1, 1);
                      else if (offset == 0)
                        to_rtx = target;
                      else
                        fancy_abort ("expr.c", 4905, __FUNCTION__);
                      emit_move_insn (to_rtx, datum);
                    }

                  if (ibit == nbits)
                    break;
                  word = 0;
                  bit_pos = 0;
                  offset += set_word_size / 8;
                }
            }
        }
      else if (!cleared)

        if (((elt)->common.chain) != (tree) ((void *)0)
            || (((elt)->list.purpose) == (tree) ((void *)0)
                ? nbits != 1
                : ( ! host_integerp (((elt)->list.value), 0)
                   || ! host_integerp (((elt)->list.purpose), 0)
                   || (tree_low_cst (((elt)->list.value), 0)
                       - tree_low_cst (((elt)->list.purpose), 0) + 1
                       != (long long) nbits))))
          clear_storage (target, expr_size (exp));

      for (; elt != (tree) ((void *)0); elt = ((elt)->common.chain))
        {

          tree startbit = ((elt)->list.purpose);

          tree endbit = ((elt)->list.value);

          long long startb, endb;

          rtx bitlength_rtx, startbit_rtx, endbit_rtx, targetx;

          bitlength_rtx = expand_expr (bitlength,
                                       (rtx) 0, MEM, EXPAND_CONST_ADDRESS);


          if (startbit == (tree) ((void *)0))
            {
              startbit = save_expr (endbit);
              endbit = startbit;
            }

          startbit = convert (sizetype_tab[(int) SIZETYPE], startbit);
          endbit = convert (sizetype_tab[(int) SIZETYPE], endbit);
          if (! integer_zerop (domain_min))
            {
              startbit = size_binop (MINUS_EXPR, startbit, domain_min);
              endbit = size_binop (MINUS_EXPR, endbit, domain_min);
            }
          startbit_rtx = expand_expr (startbit, (rtx) 0, MEM,
                                      EXPAND_CONST_ADDRESS);
          endbit_rtx = expand_expr (endbit, (rtx) 0, MEM,
                                    EXPAND_CONST_ADDRESS);

          if ((((enum rtx_code) (target)->code) == REG))
            {
              targetx
                = assign_temp
                  ((build_qualified_type (type_for_mode (((enum machine_mode) (target)->mode), 0),
                                          0x1)),
                   0, 1, 1);
              emit_move_insn (targetx, target);
            }

          else if (((enum rtx_code) (target)->code) == MEM)
            targetx = target;
          else
            fancy_abort ("expr.c", 4975, __FUNCTION__);





          if (((enum tree_code) (startbit)->common.code) == INTEGER_CST
              && ((enum tree_code) (endbit)->common.code) == INTEGER_CST
              && (startb = (((startbit)->int_cst.int_cst).low)) % 8 == 0
              && (endb = (((endbit)->int_cst.int_cst).low) + 1) % 8 == 0)
            {
              emit_library_call ((libfunc_table[LTI_memset]), LCT_NORMAL,
                                 VOIDmode, 3,
                                 plus_constant_wide (((((targetx)->fld[0]).rtx)), (long long) (startb / 8)),

                                 ((target_flags & 0x02000000) ? DImode : SImode),
                                 (const_int_rtx[64 -1]), ((integer_types[itk_int])->type.mode),
                                 gen_rtx_CONST_INT (VOIDmode, (long long) ((endb - startb) / 8)),
                                 ((sizetype_tab[(int) SIZETYPE])->type.mode));
            }
          else

            emit_library_call (gen_rtx_fmt_s (SYMBOL_REF, (((target_flags & 0x02000000) ? DImode : SImode)), ("__setbits")),
                               LCT_NORMAL, VOIDmode, 4, (((targetx)->fld[0]).rtx),
                               ((target_flags & 0x02000000) ? DImode : SImode), bitlength_rtx, ((sizetype_tab[(int) SIZETYPE])->type.mode),
                               startbit_rtx, ((sizetype_tab[(int) SIZETYPE])->type.mode),
                               endbit_rtx, ((sizetype_tab[(int) SIZETYPE])->type.mode));

          if ((((enum rtx_code) (target)->code) == REG))
            emit_move_insn (target, targetx);
        }
    }

  else
    fancy_abort ("expr.c", 5009, __FUNCTION__);
}
# 5030 "expr.c"
static rtx
store_field (target, bitsize, bitpos, mode, exp, value_mode, unsignedp, type,
             alias_set)
     rtx target;
     long long bitsize;
     long long bitpos;
     enum machine_mode mode;
     tree exp;
     enum machine_mode value_mode;
     int unsignedp;
     tree type;
     int alias_set;
{
  long long width_mask = 0;

  if (((enum tree_code) (exp)->common.code) == ERROR_MARK)
    return (const_int_rtx[64]);



  if (bitsize == 0)
    return expand_expr (exp, (const_int_rtx[64]), VOIDmode, 0);
  else if (bitsize >=0 && bitsize < (8 * 8))
    width_mask = ((long long) 1 << bitsize) - 1;
# 5066 "expr.c"
  if (mode == BLKmode
      && (((enum rtx_code) (target)->code) == REG || ((enum rtx_code) (target)->code) == SUBREG))
    {
      rtx object
        = assign_temp
          (build_qualified_type (type, ((((type)->common.readonly_flag) * 0x1) | (((type)->common.volatile_flag) * 0x2) | (((type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (type)->common.code) == RECORD_TYPE && ((type)->common.type)) * 0x8)) | 0x1),
           0, 1, 1);
      rtx blk_object = adjust_address_1 (object, BLKmode, 0, 1, 1);

      if (bitsize != (long long) (mode_bitsize[(int) (((enum machine_mode) (target)->mode))]))
        emit_move_insn (object, target);

      store_field (blk_object, bitsize, bitpos, mode, exp, VOIDmode, 0, type,
                   alias_set);

      emit_move_insn (target, object);


      return blk_object;
    }

  if (((enum rtx_code) (target)->code) == CONCAT)
    {


      if (bitpos != 0)
        fancy_abort ("expr.c", 5092, __FUNCTION__);
      return store_expr (exp, target, 0);
    }





  if (mode == VOIDmode
      || (mode != BLKmode && ! direct_store[(int) mode]
          && (mode_class[(int) (mode)]) != MODE_COMPLEX_INT
          && (mode_class[(int) (mode)]) != MODE_COMPLEX_FLOAT)
      || ((enum rtx_code) (target)->code) == REG
      || ((enum rtx_code) (target)->code) == SUBREG


      || (mode != BLKmode && 0
          && (((((target)->fld[1]).rtmem) != 0 ? (((target)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (target)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (target)->mode)) : 8)) < get_mode_alignment (mode)
              || bitpos % get_mode_alignment (mode)))



      || (bitsize >= 0
          && ((enum tree_code) (((((exp)->common.type))->type.size))->common.code) == INTEGER_CST
          && compare_tree_int (((((exp)->common.type))->type.size), bitsize) != 0))
    {
      rtx temp = expand_expr (exp, (rtx) 0, VOIDmode, 0);





      if (0 && (mode_class[(int) (((enum machine_mode) (temp)->mode))]) == MODE_INT
          && bitsize < (long long) (mode_bitsize[(int) (((enum machine_mode) (temp)->mode))])
          && ((enum tree_code) (((exp)->common.type))->common.code) == RECORD_TYPE)
        temp = expand_shift (RSHIFT_EXPR, ((enum machine_mode) (temp)->mode), temp,
                             size_int_wide ((long long) ((mode_bitsize[(int) (((enum machine_mode) (temp)->mode))]) - bitsize), SIZETYPE),

                             temp, 1);



      if (mode != VOIDmode && mode != BLKmode
          && mode != ((((exp)->common.type))->type.mode))
        temp = convert_modes (mode, ((((exp)->common.type))->type.mode), temp, 1);




      if (((enum machine_mode) (target)->mode) == BLKmode && ((enum machine_mode) (temp)->mode) == BLKmode)
        {
          if (((enum rtx_code) (target)->code) != MEM || ((enum rtx_code) (temp)->code) != MEM
              || bitpos % 8 != 0)
            fancy_abort ("expr.c", 5145, __FUNCTION__);

          target = adjust_address_1 (target, VOIDmode, bitpos / 8, 1, 1);
          emit_block_move (target, temp,
                           gen_rtx_CONST_INT (VOIDmode, (long long) ((bitsize + 8 - 1) / 8)));


          return value_mode == VOIDmode ? (const_int_rtx[64]) : target;
        }


      store_bit_field (target, bitsize, bitpos, mode, temp,
                       int_size_in_bytes (type));

      if (value_mode != VOIDmode)
        {


          if (width_mask != 0
              && ! (((enum rtx_code) (target)->code) == MEM && ((target)->volatil)))
            {
              tree count;
              enum machine_mode tmode;

              tmode = ((enum machine_mode) (temp)->mode);
              if (tmode == VOIDmode)
                tmode = value_mode;

              if (unsignedp)
                return expand_and (tmode, temp,
                                   gen_rtx_CONST_INT (VOIDmode, (long long) (trunc_int_for_mode (width_mask, tmode))),

                                   (rtx) 0);

              count = build_int_2_wide ((unsigned long long) ((mode_bitsize[(int) (tmode)]) - bitsize), (long long) (0));
              temp = expand_shift (LSHIFT_EXPR, tmode, temp, count, 0, 0);
              return expand_shift (RSHIFT_EXPR, tmode, temp, count, 0, 0);
            }

          return extract_bit_field (target, bitsize, bitpos, unsignedp,
                                    (rtx) 0, value_mode, VOIDmode,
                                    int_size_in_bytes (type));
        }
      return (const_int_rtx[64]);
    }
  else
    {
      rtx addr = (((target)->fld[0]).rtx);
      rtx to_rtx = target;




      if (value_mode != VOIDmode && ((enum rtx_code) (addr)->code) != REG
          && ! (((enum rtx_code) (addr)->code) == LABEL_REF || ((enum rtx_code) (addr)->code) == SYMBOL_REF || ((enum rtx_code) (addr)->code) == CONST_INT || ((enum rtx_code) (addr)->code) == CONST || ((enum rtx_code) (addr)->code) == CONST_DOUBLE)

          && ! (((enum rtx_code) (addr)->code) == PLUS
                && ((enum rtx_code) ((((addr)->fld[1]).rtx))->code) == CONST_INT
                && ((((addr)->fld[0]).rtx) == (global_rtl[GR_VIRTUAL_INCOMING_ARGS])
                    || (((addr)->fld[0]).rtx) == (global_rtl[GR_VIRTUAL_STACK_ARGS]))))
        to_rtx = replace_equiv_address (to_rtx, copy_to_reg (addr));



      to_rtx = adjust_address_1 (target, mode, bitpos / 8, 1, 1);

      if (to_rtx == target)
        to_rtx = copy_rtx (to_rtx);

      do { if (1) { ((to_rtx)->in_struct) = 1; ((to_rtx)->frame_related) = 0; } else { ((to_rtx)->in_struct) = 0; ((to_rtx)->frame_related) = 1; } } while (0);
      if (!((to_rtx)->jump) && ((((to_rtx)->fld[1]).rtmem) == 0 ? 0 : (((to_rtx)->fld[1]).rtmem)->alias) != 0)
        set_mem_alias_set (to_rtx, alias_set);

      return store_expr (exp, to_rtx, value_mode != VOIDmode);
    }
}
# 5244 "expr.c"
tree
get_inner_reference (exp, pbitsize, pbitpos, poffset, pmode,
                     punsignedp, pvolatilep)
     tree exp;
     long long *pbitsize;
     long long *pbitpos;
     tree *poffset;
     enum machine_mode *pmode;
     int *punsignedp;
     int *pvolatilep;
{
  tree size_tree = 0;
  enum machine_mode mode = VOIDmode;
  tree offset = global_trees[TI_SIZE_ZERO];
  tree bit_offset = global_trees[TI_BITSIZE_ZERO];
  tree placeholder_ptr = 0;
  tree tem;



  if (((enum tree_code) (exp)->common.code) == COMPONENT_REF)
    {
      size_tree = ((((exp)->exp.operands[1]))->decl.size);
      if (! ((((exp)->exp.operands[1]))->decl.bit_field_flag))
        mode = ((((exp)->exp.operands[1]))->decl.mode);

      *punsignedp = ((((exp)->exp.operands[1]))->common.unsigned_flag);
    }
  else if (((enum tree_code) (exp)->common.code) == BIT_FIELD_REF)
    {
      size_tree = ((exp)->exp.operands[1]);
      *punsignedp = ((exp)->common.unsigned_flag);
    }
  else
    {
      mode = ((((exp)->common.type))->type.mode);
      *punsignedp = ((((exp)->common.type))->common.unsigned_flag);

      if (mode == BLKmode)
        size_tree = ((((exp)->common.type))->type.size);
      else
        *pbitsize = (mode_bitsize[(int) (mode)]);
    }

  if (size_tree != 0)
    {
      if (! host_integerp (size_tree, 1))
        mode = BLKmode, *pbitsize = -1;
      else
        *pbitsize = tree_low_cst (size_tree, 1);
    }



  while (1)
    {
      if (((enum tree_code) (exp)->common.code) == BIT_FIELD_REF)
        bit_offset = size_binop (PLUS_EXPR, bit_offset, ((exp)->exp.operands[2]));
      else if (((enum tree_code) (exp)->common.code) == COMPONENT_REF)
        {
          tree field = ((exp)->exp.operands[1]);
          tree this_offset = ((field)->decl.arguments);




          if (this_offset == 0)
            break;
          else if (! ((this_offset)->common.constant_flag)
                   && contains_placeholder_p (this_offset))
            this_offset = build (WITH_RECORD_EXPR, sizetype_tab[(int) SIZETYPE], this_offset, exp);

          offset = size_binop (PLUS_EXPR, offset, this_offset);
          bit_offset = size_binop (PLUS_EXPR, bit_offset,
                                   ((field)->decl.u2.t));


        }

      else if (((enum tree_code) (exp)->common.code) == ARRAY_REF
               || ((enum tree_code) (exp)->common.code) == ARRAY_RANGE_REF)
        {
          tree index = ((exp)->exp.operands[1]);
          tree array = ((exp)->exp.operands[0]);
          tree domain = ((((array)->common.type))->type.values);
          tree low_bound = (domain ? ((domain)->type.minval) : 0);
          tree unit_size = ((((((array)->common.type))->common.type))->type.size_unit);





          if (low_bound != 0 && ! integer_zerop (low_bound))
            index = fold (build (MINUS_EXPR, ((index)->common.type),
                                 index, low_bound));




          if (! ((index)->common.constant_flag)
              && contains_placeholder_p (index))
            index = build (WITH_RECORD_EXPR, ((index)->common.type), index, exp);
          if (! ((unit_size)->common.constant_flag)
              && contains_placeholder_p (unit_size))
            unit_size = build (WITH_RECORD_EXPR, sizetype_tab[(int) SIZETYPE], unit_size, array);

          offset = size_binop (PLUS_EXPR, offset,
                               size_binop (MULT_EXPR,
                                           convert (sizetype_tab[(int) SIZETYPE], index),
                                           unit_size));
        }

      else if (((enum tree_code) (exp)->common.code) == PLACEHOLDER_EXPR)
        {
          tree new = find_placeholder (exp, &placeholder_ptr);




          if (new == 0)
            break;
          else
            exp = new;

          continue;
        }
      else if (((enum tree_code) (exp)->common.code) != NON_LVALUE_EXPR
               && ((enum tree_code) (exp)->common.code) != VIEW_CONVERT_EXPR
               && ! ((((enum tree_code) (exp)->common.code) == NOP_EXPR
                      || ((enum tree_code) (exp)->common.code) == CONVERT_EXPR)
                     && (((((exp)->common.type))->type.mode)
                         == ((((((exp)->exp.operands[0]))->common.type))->type.mode))))
        break;


      if (((exp)->common.volatile_flag))
        *pvolatilep = 1;

      exp = ((exp)->exp.operands[0]);
    }



  if (host_integerp (offset, 0)
      && 0 != (tem = size_binop (MULT_EXPR, convert (sizetype_tab[(int) BITSIZETYPE], offset),
                                 global_trees[TI_BITSIZE_UNIT]))
      && 0 != (tem = size_binop (PLUS_EXPR, tem, bit_offset))
      && host_integerp (tem, 0))
    *pbitpos = tree_low_cst (tem, 0), *poffset = 0;
  else
    *pbitpos = tree_low_cst (bit_offset, 0), *poffset = offset;

  *pmode = mode;
  return exp;
}



int
handled_component_p (t)
     tree t;
{
  switch (((enum tree_code) (t)->common.code))
    {
    case BIT_FIELD_REF:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case NON_LVALUE_EXPR:
    case VIEW_CONVERT_EXPR:
      return 1;

    case NOP_EXPR:
    case CONVERT_EXPR:
      return (((((t)->common.type))->type.mode)
              == ((((((t)->exp.operands[0]))->common.type))->type.mode));

    default:
      return 0;
    }
}
# 5433 "expr.c"
rtx
force_operand (value, target)
     rtx value, target;
{
  optab binoptab = 0;


  rtx tmp;
  rtx op2;

  rtx subtarget = get_subtarget (target);


  if ((((enum rtx_code) (value)->code) == PLUS || ((enum rtx_code) (value)->code) == MINUS)
      && (((value)->fld[0]).rtx) == pic_offset_table_rtx
      && (((enum rtx_code) ((((value)->fld[1]).rtx))->code) == SYMBOL_REF
          || ((enum rtx_code) ((((value)->fld[1]).rtx))->code) == LABEL_REF
          || ((enum rtx_code) ((((value)->fld[1]).rtx))->code) == CONST))
    {
      if (!subtarget)
        subtarget = gen_reg_rtx (((enum machine_mode) (value)->mode));
      emit_move_insn (subtarget, value);
      return subtarget;
    }

  if (((enum rtx_code) (value)->code) == PLUS)
    binoptab = (optab_table[OTI_add]);
  else if (((enum rtx_code) (value)->code) == MINUS)
    binoptab = (optab_table[OTI_sub]);
  else if (((enum rtx_code) (value)->code) == MULT)
    {
      op2 = (((value)->fld[1]).rtx);
      if (!(((enum rtx_code) (op2)->code) == LABEL_REF || ((enum rtx_code) (op2)->code) == SYMBOL_REF || ((enum rtx_code) (op2)->code) == CONST_INT || ((enum rtx_code) (op2)->code) == CONST_DOUBLE || ((enum rtx_code) (op2)->code) == CONST || ((enum rtx_code) (op2)->code) == HIGH || ((enum rtx_code) (op2)->code) == CONST_VECTOR || ((enum rtx_code) (op2)->code) == CONSTANT_P_RTX)
          && !(((enum rtx_code) (op2)->code) == REG && op2 != subtarget))
        subtarget = 0;
      tmp = force_operand ((((value)->fld[0]).rtx), subtarget);
      return expand_mult (((enum machine_mode) (value)->mode), tmp,
                          force_operand (op2, (rtx) 0),
                          target, 1);
    }

  if (binoptab)
    {
      op2 = (((value)->fld[1]).rtx);
      if (!(((enum rtx_code) (op2)->code) == LABEL_REF || ((enum rtx_code) (op2)->code) == SYMBOL_REF || ((enum rtx_code) (op2)->code) == CONST_INT || ((enum rtx_code) (op2)->code) == CONST_DOUBLE || ((enum rtx_code) (op2)->code) == CONST || ((enum rtx_code) (op2)->code) == HIGH || ((enum rtx_code) (op2)->code) == CONST_VECTOR || ((enum rtx_code) (op2)->code) == CONSTANT_P_RTX)
          && !(((enum rtx_code) (op2)->code) == REG && op2 != subtarget))
        subtarget = 0;
      if (binoptab == (optab_table[OTI_sub]) && ((enum rtx_code) (op2)->code) == CONST_INT)
        {
          binoptab = (optab_table[OTI_add]);
          op2 = negate_rtx (((enum machine_mode) (value)->mode), op2);
        }







      if (binoptab == (optab_table[OTI_add]) && ((enum rtx_code) (op2)->code) == CONST_INT
          && ((enum rtx_code) ((((value)->fld[0]).rtx))->code) == PLUS
          && ((enum rtx_code) (((((((value)->fld[0]).rtx))->fld[0]).rtx))->code) == REG
          && (((((((((value)->fld[0]).rtx))->fld[0]).rtx))->fld[0]).rtuint) >= (53)
          && (((((((((value)->fld[0]).rtx))->fld[0]).rtx))->fld[0]).rtuint) <= (((53)) + 4))
        {
          rtx temp = expand_binop (((enum machine_mode) (value)->mode), binoptab,
                                   ((((((value)->fld[0]).rtx))->fld[0]).rtx), op2,
                                   subtarget, 0, OPTAB_LIB_WIDEN);
          return expand_binop (((enum machine_mode) (value)->mode), binoptab, temp,
                               force_operand (((((((value)->fld[0]).rtx))->fld[1]).rtx), 0),
                               target, 0, OPTAB_LIB_WIDEN);
        }

      tmp = force_operand ((((value)->fld[0]).rtx), subtarget);
      return expand_binop (((enum machine_mode) (value)->mode), binoptab, tmp,
                           force_operand (op2, (rtx) 0),
                           target, 0, OPTAB_LIB_WIDEN);


    }




  if (((enum rtx_code) (value)->code) == SUBREG && ((enum rtx_code) ((((value)->fld[0]).rtx))->code) == MEM
      && ((mode_size[(int) (((enum machine_mode) (value)->mode))])
          > (mode_size[(int) (((enum machine_mode) ((((value)->fld[0]).rtx))->mode))])))
    value
      = simplify_gen_subreg (((enum machine_mode) (value)->mode),
                             force_reg (((enum machine_mode) ((((value)->fld[0]).rtx))->mode),
                                        force_operand ((((value)->fld[0]).rtx),
                                                       (rtx) 0)),
                             ((enum machine_mode) ((((value)->fld[0]).rtx))->mode),
                             (((value)->fld[1]).rtuint));


  return value;
}
# 5540 "expr.c"
int
safe_from_p (x, exp, top_p)
     rtx x;
     tree exp;
     int top_p;
{
  rtx exp_rtl = 0;
  int i, nops;
  static tree save_expr_list;

  if (x == 0






      || (top_p && ((exp)->common.type) != 0 && (((((exp)->common.type))->type.size) != (tree) ((void *)0))
          && ((enum tree_code) (((((exp)->common.type))->type.size))->common.code) != INTEGER_CST
          && (((enum tree_code) (((exp)->common.type))->common.code) != ARRAY_TYPE
              || (((((exp)->common.type)))->type.maxval) == (tree) ((void *)0)
              || ((enum tree_code) ((((((exp)->common.type)))->type.maxval))->common.code)
              != INTEGER_CST)
          && ((enum machine_mode) (x)->mode) == BLKmode)

      || (((enum rtx_code) (x)->code) == MEM
          && ((((x)->fld[0]).rtx) == (global_rtl[GR_VIRTUAL_OUTGOING_ARGS])
              || (((enum rtx_code) ((((x)->fld[0]).rtx))->code) == PLUS
                  && ((((((x)->fld[0]).rtx))->fld[0]).rtx) == (global_rtl[GR_VIRTUAL_OUTGOING_ARGS])))))
    return 1;



  if (((enum rtx_code) (x)->code) == SUBREG)
    {
      x = (((x)->fld[0]).rtx);
      if (((enum rtx_code) (x)->code) == REG && (((x)->fld[0]).rtuint) < 53)
        return 0;
    }
# 5589 "expr.c"
  if (top_p)
    {
      int rtn;
      tree t;

      save_expr_list = 0;

      rtn = safe_from_p (x, exp, 0);

      for (t = save_expr_list; t != 0; t = ((t)->common.chain))
        ((((t)->list.purpose))->common.private_flag) = 0;

      return rtn;
    }


  switch (tree_code_type[(int) (((enum tree_code) (exp)->common.code))])
    {
    case 'd':
      exp_rtl = (((exp)->decl.rtl != ((void *)0)) ? ((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)) : ((void *)0));
      break;

    case 'c':
      return 1;

    case 'x':
      if (((enum tree_code) (exp)->common.code) == TREE_LIST)
        return ((((exp)->list.value) == 0
                 || safe_from_p (x, ((exp)->list.value), 0))
                && (((exp)->common.chain) == 0
                    || safe_from_p (x, ((exp)->common.chain), 0)));
      else if (((enum tree_code) (exp)->common.code) == ERROR_MARK)
        return 1;
      else
        return 0;

    case '1':
      return safe_from_p (x, ((exp)->exp.operands[0]), 0);

    case '2':
    case '<':
      return (safe_from_p (x, ((exp)->exp.operands[0]), 0)
              && safe_from_p (x, ((exp)->exp.operands[1]), 0));

    case 'e':
    case 'r':





      switch (((enum tree_code) (exp)->common.code))
        {
        case ADDR_EXPR:


          if (staticp (((exp)->exp.operands[0]))
              || ((exp)->common.static_flag)
              || safe_from_p (x, ((exp)->exp.operands[0]), 0))
            return 1;




          exp = ((exp)->exp.operands[0]);
          if ((tree_code_type[(int) (((enum tree_code) (exp)->common.code))] == 'd'))
            {
              if (!((exp)->decl.rtl != ((void *)0))
                  || ((enum rtx_code) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->code) != MEM)
                return 0;
              else
                exp_rtl = (((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx);
            }
          break;

        case INDIRECT_REF:
          if (((enum rtx_code) (x)->code) == MEM
              && alias_sets_conflict_p (((((x)->fld[1]).rtmem) == 0 ? 0 : (((x)->fld[1]).rtmem)->alias),
                                        get_alias_set (exp)))
            return 0;
          break;

        case CALL_EXPR:


          if ((((enum rtx_code) (x)->code) == REG && (((x)->fld[0]).rtuint) < 53)
              || ((enum rtx_code) (x)->code) == MEM)
            return 0;
          break;

        case RTL_EXPR:



          if ((*(rtx *) &(exp)->exp.operands[0]))
            return 0;

          exp_rtl = (*(rtx *) &(exp)->exp.operands[1]);
          break;

        case WITH_CLEANUP_EXPR:
          exp_rtl = (*(rtx *) &(exp)->exp.operands[2]);
          break;

        case CLEANUP_POINT_EXPR:
          return safe_from_p (x, ((exp)->exp.operands[0]), 0);

        case SAVE_EXPR:
          exp_rtl = (*(rtx *) &(exp)->exp.operands[2]);
          if (exp_rtl)
            break;




          if (((exp)->common.private_flag))
            return 1;

          ((exp)->common.private_flag) = 1;
          if (! safe_from_p (x, ((exp)->exp.operands[0]), 0))
            {
              ((exp)->common.private_flag) = 0;
              return 0;
            }

          save_expr_list = tree_cons (exp, (tree) ((void *)0), save_expr_list);
          return 1;

        case BIND_EXPR:


          return safe_from_p (x, ((exp)->exp.operands[1]), 0);

        case METHOD_CALL_EXPR:

          fancy_abort ("expr.c", 5724, __FUNCTION__);

        default:
          break;
        }


      if (exp_rtl)
        break;

      nops = first_rtl_op (((enum tree_code) (exp)->common.code));
      for (i = 0; i < nops; i++)
        if (((exp)->exp.operands[i]) != 0
            && ! safe_from_p (x, ((exp)->exp.operands[i]), 0))
          return 0;



      if ((unsigned int) ((enum tree_code) (exp)->common.code)
          >= (unsigned int) LAST_AND_UNUSED_TREE_CODE
          && !(*lang_hooks.safe_from_p) (x, exp))
        return 0;
    }



  if (exp_rtl)
    {
      if (((enum rtx_code) (exp_rtl)->code) == SUBREG)
        {
          exp_rtl = (((exp_rtl)->fld[0]).rtx);
          if (((enum rtx_code) (exp_rtl)->code) == REG
              && (((exp_rtl)->fld[0]).rtuint) < 53)
            return 0;
        }



      return ! (rtx_equal_p (x, exp_rtl)
                || (((enum rtx_code) (x)->code) == MEM && ((enum rtx_code) (exp_rtl)->code) == MEM
                    && true_dependence (exp_rtl, VOIDmode, x,
                                        rtx_addr_varies_p)));
    }


  return 1;
}




static rtx
var_rtx (exp)
     tree exp;
{
  while ((((enum tree_code) (exp)->common.code) == NOP_EXPR || ((enum tree_code) (exp)->common.code) == CONVERT_EXPR || ((enum tree_code) (exp)->common.code) == NON_LVALUE_EXPR) && ((exp)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((exp)->common.type))->type.mode) == ((((((exp)->exp.operands[0]))->common.type))->type.mode))) (exp) = ((exp)->exp.operands[0]);
  switch (((enum tree_code) (exp)->common.code))
    {
    case PARM_DECL:
    case VAR_DECL:
      return ((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl));
    default:
      return 0;
    }
}
# 5848 "expr.c"
static long long
highest_pow2_factor (exp)
     tree exp;
{
  long long c0, c1;

  switch (((enum tree_code) (exp)->common.code))
    {
    case INTEGER_CST:






      if (((exp)->common.static_flag))
        return 128;
      else
        {


          c0 = (((exp)->int_cst.int_cst).low);
          c0 &= -c0;
          return c0 ? c0 : 128;
        }
      break;

    case PLUS_EXPR: case MINUS_EXPR: case MIN_EXPR: case MAX_EXPR:
      c0 = highest_pow2_factor (((exp)->exp.operands[0]));
      c1 = highest_pow2_factor (((exp)->exp.operands[1]));
      return ((c0) < (c1) ? (c0) : (c1));

    case MULT_EXPR:
      c0 = highest_pow2_factor (((exp)->exp.operands[0]));
      c1 = highest_pow2_factor (((exp)->exp.operands[1]));
      return c0 * c1;

    case ROUND_DIV_EXPR: case TRUNC_DIV_EXPR: case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
      if (integer_pow2p (((exp)->exp.operands[1]))
          && host_integerp (((exp)->exp.operands[1]), 1))
        {
          c0 = highest_pow2_factor (((exp)->exp.operands[0]));
          c1 = tree_low_cst (((exp)->exp.operands[1]), 1);
          return ((1) > (c0 / c1) ? (1) : (c0 / c1));
        }
      break;

    case NON_LVALUE_EXPR: case NOP_EXPR: case CONVERT_EXPR:
    case SAVE_EXPR: case WITH_RECORD_EXPR:
      return highest_pow2_factor (((exp)->exp.operands[0]));

    case COMPOUND_EXPR:
      return highest_pow2_factor (((exp)->exp.operands[1]));

    case COND_EXPR:
      c0 = highest_pow2_factor (((exp)->exp.operands[1]));
      c1 = highest_pow2_factor (((exp)->exp.operands[2]));
      return ((c0) < (c1) ? (c0) : (c1));

    default:
      break;
    }

  return 1;
}




static long long
highest_pow2_factor_for_type (type, exp)
     tree type;
     tree exp;
{
  long long type_align, factor;

  factor = highest_pow2_factor (exp);
  type_align = ((type)->type.align) / 8;
  return ((factor) > (type_align) ? (factor) : (type_align));
}
# 5938 "expr.c"
tree
find_placeholder (exp, plist)
     tree exp;
     tree *plist;
{
  tree type = ((exp)->common.type);
  tree placeholder_expr;

  for (placeholder_expr
       = plist && *plist ? ((*plist)->common.chain) : placeholder_list;
       placeholder_expr != 0;
       placeholder_expr = ((placeholder_expr)->common.chain))
    {
      tree need_type = ((type)->type.main_variant);
      tree elt;




      for (elt = ((placeholder_expr)->list.purpose); elt != 0;
           elt = ((((enum tree_code) (elt)->common.code) == COMPOUND_EXPR
                   || ((enum tree_code) (elt)->common.code) == COND_EXPR)
                  ? ((elt)->exp.operands[1])
                  : (tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == 'r'
                     || tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == '1'
                     || tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == '2'
                     || tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == 'e')
                  ? ((elt)->exp.operands[0]) : 0))
        if (((((elt)->common.type))->type.main_variant) == need_type)
          {
            if (plist)
              *plist = placeholder_expr;
            return elt;
          }

      for (elt = ((placeholder_expr)->list.purpose); elt != 0;
           elt
           = ((((enum tree_code) (elt)->common.code) == COMPOUND_EXPR
               || ((enum tree_code) (elt)->common.code) == COND_EXPR)
              ? ((elt)->exp.operands[1])
              : (tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == 'r'
                 || tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == '1'
                 || tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == '2'
                 || tree_code_type[(int) (((enum tree_code) (elt)->common.code))] == 'e')
              ? ((elt)->exp.operands[0]) : 0))
        if ((((enum tree_code) (((elt)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((elt)->common.type))->common.code) == REFERENCE_TYPE)
            && (((((((elt)->common.type))->common.type))->type.main_variant)
                == need_type))
          {
            if (plist)
              *plist = placeholder_expr;
            return build1 (INDIRECT_REF, need_type, elt);
          }
    }

  return 0;
}
# 6031 "expr.c"
rtx
expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  rtx op0, op1, temp;
  tree type = ((exp)->common.type);
  int unsignedp = ((type)->common.unsigned_flag);
  enum machine_mode mode;
  enum tree_code code = ((enum tree_code) (exp)->common.code);
  optab this_optab;
  rtx subtarget, original_target;
  int ignore;
  tree context;


  if (((enum tree_code) (exp)->common.code) == ERROR_MARK || ((enum tree_code) (type)->common.code) == ERROR_MARK)
    {
      op0 = (const_tiny_rtx[0][(int) (tmode)]);
      if (op0 != 0)
        return op0;
      return (const_int_rtx[64]);
    }

  mode = ((type)->type.mode);

  subtarget = get_subtarget (target);
  original_target = target;
  ignore = (target == (const_int_rtx[64])
            || ((code == NON_LVALUE_EXPR || code == NOP_EXPR
                 || code == CONVERT_EXPR || code == REFERENCE_EXPR
                 || code == COND_EXPR || code == VIEW_CONVERT_EXPR)
                && ((enum tree_code) (type)->common.code) == VOID_TYPE));







  if (ignore)
    {
      if (! ((exp)->common.side_effects_flag))
        return (const_int_rtx[64]);



      if (((exp)->common.volatile_flag)
          && ((enum tree_code) (exp)->common.code) != FUNCTION_DECL
          && mode != VOIDmode && mode != BLKmode
          && modifier != EXPAND_CONST_ADDRESS)
        {
          temp = expand_expr (exp, (rtx) 0, VOIDmode, modifier);
          if (((enum rtx_code) (temp)->code) == MEM)
            temp = copy_to_reg (temp);
          return (const_int_rtx[64]);
        }

      if (tree_code_type[(int) (code)] == '1' || code == COMPONENT_REF
          || code == INDIRECT_REF || code == BUFFER_REF)
        return expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode,
                            modifier);

      else if (tree_code_type[(int) (code)] == '2' || tree_code_type[(int) (code)] == '<'
               || code == ARRAY_REF || code == ARRAY_RANGE_REF)
        {
          expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode, modifier);
          expand_expr (((exp)->exp.operands[1]), (const_int_rtx[64]), VOIDmode, modifier);
          return (const_int_rtx[64]);
        }
      else if ((code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR)
               && ! ((((exp)->exp.operands[1]))->common.side_effects_flag))


        return expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode,
                            modifier);
      else if (code == BIT_FIELD_REF)
        {
          expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode, modifier);
          expand_expr (((exp)->exp.operands[1]), (const_int_rtx[64]), VOIDmode, modifier);
          expand_expr (((exp)->exp.operands[2]), (const_int_rtx[64]), VOIDmode, modifier);
          return (const_int_rtx[64]);
        }

      target = 0;
    }
# 6169 "expr.c"
  if (! cse_not_expected && mode != BLKmode && target
      && (((enum rtx_code) (target)->code) != REG || (((target)->fld[0]).rtuint) < 53)
      && ! (code == CONSTRUCTOR && (mode_size[(int) (mode)]) > ((target_flags & 0x02000000) ? 8 : 4)))
    target = subtarget;

  switch (code)
    {
    case LABEL_DECL:
      {
        tree function = decl_function_context (exp);

        if (function != current_function_decl
            && function != inline_function_decl && function != 0)
          {
            struct function *p = find_function_data (function);
            p->expr->x_forced_labels
              = gen_rtx_fmt_ee (EXPR_LIST, (VOIDmode), (label_rtx (exp)), (p->expr->x_forced_labels));

          }
        else
          {
            if (modifier == EXPAND_INITIALIZER)
              (cfun->expr->x_forced_labels) = gen_rtx_fmt_ee (EXPR_LIST, (VOIDmode), (label_rtx (exp)), ((cfun->expr->x_forced_labels)));


          }

        temp = gen_rtx_MEM (QImode,
                            gen_rtx_fmt_u00 (LABEL_REF, (((target_flags & 0x02000000) ? DImode : SImode)), (label_rtx (exp))));
        if (function != current_function_decl
            && function != inline_function_decl && function != 0)
          (((((temp)->fld[0]).rtx))->volatil) = 1;
        return temp;
      }

    case PARM_DECL:
      if (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)) == 0)
        {
          error_with_decl (exp, "prior parameter's size depends on `%s'");
          return (const_tiny_rtx[0][(int) (mode)]);
        }



    case VAR_DECL:


      if (((exp)->decl.size) == 0 && (((((exp)->common.type))->type.size) != (tree) ((void *)0))
          && (((exp)->common.static_flag) || ((exp)->decl.external_flag)))
        {
          rtx value = (((exp)->decl.rtl != ((void *)0)) ? ((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)) : ((void *)0));

          layout_decl (exp, 0);



          if (value != 0)
            {
              ((value)->mode = (enum machine_mode) (((exp)->decl.mode)));
              ((exp)->decl.rtl = (0));
              set_mem_attributes (value, exp, 1);
              ((exp)->decl.rtl = (value));
            }
        }



    case FUNCTION_DECL:
    case RESULT_DECL:
      if (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)) == 0)
        fancy_abort ("expr.c", 6239, __FUNCTION__);




      if (! ((exp)->common.used_flag))
        {
          assemble_external (exp);
          ((exp)->common.used_flag) = 1;
        }


      temp = 0;


      context = decl_function_context (exp);






      if (context != 0 && context != current_function_decl
          && context != inline_function_decl

          && ! (((enum rtx_code) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->code) == MEM
                && (((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == LABEL_REF || ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == SYMBOL_REF || ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == CONST_INT || ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == CONST_DOUBLE || ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == CONST || ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == HIGH || ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == CONST_VECTOR || ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == CONSTANT_P_RTX)))
        {
          rtx addr;


          ((exp)->decl.nonlocal_flag) = 1;
          if (((current_function_decl)->decl.regdecl_flag))
            fancy_abort ("expr.c", 6272, __FUNCTION__);
          mark_addressable (exp);
          if (((enum rtx_code) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->code) != MEM)
            fancy_abort ("expr.c", 6275, __FUNCTION__);
          addr = (((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx);
          if (((enum rtx_code) (addr)->code) == MEM)
            addr
              = replace_equiv_address (addr,
                                       fix_lexical_addr ((((addr)->fld[0]).rtx), exp));
          else
            addr = fix_lexical_addr (addr, exp);

          temp = replace_equiv_address (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)), addr);
        }





      else if (((enum rtx_code) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->code) == MEM
               && ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) == REG)
        temp = validize_mem (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)));





      else if (((enum rtx_code) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->code) == MEM
               && modifier != EXPAND_CONST_ADDRESS
               && modifier != EXPAND_SUM
               && modifier != EXPAND_INITIALIZER
               && (! memory_address_p (((exp)->decl.mode),
                                       (((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))
                   || (flag_force_addr
                       && ((enum rtx_code) ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx))->code) != REG)))
        temp = replace_equiv_address (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)),
                                      copy_rtx ((((((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->fld[0]).rtx)));



      if (temp != 0)
        {
          if (((enum rtx_code) (temp)->code) == MEM && ((enum rtx_code) ((((temp)->fld[0]).rtx))->code) == REG)
            mark_reg_pointer ((((temp)->fld[0]).rtx), ((exp)->decl.u1.a.align));

          return temp;
        }





      if (((enum rtx_code) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->code) == REG
          && ((enum machine_mode) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->mode) != ((exp)->decl.mode))
        {


          if (((enum machine_mode) (((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)))->mode)
              != promote_mode (type, ((exp)->decl.mode), &unsignedp,
                               (((enum tree_code) (exp)->common.code) == RESULT_DECL ? 1 : 0)))
            fancy_abort ("expr.c", 6332, __FUNCTION__);

          temp = gen_lowpart_SUBREG (mode, ((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl)));
          ((temp)->in_struct) = 1;
          ((temp)->unchanging) = unsignedp;
          return temp;
        }

      return ((exp)->decl.rtl ? (exp)->decl.rtl : (make_decl_rtl (exp, ((void *)0)), (exp)->decl.rtl));

    case INTEGER_CST:
      temp = immed_double_const ((((exp)->int_cst.int_cst).low),
                                 (((exp)->int_cst.int_cst).high), mode);






      if (((exp)->common.static_flag)
          && modifier != EXPAND_INITIALIZER)
        temp = force_reg (mode, temp);

      return temp;

    case CONST_DECL:
      return expand_expr (((exp)->decl.initial), target, VOIDmode, 0);

    case REAL_CST:
# 6371 "expr.c"
      return immed_real_const (exp);

    case COMPLEX_CST:
    case STRING_CST:
      if (! ((exp)->real_cst.rtl))
        output_constant_def (exp, 1);




      if (((enum rtx_code) (((exp)->real_cst.rtl))->code) == MEM
          && modifier != EXPAND_CONST_ADDRESS
          && modifier != EXPAND_INITIALIZER
          && modifier != EXPAND_SUM
          && (! memory_address_p (mode, (((((exp)->real_cst.rtl))->fld[0]).rtx))
              || (flag_force_addr
                  && ((enum rtx_code) ((((((exp)->real_cst.rtl))->fld[0]).rtx))->code) != REG)))
        return replace_equiv_address (((exp)->real_cst.rtl),
                                      copy_rtx ((((((exp)->real_cst.rtl))->fld[0]).rtx)));
      return ((exp)->real_cst.rtl);

    case EXPR_WITH_FILE_LOCATION:
      {
        rtx to_return;
        const char *saved_input_filename = input_filename;
        int saved_lineno = lineno;
        input_filename = ((const char *) ((((exp))->exp.operands[1]))->identifier.id.str);
        lineno = (((exp)->exp.complexity) >> 12);
        if (((exp)->common.public_flag))
          emit_line_note (input_filename, lineno);

        to_return = expand_expr ((((exp))->exp.operands[0]), target, tmode, modifier);
        input_filename = saved_input_filename;
        lineno = saved_lineno;
        return to_return;
      }

    case SAVE_EXPR:
      context = decl_function_context (exp);



      if (context == 0)
        (((exp))->exp.operands[1]) = current_function_decl;





      if (context == current_function_decl || context == inline_function_decl)
        context = 0;


      if (context)
        {


          find_function_data (context);

          temp = (*(rtx *) &(exp)->exp.operands[2]);
          if (temp && ((enum rtx_code) (temp)->code) == REG)
            {
              put_var_into_stack (exp);
              temp = (*(rtx *) &(exp)->exp.operands[2]);
            }
          if (temp == 0 || ((enum rtx_code) (temp)->code) != MEM)
            fancy_abort ("expr.c", 6437, __FUNCTION__);
          return
            replace_equiv_address (temp,
                                   fix_lexical_addr ((((temp)->fld[0]).rtx), exp));
        }
      if ((*(rtx *) &(exp)->exp.operands[2]) == 0)
        {
          if (mode == VOIDmode)
            temp = (const_int_rtx[64]);
          else
            temp = assign_temp (build_qualified_type (type,
                                                      (((((type)->common.readonly_flag) * 0x1) | (((type)->common.volatile_flag) * 0x2) | (((type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (type)->common.code) == RECORD_TYPE && ((type)->common.type)) * 0x8))
                                                       | 0x1)),
                                3, 0, 0);

          (*(rtx *) &(exp)->exp.operands[2]) = temp;
          if (!optimize && ((enum rtx_code) (temp)->code) == REG)
            (cfun->x_save_expr_regs) = gen_rtx_fmt_ee (EXPR_LIST, (VOIDmode), (temp), ((cfun->x_save_expr_regs)));
# 6463 "expr.c"
          if (((enum rtx_code) (temp)->code) == REG && ((enum machine_mode) (temp)->mode) != mode)
            {
              temp = gen_lowpart_SUBREG (mode, (*(rtx *) &(exp)->exp.operands[2]));
              ((temp)->in_struct) = 1;
              ((temp)->unchanging) = unsignedp;
            }

          if (temp == (const_int_rtx[64]))
            expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode, 0);
          else
            store_expr (((exp)->exp.operands[0]), temp, 0);

          ((exp)->common.used_flag) = 1;
        }





      if (((enum rtx_code) ((*(rtx *) &(exp)->exp.operands[2]))->code) == REG
          && ((enum machine_mode) ((*(rtx *) &(exp)->exp.operands[2]))->mode) != mode)
        {

          promote_mode (type, mode, &unsignedp, 0);
          temp = gen_lowpart_SUBREG (mode, (*(rtx *) &(exp)->exp.operands[2]));
          ((temp)->in_struct) = 1;
          ((temp)->unchanging) = unsignedp;
          return temp;
        }

      return (*(rtx *) &(exp)->exp.operands[2]);

    case UNSAVE_EXPR:
      {
        rtx temp;
        temp = expand_expr (((exp)->exp.operands[0]), target, tmode, modifier);
        ((exp)->exp.operands[0]) = unsave_expr_now (((exp)->exp.operands[0]));
        return temp;
      }

    case PLACEHOLDER_EXPR:
      {
        tree old_list = placeholder_list;
        tree placeholder_expr = 0;

        exp = find_placeholder (exp, &placeholder_expr);
        if (exp == 0)
          fancy_abort ("expr.c", 6510, __FUNCTION__);

        placeholder_list = ((placeholder_expr)->common.chain);
        temp = expand_expr (exp, original_target, tmode, modifier);
        placeholder_list = old_list;
        return temp;
      }


      fancy_abort ("expr.c", 6519, __FUNCTION__);

    case WITH_RECORD_EXPR:


      placeholder_list = tree_cons (((exp)->exp.operands[1]), (tree) ((void *)0),
                                    placeholder_list);
      target = expand_expr (((exp)->exp.operands[0]), original_target, tmode,
                            modifier);
      placeholder_list = ((placeholder_list)->common.chain);
      return target;

    case GOTO_EXPR:
      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == LABEL_DECL)
        expand_goto (((exp)->exp.operands[0]));
      else
        expand_computed_goto (((exp)->exp.operands[0]));
      return (const_int_rtx[64]);

    case EXIT_EXPR:
      expand_exit_loop_if_false (((void *)0),
                                 invert_truthvalue (((exp)->exp.operands[0])));
      return (const_int_rtx[64]);

    case LABELED_BLOCK_EXPR:
      if ((((exp))->exp.operands[1]))
        expand_expr_stmt_value ((((exp))->exp.operands[1]), 0, 1);

      do_pending_stack_adjust ();
      emit_label (label_rtx ((((exp))->exp.operands[0])));
      return (const_int_rtx[64]);

    case EXIT_BLOCK_EXPR:
      if ((((exp))->exp.operands[1]))
        sorry ("returned value in block_exit_expr");
      expand_goto (((((((exp))->exp.operands[0])))->exp.operands[0]));
      return (const_int_rtx[64]);

    case LOOP_EXPR:
      push_temp_slots ();
      expand_start_loop (1);
      expand_expr_stmt_value (((exp)->exp.operands[0]), 0, 1);
      expand_end_loop ();
      pop_temp_slots ();

      return (const_int_rtx[64]);

    case BIND_EXPR:
      {
        tree vars = ((exp)->exp.operands[0]);
        int vars_need_expansion = 0;



        expand_start_bindings_and_block(2, (tree) ((void *)0));


        if (((exp)->exp.operands[2]) != 0
            && ! ((((exp)->exp.operands[2]))->common.used_flag))
          insert_block (((exp)->exp.operands[2]));


        while (vars)
          {
            if (!((vars)->decl.rtl != ((void *)0)))
              {
                vars_need_expansion = 1;
                expand_decl (vars);
              }
            expand_decl_init (vars);
            vars = ((vars)->common.chain);
          }

        temp = expand_expr (((exp)->exp.operands[1]), target, tmode, modifier);

        expand_end_bindings (((exp)->exp.operands[0]), 0, 0);

        return temp;
      }

    case RTL_EXPR:
      if ((*(rtx *) &(exp)->exp.operands[0]))
        {
          if ((*(rtx *) &(exp)->exp.operands[0]) == (const_int_rtx[64]))
            fancy_abort ("expr.c", 6603, __FUNCTION__);
          emit_insns ((*(rtx *) &(exp)->exp.operands[0]));
          (*(rtx *) &(exp)->exp.operands[0]) = (const_int_rtx[64]);
        }
      preserve_rtl_expr_result ((*(rtx *) &(exp)->exp.operands[1]));
      free_temps_for_rtl_expr (exp);
      return (*(rtx *) &(exp)->exp.operands[1]);

    case CONSTRUCTOR:


      if (ignore)
        {
          tree elt;

          for (elt = (((exp))->exp.operands[1]); elt; elt = ((elt)->common.chain))
            expand_expr (((elt)->list.value), (const_int_rtx[64]), VOIDmode, 0);

          return (const_int_rtx[64]);
        }
# 6631 "expr.c"
      else if ((((exp)->common.static_flag)
                && ((mode == BLKmode
                     && ! (target != 0 && safe_from_p (target, exp, 1)))
                    || ((exp)->common.addressable_flag)
                    || (host_integerp (((type)->type.size_unit), 1)
                        && (! (move_by_pieces_ninsns (tree_low_cst (((type)->type.size_unit), 1), ((type)->type.align)) < (unsigned int) (optimize_size ? 3 : ix86_cost->move_ratio)))


                        && ! mostly_zeros_p (exp))))
               || (modifier == EXPAND_INITIALIZER && ((exp)->common.constant_flag)))
        {
          rtx constructor = output_constant_def (exp, 1);

          if (modifier != EXPAND_CONST_ADDRESS
              && modifier != EXPAND_INITIALIZER
              && modifier != EXPAND_SUM)
            constructor = validize_mem (constructor);

          return constructor;
        }
      else
        {


          if (target == 0 || ! safe_from_p (target, exp, 1)
              || ((enum rtx_code) (target)->code) == PARALLEL)
            target
              = assign_temp (build_qualified_type (type,
                                                   (((((type)->common.readonly_flag) * 0x1) | (((type)->common.volatile_flag) * 0x2) | (((type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (type)->common.code) == RECORD_TYPE && ((type)->common.type)) * 0x8))
                                                    | (((exp)->common.readonly_flag)
                                                       * 0x1))),
                             0, ((exp)->common.addressable_flag), 1);

          store_constructor (exp, target, 0,
                             int_size_in_bytes (((exp)->common.type)));
          return target;
        }

    case INDIRECT_REF:
      {
        tree exp1 = ((exp)->exp.operands[0]);
        tree index;
        tree string = string_constant (exp1, &index);


        if (string
            && ((enum tree_code) (string)->common.code) == STRING_CST
            && ((enum tree_code) (index)->common.code) == INTEGER_CST
            && compare_tree_int (index, ((string)->string.length)) < 0
            && (mode_class[(int) (mode)]) == MODE_INT
            && (mode_size[(int) (mode)]) == 1
            && modifier != EXPAND_WRITE)
          return
            gen_rtx_CONST_INT (VOIDmode, (long long) (trunc_int_for_mode (((string)->string.pointer) [(((index)->int_cst.int_cst).low)], mode)));


        op0 = expand_expr (exp1, (rtx) 0, VOIDmode, EXPAND_SUM);
        op0 = memory_address (mode, op0);
        temp = gen_rtx_MEM (mode, op0);
        set_mem_attributes (temp, exp, 0);




        if (modifier == EXPAND_WRITE && readonly_fields_p (type))
          ((temp)->unchanging) = 1;

        return temp;
      }

    case ARRAY_REF:
      if (((enum tree_code) (((((exp)->exp.operands[0]))->common.type))->common.code) != ARRAY_TYPE)
        fancy_abort ("expr.c", 6703, __FUNCTION__);

      {
        tree array = ((exp)->exp.operands[0]);
        tree domain = ((((array)->common.type))->type.values);
        tree low_bound = domain ? ((domain)->type.minval) : global_trees[TI_INTEGER_ZERO];
        tree index = convert (sizetype_tab[(int) SIZETYPE], ((exp)->exp.operands[1]));
        long long i;
# 6720 "expr.c"
        if (! integer_zerop (low_bound))
          index = size_diffop (index, convert (sizetype_tab[(int) SIZETYPE], low_bound));






        if (modifier != EXPAND_CONST_ADDRESS && modifier != EXPAND_INITIALIZER
            && ((enum tree_code) (array)->common.code) == STRING_CST
            && ((enum tree_code) (index)->common.code) == INTEGER_CST
            && compare_tree_int (index, ((array)->string.length)) < 0
            && (mode_class[(int) (mode)]) == MODE_INT
            && (mode_size[(int) (mode)]) == 1)
          return
            gen_rtx_CONST_INT (VOIDmode, (long long) (trunc_int_for_mode (((array)->string.pointer) [(((index)->int_cst.int_cst).low)], mode)));







        if (modifier != EXPAND_CONST_ADDRESS && modifier != EXPAND_INITIALIZER
            && ((enum tree_code) (array)->common.code) == CONSTRUCTOR && ! ((array)->common.side_effects_flag)
            && ((enum tree_code) (index)->common.code) == INTEGER_CST
            && 0 > compare_tree_int (index,
                                     list_length ((((((exp)->exp.operands[0])))->exp.operands[1]))))

          {
            tree elem;

            for (elem = (((((exp)->exp.operands[0])))->exp.operands[1]),
                 i = (((index)->int_cst.int_cst).low);
                 elem != 0 && i != 0; i--, elem = ((elem)->common.chain))
              ;

            if (elem)
              return expand_expr (fold (((elem)->list.value)), target, tmode,
                                  modifier);
          }

        else if (optimize >= 1
                 && modifier != EXPAND_CONST_ADDRESS
                 && modifier != EXPAND_INITIALIZER
                 && ((array)->common.readonly_flag) && ! ((array)->common.side_effects_flag)
                 && ((enum tree_code) (array)->common.code) == VAR_DECL && ((array)->decl.initial)
                 && ((enum tree_code) (((array)->decl.initial))->common.code) != ERROR_MARK)
          {
            if (((enum tree_code) (index)->common.code) == INTEGER_CST)
              {
                tree init = ((array)->decl.initial);

                if (((enum tree_code) (init)->common.code) == CONSTRUCTOR)
                  {
                    tree elem;

                    for (elem = (((init))->exp.operands[1]);
                         (elem
                          && !tree_int_cst_equal (((elem)->list.purpose), index));
                         elem = ((elem)->common.chain))
                      ;

                    if (elem && !((((elem)->list.value))->common.side_effects_flag))
                      return expand_expr (fold (((elem)->list.value)), target,
                                          tmode, modifier);
                  }
                else if (((enum tree_code) (init)->common.code) == STRING_CST
                         && 0 > compare_tree_int (index,
                                                  ((init)->string.length)))
                  {
                    tree type = ((((init)->common.type))->common.type);
                    enum machine_mode mode = ((type)->type.mode);

                    if ((mode_class[(int) (mode)]) == MODE_INT
                        && (mode_size[(int) (mode)]) == 1)
                      return gen_rtx_CONST_INT (VOIDmode, (long long) (trunc_int_for_mode (((init)->string.pointer) [(((index)->int_cst.int_cst).low)], mode)));


                  }
              }
          }
      }


    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_RANGE_REF:




      if (code == COMPONENT_REF
          && ((enum tree_code) (((exp)->exp.operands[0]))->common.code) == CONSTRUCTOR
          && ((((exp)->exp.operands[0]))->real_cst.rtl) == 0)
        {
          tree elt;

          for (elt = (((((exp)->exp.operands[0])))->exp.operands[1]); elt;
               elt = ((elt)->common.chain))
            if (((elt)->list.purpose) == ((exp)->exp.operands[1])







                && (! ((((elt)->list.purpose))->decl.bit_field_flag)
                    || (((mode_class[(int) (((((elt)->list.purpose))->decl.mode))])
                         == MODE_INT)
                        && ((mode_bitsize[(int) (((((elt)->list.purpose))->decl.mode))])
                            <= (8 * 8)))))
              {
                op0 = expand_expr (((elt)->list.value), target, tmode, modifier);
                if (((((elt)->list.purpose))->decl.bit_field_flag))
                  {
                    long long bitsize
                      = (((((((elt)->list.purpose))->decl.size))->int_cst.int_cst).low);
                    enum machine_mode imode
                      = ((((((elt)->list.purpose))->common.type))->type.mode);

                    if (((((((elt)->list.purpose))->common.type))->common.unsigned_flag))
                      {
                        op1 = gen_rtx_CONST_INT (VOIDmode, (long long) (((long long) 1 << bitsize) - 1));
                        op0 = expand_and (imode, op0, op1, target);
                      }
                    else
                      {
                        tree count
                          = build_int_2_wide ((unsigned long long) ((mode_bitsize[(int) (imode)]) - bitsize), (long long) (0));


                        op0 = expand_shift (LSHIFT_EXPR, imode, op0, count,
                                            target, 0);
                        op0 = expand_shift (RSHIFT_EXPR, imode, op0, count,
                                            target, 0);
                      }
                  }

                return op0;
              }
        }

      {
        enum machine_mode mode1;
        long long bitsize, bitpos;
        tree offset;
        int volatilep = 0;
        tree tem = get_inner_reference (exp, &bitsize, &bitpos, &offset,
                                        &mode1, &unsignedp, &volatilep);
        rtx orig_op0;




        if (tem == exp)
          fancy_abort ("expr.c", 6877, __FUNCTION__);





        orig_op0 = op0
          = expand_expr (tem,
                         (((enum tree_code) (((tem)->common.type))->common.code) == UNION_TYPE
                          && (((enum tree_code) (((((tem)->common.type))->type.size))->common.code)
                              != INTEGER_CST)
                          ? target : (rtx) 0),
                         VOIDmode,
                         (modifier == EXPAND_INITIALIZER
                          || modifier == EXPAND_CONST_ADDRESS)
                         ? modifier : EXPAND_NORMAL);



        if ((((enum rtx_code) (op0)->code) == LABEL_REF || ((enum rtx_code) (op0)->code) == SYMBOL_REF || ((enum rtx_code) (op0)->code) == CONST_INT || ((enum rtx_code) (op0)->code) == CONST_DOUBLE || ((enum rtx_code) (op0)->code) == CONST || ((enum rtx_code) (op0)->code) == HIGH || ((enum rtx_code) (op0)->code) == CONST_VECTOR || ((enum rtx_code) (op0)->code) == CONSTANT_P_RTX))
          {
            enum machine_mode mode = ((((tem)->common.type))->type.mode);
            if (mode != BLKmode && 1
                && offset == 0)
              op0 = force_reg (mode, op0);
            else
              op0 = validize_mem (force_const_mem (mode, op0));
          }

        if (offset != 0)
          {
            rtx offset_rtx = expand_expr (offset, (rtx) 0, VOIDmode, EXPAND_SUM);





            if (((enum rtx_code) (op0)->code) == REG || ((enum rtx_code) (op0)->code) == SUBREG
                || ((enum rtx_code) (op0)->code) == CONCAT || ((enum rtx_code) (op0)->code) == ADDRESSOF)
              {


                if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == SAVE_EXPR)
                  {
                    put_var_into_stack (((exp)->exp.operands[0]));
                    op0 = (*(rtx *) &(((exp)->exp.operands[0]))->exp.operands[2]);
                  }
                else
                  {
                    tree nt
                      = build_qualified_type (((tem)->common.type),
                                              (((((((tem)->common.type))->common.readonly_flag) * 0x1) | (((((tem)->common.type))->common.volatile_flag) * 0x2) | (((((tem)->common.type))->type.restrict_flag) * 0x4) | ((((enum tree_code) (((tem)->common.type))->common.code) == RECORD_TYPE && ((((tem)->common.type))->common.type)) * 0x8))
                                               | 0x1));
                    rtx memloc = assign_temp (nt, 1, 1, 1);

                    emit_move_insn (memloc, op0);
                    op0 = memloc;
                  }
              }

            if (((enum rtx_code) (op0)->code) != MEM)
              fancy_abort ("expr.c", 6938, __FUNCTION__);





            if (((enum machine_mode) (offset_rtx)->mode) != ptr_mode)
              offset_rtx = convert_to_mode (ptr_mode, offset_rtx, 0);




            if (((enum rtx_code) (op0)->code) == MEM
                && ((enum machine_mode) (op0)->mode) == BLKmode
                && ((enum machine_mode) ((((op0)->fld[0]).rtx))->mode) != VOIDmode
                && bitsize != 0
                && (bitpos % bitsize) == 0
                && (bitsize % get_mode_alignment (mode1)) == 0
                && ((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8)) == get_mode_alignment (mode1))
              {
                op0 = adjust_address_1 (op0, mode1, bitpos / 8, 1, 1);
                bitpos = 0;
              }

            op0 = offset_address (op0, offset_rtx,
                                  highest_pow2_factor (offset));
          }



        if (((enum rtx_code) (op0)->code) == MEM && bitpos == 0 && offset != 0
            && is_aligning_offset (offset, tem))
          set_mem_align (op0, 128);


        if (((enum rtx_code) (op0)->code) == MEM && volatilep && ! ((op0)->volatil))
          {
            if (op0 == orig_op0)
              op0 = copy_rtx (op0);

            ((op0)->volatil) = 1;
          }




        if (((enum rtx_code) (op0)->code) == CONCAT)
          {
            if (bitpos != 0 || bitsize != (mode_bitsize[(int) (((enum machine_mode) (op0)->mode))]))
              fancy_abort ("expr.c", 6987, __FUNCTION__);
            return op0;
          }







        if (mode1 == VOIDmode
            || ((enum rtx_code) (op0)->code) == REG || ((enum rtx_code) (op0)->code) == SUBREG
            || (mode1 != BLKmode && ! direct_load[(int) mode1]
                && (mode_class[(int) (mode)]) != MODE_COMPLEX_INT
                && (mode_class[(int) (mode)]) != MODE_COMPLEX_FLOAT
                && modifier != EXPAND_CONST_ADDRESS
                && modifier != EXPAND_INITIALIZER)


            || (mode1 != BLKmode
                && 0
                && ((((((tem)->common.type))->type.align)
                     < get_mode_alignment (mode))
                    || (bitpos % get_mode_alignment (mode) != 0)))



            || (bitsize >= 0
                && (((enum tree_code) (((((exp)->common.type))->type.size))->common.code)
                    == INTEGER_CST)
                && 0 != compare_tree_int (((((exp)->common.type))->type.size),
                                          bitsize)))
          {
            enum machine_mode ext_mode = mode;

            if (ext_mode == BLKmode
                && ! (target != 0 && ((enum rtx_code) (op0)->code) == MEM
                      && ((enum rtx_code) (target)->code) == MEM
                      && bitpos % 8 == 0))
              ext_mode = mode_for_size (bitsize, MODE_INT, 1);

            if (ext_mode == BLKmode)
              {


                if (((enum rtx_code) (op0)->code) != MEM
                    || (target != 0 && ((enum rtx_code) (target)->code) != MEM)
                    || bitpos % 8 != 0)
                  fancy_abort ("expr.c", 7035, __FUNCTION__);

                op0 = adjust_address_1 (op0, VOIDmode, bitpos / 8, 1, 1);
                if (target == 0)
                  target = assign_temp (type, 0, 1, 1);

                emit_block_move (target, op0,
                                 gen_rtx_CONST_INT (VOIDmode, (long long) ((bitsize + 8 - 1) / 8)));


                return target;
              }

            op0 = validize_mem (op0);

            if (((enum rtx_code) (op0)->code) == MEM && ((enum rtx_code) ((((op0)->fld[0]).rtx))->code) == REG)
              mark_reg_pointer ((((op0)->fld[0]).rtx), ((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8)));

            op0 = extract_bit_field (op0, bitsize, bitpos,
                                     unsignedp, target, ext_mode, ext_mode,
                                     int_size_in_bytes (((tem)->common.type)));




            if (((enum tree_code) (type)->common.code) == RECORD_TYPE && 0
                && (mode_class[(int) (((enum machine_mode) (op0)->mode))]) == MODE_INT
                && bitsize < (long long) (mode_bitsize[(int) (((enum machine_mode) (op0)->mode))]))
              op0 = expand_shift (LSHIFT_EXPR, ((enum machine_mode) (op0)->mode), op0,
                                  size_int_wide ((long long) ((mode_bitsize[(int) (((enum machine_mode) (op0)->mode))]) - bitsize), SIZETYPE),

                                  op0, 1);

            if (mode == BLKmode)
              {
                rtx new = assign_temp (build_qualified_type
                                       (type_for_mode (ext_mode, 0),
                                        0x1), 0, 1, 1);

                emit_move_insn (new, op0);
                op0 = copy_rtx (new);
                ((op0)->mode = (enum machine_mode) (BLKmode));
                set_mem_attributes (op0, exp, 1);
              }

            return op0;
          }



        if (mode == BLKmode)
          mode1 = BLKmode;


        if (modifier == EXPAND_CONST_ADDRESS
            || modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
          op0 = adjust_address_1 (op0, mode1, bitpos / 8, 0, 1);
        else
          op0 = adjust_address_1 (op0, mode1, bitpos / 8, 1, 1);

        if (op0 == orig_op0)
          op0 = copy_rtx (op0);

        set_mem_attributes (op0, exp, 0);
        if (((enum rtx_code) ((((op0)->fld[0]).rtx))->code) == REG)
          mark_reg_pointer ((((op0)->fld[0]).rtx), ((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8)));

        ((op0)->volatil) |= volatilep;
        if (mode == mode1 || mode1 == BLKmode || mode1 == tmode
            || modifier == EXPAND_CONST_ADDRESS
            || modifier == EXPAND_INITIALIZER)
          return op0;
        else if (target == 0)
          target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);

        convert_move (target, op0, unsignedp);
        return target;
      }

    case VTABLE_REF:
      {
        rtx insn, before = get_last_insn (), vtbl_ref;


        subtarget = expand_expr (((exp)->exp.operands[0]), target,
                                 tmode, modifier);


        if ((((enum rtx_code) (subtarget)->code) == REG))
          {
            target = subtarget;
            insn = get_last_insn ();
            if (insn == before)
              fancy_abort ("expr.c", 7128, __FUNCTION__);
            if (! ((rtx_class[(int) (((enum rtx_code) (insn)->code))]) == 'i'))
              insn = prev_nonnote_insn (insn);
          }
        else
          {
            target = gen_reg_rtx (((enum machine_mode) (subtarget)->mode));
            insn = emit_move_insn (target, subtarget);
          }


        vtbl_ref = (((((((exp)->exp.operands[1]))->decl.rtl ? (((exp)->exp.operands[1]))->decl.rtl : (make_decl_rtl (((exp)->exp.operands[1]), ((void *)0)), (((exp)->exp.operands[1]))->decl.rtl)))->fld[0]).rtx);
        vtbl_ref = plus_constant_wide ((vtbl_ref), (long long) (tree_low_cst (((exp)->exp.operands[2]), 0)));


        vtbl_ref = (((vtbl_ref)->fld[0]).rtx);

        (((insn)->fld[6]).rtx)
          = gen_rtx_fmt_ee (EXPR_LIST, (REG_VTABLE_REF), (vtbl_ref), ((((insn)->fld[6]).rtx)));

        return target;
      }




    case BUFFER_REF:
      fancy_abort ("expr.c", 7155, __FUNCTION__);

    case IN_EXPR:
      {
# 7168 "expr.c"
        tree set = ((exp)->exp.operands[0]);
        tree index = ((exp)->exp.operands[1]);
        int iunsignedp = ((((index)->common.type))->common.unsigned_flag);
        tree set_type = ((set)->common.type);
        tree set_low_bound = ((((set_type)->type.values))->type.minval);
        tree set_high_bound = ((((set_type)->type.values))->type.maxval);
        rtx index_val = expand_expr (index, 0, VOIDmode, 0);
        rtx lo_r = expand_expr (set_low_bound, 0, VOIDmode, 0);
        rtx hi_r = expand_expr (set_high_bound, 0, VOIDmode, 0);
        rtx setval = expand_expr (set, 0, VOIDmode, 0);
        rtx setaddr = (((setval)->fld[0]).rtx);
        enum machine_mode index_mode = ((((index)->common.type))->type.mode);
        rtx rlow;
        rtx diff, quo, rem, addr, bit, result;



        if (((((enum tree_code) (set_high_bound)->common.code) == INTEGER_CST
             && ((enum tree_code) (set_low_bound)->common.code) == INTEGER_CST
             && tree_int_cst_lt (set_high_bound, set_low_bound))
             || (((enum tree_code) (index)->common.code) == INTEGER_CST
                 && ((enum tree_code) (set_low_bound)->common.code) == INTEGER_CST
                 && tree_int_cst_lt (index, set_low_bound))
             || (((enum tree_code) (set_high_bound)->common.code) == INTEGER_CST
                 && ((enum tree_code) (index)->common.code) == INTEGER_CST
                 && tree_int_cst_lt (set_high_bound, index))))
          return (const_int_rtx[64]);

        if (target == 0)
          target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);




        op0 = gen_label_rtx ();
        op1 = gen_label_rtx ();

        if (! (((enum rtx_code) (index_val)->code) == CONST_INT
               && ((enum rtx_code) (lo_r)->code) == CONST_INT))
          emit_cmp_and_jump_insns (index_val, lo_r, LT, (rtx) 0,
                                   ((enum machine_mode) (index_val)->mode), iunsignedp, op1);

        if (! (((enum rtx_code) (index_val)->code) == CONST_INT
               && ((enum rtx_code) (hi_r)->code) == CONST_INT))
          emit_cmp_and_jump_insns (index_val, hi_r, GT, (rtx) 0,
                                   ((enum machine_mode) (index_val)->mode), iunsignedp, op1);



        if (((enum rtx_code) (lo_r)->code) == CONST_INT)
          rlow = gen_rtx_CONST_INT (VOIDmode, (long long) ((((lo_r)->fld[0]).rtwint) & ~((long long) 1 << 8)));

        else
          rlow = expand_binop (index_mode, (optab_table[OTI_and]), lo_r,
                               gen_rtx_CONST_INT (VOIDmode, (long long) (~((long long) 1 << 8))),
                               (rtx) 0, iunsignedp, OPTAB_LIB_WIDEN);

        diff = expand_binop (index_mode, (optab_table[OTI_sub]), index_val, rlow,
                             (rtx) 0, iunsignedp, OPTAB_LIB_WIDEN);

        quo = expand_divmod (0, TRUNC_DIV_EXPR, index_mode, diff,
                             gen_rtx_CONST_INT (VOIDmode, (long long) (8)), (rtx) 0, iunsignedp);
        rem = expand_divmod (1, TRUNC_MOD_EXPR, index_mode, index_val,
                             gen_rtx_CONST_INT (VOIDmode, (long long) (8)), (rtx) 0, iunsignedp);

        addr = memory_address (byte_mode,
                               expand_binop (index_mode, (optab_table[OTI_add]), diff,
                                             setaddr, (rtx) 0, iunsignedp,
                                             OPTAB_LIB_WIDEN));


        bit = expand_shift (RSHIFT_EXPR, byte_mode,
                            gen_rtx_MEM (byte_mode, addr),
                            make_tree (((index)->common.type), rem),
                            (rtx) 0, 1);
        result = expand_binop (byte_mode, (optab_table[OTI_and]), bit, (const_int_rtx[64 +1]),
                               ((enum machine_mode) (target)->mode) == byte_mode ? target : 0,
                               1, OPTAB_LIB_WIDEN);

        if (result != target)
          convert_move (target, result, 1);


        emit_jump (op0);
        emit_label (op1);
        emit_move_insn (target, (const_int_rtx[64]));
        emit_label (op0);
        return target;
      }

    case WITH_CLEANUP_EXPR:
      if ((*(rtx *) &(exp)->exp.operands[2]) == 0)
        {
          (*(rtx *) &(exp)->exp.operands[2])
            = expand_expr (((exp)->exp.operands[0]), target, tmode, modifier);
          expand_decl_cleanup ((tree) ((void *)0), ((exp)->exp.operands[1]));


          ((exp)->exp.operands[1]) = 0;
        }
      return (*(rtx *) &(exp)->exp.operands[2]);

    case CLEANUP_POINT_EXPR:
      {


        expand_start_bindings_and_block(2, (tree) ((void *)0));

        (cfun->x_target_temp_slot_level) = (cfun->x_temp_slot_level);

        op0 = expand_expr (((exp)->exp.operands[0]), target, tmode, modifier);

        if (! ignore)
          op0 = force_not_mem (op0);
        preserve_temp_slots (op0);
        expand_end_bindings ((tree) ((void *)0), 0, 0);
      }
      return op0;

    case CALL_EXPR:

      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == ADDR_EXPR
          && (((enum tree_code) (((((exp)->exp.operands[0]))->exp.operands[0]))->common.code)
              == FUNCTION_DECL)
          && (((((((exp)->exp.operands[0]))->exp.operands[0]))->decl.built_in_class) != NOT_BUILT_IN))
        {
          if (((((((exp)->exp.operands[0]))->exp.operands[0]))->decl.built_in_class)
              == BUILT_IN_FRONTEND)
            return (*lang_expand_expr) (exp, original_target, tmode, modifier);
          else
            return expand_builtin (exp, target, subtarget, tmode, ignore);
        }

      return expand_call (exp, target, ignore);

    case NON_LVALUE_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case REFERENCE_EXPR:
      if (((exp)->exp.operands[0]) == global_trees[TI_ERROR_MARK])
        return (const_int_rtx[64]);

      if (((enum tree_code) (type)->common.code) == UNION_TYPE)
        {
          tree valtype = ((((exp)->exp.operands[0]))->common.type);



          if (mode == BLKmode && ((valtype)->type.mode) == BLKmode)
            {
              rtx result = expand_expr (((exp)->exp.operands[0]), target, tmode,
                                        modifier);

              result = copy_rtx (result);
              set_mem_attributes (result, exp, 0);
              return result;
            }

          if (target == 0)
            target = assign_temp (type, 0, 1, 1);

          if (((enum rtx_code) (target)->code) == MEM)

            store_expr (((exp)->exp.operands[0]),
                        adjust_address_1 (target, ((valtype)->type.mode), 0, 1, 1), 0);

          else if (((enum rtx_code) (target)->code) == REG)

            store_field (target,
                         (((int_size_in_bytes (((((exp)->exp.operands[0]))->common.type)) * 8)) < ((long long) (mode_bitsize[(int) (mode)])) ? ((int_size_in_bytes (((((exp)->exp.operands[0]))->common.type)) * 8)) : ((long long) (mode_bitsize[(int) (mode)]))),



                         0, ((valtype)->type.mode), ((exp)->exp.operands[0]),
                         VOIDmode, 0, type, 0);
          else
            fancy_abort ("expr.c", 7344, __FUNCTION__);


          return target;
        }

      if (mode == ((((((exp)->exp.operands[0]))->common.type))->type.mode))
        {
          op0 = expand_expr (((exp)->exp.operands[0]), target, VOIDmode,
                             modifier);




          if (((((((exp)->exp.operands[0]))->common.type))->common.unsigned_flag) != unsignedp
              && ((enum rtx_code) (op0)->code) == SUBREG)
            ((op0)->in_struct) = 0;

          return op0;
        }

      op0 = expand_expr (((exp)->exp.operands[0]), (rtx) 0, mode, modifier);
      if (((enum machine_mode) (op0)->mode) == mode)
        return op0;


      if ((((enum rtx_code) (op0)->code) == LABEL_REF || ((enum rtx_code) (op0)->code) == SYMBOL_REF || ((enum rtx_code) (op0)->code) == CONST_INT || ((enum rtx_code) (op0)->code) == CONST_DOUBLE || ((enum rtx_code) (op0)->code) == CONST || ((enum rtx_code) (op0)->code) == HIGH || ((enum rtx_code) (op0)->code) == CONST_VECTOR || ((enum rtx_code) (op0)->code) == CONSTANT_P_RTX))
        {
          tree inner_type = ((((exp)->exp.operands[0]))->common.type);
          enum machine_mode inner_mode = ((inner_type)->type.mode);

          if (modifier == EXPAND_INITIALIZER)
            return simplify_gen_subreg (mode, op0, inner_mode,
                                        subreg_lowpart_offset (mode,
                                                               inner_mode));
          else
            return convert_modes (mode, inner_mode, op0,
                                  ((inner_type)->common.unsigned_flag));
        }

      if (modifier == EXPAND_INITIALIZER)
        return gen_rtx_fmt_e (unsignedp ? ZERO_EXTEND : SIGN_EXTEND, mode, op0);

      if (target == 0)
        return
          convert_to_mode (mode, op0,
                           ((((((exp)->exp.operands[0]))->common.type))->common.unsigned_flag));
      else
        convert_move (target, op0,
                      ((((((exp)->exp.operands[0]))->common.type))->common.unsigned_flag));
      return target;

    case VIEW_CONVERT_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), (rtx) 0, mode, modifier);





      if (((type)->type.mode) == ((enum machine_mode) (op0)->mode))
        ;
      else if (((type)->type.mode) != BLKmode && ((enum machine_mode) (op0)->mode) != BLKmode
               && (mode_size[(int) (((type)->type.mode))]) <= ((target_flags & 0x02000000) ? 8 : 4)
               && (mode_size[(int) (((enum machine_mode) (op0)->mode))]) <= ((target_flags & 0x02000000) ? 8 : 4))
        op0 = gen_lowpart (((type)->type.mode), op0);
      else if (((enum rtx_code) (op0)->code) != MEM)
        {




          tree inner_type = ((((exp)->exp.operands[0]))->common.type);

          if (((exp)->common.addressable_flag))
            fancy_abort ("expr.c", 7418, __FUNCTION__);

          if (target == 0 || ((enum machine_mode) (target)->mode) != ((inner_type)->type.mode))
            target
              = assign_stack_temp_for_type
                (((inner_type)->type.mode),
                 (mode_size[(int) (((inner_type)->type.mode))]), 0, inner_type);

          emit_move_insn (target, op0);
          op0 = target;
        }





      if (((enum rtx_code) (op0)->code) == MEM)
        {
          op0 = copy_rtx (op0);

          if (((type)->common.nothrow_flag))
            set_mem_align (op0, ((((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8))) > (((type)->type.align)) ? (((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8))) : (((type)->type.align))));
          else if (((type)->type.mode) != BLKmode && 0
                   && ((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8)) < get_mode_alignment (((type)->type.mode)))
            {
              tree inner_type = ((((exp)->exp.operands[0]))->common.type);
              long long temp_size
                = ((int_size_in_bytes (inner_type)) > ((long long) (mode_size[(int) (((type)->type.mode))])) ? (int_size_in_bytes (inner_type)) : ((long long) (mode_size[(int) (((type)->type.mode))])));

              rtx new = assign_stack_temp_for_type (((type)->type.mode),
                                                    temp_size, 0, type);
              rtx new_with_op0_mode = adjust_address_1 (new, ((enum machine_mode) (op0)->mode), 0, 1, 1);

              if (((exp)->common.addressable_flag))
                fancy_abort ("expr.c", 7452, __FUNCTION__);

              if (((enum machine_mode) (op0)->mode) == BLKmode)
                emit_block_move (new_with_op0_mode, op0,
                                 gen_rtx_CONST_INT (VOIDmode, (long long) ((mode_size[(int) (((type)->type.mode))]))));
              else
                emit_move_insn (new_with_op0_mode, op0);

              op0 = new;
            }

          op0 = adjust_address_1 (op0, ((type)->type.mode), 0, 1, 1);
        }

      return op0;

    case PLUS_EXPR:


    plus_expr:
      this_optab = ! unsignedp && flag_trapv
                   && ((mode_class[(int) (mode)]) == MODE_INT)
                   ? (optab_table[OTI_addv]) : (optab_table[OTI_add]);
# 7487 "expr.c"
      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == PLUS_EXPR
          && ((enum tree_code) (((((exp)->exp.operands[0]))->exp.operands[1]))->common.code) == INTEGER_CST
          && ((enum tree_code) (((exp)->exp.operands[1]))->common.code) == RTL_EXPR
          && ((*(rtx *) &(((exp)->exp.operands[1]))->exp.operands[1]) == (global_rtl[GR_FRAME_POINTER])
              || (*(rtx *) &(((exp)->exp.operands[1]))->exp.operands[1]) == (global_rtl[GR_STACK_POINTER])
              || (*(rtx *) &(((exp)->exp.operands[1]))->exp.operands[1]) == (global_rtl[GR_ARG_POINTER])))
        {
          tree t = ((exp)->exp.operands[1]);

          ((exp)->exp.operands[1]) = ((((exp)->exp.operands[0]))->exp.operands[0]);
          ((((exp)->exp.operands[0]))->exp.operands[0]) = t;
        }
# 7508 "expr.c"
      if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER
          || (mode == ptr_mode && (unsignedp || ! flag_trapv)))
        {
          if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == INTEGER_CST
              && (mode_bitsize[(int) (mode)]) <= (8 * 8)
              && ((((exp)->exp.operands[1]))->common.constant_flag))
            {
              rtx constant_part;

              op1 = expand_expr (((exp)->exp.operands[1]), subtarget, VOIDmode,
                                 EXPAND_SUM);




              constant_part
                = immed_double_const ((((((exp)->exp.operands[0]))->int_cst.int_cst).low),
                                      (long long) 0,
                                      ((((((exp)->exp.operands[1]))->common.type))->type.mode));
              op1 = plus_constant_wide ((op1), (long long) ((((constant_part)->fld[0]).rtwint)));
              if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
                op1 = force_operand (op1, target);
              return op1;
            }

          else if (((enum tree_code) (((exp)->exp.operands[1]))->common.code) == INTEGER_CST
                   && (mode_bitsize[(int) (mode)]) <= (8 * 4)
                   && ((((exp)->exp.operands[0]))->common.constant_flag))
            {
              rtx constant_part;

              op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode,
                                 (modifier == EXPAND_INITIALIZER
                                 ? EXPAND_INITIALIZER : EXPAND_SUM));
              if (! (((enum rtx_code) (op0)->code) == LABEL_REF || ((enum rtx_code) (op0)->code) == SYMBOL_REF || ((enum rtx_code) (op0)->code) == CONST_INT || ((enum rtx_code) (op0)->code) == CONST_DOUBLE || ((enum rtx_code) (op0)->code) == CONST || ((enum rtx_code) (op0)->code) == HIGH || ((enum rtx_code) (op0)->code) == CONST_VECTOR || ((enum rtx_code) (op0)->code) == CONSTANT_P_RTX))
                {
                  op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0,
                                     VOIDmode, modifier);


                  if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
                    goto binop2;
                  goto both_summands;
                }




              constant_part
                = immed_double_const ((((((exp)->exp.operands[1]))->int_cst.int_cst).low),
                                      (long long) 0,
                                      ((((((exp)->exp.operands[0]))->common.type))->type.mode));
              op0 = plus_constant_wide ((op0), (long long) ((((constant_part)->fld[0]).rtwint)));
              if (modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
                op0 = force_operand (op0, target);
              return op0;
            }
        }





      if ((modifier != EXPAND_SUM && modifier != EXPAND_INITIALIZER)
          || mode != ptr_mode)
        goto binop;

      if (! safe_from_p (subtarget, ((exp)->exp.operands[1]), 1))
        subtarget = 0;

      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, modifier);
      op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, modifier);

    both_summands:

      if (((enum rtx_code) (op0)->code) == PLUS
          && (((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == LABEL_REF || ((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == SYMBOL_REF || ((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == CONST_INT || ((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == CONST_DOUBLE || ((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == CONST || ((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == HIGH || ((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == CONST_VECTOR || ((enum rtx_code) ((((op0)->fld[1]).rtx))->code) == CONSTANT_P_RTX))
        {
          temp = op0;
          op0 = op1;
          op1 = temp;
        }


      if (((enum rtx_code) (op1)->code) == PLUS
          && (((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == LABEL_REF || ((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == SYMBOL_REF || ((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == CONST_INT || ((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == CONST_DOUBLE || ((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == CONST || ((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == HIGH || ((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == CONST_VECTOR || ((enum rtx_code) ((((op1)->fld[1]).rtx))->code) == CONSTANT_P_RTX))
        {
          rtx constant_term = (const_int_rtx[64]);

          temp = simplify_binary_operation (PLUS, mode, (((op1)->fld[0]).rtx), op0);
          if (temp != 0)
            op0 = temp;

          else if (((enum rtx_code) (op0)->code) == MULT)
            op0 = gen_rtx_fmt_ee (PLUS, (mode), (op0), ((((op1)->fld[0]).rtx)));
          else
            op0 = gen_rtx_fmt_ee (PLUS, (mode), ((((op1)->fld[0]).rtx)), (op0));


          op0 = eliminate_constant_term (op0, &constant_term);





          temp = simplify_binary_operation (PLUS, mode, constant_term,
                                            (((op1)->fld[1]).rtx));
          if (temp != 0)
            op1 = temp;
          else
            op1 = gen_rtx_fmt_ee (PLUS, (mode), (constant_term), ((((op1)->fld[1]).rtx)));
        }


      if ((((enum rtx_code) (op0)->code) == LABEL_REF || ((enum rtx_code) (op0)->code) == SYMBOL_REF || ((enum rtx_code) (op0)->code) == CONST_INT || ((enum rtx_code) (op0)->code) == CONST_DOUBLE || ((enum rtx_code) (op0)->code) == CONST || ((enum rtx_code) (op0)->code) == HIGH || ((enum rtx_code) (op0)->code) == CONST_VECTOR || ((enum rtx_code) (op0)->code) == CONSTANT_P_RTX) || ((enum rtx_code) (op1)->code) == MULT)
        temp = op1, op1 = op0, op0 = temp;

      temp = simplify_binary_operation (PLUS, mode, op0, op1);
      return temp ? temp : gen_rtx_fmt_ee (PLUS, (mode), (op0), (op1));

    case MINUS_EXPR:





      if ((modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
          && really_constant_p (((exp)->exp.operands[0]))
          && really_constant_p (((exp)->exp.operands[1])))
        {
          rtx op0 = expand_expr (((exp)->exp.operands[0]), (rtx) 0, VOIDmode,
                                 modifier);
          rtx op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode,
                                 modifier);



          if (((enum rtx_code) (op1)->code) == CONST_INT)
            return plus_constant_wide ((op0), (long long) (- (((op1)->fld[0]).rtwint)));
          else
            return gen_rtx_fmt_ee (MINUS, (mode), (op0), (op1));
        }

      if (((enum tree_code) (((exp)->exp.operands[1]))->common.code) == INTEGER_CST)
        {
          tree negated = fold (build1 (NEGATE_EXPR, type,
                                       ((exp)->exp.operands[1])));

          if (((type)->common.unsigned_flag) || ((negated)->common.public_flag))



                                    ;
          else
            {
              exp = build (PLUS_EXPR, type, ((exp)->exp.operands[0]), negated);
              goto plus_expr;
            }
        }
      this_optab = ! unsignedp && flag_trapv
                   && ((mode_class[(int) (mode)]) == MODE_INT)
                   ? (optab_table[OTI_subv]) : (optab_table[OTI_sub]);
      goto binop;

    case MULT_EXPR:



      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == INTEGER_CST)
        {
          tree t1 = ((exp)->exp.operands[0]);
          ((exp)->exp.operands[0]) = ((exp)->exp.operands[1]);
          ((exp)->exp.operands[1]) = t1;
        }




      if (modifier == EXPAND_SUM && mode == ptr_mode
          && host_integerp (((exp)->exp.operands[1]), 0))
        {
          op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode,
                             EXPAND_SUM);
# 7702 "expr.c"
          if (((enum rtx_code) (op0)->code) != REG)
            op0 = force_operand (op0, (rtx) 0);
          if (((enum rtx_code) (op0)->code) != REG)
            op0 = copy_to_mode_reg (mode, op0);

          return
            gen_rtx_fmt_ee (MULT, (mode), (op0), (gen_rtx_CONST_INT (VOIDmode, (long long) (tree_low_cst (((exp)->exp.operands[1]), 0)))));

        }

      if (! safe_from_p (subtarget, ((exp)->exp.operands[1]), 1))
        subtarget = 0;





      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == NOP_EXPR
          && ((enum tree_code) (type)->common.code) == INTEGER_TYPE
          && (((((((((exp)->exp.operands[0]))->exp.operands[0]))->common.type))->type.precision)
              < ((((((exp)->exp.operands[0]))->common.type))->type.precision))
          && ((((enum tree_code) (((exp)->exp.operands[1]))->common.code) == INTEGER_CST
               && int_fits_type_p (((exp)->exp.operands[1]),
                                   ((((((exp)->exp.operands[0]))->exp.operands[0]))->common.type))

               && (((mode_bitsize[(int) (((((((exp)->exp.operands[1]))->common.type))->type.mode))])
                    > (8 * 8))
                   || exact_log2_wide ((unsigned long long) ((((((exp)->exp.operands[1]))->int_cst.int_cst).low))) < 0))
              ||
              (((enum tree_code) (((exp)->exp.operands[1]))->common.code) == NOP_EXPR
               && (((((((((exp)->exp.operands[1]))->exp.operands[0]))->common.type))->type.precision)
                   ==
                   ((((((((exp)->exp.operands[0]))->exp.operands[0]))->common.type))->type.precision))


               && (((((((((exp)->exp.operands[1]))->exp.operands[0]))->common.type))->common.unsigned_flag)
                   ==
                   ((((((((exp)->exp.operands[0]))->exp.operands[0]))->common.type))->common.unsigned_flag)))))
        {
          enum machine_mode innermode
            = ((((((((exp)->exp.operands[0]))->exp.operands[0]))->common.type))->type.mode);
          optab other_optab = (((((((((exp)->exp.operands[0]))->exp.operands[0]))->common.type))->common.unsigned_flag)
                        ? (optab_table[OTI_smul_widen]) : (optab_table[OTI_umul_widen]));
          this_optab = (((((((((exp)->exp.operands[0]))->exp.operands[0]))->common.type))->common.unsigned_flag)
                        ? (optab_table[OTI_umul_widen]) : (optab_table[OTI_smul_widen]));
          if (mode == ((enum machine_mode)mode_wider_mode[(int) (innermode)]))
            {
              if (this_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
                {
                  op0 = expand_expr (((((exp)->exp.operands[0]))->exp.operands[0]),
                                     (rtx) 0, VOIDmode, 0);
                  if (((enum tree_code) (((exp)->exp.operands[1]))->common.code) == INTEGER_CST)
                    op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0,
                                       VOIDmode, 0);
                  else
                    op1 = expand_expr (((((exp)->exp.operands[1]))->exp.operands[0]),
                                       (rtx) 0, VOIDmode, 0);
                  goto binop2;
                }
              else if (other_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing
                       && innermode == word_mode)
                {
                  rtx htem;
                  op0 = expand_expr (((((exp)->exp.operands[0]))->exp.operands[0]),
                                     (rtx) 0, VOIDmode, 0);
                  if (((enum tree_code) (((exp)->exp.operands[1]))->common.code) == INTEGER_CST)
                    op1 = convert_modes (innermode, mode,
                                         expand_expr (((exp)->exp.operands[1]),
                                                      (rtx) 0, VOIDmode, 0),
                                         unsignedp);
                  else
                    op1 = expand_expr (((((exp)->exp.operands[1]))->exp.operands[0]),
                                       (rtx) 0, VOIDmode, 0);
                  temp = expand_binop (mode, other_optab, op0, op1, target,
                                       unsignedp, OPTAB_LIB_WIDEN);
                  htem = expand_mult_highpart_adjust (innermode,
                                                      gen_highpart (innermode, temp),
                                                      op0, op1,
                                                      gen_highpart (innermode, temp),
                                                      unsignedp);
                  emit_move_insn (gen_highpart (innermode, temp), htem);
                  return temp;
                }
            }
        }
      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
      op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);
      return expand_mult (mode, op0, op1, target, unsignedp);

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (! safe_from_p (subtarget, ((exp)->exp.operands[1]), 1))
        subtarget = 0;



      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
      op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);
      return expand_divmod (0, code, mode, op0, op1, target, unsignedp);

    case RDIV_EXPR:



      if (flag_unsafe_math_optimizations && optimize && !optimize_size
          && ((enum tree_code) (type)->common.code) == REAL_TYPE
          && !real_onep (((exp)->exp.operands[0])))
        return expand_expr (build (MULT_EXPR, type, ((exp)->exp.operands[0]),
                                   build (RDIV_EXPR, type,
                                          build_real (type, dconst1),
                                          ((exp)->exp.operands[1]))),
                            target, tmode, unsignedp);
      this_optab = (optab_table[OTI_sdiv]);
      goto binop;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      if (! safe_from_p (subtarget, ((exp)->exp.operands[1]), 1))
        subtarget = 0;
      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
      op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);
      return expand_divmod (1, code, mode, op0, op1, target, unsignedp);

    case FIX_ROUND_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_CEIL_EXPR:
      fancy_abort ("expr.c", 7833, __FUNCTION__);

    case FIX_TRUNC_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), (rtx) 0, VOIDmode, 0);
      if (target == 0)
        target = gen_reg_rtx (mode);
      expand_fix (target, op0, unsignedp);
      return target;

    case FLOAT_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), (rtx) 0, VOIDmode, 0);
      if (target == 0)
        target = gen_reg_rtx (mode);


      if (((enum machine_mode) (op0)->mode) == VOIDmode)
        op0 = copy_to_mode_reg (((((((exp)->exp.operands[0]))->common.type))->type.mode),
                                op0);
      expand_float (target, op0,
                    ((((((exp)->exp.operands[0]))->common.type))->common.unsigned_flag));
      return target;

    case NEGATE_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
      temp = expand_unop (mode,
                          ! unsignedp && flag_trapv
                          && ((mode_class[(int) (mode)]) == MODE_INT)
                          ? (optab_table[OTI_negv]) : (optab_table[OTI_neg]), op0, target, 0);
      if (temp == 0)
        fancy_abort ("expr.c", 7862, __FUNCTION__);
      return temp;

    case ABS_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);


      if ((mode_class[(int) (mode)]) == MODE_COMPLEX_INT
          || (mode_class[(int) (mode)]) == MODE_COMPLEX_FLOAT)
        return expand_complex_abs (mode, op0, target, unsignedp);



      if (((type)->common.unsigned_flag))
        return op0;

      return expand_abs (mode, op0, target, unsignedp,
                         safe_from_p (target, ((exp)->exp.operands[0]), 1));

    case MAX_EXPR:
    case MIN_EXPR:
      target = original_target;
      if (target == 0 || ! safe_from_p (target, ((exp)->exp.operands[1]), 1)
          || (((enum rtx_code) (target)->code) == MEM && ((target)->volatil))
          || ((enum machine_mode) (target)->mode) != mode
          || (((enum rtx_code) (target)->code) == REG
              && (((target)->fld[0]).rtuint) < 53))
        target = gen_reg_rtx (mode);
      op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);
      op0 = expand_expr (((exp)->exp.operands[0]), target, VOIDmode, 0);




      this_optab = (((type)->common.unsigned_flag)
                    ? (code == MIN_EXPR ? (optab_table[OTI_umin]) : (optab_table[OTI_umax]))
                    : (code == MIN_EXPR ? (optab_table[OTI_smin]) : (optab_table[OTI_smax])));

      temp = expand_binop (mode, this_optab, op0, op1, target, unsignedp,
                           OPTAB_WIDEN);
      if (temp != 0)
        return temp;




      if (((enum rtx_code) (target)->code) == MEM)
        target = gen_reg_rtx (mode);

      if (target != op0)
        emit_move_insn (target, op0);

      op0 = gen_label_rtx ();



      if ((mode_class[(int) (mode)]) == MODE_INT
          && ! can_compare_p (GE, mode, ccp_jump))
        {
          if (code == MAX_EXPR)
            do_jump_by_parts_greater_rtx (mode, ((type)->common.unsigned_flag),
                                          target, op1, (rtx) 0, op0);
          else
            do_jump_by_parts_greater_rtx (mode, ((type)->common.unsigned_flag),
                                          op1, target, (rtx) 0, op0);
        }
      else
        {
          int unsignedp = ((((((exp)->exp.operands[1]))->common.type))->common.unsigned_flag);
          do_compare_rtx_and_jump (target, op1, code == MAX_EXPR ? GE : LE,
                                   unsignedp, mode, (rtx) 0, (rtx) 0,
                                   op0);
        }
      emit_move_insn (target, op1);
      emit_label (op0);
      return target;

    case BIT_NOT_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, (optab_table[OTI_one_cmpl]), op0, target, 1);
      if (temp == 0)
        fancy_abort ("expr.c", 7943, __FUNCTION__);
      return temp;

    case FFS_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, (optab_table[OTI_ffs]), op0, target, 1);
      if (temp == 0)
        fancy_abort ("expr.c", 7950, __FUNCTION__);
      return temp;
# 7966 "expr.c"
    case TRUTH_AND_EXPR:
    case BIT_AND_EXPR:
      this_optab = (optab_table[OTI_and]);
      goto binop;

    case TRUTH_OR_EXPR:
    case BIT_IOR_EXPR:
      this_optab = (optab_table[OTI_ior]);
      goto binop;

    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:
      this_optab = (optab_table[OTI_xor]);
      goto binop;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (! safe_from_p (subtarget, ((exp)->exp.operands[1]), 1))
        subtarget = 0;
      op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
      return expand_shift (code, mode, op0, ((exp)->exp.operands[1]), target,
                           unsignedp);



    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
      temp = do_store_flag (exp, target, tmode != VOIDmode ? tmode : mode, 0);
      if (temp != 0)
        return temp;


      if (code == NE_EXPR && integer_zerop (((exp)->exp.operands[1]))
          && original_target
          && ((enum rtx_code) (original_target)->code) == REG
          && (((enum machine_mode) (original_target)->mode)
              == ((((((exp)->exp.operands[0]))->common.type))->type.mode)))
        {
          temp = expand_expr (((exp)->exp.operands[0]), original_target,
                              VOIDmode, 0);


          if (((enum rtx_code) (temp)->code) == CONST_INT)
            {
              if ((((temp)->fld[0]).rtwint) != 0)
                emit_move_insn (target, (const_int_rtx[64 +1]));
              else
                emit_move_insn (target, (const_int_rtx[64]));

              return target;
            }

          if (temp != original_target)
            {
              enum machine_mode mode1 = ((enum machine_mode) (temp)->mode);
              if (mode1 == VOIDmode)
                mode1 = tmode != VOIDmode ? tmode : mode;

              temp = copy_to_mode_reg (mode1, temp);
            }

          op1 = gen_label_rtx ();
          emit_cmp_and_jump_insns (temp, (const_int_rtx[64]), EQ, (rtx) 0,
                                   ((enum machine_mode) (temp)->mode), unsignedp, op1);
          emit_move_insn (temp, (const_int_rtx[64 +1]));
          emit_label (op1);
          return temp;
        }





    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      if (! ignore
          && (target == 0 || ! safe_from_p (target, exp, 1)


              || (!optimize && ((enum rtx_code) (target)->code) == REG
                  && (((target)->fld[0]).rtuint) < 53)))
        target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);

      if (target)
        emit_clr_insn (target);

      op1 = gen_label_rtx ();
      jumpifnot (exp, op1);

      if (target)
        emit_0_to_1_insn (target);

      emit_label (op1);
      return ignore ? (const_int_rtx[64]) : target;

    case TRUTH_NOT_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), target, VOIDmode, 0);


      temp = expand_binop (mode, (optab_table[OTI_xor]), op0, (const_int_rtx[64 +1]),
                           target, 1, OPTAB_LIB_WIDEN);
      if (temp == 0)
        fancy_abort ("expr.c", 8081, __FUNCTION__);
      return temp;

    case COMPOUND_EXPR:
      expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode, 0);
      emit_queue ();
      return expand_expr (((exp)->exp.operands[1]),
                          (ignore ? (const_int_rtx[64]) : target),
                          VOIDmode, 0);

    case COND_EXPR:


      if (((enum tree_code) (((exp)->exp.operands[1]))->common.code) == NOP_EXPR
          && ((enum tree_code) (((exp)->exp.operands[2]))->common.code) == NOP_EXPR
          && (((((((exp)->exp.operands[1]))->exp.operands[0]))->common.type)
              == ((((((exp)->exp.operands[2]))->exp.operands[0]))->common.type)))
        {
          tree iftrue = ((((exp)->exp.operands[1]))->exp.operands[0]);
          tree iffalse = ((((exp)->exp.operands[2]))->exp.operands[0]);

          if ((tree_code_type[(int) (((enum tree_code) (iftrue)->common.code))] == '2'
               && operand_equal_p (iffalse, ((iftrue)->exp.operands[0]), 0))
              || (tree_code_type[(int) (((enum tree_code) (iffalse)->common.code))] == '2'
                  && operand_equal_p (iftrue, ((iffalse)->exp.operands[0]), 0))
              || (tree_code_type[(int) (((enum tree_code) (iftrue)->common.code))] == '1'
                  && operand_equal_p (iffalse, ((iftrue)->exp.operands[0]), 0))
              || (tree_code_type[(int) (((enum tree_code) (iffalse)->common.code))] == '1'
                  && operand_equal_p (iftrue, ((iffalse)->exp.operands[0]), 0)))
            return expand_expr (build1 (NOP_EXPR, type,
                                        build (COND_EXPR, ((iftrue)->common.type),
                                               ((exp)->exp.operands[0]),
                                               iftrue, iffalse)),
                                target, tmode, modifier);
        }

      {
# 8126 "expr.c"
        tree singleton = 0;
        tree binary_op = 0, unary_op = 0;



        if (integer_onep (((exp)->exp.operands[1]))
            && integer_zerop (((exp)->exp.operands[2]))
            && tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[0]))->common.code))] == '<')
          {
            if (ignore)
              {
                expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode,
                             modifier);
                return (const_int_rtx[64]);
              }

            op0 = expand_expr (((exp)->exp.operands[0]), target, mode, modifier);
            if (((enum machine_mode) (op0)->mode) == mode)
              return op0;

            if (target == 0)
              target = gen_reg_rtx (mode);
            convert_move (target, op0, unsignedp);
            return target;
          }







        if (tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[1]))->common.code))] == '2'
            && operand_equal_p (((exp)->exp.operands[2]),
                                ((((exp)->exp.operands[1]))->exp.operands[0]), 0))
          singleton = ((exp)->exp.operands[2]), binary_op = ((exp)->exp.operands[1]);
        else if (tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[2]))->common.code))] == '2'
                 && operand_equal_p (((exp)->exp.operands[1]),
                                     ((((exp)->exp.operands[2]))->exp.operands[0]), 0))
          singleton = ((exp)->exp.operands[1]), binary_op = ((exp)->exp.operands[2]);
        else if (tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[1]))->common.code))] == '1'
                 && operand_equal_p (((exp)->exp.operands[2]),
                                     ((((exp)->exp.operands[1]))->exp.operands[0]), 0))
          singleton = ((exp)->exp.operands[2]), unary_op = ((exp)->exp.operands[1]);
        else if (tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[2]))->common.code))] == '1'
                 && operand_equal_p (((exp)->exp.operands[1]),
                                     ((((exp)->exp.operands[2]))->exp.operands[0]), 0))
          singleton = ((exp)->exp.operands[1]), unary_op = ((exp)->exp.operands[2]);






        if (ignore)
          temp = 0;
        else if (original_target
                 && (safe_from_p (original_target, ((exp)->exp.operands[0]), 1)
                     || (singleton && ((enum rtx_code) (original_target)->code) == REG
                         && (((original_target)->fld[0]).rtuint) >= 53
                         && original_target == var_rtx (singleton)))
                 && ((enum machine_mode) (original_target)->mode) == mode

                 && (! can_conditionally_move_p (mode)
                     || ((enum rtx_code) (original_target)->code) == REG
                     || ((type)->common.addressable_flag))

                 && (((enum rtx_code) (original_target)->code) != MEM
                     || ((type)->common.addressable_flag)))
          temp = original_target;
        else if (((type)->common.addressable_flag))
          fancy_abort ("expr.c", 8197, __FUNCTION__);
        else
          temp = assign_temp (type, 0, 0, 1);





        if (temp && singleton && binary_op
            && (((enum tree_code) (binary_op)->common.code) == PLUS_EXPR
                || ((enum tree_code) (binary_op)->common.code) == MINUS_EXPR
                || ((enum tree_code) (binary_op)->common.code) == BIT_IOR_EXPR
                || ((enum tree_code) (binary_op)->common.code) == BIT_XOR_EXPR)
            && (ix86_branch_cost >= 3 ? integer_pow2p (((binary_op)->exp.operands[1]))
                : integer_onep (((binary_op)->exp.operands[1])))
            && tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[0]))->common.code))] == '<')
          {
            rtx result;
            optab boptab = (((enum tree_code) (binary_op)->common.code) == PLUS_EXPR
                            ? ((flag_trapv && ! (((((binary_op)->common.type)))->common.unsigned_flag))
                               ? (optab_table[OTI_addv]) : (optab_table[OTI_add]))
                            : ((enum tree_code) (binary_op)->common.code) == MINUS_EXPR
                              ? ((flag_trapv && ! (((((binary_op)->common.type)))->common.unsigned_flag))
                                 ? (optab_table[OTI_subv]) : (optab_table[OTI_sub]))
                            : ((enum tree_code) (binary_op)->common.code) == BIT_IOR_EXPR ? (optab_table[OTI_ior])
                            : (optab_table[OTI_xor]));
# 8231 "expr.c"
            if (singleton == ((exp)->exp.operands[1]))
              ((exp)->exp.operands[0])
                = invert_truthvalue (((exp)->exp.operands[0]));

            result = do_store_flag (((exp)->exp.operands[0]),
                                    (safe_from_p (temp, singleton, 1)
                                     ? temp : (rtx) 0),
                                    mode, ix86_branch_cost <= 1);

            if (result != 0 && ! integer_onep (((binary_op)->exp.operands[1])))
              result = expand_shift (LSHIFT_EXPR, mode, result,
                                     build_int_2_wide ((unsigned long long) (tree_log2 (((binary_op)->exp.operands[1]))), (long long) (0)),



                                     (safe_from_p (temp, singleton, 1)
                                      ? temp : (rtx) 0), 0);

            if (result)
              {
                op1 = expand_expr (singleton, (rtx) 0, VOIDmode, 0);
                return expand_binop (mode, boptab, op1, result, temp,
                                     unsignedp, OPTAB_LIB_WIDEN);
              }
            else if (singleton == ((exp)->exp.operands[1]))
              ((exp)->exp.operands[0])
                = invert_truthvalue (((exp)->exp.operands[0]));
          }

        do_pending_stack_adjust ();
        ((cfun->expr->x_inhibit_defer_pop) += 1);
        op0 = gen_label_rtx ();

        if (singleton && ! ((((exp)->exp.operands[0]))->common.side_effects_flag))
          {
            if (temp != 0)
              {




                if ((binary_op
                     && ! safe_from_p (temp, ((binary_op)->exp.operands[1]), 1))
                    || (((enum rtx_code) (temp)->code) == REG
                        && (((temp)->fld[0]).rtuint) < 53))
                  temp = gen_reg_rtx (mode);
                store_expr (singleton, temp, 0);
              }
            else
              expand_expr (singleton,
                           ignore ? (const_int_rtx[64]) : (rtx) 0, VOIDmode, 0);
            if (singleton == ((exp)->exp.operands[1]))
              jumpif (((exp)->exp.operands[0]), op0);
            else
              jumpifnot (((exp)->exp.operands[0]), op0);

            start_cleanup_deferral ();
            if (binary_op && temp == 0)

              expand_expr (((binary_op)->exp.operands[1]),
                           ignore ? (const_int_rtx[64]) : (rtx) 0, VOIDmode, 0);
            else if (binary_op)
              store_expr (build (((enum tree_code) (binary_op)->common.code), type,
                                 make_tree (type, temp),
                                 ((binary_op)->exp.operands[1])),
                          temp, 0);
            else
              store_expr (build1 (((enum tree_code) (unary_op)->common.code), type,
                                  make_tree (type, temp)),
                          temp, 0);
            op1 = op0;
          }




        else if (temp
                 && tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[0]))->common.code))] == '<'
                 && integer_zerop (((((exp)->exp.operands[0]))->exp.operands[1]))
                 && operand_equal_p (((((exp)->exp.operands[0]))->exp.operands[0]),
                                     ((exp)->exp.operands[1]), 0)
                 && (! ((((exp)->exp.operands[0]))->common.side_effects_flag)
                     || ((enum tree_code) (((exp)->exp.operands[1]))->common.code) == SAVE_EXPR)
                 && safe_from_p (temp, ((exp)->exp.operands[2]), 1))
          {
            if (((enum rtx_code) (temp)->code) == REG
                && (((temp)->fld[0]).rtuint) < 53)
              temp = gen_reg_rtx (mode);
            store_expr (((exp)->exp.operands[1]), temp, 0);
            jumpif (((exp)->exp.operands[0]), op0);

            start_cleanup_deferral ();
            store_expr (((exp)->exp.operands[2]), temp, 0);
            op1 = op0;
          }
        else if (temp
                 && tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[0]))->common.code))] == '<'
                 && integer_zerop (((((exp)->exp.operands[0]))->exp.operands[1]))
                 && operand_equal_p (((((exp)->exp.operands[0]))->exp.operands[0]),
                                     ((exp)->exp.operands[2]), 0)
                 && (! ((((exp)->exp.operands[0]))->common.side_effects_flag)
                     || ((enum tree_code) (((exp)->exp.operands[2]))->common.code) == SAVE_EXPR)
                 && safe_from_p (temp, ((exp)->exp.operands[1]), 1))
          {
            if (((enum rtx_code) (temp)->code) == REG
                && (((temp)->fld[0]).rtuint) < 53)
              temp = gen_reg_rtx (mode);
            store_expr (((exp)->exp.operands[2]), temp, 0);
            jumpifnot (((exp)->exp.operands[0]), op0);

            start_cleanup_deferral ();
            store_expr (((exp)->exp.operands[1]), temp, 0);
            op1 = op0;
          }
        else
          {
            op1 = gen_label_rtx ();
            jumpifnot (((exp)->exp.operands[0]), op0);

            start_cleanup_deferral ();



            if (temp != 0
                && ((((exp)->exp.operands[1]))->common.type) != global_trees[TI_VOID_TYPE])
              store_expr (((exp)->exp.operands[1]), temp, 0);
            else
              expand_expr (((exp)->exp.operands[1]),
                           ignore ? (const_int_rtx[64]) : (rtx) 0, VOIDmode, 0);
            end_cleanup_deferral ();
            emit_queue ();
            emit_jump_insn (gen_jump (op1));
            emit_barrier ();
            emit_label (op0);
            start_cleanup_deferral ();
            if (temp != 0
                && ((((exp)->exp.operands[2]))->common.type) != global_trees[TI_VOID_TYPE])
              store_expr (((exp)->exp.operands[2]), temp, 0);
            else
              expand_expr (((exp)->exp.operands[2]),
                           ignore ? (const_int_rtx[64]) : (rtx) 0, VOIDmode, 0);
          }

        end_cleanup_deferral ();

        emit_queue ();
        emit_label (op1);
        ((cfun->expr->x_inhibit_defer_pop) -= 1);

        return temp;
      }

    case TARGET_EXPR:
      {
# 8394 "expr.c"
        tree slot = ((exp)->exp.operands[0]);
        tree cleanups = (tree) ((void *)0);
        tree exp1;

        if (((enum tree_code) (slot)->common.code) != VAR_DECL)
          fancy_abort ("expr.c", 8399, __FUNCTION__);

        if (! ignore)
          target = original_target;




        ((slot)->common.used_flag) = 1;

        if (target == 0)
          {
            if (((slot)->decl.rtl != ((void *)0)))
              {
                target = ((slot)->decl.rtl ? (slot)->decl.rtl : (make_decl_rtl (slot, ((void *)0)), (slot)->decl.rtl));


                if (((exp)->exp.operands[1]) == (tree) ((void *)0))
                  return target;
              }
            else
              {
                target = assign_temp (type, 2, 0, 1);

                preserve_temp_slots (target);
                ((slot)->decl.rtl = (target));
                if (((slot)->common.addressable_flag))
                  put_var_into_stack (slot);
# 8435 "expr.c"
                if (((exp)->exp.operands[2]) == 0)
                  ((exp)->exp.operands[2]) = maybe_build_cleanup (slot);
                cleanups = ((exp)->exp.operands[2]);
              }
          }
        else
          {
# 8450 "expr.c"
            if (((slot)->decl.rtl != ((void *)0)))
              {
                target = ((slot)->decl.rtl ? (slot)->decl.rtl : (make_decl_rtl (slot, ((void *)0)), (slot)->decl.rtl));


                if (((exp)->exp.operands[1]) == (tree) ((void *)0))
                  return target;
              }
            else
              {
                ((slot)->decl.rtl = (target));


                if (((slot)->common.addressable_flag))
                  put_var_into_stack (slot);
              }
          }

        exp1 = ((exp)->exp.operands[3]) = ((exp)->exp.operands[1]);

        ((exp)->exp.operands[1]) = (tree) ((void *)0);

        store_expr (exp1, target, 0);

        expand_decl_cleanup ((tree) ((void *)0), cleanups);

        return target;
      }

    case INIT_EXPR:
      {
        tree lhs = ((exp)->exp.operands[0]);
        tree rhs = ((exp)->exp.operands[1]);

        temp = expand_assignment (lhs, rhs, ! ignore, original_target != 0);
        return temp;
      }

    case MODIFY_EXPR:
      {
# 8498 "expr.c"
        tree lhs = ((exp)->exp.operands[0]);
        tree rhs = ((exp)->exp.operands[1]);

        temp = 0;
# 8511 "expr.c"
        if (ignore
            && ((enum tree_code) (lhs)->common.code) == COMPONENT_REF
            && (((enum tree_code) (rhs)->common.code) == BIT_IOR_EXPR
                || ((enum tree_code) (rhs)->common.code) == BIT_AND_EXPR)
            && ((rhs)->exp.operands[0]) == lhs
            && ((enum tree_code) (((rhs)->exp.operands[1]))->common.code) == COMPONENT_REF
            && integer_onep (((((lhs)->exp.operands[1]))->decl.size))
            && integer_onep (((((((rhs)->exp.operands[1]))->exp.operands[1]))->decl.size)))
          {
            rtx label = gen_label_rtx ();

            do_jump (((rhs)->exp.operands[1]),
                     ((enum tree_code) (rhs)->common.code) == BIT_IOR_EXPR ? label : 0,
                     ((enum tree_code) (rhs)->common.code) == BIT_AND_EXPR ? label : 0);
            expand_assignment (lhs, convert (((rhs)->common.type),
                                             (((enum tree_code) (rhs)->common.code) == BIT_IOR_EXPR
                                              ? global_trees[TI_INTEGER_ONE]
                                              : global_trees[TI_INTEGER_ZERO])),
                               0, 0);
            do_pending_stack_adjust ();
            emit_label (label);
            return (const_int_rtx[64]);
          }

        temp = expand_assignment (lhs, rhs, ! ignore, original_target != 0);

        return temp;
      }

    case RETURN_EXPR:
      if (!((exp)->exp.operands[0]))
        expand_null_return ();
      else
        expand_return (((exp)->exp.operands[0]));
      return (const_int_rtx[64]);

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      return expand_increment (exp, 0, ignore);

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:

      return expand_increment (exp, ! ignore, ignore);

    case ADDR_EXPR:

      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == FUNCTION_DECL
          && decl_function_context (((exp)->exp.operands[0])) != 0
          && ! ((((exp)->exp.operands[0]))->decl.regdecl_flag)
          && ! ((exp)->common.static_flag))
        {
          op0 = trampoline_address (((exp)->exp.operands[0]));
          op0 = force_operand (op0, target);
        }


      else if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == ERROR_MARK)
        return (const_int_rtx[64]);



      else if (cfun == 0
               && (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == CONSTRUCTOR
                   || (tree_code_type[(int) (((enum tree_code) (((exp)->exp.operands[0]))->common.code))]
                       == 'c')))
        op0 = (((output_constant_def (((exp)->exp.operands[0]), 0))->fld[0]).rtx);
      else
        {


          op0 = expand_expr (((exp)->exp.operands[0]),
                             ignore ? (const_int_rtx[64]) : (rtx) 0, VOIDmode,
                             (modifier == EXPAND_INITIALIZER
                              ? modifier : EXPAND_CONST_ADDRESS));




          if (ignore)
            return op0;



          op0 = protect_from_queue (op0, 1);





          if ((((enum rtx_code) (op0)->code) == LABEL_REF || ((enum rtx_code) (op0)->code) == SYMBOL_REF || ((enum rtx_code) (op0)->code) == CONST_INT || ((enum rtx_code) (op0)->code) == CONST_DOUBLE || ((enum rtx_code) (op0)->code) == CONST || ((enum rtx_code) (op0)->code) == HIGH || ((enum rtx_code) (op0)->code) == CONST_VECTOR || ((enum rtx_code) (op0)->code) == CONSTANT_P_RTX))
            op0 = force_const_mem (((((((exp)->exp.operands[0]))->common.type))->type.mode),
                                   op0);
          else if (((enum rtx_code) (op0)->code) == REG || ((enum rtx_code) (op0)->code) == SUBREG
                   || ((enum rtx_code) (op0)->code) == CONCAT || ((enum rtx_code) (op0)->code) == ADDRESSOF
                   || ((enum rtx_code) (op0)->code) == PARALLEL)
            {


              if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == SAVE_EXPR)
                {
                  put_var_into_stack (((exp)->exp.operands[0]));
                  op0 = (*(rtx *) &(((exp)->exp.operands[0]))->exp.operands[2]);
                }
              else
                {

                  tree inner_type = ((((exp)->exp.operands[0]))->common.type);
                  rtx memloc = assign_temp (inner_type, 1, 1, 1);

                  if (((enum rtx_code) (op0)->code) == PARALLEL)



                    emit_group_store (memloc, op0,
                                      int_size_in_bytes (inner_type));
                  else
                    emit_move_insn (memloc, op0);

                  op0 = memloc;
                }
            }

          if (((enum rtx_code) (op0)->code) != MEM)
            fancy_abort ("expr.c", 8635, __FUNCTION__);

          mark_temp_addr_taken (op0);
          if (modifier == EXPAND_SUM || modifier == EXPAND_INITIALIZER)
            {
              op0 = (((op0)->fld[0]).rtx);





              return op0;
            }
# 8659 "expr.c"
          if (0 && ((enum machine_mode) (op0)->mode) == BLKmode
              && (((((((exp)->exp.operands[0]))->common.type))->type.align)
                  > ((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8)))
              && ((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->align : (0 && ((enum machine_mode) (op0)->mode) != BLKmode ? get_mode_alignment (((enum machine_mode) (op0)->mode)) : 8)) < 128)
            {
              tree inner_type = ((((exp)->exp.operands[0]))->common.type);
              rtx new
                = assign_stack_temp_for_type
                  (((inner_type)->type.mode),
                   ((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->size : ((enum machine_mode) (op0)->mode) != BLKmode ? gen_rtx_CONST_INT (VOIDmode, (long long) ((mode_size[(int) (((enum machine_mode) (op0)->mode))]))) : 0) ? (((((((op0)->fld[1]).rtmem) != 0 ? (((op0)->fld[1]).rtmem)->size : ((enum machine_mode) (op0)->mode) != BLKmode ? gen_rtx_CONST_INT (VOIDmode, (long long) ((mode_size[(int) (((enum machine_mode) (op0)->mode))]))) : 0))->fld[0]).rtwint)
                   : int_size_in_bytes (inner_type),
                   1, build_qualified_type (inner_type,
                                            (((((inner_type)->common.readonly_flag) * 0x1) | (((inner_type)->common.volatile_flag) * 0x2) | (((inner_type)->type.restrict_flag) * 0x4) | ((((enum tree_code) (inner_type)->common.code) == RECORD_TYPE && ((inner_type)->common.type)) * 0x8))
                                             | 0x1)));

              if (((inner_type)->common.nothrow_flag))
                fancy_abort ("expr.c", 8675, __FUNCTION__);

              emit_block_move (new, op0, expr_size (((exp)->exp.operands[0])));
              op0 = new;
            }

          op0 = force_operand ((((op0)->fld[0]).rtx), target);
        }

      if (flag_force_addr
          && ((enum rtx_code) (op0)->code) != REG
          && modifier != EXPAND_CONST_ADDRESS
          && modifier != EXPAND_INITIALIZER
          && modifier != EXPAND_SUM)
        op0 = force_reg (((target_flags & 0x02000000) ? DImode : SImode), op0);

      if (((enum rtx_code) (op0)->code) == REG
          && ! ((op0)->volatil))
        mark_reg_pointer (op0, ((((type)->common.type))->type.align));







      return op0;

    case ENTRY_VALUE_EXPR:
      fancy_abort ("expr.c", 8704, __FUNCTION__);


    case COMPLEX_EXPR:
      {
        enum machine_mode mode = ((((((exp)->common.type))->common.type))->type.mode);
        rtx insns;


        op0 = expand_expr (((exp)->exp.operands[0]), 0, VOIDmode, 0);
        op1 = expand_expr (((exp)->exp.operands[1]), 0, VOIDmode, 0);

        if (! target)
          target = gen_reg_rtx (((((exp)->common.type))->type.mode));

        start_sequence ();


        emit_move_insn (gen_realpart (mode, target), op0);
        emit_move_insn (gen_imagpart (mode, target), op1);

        insns = get_insns ();
        end_sequence ();





        if (((enum rtx_code) (target)->code) != CONCAT)
          emit_no_conflict_block (insns, target, op0, op1, (rtx) 0);
        else
          emit_insns (insns);

        return target;
      }

    case REALPART_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), 0, VOIDmode, 0);
      return gen_realpart (mode, op0);

    case IMAGPART_EXPR:
      op0 = expand_expr (((exp)->exp.operands[0]), 0, VOIDmode, 0);
      return gen_imagpart (mode, op0);

    case CONJ_EXPR:
      {
        enum machine_mode partmode = ((((((exp)->common.type))->common.type))->type.mode);
        rtx imag_t;
        rtx insns;

        op0 = expand_expr (((exp)->exp.operands[0]), 0, VOIDmode, 0);

        if (! target)
          target = gen_reg_rtx (mode);

        start_sequence ();


        emit_move_insn (gen_realpart (partmode, target),
                        gen_realpart (partmode, op0));

        imag_t = gen_imagpart (partmode, target);
        temp = expand_unop (partmode,
                            ! unsignedp && flag_trapv
                            && ((mode_class[(int) (partmode)]) == MODE_INT)
                            ? (optab_table[OTI_negv]) : (optab_table[OTI_neg]),
                            gen_imagpart (partmode, op0), imag_t, 0);
        if (temp != imag_t)
          emit_move_insn (imag_t, temp);

        insns = get_insns ();
        end_sequence ();





        if (((enum rtx_code) (target)->code) != CONCAT)
          emit_no_conflict_block (insns, target, op0, (rtx) 0, (rtx) 0);
        else
          emit_insns (insns);

        return target;
      }

    case TRY_CATCH_EXPR:
      {
        tree handler = ((exp)->exp.operands[1]);

        expand_eh_region_start ();

        op0 = expand_expr (((exp)->exp.operands[0]), 0, VOIDmode, 0);

        expand_eh_region_end_cleanup (handler);

        return op0;
      }

    case TRY_FINALLY_EXPR:
      {
        tree try_block = ((exp)->exp.operands[0]);
        tree finally_block = ((exp)->exp.operands[1]);
        rtx finally_label = gen_label_rtx ();
        rtx done_label = gen_label_rtx ();
        rtx return_link = gen_reg_rtx (((target_flags & 0x02000000) ? DImode : SImode));
        tree cleanup = build (GOTO_SUBROUTINE_EXPR, global_trees[TI_VOID_TYPE],
                              (tree) finally_label, (tree) return_link);
        ((cleanup)->common.side_effects_flag) = 1;



        expand_start_bindings_and_block(2, (tree) ((void *)0));

        (cfun->x_target_temp_slot_level) = (cfun->x_temp_slot_level);

        expand_decl_cleanup ((tree) ((void *)0), cleanup);
        op0 = expand_expr (try_block, target, tmode, modifier);

        preserve_temp_slots (op0);
        expand_end_bindings ((tree) ((void *)0), 0, 0);
        emit_jump (done_label);
        emit_label (finally_label);
        expand_expr (finally_block, (const_int_rtx[64]), VOIDmode, 0);
        emit_indirect_jump (return_link);
        emit_label (done_label);
        return op0;
      }

    case GOTO_SUBROUTINE_EXPR:
      {
        rtx subr = (rtx) ((exp)->exp.operands[0]);
        rtx return_link = *(rtx *) &((exp)->exp.operands[1]);
        rtx return_address = gen_label_rtx ();
        emit_move_insn (return_link,
                        gen_rtx_fmt_u00 (LABEL_REF, (((target_flags & 0x02000000) ? DImode : SImode)), (return_address)));
        emit_jump (subr);
        emit_label (return_address);
        return (const_int_rtx[64]);
      }

    case VA_ARG_EXPR:
      return expand_builtin_va_arg (((exp)->exp.operands[0]), type);

    case EXC_PTR_EXPR:
      return get_exception_pointer (cfun);

    case FDESC_EXPR:


      fancy_abort ("expr.c", 8853, __FUNCTION__);

    default:
      return (*lang_expand_expr) (exp, original_target, tmode, modifier);
    }



 binop:
  if (! safe_from_p (subtarget, ((exp)->exp.operands[1]), 1))
    subtarget = 0;
  op0 = expand_expr (((exp)->exp.operands[0]), subtarget, VOIDmode, 0);
  op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);
 binop2:
  temp = expand_binop (mode, this_optab, op0, op1, target,
                       unsignedp, OPTAB_LIB_WIDEN);
  if (temp == 0)
    fancy_abort ("expr.c", 8870, __FUNCTION__);
  return temp;
}





static int
is_aligning_offset (offset, exp)
     tree offset;
     tree exp;
{

  while (((enum tree_code) (offset)->common.code) == NON_LVALUE_EXPR
         || ((enum tree_code) (offset)->common.code) == NOP_EXPR
         || ((enum tree_code) (offset)->common.code) == CONVERT_EXPR
         || ((enum tree_code) (offset)->common.code) == WITH_RECORD_EXPR)
    offset = ((offset)->exp.operands[0]);



  if (((enum tree_code) (offset)->common.code) != BIT_AND_EXPR
      || !host_integerp (((offset)->exp.operands[1]), 1)
      || compare_tree_int (((offset)->exp.operands[1]), 128) <= 0
      || !exact_log2_wide ((unsigned long long) (tree_low_cst (((offset)->exp.operands[1]), 1) + 1)) < 0)
    return 0;



  offset = ((offset)->exp.operands[0]);
  while (((enum tree_code) (offset)->common.code) == NON_LVALUE_EXPR
         || ((enum tree_code) (offset)->common.code) == NOP_EXPR
         || ((enum tree_code) (offset)->common.code) == CONVERT_EXPR)
    offset = ((offset)->exp.operands[0]);

  if (((enum tree_code) (offset)->common.code) != NEGATE_EXPR)
    return 0;

  offset = ((offset)->exp.operands[0]);
  while (((enum tree_code) (offset)->common.code) == NON_LVALUE_EXPR
         || ((enum tree_code) (offset)->common.code) == NOP_EXPR
         || ((enum tree_code) (offset)->common.code) == CONVERT_EXPR)
    offset = ((offset)->exp.operands[0]);



  return (((enum tree_code) (offset)->common.code) == ADDR_EXPR
          && (((offset)->exp.operands[0]) == exp
              || (((enum tree_code) (((offset)->exp.operands[0]))->common.code) == PLACEHOLDER_EXPR
                  && (((((offset)->exp.operands[0]))->common.type)
                      == ((exp)->common.type)))));
}






tree
string_constant (arg, ptr_offset)
     tree arg;
     tree *ptr_offset;
{
  while ((((enum tree_code) (arg)->common.code) == NOP_EXPR || ((enum tree_code) (arg)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg)->common.code) == NON_LVALUE_EXPR) && ((arg)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg)->common.type))->type.mode) == ((((((arg)->exp.operands[0]))->common.type))->type.mode))) (arg) = ((arg)->exp.operands[0]);

  if (((enum tree_code) (arg)->common.code) == ADDR_EXPR
      && ((enum tree_code) (((arg)->exp.operands[0]))->common.code) == STRING_CST)
    {
      *ptr_offset = global_trees[TI_SIZE_ZERO];
      return ((arg)->exp.operands[0]);
    }
  else if (((enum tree_code) (arg)->common.code) == PLUS_EXPR)
    {
      tree arg0 = ((arg)->exp.operands[0]);
      tree arg1 = ((arg)->exp.operands[1]);

      while ((((enum tree_code) (arg0)->common.code) == NOP_EXPR || ((enum tree_code) (arg0)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg0)->common.code) == NON_LVALUE_EXPR) && ((arg0)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg0)->common.type))->type.mode) == ((((((arg0)->exp.operands[0]))->common.type))->type.mode))) (arg0) = ((arg0)->exp.operands[0]);
      while ((((enum tree_code) (arg1)->common.code) == NOP_EXPR || ((enum tree_code) (arg1)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg1)->common.code) == NON_LVALUE_EXPR) && ((arg1)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg1)->common.type))->type.mode) == ((((((arg1)->exp.operands[0]))->common.type))->type.mode))) (arg1) = ((arg1)->exp.operands[0]);

      if (((enum tree_code) (arg0)->common.code) == ADDR_EXPR
          && ((enum tree_code) (((arg0)->exp.operands[0]))->common.code) == STRING_CST)
        {
          *ptr_offset = convert (sizetype_tab[(int) SIZETYPE], arg1);
          return ((arg0)->exp.operands[0]);
        }
      else if (((enum tree_code) (arg1)->common.code) == ADDR_EXPR
               && ((enum tree_code) (((arg1)->exp.operands[0]))->common.code) == STRING_CST)
        {
          *ptr_offset = convert (sizetype_tab[(int) SIZETYPE], arg0);
          return ((arg1)->exp.operands[0]);
        }
    }

  return 0;
}





static rtx
expand_increment (exp, post, ignore)
     tree exp;
     int post, ignore;
{
  rtx op0, op1;
  rtx temp, value;
  tree incremented = ((exp)->exp.operands[0]);
  optab this_optab = (optab_table[OTI_add]);
  int icode;
  enum machine_mode mode = ((((exp)->common.type))->type.mode);
  int op0_is_copy = 0;
  int single_insn = 0;



  int bad_subreg = 0;



  if (!post
      || ((enum tree_code) (incremented)->common.code) == BIT_FIELD_REF
      || (((enum tree_code) (incremented)->common.code) == COMPONENT_REF
          && (((enum tree_code) (((incremented)->exp.operands[0]))->common.code) != INDIRECT_REF
              || ((((incremented)->exp.operands[1]))->decl.bit_field_flag))))
    incremented = stabilize_reference (incremented);



  if (((enum tree_code) (incremented)->common.code) == PREINCREMENT_EXPR
      || ((enum tree_code) (incremented)->common.code) == PREDECREMENT_EXPR)
    incremented = save_expr (incremented);






  temp = get_last_insn ();
  op0 = expand_expr (incremented, (rtx) 0, VOIDmode, 0);
# 9020 "expr.c"
  if (((enum rtx_code) (op0)->code) == SUBREG && ((op0)->in_struct))
    {
      if (post)
        (((op0)->fld[0]).rtx) = copy_to_reg ((((op0)->fld[0]).rtx));
      else
        bad_subreg = 1;
    }
  else if (((enum rtx_code) (op0)->code) == SUBREG
           && (mode_bitsize[(int) (((enum machine_mode) (op0)->mode))]) < ((target_flags & 0x02000000) ? 64 : 32))
    {



      if (post)
        op0 = copy_to_reg (op0);
      else
        bad_subreg = 1;
    }

  op0_is_copy = ((((enum rtx_code) (op0)->code) == SUBREG || ((enum rtx_code) (op0)->code) == REG)
                 && temp != get_last_insn ());
  op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);


  if (((enum tree_code) (exp)->common.code) == POSTDECREMENT_EXPR
      || ((enum tree_code) (exp)->common.code) == PREDECREMENT_EXPR)
    this_optab = (optab_table[OTI_sub]);


  if (this_optab == (optab_table[OTI_sub])
      && ((enum rtx_code) (op1)->code) == CONST_INT)
    {
      op1 = gen_rtx_CONST_INT (VOIDmode, (long long) (-(((op1)->fld[0]).rtwint)));
      this_optab = (optab_table[OTI_add]);
    }

  if ((flag_trapv && ! (((((exp)->common.type)))->common.unsigned_flag)))
    this_optab = this_optab == (optab_table[OTI_add]) ? (optab_table[OTI_addv]) : (optab_table[OTI_subv]);


  if (!post)
    {
      icode = (int) this_optab->handlers[(int) mode].insn_code;
      if (icode != (int) CODE_FOR_nothing


          && (*insn_data[icode].operand[0].predicate) (op0, mode)
          && (*insn_data[icode].operand[1].predicate) (op0, mode)
          && (*insn_data[icode].operand[2].predicate) (op1, mode))
        single_insn = 1;
    }
# 9080 "expr.c"
  if (op0_is_copy || (!post && !single_insn) || bad_subreg)
    {






      tree newexp = build (((((enum tree_code) (exp)->common.code) == POSTDECREMENT_EXPR
                             || ((enum tree_code) (exp)->common.code) == PREDECREMENT_EXPR)
                            ? MINUS_EXPR : PLUS_EXPR),
                           ((exp)->common.type),
                           incremented,
                           ((exp)->exp.operands[1]));

      while (((enum tree_code) (incremented)->common.code) == NOP_EXPR
             || ((enum tree_code) (incremented)->common.code) == CONVERT_EXPR)
        {
          newexp = convert (((incremented)->common.type), newexp);
          incremented = ((incremented)->exp.operands[0]);
        }

      temp = expand_assignment (incremented, newexp, ! post && ! ignore , 0);
      return post ? op0 : temp;
    }

  if (post)
    {
# 9118 "expr.c"
      icode = (int) this_optab->handlers[(int) mode].insn_code;
      if (icode != (int) CODE_FOR_nothing


          && (*insn_data[icode].operand[0].predicate) (op0, mode)
          && (*insn_data[icode].operand[1].predicate) (op0, mode))
        {
          if (! (*insn_data[icode].operand[2].predicate) (op1, mode))
            op1 = force_reg (mode, op1);

          return enqueue_insn (op0, (*insn_data[(int) (icode)].genfun) (op0, op0, op1));
        }
      if (icode != (int) CODE_FOR_nothing && ((enum rtx_code) (op0)->code) == MEM)
        {
          rtx addr = (general_operand ((((op0)->fld[0]).rtx), mode)
                      ? force_reg (((target_flags & 0x02000000) ? DImode : SImode), (((op0)->fld[0]).rtx))
                      : copy_to_reg ((((op0)->fld[0]).rtx)));
          rtx temp, result;

          op0 = replace_equiv_address (op0, addr);
          temp = force_reg (((enum machine_mode) (op0)->mode), op0);
          if (! (*insn_data[icode].operand[2].predicate) (op1, mode))
            op1 = force_reg (mode, op1);



          enqueue_insn (op0, gen_move_insn (op0, temp));
          result = enqueue_insn (temp, (*insn_data[(int) (icode)].genfun) (temp, temp, op1));
          return result;
        }
    }


  if (post)

    temp = value = copy_to_reg (op0);
  else




    temp = copy_rtx (value = op0);


  op1 = expand_binop (mode, this_optab, value, op1, op0,
                      ((((exp)->common.type))->common.unsigned_flag), OPTAB_LIB_WIDEN);


  if (op1 != op0)
    emit_move_insn (op0, op1);

  return temp;
}




void
init_pending_stack_adjust ()
{
  (cfun->expr->x_pending_stack_adjust) = 0;
}







void
clear_pending_stack_adjust ()
{

  if (optimize > 0
      && (! flag_omit_frame_pointer || (cfun->calls_alloca))
      && 1
      && ! (((current_function_decl)->decl.inline_flag) && ! flag_no_inline)
      && ! flag_inline_functions)
    {
      (cfun->expr->x_stack_pointer_delta) -= (cfun->expr->x_pending_stack_adjust),
      (cfun->expr->x_pending_stack_adjust) = 0;
    }

}



void
do_pending_stack_adjust ()
{
  if ((cfun->expr->x_inhibit_defer_pop) == 0)
    {
      if ((cfun->expr->x_pending_stack_adjust) != 0)
        adjust_stack (gen_rtx_CONST_INT (VOIDmode, (long long) ((cfun->expr->x_pending_stack_adjust))));
      (cfun->expr->x_pending_stack_adjust) = 0;
    }
}







void
jumpifnot (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, label, (rtx) 0);
}



void
jumpif (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, (rtx) 0, label);
}
# 9252 "expr.c"
void
do_jump (exp, if_false_label, if_true_label)
     tree exp;
     rtx if_false_label, if_true_label;
{
  enum tree_code code = ((enum tree_code) (exp)->common.code);



  rtx drop_through_label = 0;
  rtx temp;
  int i;
  tree type;
  enum machine_mode mode;





  emit_queue ();

  switch (code)
    {
    case ERROR_MARK:
      break;

    case INTEGER_CST:
      temp = integer_zerop (exp) ? if_false_label : if_true_label;
      if (temp)
        emit_jump (temp);
      break;
# 9293 "expr.c"
    case NOP_EXPR:
      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == COMPONENT_REF
          || ((enum tree_code) (((exp)->exp.operands[0]))->common.code) == BIT_FIELD_REF
          || ((enum tree_code) (((exp)->exp.operands[0]))->common.code) == ARRAY_REF
          || ((enum tree_code) (((exp)->exp.operands[0]))->common.code) == ARRAY_RANGE_REF)
        goto normal;
    case CONVERT_EXPR:


      if ((((((exp)->common.type))->type.precision)
           < ((((((exp)->exp.operands[0]))->common.type))->type.precision)))
        goto normal;
    case NON_LVALUE_EXPR:
    case REFERENCE_EXPR:
    case ABS_EXPR:
    case NEGATE_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:

      do_jump (((exp)->exp.operands[0]), if_false_label, if_true_label);
      break;

    case WITH_RECORD_EXPR:


      placeholder_list = tree_cons (((exp)->exp.operands[1]), (tree) ((void *)0),
                                    placeholder_list);
      do_jump (((exp)->exp.operands[0]), if_false_label, if_true_label);
      placeholder_list = ((placeholder_list)->common.chain);
      break;
# 9336 "expr.c"
    case MINUS_EXPR:

      do_compare_and_jump (build (NE_EXPR, ((exp)->common.type),
                                  ((exp)->exp.operands[0]),
                                  ((exp)->exp.operands[1])),
                           NE, NE, if_false_label, if_true_label);
      break;

    case BIT_AND_EXPR:







      if (! 0
          && ((enum tree_code) (((exp)->exp.operands[1]))->common.code) == INTEGER_CST
          && ((((exp)->common.type))->type.precision) <= (8 * 8)
          && (i = tree_floor_log2 (((exp)->exp.operands[1]))) >= 0
          && (mode = mode_for_size (i + 1, MODE_INT, 0)) != BLKmode
          && (type = type_for_mode (mode, 1)) != 0
          && ((type)->type.precision) < ((((exp)->common.type))->type.precision)
          && ((optab_table[OTI_cmp])->handlers[(int) ((type)->type.mode)].insn_code
              != CODE_FOR_nothing))
        {
          do_jump (convert (type, exp), if_false_label, if_true_label);
          break;
        }
      goto normal;

    case TRUTH_NOT_EXPR:
      do_jump (((exp)->exp.operands[0]), if_true_label, if_false_label);
      break;

    case TRUTH_ANDIF_EXPR:
      if (if_false_label == 0)
        if_false_label = drop_through_label = gen_label_rtx ();
      do_jump (((exp)->exp.operands[0]), if_false_label, (rtx) 0);
      start_cleanup_deferral ();
      do_jump (((exp)->exp.operands[1]), if_false_label, if_true_label);
      end_cleanup_deferral ();
      break;

    case TRUTH_ORIF_EXPR:
      if (if_true_label == 0)
        if_true_label = drop_through_label = gen_label_rtx ();
      do_jump (((exp)->exp.operands[0]), (rtx) 0, if_true_label);
      start_cleanup_deferral ();
      do_jump (((exp)->exp.operands[1]), if_false_label, if_true_label);
      end_cleanup_deferral ();
      break;

    case COMPOUND_EXPR:
      push_temp_slots ();
      expand_expr (((exp)->exp.operands[0]), (const_int_rtx[64]), VOIDmode, 0);
      preserve_temp_slots ((rtx) 0);
      free_temp_slots ();
      pop_temp_slots ();
      emit_queue ();
      do_pending_stack_adjust ();
      do_jump (((exp)->exp.operands[1]), if_false_label, if_true_label);
      break;

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      {
        long long bitsize, bitpos;
        int unsignedp;
        enum machine_mode mode;
        tree type;
        tree offset;
        int volatilep = 0;



        get_inner_reference (exp, &bitsize, &bitpos, &offset, &mode,
                             &unsignedp, &volatilep);

        type = type_for_size (bitsize, unsignedp);
        if (! 0
            && type != 0 && bitsize >= 0
            && ((type)->type.precision) < ((((exp)->common.type))->type.precision)
            && ((optab_table[OTI_cmp])->handlers[(int) ((type)->type.mode)].insn_code
                != CODE_FOR_nothing))
          {
            do_jump (convert (type, exp), if_false_label, if_true_label);
            break;
          }
        goto normal;
      }

    case COND_EXPR:

      if (integer_onep (((exp)->exp.operands[1]))
          && integer_zerop (((exp)->exp.operands[2])))
        do_jump (((exp)->exp.operands[0]), if_false_label, if_true_label);

      else if (integer_zerop (((exp)->exp.operands[1]))
               && integer_onep (((exp)->exp.operands[2])))
        do_jump (((exp)->exp.operands[0]), if_true_label, if_false_label);

      else
        {
          rtx label1 = gen_label_rtx ();
          drop_through_label = gen_label_rtx ();

          do_jump (((exp)->exp.operands[0]), label1, (rtx) 0);

          start_cleanup_deferral ();

          do_jump (((exp)->exp.operands[1]),
                   if_false_label ? if_false_label : drop_through_label,
                   if_true_label ? if_true_label : drop_through_label);

          do_pending_stack_adjust ();
          emit_label (label1);


          do_jump (((exp)->exp.operands[2]),
                   if_false_label ? if_false_label : drop_through_label,
                   if_true_label ? if_true_label : drop_through_label);
          end_cleanup_deferral ();
        }
      break;

    case EQ_EXPR:
      {
        tree inner_type = ((((exp)->exp.operands[0]))->common.type);

        if ((mode_class[(int) (((inner_type)->type.mode))]) == MODE_COMPLEX_FLOAT
            || (mode_class[(int) (((inner_type)->type.mode))]) == MODE_COMPLEX_INT)
          {
            tree exp0 = save_expr (((exp)->exp.operands[0]));
            tree exp1 = save_expr (((exp)->exp.operands[1]));
            do_jump
              (fold
               (build (TRUTH_ANDIF_EXPR, ((exp)->common.type),
                       fold (build (EQ_EXPR, ((exp)->common.type),
                                    fold (build1 (REALPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp0)),
                                    fold (build1 (REALPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp1)))),
                       fold (build (EQ_EXPR, ((exp)->common.type),
                                    fold (build1 (IMAGPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp0)),
                                    fold (build1 (IMAGPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp1)))))),
               if_false_label, if_true_label);
          }

        else if (integer_zerop (((exp)->exp.operands[1])))
          do_jump (((exp)->exp.operands[0]), if_true_label, if_false_label);

        else if ((mode_class[(int) (((inner_type)->type.mode))]) == MODE_INT
                 && !can_compare_p (EQ, ((inner_type)->type.mode), ccp_jump))
          do_jump_by_parts_equality (exp, if_false_label, if_true_label);
        else
          do_compare_and_jump (exp, EQ, EQ, if_false_label, if_true_label);
        break;
      }

    case NE_EXPR:
      {
        tree inner_type = ((((exp)->exp.operands[0]))->common.type);

        if ((mode_class[(int) (((inner_type)->type.mode))]) == MODE_COMPLEX_FLOAT
            || (mode_class[(int) (((inner_type)->type.mode))]) == MODE_COMPLEX_INT)
          {
            tree exp0 = save_expr (((exp)->exp.operands[0]));
            tree exp1 = save_expr (((exp)->exp.operands[1]));
            do_jump
              (fold
               (build (TRUTH_ORIF_EXPR, ((exp)->common.type),
                       fold (build (NE_EXPR, ((exp)->common.type),
                                    fold (build1 (REALPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp0)),
                                    fold (build1 (REALPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp1)))),
                       fold (build (NE_EXPR, ((exp)->common.type),
                                    fold (build1 (IMAGPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp0)),
                                    fold (build1 (IMAGPART_EXPR,
                                                  ((inner_type)->common.type),
                                                  exp1)))))),
               if_false_label, if_true_label);
          }

        else if (integer_zerop (((exp)->exp.operands[1])))
          do_jump (((exp)->exp.operands[0]), if_false_label, if_true_label);

        else if ((mode_class[(int) (((inner_type)->type.mode))]) == MODE_INT
                 && !can_compare_p (NE, ((inner_type)->type.mode), ccp_jump))
          do_jump_by_parts_equality (exp, if_true_label, if_false_label);
        else
          do_compare_and_jump (exp, NE, NE, if_false_label, if_true_label);
        break;
      }

    case LT_EXPR:
      mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);
      if ((mode_class[(int) (mode)]) == MODE_INT
          && ! can_compare_p (LT, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 1, if_false_label, if_true_label);
      else
        do_compare_and_jump (exp, LT, LTU, if_false_label, if_true_label);
      break;

    case LE_EXPR:
      mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);
      if ((mode_class[(int) (mode)]) == MODE_INT
          && ! can_compare_p (LE, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 0, if_true_label, if_false_label);
      else
        do_compare_and_jump (exp, LE, LEU, if_false_label, if_true_label);
      break;

    case GT_EXPR:
      mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);
      if ((mode_class[(int) (mode)]) == MODE_INT
          && ! can_compare_p (GT, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 0, if_false_label, if_true_label);
      else
        do_compare_and_jump (exp, GT, GTU, if_false_label, if_true_label);
      break;

    case GE_EXPR:
      mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);
      if ((mode_class[(int) (mode)]) == MODE_INT
          && ! can_compare_p (GE, mode, ccp_jump))
        do_jump_by_parts_greater (exp, 1, if_true_label, if_false_label);
      else
        do_compare_and_jump (exp, GE, GEU, if_false_label, if_true_label);
      break;

    case UNORDERED_EXPR:
    case ORDERED_EXPR:
      {
        enum rtx_code cmp, rcmp;
        int do_rev;

        if (code == UNORDERED_EXPR)
          cmp = UNORDERED, rcmp = ORDERED;
        else
          cmp = ORDERED, rcmp = UNORDERED;
        mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);

        do_rev = 0;
        if (! can_compare_p (cmp, mode, ccp_jump)
            && (can_compare_p (rcmp, mode, ccp_jump)


                || rcmp == UNORDERED))
          do_rev = 1;

        if (! do_rev)
          do_compare_and_jump (exp, cmp, cmp, if_false_label, if_true_label);
        else
          do_compare_and_jump (exp, rcmp, rcmp, if_true_label, if_false_label);
      }
      break;

    {
      enum rtx_code rcode1;
      enum tree_code tcode2;

      case UNLT_EXPR:
        rcode1 = UNLT;
        tcode2 = LT_EXPR;
        goto unordered_bcc;
      case UNLE_EXPR:
        rcode1 = UNLE;
        tcode2 = LE_EXPR;
        goto unordered_bcc;
      case UNGT_EXPR:
        rcode1 = UNGT;
        tcode2 = GT_EXPR;
        goto unordered_bcc;
      case UNGE_EXPR:
        rcode1 = UNGE;
        tcode2 = GE_EXPR;
        goto unordered_bcc;
      case UNEQ_EXPR:
        rcode1 = UNEQ;
        tcode2 = EQ_EXPR;
        goto unordered_bcc;

      unordered_bcc:
        mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);
        if (can_compare_p (rcode1, mode, ccp_jump))
          do_compare_and_jump (exp, rcode1, rcode1, if_false_label,
                               if_true_label);
        else
          {
            tree op0 = save_expr (((exp)->exp.operands[0]));
            tree op1 = save_expr (((exp)->exp.operands[1]));
            tree cmp0, cmp1;



            cmp0 = fold (build (UNORDERED_EXPR, ((exp)->common.type), op0, op1));
            cmp1 = fold (build (tcode2, ((exp)->common.type), op0, op1));
            exp = build (TRUTH_ORIF_EXPR, ((exp)->common.type), cmp0, cmp1);
            do_jump (exp, if_false_label, if_true_label);
          }
      }
      break;
# 9661 "expr.c"
    case CALL_EXPR:

      if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == ADDR_EXPR)
        {
          tree fndecl = ((((exp)->exp.operands[0]))->exp.operands[0]);
          tree arglist = ((exp)->exp.operands[1]);

          if (((enum tree_code) (fndecl)->common.code) == FUNCTION_DECL
              && (((fndecl)->decl.built_in_class) != NOT_BUILT_IN)
              && ((fndecl)->decl.u1.f) == BUILT_IN_EXPECT
              && arglist != (tree) ((void *)0)
              && ((arglist)->common.chain) != (tree) ((void *)0))
            {
              rtx seq = expand_builtin_expect_jump (exp, if_false_label,
                                                    if_true_label);

              if (seq != (rtx) 0)
                {
                  emit_insn (seq);
                  return;
                }
            }
        }


    default:
    normal:
      temp = expand_expr (exp, (rtx) 0, VOIDmode, 0);
# 9698 "expr.c"
      do_pending_stack_adjust ();

      emit_queue ();

      if (((enum rtx_code) (temp)->code) == CONST_INT
          || (((enum rtx_code) (temp)->code) == CONST_DOUBLE && ((enum machine_mode) (temp)->mode) == VOIDmode)
          || ((enum rtx_code) (temp)->code) == LABEL_REF)
        {
          rtx target = temp == (const_int_rtx[64]) ? if_false_label : if_true_label;
          if (target)
            emit_jump (target);
        }
      else if ((mode_class[(int) (((enum machine_mode) (temp)->mode))]) == MODE_INT
               && ! can_compare_p (NE, ((enum machine_mode) (temp)->mode), ccp_jump))

        do_jump_by_parts_equality_rtx (temp, if_true_label, if_false_label);
      else if (((enum machine_mode) (temp)->mode) != VOIDmode)
        do_compare_rtx_and_jump (temp, (const_tiny_rtx[0][(int) (((enum machine_mode) (temp)->mode))]),
                                 NE, ((((exp)->common.type))->common.unsigned_flag),
                                 ((enum machine_mode) (temp)->mode), (rtx) 0,
                                 if_false_label, if_true_label);
      else
        fancy_abort ("expr.c", 9720, __FUNCTION__);
    }

  if (drop_through_label)
    {



      do_pending_stack_adjust ();
      emit_label (drop_through_label);
    }
}






static void
do_jump_by_parts_greater (exp, swap, if_false_label, if_true_label)
     tree exp;
     int swap;
     rtx if_false_label, if_true_label;
{
  rtx op0 = expand_expr (((exp)->exp.operands[swap]), (rtx) 0, VOIDmode, 0);
  rtx op1 = expand_expr (((exp)->exp.operands[!swap]), (rtx) 0, VOIDmode, 0);
  enum machine_mode mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);
  int unsignedp = ((((((exp)->exp.operands[0]))->common.type))->common.unsigned_flag);

  do_jump_by_parts_greater_rtx (mode, unsignedp, op0, op1, if_false_label, if_true_label);
}





void
do_jump_by_parts_greater_rtx (mode, unsignedp, op0, op1, if_false_label, if_true_label)
     enum machine_mode mode;
     int unsignedp;
     rtx op0, op1;
     rtx if_false_label, if_true_label;
{
  int nwords = ((mode_size[(int) (mode)]) / ((target_flags & 0x02000000) ? 8 : 4));
  rtx drop_through_label = 0;
  int i;

  if (! if_true_label || ! if_false_label)
    drop_through_label = gen_label_rtx ();
  if (! if_true_label)
    if_true_label = drop_through_label;
  if (! if_false_label)
    if_false_label = drop_through_label;


  for (i = 0; i < nwords; i++)
    {
      rtx op0_word, op1_word;

      if (0)
        {
          op0_word = operand_subword_force (op0, i, mode);
          op1_word = operand_subword_force (op1, i, mode);
        }
      else
        {
          op0_word = operand_subword_force (op0, nwords - 1 - i, mode);
          op1_word = operand_subword_force (op1, nwords - 1 - i, mode);
        }


      do_compare_rtx_and_jump (op0_word, op1_word, GT,
                               (unsignedp || i > 0), word_mode, (rtx) 0,
                               (rtx) 0, if_true_label);


      do_compare_rtx_and_jump (op0_word, op1_word, NE, unsignedp, word_mode,
                               (rtx) 0, (rtx) 0, if_false_label);
    }

  if (if_false_label)
    emit_jump (if_false_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}




static void
do_jump_by_parts_equality (exp, if_false_label, if_true_label)
     tree exp;
     rtx if_false_label, if_true_label;
{
  rtx op0 = expand_expr (((exp)->exp.operands[0]), (rtx) 0, VOIDmode, 0);
  rtx op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);
  enum machine_mode mode = ((((((exp)->exp.operands[0]))->common.type))->type.mode);
  int nwords = ((mode_size[(int) (mode)]) / ((target_flags & 0x02000000) ? 8 : 4));
  int i;
  rtx drop_through_label = 0;

  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    do_compare_rtx_and_jump (operand_subword_force (op0, i, mode),
                             operand_subword_force (op1, i, mode),
                             EQ, ((((exp)->common.type))->common.unsigned_flag),
                             word_mode, (rtx) 0, if_false_label, (rtx) 0);

  if (if_true_label)
    emit_jump (if_true_label);
  if (drop_through_label)
    emit_label (drop_through_label);
}





void
do_jump_by_parts_equality_rtx (op0, if_false_label, if_true_label)
     rtx op0;
     rtx if_false_label, if_true_label;
{
  int nwords = (mode_size[(int) (((enum machine_mode) (op0)->mode))]) / ((target_flags & 0x02000000) ? 8 : 4);
  rtx part;
  int i;
  rtx drop_through_label = 0;






  part = gen_reg_rtx (word_mode);
  emit_move_insn (part, operand_subword_force (op0, 0, ((enum machine_mode) (op0)->mode)));
  for (i = 1; i < nwords && part != 0; i++)
    part = expand_binop (word_mode, (optab_table[OTI_ior]), part,
                         operand_subword_force (op0, i, ((enum machine_mode) (op0)->mode)),
                         part, 1, OPTAB_WIDEN);

  if (part != 0)
    {
      do_compare_rtx_and_jump (part, (const_int_rtx[64]), EQ, 1, word_mode,
                               (rtx) 0, if_false_label, if_true_label);

      return;
    }


  if (! if_false_label)
    drop_through_label = if_false_label = gen_label_rtx ();

  for (i = 0; i < nwords; i++)
    do_compare_rtx_and_jump (operand_subword_force (op0, i, ((enum machine_mode) (op0)->mode)),
                             (const_int_rtx[64]), EQ, 1, word_mode, (rtx) 0,
                             if_false_label, (rtx) 0);

  if (if_true_label)
    emit_jump (if_true_label);

  if (drop_through_label)
    emit_label (drop_through_label);
}
# 9897 "expr.c"
rtx
compare_from_rtx (op0, op1, code, unsignedp, mode, size)
     rtx op0, op1;
     enum rtx_code code;
     int unsignedp;
     enum machine_mode mode;
     rtx size;
{
  rtx tem;




  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
      op0 = op1;
      op1 = tem;
      code = swap_condition (code);
    }

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  if (((enum rtx_code) (op0)->code) == CONST_INT && ((enum rtx_code) (op1)->code) == CONST_INT
      && (tem = simplify_relational_operation (code, mode, op0, op1)) != 0)
    return tem;
# 9952 "expr.c"
  emit_cmp_insn (op0, op1, code, size, mode, unsignedp);

  return gen_rtx_fmt_ee (code, VOIDmode, (global_rtl[GR_CC0]), (const_int_rtx[64]));
}







void
do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode, size,
                         if_false_label, if_true_label)
     rtx op0, op1;
     enum rtx_code code;
     int unsignedp;
     enum machine_mode mode;
     rtx size;
     rtx if_false_label, if_true_label;
{
  rtx tem;
  int dummy_true_label = 0;



  if (! if_true_label && ! ((mode_class[(int) (mode)]) == MODE_FLOAT || (mode_class[(int) (mode)]) == MODE_COMPLEX_FLOAT || (mode_class[(int) (mode)]) == MODE_VECTOR_FLOAT))
    {
      if_true_label = if_false_label;
      if_false_label = 0;
      code = reverse_condition (code);
    }




  if (swap_commutative_operands_p (op0, op1))
    {
      tem = op0;
      op0 = op1;
      op1 = tem;
      code = swap_condition (code);
    }

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  if (((enum rtx_code) (op0)->code) == CONST_INT && ((enum rtx_code) (op1)->code) == CONST_INT
      && (tem = simplify_relational_operation (code, mode, op0, op1)) != 0)
    {
      if (tem == const_true_rtx)
        {
          if (if_true_label)
            emit_jump (if_true_label);
        }
      else
        {
          if (if_false_label)
            emit_jump (if_false_label);
        }
      return;
    }
# 10042 "expr.c"
  if (! if_true_label)
    {
      dummy_true_label = 1;
      if_true_label = gen_label_rtx ();
    }

  emit_cmp_and_jump_insns (op0, op1, code, size, mode, unsignedp,
                           if_true_label);

  if (if_false_label)
    emit_jump (if_false_label);
  if (dummy_true_label)
    emit_label (if_true_label);
}
# 10067 "expr.c"
static void
do_compare_and_jump (exp, signed_code, unsigned_code, if_false_label,
                     if_true_label)
     tree exp;
     enum rtx_code signed_code, unsigned_code;
     rtx if_false_label, if_true_label;
{
  rtx op0, op1;
  tree type;
  enum machine_mode mode;
  int unsignedp;
  enum rtx_code code;


  op0 = expand_expr (((exp)->exp.operands[0]), (rtx) 0, VOIDmode, 0);
  if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == ERROR_MARK)
    return;

  op1 = expand_expr (((exp)->exp.operands[1]), (rtx) 0, VOIDmode, 0);
  if (((enum tree_code) (((exp)->exp.operands[1]))->common.code) == ERROR_MARK)
    return;

  type = ((((exp)->exp.operands[0]))->common.type);
  mode = ((type)->type.mode);
  if (((enum tree_code) (((exp)->exp.operands[0]))->common.code) == INTEGER_CST
      && (((enum tree_code) (((exp)->exp.operands[1]))->common.code) != INTEGER_CST
          || ((mode_bitsize[(int) (mode)])
              > (mode_bitsize[(int) (((((((exp)->exp.operands[1]))->common.type))->type.mode))]))))

    {


      type = ((((exp)->exp.operands[1]))->common.type);
      mode = ((type)->type.mode);
    }
  unsignedp = ((type)->common.unsigned_flag);
  code = unsignedp ? unsigned_code : signed_code;
# 10132 "expr.c"
  emit_queue ();

  do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode,
                           ((mode == BLKmode)
                            ? expr_size (((exp)->exp.operands[0])) : (rtx) 0),
                           if_false_label, if_true_label);
}
# 10160 "expr.c"
static rtx
do_store_flag (exp, target, mode, only_cheap)
     tree exp;
     rtx target;
     enum machine_mode mode;
     int only_cheap;
{
  enum rtx_code code;
  tree arg0, arg1, type;
  tree tem;
  enum machine_mode operand_mode;
  int invert = 0;
  int unsignedp;
  rtx op0, op1;
  enum insn_code icode;
  rtx subtarget = target;
  rtx result, label;






  if (((enum tree_code) (exp)->common.code) == TRUTH_NOT_EXPR)
    invert = 1, exp = ((exp)->exp.operands[0]);

  arg0 = ((exp)->exp.operands[0]);
  arg1 = ((exp)->exp.operands[1]);


  if (arg0 == global_trees[TI_ERROR_MARK] || arg1 == global_trees[TI_ERROR_MARK])
    return (const_int_rtx[64]);

  type = ((arg0)->common.type);
  operand_mode = ((type)->type.mode);
  unsignedp = ((type)->common.unsigned_flag);



  if (operand_mode == BLKmode)
    return 0;
# 10215 "expr.c"
  while ((((enum tree_code) (arg0)->common.code) == NOP_EXPR || ((enum tree_code) (arg0)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg0)->common.code) == NON_LVALUE_EXPR) && ((arg0)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg0)->common.type))->type.mode) == ((((((arg0)->exp.operands[0]))->common.type))->type.mode))) (arg0) = ((arg0)->exp.operands[0]);
  while ((((enum tree_code) (arg1)->common.code) == NOP_EXPR || ((enum tree_code) (arg1)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg1)->common.code) == NON_LVALUE_EXPR) && ((arg1)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg1)->common.type))->type.mode) == ((((((arg1)->exp.operands[0]))->common.type))->type.mode))) (arg1) = ((arg1)->exp.operands[0]);
# 10225 "expr.c"
  switch (((enum tree_code) (exp)->common.code))
    {
    case EQ_EXPR:
      code = EQ;
      break;
    case NE_EXPR:
      code = NE;
      break;
    case LT_EXPR:
      if (integer_onep (arg1))
        arg1 = global_trees[TI_INTEGER_ZERO], code = unsignedp ? LEU : LE;
      else
        code = unsignedp ? LTU : LT;
      break;
    case LE_EXPR:
      if (! unsignedp && integer_all_onesp (arg1))
        arg1 = global_trees[TI_INTEGER_ZERO], code = LT;
      else
        code = unsignedp ? LEU : LE;
      break;
    case GT_EXPR:
      if (! unsignedp && integer_all_onesp (arg1))
        arg1 = global_trees[TI_INTEGER_ZERO], code = GE;
      else
        code = unsignedp ? GTU : GT;
      break;
    case GE_EXPR:
      if (integer_onep (arg1))
        arg1 = global_trees[TI_INTEGER_ZERO], code = unsignedp ? GTU : GT;
      else
        code = unsignedp ? GEU : GE;
      break;

    case UNORDERED_EXPR:
      code = UNORDERED;
      break;
    case ORDERED_EXPR:
      code = ORDERED;
      break;
    case UNLT_EXPR:
      code = UNLT;
      break;
    case UNLE_EXPR:
      code = UNLE;
      break;
    case UNGT_EXPR:
      code = UNGT;
      break;
    case UNGE_EXPR:
      code = UNGE;
      break;
    case UNEQ_EXPR:
      code = UNEQ;
      break;

    default:
      fancy_abort ("expr.c", 10281, __FUNCTION__);
    }


  if (((enum tree_code) (arg0)->common.code) == REAL_CST || ((enum tree_code) (arg0)->common.code) == INTEGER_CST)
    {
      tem = arg0; arg0 = arg1; arg1 = tem;
      code = swap_condition (code);
    }







  if ((code == NE || code == EQ)
      && ((enum tree_code) (arg0)->common.code) == BIT_AND_EXPR && integer_zerop (arg1)
      && integer_pow2p (((arg0)->exp.operands[1])))
    {
      tree inner = ((arg0)->exp.operands[0]);
      int bitnum = tree_log2 (((arg0)->exp.operands[1]));
      int ops_unsignedp;




      if (((enum tree_code) (inner)->common.code) == RSHIFT_EXPR
          && ((enum tree_code) (((inner)->exp.operands[1]))->common.code) == INTEGER_CST
          && (((((inner)->exp.operands[1]))->int_cst.int_cst).high) == 0
          && bitnum < ((type)->type.precision)
          && 0 > compare_tree_int (((inner)->exp.operands[1]),
                                   bitnum - ((type)->type.precision)))
        {
          bitnum += (((((inner)->exp.operands[1]))->int_cst.int_cst).low);
          inner = ((inner)->exp.operands[0]);
        }




      ops_unsignedp = (bitnum == ((type)->type.precision) - 1 ? 1



                       : 1

                       );

      if (! get_subtarget (subtarget)
          || ((enum machine_mode) (subtarget)->mode) != operand_mode
          || ! safe_from_p (subtarget, inner, 1))
        subtarget = 0;

      op0 = expand_expr (inner, subtarget, VOIDmode, 0);

      if (bitnum != 0)
        op0 = expand_shift (RSHIFT_EXPR, operand_mode, op0,
                            size_int_wide ((long long) (bitnum), SIZETYPE), subtarget, ops_unsignedp);

      if (((enum machine_mode) (op0)->mode) != mode)
        op0 = convert_to_mode (mode, op0, ops_unsignedp);

      if ((code == EQ && ! invert) || (code == NE && invert))
        op0 = expand_binop (mode, (optab_table[OTI_xor]), op0, (const_int_rtx[64 +1]), subtarget,
                            ops_unsignedp, OPTAB_LIB_WIDEN);


      if (bitnum != ((type)->type.precision) - 1)
        op0 = expand_and (mode, op0, (const_int_rtx[64 +1]), subtarget);

      return op0;
    }


  if (! can_compare_p (code, operand_mode, ccp_store_flag))
    return 0;

  icode = setcc_gen_code[(int) code];
  if (icode == CODE_FOR_nothing
      || (only_cheap && insn_data[(int) icode].operand[0].mode != mode))
    {


      if ((code == LT && integer_zerop (arg1))
          || (! only_cheap && code == GE && integer_zerop (arg1)))
        ;
      else if (ix86_branch_cost >= 0
               && ! only_cheap && (code == NE || code == EQ)
               && ((enum tree_code) (type)->common.code) != REAL_TYPE
               && (((optab_table[OTI_abs])->handlers[(int) operand_mode].insn_code
                    != CODE_FOR_nothing)
                   || ((optab_table[OTI_ffs])->handlers[(int) operand_mode].insn_code
                       != CODE_FOR_nothing)))
        ;
      else
        return 0;
    }

  if (! get_subtarget (target)
      || ((enum machine_mode) (subtarget)->mode) != operand_mode
      || ! safe_from_p (subtarget, arg1, 1))
    subtarget = 0;

  op0 = expand_expr (arg0, subtarget, VOIDmode, 0);
  op1 = expand_expr (arg1, (rtx) 0, VOIDmode, 0);

  if (target == 0)
    target = gen_reg_rtx (mode);





  result = emit_store_flag (target, code,
                            queued_subexp_p (op0) ? copy_rtx (op0) : op0,
                            queued_subexp_p (op1) ? copy_rtx (op1) : op1,
                            operand_mode, unsignedp, 1);

  if (result)
    {
      if (invert)
        result = expand_binop (mode, (optab_table[OTI_xor]), result, (const_int_rtx[64 +1]),
                               result, 0, OPTAB_LIB_WIDEN);
      return result;
    }


  if (((enum rtx_code) (target)->code) != REG
      || reg_mentioned_p (target, op0) || reg_mentioned_p (target, op1))
    target = gen_reg_rtx (((enum machine_mode) (target)->mode));

  emit_move_insn (target, invert ? (const_int_rtx[64]) : (const_int_rtx[64 +1]));
  result = compare_from_rtx (op0, op1, code, unsignedp,
                             operand_mode, (rtx) 0);
  if (((enum rtx_code) (result)->code) == CONST_INT)
    return (((result == (const_int_rtx[64]) && ! invert)
             || (result != (const_int_rtx[64]) && invert))
            ? (const_int_rtx[64]) : (const_int_rtx[64 +1]));







  code = ((enum rtx_code) (result)->code);

  label = gen_label_rtx ();
  if (bcc_gen_fctn[(int) code] == 0)
    fancy_abort ("expr.c", 10431, __FUNCTION__);

  emit_jump_insn ((*bcc_gen_fctn[(int) code]) (label));
  emit_move_insn (target, invert ? (const_int_rtx[64 +1]) : (const_int_rtx[64]));
  emit_label (label);

  return target;
}
# 10455 "expr.c"
unsigned int
case_values_threshold ()
{
  return (0 ? 4 : 5);
}



int
try_casesi (index_type, index_expr, minval, range,
            table_label, default_label)
     tree index_type, index_expr, minval, range;
     rtx table_label __attribute__ ((__unused__));
     rtx default_label;
{
  enum machine_mode index_mode = SImode;
  int index_bits = (mode_bitsize[(int) (index_mode)]);
  rtx op1, op2, index;
  enum machine_mode op_mode;

  if (! 0)
    return 0;


  if ((mode_bitsize[(int) (((index_type)->type.mode))]) > (mode_bitsize[(int) (index_mode)]))
    {
      enum machine_mode omode = ((index_type)->type.mode);
      rtx rangertx = expand_expr (range, (rtx) 0, VOIDmode, 0);


      index_expr = build (MINUS_EXPR, index_type,
                          index_expr, minval);
      minval = global_trees[TI_INTEGER_ZERO];
      index = expand_expr (index_expr, (rtx) 0, VOIDmode, 0);
      emit_cmp_and_jump_insns (rangertx, index, LTU, (rtx) 0,
                               omode, 1, default_label);

      index = convert_to_mode (index_mode, index, 0);
    }
  else
    {
      if (((index_type)->type.mode) != index_mode)
        {
          index_expr = convert (type_for_size (index_bits, 0),
                                index_expr);
          index_type = ((index_expr)->common.type);
        }

      index = expand_expr (index_expr, (rtx) 0, VOIDmode, 0);
    }
  emit_queue ();
  index = protect_from_queue (index, 0);
  do_pending_stack_adjust ();

  op_mode = insn_data[(int) CODE_FOR_nothing].operand[0].mode;
  if (! (*insn_data[(int) CODE_FOR_nothing].operand[0].predicate)
      (index, op_mode))
    index = copy_to_mode_reg (op_mode, index);

  op1 = expand_expr (minval, (rtx) 0, VOIDmode, 0);

  op_mode = insn_data[(int) CODE_FOR_nothing].operand[1].mode;
  op1 = convert_modes (op_mode, ((((minval)->common.type))->type.mode),
                       op1, ((((minval)->common.type))->common.unsigned_flag));
  if (! (*insn_data[(int) CODE_FOR_nothing].operand[1].predicate)
      (op1, op_mode))
    op1 = copy_to_mode_reg (op_mode, op1);

  op2 = expand_expr (range, (rtx) 0, VOIDmode, 0);

  op_mode = insn_data[(int) CODE_FOR_nothing].operand[2].mode;
  op2 = convert_modes (op_mode, ((((range)->common.type))->type.mode),
                       op2, ((((range)->common.type))->common.unsigned_flag));
  if (! (*insn_data[(int) CODE_FOR_nothing].operand[2].predicate)
      (op2, op_mode))
    op2 = copy_to_mode_reg (op_mode, op2);

  emit_jump_insn ((0));

  return 1;
}
# 10554 "expr.c"
static void
do_tablejump (index, mode, range, table_label, default_label)
     rtx index, range, table_label, default_label;
     enum machine_mode mode;
{
  rtx temp, vector;
# 10569 "expr.c"
  emit_cmp_and_jump_insns (index, range, GTU, (rtx) 0, mode, 1,
                           default_label);



  if (mode != ((target_flags & 0x02000000) ? DImode : SImode))
    index = convert_to_mode (((target_flags & 0x02000000) ? DImode : SImode), index, 1);
# 10593 "expr.c"
  index = gen_rtx_fmt_ee (PLUS, (((target_flags & 0x02000000) ? DImode : SImode)), (gen_rtx_fmt_ee (MULT, (((target_flags & 0x02000000) ? DImode : SImode)), (index), (gen_rtx_CONST_INT (VOIDmode, (long long) ((mode_size[(int) ((!(target_flags & 0x02000000) || flag_pic ? SImode : DImode))])))))), (gen_rtx_fmt_u00 (LABEL_REF, (((target_flags & 0x02000000) ? DImode : SImode)), (table_label))));
# 10602 "expr.c"
    index = memory_address_noforce ((!(target_flags & 0x02000000) || flag_pic ? SImode : DImode), index);
  temp = gen_reg_rtx ((!(target_flags & 0x02000000) || flag_pic ? SImode : DImode));
  vector = gen_rtx_MEM ((!(target_flags & 0x02000000) || flag_pic ? SImode : DImode), index);
  ((vector)->unchanging) = 1;
  convert_move (temp, vector, 0);

  emit_jump_insn (gen_tablejump (temp, table_label));



  if (! 0 && ! flag_pic)
    emit_barrier ();
}

int
try_tablejump (index_type, index_expr, minval, range,
               table_label, default_label)
     tree index_type, index_expr, minval, range;
     rtx table_label, default_label;
{
  rtx index;

  if (! 1)
    return 0;

  index_expr = fold (build (MINUS_EXPR, index_type,
                            convert (index_type, index_expr),
                            convert (index_type, minval)));
  index = expand_expr (index_expr, (rtx) 0, VOIDmode, 0);
  emit_queue ();
  index = protect_from_queue (index, 0);
  do_pending_stack_adjust ();

  do_tablejump (index, ((index_type)->type.mode),
                convert_modes (((index_type)->type.mode),
                               ((((range)->common.type))->type.mode),
                               expand_expr (range, (rtx) 0,
                                            VOIDmode, 0),
                               ((((range)->common.type))->common.unsigned_flag)),
                table_label, default_label);
  return 1;
}
