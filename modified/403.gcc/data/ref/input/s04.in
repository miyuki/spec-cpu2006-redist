# 1 "sched-deps.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "sched-deps.c"
# 25 "sched-deps.c"
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
# 26 "sched-deps.c" 2
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
        
# 27 "sched-deps.c" 2
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
# 28 "sched-deps.c" 2
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
# 29 "sched-deps.c" 2
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
# 30 "sched-deps.c" 2
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
# 31 "sched-deps.c" 2
# 1 "basic-block.h" 1
# 25 "basic-block.h"
# 1 "bitmap.h" 1
# 43 "bitmap.h"
typedef struct bitmap_element_def
{
  struct bitmap_element_def *next;
  struct bitmap_element_def *prev;
  unsigned int indx;
  unsigned long long bits[2];
} bitmap_element;


typedef struct bitmap_head_def {
  bitmap_element *first;
  bitmap_element *current;
  unsigned int indx;

} bitmap_head, *bitmap;


enum bitmap_bits {
  BITMAP_AND,
  BITMAP_AND_COMPL,
  BITMAP_IOR,
  BITMAP_XOR,
  BITMAP_IOR_COMPL
};


extern bitmap_element bitmap_zero_bits;


extern void bitmap_clear (bitmap);


extern void bitmap_copy (bitmap, bitmap);


extern int bitmap_equal_p (bitmap, bitmap);


extern int bitmap_operation (bitmap, bitmap, bitmap, enum bitmap_bits);



extern void bitmap_ior_and_compl (bitmap, bitmap, bitmap);


extern void bitmap_clear_bit (bitmap, int);


extern void bitmap_set_bit (bitmap, int);


extern int bitmap_bit_p (bitmap, int);


extern void debug_bitmap (bitmap);
extern void debug_bitmap_file (FILE *, bitmap);


extern void bitmap_print (FILE *, bitmap, const char *, const char *);


extern bitmap bitmap_initialize (bitmap);


extern void bitmap_release_memory (void);






extern int bitmap_union_of_diff (bitmap, bitmap, bitmap, bitmap);
extern int bitmap_first_set_bit (bitmap);
extern int bitmap_last_set_bit (bitmap);
# 26 "basic-block.h" 2
# 1 "sbitmap.h" 1
# 31 "sbitmap.h"
typedef struct simple_bitmap_def
{
  unsigned int n_bits;
  unsigned int size;
  unsigned int bytes;
  unsigned long long elms[1];
} *sbitmap;

typedef unsigned long long *sbitmap_ptr;
# 91 "sbitmap.h"
struct int_list;

extern void dump_sbitmap (FILE *, sbitmap);
extern void dump_sbitmap_vector (FILE *, const char *, const char *, sbitmap *, int);


extern sbitmap sbitmap_alloc (unsigned int);
extern sbitmap *sbitmap_vector_alloc (unsigned int, unsigned int);
extern void sbitmap_copy (sbitmap, sbitmap);
extern int sbitmap_equal (sbitmap, sbitmap);
extern void sbitmap_zero (sbitmap);
extern void sbitmap_ones (sbitmap);
extern void sbitmap_vector_zero (sbitmap *, unsigned int);
extern void sbitmap_vector_ones (sbitmap *, unsigned int);

extern int sbitmap_union_of_diff (sbitmap, sbitmap, sbitmap, sbitmap);

extern void sbitmap_difference (sbitmap, sbitmap, sbitmap);
extern void sbitmap_not (sbitmap, sbitmap);
extern int sbitmap_a_or_b_and_c (sbitmap, sbitmap, sbitmap, sbitmap);

extern int sbitmap_a_and_b_or_c (sbitmap, sbitmap, sbitmap, sbitmap);

extern int sbitmap_a_and_b (sbitmap, sbitmap, sbitmap);
extern int sbitmap_a_or_b (sbitmap, sbitmap, sbitmap);
extern int sbitmap_a_xor_b (sbitmap, sbitmap, sbitmap);
extern int sbitmap_a_subset_b_p (sbitmap, sbitmap);

extern int sbitmap_first_set_bit (sbitmap);
extern int sbitmap_last_set_bit (sbitmap);

extern void sbitmap_intersect_of_predsucc (sbitmap, sbitmap *, int, struct int_list **);




extern void sbitmap_union_of_predsucc (sbitmap, sbitmap *, int, struct int_list **);







extern void sbitmap_intersection_of_succs (sbitmap, sbitmap *, int);
extern void sbitmap_intersection_of_preds (sbitmap, sbitmap *, int);
extern void sbitmap_union_of_succs (sbitmap, sbitmap *, int);
extern void sbitmap_union_of_preds (sbitmap, sbitmap *, int);

extern void debug_sbitmap (sbitmap);
# 27 "basic-block.h" 2
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
# 28 "basic-block.h" 2
# 1 "partition.h" 1
# 43 "partition.h"
# 1 "/usr/include/ansidecl.h" 1 3 4
# 44 "partition.h" 2


struct partition_elem
{


  int class_element;


  struct partition_elem* next;


  unsigned class_count;
};

typedef struct partition_def
{

  int num_elements;

  struct partition_elem elements[1];
} *partition;

extern partition partition_new (int);
extern void partition_delete (partition);
extern int partition_union (partition, int, int);


extern void partition_print (partition, FILE*);
# 29 "basic-block.h" 2


typedef bitmap_head regset_head;

typedef bitmap regset;
# 74 "basic-block.h"
extern void reg_set_to_hard_reg_set (HARD_REG_ELT_TYPE *, bitmap);
# 116 "basic-block.h"
typedef long long gcov_type;


typedef struct edge_def {

  struct edge_def *pred_next, *succ_next;


  struct basic_block_def *src, *dest;


  rtx insns;


  void *aux;

  int flags;
  int probability;
  gcov_type count;

} *edge;
# 174 "basic-block.h"
typedef struct basic_block_def {

  rtx head, end;


  tree head_tree;
  tree end_tree;


  edge pred, succ;




  regset local_set;



  regset cond_local_set;





  regset global_live_at_start;

  regset global_live_at_end;


  void *aux;


  int index;


  int loop_depth;


  gcov_type count;


  int frequency;


  int flags;
} *basic_block;
# 228 "basic-block.h"
extern int n_basic_blocks;



extern int n_edges;



extern varray_type basic_block_info;





extern regset regs_live_at_setjmp;



extern rtx label_value_list, tail_recursion_label_list;

extern struct obstack flow_obstack;
# 278 "basic-block.h"
extern struct basic_block_def entry_exit_blocks[2];



extern varray_type basic_block_for_insn;



extern void compute_bb_for_insn (int);
extern void free_bb_for_insn (void);
extern void update_bb_for_insn (basic_block);
extern void set_block_for_insn (rtx, basic_block);

extern void free_basic_block_vars (int);

extern edge split_block (basic_block, rtx);
extern basic_block split_edge (edge);
extern void insert_insn_on_edge (rtx, edge);
extern void commit_edge_insertions (void);
extern void remove_fake_edges (void);
extern void add_noreturn_fake_exit_edges (void);
extern void connect_infinite_loops_to_exit (void);
extern int flow_call_edges_add (sbitmap);
extern edge cached_make_edge (sbitmap *, basic_block, basic_block, int);

extern edge make_edge (basic_block, basic_block, int);

extern edge make_single_succ_edge (basic_block, basic_block, int);

extern void remove_edge (edge);
extern void redirect_edge_succ (edge, basic_block);
extern edge redirect_edge_succ_nodup (edge, basic_block);
extern void redirect_edge_pred (edge, basic_block);
extern basic_block create_basic_block_structure (int, rtx, rtx, rtx);
extern basic_block create_basic_block (int, rtx, rtx);
extern int flow_delete_block (basic_block);
extern int flow_delete_block_noexpunge (basic_block);
extern void merge_blocks_nomove (basic_block, basic_block);
extern void tidy_fallthru_edge (edge, basic_block, basic_block);

extern void tidy_fallthru_edges (void);
extern void flow_reverse_top_sort_order_compute (int *);
extern int flow_depth_first_order_compute (int *, int *);
extern void flow_preorder_transversal_compute (int *);
extern void dump_edge_info (FILE *, edge, int);
extern void clear_edges (void);
extern void mark_critical_edges (void);
extern rtx first_insn_after_basic_block_note (basic_block);


struct loop
{

  int num;


  basic_block header;


  basic_block latch;


  basic_block pre_header;




  edge *pre_header_edges;


  int num_pre_header_edges;



  basic_block first;



  basic_block last;


  sbitmap nodes;


  int num_nodes;


  edge *entry_edges;


  int num_entries;


  edge *exit_edges;


  int num_exits;


  sbitmap exits_doms;


  int depth;



  int level;


  struct loop *outer;


  struct loop *inner;


  struct loop *next;


  int shared;


  int invalid;


  void *aux;





  rtx vtop;



  rtx cont;


  rtx cont_dominator;


  rtx start;


  rtx end;



  rtx top;


  rtx scan_start;


  rtx sink;
# 442 "basic-block.h"
  rtx exit_labels;



  int exit_count;
};



struct loops
{

  int num;


  int levels;



  struct loop *array;


  struct loop *tree_root;


  struct cfg
  {

    sbitmap *dom;


    int *dfs_order;



    int *rc_order;
  } cfg;


  sbitmap shared_headers;
};

extern int flow_loops_find (struct loops *, int flags);
extern int flow_loops_update (struct loops *, int flags);
extern void flow_loops_free (struct loops *);
extern void flow_loops_dump (const struct loops *, FILE *, void (*)(const struct loop *, FILE *, int), int);


extern void flow_loop_dump (const struct loop *, FILE *, void (*)(const struct loop *, FILE *, int), int);


extern int flow_loop_scan (struct loops *, struct loop *, int);


struct edge_list
{
  int num_blocks;
  int num_edges;
  edge *index_to_edge;
};
# 539 "basic-block.h"
struct edge_list * create_edge_list (void);
void free_edge_list (struct edge_list *);
void print_edge_list (FILE *, struct edge_list *);
void verify_edge_list (FILE *, struct edge_list *);
int find_edge_index (struct edge_list *, basic_block, basic_block);



enum update_life_extent
{
  UPDATE_LIFE_LOCAL = 0,
  UPDATE_LIFE_GLOBAL = 1,
  UPDATE_LIFE_GLOBAL_RM_NOTES = 2
};
# 588 "basic-block.h"
extern void life_analysis (rtx, FILE *, int);
extern void update_life_info (sbitmap, enum update_life_extent, int);

extern int count_or_remove_death_notes (sbitmap, int);
extern int propagate_block (basic_block, regset, regset, regset, int);


struct propagate_block_info;
extern rtx propagate_one_insn (struct propagate_block_info *, rtx);
extern struct propagate_block_info *init_propagate_block_info
  (basic_block, regset, regset, regset, int);
extern void free_propagate_block_info (struct propagate_block_info *);


extern struct edge_list *pre_edge_lcm (FILE *, int, sbitmap *, sbitmap *, sbitmap *, sbitmap *, sbitmap **, sbitmap **);



extern struct edge_list *pre_edge_rev_lcm (FILE *, int, sbitmap *, sbitmap *, sbitmap *, sbitmap *, sbitmap **, sbitmap **);



extern void compute_available (sbitmap *, sbitmap *, sbitmap *, sbitmap *);

extern int optimize_mode_switching (FILE *);


extern rtx emit_block_insn_after (rtx, rtx, basic_block);
extern rtx emit_block_insn_before (rtx, rtx, basic_block);


extern void estimate_probability (struct loops *);
extern void expected_value_to_br_prob (void);


extern void init_flow (void);
extern void reorder_basic_blocks (void);
extern void dump_bb (basic_block, FILE *);
extern void debug_bb (basic_block);
extern void debug_bb_n (int);
extern void dump_regset (regset, FILE *);
extern void debug_regset (regset);
extern void allocate_reg_life_data (void);
extern void allocate_bb_life_data (void);
extern void expunge_block (basic_block);
extern void expunge_block_nocompact (basic_block);
extern basic_block alloc_block (void);
extern void find_unreachable_blocks (void);
extern void delete_noop_moves (rtx);
extern basic_block redirect_edge_and_branch_force (edge, basic_block);
extern basic_block force_nonfallthru (edge);
extern _Bool redirect_edge_and_branch (edge, basic_block);
extern rtx block_label (basic_block);
extern _Bool forwarder_block_p (basic_block);
extern _Bool purge_all_dead_edges (int);
extern _Bool purge_dead_edges (basic_block);
extern void find_sub_basic_blocks (basic_block);
extern void find_many_sub_basic_blocks (sbitmap);
extern _Bool can_fallthru (basic_block, basic_block);
extern void flow_nodes_print (const char *, const sbitmap, FILE *);

extern void flow_edge_list_print (const char *, const edge *, int, FILE *);

extern void alloc_aux_for_block (basic_block, int);
extern void alloc_aux_for_blocks (int);
extern void clear_aux_for_blocks (void);
extern void free_aux_for_blocks (void);
extern void alloc_aux_for_edge (edge, int);
extern void alloc_aux_for_edges (int);
extern void clear_aux_for_edges (void);
extern void free_aux_for_edges (void);




extern void verify_flow_info (void);
extern int flow_loop_outside_edge_p (const struct loop *, edge);

typedef struct conflict_graph_def *conflict_graph;




typedef int (*conflict_graph_enum_fn) (int, int, void *);




extern conflict_graph conflict_graph_new
                                        (int);
extern void conflict_graph_delete (conflict_graph);
extern int conflict_graph_add (conflict_graph, int, int);

extern int conflict_graph_conflict_p (conflict_graph, int, int);

extern void conflict_graph_enum (conflict_graph, int, conflict_graph_enum_fn, void *);


extern void conflict_graph_merge_regs (conflict_graph, int, int);

extern void conflict_graph_print (conflict_graph, FILE*);
extern conflict_graph conflict_graph_compute
                                        (regset, partition);

extern _Bool mark_dfs_back_edges (void);
extern void update_br_prob_note (basic_block);
extern void fixup_abnormal_edges (void);



enum cdi_direction
{
  CDI_DOMINATORS,
  CDI_POST_DOMINATORS
};

extern void calculate_dominance_info (int *, sbitmap *, enum cdi_direction);
# 32 "sched-deps.c" 2
# 1 "regs.h" 1
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
# 33 "sched-deps.c" 2
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
# 34 "sched-deps.c" 2
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
# 35 "sched-deps.c" 2
# 1 "insn-config.h" 1
# 36 "sched-deps.c" 2
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
# 37 "sched-deps.c" 2
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
# 38 "sched-deps.c" 2

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
# 40 "sched-deps.c" 2
# 1 "sched-int.h" 1
# 24 "sched-int.h"
struct ready_list;


struct deps
{
# 39 "sched-int.h"
  rtx pending_read_insns;


  rtx pending_read_mems;


  rtx pending_write_insns;


  rtx pending_write_mems;





  int pending_lists_length;



  int pending_flush_length;
# 69 "sched-int.h"
  rtx last_pending_memory_flush;




  rtx last_function_call;





  rtx sched_before_next_call;



  _Bool in_post_call_group_p;



  int max_reg;





  struct deps_reg
    {
      rtx uses;
      rtx sets;
      rtx clobbers;
      int uses_length;
      int clobbers_length;
    } *reg_last;



  regset_head reg_last_in_use;
};






struct sched_info
{


  void (*init_ready_list) (struct ready_list *);


  int (*can_schedule_ready_p) (rtx);

  int (*schedule_more_p) (void);



  int (*new_ready) (rtx);



  int (*rank) (rtx, rtx);




  const char *(*print_insn) (rtx, int);


  int (*contributes_to_priority) (rtx, rtx);



  void (*compute_jump_reg_dependencies) (rtx, regset);


  rtx prev_head, next_tail;



  rtx head, tail;


  unsigned int queue_must_finish_empty:1;




  unsigned int use_cselib:1;
};

extern struct sched_info *current_sched_info;




struct haifa_insn_data
{


  rtx depend;



  rtx line_note;


  int luid;


  int priority;




  int dep_count;



  unsigned int blockage;


  int ref_count;



  int tick;

  short cost;


  short units;



  short reg_weight;


  unsigned int cant_move : 1;



  unsigned int fed_by_spec_load : 1;
  unsigned int is_load_insn : 1;


  unsigned int priority_known : 1;
};

extern struct haifa_insn_data *h_i_d;
# 248 "sched-int.h"
extern FILE *sched_dump;
extern int sched_verbose;
# 260 "sched-int.h"
extern void init_target_units (void);
extern void insn_print_units (rtx);
extern void init_block_visualization (void);
extern void print_block_visualization (const char *);
extern void visualize_scheduled_insns (int);
extern void visualize_no_unit (rtx);
extern void visualize_stall_cycles (int);
extern void visualize_alloc (void);
extern void visualize_free (void);


extern void add_dependence (rtx, rtx, enum reg_note);
extern void add_insn_mem_dependence (struct deps *, rtx *, rtx *, rtx, rtx);

extern void sched_analyze (struct deps *, rtx, rtx);
extern void init_deps (struct deps *);
extern void free_deps (struct deps *);
extern void init_deps_global (void);
extern void finish_deps_global (void);
extern void compute_forward_dependences (rtx, rtx);
extern rtx find_insn_list (rtx, rtx);
extern void init_dependency_caches (int);
extern void free_dependency_caches (void);


extern void get_block_head_tail (int, rtx *, rtx *);
extern int no_real_insns_p (rtx, rtx);

extern void rm_line_notes (rtx, rtx);
extern void save_line_notes (int, rtx, rtx);
extern void restore_line_notes (rtx, rtx);
extern void rm_redundant_line_notes (void);
extern void rm_other_notes (rtx, rtx);

extern int insn_issue_delay (rtx);
extern int set_priorities (rtx, rtx);

extern rtx sched_emit_insn (rtx);
extern void schedule_block (int, int);
extern void sched_init (FILE *);
extern void sched_finish (void);

extern void ready_add (struct ready_list *, rtx);



extern int insn_unit (rtx);
extern int insn_cost (rtx, rtx, rtx);
extern rtx get_unit_last_insn (int);
extern int actual_hazard_this_instance (int, int, rtx, int, int);
# 41 "sched-deps.c" 2
# 1 "params.h" 1
# 44 "params.h"
typedef struct param_info
{


  const char *const option;

  int value;

  const char *const help;
} param_info;




extern param_info *compiler_params;



extern void add_params
  (const param_info params[], size_t n);



extern void set_param_value
  (const char *name, int value);




typedef enum compiler_param
{


# 1 "params.def" 1
# 45 "params.def"
PARAM_MAX_INLINE_INSNS,
# 56 "params.def"
PARAM_MAX_DELAY_SLOT_INSN_SEARCH,
# 67 "params.def"
PARAM_MAX_DELAY_SLOT_LIVE_SEARCH,
# 77 "params.def"
PARAM_MAX_PENDING_LIST_LENGTH,







PARAM_MAX_GCSE_MEMORY,




PARAM_MAX_GCSE_PASSES,
# 78 "params.h" 2

  LAST_PARAM
} compiler_param;
# 42 "sched-deps.c" 2
# 1 "cselib.h" 1
# 23 "cselib.h"
typedef struct cselib_val_struct
{

  unsigned int value;
  union
  {

    rtx val_rtx;

    struct cselib_val_struct *next_free;
  } u;



  struct elt_loc_list *locs;


  struct elt_list *addr_list;
} cselib_val;


struct elt_loc_list
{

  struct elt_loc_list *next;

  rtx loc;

  rtx setting_insn;
};


struct elt_list
{
  struct elt_list *next;
  cselib_val *elt;
};

extern cselib_val *cselib_lookup (rtx, enum machine_mode, int);
extern void cselib_update_varray_sizes (void);
extern void cselib_init (void);
extern void cselib_finish (void);
extern void cselib_process_insn (rtx);
extern int rtx_equal_for_cselib_p (rtx, rtx);
extern int references_value_p (rtx, int);
extern rtx cselib_subst_to_values (rtx);
# 43 "sched-deps.c" 2

extern char *reg_known_equiv_p;
extern rtx *reg_known_value;

static regset_head reg_pending_sets_head;
static regset_head reg_pending_clobbers_head;
static regset_head reg_pending_uses_head;

static regset reg_pending_sets;
static regset reg_pending_clobbers;
static regset reg_pending_uses;
static _Bool reg_pending_barrier;
# 69 "sched-deps.c"
static sbitmap *true_dependency_cache;
static sbitmap *anti_dependency_cache;
static sbitmap *output_dependency_cache;
# 81 "sched-deps.c"
static int deps_may_trap_p (rtx);
static void add_dependence_list (rtx, rtx, enum reg_note);
#if 0
static void add_dependence_list_and_free (rtx, rtx *, enum reg_note);
#else
extern void add_dependence_list_and_free (rtx, rtx *, enum reg_note);
#endif
static void remove_dependence (rtx, rtx);
static void set_sched_group_p (rtx);

static void flush_pending_lists (struct deps *, rtx, int, int);
static void sched_analyze_1 (struct deps *, rtx, rtx);
static void sched_analyze_2 (struct deps *, rtx, rtx);
static void sched_analyze_insn (struct deps *, rtx, rtx, rtx);
static rtx group_leader (rtx);

static rtx get_condition (rtx);
static int conditions_mutex_p (rtx, rtx);



static int
deps_may_trap_p (mem)
     rtx mem;
{
  rtx addr = (((mem)->fld[0]).rtx);

  if ((((enum rtx_code) (addr)->code) == REG)
      && (((addr)->fld[0]).rtuint) >= 53
      && reg_known_value[(((addr)->fld[0]).rtuint)])
    addr = reg_known_value[(((addr)->fld[0]).rtuint)];
  return rtx_addr_can_trap_p (addr);
}




rtx
find_insn_list (insn, list)
     rtx insn;
     rtx list;
{
  while (list)
    {
      if ((((list)->fld[0]).rtx) == insn)
        return list;
      list = (((list)->fld[1]).rtx);
    }
  return 0;
}



static rtx
get_condition (insn)
     rtx insn;
{
  rtx pat = (((insn)->fld[3]).rtx);
  rtx cond;

  if (pat == 0)
    return 0;
  if (((enum rtx_code) (pat)->code) == COND_EXEC)
    return (((pat)->fld[0]).rtx);
  if (((enum rtx_code) (insn)->code) != JUMP_INSN)
    return 0;
  if (((enum rtx_code) (pat)->code) != SET || (((pat)->fld[1]).rtx) != (global_rtl[GR_PC]))
    return 0;
  if (((enum rtx_code) ((((pat)->fld[0]).rtx))->code) != IF_THEN_ELSE)
    return 0;
  pat = (((pat)->fld[0]).rtx);
  cond = (((pat)->fld[0]).rtx);
  if (((enum rtx_code) ((((cond)->fld[1]).rtx))->code) == LABEL_REF
      && (((cond)->fld[2]).rtx) == (global_rtl[GR_PC]))
    return cond;
  else if (((enum rtx_code) ((((cond)->fld[2]).rtx))->code) == LABEL_REF
           && (((cond)->fld[1]).rtx) == (global_rtl[GR_PC]))
    return gen_rtx_fmt_ee (reverse_condition (((enum rtx_code) (cond)->code)), ((enum machine_mode) (cond)->mode),
                           (((cond)->fld[0]).rtx), (((cond)->fld[1]).rtx));
  else
    return 0;
}



static int
conditions_mutex_p (cond1, cond2)
     rtx cond1, cond2;
{
  if ((rtx_class[(int) (((enum rtx_code) (cond1)->code))]) == '<'
      && (rtx_class[(int) (((enum rtx_code) (cond2)->code))]) == '<'
      && ((enum rtx_code) (cond1)->code) == reverse_condition (((enum rtx_code) (cond2)->code))
      && (((cond1)->fld[0]).rtx) == (((cond2)->fld[0]).rtx)
      && (((cond1)->fld[1]).rtx) == (((cond2)->fld[1]).rtx))
    return 1;
  return 0;
}





void
add_dependence (insn, elem, dep_type)
     rtx insn;
     rtx elem;
     enum reg_note dep_type;
{
  rtx link, next;
  int present_p;
  rtx cond1, cond2;


  if (insn == elem)
    return;




  if (((enum rtx_code) (elem)->code) == NOTE)
    return;






  if (((enum rtx_code) (insn)->code) != CALL_INSN && ((enum rtx_code) (elem)->code) != CALL_INSN)
    {
      cond1 = get_condition (insn);
      cond2 = get_condition (elem);
      if (cond1 && cond2
          && conditions_mutex_p (cond1, cond2)


          && !modified_in_p (cond1, elem)


          && !modified_in_p (cond2, insn))
        return;
    }






  next = next_nonnote_insn (elem);
  if (next && ((next)->in_struct)
      && ((enum rtx_code) (next)->code) != CODE_LABEL)
    {
# 236 "sched-deps.c"
      rtx nnext;
      while ((nnext = next_nonnote_insn (next)) != ((void *)0)
             && ((nnext)->in_struct)
             && ((enum rtx_code) (nnext)->code) != CODE_LABEL)
        next = nnext;


      if (insn == next)
        return;



      elem = next;
    }

  present_p = 1;
# 268 "sched-deps.c"
  if (true_dependency_cache != ((void *)0))
    {
      enum reg_note present_dep_type = 0;

      if (anti_dependency_cache == ((void *)0) || output_dependency_cache == ((void *)0))
        fancy_abort ("sched-deps.c", 273, __FUNCTION__);
      if (((true_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] >> ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)) & 1))

        ;
      else if (((anti_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] >> ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)) & 1))

        present_dep_type = REG_DEP_ANTI;
      else if (((output_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] >> ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)) & 1))

        present_dep_type = REG_DEP_OUTPUT;
      else
        present_p = 0;
      if (present_p && (int) dep_type >= (int) present_dep_type)
        return;
    }



  if (present_p)
    for (link = (((insn)->fld[5]).rtx); link; link = (((link)->fld[1]).rtx))
      if ((((link)->fld[0]).rtx) == elem)
        {



          if (true_dependency_cache != ((void *)0))
            {
              if (((enum reg_note) ((enum machine_mode) (link)->mode)) == REG_DEP_ANTI)
                ((anti_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] &= ~((unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8))));

              else if (((enum reg_note) ((enum machine_mode) (link)->mode)) == REG_DEP_OUTPUT
                       && output_dependency_cache)
                ((output_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] &= ~((unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8))));

              else
                fancy_abort ("sched-deps.c", 308, __FUNCTION__);
            }




          if ((int) dep_type < (int) ((enum reg_note) ((enum machine_mode) (link)->mode)))
            ((link)->mode = (enum machine_mode) ((enum machine_mode) (dep_type)));




          if (true_dependency_cache != ((void *)0))
            {
              if ((int) ((enum reg_note) ((enum machine_mode) (link)->mode)) == 0)
                ((true_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] |= (unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)));

              else if (((enum reg_note) ((enum machine_mode) (link)->mode)) == REG_DEP_ANTI)
                ((anti_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] |= (unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)));

              else if (((enum reg_note) ((enum machine_mode) (link)->mode)) == REG_DEP_OUTPUT)
                ((output_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] |= (unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)));

            }

          return;
      }


  link = alloc_INSN_LIST (elem, (((insn)->fld[5]).rtx));
  (((insn)->fld[5]).rtx) = link;


  ((link)->mode = (enum machine_mode) ((enum machine_mode) (dep_type)));




  if (true_dependency_cache != ((void *)0))
    {
      if ((int) dep_type == 0)
        ((true_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] |= (unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)));
      else if (dep_type == REG_DEP_ANTI)
        ((anti_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] |= (unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)));
      else if (dep_type == REG_DEP_OUTPUT)
        ((output_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] |= (unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8)));
    }

}



static void
add_dependence_list (insn, list, dep_type)
     rtx insn, list;
     enum reg_note dep_type;
{
  for (; list; list = (((list)->fld[1]).rtx))
    add_dependence (insn, (((list)->fld[0]).rtx), dep_type);
}



#if 0
static void
add_dependence_list_and_free (insn, listp, dep_type)
     rtx insn;
     rtx *listp;
     enum reg_note dep_type;
{
  rtx list, next;
  for (list = *listp, *listp = ((void *)0); list ; list = next)
    {
      next = (((list)->fld[1]).rtx);
      add_dependence (insn, (((list)->fld[0]).rtx), dep_type);
      free_INSN_LIST_node (list);
    }
}
#endif




static void
remove_dependence (insn, elem)
     rtx insn;
     rtx elem;
{
  rtx prev, link, next;
  int found = 0;

  for (prev = 0, link = (((insn)->fld[5]).rtx); link; link = next)
    {
      next = (((link)->fld[1]).rtx);
      if ((((link)->fld[0]).rtx) == elem)
        {
          if (prev)
            (((prev)->fld[1]).rtx) = next;
          else
            (((insn)->fld[5]).rtx) = next;




          if (true_dependency_cache != ((void *)0))
            {
              if (((enum reg_note) ((enum machine_mode) (link)->mode)) == 0)
                ((true_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] &= ~((unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8))));

              else if (((enum reg_note) ((enum machine_mode) (link)->mode)) == REG_DEP_ANTI)
                ((anti_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] &= ~((unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8))));

              else if (((enum reg_note) ((enum machine_mode) (link)->mode)) == REG_DEP_OUTPUT)
                ((output_dependency_cache[(h_i_d[(((insn)->fld[0]).rtint)].luid)])->elms [((h_i_d[(((elem)->fld[0]).rtint)].luid)) / ((unsigned) (8 * 8))] &= ~((unsigned long long) 1 << ((h_i_d[(((elem)->fld[0]).rtint)].luid)) % ((unsigned) (8 * 8))));

            }


          free_INSN_LIST_node (link);

          found = 1;
        }
      else
        prev = link;
    }

  if (!found)
    fancy_abort ("sched-deps.c", 433, __FUNCTION__);
  return;
}




static rtx
group_leader (insn)
     rtx insn;
{
  rtx prev;

  do
    {
      prev = insn;
      insn = next_nonnote_insn (insn);
    }
  while (insn && ((insn)->in_struct) && (((enum rtx_code) (insn)->code) != CODE_LABEL));

  return prev;
}




static void
set_sched_group_p (insn)
     rtx insn;
{
  rtx link, prev;

  ((insn)->in_struct) = 1;




  prev = prev_nonnote_insn (insn);






  if (find_insn_list (prev, (((insn)->fld[5]).rtx)))
    remove_dependence (insn, prev);

  for (link = (((prev)->fld[5]).rtx); link; link = (((link)->fld[1]).rtx))
    add_dependence (insn, (((link)->fld[0]).rtx), ((enum reg_note) ((enum machine_mode) (link)->mode)));
}
# 499 "sched-deps.c"
void
add_insn_mem_dependence (deps, insn_list, mem_list, insn, mem)
     struct deps *deps;
     rtx *insn_list, *mem_list, insn, mem;
{
  rtx link;

  link = alloc_INSN_LIST (insn, *insn_list);
  *insn_list = link;

  if (current_sched_info->use_cselib)
    {
      mem = shallow_copy_rtx (mem);
      (((mem)->fld[0]).rtx) = cselib_subst_to_values ((((mem)->fld[0]).rtx));
    }
  link = alloc_EXPR_LIST (VOIDmode, mem, *mem_list);
  *mem_list = link;

  deps->pending_lists_length++;
}





static void
flush_pending_lists (deps, insn, for_read, for_write)
     struct deps *deps;
     rtx insn;
     int for_read, for_write;
{
  if (for_write)
    {
      add_dependence_list_and_free (insn, &deps->pending_read_insns,
                                    REG_DEP_ANTI);
      free_EXPR_LIST_list (&deps->pending_read_mems);
    }

  add_dependence_list_and_free (insn, &deps->pending_write_insns,
                                for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);
  free_EXPR_LIST_list (&deps->pending_write_mems);
  deps->pending_lists_length = 0;

  add_dependence_list_and_free (insn, &deps->last_pending_memory_flush,
                                for_read ? REG_DEP_ANTI : REG_DEP_OUTPUT);
  deps->last_pending_memory_flush = alloc_INSN_LIST (insn, (rtx) 0);
  deps->pending_flush_length = 1;
}





static void
sched_analyze_1 (deps, x, insn)
     struct deps *deps;
     rtx x;
     rtx insn;
{
  int regno;
  rtx dest = (((x)->fld[0]).rtx);
  enum rtx_code code = ((enum rtx_code) (x)->code);

  if (dest == 0)
    return;

  if (((enum rtx_code) (dest)->code) == PARALLEL)
    {
      int i;

      for (i = (((((dest)->fld[0]).rtvec))->num_elem) - 1; i >= 0; i--)
        if (((((((((dest)->fld[0]).rtvec))->elem[i]))->fld[0]).rtx) != 0)
          sched_analyze_1 (deps,
                           gen_rtx_fmt_e (CLOBBER, (VOIDmode), (((((((((dest)->fld[0]).rtvec))->elem[i]))->fld[0]).rtx))),

                           insn);

      if (((enum rtx_code) (x)->code) == SET)
        sched_analyze_2 (deps, (((x)->fld[1]).rtx), insn);
      return;
    }

  while (((enum rtx_code) (dest)->code) == STRICT_LOW_PART || ((enum rtx_code) (dest)->code) == SUBREG
         || ((enum rtx_code) (dest)->code) == ZERO_EXTRACT || ((enum rtx_code) (dest)->code) == SIGN_EXTRACT)
    {
      if (((enum rtx_code) (dest)->code) == ZERO_EXTRACT || ((enum rtx_code) (dest)->code) == SIGN_EXTRACT)
        {

          sched_analyze_2 (deps, (((dest)->fld[1]).rtx), insn);
          sched_analyze_2 (deps, (((dest)->fld[2]).rtx), insn);
        }
      dest = (((dest)->fld[0]).rtx);
    }

  if (((enum rtx_code) (dest)->code) == REG)
    {
      regno = (((dest)->fld[0]).rtuint);



      if (regno < 53)
        {
          int i = (((regno) >= 8 && (regno) <= (8 + 7)) || (((regno) >= (20 + 1) && (regno) <= ((20 + 1) + 7)) || ((regno) >= (((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) && (regno) <= ((((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) + 7))) || ((regno) >= (((20 + 1) + 7) + 1) && (regno) <= ((((20 + 1) + 7) + 1) + 7)) ? (((mode_class[(int) (((enum machine_mode) (dest)->mode))]) == MODE_COMPLEX_INT || (mode_class[(int) (((enum machine_mode) (dest)->mode))]) == MODE_COMPLEX_FLOAT) ? 2 : 1) : ((((enum machine_mode) (dest)->mode)) == TFmode ? ((target_flags & 0x02000000) ? 2 : 3) : (((enum machine_mode) (dest)->mode)) == TCmode ? ((target_flags & 0x02000000) ? 4 : 6) : (((mode_size[(int) (((enum machine_mode) (dest)->mode))]) + ((target_flags & 0x02000000) ? 8 : 4) - 1) / ((target_flags & 0x02000000) ? 8 : 4))));
          if (code == SET)
            {
              while (--i >= 0)
                bitmap_set_bit (reg_pending_sets, regno + i);
            }
          else
            {
              while (--i >= 0)
                bitmap_set_bit (reg_pending_clobbers, regno + i);
            }
        }



      else if (regno >= deps->max_reg)
        {
          if (((enum rtx_code) ((((insn)->fld[3]).rtx))->code) != USE
              && ((enum rtx_code) ((((insn)->fld[3]).rtx))->code) != CLOBBER)
            fancy_abort ("sched-deps.c", 620, __FUNCTION__);
        }
      else
        {
          if (code == SET)
            bitmap_set_bit (reg_pending_sets, regno);
          else
            bitmap_set_bit (reg_pending_clobbers, regno);




          if (!reload_completed
              && reg_known_equiv_p[regno]
              && ((enum rtx_code) (reg_known_value[regno])->code) == MEM)
            sched_analyze_2 (deps, (((reg_known_value[regno])->fld[0]).rtx), insn);



          if ((((reg_n_info)->data.reg[regno])->calls_crossed) == 0)
            add_dependence_list (insn, deps->last_function_call, REG_DEP_ANTI);
        }
    }
  else if (((enum rtx_code) (dest)->code) == MEM)
    {

      rtx t = dest;

      if (current_sched_info->use_cselib)
        {
          t = shallow_copy_rtx (dest);
          cselib_lookup ((((t)->fld[0]).rtx), ((target_flags & 0x02000000) ? DImode : SImode), 1);
          (((t)->fld[0]).rtx) = cselib_subst_to_values ((((t)->fld[0]).rtx));
        }

      if (deps->pending_lists_length > (compiler_params[(int) PARAM_MAX_PENDING_LIST_LENGTH].value))
        {





          flush_pending_lists (deps, insn, 0, 1);
        }
      else
        {
          rtx pending, pending_mem;

          pending = deps->pending_read_insns;
          pending_mem = deps->pending_read_mems;
          while (pending)
            {
              if (anti_dependence ((((pending_mem)->fld[0]).rtx), t))
                add_dependence (insn, (((pending)->fld[0]).rtx), REG_DEP_ANTI);

              pending = (((pending)->fld[1]).rtx);
              pending_mem = (((pending_mem)->fld[1]).rtx);
            }

          pending = deps->pending_write_insns;
          pending_mem = deps->pending_write_mems;
          while (pending)
            {
              if (output_dependence ((((pending_mem)->fld[0]).rtx), t))
                add_dependence (insn, (((pending)->fld[0]).rtx), REG_DEP_OUTPUT);

              pending = (((pending)->fld[1]).rtx);
              pending_mem = (((pending_mem)->fld[1]).rtx);
            }

          add_dependence_list (insn, deps->last_pending_memory_flush,
                               REG_DEP_ANTI);

          add_insn_mem_dependence (deps, &deps->pending_write_insns,
                                   &deps->pending_write_mems, insn, dest);
        }
      sched_analyze_2 (deps, (((dest)->fld[0]).rtx), insn);
    }


  if (((enum rtx_code) (x)->code) == SET)
    sched_analyze_2 (deps, (((x)->fld[1]).rtx), insn);
}



static void
sched_analyze_2 (deps, x, insn)
     struct deps *deps;
     rtx x;
     rtx insn;
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0)
    return;

  code = ((enum rtx_code) (x)->code);

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:



      return;
# 742 "sched-deps.c"
    case REG:
      {
        int regno = (((x)->fld[0]).rtuint);
        if (regno < 53)
          {
            int i = (((regno) >= 8 && (regno) <= (8 + 7)) || (((regno) >= (20 + 1) && (regno) <= ((20 + 1) + 7)) || ((regno) >= (((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) && (regno) <= ((((((((20 + 1) + 7) + 1) + 7) + 1) + 7) + 1) + 7))) || ((regno) >= (((20 + 1) + 7) + 1) && (regno) <= ((((20 + 1) + 7) + 1) + 7)) ? (((mode_class[(int) (((enum machine_mode) (x)->mode))]) == MODE_COMPLEX_INT || (mode_class[(int) (((enum machine_mode) (x)->mode))]) == MODE_COMPLEX_FLOAT) ? 2 : 1) : ((((enum machine_mode) (x)->mode)) == TFmode ? ((target_flags & 0x02000000) ? 2 : 3) : (((enum machine_mode) (x)->mode)) == TCmode ? ((target_flags & 0x02000000) ? 4 : 6) : (((mode_size[(int) (((enum machine_mode) (x)->mode))]) + ((target_flags & 0x02000000) ? 8 : 4) - 1) / ((target_flags & 0x02000000) ? 8 : 4))));
            while (--i >= 0)
              bitmap_set_bit (reg_pending_uses, regno + i);
          }



        else if (regno >= deps->max_reg)
          {
            if (((enum rtx_code) ((((insn)->fld[3]).rtx))->code) != USE
                && ((enum rtx_code) ((((insn)->fld[3]).rtx))->code) != CLOBBER)
              fancy_abort ("sched-deps.c", 758, __FUNCTION__);
          }
        else
          {
            bitmap_set_bit (reg_pending_uses, regno);




            if (!reload_completed
                && reg_known_equiv_p[regno]
                && ((enum rtx_code) (reg_known_value[regno])->code) == MEM)
              sched_analyze_2 (deps, (((reg_known_value[regno])->fld[0]).rtx), insn);




            if ((((reg_n_info)->data.reg[regno])->calls_crossed) == 0)
              deps->sched_before_next_call
                = alloc_INSN_LIST (insn, deps->sched_before_next_call);
          }
        return;
      }

    case MEM:
      {

        rtx u;
        rtx pending, pending_mem;
        rtx t = x;

        if (current_sched_info->use_cselib)
          {
            t = shallow_copy_rtx (t);
            cselib_lookup ((((t)->fld[0]).rtx), ((target_flags & 0x02000000) ? DImode : SImode), 1);
            (((t)->fld[0]).rtx) = cselib_subst_to_values ((((t)->fld[0]).rtx));
          }
        pending = deps->pending_read_insns;
        pending_mem = deps->pending_read_mems;
        while (pending)
          {
            if (read_dependence ((((pending_mem)->fld[0]).rtx), t))
              add_dependence (insn, (((pending)->fld[0]).rtx), REG_DEP_ANTI);

            pending = (((pending)->fld[1]).rtx);
            pending_mem = (((pending_mem)->fld[1]).rtx);
          }

        pending = deps->pending_write_insns;
        pending_mem = deps->pending_write_mems;
        while (pending)
          {
            if (true_dependence ((((pending_mem)->fld[0]).rtx), VOIDmode,
                                 t, rtx_varies_p))
              add_dependence (insn, (((pending)->fld[0]).rtx), 0);

            pending = (((pending)->fld[1]).rtx);
            pending_mem = (((pending_mem)->fld[1]).rtx);
          }

        for (u = deps->last_pending_memory_flush; u; u = (((u)->fld[1]).rtx))
          if (((enum rtx_code) ((((u)->fld[0]).rtx))->code) != JUMP_INSN
              || deps_may_trap_p (x))
            add_dependence (insn, (((u)->fld[0]).rtx), REG_DEP_ANTI);



        add_insn_mem_dependence (deps, &deps->pending_read_insns,
                                 &deps->pending_read_mems, insn, x);


        sched_analyze_2 (deps, (((x)->fld[0]).rtx), insn);
        return;
      }


    case TRAP_IF:
      flush_pending_lists (deps, insn, 1, 0);
      break;

    case ASM_OPERANDS:
    case ASM_INPUT:
    case UNSPEC_VOLATILE:
      {







        if (code != ASM_OPERANDS || ((x)->volatil))
          reg_pending_barrier = 1;






        if (code == ASM_OPERANDS)
          {
            for (j = 0; j < (((((x)->fld[3]).rtvec))->num_elem); j++)
              sched_analyze_2 (deps, (((((x)->fld[3]).rtvec))->elem[j]), insn);
            return;
          }
        break;
      }

    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:






      sched_analyze_2 (deps, (((x)->fld[0]).rtx), insn);
      sched_analyze_1 (deps, x, insn);
      return;

    case POST_MODIFY:
    case PRE_MODIFY:

      sched_analyze_2 (deps, (((x)->fld[0]).rtx), insn);
      sched_analyze_2 (deps, (((x)->fld[1]).rtx), insn);
      sched_analyze_1 (deps, x, insn);
      return;

    default:
      break;
    }


  fmt = (rtx_format[(int) (code)]);
  for (i = (rtx_length[(int) (code)]) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
        sched_analyze_2 (deps, (((x)->fld[i]).rtx), insn);
      else if (fmt[i] == 'E')
        for (j = 0; j < (((((x)->fld[i]).rtvec))->num_elem); j++)
          sched_analyze_2 (deps, (((((x)->fld[i]).rtvec))->elem[j]), insn);
    }
}




static void
sched_analyze_insn (deps, x, insn, loop_notes)
     struct deps *deps;
     rtx x, insn;
     rtx loop_notes;
{
  enum rtx_code code = ((enum rtx_code) (x)->code);
  rtx link;
  int i;

  if (code == COND_EXEC)
    {
      sched_analyze_2 (deps, (((x)->fld[0]).rtx), insn);



      x = (((x)->fld[1]).rtx);
      code = ((enum rtx_code) (x)->code);
    }
  if (code == SET || code == CLOBBER)
    sched_analyze_1 (deps, x, insn);
  else if (code == PARALLEL)
    {
      int i;
      for (i = (((((x)->fld[0]).rtvec))->num_elem) - 1; i >= 0; i--)
        {
          rtx sub = (((((x)->fld[0]).rtvec))->elem[i]);
          code = ((enum rtx_code) (sub)->code);

          if (code == COND_EXEC)
            {
              sched_analyze_2 (deps, (((sub)->fld[0]).rtx), insn);
              sub = (((sub)->fld[1]).rtx);
              code = ((enum rtx_code) (sub)->code);
            }
          if (code == SET || code == CLOBBER)
            sched_analyze_1 (deps, sub, insn);
          else
            sched_analyze_2 (deps, sub, insn);
        }
    }
  else
    sched_analyze_2 (deps, x, insn);


  if (((enum rtx_code) (insn)->code) == CALL_INSN)
    {
      for (link = (((insn)->fld[7]).rtx); link; link = (((link)->fld[1]).rtx))
        {
          if (((enum rtx_code) ((((link)->fld[0]).rtx))->code) == CLOBBER)
            sched_analyze_1 (deps, (((link)->fld[0]).rtx), insn);
          else
            sched_analyze_2 (deps, (((link)->fld[0]).rtx), insn);
        }
      if (find_reg_note (insn, REG_SETJMP, ((void *)0)))
        reg_pending_barrier = 1;
    }

  if (((enum rtx_code) (insn)->code) == JUMP_INSN)
    {
      rtx next;
      next = next_nonnote_insn (insn);
      if (next && ((enum rtx_code) (next)->code) == BARRIER)
        reg_pending_barrier = 1;
      else
        {
          rtx pending, pending_mem;
          regset_head tmp;
          bitmap_initialize (&tmp);

          (*current_sched_info->compute_jump_reg_dependencies) (insn, &tmp);
          bitmap_operation (reg_pending_uses, reg_pending_uses, &tmp, BITMAP_IOR);
          bitmap_clear (&tmp);





          pending = deps->pending_write_insns;
          pending_mem = deps->pending_write_mems;
          while (pending)
            {
              add_dependence (insn, (((pending)->fld[0]).rtx), REG_DEP_OUTPUT);
              pending = (((pending)->fld[1]).rtx);
              pending_mem = (((pending_mem)->fld[1]).rtx);
            }

          pending = deps->pending_read_insns;
          pending_mem = deps->pending_read_mems;
          while (pending)
            {
              if ((((((pending_mem)->fld[0]).rtx))->volatil))
                add_dependence (insn, (((pending)->fld[0]).rtx), REG_DEP_OUTPUT);
              pending = (((pending)->fld[1]).rtx);
              pending_mem = (((pending_mem)->fld[1]).rtx);
            }

          add_dependence_list (insn, deps->last_pending_memory_flush,
                               REG_DEP_ANTI);
        }
    }





  if (loop_notes)
    {
      rtx link;




      link = loop_notes;
      while ((((link)->fld[1]).rtx))
        {
          if (((((((link)->fld[0]).rtx))->fld[0]).rtwint) == NOTE_INSN_LOOP_BEG
              || ((((((link)->fld[0]).rtx))->fld[0]).rtwint) == NOTE_INSN_LOOP_END
              || ((((((link)->fld[0]).rtx))->fld[0]).rtwint) == NOTE_INSN_EH_REGION_BEG
              || ((((((link)->fld[0]).rtx))->fld[0]).rtwint) == NOTE_INSN_EH_REGION_END)
            reg_pending_barrier = 1;

          link = (((link)->fld[1]).rtx);
        }
      (((link)->fld[1]).rtx) = (((insn)->fld[6]).rtx);
      (((insn)->fld[6]).rtx) = loop_notes;
    }




  if (can_throw_internal (insn))
    reg_pending_barrier = 1;

  if (reg_pending_barrier)
    {
      if (((enum rtx_code) ((((insn)->fld[3]).rtx))->code) == COND_EXEC)
        {
          do { bitmap_element *ptr_ = (&deps->reg_last_in_use)->first; unsigned int indx_ = (0) / ((unsigned) (2 * (8 * 8))); unsigned bit_num_ = (0) % ((unsigned) (8 * 8)); unsigned word_num_ = (((0) / ((unsigned) (8 * 8))) % 2); while (ptr_ != 0 && ptr_->indx < indx_) ptr_ = ptr_->next; if (ptr_ != 0 && ptr_->indx != indx_) { bit_num_ = 0; word_num_ = 0; } for (; ptr_ != 0; ptr_ = ptr_->next) { for (; word_num_ < 2; word_num_++) { unsigned long long word_ = ptr_->bits[word_num_]; if (word_ != 0) { for (; bit_num_ < (8 * 8); bit_num_++) { unsigned long long mask_ = ((unsigned long long) 1) << bit_num_; if ((word_ & mask_) != 0) { word_ &= ~ mask_; (i) = (ptr_->indx * ((unsigned) (2 * (8 * 8))) + word_num_ * (8 * 8) + bit_num_); { struct deps_reg *reg_last = &deps->reg_last[i]; add_dependence_list (insn, reg_last->uses, REG_DEP_ANTI); add_dependence_list (insn, reg_last->sets, 0); add_dependence_list (insn, reg_last->clobbers, 0); }; if (word_ == 0) break; } } } bit_num_ = 0; } word_num_ = 0; } } while (0);






        }
      else
        {
          do { 
		bitmap_element *ptr_ = (&deps->reg_last_in_use)->first; 
		unsigned int indx_ = (0) / ((unsigned) (2 * (8 * 8))); 
		unsigned bit_num_ = (0) % ((unsigned) (8 * 8)); 
		unsigned word_num_ = (((0) / ((unsigned) (8 * 8))) % 2); 
		while (ptr_ != 0 && ptr_->indx < indx_) ptr_ = ptr_->next; 
		if (ptr_ != 0 && ptr_->indx != indx_) 
			{ bit_num_ = 0; word_num_ = 0; } 
		for (; ptr_ != 0; ptr_ = ptr_->next) { 
			for (; word_num_ < 2; word_num_++) { 
				unsigned long long word_ = ptr_->bits[word_num_]; 
				if (word_ != 0) { 
					for (; bit_num_ < (8 * 8); bit_num_++) { 
						unsigned long long mask_ = ((unsigned long long) 1) << bit_num_; 
						if ((word_ & mask_) != 0) { 
							word_ &= ~ mask_; 
							(i) = (ptr_->indx * ((unsigned) (2 * (8 * 8))) + word_num_ * (8 * 8) + bit_num_); 
							{ 
							struct deps_reg *reg_last = &deps->reg_last[i]; 
							add_dependence_list_and_free (insn, &reg_last->uses, REG_DEP_ANTI); 
							add_dependence_list_and_free (insn, &reg_last->sets, 0); 
							add_dependence_list_and_free (insn, &reg_last->clobbers, 0); 
							reg_last->uses_length = 0; 
							reg_last->clobbers_length = 0; 
							}; 
							if (word_ == 0) break; 
						} 
					} 
				} 
			bit_num_ = 0; 
		} 
		word_num_ = 0; 
		} 
	    } while (0);
# 1065 "sched-deps.c"
        }

      for (i = 0; i < deps->max_reg; i++)
        {
          struct deps_reg *reg_last = &deps->reg_last[i];
          reg_last->sets = alloc_INSN_LIST (insn, reg_last->sets);
          bitmap_set_bit (&deps->reg_last_in_use, i);
        }

      flush_pending_lists (deps, insn, 1, 1);
      reg_pending_barrier = 0;
    }
  else
    {


      bitmap_operation (&deps->reg_last_in_use, &deps->reg_last_in_use, reg_pending_uses, BITMAP_IOR);
      bitmap_operation (&deps->reg_last_in_use, &deps->reg_last_in_use, reg_pending_clobbers, BITMAP_IOR);
      bitmap_operation (&deps->reg_last_in_use, &deps->reg_last_in_use, reg_pending_sets, BITMAP_IOR);
    }
  bitmap_clear (reg_pending_uses);
  bitmap_clear (reg_pending_clobbers);
  bitmap_clear (reg_pending_sets);
# 1175 "sched-deps.c"
/* deleting from here to end collapsed it */
  if (deps->in_post_call_group_p)
    {
      rtx tmp, set = (((rtx_class[(int) (((enum rtx_code) (insn)->code))]) == 'i') ? (((enum rtx_code) ((((insn)->fld[3]).rtx))->code) == SET ? (((insn)->fld[3]).rtx) : single_set_2 (insn, (((insn)->fld[3]).rtx))) : (rtx) 0);
      int src_regno, dest_regno;

      if (set == ((void *)0))
        goto end_call_group;

      tmp = (((set)->fld[0]).rtx);
      if (((enum rtx_code) (tmp)->code) == SUBREG)
        tmp = (((tmp)->fld[0]).rtx);
      if (((enum rtx_code) (tmp)->code) == REG)
        dest_regno = (((tmp)->fld[0]).rtuint);
      else
        goto end_call_group;

      tmp = (((set)->fld[1]).rtx);
      if (((enum rtx_code) (tmp)->code) == SUBREG)
        tmp = (((tmp)->fld[0]).rtx);
      if (((enum rtx_code) (tmp)->code) == REG)
        src_regno = (((tmp)->fld[0]).rtuint);
      else
        goto end_call_group;

      if (src_regno < 53
          || dest_regno < 53)
        {
          set_sched_group_p (insn);
          (h_i_d[(((insn)->fld[0]).rtint)].cant_move) = 1;
        }
      else
        {
        end_call_group:
          deps->in_post_call_group_p = 0;
        }
    }
}



void
sched_analyze (deps, head, tail)
     struct deps *deps;
     rtx head, tail;
{
  rtx insn;
  rtx loop_notes = 0;

  if (current_sched_info->use_cselib)
    cselib_init ();

  for (insn = head;; insn = (((insn)->fld[2]).rtx))
    {
      if (((enum rtx_code) (insn)->code) == INSN || ((enum rtx_code) (insn)->code) == JUMP_INSN)
        {

          free_INSN_LIST_list (&(((insn)->fld[5]).rtx));


          ((insn)->in_struct) = 0;



          if (((enum rtx_code) (insn)->code) == JUMP_INSN)
            {

              if (deps->pending_flush_length++ > (compiler_params[(int) PARAM_MAX_PENDING_LIST_LENGTH].value))
                flush_pending_lists (deps, insn, 1, 1);
              else
                deps->last_pending_memory_flush
                  = alloc_INSN_LIST (insn, deps->last_pending_memory_flush);
            }
          sched_analyze_insn (deps, (((insn)->fld[3]).rtx), insn, loop_notes);
          loop_notes = 0;
        }
      else if (((enum rtx_code) (insn)->code) == CALL_INSN)
        {
          int i;


          ((insn)->in_struct) = 0;

          (h_i_d[(((insn)->fld[0]).rtint)].cant_move) = 1;


          free_INSN_LIST_list (&(((insn)->fld[5]).rtx));

          if (find_reg_note (insn, REG_SETJMP, ((void *)0)))
            {


              reg_pending_barrier = 1;
            }
          else
            {
              for (i = 0; i < 53; i++)

                if (global_regs[i])
                  {
                    bitmap_set_bit (reg_pending_sets, i);
                    bitmap_set_bit (reg_pending_uses, i);
                  }

                else if (((regs_invalidated_by_call) & (((HARD_REG_ELT_TYPE) (1)) << (i))))
                  bitmap_set_bit (reg_pending_clobbers, i);



                else if (fixed_regs[i])
                  bitmap_set_bit (reg_pending_uses, i);





                else if (i == 20
                         || (i == 6
                             && (! reload_completed || frame_pointer_needed)))
                  bitmap_set_bit (reg_pending_uses, i);
            }



          add_dependence_list_and_free (insn, &deps->sched_before_next_call,
                                        REG_DEP_ANTI);

          sched_analyze_insn (deps, (((insn)->fld[3]).rtx), insn, loop_notes);
          loop_notes = 0;





          flush_pending_lists (deps, insn, 1, !((insn)->unchanging));


          free_INSN_LIST_list (&deps->last_function_call);
          deps->last_function_call = alloc_INSN_LIST (insn, (rtx) 0);



          if (! reload_completed)
            deps->in_post_call_group_p = 1;
        }




      else if (((enum rtx_code) (insn)->code) == NOTE
               && ((((insn)->fld[4]).rtint) == NOTE_INSN_RANGE_BEG
                   || (((insn)->fld[4]).rtint) == NOTE_INSN_RANGE_END))
        {
          loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE, (((insn)->fld[3]).rtx),
                                        loop_notes);
          loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
                                        gen_rtx_CONST_INT (VOIDmode, (long long) ((((insn)->fld[4]).rtint))),
                                        loop_notes);
        }
      else if (((enum rtx_code) (insn)->code) == NOTE
               && ((((insn)->fld[4]).rtint) == NOTE_INSN_LOOP_BEG
                   || (((insn)->fld[4]).rtint) == NOTE_INSN_LOOP_END
                   || (((insn)->fld[4]).rtint) == NOTE_INSN_EH_REGION_BEG
                   || (((insn)->fld[4]).rtint) == NOTE_INSN_EH_REGION_END))
        {
          rtx rtx_region;

          if ((((insn)->fld[4]).rtint) == NOTE_INSN_EH_REGION_BEG
              || (((insn)->fld[4]).rtint) == NOTE_INSN_EH_REGION_END)
            rtx_region = gen_rtx_CONST_INT (VOIDmode, (long long) ((((insn)->fld[3]).rtint)));
          else
            rtx_region = gen_rtx_CONST_INT (VOIDmode, (long long) (0));

          loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
                                        rtx_region,
                                        loop_notes);
          loop_notes = alloc_EXPR_LIST (REG_SAVE_NOTE,
                                        gen_rtx_CONST_INT (VOIDmode, (long long) ((((insn)->fld[4]).rtint))),
                                        loop_notes);
          ((loop_notes)->unchanging) = ((insn)->unchanging);
        }

      if (current_sched_info->use_cselib)
        cselib_process_insn (insn);
      if (insn == tail)
        {
          if (current_sched_info->use_cselib)
            cselib_finish ();
          return;
        }
    }
  fancy_abort ("sched-deps.c", 1366, __FUNCTION__);
}





void
compute_forward_dependences (head, tail)
     rtx head, tail;
{
  rtx insn, link;
  rtx next_tail;
  enum reg_note dep_type;

  next_tail = (((tail)->fld[2]).rtx);
  for (insn = head; insn != next_tail; insn = (((insn)->fld[2]).rtx))
    {
      if (! ((rtx_class[(int) (((enum rtx_code) (insn)->code))]) == 'i'))
        continue;

      insn = group_leader (insn);

      for (link = (((insn)->fld[5]).rtx); link; link = (((link)->fld[1]).rtx))
        {
          rtx x = group_leader ((((link)->fld[0]).rtx));
          rtx new_link;

          if (x != (((link)->fld[0]).rtx))
            continue;
# 1417 "sched-deps.c"
          new_link = alloc_INSN_LIST (insn, (h_i_d[(((x)->fld[0]).rtint)].depend));

          dep_type = ((enum reg_note) ((enum machine_mode) (link)->mode));
          ((new_link)->mode = (enum machine_mode) ((enum machine_mode) (dep_type)));

          (h_i_d[(((x)->fld[0]).rtint)].depend) = new_link;
          (h_i_d[(((insn)->fld[0]).rtint)].dep_count) += 1;
        }
    }
}




void
init_deps (deps)
     struct deps *deps;
{
  int max_reg = (reload_completed ? 53 : max_reg_num ());

  deps->max_reg = max_reg;
  deps->reg_last = (struct deps_reg *)
    xcalloc (max_reg, sizeof (struct deps_reg));
  bitmap_initialize (&deps->reg_last_in_use);

  deps->pending_read_insns = 0;
  deps->pending_read_mems = 0;
  deps->pending_write_insns = 0;
  deps->pending_write_mems = 0;
  deps->pending_lists_length = 0;
  deps->pending_flush_length = 0;
  deps->last_pending_memory_flush = 0;
  deps->last_function_call = 0;
  deps->sched_before_next_call = 0;
  deps->in_post_call_group_p = 0;
}



void
free_deps (deps)
     struct deps *deps;
{
  int i;

  free_INSN_LIST_list (&deps->pending_read_insns);
  free_EXPR_LIST_list (&deps->pending_read_mems);
  free_INSN_LIST_list (&deps->pending_write_insns);
  free_EXPR_LIST_list (&deps->pending_write_mems);
  free_INSN_LIST_list (&deps->last_pending_memory_flush);




  do { bitmap_element *ptr_ = (&deps->reg_last_in_use)->first; unsigned int indx_ = (0) / ((unsigned) (2 * (8 * 8))); unsigned bit_num_ = (0) % ((unsigned) (8 * 8)); unsigned word_num_ = (((0) / ((unsigned) (8 * 8))) % 2); while (ptr_ != 0 && ptr_->indx < indx_) ptr_ = ptr_->next; if (ptr_ != 0 && ptr_->indx != indx_) { bit_num_ = 0; word_num_ = 0; } for (; ptr_ != 0; ptr_ = ptr_->next) { for (; word_num_ < 2; word_num_++) { unsigned long long word_ = ptr_->bits[word_num_]; if (word_ != 0) { for (; bit_num_ < (8 * 8); bit_num_++) { unsigned long long mask_ = ((unsigned long long) 1) << bit_num_; if ((word_ & mask_) != 0) { word_ &= ~ mask_; (i) = (ptr_->indx * ((unsigned) (2 * (8 * 8))) + word_num_ * (8 * 8) + bit_num_); { struct deps_reg *reg_last = &deps->reg_last[i]; free_INSN_LIST_list (&reg_last->uses); free_INSN_LIST_list (&reg_last->sets); free_INSN_LIST_list (&reg_last->clobbers); }; if (word_ == 0) break; } } } bit_num_ = 0; } word_num_ = 0; } } while (0);






  bitmap_clear (&deps->reg_last_in_use);

  free (deps->reg_last);
}





void
init_dependency_caches (luid)
     int luid;
{






  if (luid / n_basic_blocks > 100 * 5)
    {
      true_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (true_dependency_cache, luid);
      anti_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (anti_dependency_cache, luid);
      output_dependency_cache = sbitmap_vector_alloc (luid, luid);
      sbitmap_vector_zero (output_dependency_cache, luid);




    }
}



void
free_dependency_caches ()
{
  if (true_dependency_cache)
    {
      free(true_dependency_cache);
      true_dependency_cache = ((void *)0);
      free(anti_dependency_cache);
      anti_dependency_cache = ((void *)0);
      free(output_dependency_cache);
      output_dependency_cache = ((void *)0);




    }
}




void
init_deps_global ()
{
  reg_pending_sets = bitmap_initialize (&reg_pending_sets_head);
  reg_pending_clobbers = bitmap_initialize (&reg_pending_clobbers_head);
  reg_pending_uses = bitmap_initialize (&reg_pending_uses_head);
  reg_pending_barrier = 0;
}



void
finish_deps_global ()
{
  do { if (reg_pending_sets) { bitmap_clear (reg_pending_sets); (reg_pending_sets) = 0; } } while (0);
  do { if (reg_pending_clobbers) { bitmap_clear (reg_pending_clobbers); (reg_pending_clobbers) = 0; } } while (0);
  do { if (reg_pending_uses) { bitmap_clear (reg_pending_uses); (reg_pending_uses) = 0; } } while (0);
}


