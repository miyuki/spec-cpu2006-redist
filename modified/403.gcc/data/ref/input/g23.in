struct rtx_def;
typedef struct rtx_def *rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
union tree_node;
typedef union tree_node *tree;
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
typedef struct ix86_args {
  int words;
  int nregs;
  int regno;
  int sse_words;
  int sse_nregs;
  int sse_regno;
  int maybe_vaarg;
} CUMULATIVE_ARGS;
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
extern int const dbx_register_map[53];
extern int const dbx64_register_map[53];
extern int const svr4_dbx_register_map[53];
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
enum fp_cw_mode {FP_CW_STORED, FP_CW_UNINITIALIZED, FP_CW_ANY};
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
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
typedef long int ptrdiff_t;
typedef long unsigned int size_t;
typedef int wchar_t;
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
typedef struct _IO_FILE FILE;





typedef struct _IO_FILE __FILE;
typedef unsigned int wint_t;
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;

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
struct _IO_jump_t; struct _IO_FILE;
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
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
extern int _IO_getc (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_putc (int __c, _IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__));

extern int _IO_peekc_locked (_IO_FILE *__fp) __attribute__ ((__nothrow__));





extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
                        __gnuc_va_list, int *__restrict) __attribute__ ((__nothrow__));
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
                         __gnuc_va_list) __attribute__ ((__nothrow__));
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) __attribute__ ((__nothrow__));
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) __attribute__ ((__nothrow__));

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) __attribute__ ((__nothrow__));
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) __attribute__ ((__nothrow__));

extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__));
typedef _G_fpos_t fpos_t;





typedef _G_fpos64_t fpos64_t;
extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;









extern int remove (__const char *__filename) __attribute__ ((__nothrow__));

extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));









extern FILE *tmpfile (void);
extern FILE *tmpfile64 (void);



extern char *tmpnam (char *__s) __attribute__ ((__nothrow__));





extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__));
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);

extern int fflush_unlocked (FILE *__stream);
extern int fcloseall (void);









extern FILE *fopen (__const char *__restrict __filename,
                    __const char *__restrict __modes);




extern FILE *freopen (__const char *__restrict __filename,
                      __const char *__restrict __modes,
                      FILE *__restrict __stream);


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

extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);

extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);






extern char *gets (char *__s);

extern char *fgets_unlocked (char *__restrict __s, int __n,
                             FILE *__restrict __stream);
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

extern int fputs_unlocked (__const char *__restrict __s,
                           FILE *__restrict __stream);
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
                              size_t __n, FILE *__restrict __stream);
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
                               size_t __n, FILE *__restrict __stream);








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream);




extern void rewind (FILE *__stream);

extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream);






extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, __const fpos_t *__pos);



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






extern int sys_nerr;
extern __const char *__const sys_errlist[];


extern int _sys_nerr;
extern __const char *__const _sys_errlist[];
extern int fileno (FILE *__stream) __attribute__ ((__nothrow__));




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
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
extern const unsigned char _sch_toupper[256];
extern const unsigned char _sch_tolower[256];







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
typedef __clock_t clock_t;



typedef __time_t time_t;



typedef __clockid_t clockid_t;
typedef __timer_t timer_t;
typedef __useconds_t useconds_t;



typedef __suseconds_t suseconds_t;








typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));


typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

typedef int register_t __attribute__ ((__mode__ (__word__)));




typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;



typedef __sigset_t sigset_t;





struct timespec
  {
    __time_t tv_sec;
    long int tv_nsec;
  };

struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
typedef long int __fd_mask;
typedef struct
  {



    __fd_mask fds_bits[1024 / (8 * sizeof (__fd_mask))];





  } fd_set;






typedef __fd_mask fd_mask;

extern int select (int __nfds, fd_set *__restrict __readfds,
                   fd_set *__restrict __writefds,
                   fd_set *__restrict __exceptfds,
                   struct timeval *__restrict __timeout);
extern int pselect (int __nfds, fd_set *__restrict __readfds,
                    fd_set *__restrict __writefds,
                    fd_set *__restrict __exceptfds,
                    const struct timespec *__restrict __timeout,
                    const __sigset_t *__restrict __sigmask);



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
typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;





struct __sched_param
  {
    int __sched_priority;
  };


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
extern int *__errno_location (void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern char *program_invocation_name, *program_invocation_short_name;
typedef int error_t;
extern void *memmove (void *__dest, __const void *__src, size_t __n)
     __attribute__ ((__nothrow__));






extern void *memccpy (void *__restrict __dest, __const void *__restrict __src,
                      int __c, size_t __n)
     __attribute__ ((__nothrow__));






extern void *memchr (__const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern void *rawmemchr (__const void *__s, int __c) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));


extern void *memrchr (__const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__)) __attribute__ ((__pure__));





extern char *strcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__));

extern char *strcat (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__));

extern int strcmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern int strcoll (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern size_t strxfrm (char *__restrict __dest,
                       __const char *__restrict __src, size_t __n) __attribute__ ((__nothrow__));






typedef struct __locale_struct
{

  struct locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
} *__locale_t;
extern int strcoll_l (__const char *__s1, __const char *__s2, __locale_t __l)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__));

extern size_t strxfrm_l (char *__dest, __const char *__src, size_t __n,
                         __locale_t __l) __attribute__ ((__nothrow__));




extern char *strdup (__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));






extern char *strndup (__const char *__string, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));


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
typedef union
  {
    union wait *__uptr;
    int *__iptr;
  } __WAIT_STATUS __attribute__ ((__transparent_union__));


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
extern char *l64a (long int __n) __attribute__ ((__nothrow__));


extern long int a64l (__const char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__pure__));
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



extern void *alloca (size_t __size) __attribute__ ((__nothrow__));






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
extern char *mktemp (char *__template) __attribute__ ((__nothrow__));
extern int mkstemp (char *__template);
extern int mkstemp64 (char *__template);
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__));








extern int system (__const char *__command);







extern char *canonicalize_file_name (__const char *__name) __attribute__ ((__nothrow__));
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
extern time_t timegm (struct tm *__tp) __attribute__ ((__nothrow__));


extern time_t timelocal (struct tm *__tp) __attribute__ ((__nothrow__));


extern int dysize (int __year) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
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
extern int getdate_err;
extern struct tm *getdate (__const char *__string);
extern int getdate_r (__const char *__restrict __string,
                      struct tm *__restrict __resbufp);



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



extern ssize_t readahead (int __fd, __off64_t __offset, size_t __count)
    __attribute__ ((__nothrow__));







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
    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
    long int __unused[3];
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
    long int __unused[3];



  };
extern int stat (__const char *__restrict __file,
                 struct stat *__restrict __buf) __attribute__ ((__nothrow__));



extern int fstat (int __fd, struct stat *__buf) __attribute__ ((__nothrow__));
extern int stat64 (__const char *__restrict __file,
                   struct stat64 *__restrict __buf) __attribute__ ((__nothrow__));
extern int fstat64 (int __fd, struct stat64 *__buf) __attribute__ ((__nothrow__));






extern int lstat (__const char *__restrict __file,
                  struct stat *__restrict __buf) __attribute__ ((__nothrow__));
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
extern int __fxstat (int __ver, int __fildes, struct stat *__stat_buf) __attribute__ ((__nothrow__));
extern int __xstat (int __ver, __const char *__filename,
                    struct stat *__stat_buf) __attribute__ ((__nothrow__));
extern int __lxstat (int __ver, __const char *__filename,
                     struct stat *__stat_buf) __attribute__ ((__nothrow__));
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





extern int fcntl (int __fd, int __cmd, ...);
extern int open (__const char *__file, int __oflag, ...);
extern int open64 (__const char *__file, int __oflag, ...);
extern int creat (__const char *__file, __mode_t __mode);
extern int creat64 (__const char *__file, __mode_t __mode);
extern int lockf (int __fd, int __cmd, __off_t __len);
extern int lockf64 (int __fd, int __cmd, __off64_t __len);







extern int posix_fadvise (int __fd, __off_t __offset, __off_t __len,
                          int __advise) __attribute__ ((__nothrow__));
extern int posix_fadvise64 (int __fd, __off64_t __offset, __off64_t __len,
                            int __advise) __attribute__ ((__nothrow__));
extern int posix_fallocate (int __fd, __off_t __offset, __off_t __len);
extern int posix_fallocate64 (int __fd, __off64_t __offset, __off64_t __len);




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
extern char **buildargv (const char *) __attribute__ ((__malloc__));



extern void freeargv (char **);




extern char **dupargv (char **) __attribute__ ((__malloc__));
extern char *basename (const char *);
extern const char *lbasename (const char *);





extern char *concat (const char *, ...) __attribute__ ((__malloc__));
extern char *reconcat (char *, const char *, ...) __attribute__ ((__malloc__));





extern unsigned long concat_length (const char *, ...);






extern char *concat_copy (char *, const char *, ...);






extern char *concat_copy2 (const char *, ...);



extern char *libiberty_concat_ptr;
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
extern int pexecute (const char *, char * const *, const char *, const char *, char **, char **, int);




extern int pwait (int, int *, int);




extern int asprintf (char **, const char *, ...) __attribute__ ((__format__ (__printf__, 2, 3)));




extern int vasprintf (char **, const char *, va_list)
  __attribute__ ((__format__ (__printf__, 2, 0)));
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
extern int flag_verbose_asm;
extern int flag_debug_asm;

extern int flag_dump_rtl_in_asm;



extern int flag_gnu_linker;


extern int flag_pack_struct;
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
enum machine_mode {
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
CCmode,


CCGCmode, CCGOCmode, CCNOmode, CCZmode, CCFPmode, CCFPUmode,
MAX_MACHINE_MODE };
extern const char * const mode_name[(int) MAX_MACHINE_MODE];


enum mode_class { MODE_RANDOM, MODE_INT, MODE_FLOAT, MODE_PARTIAL_INT, MODE_CC,
                  MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT,
                  MODE_VECTOR_INT, MODE_VECTOR_FLOAT,
                  MAX_MODE_CLASS};




extern const enum mode_class mode_class[(int) MAX_MACHINE_MODE];
extern const unsigned char mode_size[(int) MAX_MACHINE_MODE];




extern const unsigned char mode_unit_size[(int) MAX_MACHINE_MODE];
extern const unsigned short mode_bitsize[(int) MAX_MACHINE_MODE];
extern const unsigned long long mode_mask_array[(int) MAX_MACHINE_MODE];



extern const enum machine_mode inner_mode_array[(int) MAX_MACHINE_MODE];
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
extern const char *const version_string;
enum tree_code {
ERROR_MARK,





IDENTIFIER_NODE,






TREE_LIST,


TREE_VEC,
BLOCK,
VOID_TYPE,
INTEGER_TYPE,



REAL_TYPE,



COMPLEX_TYPE,



VECTOR_TYPE,
ENUMERAL_TYPE,



BOOLEAN_TYPE,



CHAR_TYPE,



POINTER_TYPE,





OFFSET_TYPE,



REFERENCE_TYPE,






METHOD_TYPE,


FILE_TYPE,
ARRAY_TYPE,





SET_TYPE,
RECORD_TYPE,





UNION_TYPE,





QUAL_UNION_TYPE,







FUNCTION_TYPE,





LANG_TYPE,
INTEGER_CST,


REAL_CST,




COMPLEX_CST,


VECTOR_CST,



STRING_CST,
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
BIT_FIELD_REF,


INDIRECT_REF,


BUFFER_REF,



ARRAY_REF,




ARRAY_RANGE_REF,






VTABLE_REF,
CONSTRUCTOR,
COMPOUND_EXPR,


MODIFY_EXPR,



INIT_EXPR,






TARGET_EXPR,
COND_EXPR,
BIND_EXPR,




CALL_EXPR,




METHOD_CALL_EXPR,
WITH_CLEANUP_EXPR,
CLEANUP_POINT_EXPR,
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
LSHIFT_EXPR,
RSHIFT_EXPR,
LROTATE_EXPR,
RROTATE_EXPR,


BIT_IOR_EXPR,
BIT_XOR_EXPR,
BIT_AND_EXPR,
BIT_ANDTC_EXPR,
BIT_NOT_EXPR,
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
VIEW_CONVERT_EXPR,





SAVE_EXPR,
UNSAVE_EXPR,
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
TRY_FINALLY_EXPR,






GOTO_SUBROUTINE_EXPR,







LABEL_EXPR,



GOTO_EXPR,






RETURN_EXPR,



EXIT_EXPR,




LOOP_EXPR,




LABELED_BLOCK_EXPR,




EXIT_BLOCK_EXPR,
EXPR_WITH_FILE_LOCATION,





SWITCH_EXPR,


EXC_PTR_EXPR,
  LAST_AND_UNUSED_TREE_CODE

};
extern char tree_code_type[256];
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
  END_BUILTINS
};



extern const char *const built_in_names[(int) END_BUILTINS];


extern tree built_in_decls[(int) END_BUILTINS];
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
struct tree_int_cst
{
  struct tree_common common;
  rtx rtl;




  struct {
    unsigned long long low;
    long long high;
  } int_cst;
};
typedef struct {
  long long r[(19 + sizeof (long long))/(sizeof (long long))];
} realvaluetype;
extern unsigned int significand_size (enum machine_mode);
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
extern realvaluetype real_value_truncate (enum machine_mode, realvaluetype);
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
extern struct rtx_def *immed_real_const_1 (realvaluetype, enum machine_mode);
extern int exact_real_inverse (enum machine_mode, realvaluetype *);
extern int target_isnan (realvaluetype);
extern int target_isinf (realvaluetype);
extern int target_negative (realvaluetype);
extern void debug_real (realvaluetype);
extern realvaluetype ereal_atof (const char *, enum machine_mode);

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
extern void (*obstack_alloc_failed_handler) (void);





extern int obstack_exit_failure;



typedef struct ht_identifier ht_identifier;
struct ht_identifier
{
  unsigned int len;
  const unsigned char *str;
};
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
struct tree_exp
{
  struct tree_common common;
  int complexity;
  tree operands[1];
};
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
extern tree get_qualified_type (tree, int);




extern tree build_qualified_type (tree, int);
extern tree build_type_copy (tree);





extern void layout_type (tree);
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
extern void fancy_abort (const char *, int, const char *)
    __attribute__ ((__noreturn__));
struct function;

enum rtx_code {


UNKNOWN ,



NIL ,




INCLUDE ,






EXPR_LIST ,



INSN_LIST ,
MATCH_OPERAND ,






MATCH_SCRATCH ,




MATCH_DUP ,







MATCH_OPERATOR ,
MATCH_PARALLEL ,




MATCH_OP_DUP ,




MATCH_PAR_DUP ,




MATCH_INSN ,
DEFINE_INSN ,







DEFINE_PEEPHOLE ,
DEFINE_SPLIT ,
DEFINE_INSN_AND_SPLIT ,



DEFINE_PEEPHOLE2 ,



DEFINE_COMBINE ,
DEFINE_EXPAND ,
DEFINE_DELAY ,
DEFINE_FUNCTION_UNIT ,


DEFINE_ASM_ATTRIBUTES ,
DEFINE_COND_EXEC ,





SEQUENCE ,


ADDRESS ,
DEFINE_ATTR ,


ATTR ,







SET_ATTR ,
SET_ATTR_ALTERNATIVE ,




EQ_ATTR ,
ATTR_FLAG ,
INSN ,



JUMP_INSN ,






CALL_INSN ,


BARRIER ,
CODE_LABEL ,






NOTE ,
COND_EXEC ,


PARALLEL ,







ASM_INPUT ,
ASM_OPERANDS ,
UNSPEC ,


UNSPEC_VOLATILE ,



ADDR_VEC ,
ADDR_DIFF_VEC ,
PREFETCH ,
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
REG ,






SCRATCH ,
SUBREG ,
STRICT_LOW_PART ,





CONCAT ,




MEM ,





LABEL_REF ,





SYMBOL_REF ,






CC0 ,
ADDRESSOF ,
QUEUED ,
IF_THEN_ELSE ,
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
PRE_DEC ,
PRE_INC ,
POST_DEC ,
POST_INC ,
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
SIGN_EXTRACT ,


ZERO_EXTRACT ,




HIGH ,



LO_SUM ,
RANGE_INFO ,
RANGE_REG ,





RANGE_VAR ,



RANGE_LIVE ,




CONSTANT_P_RTX ,
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
PHI ,


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
  unsigned int unchanging : 1;







  unsigned int volatil : 1;
  unsigned int in_struct : 1;






  unsigned int used : 1;




  unsigned integrated : 1;
  unsigned frame_related : 1;




  rtunion fld[1];
};
struct rtvec_def {
  int num_elem;
  rtx elem[1];
};
enum reg_note
{



  REG_DEAD = 1,


  REG_INC,
  REG_EQUIV,




  REG_EQUAL,





  REG_WAS_0,





  REG_RETVAL,




  REG_LIBCALL,






  REG_NONNEG,



  REG_NO_CONFLICT,


  REG_UNUSED,
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
extern const char * const reg_note_name[];
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
extern unsigned int subreg_lsb (rtx);
extern unsigned int subreg_regno_offset (unsigned int, enum machine_mode, unsigned int, enum machine_mode);



extern unsigned int subreg_regno (rtx);
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
enum global_rtl_index
{
  GR_PC,
  GR_CC0,
  GR_STACK_POINTER,
  GR_FRAME_POINTER,
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
extern rtx pic_offset_table_rtx;
extern rtx struct_value_rtx;
extern rtx struct_value_incoming_rtx;
extern rtx static_chain_rtx;
extern rtx static_chain_incoming_rtx;
extern rtx return_address_pointer_rtx;









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
extern rtx gen_rtx_CONST_DOUBLE (enum machine_mode, long long, long long);

extern rtx gen_rtx_CONST_INT (enum machine_mode, long long);
extern rtx gen_raw_REG (enum machine_mode, int);
extern rtx gen_rtx_REG (enum machine_mode, int);
extern rtx gen_rtx_SUBREG (enum machine_mode, rtx, int);
extern rtx gen_rtx_MEM (enum machine_mode, rtx);

extern rtx gen_lowpart_SUBREG (enum machine_mode, rtx);
extern rtx find_next_ref (rtx, rtx);

extern rtx output_constant_def (tree, int);
extern rtx immed_real_const (tree);
extern int flow2_completed;




extern int reload_completed;




extern int reload_in_progress;







extern int cse_not_expected;



extern int no_new_pseudos;





extern int rtx_to_tree_code (enum rtx_code);


struct obstack;
extern void gcc_obstack_init (struct obstack *);


struct cse_basic_block_data;
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
enum expand_modifier {EXPAND_NORMAL, EXPAND_SUM, EXPAND_CONST_ADDRESS,
                        EXPAND_INITIALIZER, EXPAND_WRITE};
struct args_size
{
  long long constant;
  tree var;
};
enum direction {none, upward, downward};
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
extern void store_by_pieces (rtx, unsigned long long, rtx (*) (void *, long long, enum machine_mode), void *, unsigned int);





extern rtx emit_move_insn (rtx, rtx);


extern rtx emit_move_insn_1 (rtx, rtx);



extern rtx push_block (rtx, int, int);



extern void emit_push_insn (rtx, enum machine_mode, tree, rtx, unsigned int, int, rtx, int, rtx, rtx, int, rtx);




extern rtx expand_assignment (tree, tree, int, int);





extern rtx store_expr (tree, rtx, int);





extern rtx force_operand (rtx, rtx);
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
extern int exact_log2_wide (unsigned long long);
extern int floor_log2_wide (unsigned long long);
struct const_equiv_data {
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
extern varray_type varray_grow (varray_type, size_t);
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
extern void init_ggc (void);
extern void init_stringpool (void);



extern void ggc_push_context (void);



extern void ggc_pop_context (void);




extern void *ggc_alloc (size_t);

extern void *ggc_alloc_cleared (size_t);
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
typedef unsigned int hashval_t;




typedef hashval_t (*htab_hash) (const void *);






typedef int (*htab_eq) (const void *, const void *);



typedef void (*htab_del) (void *);





typedef int (*htab_trav) (void **, void *);






struct htab
{

  htab_hash hash_f;


  htab_eq eq_f;


  htab_del del_f;


  void * *entries;


  size_t size;


  size_t n_elements;


  size_t n_deleted;



  unsigned int searches;



  unsigned int collisions;



  int return_allocation_failure;
};

typedef struct htab *htab_t;


enum insert_option {NO_INSERT, INSERT};



extern htab_t htab_create (size_t, htab_hash, htab_eq, htab_del);





extern htab_t htab_try_create (size_t, htab_hash, htab_eq, htab_del);

extern void htab_delete (htab_t);
extern void htab_empty (htab_t);

extern void * htab_find (htab_t, const void *);
extern void * *htab_find_slot (htab_t, const void *, enum insert_option);

extern void * htab_find_with_hash (htab_t, const void *, hashval_t);

extern void * *htab_find_slot_with_hash (htab_t, const void *, hashval_t, enum insert_option);


extern void htab_clear_slot (htab_t, void **);
extern void htab_remove_elt (htab_t, void *);

extern void htab_traverse (htab_t, htab_trav, void *);

extern size_t htab_size (htab_t);
extern size_t htab_elements (htab_t);
extern double htab_collisions (htab_t);


extern htab_hash htab_hash_pointer;


extern htab_eq htab_eq_pointer;


extern hashval_t htab_hash_string (const void *);
static void encode (long long *, unsigned long long, long long);


static void decode (long long *, unsigned long long *, long long *);





static tree negate_expr (tree);
static tree split_tree (tree, enum tree_code, tree *, tree *, tree *, int);

/* MJP */
extern tree associate_trees (tree, tree, enum tree_code, tree);
extern void const_binop_1 (void *);
/* MJP */
static tree int_const_binop (enum tree_code, tree, tree, int);
static tree const_binop (enum tree_code, tree, tree, int);
static hashval_t size_htab_hash (const void *);
static int size_htab_eq (const void *, const void *);
static void fold_convert_1 (void *);
static tree fold_convert (tree, tree);
static enum tree_code invert_tree_comparison (enum tree_code);
static enum tree_code swap_tree_comparison (enum tree_code);
static int truth_value_p (enum tree_code);
static int operand_equal_for_comparison_p (tree, tree, tree);
static int twoval_comparison_p (tree, tree *, tree *, int *);
static tree eval_subst (tree, tree, tree, tree, tree);
static tree omit_one_operand (tree, tree, tree);
static tree pedantic_omit_one_operand (tree, tree, tree);
static tree distribute_bit_expr (enum tree_code, tree, tree, tree);
static tree make_bit_field_ref (tree, tree, int, int, int);
static tree optimize_bit_field_compare (enum tree_code, tree, tree, tree);

static tree decode_field_reference (tree, long long *, long long *, enum machine_mode *, int *, int *, tree *, tree *);



static int all_ones_mask_p (tree, int);
static int simple_operand_p (tree);
static tree range_binop (enum tree_code, tree, tree, int, tree, int);

static tree make_range (tree, int *, tree *, tree *);
static tree build_range_check (tree, tree, int, tree, tree);
static int merge_ranges (int *, tree *, tree *, int, tree, tree, int, tree, tree);

static tree fold_range_test (tree);
static tree unextend (tree, int, int, tree);
static tree fold_truthop (enum tree_code, tree, tree, tree);
tree optimize_minmax_comparison (tree);	/* MJP recursive */
tree extract_muldiv (tree, tree, enum tree_code, tree);	/* MJP recursive */
tree strip_compound_expr (tree, tree);	/* MJP */
static int multiple_of_p (tree, tree, tree);
tree constant_boolean_node (int, tree);	/* MJP */
int count_cond (tree, int);		/* MJP recursive */
tree fold_binary_op_with_conditional_arg 
  (enum tree_code, tree, tree, tree, int); /* MJP recursive */
static void
encode (words, low, hi)
     long long *words;
     unsigned long long low;
     long long hi;
{
  words[0] = ((low) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
  words[1] = ((unsigned long long) (low) >> (8 * 8) / 2);
  words[2] = ((hi) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
  words[3] = ((unsigned long long) (hi) >> (8 * 8) / 2);
}





static void
decode (words, low, hi)
     long long *words;
     unsigned long long *low;
     long long *hi;
{
  *low = words[0] + words[1] * ((unsigned long long) 1 << (8 * 8) / 2);
  *hi = words[2] + words[3] * ((unsigned long long) 1 << (8 * 8) / 2);
}

int
force_fit_type (t, overflow)
     tree t;
     int overflow;
{
  unsigned long long low;
  long long high;
  unsigned int prec;

  if (((enum tree_code) (t)->common.code) == REAL_CST)
    {




      return overflow;
    }

  else if (((enum tree_code) (t)->common.code) != INTEGER_CST)
    return overflow;

  low = (((t)->int_cst.int_cst).low);
  high = (((t)->int_cst.int_cst).high);

  if ((((enum tree_code) (((t)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((t)->common.type))->common.code) == REFERENCE_TYPE))
    prec = ((target_flags & 0x02000000) ? 64 : 32);
  else
    prec = ((((t)->common.type))->type.precision);



  if (prec == 2 * (8 * 8))
    ;
  else if (prec > (8 * 8))
    (((t)->int_cst.int_cst).high)
      &= ~((long long) (-1) << (prec - (8 * 8)));
  else
    {
      (((t)->int_cst.int_cst).high) = 0;
      if (prec < (8 * 8))
        (((t)->int_cst.int_cst).low) &= ~((unsigned long long) (-1) << prec);
    }



  if (((((t)->common.type))->common.unsigned_flag)
      && ! (((enum tree_code) (((t)->common.type))->common.code) == INTEGER_TYPE
            && ((((t)->common.type))->type.no_force_blk_flag)))
    return overflow;


  if (prec != 2 * (8 * 8)
      && (prec > (8 * 8)
          ? 0 != ((((t)->int_cst.int_cst).high)
                  & ((long long) 1
                     << (prec - (8 * 8) - 1)))
          : 0 != ((((t)->int_cst.int_cst).low)
                  & ((unsigned long long) 1 << (prec - 1)))))
    {


      if (prec > (8 * 8))
        (((t)->int_cst.int_cst).high)
          |= ((long long) (-1) << (prec - (8 * 8)));
      else
        {
          (((t)->int_cst.int_cst).high) = -1;
          if (prec < (8 * 8))
            (((t)->int_cst.int_cst).low) |= ((unsigned long long) (-1) << prec);
        }
    }


  return
    ((overflow | (low ^ (((t)->int_cst.int_cst).low)) | (high ^ (((t)->int_cst.int_cst).high)))
     != 0);
}






int
add_double (l1, h1, l2, h2, lv, hv)
     unsigned long long l1, l2;
     long long h1, h2;
     unsigned long long *lv;
     long long *hv;
{
  unsigned long long l;
  long long h;

  l = l1 + l2;
  h = h1 + h2 + (l < l1);

  *lv = l;
  *hv = h;
  return ((~((h1) ^ (h2)) & ((h1) ^ (h))) < 0);
}






int
neg_double (l1, h1, lv, hv)
     unsigned long long l1;
     long long h1;
     unsigned long long *lv;
     long long *hv;
{
  if (l1 == 0)
    {
      *lv = 0;
      *hv = - h1;
      return (*hv & h1) < 0;
    }
  else
    {
      *lv = -l1;
      *hv = ~h1;
      return 0;
    }
}







int
mul_double (l1, h1, l2, h2, lv, hv)
     unsigned long long l1, l2;
     long long h1, h2;
     unsigned long long *lv;
     long long *hv;
{
  long long arg1[4];
  long long arg2[4];
  long long prod[4 * 2];
  unsigned long long carry;
  int i, j, k;
  unsigned long long toplow, neglow;
  long long tophigh, neghigh;

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  memset ((char *) prod, 0, sizeof prod);

  for (i = 0; i < 4; i++)
    {
      carry = 0;
      for (j = 0; j < 4; j++)
        {
          k = i + j;

          carry += arg1[i] * arg2[j];

          carry += prod[k];
          prod[k] = ((carry) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
          carry = ((unsigned long long) (carry) >> (8 * 8) / 2);
        }
      prod[i + 4] = carry;
    }

  decode (prod, lv, hv);



  decode (prod + 4, &toplow, &tophigh);
  if (h1 < 0)
    {
      neg_double (l2, h2, &neglow, &neghigh);
      add_double (neglow, neghigh, toplow, tophigh, &toplow, &tophigh);
    }
  if (h2 < 0)
    {
      neg_double (l1, h1, &neglow, &neghigh);
      add_double (neglow, neghigh, toplow, tophigh, &toplow, &tophigh);
    }
  return (*hv < 0 ? ~(toplow & tophigh) : toplow | tophigh) != 0;
}







void
lshift_double (l1, h1, count, prec, lv, hv, arith)
     unsigned long long l1;
     long long h1, count;
     unsigned int prec;
     unsigned long long *lv;
     long long *hv;
     int arith;
{
  unsigned long long signmask;

  if (count < 0)
    {
      rshift_double (l1, h1, -count, prec, lv, hv, arith);
      return;
    }






  if (count >= 2 * (8 * 8))
    {


      *hv = 0;
      *lv = 0;
    }
  else if (count >= (8 * 8))
    {
      *hv = l1 << (count - (8 * 8));
      *lv = 0;
    }
  else
    {
      *hv = (((unsigned long long) h1 << count)
             | (l1 >> ((8 * 8) - count - 1) >> 1));
      *lv = l1 << count;
    }



  signmask = -((prec > (8 * 8)
                ? (*hv >> (prec - (8 * 8) - 1))
                : (*lv >> (prec - 1))) & 1);

  if (prec >= 2 * (8 * 8))
    ;
  else if (prec >= (8 * 8))
    {
      *hv &= ~((long long) (-1) << (prec - (8 * 8)));
      *hv |= signmask << (prec - (8 * 8));
    }
  else
    {
      *hv = signmask;
      *lv &= ~((unsigned long long) (-1) << prec);
      *lv |= signmask << prec;
    }
}






void
rshift_double (l1, h1, count, prec, lv, hv, arith)
     unsigned long long l1;
     long long h1, count;
     unsigned int prec;
     unsigned long long *lv;
     long long *hv;
     int arith;
{
  unsigned long long signmask;

  signmask = (arith
              ? -((unsigned long long) h1 >> ((8 * 8) - 1))
              : 0);






  if (count >= 2 * (8 * 8))
    {


      *hv = 0;
      *lv = 0;
    }
  else if (count >= (8 * 8))
    {
      *hv = 0;
      *lv = (unsigned long long) h1 >> (count - (8 * 8));
    }
  else
    {
      *hv = (unsigned long long) h1 >> count;
      *lv = ((l1 >> count)
             | ((unsigned long long) h1 << ((8 * 8) - count - 1) << 1));
    }



  if (count >= (long long)prec)
    {
      *hv = signmask;
      *lv = signmask;
    }
  else if ((prec - count) >= 2 * (8 * 8))
    ;
  else if ((prec - count) >= (8 * 8))
    {
      *hv &= ~((long long) (-1) << (prec - count - (8 * 8)));
      *hv |= signmask << (prec - count - (8 * 8));
    }
  else
    {
      *hv = signmask;
      *lv &= ~((unsigned long long) (-1) << (prec - count));
      *lv |= signmask << (prec - count);
    }
}






void
lrotate_double (l1, h1, count, prec, lv, hv)
     unsigned long long l1;
     long long h1, count;
     unsigned int prec;
     unsigned long long *lv;
     long long *hv;
{
  unsigned long long s1l, s2l;
  long long s1h, s2h;

  count %= prec;
  if (count < 0)
    count += prec;

  lshift_double (l1, h1, count, prec, &s1l, &s1h, 0);
  rshift_double (l1, h1, prec - count, prec, &s2l, &s2h, 0);
  *lv = s1l | s2l;
  *hv = s1h | s2h;
}





void
rrotate_double (l1, h1, count, prec, lv, hv)
     unsigned long long l1;
     long long h1, count;
     unsigned int prec;
     unsigned long long *lv;
     long long *hv;
{
  unsigned long long s1l, s2l;
  long long s1h, s2h;

  count %= prec;
  if (count < 0)
    count += prec;

  rshift_double (l1, h1, count, prec, &s1l, &s1h, 0);
  lshift_double (l1, h1, prec - count, prec, &s2l, &s2h, 0);
  *lv = s1l | s2l;
  *hv = s1h | s2h;
}

int
div_and_round_double (code, uns,
                      lnum_orig, hnum_orig, lden_orig, hden_orig,
                      lquo, hquo, lrem, hrem)
     enum tree_code code;
     int uns;
     unsigned long long lnum_orig;
     long long hnum_orig;
     unsigned long long lden_orig;
     long long hden_orig;
     unsigned long long *lquo, *lrem;
     long long *hquo, *hrem;
{
  int quo_neg = 0;
  long long num[4 + 1];
  long long den[4], quo[4];
  int i, j;
  unsigned long long work;
  unsigned long long carry = 0;
  unsigned long long lnum = lnum_orig;
  long long hnum = hnum_orig;
  unsigned long long lden = lden_orig;
  long long hden = hden_orig;
  int overflow = 0;

  if (hden == 0 && lden == 0)
    overflow = 1, lden = 1;


  if (!uns)
    {
      if (hnum < 0)
        {
          quo_neg = ~ quo_neg;

          if (neg_double (lnum, hnum, &lnum, &hnum)
              && ((long long) lden & hden) == -1)
            overflow = 1;
        }
      if (hden < 0)
        {
          quo_neg = ~ quo_neg;
          neg_double (lden, hden, &lden, &hden);
        }
    }

  if (hnum == 0 && hden == 0)
    {
      *hquo = *hrem = 0;

      *lquo = lnum / lden;
      goto finish_up;		/* GOTO */
    }

  if (hnum == 0)
    {

      *hquo = *lquo = 0;
      *hrem = hnum;
      *lrem = lnum;
      goto finish_up;		/* GOTO */
    }

  memset ((char *) quo, 0, sizeof quo);

  memset ((char *) num, 0, sizeof num);
  memset ((char *) den, 0, sizeof den);

  encode (num, lnum, hnum);
  encode (den, lden, hden);


  if (hden == 0 && lden < (unsigned long long) ((unsigned long long) 1 << (8 * 8) / 2))
    {

      for (i = 4 - 1; i >= 0; i--)
        {
          work = num[i] + carry * ((unsigned long long) 1 << (8 * 8) / 2);
          quo[i] = work / lden;
          carry = work % lden;
        }
    }
  else
    {


      int num_hi_sig, den_hi_sig;
      unsigned long long quo_est, scale;


      for (i = 4 - 1;; i--)
        if (den[i] != 0)
          {
            den_hi_sig = i;
            break;
          }




      scale = ((unsigned long long) 1 << (8 * 8) / 2) / (den[den_hi_sig] + 1);
      if (scale > 1)
        {
          carry = 0;
          for (i = 0; i <= 4 - 1; i++)
            {
              work = (num[i] * scale) + carry;
              num[i] = ((work) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
              carry = ((unsigned long long) (work) >> (8 * 8) / 2);
            }

          num[4] = carry;
          carry = 0;
          for (i = 0; i <= 4 - 1; i++)
            {
              work = (den[i] * scale) + carry;
              den[i] = ((work) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
              carry = ((unsigned long long) (work) >> (8 * 8) / 2);
              if (den[i] != 0) den_hi_sig = i;
            }
        }

      num_hi_sig = 4;


      for (i = num_hi_sig - den_hi_sig - 1; i >= 0; i--)
        {



          unsigned long long tmp;

          num_hi_sig = i + den_hi_sig + 1;
          work = num[num_hi_sig] * ((unsigned long long) 1 << (8 * 8) / 2) + num[num_hi_sig - 1];
          if (num[num_hi_sig] != den[den_hi_sig])
            quo_est = work / den[den_hi_sig];
          else
            quo_est = ((unsigned long long) 1 << (8 * 8) / 2) - 1;


          tmp = work - quo_est * den[den_hi_sig];
          if (tmp < ((unsigned long long) 1 << (8 * 8) / 2)
              && (den[den_hi_sig - 1] * quo_est
                  > (tmp * ((unsigned long long) 1 << (8 * 8) / 2) + num[num_hi_sig - 2])))
            quo_est--;





          carry = 0;
          for (j = 0; j <= den_hi_sig; j++)
            {
              work = quo_est * den[j] + carry;
              carry = ((unsigned long long) (work) >> (8 * 8) / 2);
              work = num[i + j] - ((work) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
              num[i + j] = ((work) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
              carry += ((unsigned long long) (work) >> (8 * 8) / 2) != 0;
            }



          if (num[num_hi_sig] < carry)
            {
              quo_est--;
              carry = 0;
              for (j = 0; j <= den_hi_sig; j++)
                {
                  work = num[i + j] + den[j] + carry;
                  carry = ((unsigned long long) (work) >> (8 * 8) / 2);
                  num[i + j] = ((work) & (((unsigned long long) 1 << ((8 * 8) / 2)) - 1));
                }

              num [num_hi_sig] += carry;
            }


          quo[i] = quo_est;
        }
    }

  decode (quo, lquo, hquo);

 finish_up:

  if (quo_neg)
    neg_double (*lquo, *hquo, lquo, hquo);


  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);

  switch (code)
    {
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case EXACT_DIV_EXPR:
      return overflow;

    case FLOOR_DIV_EXPR:
    case FLOOR_MOD_EXPR:
      if (quo_neg && (*lrem != 0 || *hrem != 0))
        {

          add_double (*lquo, *hquo, (long long) -1, (long long) -1,
                      lquo, hquo);
        }
      else
        return overflow;
      break;

    case CEIL_DIV_EXPR:
    case CEIL_MOD_EXPR:
      if (!quo_neg && (*lrem != 0 || *hrem != 0))
        {
          add_double (*lquo, *hquo, (long long) 1, (long long) 0,
                      lquo, hquo);
        }
      else
        return overflow;
      break;

    case ROUND_DIV_EXPR:
    case ROUND_MOD_EXPR:
      {
        unsigned long long labs_rem = *lrem;
        long long habs_rem = *hrem;
        unsigned long long labs_den = lden, ltwice;
        long long habs_den = hden, htwice;


        if (*hrem < 0)
          neg_double (*lrem, *hrem, &labs_rem, &habs_rem);
        if (hden < 0)
          neg_double (lden, hden, &labs_den, &habs_den);


        mul_double ((long long) 2, (long long) 0,
                    labs_rem, habs_rem, &ltwice, &htwice);

        if (((unsigned long long) habs_den
             < (unsigned long long) htwice)
            || (((unsigned long long) habs_den
                 == (unsigned long long) htwice)
                && (labs_den < ltwice)))
          {
            if (*hquo < 0)

              add_double (*lquo, *hquo,
                          (long long) -1, (long long) -1, lquo, hquo);
            else

              add_double (*lquo, *hquo, (long long) 1, (long long) 0,
                          lquo, hquo);
          }
        else
          return overflow;
      }
      break;

    default:
      fancy_abort ("fold-const.c", 823, __FUNCTION__);
    }


  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);
  return overflow;
}

static tree
negate_expr (t)
     tree t;
{
  tree type;
  tree tem;

  if (t == 0)
    return 0;

  type = ((t)->common.type);
  while ((((enum tree_code) (t)->common.code) == NOP_EXPR || ((enum tree_code) (t)->common.code) == CONVERT_EXPR || ((enum tree_code) (t)->common.code) == NON_LVALUE_EXPR) && ((t)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((t)->common.type))->type.mode) == ((((((t)->exp.operands[0]))->common.type))->type.mode)) && (((((t)->common.type))->common.unsigned_flag) == ((((((t)->exp.operands[0]))->common.type))->common.unsigned_flag))) (t) = ((t)->exp.operands[0]);

  switch (((enum tree_code) (t)->common.code))
    {
    case INTEGER_CST:
    case REAL_CST:
      if (! ((type)->common.unsigned_flag)
          && 0 != (tem = fold (build1 (NEGATE_EXPR, type, t)))
          && ! ((tem)->common.public_flag))
        return tem;
      break;

    case NEGATE_EXPR:
      return convert (type, ((t)->exp.operands[0]));

    case MINUS_EXPR:

      if (! (((enum tree_code) (type)->common.code) == REAL_TYPE || (((enum tree_code) (type)->common.code) == COMPLEX_TYPE && ((enum tree_code) (((type)->common.type))->common.code) == REAL_TYPE)) || flag_unsafe_math_optimizations)
        return convert (type,
                        fold (build (MINUS_EXPR, ((t)->common.type),
                                     ((t)->exp.operands[1]),
                                     ((t)->exp.operands[0]))));
      break;

    default:
      break;
    }

  return convert (type, fold (build1 (NEGATE_EXPR, ((t)->common.type), t)));
}

static tree
split_tree (in, code, conp, litp, minus_litp, negate_p)
     tree in;
     enum tree_code code;
     tree *conp, *litp, *minus_litp;
     int negate_p;
{
  tree var = 0;

  *conp = 0;
  *litp = 0;
  *minus_litp = 0;


  while ((((enum tree_code) (in)->common.code) == NOP_EXPR || ((enum tree_code) (in)->common.code) == CONVERT_EXPR || ((enum tree_code) (in)->common.code) == NON_LVALUE_EXPR) && ((in)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((in)->common.type))->type.mode) == ((((((in)->exp.operands[0]))->common.type))->type.mode)) && (((((in)->common.type))->common.unsigned_flag) == ((((((in)->exp.operands[0]))->common.type))->common.unsigned_flag))) (in) = ((in)->exp.operands[0]);

  if (((enum tree_code) (in)->common.code) == INTEGER_CST || ((enum tree_code) (in)->common.code) == REAL_CST)
    *litp = in;
  else if (((enum tree_code) (in)->common.code) == code
           || (! (((enum tree_code) (((in)->common.type))->common.code) == REAL_TYPE || (((enum tree_code) (((in)->common.type))->common.code) == COMPLEX_TYPE && ((enum tree_code) (((((in)->common.type))->common.type))->common.code) == REAL_TYPE))




               && ((code == PLUS_EXPR && ((enum tree_code) (in)->common.code) == MINUS_EXPR)
                   || (code == MINUS_EXPR && ((enum tree_code) (in)->common.code) == PLUS_EXPR))))
    {
      tree op0 = ((in)->exp.operands[0]);
      tree op1 = ((in)->exp.operands[1]);
      int neg1_p = ((enum tree_code) (in)->common.code) == MINUS_EXPR;
      int neg_litp_p = 0, neg_conp_p = 0, neg_var_p = 0;


      if (((enum tree_code) (op0)->common.code) == INTEGER_CST || ((enum tree_code) (op0)->common.code) == REAL_CST)
        *litp = op0, op0 = 0;
      else if (((enum tree_code) (op1)->common.code) == INTEGER_CST || ((enum tree_code) (op1)->common.code) == REAL_CST)
        *litp = op1, neg_litp_p = neg1_p, op1 = 0;

      if (op0 != 0 && ((op0)->common.constant_flag))
        *conp = op0, op0 = 0;
      else if (op1 != 0 && ((op1)->common.constant_flag))
        *conp = op1, neg_conp_p = neg1_p, op1 = 0;



      if (op0 != 0 && op1 != 0)
        var = in;
      else if (op0 != 0)
        var = op0;
      else
        var = op1, neg_var_p = neg1_p;


      if (neg_litp_p)
        *minus_litp = *litp, *litp = 0;
      if (neg_conp_p)
        *conp = negate_expr (*conp);
      if (neg_var_p)
        var = negate_expr (var);
    }
  else if (((in)->common.constant_flag))
    *conp = in;
  else
    var = in;

  if (negate_p)
    {
      if (*litp)
        *minus_litp = *litp, *litp = 0;
      else if (*minus_litp)
        *litp = *minus_litp, *minus_litp = 0;
      *conp = negate_expr (*conp);
      var = negate_expr (var);
    }

  return var;
}







static tree
int_const_binop (code, arg1, arg2, notrunc)
     enum tree_code code;
     tree arg1, arg2;
     int notrunc;
{
  unsigned long long int1l, int2l;
  long long int1h, int2h;
  unsigned long long low;
  long long hi;
  unsigned long long garbagel;
  long long garbageh;
  tree t;
  tree type = ((arg1)->common.type);
  int uns = ((type)->common.unsigned_flag);
  int is_sizetype
    = (((enum tree_code) (type)->common.code) == INTEGER_TYPE && ((type)->type.no_force_blk_flag));
  int overflow = 0;
  int no_overflow = 0;

  int1l = (((arg1)->int_cst.int_cst).low);
  int1h = (((arg1)->int_cst.int_cst).high);
  int2l = (((arg2)->int_cst.int_cst).low);
  int2h = (((arg2)->int_cst.int_cst).high);

  switch (code)
    {
    case BIT_IOR_EXPR:
      low = int1l | int2l, hi = int1h | int2h;
      break;

    case BIT_XOR_EXPR:
      low = int1l ^ int2l, hi = int1h ^ int2h;
      break;

    case BIT_AND_EXPR:
      low = int1l & int2l, hi = int1h & int2h;
      break;

    case BIT_ANDTC_EXPR:
      low = int1l & ~int2l, hi = int1h & ~int2h;
      break;

    case RSHIFT_EXPR:
      int2l = -int2l;
    case LSHIFT_EXPR:



      lshift_double (int1l, int1h, int2l, ((type)->type.precision),
                     &low, &hi, !uns);
      no_overflow = 1;
      break;

    case RROTATE_EXPR:
      int2l = - int2l;
    case LROTATE_EXPR:
      lrotate_double (int1l, int1h, int2l, ((type)->type.precision),
                      &low, &hi);
      break;

    case PLUS_EXPR:
      overflow = add_double (int1l, int1h, int2l, int2h, &low, &hi);
      break;

    case MINUS_EXPR:
      neg_double (int2l, int2h, &low, &hi);
      add_double (int1l, int1h, low, hi, &low, &hi);
      overflow = ((~((hi) ^ (int2h)) & ((hi) ^ (int1h))) < 0);
      break;

    case MULT_EXPR:
      overflow = mul_double (int1l, int1h, int2l, int2h, &low, &hi);
      break;

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR: case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:

      if (int2h == 0 && (long long) int2l > 0
          && ! ((arg1)->common.static_flag)
          && ! ((arg2)->common.static_flag)
          && int1h == 0 && (long long) int1l >= 0)
        {
          if (code == CEIL_DIV_EXPR)
            int1l += int2l - 1;

          low = int1l / int2l, hi = 0;
          break;
        }



    case ROUND_DIV_EXPR:
      if (int2h == 0 && int2l == 1)
        {
          low = int1l, hi = int1h;
          break;
        }
      if (int1l == int2l && int1h == int2h
          && ! (int1l == 0 && int1h == 0))
        {
          low = 1, hi = 0;
          break;
        }
      overflow = div_and_round_double (code, uns, int1l, int1h, int2l, int2h,
                                       &low, &hi, &garbagel, &garbageh);
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR: case CEIL_MOD_EXPR:

      if (int2h == 0 && (long long) int2l > 0
          && ! ((arg1)->common.static_flag)
          && ! ((arg2)->common.static_flag)
          && int1h == 0 && (long long) int1l >= 0)
        {
          if (code == CEIL_MOD_EXPR)
            int1l += int2l - 1;
          low = int1l % int2l, hi = 0;
          break;
        }



    case ROUND_MOD_EXPR:
      overflow = div_and_round_double (code, uns,
                                       int1l, int1h, int2l, int2h,
                                       &garbagel, &garbageh, &low, &hi);
      break;

    case MIN_EXPR:
    case MAX_EXPR:
      if (uns)
        low = (((unsigned long long) int1h
                < (unsigned long long) int2h)
               || (((unsigned long long) int1h
                    == (unsigned long long) int2h)
                   && int1l < int2l));
      else
        low = (int1h < int2h
               || (int1h == int2h && int1l < int2l));

      if (low == (code == MIN_EXPR))
        low = int1l, hi = int1h;
      else
        low = int2l, hi = int2h;
      break;

    default:
      fancy_abort ("fold-const.c", 1671, __FUNCTION__);
    }




  if (is_sizetype
      && ((hi == 0 && (long long) low >= 0)
          || (hi == -1 && (long long) low < 0))
      && overflow == 0 && ! ((arg1)->common.public_flag) && ! ((arg2)->common.public_flag))
    return size_int_type_wide (low, type);
  else
    {
      t = build_int_2_wide ((unsigned long long) (low), (long long) (hi));
      ((t)->common.type) = ((arg1)->common.type);
    }

  ((t)->common.public_flag)
    = ((notrunc
        ? (!uns || is_sizetype) && overflow
        : (force_fit_type (t, (!uns || is_sizetype) && overflow)
           && ! no_overflow))
       | ((arg1)->common.public_flag)
       | ((arg2)->common.public_flag));



  if (is_sizetype
      && ! ((t)->common.public_flag)
      && ((((t)->int_cst.int_cst).high) != hi
          || (((t)->int_cst.int_cst).low) != low))
    ((t)->common.public_flag) = 1;

  ((t)->common.static_flag) = (((t)->common.public_flag)
                                | ((arg1)->common.static_flag)
                                | ((arg2)->common.static_flag));
  return t;
}


struct cb_args
{
  enum tree_code code;
  tree type;
  realvaluetype d1, d2;
  tree t;
};




static tree
const_binop (code, arg1, arg2, notrunc)
     enum tree_code code;
     tree arg1, arg2;
     int notrunc;
{
  while ((((enum tree_code) (arg1)->common.code) == NOP_EXPR || ((enum tree_code) (arg1)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg1)->common.code) == NON_LVALUE_EXPR) && ((arg1)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg1)->common.type))->type.mode) == ((((((arg1)->exp.operands[0]))->common.type))->type.mode))) (arg1) = ((arg1)->exp.operands[0]);
  while ((((enum tree_code) (arg2)->common.code) == NOP_EXPR || ((enum tree_code) (arg2)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg2)->common.code) == NON_LVALUE_EXPR) && ((arg2)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg2)->common.type))->type.mode) == ((((((arg2)->exp.operands[0]))->common.type))->type.mode))) (arg2) = ((arg2)->exp.operands[0]);

  if (((enum tree_code) (arg1)->common.code) == INTEGER_CST)
    return int_const_binop (code, arg1, arg2, notrunc);


  if (((enum tree_code) (arg1)->common.code) == REAL_CST)
    {
      realvaluetype d1;
      realvaluetype d2;
      int overflow = 0;
      tree t;
      struct cb_args args;

      d1 = ((arg1)->real_cst.real_cst);
      d2 = ((arg2)->real_cst.real_cst);



      if ((target_isnan (d1)))
        return arg1;
      else if ((target_isnan (d2)))
        return arg2;


      args.type = ((arg1)->common.type);
      args.d1 = d1;
      args.d2 = d2;
      args.code = code;

      if (do_float_handler (const_binop_1, (void *) &args))

        t = args.t;
      else
        {

          t = copy_node (arg1);
          overflow = 1;
        }

      ((t)->common.public_flag)
        = (force_fit_type (t, overflow)
           | ((arg1)->common.public_flag) | ((arg2)->common.public_flag));
      ((t)->common.static_flag)
        = ((t)->common.public_flag)
          | ((arg1)->common.static_flag)
          | ((arg2)->common.static_flag);
      return t;
    }

  if (((enum tree_code) (arg1)->common.code) == COMPLEX_CST)
    {
      tree type = ((arg1)->common.type);
      tree r1 = ((arg1)->complex.real);
      tree i1 = ((arg1)->complex.imag);
      tree r2 = ((arg2)->complex.real);
      tree i2 = ((arg2)->complex.imag);
      tree t;

      switch (code)
        {
        case PLUS_EXPR:
          t = build_complex (type,
                             const_binop (PLUS_EXPR, r1, r2, notrunc),
                             const_binop (PLUS_EXPR, i1, i2, notrunc));
          break;

        case MINUS_EXPR:
          t = build_complex (type,
                             const_binop (MINUS_EXPR, r1, r2, notrunc),
                             const_binop (MINUS_EXPR, i1, i2, notrunc));
          break;

        case MULT_EXPR:
          t = build_complex (type,
                             const_binop (MINUS_EXPR,
                                          const_binop (MULT_EXPR,
                                                       r1, r2, notrunc),
                                          const_binop (MULT_EXPR,
                                                       i1, i2, notrunc),
                                          notrunc),
                             const_binop (PLUS_EXPR,
                                          const_binop (MULT_EXPR,
                                                       r1, i2, notrunc),
                                          const_binop (MULT_EXPR,
                                                       i1, r2, notrunc),
                                          notrunc));
          break;

        case RDIV_EXPR:
          {
            tree magsquared
              = const_binop (PLUS_EXPR,
                             const_binop (MULT_EXPR, r2, r2, notrunc),
                             const_binop (MULT_EXPR, i2, i2, notrunc),
                             notrunc);

            t = build_complex (type,
                               const_binop
                               ((((enum tree_code) (((r1)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((r1)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((r1)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((r1)->common.type))->common.code) == CHAR_TYPE)
                                ? TRUNC_DIV_EXPR : RDIV_EXPR,
                                const_binop (PLUS_EXPR,
                                             const_binop (MULT_EXPR, r1, r2,
                                                          notrunc),
                                             const_binop (MULT_EXPR, i1, i2,
                                                          notrunc),
                                             notrunc),
                                magsquared, notrunc),
                               const_binop
                               ((((enum tree_code) (((r1)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((r1)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((r1)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((r1)->common.type))->common.code) == CHAR_TYPE)
                                ? TRUNC_DIV_EXPR : RDIV_EXPR,
                                const_binop (MINUS_EXPR,
                                             const_binop (MULT_EXPR, i1, r2,
                                                          notrunc),
                                             const_binop (MULT_EXPR, r1, i2,
                                                          notrunc),
                                             notrunc),
                                magsquared, notrunc));
          }
          break;

        default:
          fancy_abort ("fold-const.c", 1908, __FUNCTION__);
        }
      return t;
    }
  return 0;
}






static hashval_t
size_htab_hash (x)
     const void *x;
{
  tree t = (tree) x;

  return ((((t)->int_cst.int_cst).high) ^ (((t)->int_cst.int_cst).low)
          ^ (hashval_t) ((long) ((t)->common.type) >> 3)
          ^ (((t)->common.public_flag) << 20));
}




static int
size_htab_eq (x, y)
     const void *x;
     const void *y;
{
  tree xt = (tree) x;
  tree yt = (tree) y;

  return ((((xt)->int_cst.int_cst).high) == (((yt)->int_cst.int_cst).high)
          && (((xt)->int_cst.int_cst).low) == (((yt)->int_cst.int_cst).low)
          && ((xt)->common.type) == ((yt)->common.type)
          && ((xt)->common.public_flag) == ((yt)->common.public_flag));
}




tree
size_int_wide (number, kind)
     long long number;
     enum size_type_kind kind;
{
  return size_int_type_wide (number, sizetype_tab[(int) kind]);
}



tree
size_int_type_wide (number, type)
     long long number;
     tree type;
{
  static htab_t size_htab = 0;
  static tree new_const = 0;
  void * *slot;

  if (size_htab == 0)
    {
      size_htab = htab_create (1024, size_htab_hash, size_htab_eq, ((void *)0));
      ggc_add_deletable_htab (size_htab, ((void *)0), ((void *)0));
      new_const = make_node (INTEGER_CST);
      ggc_add_tree_root (&new_const, 1);
    }




  (((new_const)->int_cst.int_cst).low) = number;
  (((new_const)->int_cst.int_cst).high) = number < 0 ? -1 : 0;
  ((new_const)->common.type) = type;
  ((new_const)->common.public_flag) = ((new_const)->common.static_flag)
    = force_fit_type (new_const, 0);

  slot = htab_find_slot (size_htab, new_const, INSERT);
  if (*slot == 0)
    {
      tree t = new_const;

      *slot = (void *) new_const;
      new_const = make_node (INTEGER_CST);
      return t;
    }
  else
    return (tree) *slot;
}






tree
size_binop (code, arg0, arg1)
     enum tree_code code;
     tree arg0, arg1;
{
  tree type = ((arg0)->common.type);

  if (((enum tree_code) (type)->common.code) != INTEGER_TYPE || ! ((type)->type.no_force_blk_flag)
      || type != ((arg1)->common.type))
    fancy_abort ("fold-const.c", 2014, __FUNCTION__);


  if (((enum tree_code) (arg0)->common.code) == INTEGER_CST && ((enum tree_code) (arg1)->common.code) == INTEGER_CST)
    {

      if (code == PLUS_EXPR && integer_zerop (arg0))
        return arg1;
      else if ((code == MINUS_EXPR || code == PLUS_EXPR)
               && integer_zerop (arg1))
        return arg0;
      else if (code == MULT_EXPR && integer_onep (arg0))
        return arg1;


      return int_const_binop (code, arg0, arg1, 0);
    }

  if (arg0 == global_trees[TI_ERROR_MARK] || arg1 == global_trees[TI_ERROR_MARK])
    return global_trees[TI_ERROR_MARK];

  return fold (build (code, type, arg0, arg1));
}





tree
size_diffop (arg0, arg1)
     tree arg0, arg1;
{
  tree type = ((arg0)->common.type);
  tree ctype;

  if (((enum tree_code) (type)->common.code) != INTEGER_TYPE || ! ((type)->type.no_force_blk_flag)
      || type != ((arg1)->common.type))
    fancy_abort ("fold-const.c", 2051, __FUNCTION__);


  if (! ((type)->common.unsigned_flag))
    return size_binop (MINUS_EXPR, arg0, arg1);

  ctype = (type == sizetype_tab[(int) BITSIZETYPE] || type == sizetype_tab[(int) UBITSIZETYPE]
           ? sizetype_tab[(int) SBITSIZETYPE] : sizetype_tab[(int) SSIZETYPE]);




  if (((enum tree_code) (arg0)->common.code) != INTEGER_CST || ((enum tree_code) (arg1)->common.code) != INTEGER_CST)
    return size_binop (MINUS_EXPR, convert (ctype, arg0),
                       convert (ctype, arg1));





  if (tree_int_cst_equal (arg0, arg1))
    return convert (ctype, global_trees[TI_INTEGER_ZERO]);
  else if (tree_int_cst_lt (arg1, arg0))
    return convert (ctype, size_binop (MINUS_EXPR, arg0, arg1));
  else
    return size_binop (MINUS_EXPR, convert (ctype, global_trees[TI_INTEGER_ZERO]),
                       convert (ctype, size_binop (MINUS_EXPR, arg1, arg0)));
}


struct fc_args
{
  tree arg1;
  tree type;
  tree t;
};




static void
fold_convert_1 (data)
     void * data;
{
  struct fc_args *args = (struct fc_args *) data;

  args->t = build_real (args->type,
                        real_value_truncate (((args->type)->type.mode),
                                             ((args->arg1)->real_cst.real_cst)));
}




static tree
fold_convert (t, arg1)
     tree t;
     tree arg1;
{
  tree type = ((t)->common.type);
  int overflow = 0;

  if ((((enum tree_code) (type)->common.code) == POINTER_TYPE || ((enum tree_code) (type)->common.code) == REFERENCE_TYPE) || (((enum tree_code) (type)->common.code) == INTEGER_TYPE || ((enum tree_code) (type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (type)->common.code) == CHAR_TYPE))
    {
      if (((enum tree_code) (arg1)->common.code) == INTEGER_CST)
        {


          if (((type)->type.precision) > 2 * (8 * 8))
            return t;



          if (((enum tree_code) (type)->common.code) == INTEGER_TYPE && ((type)->type.no_force_blk_flag)
              && !((arg1)->common.static_flag)
              && compare_tree_int (arg1, 10000) < 0)
            return size_int_type_wide ((((arg1)->int_cst.int_cst).low), type);



          t = build_int_2_wide ((unsigned long long) ((((arg1)->int_cst.int_cst).low)), (long long) ((((arg1)->int_cst.int_cst).high)));

          ((t)->common.type) = type;





          ((t)->common.public_flag)
            = ((force_fit_type (t,
                                ((((arg1)->int_cst.int_cst).high) < 0
                                 && (((type)->common.unsigned_flag)
                                    < ((((arg1)->common.type))->common.unsigned_flag))))
                && ! (((enum tree_code) (((arg1)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == REFERENCE_TYPE))
               || ((arg1)->common.public_flag));
          ((t)->common.static_flag)
            = ((t)->common.public_flag) | ((arg1)->common.static_flag);
        }

      else if (((enum tree_code) (arg1)->common.code) == REAL_CST)
        {


          realvaluetype x;
          realvaluetype l;
          realvaluetype u;
          tree type1 = ((arg1)->common.type);
          int no_upper_bound;

          x = ((arg1)->real_cst.real_cst);
          l = real_value_from_int_cst (type1, ((type)->type.minval));

          no_upper_bound = (((type)->type.maxval) == ((void *)0));
          if (!no_upper_bound)
            u = real_value_from_int_cst (type1, ((type)->type.maxval));





          earith (&(l), (MINUS_EXPR), &(l), &(dconst1));
          if (!no_upper_bound)
            earith (&(u), (PLUS_EXPR), &(u), &(dconst1));







          if ((target_isnan (x)))
            overflow = 1, x = dconst0;
          else if (! ((ereal_cmp ((l), (x)) == -1)
                      && !no_upper_bound
                      && (ereal_cmp ((x), (u)) == -1)))
            overflow = 1;

          {
            long long low, high;
            ereal_to_int (&low, &high, x);
            t = build_int_2_wide ((unsigned long long) (low), (long long) (high));
          }

          ((t)->common.type) = type;
          ((t)->common.public_flag)
            = ((arg1)->common.public_flag) | force_fit_type (t, overflow);
          ((t)->common.static_flag)
            = ((t)->common.public_flag) | ((arg1)->common.static_flag);
        }

      ((t)->common.type) = type;
    }
  else if (((enum tree_code) (type)->common.code) == REAL_TYPE)
    {

      if (((enum tree_code) (arg1)->common.code) == INTEGER_CST)
        return build_real_from_int_cst (type, arg1);

      if (((enum tree_code) (arg1)->common.code) == REAL_CST)
        {
          struct fc_args args;

          if ((target_isnan (((arg1)->real_cst.real_cst))))
            {
              t = arg1;
              ((arg1)->common.type) = type;
              return t;
            }


          args.arg1 = arg1;
          args.type = type;

          if (do_float_handler (fold_convert_1, (void *) &args))
            {

              t = args.t;
            }
          else
            {

              overflow = 1;
              t = copy_node (arg1);
            }

          ((t)->common.public_flag)
            = ((arg1)->common.public_flag) | force_fit_type (t, overflow);
          ((t)->common.static_flag)
            = ((t)->common.public_flag) | ((arg1)->common.static_flag);
          return t;
        }
    }
  ((t)->common.constant_flag) = 1;
  return t;
}



tree
non_lvalue (x)
     tree x;
{
  tree result;


  if (((enum tree_code) (x)->common.code) == NON_LVALUE_EXPR
      || ((enum tree_code) (x)->common.code) == INTEGER_CST
      || ((enum tree_code) (x)->common.code) == REAL_CST
      || ((enum tree_code) (x)->common.code) == STRING_CST
      || ((enum tree_code) (x)->common.code) == ADDR_EXPR)
    return x;

  result = build1 (NON_LVALUE_EXPR, ((x)->common.type), x);
  ((result)->common.constant_flag) = ((x)->common.constant_flag);
  return result;
}




int pedantic_lvalues;




tree
pedantic_non_lvalue (x)
     tree x;
{
  if (pedantic_lvalues)
    return non_lvalue (x);
  else
    return x;
}





static enum tree_code
invert_tree_comparison (code)
     enum tree_code code;
{
  switch (code)
    {
    case EQ_EXPR:
      return NE_EXPR;
    case NE_EXPR:
      return EQ_EXPR;
    case GT_EXPR:
      return LE_EXPR;
    case GE_EXPR:
      return LT_EXPR;
    case LT_EXPR:
      return GE_EXPR;
    case LE_EXPR:
      return GT_EXPR;
    default:
      fancy_abort ("fold-const.c", 2332, __FUNCTION__);
    }
}




static enum tree_code
swap_tree_comparison (code)
     enum tree_code code;
{
  switch (code)
    {
    case EQ_EXPR:
    case NE_EXPR:
      return code;
    case GT_EXPR:
      return LT_EXPR;
    case GE_EXPR:
      return LE_EXPR;
    case LT_EXPR:
      return GT_EXPR;
    case LE_EXPR:
      return GE_EXPR;
    default:
      fancy_abort ("fold-const.c", 2357, __FUNCTION__);
    }
}



static int
truth_value_p (code)
     enum tree_code code;
{
  return (tree_code_type[(int) (code)] == '<'
          || code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR
          || code == TRUTH_OR_EXPR || code == TRUTH_ORIF_EXPR
          || code == TRUTH_XOR_EXPR || code == TRUTH_NOT_EXPR);
}

int
operand_equal_p (arg0, arg1, only_const)
     tree arg0, arg1;
     int only_const;
{



  if (((((arg0)->common.type))->common.unsigned_flag) != ((((arg1)->common.type))->common.unsigned_flag))
    return 0;

  while ((((enum tree_code) (arg0)->common.code) == NOP_EXPR || ((enum tree_code) (arg0)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg0)->common.code) == NON_LVALUE_EXPR) && ((arg0)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg0)->common.type))->type.mode) == ((((((arg0)->exp.operands[0]))->common.type))->type.mode))) (arg0) = ((arg0)->exp.operands[0]);
  while ((((enum tree_code) (arg1)->common.code) == NOP_EXPR || ((enum tree_code) (arg1)->common.code) == CONVERT_EXPR || ((enum tree_code) (arg1)->common.code) == NON_LVALUE_EXPR) && ((arg1)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((arg1)->common.type))->type.mode) == ((((((arg1)->exp.operands[0]))->common.type))->type.mode))) (arg1) = ((arg1)->exp.operands[0]);

  if (((enum tree_code) (arg0)->common.code) != ((enum tree_code) (arg1)->common.code)


      || ((enum tree_code) (((arg0)->common.type))->common.code) == ERROR_MARK
      || ((enum tree_code) (((arg1)->common.type))->common.code) == ERROR_MARK
      || ((((arg0)->common.type))->type.mode) != ((((arg1)->common.type))->type.mode))
    return 0;

  if (arg0 == arg1 && ! only_const
      && (((enum tree_code) (arg0)->common.code) == SAVE_EXPR
          || (! ((arg0)->common.side_effects_flag) && ! ((arg1)->common.side_effects_flag))))
    return 1;



  if (((arg0)->common.constant_flag) && ((arg1)->common.constant_flag))
    switch (((enum tree_code) (arg0)->common.code))
      {
      case INTEGER_CST:
        return (! ((arg0)->common.static_flag)
                && ! ((arg1)->common.static_flag)
                && tree_int_cst_equal (arg0, arg1));

      case REAL_CST:
        return (! ((arg0)->common.static_flag)
                && ! ((arg1)->common.static_flag)
                && (!memcmp ((char *) &(((arg0)->real_cst.real_cst)), (char *) &(((arg1)->real_cst.real_cst)), sizeof (realvaluetype))));


      case VECTOR_CST:
        {
          tree v1, v2;

          if (((arg0)->common.static_flag)
              || ((arg1)->common.static_flag))
            return 0;

          v1 = ((arg0)->vector.elements);
          v2 = ((arg1)->vector.elements);
          while (v1 && v2)
            {
              if (!operand_equal_p (v1, v2, only_const))
                return 0;
              v1 = ((v1)->common.chain);
              v2 = ((v2)->common.chain);
            }

          return 1;
        }

      case COMPLEX_CST:
        return (operand_equal_p (((arg0)->complex.real), ((arg1)->complex.real),
                                 only_const)
                && operand_equal_p (((arg0)->complex.imag), ((arg1)->complex.imag),
                                    only_const));

      case STRING_CST:
        return (((arg0)->string.length) == ((arg1)->string.length)
                && ! memcmp (((arg0)->string.pointer),
                              ((arg1)->string.pointer),
                              ((arg0)->string.length)));

      case ADDR_EXPR:
        return operand_equal_p (((arg0)->exp.operands[0]), ((arg1)->exp.operands[0]),
                                0);
      default:
        break;
      }

  if (only_const)
    return 0;

  switch (tree_code_type[(int) (((enum tree_code) (arg0)->common.code))])
    {
    case '1':

      if ((((enum tree_code) (arg0)->common.code) == NOP_EXPR || ((enum tree_code) (arg0)->common.code) == CONVERT_EXPR)
          && (((((arg0)->common.type))->common.unsigned_flag)
              != ((((arg1)->common.type))->common.unsigned_flag)))
        return 0;

      return operand_equal_p (((arg0)->exp.operands[0]),
                              ((arg1)->exp.operands[0]), 0);

    case '<':
    case '2':
      if (operand_equal_p (((arg0)->exp.operands[0]), ((arg1)->exp.operands[0]), 0)
          && operand_equal_p (((arg0)->exp.operands[1]), ((arg1)->exp.operands[1]),
                              0))
        return 1;


      return ((((enum tree_code) (arg0)->common.code) == PLUS_EXPR || ((enum tree_code) (arg0)->common.code) == MULT_EXPR
               || ((enum tree_code) (arg0)->common.code) == MIN_EXPR || ((enum tree_code) (arg0)->common.code) == MAX_EXPR
               || ((enum tree_code) (arg0)->common.code) == BIT_IOR_EXPR
               || ((enum tree_code) (arg0)->common.code) == BIT_XOR_EXPR
               || ((enum tree_code) (arg0)->common.code) == BIT_AND_EXPR
               || ((enum tree_code) (arg0)->common.code) == NE_EXPR || ((enum tree_code) (arg0)->common.code) == EQ_EXPR)
              && operand_equal_p (((arg0)->exp.operands[0]),
                                  ((arg1)->exp.operands[1]), 0)
              && operand_equal_p (((arg0)->exp.operands[1]),
                                  ((arg1)->exp.operands[0]), 0));

    case 'r':


      if (((arg0)->common.side_effects_flag)
          || ((arg1)->common.side_effects_flag))
        return 0;

      switch (((enum tree_code) (arg0)->common.code))
        {
        case INDIRECT_REF:
          return operand_equal_p (((arg0)->exp.operands[0]),
                                  ((arg1)->exp.operands[0]), 0);

        case COMPONENT_REF:
        case ARRAY_REF:
        case ARRAY_RANGE_REF:
          return (operand_equal_p (((arg0)->exp.operands[0]),
                                   ((arg1)->exp.operands[0]), 0)
                  && operand_equal_p (((arg0)->exp.operands[1]),
                                      ((arg1)->exp.operands[1]), 0));

        case BIT_FIELD_REF:
          return (operand_equal_p (((arg0)->exp.operands[0]),
                                   ((arg1)->exp.operands[0]), 0)
                  && operand_equal_p (((arg0)->exp.operands[1]),
                                      ((arg1)->exp.operands[1]), 0)
                  && operand_equal_p (((arg0)->exp.operands[2]),
                                      ((arg1)->exp.operands[2]), 0));
        default:
          return 0;
        }

    case 'e':
      if (((enum tree_code) (arg0)->common.code) == RTL_EXPR)
        return rtx_equal_p ((*(rtx *) &(arg0)->exp.operands[1]), (*(rtx *) &(arg1)->exp.operands[1]));
      return 0;

    default:
      return 0;
    }
}






static int
operand_equal_for_comparison_p (arg0, arg1, other)
     tree arg0, arg1;
     tree other;
{
  int unsignedp1, unsignedpo;
  tree primarg0, primarg1, primother;
  unsigned int correct_width;

  if (operand_equal_p (arg0, arg1, 0))
    return 1;

  if (! (((enum tree_code) (((arg0)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((arg0)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((arg0)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((arg0)->common.type))->common.code) == CHAR_TYPE)
      || ! (((enum tree_code) (((arg1)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == CHAR_TYPE))
    return 0;




  primarg0 = arg0, primarg1 = arg1;
  while ((((enum tree_code) (primarg0)->common.code) == NOP_EXPR || ((enum tree_code) (primarg0)->common.code) == CONVERT_EXPR || ((enum tree_code) (primarg0)->common.code) == NON_LVALUE_EXPR) && ((primarg0)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((primarg0)->common.type))->type.mode) == ((((((primarg0)->exp.operands[0]))->common.type))->type.mode))) (primarg0) = ((primarg0)->exp.operands[0]);
  while ((((enum tree_code) (primarg1)->common.code) == NOP_EXPR || ((enum tree_code) (primarg1)->common.code) == CONVERT_EXPR || ((enum tree_code) (primarg1)->common.code) == NON_LVALUE_EXPR) && ((primarg1)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((primarg1)->common.type))->type.mode) == ((((((primarg1)->exp.operands[0]))->common.type))->type.mode))) (primarg1) = ((primarg1)->exp.operands[0]);
  if (operand_equal_p (primarg0, primarg1, 0))
    return 1;







  primarg1 = get_narrower (arg1, &unsignedp1);
  primother = get_narrower (other, &unsignedpo);

  correct_width = ((((arg1)->common.type))->type.precision);
  if (unsignedp1 == unsignedpo
      && ((((primarg1)->common.type))->type.precision) < correct_width
      && ((((primother)->common.type))->type.precision) < correct_width)
    {
      tree type = ((arg0)->common.type);



      primarg1 = convert (signed_or_unsigned_type (unsignedp1,
                                                   ((primarg1)->common.type)),
                          primarg1);

      if (operand_equal_p (arg0, convert (type, primarg1), 0))
        return 1;
    }

  return 0;
}

static int
twoval_comparison_p (arg, cval1, cval2, save_p)
     tree arg;
     tree *cval1, *cval2;
     int *save_p;
{
  enum tree_code code = ((enum tree_code) (arg)->common.code);
  char class = tree_code_type[(int) (code)];


  if (class == 'e' && code == TRUTH_NOT_EXPR)
    class = '1';
  else if (class == 'e'
           && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR
               || code == COMPOUND_EXPR))
    class = '2';

  else if (class == 'e' && code == SAVE_EXPR && (*(rtx *) &(arg)->exp.operands[2]) == 0
           && ! ((((arg)->exp.operands[0]))->common.side_effects_flag))
    {


      if (*cval1 || *cval2)
        return 0;

      class = '1';
      *save_p = 1;
    }

  switch (class)
    {
    case '1':
      return twoval_comparison_p (((arg)->exp.operands[0]), cval1, cval2, save_p);

    case '2':
      return (twoval_comparison_p (((arg)->exp.operands[0]), cval1, cval2, save_p)
              && twoval_comparison_p (((arg)->exp.operands[1]),
                                      cval1, cval2, save_p));

    case 'c':
      return 1;

    case 'e':
      if (code == COND_EXPR)
        return (twoval_comparison_p (((arg)->exp.operands[0]),
                                     cval1, cval2, save_p)
                && twoval_comparison_p (((arg)->exp.operands[1]),
                                        cval1, cval2, save_p)
                && twoval_comparison_p (((arg)->exp.operands[2]),
                                        cval1, cval2, save_p));
      return 0;

    case '<':






      if (operand_equal_p (((arg)->exp.operands[0]),
                           ((arg)->exp.operands[1]), 0))
        return 0;

      if (*cval1 == 0)
        *cval1 = ((arg)->exp.operands[0]);
      else if (operand_equal_p (*cval1, ((arg)->exp.operands[0]), 0))
        ;
      else if (*cval2 == 0)
        *cval2 = ((arg)->exp.operands[0]);
      else if (operand_equal_p (*cval2, ((arg)->exp.operands[0]), 0))
        ;
      else
        return 0;

      if (operand_equal_p (*cval1, ((arg)->exp.operands[1]), 0))
        ;
      else if (*cval2 == 0)
        *cval2 = ((arg)->exp.operands[1]);
      else if (operand_equal_p (*cval2, ((arg)->exp.operands[1]), 0))
        ;
      else
        return 0;

      return 1;

    default:
      return 0;
    }
}






static tree
eval_subst (arg, old0, new0, old1, new1)
     tree arg;
     tree old0, new0, old1, new1;
{
  tree type = ((arg)->common.type);
  enum tree_code code = ((enum tree_code) (arg)->common.code);
  char class = tree_code_type[(int) (code)];


  if (class == 'e' && code == TRUTH_NOT_EXPR)
    class = '1';
  else if (class == 'e'
           && (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR))
    class = '2';

  switch (class)
    {
    case '1':
      return fold (build1 (code, type,
                           eval_subst (((arg)->exp.operands[0]),
                                       old0, new0, old1, new1)));

    case '2':
      return fold (build (code, type,
                          eval_subst (((arg)->exp.operands[0]),
                                      old0, new0, old1, new1),
                          eval_subst (((arg)->exp.operands[1]),
                                      old0, new0, old1, new1)));

    case 'e':
      switch (code)
        {
        case SAVE_EXPR:
          return eval_subst (((arg)->exp.operands[0]), old0, new0, old1, new1);

        case COMPOUND_EXPR:
          return eval_subst (((arg)->exp.operands[1]), old0, new0, old1, new1);

        case COND_EXPR:
          return fold (build (code, type,
                              eval_subst (((arg)->exp.operands[0]),
                                          old0, new0, old1, new1),
                              eval_subst (((arg)->exp.operands[1]),
                                          old0, new0, old1, new1),
                              eval_subst (((arg)->exp.operands[2]),
                                          old0, new0, old1, new1)));
        default:
          break;
        }


    case '<':
      {
        tree arg0 = ((arg)->exp.operands[0]);
        tree arg1 = ((arg)->exp.operands[1]);





        if (arg0 == old0 || operand_equal_p (arg0, old0, 0))
          arg0 = new0;
        else if (arg0 == old1 || operand_equal_p (arg0, old1, 0))
          arg0 = new1;

        if (arg1 == old0 || operand_equal_p (arg1, old0, 0))
          arg1 = new0;
        else if (arg1 == old1 || operand_equal_p (arg1, old1, 0))
          arg1 = new1;

        return fold (build (code, type, arg0, arg1));
      }

    default:
      return arg;
    }
}

static tree
omit_one_operand (type, result, omitted)
     tree type, result, omitted;
{
  tree t = convert (type, result);

  if (((omitted)->common.side_effects_flag))
    return build (COMPOUND_EXPR, type, omitted, t);

  return non_lvalue (t);
}



static tree
pedantic_omit_one_operand (type, result, omitted)
     tree type, result, omitted;
{
  tree t = convert (type, result);

  if (((omitted)->common.side_effects_flag))
    return build (COMPOUND_EXPR, type, omitted, t);

  return pedantic_non_lvalue (t);
}





tree
invert_truthvalue (arg)
     tree arg;
{
  tree type = ((arg)->common.type);
  enum tree_code code = ((enum tree_code) (arg)->common.code);

  if (code == ERROR_MARK)
    return arg;





  if (tree_code_type[(int) (code)] == '<')
    {
      if ((((enum tree_code) (((((arg)->exp.operands[0]))->common.type))->common.code) == REAL_TYPE || (((enum tree_code) (((((arg)->exp.operands[0]))->common.type))->common.code) == COMPLEX_TYPE && ((enum tree_code) (((((((arg)->exp.operands[0]))->common.type))->common.type))->common.code) == REAL_TYPE))
          && !flag_unsafe_math_optimizations
          && code != NE_EXPR
          && code != EQ_EXPR)
        return build1 (TRUTH_NOT_EXPR, type, arg);
      else
        return build (invert_tree_comparison (code), type,
                      ((arg)->exp.operands[0]), ((arg)->exp.operands[1]));
    }

  switch (code)
    {
    case INTEGER_CST:
      return convert (type, build_int_2_wide ((unsigned long long) (integer_zerop (arg)), (long long) (0)));

    case TRUTH_AND_EXPR:
      return build (TRUTH_OR_EXPR, type,
                    invert_truthvalue (((arg)->exp.operands[0])),
                    invert_truthvalue (((arg)->exp.operands[1])));

    case TRUTH_OR_EXPR:
      return build (TRUTH_AND_EXPR, type,
                    invert_truthvalue (((arg)->exp.operands[0])),
                    invert_truthvalue (((arg)->exp.operands[1])));

    case TRUTH_XOR_EXPR:





      if (((enum tree_code) (((arg)->exp.operands[1]))->common.code) == TRUTH_NOT_EXPR)
        return build (TRUTH_XOR_EXPR, type, ((arg)->exp.operands[0]),
                      ((((arg)->exp.operands[1]))->exp.operands[0]));
      else
        return build (TRUTH_XOR_EXPR, type,
                      invert_truthvalue (((arg)->exp.operands[0])),
                      ((arg)->exp.operands[1]));

    case TRUTH_ANDIF_EXPR:
      return build (TRUTH_ORIF_EXPR, type,
                    invert_truthvalue (((arg)->exp.operands[0])),
                    invert_truthvalue (((arg)->exp.operands[1])));

    case TRUTH_ORIF_EXPR:
      return build (TRUTH_ANDIF_EXPR, type,
                    invert_truthvalue (((arg)->exp.operands[0])),
                    invert_truthvalue (((arg)->exp.operands[1])));

    case TRUTH_NOT_EXPR:
      return ((arg)->exp.operands[0]);

    case COND_EXPR:
      return build (COND_EXPR, type, ((arg)->exp.operands[0]),
                    invert_truthvalue (((arg)->exp.operands[1])),
                    invert_truthvalue (((arg)->exp.operands[2])));

    case COMPOUND_EXPR:
      return build (COMPOUND_EXPR, type, ((arg)->exp.operands[0]),
                    invert_truthvalue (((arg)->exp.operands[1])));

    case WITH_RECORD_EXPR:
      return build (WITH_RECORD_EXPR, type,
                    invert_truthvalue (((arg)->exp.operands[0])),
                    ((arg)->exp.operands[1]));

    case NON_LVALUE_EXPR:
      return invert_truthvalue (((arg)->exp.operands[0]));

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
      return build1 (((enum tree_code) (arg)->common.code), type,
                     invert_truthvalue (((arg)->exp.operands[0])));

    case BIT_AND_EXPR:
      if (!integer_onep (((arg)->exp.operands[1])))
        break;
      return build (EQ_EXPR, type, arg, convert (type, global_trees[TI_INTEGER_ZERO]));

    case SAVE_EXPR:
      return build1 (TRUTH_NOT_EXPR, type, arg);

    case CLEANUP_POINT_EXPR:
      return build1 (CLEANUP_POINT_EXPR, type,
                     invert_truthvalue (((arg)->exp.operands[0])));

    default:
      break;
    }
  if (((enum tree_code) (((arg)->common.type))->common.code) != BOOLEAN_TYPE)
    fancy_abort ("fold-const.c", 2934, __FUNCTION__);
  return build1 (TRUTH_NOT_EXPR, type, arg);
}

static tree
distribute_bit_expr (code, type, arg0, arg1)
     enum tree_code code;
     tree type;
     tree arg0, arg1;
{
  tree common;
  tree left, right;

  if (((enum tree_code) (arg0)->common.code) != ((enum tree_code) (arg1)->common.code)
      || ((enum tree_code) (arg0)->common.code) == code
      || (((enum tree_code) (arg0)->common.code) != BIT_AND_EXPR
          && ((enum tree_code) (arg0)->common.code) != BIT_IOR_EXPR))
    return 0;

  if (operand_equal_p (((arg0)->exp.operands[0]), ((arg1)->exp.operands[0]), 0))
    {
      common = ((arg0)->exp.operands[0]);
      left = ((arg0)->exp.operands[1]);
      right = ((arg1)->exp.operands[1]);
    }
  else if (operand_equal_p (((arg0)->exp.operands[0]), ((arg1)->exp.operands[1]), 0))
    {
      common = ((arg0)->exp.operands[0]);
      left = ((arg0)->exp.operands[1]);
      right = ((arg1)->exp.operands[0]);
    }
  else if (operand_equal_p (((arg0)->exp.operands[1]), ((arg1)->exp.operands[0]), 0))
    {
      common = ((arg0)->exp.operands[1]);
      left = ((arg0)->exp.operands[0]);
      right = ((arg1)->exp.operands[1]);
    }
  else if (operand_equal_p (((arg0)->exp.operands[1]), ((arg1)->exp.operands[1]), 0))
    {
      common = ((arg0)->exp.operands[1]);
      left = ((arg0)->exp.operands[0]);
      right = ((arg1)->exp.operands[0]);
    }
  else
    return 0;

  return fold (build (((enum tree_code) (arg0)->common.code), type, common,
                      fold (build (code, type, left, right))));
}




static tree
make_bit_field_ref (inner, type, bitsize, bitpos, unsignedp)
     tree inner;
     tree type;
     int bitsize, bitpos;
     int unsignedp;
{
  tree result = build (BIT_FIELD_REF, type, inner,
                       size_int_wide ((long long) (bitsize), SIZETYPE), size_int_wide ((long long) (bitpos), BITSIZETYPE));

  ((result)->common.unsigned_flag) = unsignedp;

  return result;
}

static tree
optimize_bit_field_compare (code, compare_type, lhs, rhs)
     enum tree_code code;
     tree compare_type;
     tree lhs, rhs;
{
  long long lbitpos, lbitsize, rbitpos, rbitsize, nbitpos, nbitsize;
  tree type = ((lhs)->common.type);
  tree signed_type, unsigned_type;
  int const_p = ((enum tree_code) (rhs)->common.code) == INTEGER_CST;
  enum machine_mode lmode, rmode, nmode;
  int lunsignedp, runsignedp;
  int lvolatilep = 0, rvolatilep = 0;
  tree linner, rinner = (tree) ((void *)0);
  tree mask;
  tree offset;






  linner = get_inner_reference (lhs, &lbitsize, &lbitpos, &offset, &lmode,
                                &lunsignedp, &lvolatilep);
  if (linner == lhs || lbitsize == (mode_bitsize[(int) (lmode)]) || lbitsize < 0
      || offset != 0 || ((enum tree_code) (linner)->common.code) == PLACEHOLDER_EXPR)
    return 0;

 if (!const_p)
   {


     rinner = get_inner_reference (rhs, &rbitsize, &rbitpos, &offset, &rmode,
                                   &runsignedp, &rvolatilep);

     if (rinner == rhs || lbitpos != rbitpos || lbitsize != rbitsize
         || lunsignedp != runsignedp || offset != 0
         || ((enum tree_code) (rinner)->common.code) == PLACEHOLDER_EXPR)
       return 0;
   }



  nmode = get_best_mode (lbitsize, lbitpos,
                         const_p ? ((((linner)->common.type))->type.align)
                         : ((((((linner)->common.type))->type.align)) < (((((rinner)->common.type))->type.align)) ? (((((linner)->common.type))->type.align)) : (((((rinner)->common.type))->type.align))),

                         word_mode, lvolatilep || rvolatilep);
  if (nmode == VOIDmode)
    return 0;



  signed_type = type_for_mode (nmode, 0);
  unsigned_type = type_for_mode (nmode, 1);




  nbitsize = (mode_bitsize[(int) (nmode)]);
  nbitpos = lbitpos & ~ (nbitsize - 1);
  lbitpos -= nbitpos;
  if (nbitsize == lbitsize)
    return 0;

  if (0)
    lbitpos = nbitsize - lbitsize - lbitpos;


  mask = build_int_2_wide ((unsigned long long) (~0), (long long) (~0));
  ((mask)->common.type) = unsigned_type;
  force_fit_type (mask, 0);
  mask = convert (unsigned_type, mask);
  mask = const_binop (LSHIFT_EXPR, mask, size_int_wide ((long long) (nbitsize - lbitsize), SIZETYPE), 0);
  mask = const_binop (RSHIFT_EXPR, mask,
                      size_int_wide ((long long) (nbitsize - lbitsize - lbitpos), SIZETYPE), 0);

  if (! const_p)


    return build (code, compare_type,
                  build (BIT_AND_EXPR, unsigned_type,
                         make_bit_field_ref (linner, unsigned_type,
                                             nbitsize, nbitpos, 1),
                         mask),
                  build (BIT_AND_EXPR, unsigned_type,
                         make_bit_field_ref (rinner, unsigned_type,
                                             nbitsize, nbitpos, 1),
                         mask));

  if (lunsignedp)
    {
      if (! integer_zerop (const_binop (RSHIFT_EXPR,
                                        convert (unsigned_type, rhs),
                                        size_int_wide ((long long) (lbitsize), SIZETYPE), 0)))
        {
          warning ("comparison is always %d due to width of bit-field",
                   code == NE_EXPR);
          return convert (compare_type,
                          (code == NE_EXPR
                           ? global_trees[TI_INTEGER_ONE] : global_trees[TI_INTEGER_ZERO]));
        }
    }
  else
    {
      tree tem = const_binop (RSHIFT_EXPR, convert (signed_type, rhs),
                              size_int_wide ((long long) (lbitsize - 1), SIZETYPE), 0);
      if (! integer_zerop (tem) && ! integer_all_onesp (tem))
        {
          warning ("comparison is always %d due to width of bit-field",
                   code == NE_EXPR);
          return convert (compare_type,
                          (code == NE_EXPR
                           ? global_trees[TI_INTEGER_ONE] : global_trees[TI_INTEGER_ZERO]));
        }
    }


  if (lbitsize == 1 && ! integer_zerop (rhs))
    {
      code = code == EQ_EXPR ? NE_EXPR : EQ_EXPR;
      rhs = convert (type, global_trees[TI_INTEGER_ZERO]);
    }




  lhs = make_bit_field_ref (linner, unsigned_type, nbitsize, nbitpos, 1);
  if (lvolatilep)
    {
      ((lhs)->common.side_effects_flag) = 1;
      ((lhs)->common.volatile_flag) = 1;
    }

  rhs = fold (const_binop (BIT_AND_EXPR,
                           const_binop (LSHIFT_EXPR,
                                        convert (unsigned_type, rhs),
                                        size_int_wide ((long long) (lbitpos), SIZETYPE), 0),
                           mask, 0));

  return build (code, compare_type,
                build (BIT_AND_EXPR, unsigned_type, lhs, mask),
                rhs);
}

static tree
decode_field_reference (exp, pbitsize, pbitpos, pmode, punsignedp,
                        pvolatilep, pmask, pand_mask)
     tree exp;
     long long *pbitsize, *pbitpos;
     enum machine_mode *pmode;
     int *punsignedp, *pvolatilep;
     tree *pmask;
     tree *pand_mask;
{
  tree and_mask = 0;
  tree mask, inner, offset;
  tree unsigned_type;
  unsigned int precision;




  if (! (((enum tree_code) (((exp)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((exp)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((exp)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((exp)->common.type))->common.code) == CHAR_TYPE))
    return 0;

  while ((((enum tree_code) (exp)->common.code) == NOP_EXPR || ((enum tree_code) (exp)->common.code) == CONVERT_EXPR || ((enum tree_code) (exp)->common.code) == NON_LVALUE_EXPR) && ((exp)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((exp)->common.type))->type.mode) == ((((((exp)->exp.operands[0]))->common.type))->type.mode))) (exp) = ((exp)->exp.operands[0]);

  if (((enum tree_code) (exp)->common.code) == BIT_AND_EXPR)
    {
      and_mask = ((exp)->exp.operands[1]);
      exp = ((exp)->exp.operands[0]);
      while ((((enum tree_code) (exp)->common.code) == NOP_EXPR || ((enum tree_code) (exp)->common.code) == CONVERT_EXPR || ((enum tree_code) (exp)->common.code) == NON_LVALUE_EXPR) && ((exp)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((exp)->common.type))->type.mode) == ((((((exp)->exp.operands[0]))->common.type))->type.mode))) (exp) = ((exp)->exp.operands[0]); while ((((enum tree_code) (and_mask)->common.code) == NOP_EXPR || ((enum tree_code) (and_mask)->common.code) == CONVERT_EXPR || ((enum tree_code) (and_mask)->common.code) == NON_LVALUE_EXPR) && ((and_mask)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((and_mask)->common.type))->type.mode) == ((((((and_mask)->exp.operands[0]))->common.type))->type.mode))) (and_mask) = ((and_mask)->exp.operands[0]);
      if (((enum tree_code) (and_mask)->common.code) != INTEGER_CST)
        return 0;
    }

  inner = get_inner_reference (exp, pbitsize, pbitpos, &offset, pmode,
                               punsignedp, pvolatilep);
  if ((inner == exp && and_mask == 0)
      || *pbitsize < 0 || offset != 0
      || ((enum tree_code) (inner)->common.code) == PLACEHOLDER_EXPR)
    return 0;


  unsigned_type = type_for_size (*pbitsize, 1);
  precision = ((unsigned_type)->type.precision);

  mask = build_int_2_wide ((unsigned long long) (~0), (long long) (~0));
  ((mask)->common.type) = unsigned_type;
  force_fit_type (mask, 0);
  mask = const_binop (LSHIFT_EXPR, mask, size_int_wide ((long long) (precision - *pbitsize), SIZETYPE), 0);
  mask = const_binop (RSHIFT_EXPR, mask, size_int_wide ((long long) (precision - *pbitsize), SIZETYPE), 0);


  if (and_mask != 0)
    mask = fold (build (BIT_AND_EXPR, unsigned_type,
                        convert (unsigned_type, and_mask), mask));

  *pmask = mask;
  *pand_mask = and_mask;
  return inner;
}




static int
all_ones_mask_p (mask, size)
     tree mask;
     int size;
{
  tree type = ((mask)->common.type);
  unsigned int precision = ((type)->type.precision);
  tree tmask;

  tmask = build_int_2_wide ((unsigned long long) (~0), (long long) (~0));
  ((tmask)->common.type) = signed_type (type);
  force_fit_type (tmask, 0);
  return
    tree_int_cst_equal (mask,
                        const_binop (RSHIFT_EXPR,
                                     const_binop (LSHIFT_EXPR, tmask,
                                                  size_int_wide ((long long) (precision - size), SIZETYPE),
                                                  0),
                                     size_int_wide ((long long) (precision - size), SIZETYPE), 0));
}




static int
simple_operand_p (exp)
     tree exp;
{

  while ((((enum tree_code) (exp)->common.code) == NOP_EXPR
          || ((enum tree_code) (exp)->common.code) == CONVERT_EXPR)
         && (((((exp)->common.type))->type.mode)
             == ((((((exp)->exp.operands[0]))->common.type))->type.mode)))
    exp = ((exp)->exp.operands[0]);

  return (tree_code_type[(int) (((enum tree_code) (exp)->common.code))] == 'c'
          || ((tree_code_type[(int) (((enum tree_code) (exp)->common.code))] == 'd')
              && ! ((exp)->common.addressable_flag)
              && ! ((exp)->common.volatile_flag)
              && ! ((exp)->decl.nonlocal_flag)



              && ! ((exp)->common.public_flag)
              && ! ((exp)->decl.external_flag)


              && (! ((exp)->common.static_flag) || ((exp)->decl.regdecl_flag))));
}

static tree
range_binop (code, type, arg0, upper0_p, arg1, upper1_p)
     enum tree_code code;
     tree type;
     tree arg0, arg1;
     int upper0_p, upper1_p;
{
  tree tem;
  int result;
  int sgn0, sgn1;






  if (arg0 != 0 && arg1 != 0)
    {
      tem = fold (build (code, type != 0 ? type : ((arg0)->common.type),
                         arg0, convert (((arg0)->common.type), arg1)));
      while ((((enum tree_code) (tem)->common.code) == NOP_EXPR || ((enum tree_code) (tem)->common.code) == CONVERT_EXPR || ((enum tree_code) (tem)->common.code) == NON_LVALUE_EXPR) && ((tem)->exp.operands[0]) != global_trees[TI_ERROR_MARK] && (((((tem)->common.type))->type.mode) == ((((((tem)->exp.operands[0]))->common.type))->type.mode))) (tem) = ((tem)->exp.operands[0]);
      return ((enum tree_code) (tem)->common.code) == INTEGER_CST ? tem : 0;
    }

  if (tree_code_type[(int) (code)] != '<')
    return 0;







  sgn0 = arg0 != 0 ? 0 : (upper0_p ? 1 : -1);
  sgn1 = arg1 != 0 ? 0 : (upper1_p ? 1 : -1);
  switch (code)
    {
    case EQ_EXPR:
      result = sgn0 == sgn1;
      break;
    case NE_EXPR:
      result = sgn0 != sgn1;
      break;
    case LT_EXPR:
      result = sgn0 < sgn1;
      break;
    case LE_EXPR:
      result = sgn0 <= sgn1;
      break;
    case GT_EXPR:
      result = sgn0 > sgn1;
      break;
    case GE_EXPR:
      result = sgn0 >= sgn1;
      break;
    default:
      fancy_abort ("fold-const.c", 3411, __FUNCTION__);
    }

  return convert (type, result ? global_trees[TI_INTEGER_ONE] : global_trees[TI_INTEGER_ZERO]);
}







static tree
make_range (exp, pin_p, plow, phigh)
     tree exp;
     int *pin_p;
     tree *plow, *phigh;
{
  enum tree_code code;
  tree arg0 = (tree) ((void *)0), arg1 = (tree) ((void *)0), type = (tree) ((void *)0);
  tree orig_type = (tree) ((void *)0);
  int in_p, n_in_p;
  tree low, high, n_low, n_high;







  in_p = 0, low = high = convert (((exp)->common.type), global_trees[TI_INTEGER_ZERO]);

  while (1)
    {
      code = ((enum tree_code) (exp)->common.code);

      if (((tree_code_type[(int) (code)]) == '<' || (tree_code_type[(int) (code)]) == '1' || (tree_code_type[(int) (code)]) == '2' || (tree_code_type[(int) (code)]) == 'e'))
        {
          arg0 = ((exp)->exp.operands[0]);
          if (tree_code_type[(int) (code)] == '<'
              || tree_code_type[(int) (code)] == '1'
              || tree_code_type[(int) (code)] == '2')
            type = ((arg0)->common.type);
          if (tree_code_type[(int) (code)] == '2'
              || tree_code_type[(int) (code)] == '<'
              || (tree_code_type[(int) (code)] == 'e'
                  && tree_code_length[(int) (code)] > 1))
            arg1 = ((exp)->exp.operands[1]);
        }



      if (type != (tree) ((void *)0) && orig_type == (tree) ((void *)0))
        orig_type = type;

      switch (code)
        {
        case TRUTH_NOT_EXPR:
          in_p = ! in_p, exp = arg0;
          continue;

        case EQ_EXPR: case NE_EXPR:
        case LT_EXPR: case LE_EXPR: case GE_EXPR: case GT_EXPR:





          if (low == 0 || high == 0
              || ! integer_zerop (low) || ! integer_zerop (high)
              || ((enum tree_code) (arg1)->common.code) != INTEGER_CST)
            break;

          switch (code)
            {
            case NE_EXPR:
              low = high = arg1;
              break;
            case EQ_EXPR:
              in_p = ! in_p, low = high = arg1;
              break;
            case GT_EXPR:
              low = 0, high = arg1;
              break;
            case GE_EXPR:
              in_p = ! in_p, low = arg1, high = 0;
              break;
            case LT_EXPR:
              low = arg1, high = 0;
              break;
            case LE_EXPR:
              in_p = ! in_p, low = 0, high = arg1;
              break;
            default:
              fancy_abort ("fold-const.c", 3505, __FUNCTION__);
            }

          exp = arg0;





          if (((type)->common.unsigned_flag) && (low == 0 || high == 0))
            {
              if (! merge_ranges (&n_in_p, &n_low, &n_high, in_p, low, high,
                                  1, convert (type, global_trees[TI_INTEGER_ZERO]),
                                  (tree) ((void *)0)))
                break;

              in_p = n_in_p, low = n_low, high = n_high;




              if (high == 0 && low)
                {
                  in_p = ! in_p;
                  high = range_binop (MINUS_EXPR, (tree) ((void *)0), low, 0,
                                      global_trees[TI_INTEGER_ONE], 0);
                  low = convert (type, global_trees[TI_INTEGER_ZERO]);
                }
            }
          continue;

        case NEGATE_EXPR:

          n_low = range_binop (MINUS_EXPR, type,
                               convert (type, global_trees[TI_INTEGER_ZERO]), 0, high, 1);
          n_high = range_binop (MINUS_EXPR, type,
                                convert (type, global_trees[TI_INTEGER_ZERO]), 0, low, 0);
          low = n_low, high = n_high;
          exp = arg0;
          continue;

        case BIT_NOT_EXPR:

          exp = build (MINUS_EXPR, type, negate_expr (arg0),
                       convert (type, global_trees[TI_INTEGER_ONE]));
          continue;

        case PLUS_EXPR: case MINUS_EXPR:
          if (((enum tree_code) (arg1)->common.code) != INTEGER_CST)
            break;





          n_low = range_binop (code == MINUS_EXPR ? PLUS_EXPR : MINUS_EXPR,
                               type, low, 0, arg1, 0);
          n_high = range_binop (code == MINUS_EXPR ? PLUS_EXPR : MINUS_EXPR,
                                type, high, 1, arg1, 0);
          if ((n_low != 0 && ((n_low)->common.public_flag))
              || (n_high != 0 && ((n_high)->common.public_flag)))
            break;



          if (n_low && n_high && tree_int_cst_lt (n_high, n_low))
            {
              low = range_binop (PLUS_EXPR, type, n_high, 0,
                                 global_trees[TI_INTEGER_ONE], 0);
              high = range_binop (MINUS_EXPR, type, n_low, 0,
                                  global_trees[TI_INTEGER_ONE], 0);





              if (tree_int_cst_equal (n_low, low)
                  && tree_int_cst_equal (n_high, high))
                low = high = 0;
              else
                in_p = ! in_p;
            }
          else
            low = n_low, high = n_high;

          exp = arg0;
          continue;

        case NOP_EXPR: case NON_LVALUE_EXPR: case CONVERT_EXPR:
          if (((type)->type.precision) > ((orig_type)->type.precision))
            break;

          if (! (((enum tree_code) (type)->common.code) == INTEGER_TYPE || ((enum tree_code) (type)->common.code) == ENUMERAL_TYPE || ((enum tree_code) (type)->common.code) == BOOLEAN_TYPE || ((enum tree_code) (type)->common.code) == CHAR_TYPE)
              || (low != 0 && ! int_fits_type_p (low, type))
              || (high != 0 && ! int_fits_type_p (high, type)))
            break;

          n_low = low, n_high = high;

          if (n_low != 0)
            n_low = convert (type, n_low);

          if (n_high != 0)
            n_high = convert (type, n_high);







          if (((type)->common.unsigned_flag) && ! ((((exp)->common.type))->common.unsigned_flag))
            {
              tree equiv_type = type_for_mode (((type)->type.mode), 1);
              tree high_positive;




              high_positive
                = ((equiv_type)->type.maxval) ? ((equiv_type)->type.maxval)
                  : ((type)->type.maxval);

              high_positive = fold (build (RSHIFT_EXPR, type,
                                           convert (type, high_positive),
                                           convert (type, global_trees[TI_INTEGER_ONE])));




              if (low != 0)
                {
                  if (! merge_ranges (&n_in_p, &n_low, &n_high,
                                      1, n_low, n_high,
                                      1, convert (type, global_trees[TI_INTEGER_ZERO]),
                                      high_positive))
                    break;

                  in_p = (n_in_p == in_p);
                }
              else
                {


                  if (! merge_ranges (&n_in_p, &n_low, &n_high,
                                      0, n_low, n_high,
                                      1, convert (type, global_trees[TI_INTEGER_ZERO]),
                                      high_positive))
                    break;

                  in_p = (in_p != n_in_p);
                }
            }

          exp = arg0;
          low = n_low, high = n_high;
          continue;

        default:
          break;
        }

      break;
    }


  if (((enum tree_code) (exp)->common.code) == INTEGER_CST)
    {
      in_p = in_p == (integer_onep (range_binop (GE_EXPR, integer_types[itk_int],
                                                 exp, 0, low, 0))
                      && integer_onep (range_binop (LE_EXPR, integer_types[itk_int],
                                                    exp, 1, high, 1)));
      low = high = 0;
      exp = 0;
    }

  *pin_p = in_p, *plow = low, *phigh = high;
  return exp;
}





static tree
build_range_check (type, exp, in_p, low, high)
     tree type;
     tree exp;
     int in_p;
     tree low, high;
{
  tree etype = ((exp)->common.type);
  tree utype, value;

  if (! in_p
      && (0 != (value = build_range_check (type, exp, 1, low, high))))
    return invert_truthvalue (value);

  else if (low == 0 && high == 0)
    return convert (type, global_trees[TI_INTEGER_ONE]);

  else if (low == 0)
    return fold (build (LE_EXPR, type, exp, high));

  else if (high == 0)
    return fold (build (GE_EXPR, type, exp, low));

  else if (operand_equal_p (low, high, 0))
    return fold (build (EQ_EXPR, type, exp, low));

  else if (((etype)->common.unsigned_flag) && integer_zerop (low))
    return build_range_check (type, exp, 1, 0, high);

  else if (integer_zerop (low))
    {
      utype = unsigned_type (etype);
      return build_range_check (type, convert (utype, exp), 1, 0,
                                convert (utype, high));
    }

  else if (0 != (value = const_binop (MINUS_EXPR, high, low, 0))
           && ! ((value)->common.public_flag))
    return build_range_check (type,
                              fold (build (MINUS_EXPR, etype, exp, low)),
                              1, convert (etype, global_trees[TI_INTEGER_ZERO]), value);
  else
    return 0;
}




static int
merge_ranges (pin_p, plow, phigh, in0_p, low0, high0, in1_p, low1, high1)
     int *pin_p;
     tree *plow, *phigh;
     int in0_p, in1_p;
     tree low0, high0, low1, high1;
{
  int no_overlap;
  int subset;
  int temp;
  tree tem;
  int in_p;
  tree low, high;
  int lowequal = ((low0 == 0 && low1 == 0)
                  || integer_onep (range_binop (EQ_EXPR, integer_types[itk_int],
                                                low0, 0, low1, 0)));
  int highequal = ((high0 == 0 && high1 == 0)
                   || integer_onep (range_binop (EQ_EXPR, integer_types[itk_int],
                                                 high0, 1, high1, 1)));



  if (integer_onep (range_binop (GT_EXPR, integer_types[itk_int],
                                 low0, 0, low1, 0))
      || (lowequal
          && integer_onep (range_binop (GT_EXPR, integer_types[itk_int],
                                        high1, 1, high0, 1))))
    {
      temp = in0_p, in0_p = in1_p, in1_p = temp;
      tem = low0, low0 = low1, low1 = tem;
      tem = high0, high0 = high1, high1 = tem;
    }




  no_overlap = integer_onep (range_binop (LT_EXPR, integer_types[itk_int],
                                          high0, 1, low1, 0));
  subset = integer_onep (range_binop (LE_EXPR, integer_types[itk_int],
                                      high1, 1, high0, 1));



  if (in0_p && in1_p)
    {



      if (no_overlap)
        in_p = 0, low = high = 0;
      else if (subset)
        in_p = 1, low = low1, high = high1;
      else
        in_p = 1, low = low1, high = high0;
    }

  else if (in0_p && ! in1_p)
    {

      if (no_overlap)
        in_p = 1, low = low0, high = high0;
      else if (lowequal && highequal)
        in_p = 0, low = high = 0;
      else if (subset && lowequal)
        {
          in_p = 1, high = high0;
          low = range_binop (PLUS_EXPR, (tree) ((void *)0), high1, 0,
                             global_trees[TI_INTEGER_ONE], 0);
        }
      else if (! subset || highequal)
        {
          in_p = 1, low = low0;
          high = range_binop (MINUS_EXPR, (tree) ((void *)0), low1, 0,
                              global_trees[TI_INTEGER_ONE], 0);
        }
      else
        return 0;
    }

  else if (! in0_p && in1_p)
    {




      if (no_overlap)
        in_p = 1, low = low1, high = high1;
      else if (subset || highequal)
        in_p = 0, low = high = 0;
      else
        {
          in_p = 1, high = high1;
          low = range_binop (PLUS_EXPR, (tree) ((void *)0), high0, 1,
                             global_trees[TI_INTEGER_ONE], 0);
        }
    }

  else
    {






      if (no_overlap)
        {
          if (integer_onep (range_binop (EQ_EXPR, integer_types[itk_int],
                                         range_binop (PLUS_EXPR, (tree) ((void *)0),
                                                      high0, 1,
                                                      global_trees[TI_INTEGER_ONE], 1),
                                         1, low1, 0)))
            in_p = 0, low = low0, high = high1;
          else
            return 0;
        }
      else if (subset)
        in_p = 0, low = low0, high = high0;
      else
        in_p = 0, low = low0, high = high1;
    }

  *pin_p = in_p, *plow = low, *phigh = high;
  return 1;
}




static tree
fold_range_test (exp)
     tree exp;
{
  int or_op = (((enum tree_code) (exp)->common.code) == TRUTH_ORIF_EXPR
               || ((enum tree_code) (exp)->common.code) == TRUTH_OR_EXPR);
  int in0_p, in1_p, in_p;
  tree low0, low1, low, high0, high1, high;
  tree lhs = make_range (((exp)->exp.operands[0]), &in0_p, &low0, &high0);
  tree rhs = make_range (((exp)->exp.operands[1]), &in1_p, &low1, &high1);
  tree tem;



  if (or_op)
    in0_p = ! in0_p, in1_p = ! in1_p;





  if ((lhs == 0 || rhs == 0 || operand_equal_p (lhs, rhs, 0))
      && merge_ranges (&in_p, &low, &high, in0_p, low0, high0,
                       in1_p, low1, high1)
      && 0 != (tem = (build_range_check (((exp)->common.type),
                                         lhs != 0 ? lhs
                                         : rhs != 0 ? rhs : global_trees[TI_INTEGER_ZERO],
                                         in_p, low, high))))
    return or_op ? invert_truthvalue (tem) : tem;




  else if (ix86_branch_cost >= 2
           && lhs != 0 && rhs != 0
           && (((enum tree_code) (exp)->common.code) == TRUTH_ANDIF_EXPR
               || ((enum tree_code) (exp)->common.code) == TRUTH_ORIF_EXPR)
           && operand_equal_p (lhs, rhs, 0))
    {



      if (simple_operand_p (lhs))
        return build (((enum tree_code) (exp)->common.code) == TRUTH_ANDIF_EXPR
                      ? TRUTH_AND_EXPR : TRUTH_OR_EXPR,
                      ((exp)->common.type), ((exp)->exp.operands[0]),
                      ((exp)->exp.operands[1]));

      else if (global_bindings_p () == 0
               && ! contains_placeholder_p (lhs))
        {
          tree common = save_expr (lhs);

          if (0 != (lhs = build_range_check (((exp)->common.type), common,
                                             or_op ? ! in0_p : in0_p,
                                             low0, high0))
              && (0 != (rhs = build_range_check (((exp)->common.type), common,
                                                 or_op ? ! in1_p : in1_p,
                                                 low1, high1))))
            return build (((enum tree_code) (exp)->common.code) == TRUTH_ANDIF_EXPR
                          ? TRUTH_AND_EXPR : TRUTH_OR_EXPR,
                          ((exp)->common.type), lhs, rhs);
        }
    }

  return 0;
}






static tree
unextend (c, p, unsignedp, mask)
     tree c;
     int p;
     int unsignedp;
     tree mask;
{
  tree type = ((c)->common.type);
  int modesize = (mode_bitsize[(int) (((type)->type.mode))]);
  tree temp;

  if (p == modesize || unsignedp)
    return c;




  temp = const_binop (RSHIFT_EXPR, c, size_int_wide ((long long) (p - 1), SIZETYPE), 0);
  temp = const_binop (BIT_AND_EXPR, temp, size_int_wide ((long long) (1), SIZETYPE), 0);







  if (((type)->common.unsigned_flag))
    temp = convert (signed_type (type), temp);

  temp = const_binop (LSHIFT_EXPR, temp, size_int_wide ((long long) (modesize - 1), SIZETYPE), 0);
  temp = const_binop (RSHIFT_EXPR, temp, size_int_wide ((long long) (modesize - p - 1), SIZETYPE), 0);
  if (mask != 0)
    temp = const_binop (BIT_AND_EXPR, temp, convert (((c)->common.type), mask), 0);

  if (((type)->common.unsigned_flag))
    temp = convert (type, temp);

  return convert (type, const_binop (BIT_XOR_EXPR, c, temp, 0));
}


/* MJP attempting to reduce thesize of this recursive function */

static tree
fold_truthop (code, truth_type, lhs, rhs)
     enum tree_code code;
     tree truth_type, lhs, rhs;
{

  enum tree_code wanted_code;
  enum tree_code lcode, rcode;
  tree ll_arg, lr_arg, rl_arg, rr_arg;
  tree ll_inner, lr_inner, rl_inner, rr_inner;
  long long ll_bitsize, ll_bitpos, lr_bitsize, lr_bitpos;
  long long rl_bitsize, rl_bitpos, rr_bitsize, rr_bitpos;
  long long xll_bitpos, xlr_bitpos, xrl_bitpos, xrr_bitpos;
  long long lnbitsize, lnbitpos, rnbitsize, rnbitpos;
  int ll_unsignedp, lr_unsignedp, rl_unsignedp, rr_unsignedp;
  enum machine_mode ll_mode, lr_mode, rl_mode, rr_mode;
  enum machine_mode lnmode, rnmode;
  tree ll_mask, lr_mask, rl_mask, rr_mask;
  tree ll_and_mask, lr_and_mask, rl_and_mask, rr_and_mask;
  tree l_const, r_const;
  tree lntype, rntype, result;
  int first_bit, end_bit;
  int volatilep;





  if (((lhs)->common.side_effects_flag) || ((rhs)->common.side_effects_flag))
    return 0;

  lcode = ((enum tree_code) (lhs)->common.code);
  rcode = ((enum tree_code) (rhs)->common.code);

  if (lcode == BIT_AND_EXPR && integer_onep (((lhs)->exp.operands[1])))
    lcode = NE_EXPR, lhs = build (NE_EXPR, truth_type, lhs, global_trees[TI_INTEGER_ZERO]);

  if (rcode == BIT_AND_EXPR && integer_onep (((rhs)->exp.operands[1])))
    rcode = NE_EXPR, rhs = build (NE_EXPR, truth_type, rhs, global_trees[TI_INTEGER_ZERO]);

  if (tree_code_type[(int) (lcode)] != '<' || tree_code_type[(int) (rcode)] != '<')
    return 0;

  code = ((code == TRUTH_AND_EXPR || code == TRUTH_ANDIF_EXPR)
          ? TRUTH_AND_EXPR : TRUTH_OR_EXPR);

  ll_arg = ((lhs)->exp.operands[0]);
  lr_arg = ((lhs)->exp.operands[1]);
  rl_arg = ((rhs)->exp.operands[0]);
  rr_arg = ((rhs)->exp.operands[1]);







  if (ix86_branch_cost >= 2
      && ! (((enum tree_code) (((rl_arg)->common.type))->common.code) == REAL_TYPE || (((enum tree_code) (((rl_arg)->common.type))->common.code) == COMPLEX_TYPE && ((enum tree_code) (((((rl_arg)->common.type))->common.type))->common.code) == REAL_TYPE))
      && simple_operand_p (rl_arg)
      && simple_operand_p (rr_arg))
    return build (code, truth_type, lhs, rhs);




  if ((lcode != EQ_EXPR && lcode != NE_EXPR)
      || (rcode != EQ_EXPR && rcode != NE_EXPR))
    return 0;

  volatilep = 0;
  ll_inner = decode_field_reference (ll_arg,
                                     &ll_bitsize, &ll_bitpos, &ll_mode,
                                     &ll_unsignedp, &volatilep, &ll_mask,
                                     &ll_and_mask);
  lr_inner = decode_field_reference (lr_arg,
                                     &lr_bitsize, &lr_bitpos, &lr_mode,
                                     &lr_unsignedp, &volatilep, &lr_mask,
                                     &lr_and_mask);
  rl_inner = decode_field_reference (rl_arg,
                                     &rl_bitsize, &rl_bitpos, &rl_mode,
                                     &rl_unsignedp, &volatilep, &rl_mask,
                                     &rl_and_mask);
  rr_inner = decode_field_reference (rr_arg,
                                     &rr_bitsize, &rr_bitpos, &rr_mode,
                                     &rr_unsignedp, &volatilep, &rr_mask,
                                     &rr_and_mask);





  if (volatilep || ll_inner == 0 || rl_inner == 0
      || ! operand_equal_p (ll_inner, rl_inner, 0))
    return 0;

  if (((enum tree_code) (lr_arg)->common.code) == INTEGER_CST
      && ((enum tree_code) (rr_arg)->common.code) == INTEGER_CST)
    l_const = lr_arg, r_const = rr_arg;
  else if (lr_inner == 0 || rr_inner == 0
           || ! operand_equal_p (lr_inner, rr_inner, 0))
    return 0;
  else
    l_const = r_const = 0;





  wanted_code = (code == TRUTH_AND_EXPR ? EQ_EXPR : NE_EXPR);
  if (lcode != wanted_code)
    {
      if (l_const && integer_zerop (l_const) && integer_pow2p (ll_mask))
        {



          ll_unsignedp = 1;
          l_const = ll_mask;
        }
      else
        return 0;
    }


  if (rcode != wanted_code)
    {
      if (r_const && integer_zerop (r_const) && integer_pow2p (rl_mask))
        {
          rl_unsignedp = 1;
          r_const = rl_mask;
        }
      else
        return 0;
    }




  first_bit = ((ll_bitpos) < (rl_bitpos) ? (ll_bitpos) : (rl_bitpos));
  end_bit = ((ll_bitpos + ll_bitsize) > (rl_bitpos + rl_bitsize) ? (ll_bitpos + ll_bitsize) : (rl_bitpos + rl_bitsize));
  lnmode = get_best_mode (end_bit - first_bit, first_bit,
                          ((((ll_inner)->common.type))->type.align), word_mode,
                          volatilep);
  if (lnmode == VOIDmode)
    return 0;

  lnbitsize = (mode_bitsize[(int) (lnmode)]);
  lnbitpos = first_bit & ~ (lnbitsize - 1);
  lntype = type_for_size (lnbitsize, 1);
  xll_bitpos = ll_bitpos - lnbitpos, xrl_bitpos = rl_bitpos - lnbitpos;

  if (0)
    {
      xll_bitpos = lnbitsize - xll_bitpos - ll_bitsize;
      xrl_bitpos = lnbitsize - xrl_bitpos - rl_bitsize;
    }

  ll_mask = const_binop (LSHIFT_EXPR, convert (lntype, ll_mask),
                         size_int_wide ((long long) (xll_bitpos), SIZETYPE), 0);
  rl_mask = const_binop (LSHIFT_EXPR, convert (lntype, rl_mask),
                         size_int_wide ((long long) (xrl_bitpos), SIZETYPE), 0);

  if (l_const)
    {
      l_const = convert (lntype, l_const);
      l_const = unextend (l_const, ll_bitsize, ll_unsignedp, ll_and_mask);
      l_const = const_binop (LSHIFT_EXPR, l_const, size_int_wide ((long long) (xll_bitpos), SIZETYPE), 0);
      if (! integer_zerop (const_binop (BIT_AND_EXPR, l_const,
                                        fold (build1 (BIT_NOT_EXPR,
                                                      lntype, ll_mask)),
                                        0)))
        {
          warning ("comparison is always %d", wanted_code == NE_EXPR);

          return convert (truth_type,
                          wanted_code == NE_EXPR
                          ? global_trees[TI_INTEGER_ONE] : global_trees[TI_INTEGER_ZERO]);
        }
    }
  if (r_const)
    {
      r_const = convert (lntype, r_const);
      r_const = unextend (r_const, rl_bitsize, rl_unsignedp, rl_and_mask);
      r_const = const_binop (LSHIFT_EXPR, r_const, size_int_wide ((long long) (xrl_bitpos), SIZETYPE), 0);
      if (! integer_zerop (const_binop (BIT_AND_EXPR, r_const,
                                        fold (build1 (BIT_NOT_EXPR,
                                                      lntype, rl_mask)),
                                        0)))
        {
          warning ("comparison is always %d", wanted_code == NE_EXPR);

          return convert (truth_type,
                          wanted_code == NE_EXPR
                          ? global_trees[TI_INTEGER_ONE] : global_trees[TI_INTEGER_ZERO]);
        }
    }




  if (l_const == 0)
    {
      if (ll_bitsize != lr_bitsize || rl_bitsize != rr_bitsize
          || ll_unsignedp != lr_unsignedp || rl_unsignedp != rr_unsignedp


          || ll_bitpos - rl_bitpos != lr_bitpos - rr_bitpos)
        return 0;

      first_bit = ((lr_bitpos) < (rr_bitpos) ? (lr_bitpos) : (rr_bitpos));
      end_bit = ((lr_bitpos + lr_bitsize) > (rr_bitpos + rr_bitsize) ? (lr_bitpos + lr_bitsize) : (rr_bitpos + rr_bitsize));
      rnmode = get_best_mode (end_bit - first_bit, first_bit,
                              ((((lr_inner)->common.type))->type.align), word_mode,
                              volatilep);
      if (rnmode == VOIDmode)
        return 0;

      rnbitsize = (mode_bitsize[(int) (rnmode)]);
      rnbitpos = first_bit & ~ (rnbitsize - 1);
      rntype = type_for_size (rnbitsize, 1);
      xlr_bitpos = lr_bitpos - rnbitpos, xrr_bitpos = rr_bitpos - rnbitpos;

      if (0)
        {
          xlr_bitpos = rnbitsize - xlr_bitpos - lr_bitsize;
          xrr_bitpos = rnbitsize - xrr_bitpos - rr_bitsize;
        }

      lr_mask = const_binop (LSHIFT_EXPR, convert (rntype, lr_mask),
                             size_int_wide ((long long) (xlr_bitpos), SIZETYPE), 0);
      rr_mask = const_binop (LSHIFT_EXPR, convert (rntype, rr_mask),
                             size_int_wide ((long long) (xrr_bitpos), SIZETYPE), 0);






      ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask, 0);
      lr_mask = const_binop (BIT_IOR_EXPR, lr_mask, rr_mask, 0);
      if (lnbitsize == rnbitsize && xll_bitpos == xlr_bitpos)
        {
          lhs = make_bit_field_ref (ll_inner, lntype, lnbitsize, lnbitpos,
                                    ll_unsignedp || rl_unsignedp);
          if (! all_ones_mask_p (ll_mask, lnbitsize))
            lhs = build (BIT_AND_EXPR, lntype, lhs, ll_mask);

          rhs = make_bit_field_ref (lr_inner, rntype, rnbitsize, rnbitpos,
                                    lr_unsignedp || rr_unsignedp);
          if (! all_ones_mask_p (lr_mask, rnbitsize))
            rhs = build (BIT_AND_EXPR, rntype, rhs, lr_mask);

          return build (wanted_code, truth_type, lhs, rhs);
        }

      if ((ll_bitsize + ll_bitpos == rl_bitpos
           && lr_bitsize + lr_bitpos == rr_bitpos)
          || (ll_bitpos == rl_bitpos + rl_bitsize
              && lr_bitpos == rr_bitpos + rr_bitsize))
        {
          tree type;

          lhs = make_bit_field_ref (ll_inner, lntype, ll_bitsize + rl_bitsize,
                                    ((ll_bitpos) < (rl_bitpos) ? (ll_bitpos) : (rl_bitpos)), ll_unsignedp);
          rhs = make_bit_field_ref (lr_inner, rntype, lr_bitsize + rr_bitsize,
                                    ((lr_bitpos) < (rr_bitpos) ? (lr_bitpos) : (rr_bitpos)), lr_unsignedp);

          ll_mask = const_binop (RSHIFT_EXPR, ll_mask,
                                 size_int_wide ((long long) (((xll_bitpos) < (xrl_bitpos) ? (xll_bitpos) : (xrl_bitpos))), SIZETYPE), 0);
          lr_mask = const_binop (RSHIFT_EXPR, lr_mask,
                                 size_int_wide ((long long) (((xlr_bitpos) < (xrr_bitpos) ? (xlr_bitpos) : (xrr_bitpos))), SIZETYPE), 0);


          type = lntype;
          if (lntype != rntype)
            {
              if (lnbitsize > rnbitsize)
                {
                  lhs = convert (rntype, lhs);
                  ll_mask = convert (rntype, ll_mask);
                  type = rntype;
                }
              else if (lnbitsize < rnbitsize)
                {
                  rhs = convert (lntype, rhs);
                  lr_mask = convert (lntype, lr_mask);
                  type = lntype;
                }
            }

          if (! all_ones_mask_p (ll_mask, ll_bitsize + rl_bitsize))
            lhs = build (BIT_AND_EXPR, type, lhs, ll_mask);

          if (! all_ones_mask_p (lr_mask, lr_bitsize + rr_bitsize))
            rhs = build (BIT_AND_EXPR, type, rhs, lr_mask);

          return build (wanted_code, truth_type, lhs, rhs);
        }

      return 0;
    }





  result = const_binop (BIT_AND_EXPR, ll_mask, rl_mask, 0);
  if (! integer_zerop (result)
      && simple_cst_equal (const_binop (BIT_AND_EXPR, result, l_const, 0),
                           const_binop (BIT_AND_EXPR, result, r_const, 0)) != 1)
    {
      if (wanted_code == NE_EXPR)
        {
          warning ("`or' of unmatched not-equal tests is always 1");
          return convert (truth_type, global_trees[TI_INTEGER_ONE]);
        }
      else
        {
          warning ("`and' of mutually exclusive equal-tests is always 0");
          return convert (truth_type, global_trees[TI_INTEGER_ZERO]);
        }
    }





  result = make_bit_field_ref (ll_inner, lntype, lnbitsize, lnbitpos,
                               ll_unsignedp || rl_unsignedp);

  ll_mask = const_binop (BIT_IOR_EXPR, ll_mask, rl_mask, 0);
  if (! all_ones_mask_p (ll_mask, lnbitsize))
    result = build (BIT_AND_EXPR, lntype, result, ll_mask);

  return build (wanted_code, truth_type, result,
                const_binop (BIT_IOR_EXPR, l_const, r_const, 0));
}



static int
multiple_of_p (type, top, bottom)
     tree type;
     tree top;
     tree bottom;
{
  if (operand_equal_p (top, bottom, 0))
    return 1;

  if (((enum tree_code) (type)->common.code) != INTEGER_TYPE)
    return 0;

  switch (((enum tree_code) (top)->common.code))
    {
    case MULT_EXPR:
      return (multiple_of_p (type, ((top)->exp.operands[0]), bottom)
              || multiple_of_p (type, ((top)->exp.operands[1]), bottom));

    case PLUS_EXPR:
    case MINUS_EXPR:
      return (multiple_of_p (type, ((top)->exp.operands[0]), bottom)
              && multiple_of_p (type, ((top)->exp.operands[1]), bottom));

    case LSHIFT_EXPR:
      if (((enum tree_code) (((top)->exp.operands[1]))->common.code) == INTEGER_CST)
        {
          tree op1, t1;

          op1 = ((top)->exp.operands[1]);


          if (((((global_trees[TI_SIZE_ONE])->common.type))->type.precision)
              > (((op1)->int_cst.int_cst).low)
              && (((op1)->int_cst.int_cst).high) == 0
              && 0 != (t1 = convert (type,
                                     const_binop (LSHIFT_EXPR, global_trees[TI_SIZE_ONE],
                                                  op1, 0)))
              && ! ((t1)->common.public_flag))
            return multiple_of_p (type, t1, bottom);
        }
      return 0;

    case NOP_EXPR:

      if ((((enum tree_code) (((((top)->exp.operands[0]))->common.type))->common.code) != INTEGER_TYPE)
          || (((type)->type.precision)
              < ((((((top)->exp.operands[0]))->common.type))->type.precision)))
        return 0;



    case SAVE_EXPR:
      return multiple_of_p (type, ((top)->exp.operands[0]), bottom);

    case INTEGER_CST:
      if (((enum tree_code) (bottom)->common.code) != INTEGER_CST
          || (((type)->common.unsigned_flag)
              && (tree_int_cst_sgn (top) < 0
                  || tree_int_cst_sgn (bottom) < 0)))
        return 0;
      return integer_zerop (const_binop (TRUNC_MOD_EXPR,
                                         top, bottom, 0));

    default:
      return 0;
    }
}




tree
fold (expr)
     tree expr;
{
  tree t = expr;
  tree t1 = (tree) ((void *)0);
  tree tem;
  tree type = ((expr)->common.type);
  tree arg0 = (tree) ((void *)0), arg1 = (tree) ((void *)0);
  enum tree_code code = ((enum tree_code) (t)->common.code);
  int kind = tree_code_type[(int) (code)];
  int invert;
  int wins = 1;

  if (code == RTL_EXPR || (code == SAVE_EXPR && (*(rtx *) &(t)->exp.operands[2]) != 0))
    return t;
  if (kind == 'c')
    return t;

  switch (code)
    {
    case BIT_NOT_EXPR:
      if (wins)
        {
          t = build_int_2_wide ((unsigned long long) (~ (((arg0)->int_cst.int_cst).low)), (long long) (~ (((arg0)->int_cst.int_cst).high)));

          ((t)->common.type) = type;
          force_fit_type (t, 0);
          ((t)->common.public_flag) = ((arg0)->common.public_flag);
          ((t)->common.static_flag) = ((arg0)->common.static_flag);
        }
      else if (((enum tree_code) (arg0)->common.code) == BIT_NOT_EXPR)
        return ((arg0)->exp.operands[0]);
      return t;

    case PLUS_EXPR:

      if (((enum tree_code) (arg1)->common.code) == NEGATE_EXPR)
        return fold (build (MINUS_EXPR, type, arg0, ((arg1)->exp.operands[0])));

      if (((enum tree_code) (arg0)->common.code) == NEGATE_EXPR)
        return fold (build (MINUS_EXPR, type, arg1, ((arg0)->exp.operands[0])));
      else if (! (((enum tree_code) (type)->common.code) == REAL_TYPE || (((enum tree_code) (type)->common.code) == COMPLEX_TYPE && ((enum tree_code) (((type)->common.type))->common.code) == REAL_TYPE)))
        {
          if (((enum tree_code) (arg0)->common.code) == MULT_EXPR && ((enum tree_code) (arg1)->common.code) == MULT_EXPR)
            {
              tree arg00, arg01, arg10, arg11;
              tree alt0 = (tree) ((void *)0), alt1 = (tree) ((void *)0), same;
              arg00 = ((arg0)->exp.operands[0]);
              arg01 = ((arg0)->exp.operands[1]);
              arg10 = ((arg1)->exp.operands[0]);
              arg11 = ((arg1)->exp.operands[1]);
              same = (tree) ((void *)0);

              if (operand_equal_p (arg01, arg11, 0))
                same = arg01, alt0 = arg00, alt1 = arg10;
              else if (operand_equal_p (arg00, arg10, 0))
                same = arg00, alt0 = arg01, alt1 = arg11;
              else if (operand_equal_p (arg00, arg11, 0))
                same = arg00, alt0 = arg01, alt1 = arg10;
              else if (operand_equal_p (arg01, arg10, 0))
                same = arg01, alt0 = arg00, alt1 = arg11;
              else if (((enum tree_code) (arg01)->common.code) == INTEGER_CST
                       && ((enum tree_code) (arg11)->common.code) == INTEGER_CST
                       && (((arg01)->int_cst.int_cst).high) == 0
                       && (((arg11)->int_cst.int_cst).high) == 0)
                {
                  long long int01, int11, tmp;
                  int01 = (((arg01)->int_cst.int_cst).low);
                  int11 = (((arg11)->int_cst.int_cst).low);


                  if ((int01 >= 0 ? int01 : -int01)
                      < (int11 >= 0 ? int11 : -int11))
                    {
                      tmp = int01, int01 = int11, int11 = tmp;
                      alt0 = arg00, arg00 = arg10, arg10 = alt0;
                      alt0 = arg01, arg01 = arg11, arg11 = alt0;
                    }

                  if (exact_log2_wide ((unsigned long long) (int11)) > 0 && int01 % int11 == 0)
                    {
                      alt0 = fold (build (MULT_EXPR, type, arg00,
                                          build_int_2_wide ((unsigned long long) (int01 / int11), (long long) (0))));
                      alt1 = arg10;
                      same = arg11;
                    }
                }

              if (same)
                return fold (build (MULT_EXPR, type,
                                    fold (build (PLUS_EXPR, type, alt0, alt1)),
                                    same));
            }
        }

      else if ((1 != 1
                || flag_unsafe_math_optimizations)
               && real_zerop (arg1))
        return non_lvalue (convert (type, arg0));

      else if (((enum tree_code) (arg1)->common.code) == REAL_CST
               && ((ereal_cmp (((arg1)->real_cst.real_cst), dconst0) == 0) && (ereal_isneg (((arg1)->real_cst.real_cst)) != 0 )))
        return non_lvalue (convert (type, arg0));

     bit_rotate:	/* LABEL */
      {
        enum tree_code code0, code1;
        code0 = ((enum tree_code) (arg0)->common.code);
        code1 = ((enum tree_code) (arg1)->common.code);
        if (((code0 == RSHIFT_EXPR && code1 == LSHIFT_EXPR)
             || (code1 == RSHIFT_EXPR && code0 == LSHIFT_EXPR))
            && operand_equal_p (((arg0)->exp.operands[0]),
                                ((arg1)->exp.operands[0]), 0)
            && ((((((arg0)->exp.operands[0]))->common.type))->common.unsigned_flag))
          {
            tree tree01, tree11;
            enum tree_code code01, code11;

            tree01 = ((arg0)->exp.operands[1]);
            tree11 = ((arg1)->exp.operands[1]);
            code01 = ((enum tree_code) (tree01)->common.code);
            code11 = ((enum tree_code) (tree11)->common.code);
            if (code01 == INTEGER_CST
                && code11 == INTEGER_CST
                && (((tree01)->int_cst.int_cst).high) == 0
                && (((tree11)->int_cst.int_cst).high) == 0
                && (((((tree01)->int_cst.int_cst).low) + (((tree11)->int_cst.int_cst).low))
                    == ((((((arg0)->exp.operands[0]))->common.type))->type.precision)))
              return build (LROTATE_EXPR, type, ((arg0)->exp.operands[0]),
                            code0 == LSHIFT_EXPR ? tree01 : tree11);
            else if (code11 == MINUS_EXPR)
              {
                tree tree110, tree111;
                tree110 = ((tree11)->exp.operands[0]);
                tree111 = ((tree11)->exp.operands[1]);
                if (((enum tree_code) (tree110)->common.code) == INTEGER_CST
                    && 0 == compare_tree_int (tree110,
                                              ((((((arg0)->exp.operands[0]))->common.type))->type.precision))


                    && operand_equal_p (tree01, tree111, 0))
                  return build ((code0 == LSHIFT_EXPR
                                 ? LROTATE_EXPR
                                 : RROTATE_EXPR),
                                type, ((arg0)->exp.operands[0]), tree01);
              }
            else if (code01 == MINUS_EXPR)
              {
                tree tree010, tree011;
                tree010 = ((tree01)->exp.operands[0]);
                tree011 = ((tree01)->exp.operands[1]);
                if (((enum tree_code) (tree010)->common.code) == INTEGER_CST
                    && 0 == compare_tree_int (tree010,
                                              ((((((arg0)->exp.operands[0]))->common.type))->type.precision))


                    && operand_equal_p (tree11, tree011, 0))
                  return build ((code0 != LSHIFT_EXPR
                                 ? LROTATE_EXPR
                                 : RROTATE_EXPR),
                                type, ((arg0)->exp.operands[0]), tree11);
              }
          }
      }

    associate:		/* LABEL */
      if (! wins
          && (! (((enum tree_code) (type)->common.code) == REAL_TYPE || (((enum tree_code) (type)->common.code) == COMPLEX_TYPE && ((enum tree_code) (((type)->common.type))->common.code) == REAL_TYPE))
              || (flag_unsafe_math_optimizations && code == MULT_EXPR)))
        {
          tree var0, con0, lit0, minus_lit0;
          tree var1, con1, lit1, minus_lit1;
          var0 = split_tree (arg0, code, &con0, &lit0, &minus_lit0, 0);
          var1 = split_tree (arg1, code, &con1, &lit1, &minus_lit1,
                             code == MINUS_EXPR);

          if (2 < ((var0 != 0) + (var1 != 0)
                   + (con0 != 0) + (con1 != 0)
                   + (lit0 != 0) + (lit1 != 0)
                   + (minus_lit0 != 0) + (minus_lit1 != 0)))
            {

              if (code == MINUS_EXPR)
                code = PLUS_EXPR;

              var0 = associate_trees (var0, var1, code, type);
              con0 = associate_trees (con0, con1, code, type);
              lit0 = associate_trees (lit0, lit1, code, type);
              minus_lit0 = associate_trees (minus_lit0, minus_lit1, code, type);
              if (minus_lit0 && lit0)
                {
                  if (tree_int_cst_lt (lit0, minus_lit0))
                    {
                      minus_lit0 = associate_trees (minus_lit0, lit0,
                                                    MINUS_EXPR, type);
                      lit0 = 0;
                    }
                  else
                    {
                      lit0 = associate_trees (lit0, minus_lit0,
                                              MINUS_EXPR, type);
                      minus_lit0 = 0;
                    }
                }
              if (minus_lit0)
                {
                  if (con0 == 0)
                    return convert (type, associate_trees (var0, minus_lit0,
                                                           MINUS_EXPR, type));
                  else
                    {
                      con0 = associate_trees (con0, minus_lit0,
                                              MINUS_EXPR, type);
                      return convert (type, associate_trees (var0, con0,
                                                             PLUS_EXPR, type));
                    }
                }

              con0 = associate_trees (con0, lit0, code, type);
              return convert (type, associate_trees (var0, con0, code, type));
            }
        }

    binary:		/* LABEL */
      if (wins)
        t1 = const_binop (code, arg0, arg1, 0);
      if (t1 != (tree) ((void *)0))
        {
          if (((t1)->common.type) != ((t)->common.type))
            t1 = convert (((t)->common.type), t1);

          return t1;
        }
      return t;
    case TRUTH_NOT_EXPR:
      tem = invert_truthvalue (arg0);
      if (((enum tree_code) (tem)->common.code) == TRUTH_NOT_EXPR)
        return t;
      return convert (type, tem);
    case TRUTH_ANDIF_EXPR:
      if (integer_zerop (arg0))
        return convert (type, arg0);
    case TRUTH_AND_EXPR:
      if (((enum tree_code) (arg0)->common.code) == INTEGER_CST && ! integer_zerop (arg0))
        return non_lvalue (convert (type, arg1));
      if (((enum tree_code) (arg1)->common.code) == INTEGER_CST && ! integer_zerop (arg1)
          && (code != TRUTH_ANDIF_EXPR || ! ((arg0)->common.side_effects_flag)))
        return non_lvalue (convert (type, arg0));
      if (integer_zerop (arg1))
        return omit_one_operand (type, arg1, arg0);
      if (integer_zerop (arg0))
        return omit_one_operand (type, arg0, arg1);
    truth_andor:
      if (!optimize)
        return t;
      if (((enum tree_code) (arg0)->common.code) == ((enum tree_code) (arg1)->common.code)
          && (((enum tree_code) (arg0)->common.code) == TRUTH_ANDIF_EXPR
              || ((enum tree_code) (arg0)->common.code) == TRUTH_ORIF_EXPR
              || ((enum tree_code) (arg0)->common.code) == TRUTH_AND_EXPR
              || ((enum tree_code) (arg0)->common.code) == TRUTH_OR_EXPR)
          && ! ((((arg0)->exp.operands[1]))->common.side_effects_flag))
        {
        }
      if (((enum tree_code) (arg0)->common.code) == code
          && 0 != (tem = fold_truthop (code, type,
                                       ((arg0)->exp.operands[1]), arg1))) ;
      return t;

    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      if ((((enum tree_code) (((arg0)->common.type))->common.code) == REAL_TYPE || (((enum tree_code) (((arg0)->common.type))->common.code) == COMPLEX_TYPE && ((enum tree_code) (((((arg0)->common.type))->common.type))->common.code) == REAL_TYPE)))
        {

          if (((enum tree_code) (arg0)->common.code) == NEGATE_EXPR
              && ((enum tree_code) (arg1)->common.code) == NEGATE_EXPR)
            return fold (build (code, type, ((arg1)->exp.operands[0]),
                                ((arg0)->exp.operands[0])));

          if (((enum tree_code) (arg0)->common.code) == NEGATE_EXPR && ((enum tree_code) (arg1)->common.code) == REAL_CST)
            return
              fold (build
                    (swap_tree_comparison (code), type,
                     ((arg0)->exp.operands[0]),
                     build_real (((arg1)->common.type),
                                 ereal_negate (((arg1)->real_cst.real_cst)))));


          if (((enum tree_code) (arg1)->common.code) == REAL_CST
              && ((ereal_cmp (((arg1)->real_cst.real_cst), dconst0) == 0) && (ereal_isneg (((arg1)->real_cst.real_cst)) != 0 )))
            return fold (build (code, type, arg0,
                                build_real (((arg1)->common.type), dconst0)));
        }


      if (((enum tree_code) (arg0)->common.code) == INTEGER_CST
          && ((enum tree_code) (arg1)->common.code) != INTEGER_CST)
        {
          ((t)->exp.operands[0]) = arg1;
          ((t)->exp.operands[1]) = arg0;
          arg0 = ((t)->exp.operands[0]);
          arg1 = ((t)->exp.operands[1]);
          code = swap_tree_comparison (code);
          ((t)->common.code = (enum tree_code) (code));
        }
      {
        tree constop = 0, varop = (tree) ((void *)0);
        int constopnum = -1;

        if (((arg1)->common.constant_flag))
          constopnum = 1, constop = arg1, varop = arg0;
        if (((arg0)->common.constant_flag))
          constopnum = 0, constop = arg0, varop = arg1;

        if (constop && ((enum tree_code) (varop)->common.code) == POSTINCREMENT_EXPR)
          {
            if ((((enum tree_code) (((varop)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((varop)->common.type))->common.code) == REFERENCE_TYPE)
                || (! (((enum tree_code) (((varop)->common.type))->common.code) == REAL_TYPE || (((enum tree_code) (((varop)->common.type))->common.code) == COMPLEX_TYPE && ((enum tree_code) (((((varop)->common.type))->common.type))->common.code) == REAL_TYPE))
                    && (code == EQ_EXPR || code == NE_EXPR)))
              {
                tree newconst
                  = fold (build (PLUS_EXPR, ((varop)->common.type),
                                 constop, ((varop)->exp.operands[1])));

                varop = build (PREINCREMENT_EXPR, ((varop)->common.type),
                               ((varop)->exp.operands[0]),
                               ((varop)->exp.operands[1]));

                if (((enum tree_code) (((varop)->exp.operands[0]))->common.code) == COMPONENT_REF
                    && ((((((varop)->exp.operands[0]))->exp.operands[1]))->decl.bit_field_flag))

                  {
                    int size
                      = (((((((((varop)->exp.operands[0]))->exp.operands[1]))->decl.size))->int_cst.int_cst).low);

                    tree mask, unsigned_type;
                    unsigned int precision;
                    tree folded_compare;

                    if (constopnum == 0)
                      folded_compare = fold (build (code, type, constop,
                                                    ((varop)->exp.operands[0])));
                    else
                      folded_compare = fold (build (code, type,
                                                    ((varop)->exp.operands[0]),
                                                    constop));
                    if (integer_zerop (folded_compare)
                        || integer_onep (folded_compare))
                      return omit_one_operand (type, folded_compare, varop);

                    unsigned_type = type_for_size (size, 1);
                    precision = ((unsigned_type)->type.precision);
                    mask = build_int_2_wide ((unsigned long long) (~0), (long long) (~0));
                    ((mask)->common.type) = unsigned_type;
                    force_fit_type (mask, 0);
                    mask = const_binop (RSHIFT_EXPR, mask,
                                        size_int_wide ((long long) (precision - size), SIZETYPE), 0);
                    newconst = fold (build (BIT_AND_EXPR,
                                            ((varop)->common.type), newconst,
                                            convert (((varop)->common.type),
                                                     mask)));
                  }

                t = build (code, type,
                           (constopnum == 0) ? newconst : varop,
                           (constopnum == 1) ? newconst : varop);
                return t;
              }
          }
        else if (constop && ((enum tree_code) (varop)->common.code) == POSTDECREMENT_EXPR)
          {
            if ((((enum tree_code) (((varop)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((varop)->common.type))->common.code) == REFERENCE_TYPE)
                || (! (((enum tree_code) (((varop)->common.type))->common.code) == REAL_TYPE || (((enum tree_code) (((varop)->common.type))->common.code) == COMPLEX_TYPE && ((enum tree_code) (((((varop)->common.type))->common.type))->common.code) == REAL_TYPE))
                    && (code == EQ_EXPR || code == NE_EXPR)))
              {
                tree newconst
                  = fold (build (MINUS_EXPR, ((varop)->common.type),
                                 constop, ((varop)->exp.operands[1])));

                varop = build (PREDECREMENT_EXPR, ((varop)->common.type),
                               ((varop)->exp.operands[0]),
                               ((varop)->exp.operands[1]));

                if (((enum tree_code) (((varop)->exp.operands[0]))->common.code) == COMPONENT_REF
                    && ((((((varop)->exp.operands[0]))->exp.operands[1]))->decl.bit_field_flag))

                  {
                    int size
                      = (((((((((varop)->exp.operands[0]))->exp.operands[1]))->decl.size))->int_cst.int_cst).low);


                    tree mask, unsigned_type;
                    unsigned int precision;
                    tree folded_compare;

                    if (constopnum == 0)
                      folded_compare = fold (build (code, type, constop,
                                                    ((varop)->exp.operands[0])));
                    else
                      folded_compare = fold (build (code, type,
                                                    ((varop)->exp.operands[0]),
                                                    constop));
                    if (integer_zerop (folded_compare)
                        || integer_onep (folded_compare))
                      return omit_one_operand (type, folded_compare, varop);

                    unsigned_type = type_for_size (size, 1);
                    precision = ((unsigned_type)->type.precision);
                    mask = build_int_2_wide ((unsigned long long) (~0), (long long) (~0));
                    ((mask)->common.type) = ((varop)->common.type);
                    force_fit_type (mask, 0);
                    mask = const_binop (RSHIFT_EXPR, mask,
                                        size_int_wide ((long long) (precision - size), SIZETYPE), 0);
                    newconst = fold (build (BIT_AND_EXPR,
                                            ((varop)->common.type), newconst,
                                            convert (((varop)->common.type),
                                                     mask)));
                  }

                t = build (code, type,
                           (constopnum == 0) ? newconst : varop,
                           (constopnum == 1) ? newconst : varop);
                return t;
              }
          }
      }
      {
        int width = (mode_bitsize[(int) (((((arg1)->common.type))->type.mode))]);

        if (((enum tree_code) (arg1)->common.code) == INTEGER_CST
            && ! ((arg1)->common.static_flag)
            && width <= (8 * 8)
            && ((((enum tree_code) (((arg1)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == CHAR_TYPE)
                || (((enum tree_code) (((arg1)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == REFERENCE_TYPE)))
          {
          }
      }

      if (((enum tree_code) (arg1)->common.code) == INTEGER_CST
          && ((enum tree_code) (arg0)->common.code) != INTEGER_CST
          && tree_int_cst_sgn (arg1) > 0)
        {
          switch (((enum tree_code) (t)->common.code))
            {
            case GE_EXPR:
              code = GT_EXPR;
              arg1 = const_binop (MINUS_EXPR, arg1, global_trees[TI_INTEGER_ONE], 0);
              t = build (code, type, ((t)->exp.operands[0]), arg1);
              break;

            case LT_EXPR:
              code = LE_EXPR;
              arg1 = const_binop (MINUS_EXPR, arg1, global_trees[TI_INTEGER_ONE], 0);
              t = build (code, type, ((t)->exp.operands[0]), arg1);
              break;

            default:
              break;
            }
        }

      if (integer_zerop (arg1)
          && ((((enum tree_code) (((arg1)->common.type))->common.code) == INTEGER_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == ENUMERAL_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == BOOLEAN_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == CHAR_TYPE)
              || (((enum tree_code) (((arg1)->common.type))->common.code) == POINTER_TYPE || ((enum tree_code) (((arg1)->common.type))->common.code) == REFERENCE_TYPE))
          && ((((arg1)->common.type))->common.unsigned_flag))
        {
          switch (((enum tree_code) (t)->common.code))
            {
            case GT_EXPR:
              code = NE_EXPR;
              ((t)->common.code = (enum tree_code) (NE_EXPR));
              break;
            case LE_EXPR:
              code = EQ_EXPR;
              ((t)->common.code = (enum tree_code) (EQ_EXPR));
              break;
            case GE_EXPR:
              return omit_one_operand (type,
                                       convert (type, global_trees[TI_INTEGER_ONE]),
                                       arg0);
            case LT_EXPR:
              return omit_one_operand (type,
                                       convert (type, global_trees[TI_INTEGER_ZERO]),
                                       arg0);
            default:
              break;
            }
        }

      if ((code == EQ_EXPR || code == NE_EXPR)
          && ((enum tree_code) (arg1)->common.code) == INTEGER_CST
          && (((enum tree_code) (arg0)->common.code) == PLUS_EXPR
              || ((enum tree_code) (arg0)->common.code) == MINUS_EXPR)
          && ((enum tree_code) (((arg0)->exp.operands[1]))->common.code) == INTEGER_CST
          && 0 != (tem = const_binop (((enum tree_code) (arg0)->common.code) == PLUS_EXPR
                                      ? MINUS_EXPR : PLUS_EXPR,
                                      arg1, ((arg0)->exp.operands[1]), 0))
          && ! ((tem)->common.static_flag))
        return fold (build (code, type, ((arg0)->exp.operands[0]), tem));

      else if ((code == EQ_EXPR || code == NE_EXPR)
               && ((enum tree_code) (arg0)->common.code) == NEGATE_EXPR
               && ((enum tree_code) (arg1)->common.code) == INTEGER_CST
               && 0 != (tem = negate_expr (arg1))
               && ((enum tree_code) (tem)->common.code) == INTEGER_CST
               && ! ((tem)->common.static_flag))
        return fold (build (code, type, ((arg0)->exp.operands[0]), tem));

      else if ((code == NE_EXPR || code == EQ_EXPR)
               && integer_zerop (arg1) && ((enum tree_code) (arg0)->common.code) == MINUS_EXPR)
        return fold (build (code, type,
                            ((arg0)->exp.operands[0]), ((arg0)->exp.operands[1])));

      else if (((enum tree_code) (((arg0)->common.type))->common.code) == INTEGER_TYPE
               && ((enum tree_code) (arg0)->common.code) == NOP_EXPR
               && (tem = get_unwidened (arg0, (tree) ((void *)0))) != arg0
               && (t1 = get_unwidened (arg1, ((tem)->common.type))) != 0
               && (((t1)->common.type) == ((tem)->common.type)
                   || (((enum tree_code) (t1)->common.code) == INTEGER_CST
                       && int_fits_type_p (t1, ((tem)->common.type)))))
        return fold (build (code, type, tem, convert (((tem)->common.type), t1)));

      else if (((enum tree_code) (arg1)->common.code) == INTEGER_CST
               && (((enum tree_code) (arg0)->common.code) == MIN_EXPR
                   || ((enum tree_code) (arg0)->common.code) == MAX_EXPR)
               && ((enum tree_code) (((arg0)->exp.operands[1]))->common.code) == INTEGER_CST)
        return optimize_minmax_comparison (t);

      else if (code == LE_EXPR && ((enum tree_code) (arg1)->common.code) == INTEGER_CST
               && ((enum tree_code) (arg0)->common.code) == ABS_EXPR
               && ! ((arg0)->common.side_effects_flag)
               && (0 != (tem = negate_expr (arg1)))
               && ((enum tree_code) (tem)->common.code) == INTEGER_CST
               && ! ((tem)->common.static_flag))
        return fold (build (TRUTH_ANDIF_EXPR, type,
                            build (GE_EXPR, type, ((arg0)->exp.operands[0]), tem),
                            build (LE_EXPR, type,
                                   ((arg0)->exp.operands[0]), arg1)));

      if (integer_zerop (arg1) && (code == EQ_EXPR || code == NE_EXPR)
          && ((enum tree_code) (arg0)->common.code) == BIT_AND_EXPR)
        {
          if (((enum tree_code) (((arg0)->exp.operands[0]))->common.code) == LSHIFT_EXPR
              && integer_onep (((((arg0)->exp.operands[0]))->exp.operands[0])))
            return
              fold (build (code, type,
                           build (BIT_AND_EXPR, ((arg0)->common.type),
                                  build (RSHIFT_EXPR,
                                         ((((arg0)->exp.operands[0]))->common.type),
                                         ((arg0)->exp.operands[1]),
                                         ((((arg0)->exp.operands[0]))->exp.operands[1])),
                                  convert (((arg0)->common.type),
                                           global_trees[TI_INTEGER_ONE])),
                           arg1));
          else if (((enum tree_code) (((arg0)->exp.operands[1]))->common.code) == LSHIFT_EXPR
                   && integer_onep (((((arg0)->exp.operands[1]))->exp.operands[0])))
            return
              fold (build (code, type,
                           build (BIT_AND_EXPR, ((arg0)->common.type),
                                  build (RSHIFT_EXPR,
                                         ((((arg0)->exp.operands[1]))->common.type),
                                         ((arg0)->exp.operands[0]),
                                         ((((arg0)->exp.operands[1]))->exp.operands[1])),
                                  convert (((arg0)->common.type),
                                           global_trees[TI_INTEGER_ONE])),
                           arg1));
        }

      if ((code == NE_EXPR || code == EQ_EXPR)
          && integer_zerop (arg1)
          && ! ((((arg0)->common.type))->common.unsigned_flag)
          && (((enum tree_code) (arg0)->common.code) == TRUNC_MOD_EXPR
              || ((enum tree_code) (arg0)->common.code) == CEIL_MOD_EXPR
              || ((enum tree_code) (arg0)->common.code) == FLOOR_MOD_EXPR
              || ((enum tree_code) (arg0)->common.code) == ROUND_MOD_EXPR)
          && integer_pow2p (((arg0)->exp.operands[1])))
        {
          tree newtype = unsigned_type (((arg0)->common.type));
          tree newmod = build (((enum tree_code) (arg0)->common.code), newtype,
                               convert (newtype, ((arg0)->exp.operands[0])),
                               convert (newtype, ((arg0)->exp.operands[1])));

          return build (code, type, newmod, convert (newtype, arg1));
        }

      if (code == NE_EXPR && integer_zerop (arg1)
          && ((enum tree_code) (arg0)->common.code) == BIT_AND_EXPR
          && integer_onep (((arg0)->exp.operands[1])))
        return convert (type, arg0);

      if ((code == EQ_EXPR || code == NE_EXPR)
          && ((enum tree_code) (arg0)->common.code) == BIT_AND_EXPR
          && integer_pow2p (((arg0)->exp.operands[1]))
          && operand_equal_p (((arg0)->exp.operands[1]), arg1, 0))
        return build (code == EQ_EXPR ? NE_EXPR : EQ_EXPR, type,
                      arg0, global_trees[TI_INTEGER_ZERO]);

      if ((code == LT_EXPR || code == GE_EXPR)
          && ((((arg0)->common.type))->common.unsigned_flag)
          && ((enum tree_code) (arg1)->common.code) == LSHIFT_EXPR
          && integer_onep (((arg1)->exp.operands[0])))
        return build (code == LT_EXPR ? EQ_EXPR : NE_EXPR, type,
                      build (RSHIFT_EXPR, ((arg0)->common.type), arg0,
                             ((arg1)->exp.operands[1])),
                      convert (((arg0)->common.type), global_trees[TI_INTEGER_ZERO]));

      else if ((code == LT_EXPR || code == GE_EXPR)
               && ((((arg0)->common.type))->common.unsigned_flag)
               && (((enum tree_code) (arg1)->common.code) == NOP_EXPR
                   || ((enum tree_code) (arg1)->common.code) == CONVERT_EXPR)
               && ((enum tree_code) (((arg1)->exp.operands[0]))->common.code) == LSHIFT_EXPR
               && integer_onep (((((arg1)->exp.operands[0]))->exp.operands[0])))
        return
          build (code == LT_EXPR ? EQ_EXPR : NE_EXPR, type,
                 convert (((arg0)->common.type),
                          build (RSHIFT_EXPR, ((arg0)->common.type), arg0,
                                 ((((arg1)->exp.operands[0]))->exp.operands[1]))),
                 convert (((arg0)->common.type), global_trees[TI_INTEGER_ZERO]));

      if ((((enum tree_code) (arg0)->common.code) == COMPONENT_REF
           || ((enum tree_code) (arg0)->common.code) == BIT_FIELD_REF)
          && (code == EQ_EXPR || code == NE_EXPR)


          && (optimize || ((enum tree_code) (arg1)->common.code) == INTEGER_CST))
        {
          t1 = optimize_bit_field_compare (code, type, arg0, arg1);
          return t1 ? t1 : t;
        }

      if ((code == EQ_EXPR || code == NE_EXPR)
          && ((enum tree_code) (((arg0)->common.type))->common.code) == COMPLEX_TYPE
          && (((enum tree_code) (arg0)->common.code) == COMPLEX_EXPR
              || ((enum tree_code) (arg1)->common.code) == COMPLEX_EXPR
              || ((enum tree_code) (arg0)->common.code) == COMPLEX_CST
              || ((enum tree_code) (arg1)->common.code) == COMPLEX_CST))
        {
          tree subtype = ((((arg0)->common.type))->common.type);
          tree real0, imag0, real1, imag1;

          arg0 = save_expr (arg0);
          arg1 = save_expr (arg1);
          real0 = fold (build1 (REALPART_EXPR, subtype, arg0));
          imag0 = fold (build1 (IMAGPART_EXPR, subtype, arg0));
          real1 = fold (build1 (REALPART_EXPR, subtype, arg1));
          imag1 = fold (build1 (IMAGPART_EXPR, subtype, arg1));

          return fold (build ((code == EQ_EXPR ? TRUTH_ANDIF_EXPR
                               : TRUTH_ORIF_EXPR),
                              type,
                              fold (build (code, type, real0, real1)),
                              fold (build (code, type, imag0, imag1))));
        }
      if ((code == EQ_EXPR || code == NE_EXPR)
          && integer_zerop (arg1)
          && ((enum tree_code) (arg0)->common.code) == CALL_EXPR
          && ((enum tree_code) (((arg0)->exp.operands[0]))->common.code) == ADDR_EXPR)
        {
          tree fndecl = ((((arg0)->exp.operands[0]))->exp.operands[0]);
          tree arglist;

          if (((enum tree_code) (fndecl)->common.code) == FUNCTION_DECL
              && (((fndecl)->decl.built_in_class) != NOT_BUILT_IN)
              && ((fndecl)->decl.built_in_class) != BUILT_IN_MD
              && ((fndecl)->decl.u1.f) == BUILT_IN_STRLEN
              && (arglist = ((arg0)->exp.operands[1]))
              && ((enum tree_code) (((((arglist)->list.value))->common.type))->common.code) == POINTER_TYPE
              && ! ((arglist)->common.chain))
            return fold (build (code, type,
                                build1 (INDIRECT_REF, integer_types[itk_char],
                                        ((arglist)->list.value)),
                                global_trees[TI_INTEGER_ZERO]));
        }

      if (code == LE_EXPR || code == GT_EXPR)
        {
          tem = arg0, arg0 = arg1, arg1 = tem;
          code = swap_tree_comparison (code);
        }




      t1 = (tree) ((void *)0);
      invert = 0;
      if (code == NE_EXPR || code == GE_EXPR)
        {
          invert = 1;
          code = invert_tree_comparison (code);
        }



      if (((enum tree_code) (arg0)->common.code) == INTEGER_CST && ((enum tree_code) (arg1)->common.code) == INTEGER_CST)
        {
          if (code == EQ_EXPR)
            t1 = build_int_2_wide ((unsigned long long) (tree_int_cst_equal (arg0, arg1)), (long long) (0));
          else
            t1 = build_int_2_wide ((unsigned long long) ((((((arg0)->common.type))->common.unsigned_flag) ? (((unsigned long long) (((arg0)->int_cst.int_cst).high) < (unsigned long long) (((arg1)->int_cst.int_cst).high)) || (((unsigned long long) (((arg0)->int_cst.int_cst).high) == (unsigned long long) (((arg1)->int_cst.int_cst).high)) && (((arg0)->int_cst.int_cst).low) < (((arg1)->int_cst.int_cst).low))) : ((((arg0)->int_cst.int_cst).high) < (((arg1)->int_cst.int_cst).high) || ((((arg0)->int_cst.int_cst).high) == (((arg1)->int_cst.int_cst).high) && (((arg0)->int_cst.int_cst).low) < (((arg1)->int_cst.int_cst).low))))), (long long) (0));



        }

      else if (((enum tree_code) (arg0)->common.code) == REAL_CST && ((enum tree_code) (arg1)->common.code) == REAL_CST)
        {
          if ((target_isnan (((arg0)->real_cst.real_cst)))
              || (target_isnan (((arg1)->real_cst.real_cst))))
            t1 = build_int_2_wide ((unsigned long long) (invert && code == LT_EXPR), (long long) (0));
          else if (code == EQ_EXPR)
            t1 = build_int_2_wide ((unsigned long long) ((ereal_cmp ((((arg0)->real_cst.real_cst)), (((arg1)->real_cst.real_cst))) == 0)), (long long) (0));
          else
            t1 = build_int_2_wide ((unsigned long long) ((ereal_cmp ((((arg0)->real_cst.real_cst)), (((arg1)->real_cst.real_cst))) == -1)), (long long) (0));
        }

      if (t1 == (tree) ((void *)0))
        return t;

      if (invert)
        (((t1)->int_cst.int_cst).low) ^= 1;

      ((t1)->common.type) = type;
      if (((enum tree_code) (type)->common.code) == BOOLEAN_TYPE)
        return truthvalue_conversion (t1);
      return t1;

    }
}
