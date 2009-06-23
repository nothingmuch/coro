/* list the interpreter variables that need to be saved/restored */
/* mostly copied from thrdvar.h */

VAR(defoutgv,      GV *)           /* default FH for output */
VAR(stack_sp,      SV **)          /* the main stack */
#ifdef OP_IN_REGISTER
VAR(opsave,        OP *)           /* probably not necessary */
#else
VAR(op,            OP *)           /* currently executing op */
#endif
VAR(curpad,        SV **)          /* active pad (lexicals+tmps) */

VAR(stack_base,    SV **)
VAR(stack_max,     SV **)

VAR(scopestack,    I32 *)          /* scopes we've ENTERed */
VAR(scopestack_ix, I32)
VAR(scopestack_max,I32)

VAR(savestack,     ANY *)          /* items that need to be restored
                                      when LEAVEing scopes we've ENTERed */
VAR(savestack_ix,  I32)
VAR(savestack_max, I32)

VAR(tmps_stack,    SV **)          /* mortals we've made */
VAR(tmps_ix,       I32)
VAR(tmps_floor,    I32)
VAR(tmps_max,      I32)

VAR(markstack,     I32 *)          /* stack_sp locations we're remembering */
VAR(markstack_ptr, I32 *)
VAR(markstack_max, I32 *)

#if !PERL_VERSION_ATLEAST (5,9,0)
VAR(retstack,      OP **)          /* OPs we have postponed executing */
VAR(retstack_ix,   I32)
VAR(retstack_max,  I32)
#endif

VAR(tainted,       bool)           /* using variables controlled by $< */
VAR(curpm,         PMOP *)         /* what to do \ interps in REs from */
VAR(rs,            SV *)           /* input record separator $/ */
VAR(curcop,        COP *)

VAR(in_eval,       int)            /* trap "fatal" errors? */
VAR(localizing,    int)            /* are we processing a local() list? */

VAR(curstack,      AV *)           /* THE STACK */
VAR(curstackinfo,  PERL_SI *)      /* current stack + context */

VAR(sortcop,       OP *)           /* user defined sort routine */
VAR(sortstash,     HV *)           /* which is in some package or other */
#if !PERL_VERSION_ATLEAST (5,9,0)
VAR(sortcxix,      I32)            /* from pp_ctl.c */
#endif

VAR(comppad,       AV *)           /* storage for lexically scoped temporaries */
VAR(comppad_name,  AV *)           /* variable names for "my" variables */
VAR(comppad_name_fill,     I32)    /* last "introduced" variable offset */
VAR(comppad_name_floor,    I32)    /* start of vars in innermost block */

/* compcv is intrpvar, but seems to be thread-specific to me */
/* but, well, I thoroughly misunderstand what thrdvar and intrpvar is. still. */
VAR(compcv,        CV *)           /* currently compiling subroutine */

VAR(diehook,       SV *)
VAR(warnhook,      SV *)
VAR(runops,        runops_proc_t)  /* for tracing support */

#if PERL_VERSION_ATLEAST (5,10,0)
VAR(parser,        yy_parser *)
#endif

VAR(hints,         U32)            /* pragma-tic compile-time flags */

