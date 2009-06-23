#include "libcoro/coro.c"

#define PERL_NO_GET_CONTEXT
#define PERL_EXT

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "perliol.h"

#include "patchlevel.h"

#include <stdio.h>
#include <errno.h>
#include <assert.h>

#ifdef WIN32
# undef setjmp
# undef longjmp
# undef _exit
# define setjmp _setjmp /* deep magic */
#else
# include <inttypes.h> /* most portable stdint.h */
#endif

#ifdef HAVE_MMAP
# include <unistd.h>
# include <sys/mman.h>
# ifndef MAP_ANONYMOUS
#  ifdef MAP_ANON
#   define MAP_ANONYMOUS MAP_ANON
#  else
#   undef HAVE_MMAP
#  endif
# endif
# include <limits.h>
# ifndef PAGESIZE
#  define PAGESIZE pagesize
#  define BOOT_PAGESIZE pagesize = sysconf (_SC_PAGESIZE)
static long pagesize;
# else
#  define BOOT_PAGESIZE (void)0
# endif
#else
# define PAGESIZE 0
# define BOOT_PAGESIZE (void)0
#endif

#if CORO_USE_VALGRIND
# include <valgrind/valgrind.h>
#endif

/* the maximum number of idle cctx that will be pooled */
static int cctx_max_idle = 4;

#define PERL_VERSION_ATLEAST(a,b,c)				\
  (PERL_REVISION > (a)						\
   || (PERL_REVISION == (a)					\
       && (PERL_VERSION > (b)					\
           || (PERL_VERSION == (b) && PERL_SUBVERSION >= (c)))))

#if !PERL_VERSION_ATLEAST (5,6,0)
# ifndef PL_ppaddr
#  define PL_ppaddr ppaddr
# endif
# ifndef call_sv
#  define call_sv perl_call_sv
# endif
# ifndef get_sv
#  define get_sv perl_get_sv
# endif
# ifndef get_cv
#  define get_cv perl_get_cv
# endif
# ifndef IS_PADGV
#  define IS_PADGV(v) 0
# endif
# ifndef IS_PADCONST
#  define IS_PADCONST(v) 0
# endif
#endif

/* 5.11 */
#ifndef CxHASARGS
# define CxHASARGS(cx) (cx)->blk_sub.hasargs
#endif

/* 5.10.0 */
#ifndef SvREFCNT_inc_NN
# define SvREFCNT_inc_NN(sv) SvREFCNT_inc (sv)
#endif

/* 5.8.8 */
#ifndef GV_NOTQUAL
# define GV_NOTQUAL 0
#endif
#ifndef newSV
# define newSV(l) NEWSV(0,l)
#endif
#ifndef CvISXSUB_on
# define CvISXSUB_on(cv) (void)cv
#endif
#ifndef CvISXSUB
# define CvISXSUB(cv) (CvXSUB (cv) ? TRUE : FALSE)
#endif
#ifndef Newx
# define Newx(ptr,nitems,type) New (0,ptr,nitems,type)
#endif

/* 5.8.7 */
#ifndef SvRV_set
# define SvRV_set(s,v) SvRV(s) = (v)
#endif

#if !__i386 && !__x86_64 && !__powerpc && !__m68k && !__alpha && !__mips && !__sparc64
# undef CORO_STACKGUARD
#endif

#ifndef CORO_STACKGUARD
# define CORO_STACKGUARD 0
#endif

/* prefer perl internal functions over our own? */
#ifndef CORO_PREFER_PERL_FUNCTIONS
# define CORO_PREFER_PERL_FUNCTIONS 0
#endif

/* The next macros try to return the current stack pointer, in an as
 * portable way as possible. */
#if __GNUC__ >= 4
# define dSTACKLEVEL int stacklevel_dummy
# define STACKLEVEL __builtin_frame_address (0)
#else
# define dSTACKLEVEL volatile void *stacklevel
# define STACKLEVEL ((void *)&stacklevel)
#endif

#define IN_DESTRUCT PL_dirty

#if __GNUC__ >= 3
# define attribute(x) __attribute__(x)
# define expect(expr,value) __builtin_expect ((expr), (value))
# define INLINE static inline
#else
# define attribute(x)
# define expect(expr,value) (expr)
# define INLINE static
#endif

#define expect_false(expr) expect ((expr) != 0, 0)
#define expect_true(expr)  expect ((expr) != 0, 1)

#define NOINLINE attribute ((noinline))

#include "CoroAPI.h"
#define GCoroAPI (&coroapi) /* very sneaky */

#ifdef USE_ITHREADS
# if CORO_PTHREAD
static void *coro_thx;
# endif
#endif

static double (*nvtime)(); /* so why doesn't it take void? */

/* we hijack an hopefully unused CV flag for our purposes */
#define CVf_SLF 0x4000
static OP *pp_slf (pTHX);

static U32 cctx_gen;
static size_t cctx_stacksize = CORO_STACKSIZE;
static struct CoroAPI coroapi;
static AV *main_mainstack; /* used to differentiate between $main and others */
static JMPENV *main_top_env;
static HV *coro_state_stash, *coro_stash;
static volatile SV *coro_mortal; /* will be freed/thrown after next transfer */

static AV *av_destroy; /* destruction queue */
static SV *sv_manager; /* the manager coro */
static SV *sv_idle; /* $Coro::idle */

static GV *irsgv;    /* $/ */
static GV *stdoutgv; /* *STDOUT */
static SV *rv_diehook;
static SV *rv_warnhook;
static HV *hv_sig;   /* %SIG */

/* async_pool helper stuff */
static SV *sv_pool_rss;
static SV *sv_pool_size;
static SV *sv_async_pool_idle; /* description string */
static AV *av_async_pool; /* idle pool */
static SV *sv_Coro; /* class string */
static CV *cv_pool_handler;
static CV *cv_coro_state_new;

/* Coro::AnyEvent */
static SV *sv_activity;

static struct coro_cctx *cctx_first;
static int cctx_count, cctx_idle;

enum {
  CC_MAPPED     = 0x01,
  CC_NOREUSE    = 0x02, /* throw this away after tracing */
  CC_TRACE      = 0x04,
  CC_TRACE_SUB  = 0x08, /* trace sub calls */
  CC_TRACE_LINE = 0x10, /* trace each statement */
  CC_TRACE_ALL  = CC_TRACE_SUB | CC_TRACE_LINE,
};

/* this is a structure representing a c-level coroutine */
typedef struct coro_cctx
{
  struct coro_cctx *next;

  /* the stack */
  void *sptr;
  size_t ssize;

  /* cpu state */
  void *idle_sp;   /* sp of top-level transfer/schedule/cede call */
  JMPENV *idle_te; /* same as idle_sp, but for top_env, TODO: remove once stable */
  JMPENV *top_env;
  coro_context cctx;

  U32 gen;
#if CORO_USE_VALGRIND
  int valgrind_id;
#endif
  unsigned char flags;
} coro_cctx;

coro_cctx *cctx_current; /* the currently running cctx */

/*****************************************************************************/

enum {
  CF_RUNNING   = 0x0001, /* coroutine is running */
  CF_READY     = 0x0002, /* coroutine is ready */
  CF_NEW       = 0x0004, /* has never been switched to */
  CF_DESTROYED = 0x0008, /* coroutine data has been freed */
  CF_SUSPENDED = 0x0010, /* coroutine can't be scheduled */
};

/* the structure where most of the perl state is stored, overlaid on the cxstack */
typedef struct
{
  SV *defsv;
  AV *defav;
  SV *errsv;
  SV *irsgv;
  HV *hinthv;
#define VAR(name,type) type name;
# include "state.h"
#undef VAR
} perl_slots;

#define SLOT_COUNT ((sizeof (perl_slots) + sizeof (PERL_CONTEXT) - 1) / sizeof (PERL_CONTEXT))

/* this is a structure representing a perl-level coroutine */
struct coro {
  /* the C coroutine allocated to this perl coroutine, if any */
  coro_cctx *cctx;

  /* ready queue */
  struct coro *next_ready;

  /* state data */
  struct CoroSLF slf_frame; /* saved slf frame */
  AV *mainstack;
  perl_slots *slot; /* basically the saved sp */

  CV *startcv; /* the CV to execute */
  AV *args;    /* data associated with this coroutine (initial args) */
  int refcnt;  /* coroutines are refcounted, yes */
  int flags;   /* CF_ flags */
  HV *hv;      /* the perl hash associated with this coro, if any */
  void (*on_destroy)(pTHX_ struct coro *coro);

  /* statistics */
  int usecount; /* number of transfers to this coro */

  /* coro process data */
  int prio;
  SV *except; /* exception to be thrown */
  SV *rouse_cb;

  /* async_pool */
  SV *saved_deffh;
  SV *invoke_cb;
  AV *invoke_av;

  /* on_enter/on_leave */
  AV *on_enter;
  AV *on_leave;

  /* linked list */
  struct coro *next, *prev;
};

typedef struct coro *Coro__State;
typedef struct coro *Coro__State_or_hashref;

/* the following variables are effectively part of the perl context */
/* and get copied between struct coro and these variables */
/* the mainr easonw e don't support windows process emulation */
static struct CoroSLF slf_frame; /* the current slf frame */

/** Coro ********************************************************************/

#define PRIO_MAX     3
#define PRIO_HIGH    1
#define PRIO_NORMAL  0
#define PRIO_LOW    -1
#define PRIO_IDLE   -3
#define PRIO_MIN    -4

/* for Coro.pm */
static SV *coro_current;
static SV *coro_readyhook;
static struct coro *coro_ready [PRIO_MAX - PRIO_MIN + 1][2]; /* head|tail */
static CV *cv_coro_run, *cv_coro_terminate;
static struct coro *coro_first;
#define coro_nready coroapi.nready

/** lowlevel stuff **********************************************************/

static SV *
coro_get_sv (pTHX_ const char *name, int create)
{
#if PERL_VERSION_ATLEAST (5,10,0)
         /* silence stupid and wrong 5.10 warning that I am unable to switch off */
         get_sv (name, create);
#endif
  return get_sv (name, create);
}

static AV *
coro_get_av (pTHX_ const char *name, int create)
{
#if PERL_VERSION_ATLEAST (5,10,0)
         /* silence stupid and wrong 5.10 warning that I am unable to switch off */
         get_av (name, create);
#endif
  return get_av (name, create);
}

static HV *
coro_get_hv (pTHX_ const char *name, int create)
{
#if PERL_VERSION_ATLEAST (5,10,0)
         /* silence stupid and wrong 5.10 warning that I am unable to switch off */
         get_hv (name, create);
#endif
  return get_hv (name, create);
}

/* may croak */
INLINE CV *
coro_sv_2cv (pTHX_ SV *sv)
{
  HV *st;
  GV *gvp;
  CV *cv = sv_2cv (sv, &st, &gvp, 0);

  if (!cv)
    croak ("code reference expected");

  return cv;
}

/*****************************************************************************/
/* magic glue */

#define CORO_MAGIC_type_cv    26
#define CORO_MAGIC_type_state PERL_MAGIC_ext

#define CORO_MAGIC_NN(sv, type)			\
  (expect_true (SvMAGIC (sv)->mg_type == type)	\
    ? SvMAGIC (sv)				\
    : mg_find (sv, type))

#define CORO_MAGIC(sv, type)			\
  (expect_true (SvMAGIC (sv))			\
    ? CORO_MAGIC_NN (sv, type)			\
    : 0)

#define CORO_MAGIC_cv(cv)    CORO_MAGIC    (((SV *)(cv)), CORO_MAGIC_type_cv)
#define CORO_MAGIC_state(sv) CORO_MAGIC_NN (((SV *)(sv)), CORO_MAGIC_type_state)

INLINE struct coro *
SvSTATE_ (pTHX_ SV *coro)
{
  HV *stash;
  MAGIC *mg;

  if (SvROK (coro))
    coro = SvRV (coro);

  if (expect_false (SvTYPE (coro) != SVt_PVHV))
    croak ("Coro::State object required");

  stash = SvSTASH (coro);
  if (expect_false (stash != coro_stash && stash != coro_state_stash))
    {
      /* very slow, but rare, check */
      if (!sv_derived_from (sv_2mortal (newRV_inc (coro)), "Coro::State"))
        croak ("Coro::State object required");
    }

  mg = CORO_MAGIC_state (coro);
  return (struct coro *)mg->mg_ptr;
}

#define SvSTATE(sv) SvSTATE_ (aTHX_ (sv))

/* faster than SvSTATE, but expects a coroutine hv */
#define SvSTATE_hv(hv)  ((struct coro *)CORO_MAGIC_NN ((SV *)hv, CORO_MAGIC_type_state)->mg_ptr)
#define SvSTATE_current SvSTATE_hv (SvRV (coro_current))

/*****************************************************************************/
/* padlist management and caching */

static AV *
coro_derive_padlist (pTHX_ CV *cv)
{
  AV *padlist = CvPADLIST (cv);
  AV *newpadlist, *newpad;

  newpadlist = newAV ();
  AvREAL_off (newpadlist);
#if PERL_VERSION_ATLEAST (5,10,0)
  Perl_pad_push (aTHX_ padlist, AvFILLp (padlist) + 1);
#else
  Perl_pad_push (aTHX_ padlist, AvFILLp (padlist) + 1, 1);
#endif
  newpad = (AV *)AvARRAY (padlist)[AvFILLp (padlist)];
  --AvFILLp (padlist);

  av_store (newpadlist, 0, SvREFCNT_inc_NN (AvARRAY (padlist)[0]));
  av_store (newpadlist, 1, (SV *)newpad);

  return newpadlist;
}

static void
free_padlist (pTHX_ AV *padlist)
{
  /* may be during global destruction */
  if (!IN_DESTRUCT)
    {
      I32 i = AvFILLp (padlist);

      while (i > 0) /* special-case index 0 */
        {
          /* we try to be extra-careful here */
          AV *av = (AV *)AvARRAY (padlist)[i--];
          I32 j = AvFILLp (av);

          while (j >= 0)
            SvREFCNT_dec (AvARRAY (av)[j--]);

          AvFILLp (av) = -1;
          SvREFCNT_dec (av);
        }

      SvREFCNT_dec (AvARRAY (padlist)[0]);

      AvFILLp (padlist) = -1;
      SvREFCNT_dec ((SV*)padlist);
    }
}

static int
coro_cv_free (pTHX_ SV *sv, MAGIC *mg)
{
  AV *padlist;
  AV *av = (AV *)mg->mg_obj;

  /* casting is fun. */
  while (&PL_sv_undef != (SV *)(padlist = (AV *)av_pop (av)))
    free_padlist (aTHX_ padlist);

  SvREFCNT_dec (av); /* sv_magicext increased the refcount */

  return 0;
}

static MGVTBL coro_cv_vtbl = {
  0, 0, 0, 0,
  coro_cv_free
};

/* the next two functions merely cache the padlists */
static void
get_padlist (pTHX_ CV *cv)
{
  MAGIC *mg = CORO_MAGIC_cv (cv);
  AV *av;

  if (expect_true (mg && AvFILLp ((av = (AV *)mg->mg_obj)) >= 0))
    CvPADLIST (cv) = (AV *)AvARRAY (av)[AvFILLp (av)--];
  else
   {
#if CORO_PREFER_PERL_FUNCTIONS
     /* this is probably cleaner? but also slower! */
     /* in practise, it seems to be less stable */
     CV *cp = Perl_cv_clone (aTHX_ cv);
     CvPADLIST (cv) = CvPADLIST (cp);
     CvPADLIST (cp) = 0;
     SvREFCNT_dec (cp);
#else
     CvPADLIST (cv) = coro_derive_padlist (aTHX_ cv);
#endif
   }
}

static void
put_padlist (pTHX_ CV *cv)
{
  MAGIC *mg = CORO_MAGIC_cv (cv);
  AV *av;

  if (expect_false (!mg))
    mg = sv_magicext ((SV *)cv, (SV *)newAV (), CORO_MAGIC_type_cv, &coro_cv_vtbl, 0, 0);

  av = (AV *)mg->mg_obj;

  if (expect_false (AvFILLp (av) >= AvMAX (av)))
    av_extend (av, AvFILLp (av) + 1);

  AvARRAY (av)[++AvFILLp (av)] = (SV *)CvPADLIST (cv);
}

/** load & save, init *******************************************************/

static void
on_enterleave_call (pTHX_ SV *cb);

static void
load_perl (pTHX_ Coro__State c)
{
  perl_slots *slot = c->slot;
  c->slot = 0;

  PL_mainstack = c->mainstack;

  GvSV (PL_defgv)  = slot->defsv;
  GvAV (PL_defgv)  = slot->defav;
  GvSV (PL_errgv)  = slot->errsv;
  GvSV (irsgv)     = slot->irsgv;
  GvHV (PL_hintgv) = slot->hinthv;

  #define VAR(name,type) PL_ ## name = slot->name;
  # include "state.h"
  #undef VAR

  {
    dSP;

    CV *cv;

    /* now do the ugly restore mess */
    while (expect_true (cv = (CV *)POPs))
      {
        put_padlist (aTHX_ cv); /* mark this padlist as available */
        CvDEPTH (cv) = PTR2IV (POPs);
        CvPADLIST (cv) = (AV *)POPs;
      }

    PUTBACK;
  }

  slf_frame  = c->slf_frame;
  CORO_THROW = c->except;

  if (expect_false (c->on_enter))
    {
      int i;

      for (i = 0; i <= AvFILLp (c->on_enter); ++i)
        on_enterleave_call (aTHX_ AvARRAY (c->on_enter)[i]);
    }
}

static void
save_perl (pTHX_ Coro__State c)
{
  if (expect_false (c->on_leave))
    {
      int i;

      for (i = AvFILLp (c->on_leave); i >= 0; --i)
        on_enterleave_call (aTHX_ AvARRAY (c->on_leave)[i]);
    }

  c->except    = CORO_THROW;
  c->slf_frame = slf_frame;

  {
    dSP;
    I32 cxix = cxstack_ix;
    PERL_CONTEXT *ccstk = cxstack;
    PERL_SI *top_si = PL_curstackinfo;

    /*
     * the worst thing you can imagine happens first - we have to save
     * (and reinitialize) all cv's in the whole callchain :(
     */

    XPUSHs (Nullsv);
    /* this loop was inspired by pp_caller */
    for (;;)
      {
        while (expect_true (cxix >= 0))
          {
            PERL_CONTEXT *cx = &ccstk[cxix--];

            if (expect_true (CxTYPE (cx) == CXt_SUB) || expect_false (CxTYPE (cx) == CXt_FORMAT))
              {
                CV *cv = cx->blk_sub.cv;

                if (expect_true (CvDEPTH (cv)))
                  {
                    EXTEND (SP, 3);
                    PUSHs ((SV *)CvPADLIST (cv));
                    PUSHs (INT2PTR (SV *, (IV)CvDEPTH (cv)));
                    PUSHs ((SV *)cv);

                    CvDEPTH (cv) = 0;
                    get_padlist (aTHX_ cv);
                  }
              }
          }

        if (expect_true (top_si->si_type == PERLSI_MAIN))
          break;

        top_si = top_si->si_prev;
        ccstk  = top_si->si_cxstack;
        cxix   = top_si->si_cxix;
      }

    PUTBACK;
  }

  /* allocate some space on the context stack for our purposes */
  /* we manually unroll here, as usually 2 slots is enough */
  if (SLOT_COUNT >= 1) CXINC;
  if (SLOT_COUNT >= 2) CXINC;
  if (SLOT_COUNT >= 3) CXINC;
  {
    int i;
    for (i = 3; i < SLOT_COUNT; ++i)
      CXINC;
  }
  cxstack_ix -= SLOT_COUNT; /* undo allocation */

  c->mainstack = PL_mainstack;

  {
    perl_slots *slot = c->slot = (perl_slots *)(cxstack + cxstack_ix + 1);

    slot->defav  = GvAV (PL_defgv);
    slot->defsv  = DEFSV;
    slot->errsv  = ERRSV;
    slot->irsgv  = GvSV (irsgv);
    slot->hinthv = GvHV (PL_hintgv);

    #define VAR(name,type) slot->name = PL_ ## name;
    # include "state.h"
    #undef VAR
  }
}

/*
 * allocate various perl stacks. This is almost an exact copy
 * of perl.c:init_stacks, except that it uses less memory
 * on the (sometimes correct) assumption that coroutines do
 * not usually need a lot of stackspace.
 */
#if CORO_PREFER_PERL_FUNCTIONS
# define coro_init_stacks(thx) init_stacks ()
#else
static void
coro_init_stacks (pTHX)
{
    PL_curstackinfo = new_stackinfo(32, 8);
    PL_curstackinfo->si_type = PERLSI_MAIN;
    PL_curstack = PL_curstackinfo->si_stack;
    PL_mainstack = PL_curstack;		/* remember in case we switch stacks */

    PL_stack_base = AvARRAY(PL_curstack);
    PL_stack_sp = PL_stack_base;
    PL_stack_max = PL_stack_base + AvMAX(PL_curstack);

    New(50,PL_tmps_stack,32,SV*);
    PL_tmps_floor = -1;
    PL_tmps_ix = -1;
    PL_tmps_max = 32;

    New(54,PL_markstack,16,I32);
    PL_markstack_ptr = PL_markstack;
    PL_markstack_max = PL_markstack + 16;

#ifdef SET_MARK_OFFSET
    SET_MARK_OFFSET;
#endif

    New(54,PL_scopestack,8,I32);
    PL_scopestack_ix = 0;
    PL_scopestack_max = 8;

    New(54,PL_savestack,24,ANY);
    PL_savestack_ix = 0;
    PL_savestack_max = 24;

#if !PERL_VERSION_ATLEAST (5,10,0)
    New(54,PL_retstack,4,OP*);
    PL_retstack_ix = 0;
    PL_retstack_max = 4;
#endif
}
#endif

/*
 * destroy the stacks, the callchain etc...
 */
static void
coro_destruct_stacks (pTHX)
{
  while (PL_curstackinfo->si_next)
    PL_curstackinfo = PL_curstackinfo->si_next;

  while (PL_curstackinfo)
    {
      PERL_SI *p = PL_curstackinfo->si_prev;

      if (!IN_DESTRUCT)
        SvREFCNT_dec (PL_curstackinfo->si_stack);

      Safefree (PL_curstackinfo->si_cxstack);
      Safefree (PL_curstackinfo);
      PL_curstackinfo = p;
  }

  Safefree (PL_tmps_stack);
  Safefree (PL_markstack);
  Safefree (PL_scopestack);
  Safefree (PL_savestack);
#if !PERL_VERSION_ATLEAST (5,10,0)
  Safefree (PL_retstack);
#endif
}

#define CORO_RSS										\
  rss += sizeof (SYM (curstackinfo));								\
  rss += (SYM (curstackinfo->si_cxmax) + 1) * sizeof (PERL_CONTEXT);				\
  rss += sizeof (SV) + sizeof (struct xpvav) + (1 + AvMAX (SYM (curstack))) * sizeof (SV *);	\
  rss += SYM (tmps_max) * sizeof (SV *);							\
  rss += (SYM (markstack_max) - SYM (markstack_ptr)) * sizeof (I32);				\
  rss += SYM (scopestack_max) * sizeof (I32);							\
  rss += SYM (savestack_max) * sizeof (ANY);

static size_t
coro_rss (pTHX_ struct coro *coro)
{
  size_t rss = sizeof (*coro);

  if (coro->mainstack)
    {
      if (coro->flags & CF_RUNNING)
        {
          #define SYM(sym) PL_ ## sym
          CORO_RSS;
          #undef SYM
        }
      else
        {
          #define SYM(sym) coro->slot->sym
          CORO_RSS;
          #undef SYM
        }
    }

  return rss;
}

/** coroutine stack handling ************************************************/

static int (*orig_sigelem_get) (pTHX_ SV *sv, MAGIC *mg);
static int (*orig_sigelem_set) (pTHX_ SV *sv, MAGIC *mg);
static int (*orig_sigelem_clr) (pTHX_ SV *sv, MAGIC *mg);

/* apparently < 5.8.8 */
#ifndef MgPV_nolen_const
#define MgPV_nolen_const(mg)    (((((int)(mg)->mg_len)) == HEf_SVKEY) ?   \
                                 SvPV_nolen((SV*)((mg)->mg_ptr)) :  \
                                 (const char*)(mg)->mg_ptr)
#endif

/*
 * This overrides the default magic get method of %SIG elements.
 * The original one doesn't provide for reading back of PL_diehook/PL_warnhook
 * and instead of trying to save and restore the hash elements, we just provide
 * readback here.
 */
static int
coro_sigelem_get (pTHX_ SV *sv, MAGIC *mg)
{
  const char *s = MgPV_nolen_const (mg);

  if (*s == '_')
    {
      SV **svp = 0;

      if (strEQ (s, "__DIE__" )) svp = &PL_diehook;
      if (strEQ (s, "__WARN__")) svp = &PL_warnhook;
      
      if (svp)
        {
          sv_setsv (sv, *svp ? *svp : &PL_sv_undef);
          return 0;
        }
    }

  return orig_sigelem_get ? orig_sigelem_get (aTHX_ sv, mg) : 0;
}

static int
coro_sigelem_clr (pTHX_ SV *sv, MAGIC *mg)
{
  const char *s = MgPV_nolen_const (mg);

  if (*s == '_')
    {
      SV **svp = 0;

      if (strEQ (s, "__DIE__" )) svp = &PL_diehook;
      if (strEQ (s, "__WARN__")) svp = &PL_warnhook;

      if (svp)
        {
          SV *old = *svp;
          *svp = 0;
          SvREFCNT_dec (old);
          return 0;
        }
    }

  return orig_sigelem_clr ? orig_sigelem_clr (aTHX_ sv, mg) : 0;
}

static int
coro_sigelem_set (pTHX_ SV *sv, MAGIC *mg)
{
  const char *s = MgPV_nolen_const (mg);

  if (*s == '_')
    {
      SV **svp = 0;

      if (strEQ (s, "__DIE__" )) svp = &PL_diehook;
      if (strEQ (s, "__WARN__")) svp = &PL_warnhook;

      if (svp)
        {
          SV *old = *svp;
          *svp = SvOK (sv) ? newSVsv (sv) : 0;
          SvREFCNT_dec (old);
          return 0;
        }
    }

  return orig_sigelem_set ? orig_sigelem_set (aTHX_ sv, mg) : 0;
}

static void
prepare_nop (pTHX_ struct coro_transfer_args *ta)
{
  /* kind of mega-hacky, but works */
  ta->next = ta->prev = (struct coro *)ta;
}

static int
slf_check_nop (pTHX_ struct CoroSLF *frame)
{
  return 0;
}

static int
slf_check_repeat (pTHX_ struct CoroSLF *frame)
{
  return 1;
}

static UNOP coro_setup_op;

static void NOINLINE /* noinline to keep it out of the transfer fast path */
coro_setup (pTHX_ struct coro *coro)
{
  /*
   * emulate part of the perl startup here.
   */
  coro_init_stacks (aTHX);

  PL_runops     = RUNOPS_DEFAULT;
  PL_curcop     = &PL_compiling;
  PL_in_eval    = EVAL_NULL;
  PL_comppad    = 0;
  PL_comppad_name       = 0;
  PL_comppad_name_fill  = 0;
  PL_comppad_name_floor = 0;
  PL_curpm      = 0;
  PL_curpad     = 0;
  PL_localizing = 0;
  PL_dirty      = 0;
  PL_restartop  = 0;
#if PERL_VERSION_ATLEAST (5,10,0)
  PL_parser     = 0;
#endif
  PL_hints      = 0;

  /* recreate the die/warn hooks */
  PL_diehook  = 0; SvSetMagicSV (*hv_fetch (hv_sig, "__DIE__" , sizeof ("__DIE__" ) - 1, 1), rv_diehook );
  PL_warnhook = 0; SvSetMagicSV (*hv_fetch (hv_sig, "__WARN__", sizeof ("__WARN__") - 1, 1), rv_warnhook);
  
  GvSV (PL_defgv)    = newSV (0);
  GvAV (PL_defgv)    = coro->args; coro->args = 0;
  GvSV (PL_errgv)    = newSV (0);
  GvSV (irsgv)       = newSVpvn ("\n", 1); sv_magic (GvSV (irsgv), (SV *)irsgv, PERL_MAGIC_sv, "/", 0);
  GvHV (PL_hintgv)   = 0;
  PL_rs              = newSVsv (GvSV (irsgv));
  PL_defoutgv        = (GV *)SvREFCNT_inc_NN (stdoutgv);

  {
    dSP;
    UNOP myop;

    Zero (&myop, 1, UNOP);
    myop.op_next  = Nullop;
    myop.op_type  = OP_ENTERSUB;
    myop.op_flags = OPf_WANT_VOID;

    PUSHMARK (SP);
    PUSHs ((SV *)coro->startcv);
    PUTBACK;
    PL_op = (OP *)&myop;
    PL_op = PL_ppaddr[OP_ENTERSUB](aTHX);
  }

  /* this newly created coroutine might be run on an existing cctx which most
   * likely was suspended in pp_slf, so we have to emulate entering pp_slf here.
   */
  slf_frame.prepare = prepare_nop;   /* provide a nop function for an eventual pp_slf */
  slf_frame.check   = slf_check_nop; /* signal pp_slf to not repeat */

  /* and we have to provide the pp_slf op in any case, so pp_slf can skip it */
  coro_setup_op.op_next   = PL_op;
  coro_setup_op.op_type   = OP_ENTERSUB;
  coro_setup_op.op_ppaddr = pp_slf;
  /* no flags etc. required, as an init function won't be called */

  PL_op = (OP *)&coro_setup_op;

  /* copy throw, in case it was set before coro_setup */
  CORO_THROW = coro->except;
}

static void
coro_unwind_stacks (pTHX)
{
  if (!IN_DESTRUCT)
    {
      /* restore all saved variables and stuff */
      LEAVE_SCOPE (0);
      assert (PL_tmps_floor == -1);

      /* free all temporaries */
      FREETMPS;
      assert (PL_tmps_ix == -1);

      /* unwind all extra stacks */
      POPSTACK_TO (PL_mainstack);

      /* unwind main stack */
      dounwind (-1);
    }
}

static void
coro_destruct_perl (pTHX_ struct coro *coro)
{
  SV *svf [9];

  {
    struct coro *current = SvSTATE_current;

    assert (("FATAL: tried to destroy currently running coroutine", coro->mainstack != PL_mainstack));

    save_perl (aTHX_ current);
    load_perl (aTHX_ coro);

    coro_unwind_stacks (aTHX);
    coro_destruct_stacks (aTHX);

    // now save some sv's to be free'd later
    svf    [0] =       GvSV (PL_defgv);
    svf    [1] = (SV *)GvAV (PL_defgv);
    svf    [2] =       GvSV (PL_errgv);
    svf    [3] = (SV *)PL_defoutgv;
    svf    [4] =       PL_rs;
    svf    [5] =       GvSV (irsgv);
    svf    [6] = (SV *)GvHV (PL_hintgv);
    svf    [7] =       PL_diehook;
    svf    [8] =       PL_warnhook;
    assert (9 == sizeof (svf) / sizeof (*svf));

    load_perl (aTHX_ current);
  }

  {
    int i;

    for (i = 0; i < sizeof (svf) / sizeof (*svf); ++i)
      SvREFCNT_dec (svf [i]);

    SvREFCNT_dec (coro->saved_deffh);
    SvREFCNT_dec (coro->rouse_cb);
    SvREFCNT_dec (coro->invoke_cb);
    SvREFCNT_dec (coro->invoke_av);
  }
}

INLINE void
free_coro_mortal (pTHX)
{
  if (expect_true (coro_mortal))
    {
      SvREFCNT_dec (coro_mortal);
      coro_mortal = 0;
    }
}

static int
runops_trace (pTHX)
{
  COP *oldcop = 0;
  int oldcxix = -2;

  while ((PL_op = CALL_FPTR (PL_op->op_ppaddr) (aTHX)))
    {
      PERL_ASYNC_CHECK ();

      if (cctx_current->flags & CC_TRACE_ALL)
        {
          if (PL_op->op_type == OP_LEAVESUB && cctx_current->flags & CC_TRACE_SUB)
            {
              PERL_CONTEXT *cx = &cxstack[cxstack_ix];
              SV **bot, **top;
              AV *av = newAV (); /* return values */
              SV **cb;
              dSP;

              GV *gv = CvGV (cx->blk_sub.cv);
              SV *fullname = sv_2mortal (newSV (0));
              if (isGV (gv))
                gv_efullname3 (fullname, gv, 0);

              bot = PL_stack_base + cx->blk_oldsp + 1;
              top = cx->blk_gimme == G_ARRAY  ? SP + 1
                  : cx->blk_gimme == G_SCALAR ? bot + 1
                  :                             bot;

              av_extend (av, top - bot);
              while (bot < top)
                av_push (av, SvREFCNT_inc_NN (*bot++));

              PL_runops = RUNOPS_DEFAULT;
              ENTER;
              SAVETMPS;
              EXTEND (SP, 3);
              PUSHMARK (SP);
              PUSHs (&PL_sv_no);
              PUSHs (fullname);
              PUSHs (sv_2mortal (newRV_noinc ((SV *)av)));
              PUTBACK;
              cb = hv_fetch ((HV *)SvRV (coro_current), "_trace_sub_cb", sizeof ("_trace_sub_cb") - 1, 0);
              if (cb) call_sv (*cb, G_KEEPERR | G_EVAL | G_VOID | G_DISCARD);
              SPAGAIN;
              FREETMPS;
              LEAVE;
              PL_runops = runops_trace;
            }

          if (oldcop != PL_curcop)
            {
              oldcop = PL_curcop;

              if (PL_curcop != &PL_compiling)
                {
                  SV **cb;

                  if (oldcxix != cxstack_ix && cctx_current->flags & CC_TRACE_SUB)
                    {
                      PERL_CONTEXT *cx = &cxstack[cxstack_ix];

                      if (CxTYPE (cx) == CXt_SUB && oldcxix < cxstack_ix)
                        {
                          dSP;
                          GV *gv = CvGV (cx->blk_sub.cv);
                          SV *fullname = sv_2mortal (newSV (0));

                          if (isGV (gv))
                            gv_efullname3 (fullname, gv, 0);

                          PL_runops = RUNOPS_DEFAULT;
                          ENTER;
                          SAVETMPS;
                          EXTEND (SP, 3);
                          PUSHMARK (SP);
                          PUSHs (&PL_sv_yes);
                          PUSHs (fullname);
                          PUSHs (CxHASARGS (cx) ? sv_2mortal (newRV_inc ((SV *)cx->blk_sub.argarray)) : &PL_sv_undef);
                          PUTBACK;
                          cb = hv_fetch ((HV *)SvRV (coro_current), "_trace_sub_cb", sizeof ("_trace_sub_cb") - 1, 0);
                          if (cb) call_sv (*cb, G_KEEPERR | G_EVAL | G_VOID | G_DISCARD);
                          SPAGAIN;
                          FREETMPS;
                          LEAVE;
                          PL_runops = runops_trace;
                        }

                      oldcxix = cxstack_ix;
                    }

                  if (cctx_current->flags & CC_TRACE_LINE)
                    {
                      dSP;

                      PL_runops = RUNOPS_DEFAULT;
                      ENTER;
                      SAVETMPS;
                      EXTEND (SP, 3);
                      PL_runops = RUNOPS_DEFAULT;
                      PUSHMARK (SP);
                      PUSHs (sv_2mortal (newSVpv (OutCopFILE (oldcop), 0)));
                      PUSHs (sv_2mortal (newSViv (CopLINE (oldcop))));
                      PUTBACK;
                      cb = hv_fetch ((HV *)SvRV (coro_current), "_trace_line_cb", sizeof ("_trace_line_cb") - 1, 0);
                      if (cb) call_sv (*cb, G_KEEPERR | G_EVAL | G_VOID | G_DISCARD);
                      SPAGAIN;
                      FREETMPS;
                      LEAVE;
                      PL_runops = runops_trace;
                    }
                }
            }
        }
    }

  TAINT_NOT;
  return 0;
}

static struct CoroSLF cctx_ssl_frame;

static void
slf_prepare_set_stacklevel (pTHX_ struct coro_transfer_args *ta)
{
  ta->prev = 0;
}

static int
slf_check_set_stacklevel (pTHX_ struct CoroSLF *frame)
{
  *frame = cctx_ssl_frame;

  return frame->check (aTHX_ frame); /* execute the restored frame - there must be one */
}

/* initialises PL_top_env and injects a pseudo-slf-call to set the stacklevel */
static void NOINLINE
cctx_prepare (pTHX)
{
  PL_top_env = &PL_start_env;

  if (cctx_current->flags & CC_TRACE)
    PL_runops = runops_trace;

  /* we already must be executing an SLF op, there is no other valid way
   * that can lead to creation of a new cctx */
  assert (("FATAL: can't prepare slf-less cctx in Coro module (please report)",
           slf_frame.prepare && PL_op->op_ppaddr == pp_slf));

  /* we must emulate leaving pp_slf, which is done inside slf_check_set_stacklevel */
  cctx_ssl_frame = slf_frame;

  slf_frame.prepare = slf_prepare_set_stacklevel;
  slf_frame.check   = slf_check_set_stacklevel;
}

/* the tail of transfer: execute stuff we can only do after a transfer */
INLINE void
transfer_tail (pTHX)
{
  free_coro_mortal (aTHX);
}

/*
 * this is a _very_ stripped down perl interpreter ;)
 */
static void
cctx_run (void *arg)
{
#ifdef USE_ITHREADS
# if CORO_PTHREAD
  PERL_SET_CONTEXT (coro_thx);
# endif
#endif
  {
    dTHX;

    /* normally we would need to skip the entersub here */
    /* not doing so will re-execute it, which is exactly what we want */
    /* PL_nop = PL_nop->op_next */

    /* inject a fake subroutine call to cctx_init */
    cctx_prepare (aTHX);

    /* cctx_run is the alternative tail of transfer() */
    transfer_tail (aTHX);

    /* somebody or something will hit me for both perl_run and PL_restartop */
    PL_restartop = PL_op;
    perl_run (PL_curinterp);
    /*
     * Unfortunately, there is no way to get at the return values of the
     * coro body here, as perl_run destroys these
     */

    /*
     * If perl-run returns we assume exit() was being called or the coro
     * fell off the end, which seems to be the only valid (non-bug)
     * reason for perl_run to return. We try to exit by jumping to the
     * bootstrap-time "top" top_env, as we cannot restore the "main"
     * coroutine as Coro has no such concept.
     * This actually isn't valid with the pthread backend, but OSes requiring
     * that backend are too broken to do it in a standards-compliant way.
     */
    PL_top_env = main_top_env;
    JMPENV_JUMP (2); /* I do not feel well about the hardcoded 2 at all */
  }
}

static coro_cctx *
cctx_new ()
{
  coro_cctx *cctx;

  ++cctx_count;
  New (0, cctx, 1, coro_cctx);

  cctx->gen     = cctx_gen;
  cctx->flags   = 0;
  cctx->idle_sp = 0; /* can be accessed by transfer between cctx_run and set_stacklevel, on throw */

  return cctx;
}

/* create a new cctx only suitable as source */
static coro_cctx *
cctx_new_empty ()
{
  coro_cctx *cctx = cctx_new ();

  cctx->sptr = 0;
  coro_create (&cctx->cctx, 0, 0, 0, 0);

  return cctx;
}

/* create a new cctx suitable as destination/running a perl interpreter */
static coro_cctx *
cctx_new_run ()
{
  coro_cctx *cctx = cctx_new ();
  void *stack_start;
  size_t stack_size;

#if HAVE_MMAP
  cctx->ssize = ((cctx_stacksize * sizeof (long) + PAGESIZE - 1) / PAGESIZE + CORO_STACKGUARD) * PAGESIZE;
  /* mmap supposedly does allocate-on-write for us */
  cctx->sptr = mmap (0, cctx->ssize, PROT_EXEC|PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);

  if (cctx->sptr != (void *)-1)
    {
      #if CORO_STACKGUARD
        mprotect (cctx->sptr, CORO_STACKGUARD * PAGESIZE, PROT_NONE);
      #endif
      stack_start = (char *)cctx->sptr + CORO_STACKGUARD * PAGESIZE;
      stack_size  = cctx->ssize        - CORO_STACKGUARD * PAGESIZE;
      cctx->flags |= CC_MAPPED;
    }
  else
#endif
    {
      cctx->ssize = cctx_stacksize * (long)sizeof (long);
      New (0, cctx->sptr, cctx_stacksize, long);

      if (!cctx->sptr)
        {
          perror ("FATAL: unable to allocate stack for coroutine, exiting.");
          _exit (EXIT_FAILURE);
        }

      stack_start = cctx->sptr;
      stack_size  = cctx->ssize;
    }

  #if CORO_USE_VALGRIND
    cctx->valgrind_id = VALGRIND_STACK_REGISTER ((char *)stack_start, (char *)stack_start + stack_size);
  #endif

  coro_create (&cctx->cctx, cctx_run, (void *)cctx, stack_start, stack_size);

  return cctx;
}

static void
cctx_destroy (coro_cctx *cctx)
{
  if (!cctx)
    return;

  assert (("FATAL: tried to destroy current cctx", cctx != cctx_current));//D temporary?

  --cctx_count;
  coro_destroy (&cctx->cctx);

  /* coro_transfer creates new, empty cctx's */
  if (cctx->sptr)
    {
      #if CORO_USE_VALGRIND
        VALGRIND_STACK_DEREGISTER (cctx->valgrind_id);
      #endif

#if HAVE_MMAP
      if (cctx->flags & CC_MAPPED)
        munmap (cctx->sptr, cctx->ssize);
      else
#endif
        Safefree (cctx->sptr);
    }

  Safefree (cctx);
}

/* wether this cctx should be destructed */
#define CCTX_EXPIRED(cctx) ((cctx)->gen != cctx_gen || ((cctx)->flags & CC_NOREUSE))

static coro_cctx *
cctx_get (pTHX)
{
  while (expect_true (cctx_first))
    {
      coro_cctx *cctx = cctx_first;
      cctx_first = cctx->next;
      --cctx_idle;

      if (expect_true (!CCTX_EXPIRED (cctx)))
        return cctx;

      cctx_destroy (cctx);
    }

  return cctx_new_run ();
}

static void
cctx_put (coro_cctx *cctx)
{
  assert (("FATAL: cctx_put called on non-initialised cctx in Coro (please report)", cctx->sptr));

  /* free another cctx if overlimit */
  if (expect_false (cctx_idle >= cctx_max_idle))
    {
      coro_cctx *first = cctx_first;
      cctx_first = first->next;
      --cctx_idle;

      cctx_destroy (first);
    }

  ++cctx_idle;
  cctx->next = cctx_first;
  cctx_first = cctx;
}

/** coroutine switching *****************************************************/

static void
transfer_check (pTHX_ struct coro *prev, struct coro *next)
{
  /* TODO: throwing up here is considered harmful */

  if (expect_true (prev != next))
    {
      if (expect_false (!(prev->flags & (CF_RUNNING | CF_NEW))))
        croak ("Coro::State::transfer called with a blocked prev Coro::State, but can only transfer from running or new states,");

      if (expect_false (next->flags & (CF_RUNNING | CF_DESTROYED | CF_SUSPENDED)))
        croak ("Coro::State::transfer called with running, destroyed or suspended next Coro::State, but can only transfer to inactive states,");

#if !PERL_VERSION_ATLEAST (5,10,0)
      if (expect_false (PL_lex_state != LEX_NOTPARSING))
        croak ("Coro::State::transfer called while parsing, but this is not supported in your perl version,");
#endif
    }
}

/* always use the TRANSFER macro */
static void NOINLINE /* noinline so we have a fixed stackframe */
transfer (pTHX_ struct coro *prev, struct coro *next, int force_cctx)
{
  dSTACKLEVEL;

  /* sometimes transfer is only called to set idle_sp */
  if (expect_false (!prev))
    {
      cctx_current->idle_sp = STACKLEVEL;
      assert (cctx_current->idle_te = PL_top_env); /* just for the side-effect when asserts are enabled */
    }
  else if (expect_true (prev != next))
    {
      coro_cctx *cctx_prev;

      if (expect_false (prev->flags & CF_NEW))
        {
          /* create a new empty/source context */
          prev->flags &= ~CF_NEW;
          prev->flags |=  CF_RUNNING;
        }

      prev->flags &= ~CF_RUNNING;
      next->flags |=  CF_RUNNING;

      /* first get rid of the old state */
      save_perl (aTHX_ prev);

      if (expect_false (next->flags & CF_NEW))
        {
          /* need to start coroutine */
          next->flags &= ~CF_NEW;
          /* setup coroutine call */
          coro_setup (aTHX_ next);
        }
      else
        load_perl (aTHX_ next);

      /* possibly untie and reuse the cctx */
      if (expect_true (
            cctx_current->idle_sp == STACKLEVEL
            && !(cctx_current->flags & CC_TRACE)
            && !force_cctx
         ))
        {
          /* I assume that stacklevel is a stronger indicator than PL_top_env changes */
          assert (("FATAL: current top_env must equal previous top_env in Coro (please report)", PL_top_env == cctx_current->idle_te));

          /* if the cctx is about to be destroyed we need to make sure we won't see it in cctx_get. */
          /* without this the next cctx_get might destroy the running cctx while still in use */
          if (expect_false (CCTX_EXPIRED (cctx_current)))
            if (expect_true (!next->cctx))
              next->cctx = cctx_get (aTHX);

          cctx_put (cctx_current);
        }
      else
        prev->cctx = cctx_current;

      ++next->usecount;

      cctx_prev    = cctx_current;
      cctx_current = expect_false (next->cctx) ? next->cctx : cctx_get (aTHX);

      next->cctx = 0;

      if (expect_false (cctx_prev != cctx_current))
        {
          cctx_prev->top_env = PL_top_env;
          PL_top_env = cctx_current->top_env;
          coro_transfer (&cctx_prev->cctx, &cctx_current->cctx);
        }

      transfer_tail (aTHX);
    }
}

#define TRANSFER(ta, force_cctx) transfer (aTHX_ (ta).prev, (ta).next, (force_cctx))
#define TRANSFER_CHECK(ta) transfer_check (aTHX_ (ta).prev, (ta).next)

/** high level stuff ********************************************************/

static int
coro_state_destroy (pTHX_ struct coro *coro)
{
  if (coro->flags & CF_DESTROYED)
    return 0;

  if (coro->on_destroy && !PL_dirty)
    coro->on_destroy (aTHX_ coro);

  coro->flags |= CF_DESTROYED;
  
  if (coro->flags & CF_READY)
    {
      /* reduce nready, as destroying a ready coro effectively unreadies it */
      /* alternative: look through all ready queues and remove the coro */
      --coro_nready;
    }
  else
    coro->flags |= CF_READY; /* make sure it is NOT put into the readyqueue */

  if (coro->mainstack
      && coro->mainstack != main_mainstack
      && coro->slot
      && !PL_dirty)
    coro_destruct_perl (aTHX_ coro);

  cctx_destroy (coro->cctx);
  SvREFCNT_dec (coro->startcv);
  SvREFCNT_dec (coro->args);
  SvREFCNT_dec (CORO_THROW);

  if (coro->next) coro->next->prev = coro->prev;
  if (coro->prev) coro->prev->next = coro->next;
  if (coro == coro_first) coro_first = coro->next;

  return 1;
}

static int
coro_state_free (pTHX_ SV *sv, MAGIC *mg)
{
  struct coro *coro = (struct coro *)mg->mg_ptr;
  mg->mg_ptr = 0;

  coro->hv = 0;

  if (--coro->refcnt < 0)
    {
      coro_state_destroy (aTHX_ coro);
      Safefree (coro);
    }

  return 0;
}

static int
coro_state_dup (pTHX_ MAGIC *mg, CLONE_PARAMS *params)
{
  struct coro *coro = (struct coro *)mg->mg_ptr;

  ++coro->refcnt;

  return 0;
}

static MGVTBL coro_state_vtbl = {
  0, 0, 0, 0,
  coro_state_free,
  0,
#ifdef MGf_DUP
  coro_state_dup,
#else
# define MGf_DUP 0
#endif
};

static void
prepare_transfer (pTHX_ struct coro_transfer_args *ta, SV *prev_sv, SV *next_sv)
{
  ta->prev = SvSTATE (prev_sv);
  ta->next = SvSTATE (next_sv);
  TRANSFER_CHECK (*ta);
}

static void
api_transfer (pTHX_ SV *prev_sv, SV *next_sv)
{
  struct coro_transfer_args ta;

  prepare_transfer (aTHX_ &ta, prev_sv, next_sv);
  TRANSFER (ta, 1);
}

/*****************************************************************************/
/* gensub: simple closure generation utility */

#define GENSUB_ARG CvXSUBANY (cv).any_ptr

/* create a closure from XS, returns a code reference */
/* the arg can be accessed via GENSUB_ARG from the callback */
/* the callback must use dXSARGS/XSRETURN */
static SV *
gensub (pTHX_ void (*xsub)(pTHX_ CV *), void *arg)
{
  CV *cv = (CV *)newSV (0);

  sv_upgrade ((SV *)cv, SVt_PVCV);

  CvANON_on (cv);
  CvISXSUB_on (cv);
  CvXSUB (cv) = xsub;
  GENSUB_ARG = arg;

  return newRV_noinc ((SV *)cv);
}

/** Coro ********************************************************************/

INLINE void
coro_enq (pTHX_ struct coro *coro)
{
  struct coro **ready = coro_ready [coro->prio - PRIO_MIN];

  SvREFCNT_inc_NN (coro->hv);

  coro->next_ready = 0;
  *(ready [0] ? &ready [1]->next_ready : &ready [0]) = coro;
  ready [1] = coro;
}

INLINE struct coro *
coro_deq (pTHX)
{
  int prio;

  for (prio = PRIO_MAX - PRIO_MIN + 1; --prio >= 0; )
    {
      struct coro **ready = coro_ready [prio];

      if (ready [0])
        {
          struct coro *coro = ready [0];
          ready [0] = coro->next_ready;
          return coro;
        }
    }

  return 0;
}

static int
api_ready (pTHX_ SV *coro_sv)
{
  struct coro *coro;
  SV *sv_hook;
  void (*xs_hook)(void);

  coro = SvSTATE (coro_sv);

  if (coro->flags & CF_READY)
    return 0;

  coro->flags |= CF_READY;

  sv_hook = coro_nready ? 0 : coro_readyhook;
  xs_hook = coro_nready ? 0 : coroapi.readyhook;

  coro_enq (aTHX_ coro);
  ++coro_nready;

  if (sv_hook)
    {
      dSP;

      ENTER;
      SAVETMPS;

      PUSHMARK (SP);
      PUTBACK;
      call_sv (sv_hook, G_VOID | G_DISCARD);

      FREETMPS;
      LEAVE;
    }

  if (xs_hook)
    xs_hook ();

  return 1;
}

static int
api_is_ready (pTHX_ SV *coro_sv)
{
  return !!(SvSTATE (coro_sv)->flags & CF_READY);
}

/* expects to own a reference to next->hv */
INLINE void
prepare_schedule_to (pTHX_ struct coro_transfer_args *ta, struct coro *next)
{
  SV *prev_sv = SvRV (coro_current);

  ta->prev = SvSTATE_hv (prev_sv);
  ta->next = next;

  TRANSFER_CHECK (*ta);

  SvRV_set (coro_current, (SV *)next->hv);

  free_coro_mortal (aTHX);
  coro_mortal = prev_sv;
}

static void
prepare_schedule (pTHX_ struct coro_transfer_args *ta)
{
  for (;;)
    {
      struct coro *next = coro_deq (aTHX);

      if (expect_true (next))
        {
          /* cannot transfer to destroyed coros, skip and look for next */
          if (expect_false (next->flags & (CF_DESTROYED | CF_SUSPENDED)))
            SvREFCNT_dec (next->hv); /* coro_nready has already been taken care of by destroy */
          else
            {
              next->flags &= ~CF_READY;
              --coro_nready;

              prepare_schedule_to (aTHX_ ta, next);
              break;
            }
        }
      else
        {
          /* nothing to schedule: call the idle handler */
          if (SvROK (sv_idle)
              && SvOBJECT (SvRV (sv_idle)))
            {
              ++coro_nready; /* hack so that api_ready doesn't invoke ready hook */
              api_ready (aTHX_ SvRV (sv_idle));
              --coro_nready;
            }
          else
            {
              dSP;

              ENTER;
              SAVETMPS;

              PUSHMARK (SP);
              PUTBACK;
              call_sv (sv_idle, G_VOID | G_DISCARD);

              FREETMPS;
              LEAVE;
            }
        }
    }
}

INLINE void
prepare_cede (pTHX_ struct coro_transfer_args *ta)
{
  api_ready (aTHX_ coro_current);
  prepare_schedule (aTHX_ ta);
}

INLINE void
prepare_cede_notself (pTHX_ struct coro_transfer_args *ta)
{
  SV *prev = SvRV (coro_current);

  if (coro_nready)
    {
      prepare_schedule (aTHX_ ta);
      api_ready (aTHX_ prev);
    }
  else
    prepare_nop (aTHX_ ta);
}

static void
api_schedule (pTHX)
{
  struct coro_transfer_args ta;

  prepare_schedule (aTHX_ &ta);
  TRANSFER (ta, 1);
}

static void
api_schedule_to (pTHX_ SV *coro_sv)
{
  struct coro_transfer_args ta;
  struct coro *next = SvSTATE (coro_sv);

  SvREFCNT_inc_NN (coro_sv);
  prepare_schedule_to (aTHX_ &ta, next);
}

static int
api_cede (pTHX)
{
  struct coro_transfer_args ta;

  prepare_cede (aTHX_ &ta);

  if (expect_true (ta.prev != ta.next))
    {
      TRANSFER (ta, 1);
      return 1;
    }
  else
    return 0;
}

static int
api_cede_notself (pTHX)
{
  if (coro_nready)
    {
      struct coro_transfer_args ta;

      prepare_cede_notself (aTHX_ &ta);
      TRANSFER (ta, 1);
      return 1;
    }
  else
    return 0;
}

static void
api_trace (pTHX_ SV *coro_sv, int flags)
{
  struct coro *coro = SvSTATE (coro_sv);

  if (coro->flags & CF_RUNNING)
    croak ("cannot enable tracing on a running coroutine, caught");

  if (flags & CC_TRACE)
    {
      if (!coro->cctx)
        coro->cctx = cctx_new_run ();
      else if (!(coro->cctx->flags & CC_TRACE))
        croak ("cannot enable tracing on coroutine with custom stack, caught");

      coro->cctx->flags |= CC_NOREUSE | (flags & (CC_TRACE | CC_TRACE_ALL));
    }
  else if (coro->cctx && coro->cctx->flags & CC_TRACE)
    {
      coro->cctx->flags &= ~(CC_TRACE | CC_TRACE_ALL);

      if (coro->flags & CF_RUNNING)
        PL_runops = RUNOPS_DEFAULT;
      else
        coro->slot->runops = RUNOPS_DEFAULT;
    }
}

static void
coro_call_on_destroy (pTHX_ struct coro *coro)
{
  SV **on_destroyp = hv_fetch (coro->hv, "_on_destroy", sizeof ("_on_destroy") - 1, 0);
  SV **statusp     = hv_fetch (coro->hv, "_status", sizeof ("_status") - 1, 0);

  if (on_destroyp)
    {
      AV *on_destroy = (AV *)SvRV (*on_destroyp);

      while (AvFILLp (on_destroy) >= 0)
        {
          dSP; /* don't disturb outer sp */
          SV *cb = av_pop (on_destroy);

          PUSHMARK (SP);

          if (statusp)
            {
              int i;
              AV *status = (AV *)SvRV (*statusp);
              EXTEND (SP, AvFILLp (status) + 1);

              for (i = 0; i <= AvFILLp (status); ++i)
                PUSHs (AvARRAY (status)[i]);
            }

          PUTBACK;
          call_sv (sv_2mortal (cb), G_VOID | G_DISCARD);
        }
    }
}

static void
slf_init_terminate (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  int i;
  HV *hv = (HV *)SvRV (coro_current);
  AV *av = newAV ();

  av_extend (av, items - 1);
  for (i = 0; i < items; ++i)
    av_push (av, SvREFCNT_inc_NN (arg [i]));

  hv_store (hv, "_status", sizeof ("_status") - 1, newRV_noinc ((SV *)av), 0);

  av_push (av_destroy, (SV *)newRV_inc ((SV *)hv)); /* RVinc for perl */
  api_ready (aTHX_ sv_manager);

  frame->prepare = prepare_schedule;
  frame->check   = slf_check_repeat;

  /* as a minor optimisation, we could unwind all stacks here */
  /* but that puts extra pressure on pp_slf, and is not worth much */
  /*coro_unwind_stacks (aTHX);*/
}

/*****************************************************************************/
/* async pool handler */

static int
slf_check_pool_handler (pTHX_ struct CoroSLF *frame)
{
  HV *hv = (HV *)SvRV (coro_current);
  struct coro *coro = (struct coro *)frame->data;

  if (!coro->invoke_cb)
    return 1; /* loop till we have invoke */
  else
    {
      hv_store (hv, "desc", sizeof ("desc") - 1,
                newSVpvn ("[async_pool]", sizeof ("[async_pool]") - 1), 0);

      coro->saved_deffh = SvREFCNT_inc_NN ((SV *)PL_defoutgv);

      {
        dSP;
        XPUSHs (sv_2mortal (coro->invoke_cb)); coro->invoke_cb = 0;
        PUTBACK;
      }

      SvREFCNT_dec (GvAV (PL_defgv));
      GvAV (PL_defgv) = coro->invoke_av;
      coro->invoke_av = 0;

      return 0;
    }
}

static void
slf_init_pool_handler (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  HV *hv = (HV *)SvRV (coro_current);
  struct coro *coro = SvSTATE_hv ((SV *)hv);

  if (expect_true (coro->saved_deffh))
    {
      /* subsequent iteration */
      SvREFCNT_dec ((SV *)PL_defoutgv); PL_defoutgv = (GV *)coro->saved_deffh;
      coro->saved_deffh = 0;

      if (coro_rss (aTHX_ coro) > SvUV (sv_pool_rss)
          || av_len (av_async_pool) + 1 >= SvIV (sv_pool_size))
        {
          coro->invoke_cb = SvREFCNT_inc_NN ((SV *)cv_coro_terminate);
          coro->invoke_av = newAV ();

          frame->prepare = prepare_nop;
        }
      else
        {
          av_clear (GvAV (PL_defgv));
          hv_store (hv, "desc", sizeof ("desc") - 1, SvREFCNT_inc_NN (sv_async_pool_idle), 0);

          coro->prio = 0;

          if (coro->cctx && (coro->cctx->flags & CC_TRACE))
            api_trace (aTHX_ coro_current, 0);

          frame->prepare = prepare_schedule;
          av_push (av_async_pool, SvREFCNT_inc (hv));
        }
    }
  else
    {
      /* first iteration, simply fall through */
      frame->prepare = prepare_nop;
    }

  frame->check = slf_check_pool_handler;
  frame->data  = (void *)coro;
}

/*****************************************************************************/
/* rouse callback */

#define CORO_MAGIC_type_rouse PERL_MAGIC_ext

static void
coro_rouse_callback (pTHX_ CV *cv)
{
  dXSARGS;
  SV *data = (SV *)GENSUB_ARG;

  if (SvTYPE (SvRV (data)) != SVt_PVAV)
    {
      /* first call, set args */
      SV *coro = SvRV (data);
      AV *av = newAV ();

      SvRV_set (data, (SV *)av);

      /* better take a full copy of the arguments */
      while (items--)
        av_store (av, items, newSVsv (ST (items)));

      api_ready (aTHX_ coro);
      SvREFCNT_dec (coro);
    }

  XSRETURN_EMPTY;
}

static int
slf_check_rouse_wait (pTHX_ struct CoroSLF *frame)
{
  SV *data = (SV *)frame->data;
  
  if (CORO_THROW)
    return 0;

  if (SvTYPE (SvRV (data)) != SVt_PVAV)
    return 1;

  /* now push all results on the stack */
  {
    dSP;
    AV *av = (AV *)SvRV (data);
    int i;

    EXTEND (SP, AvFILLp (av) + 1);
    for (i = 0; i <= AvFILLp (av); ++i)
      PUSHs (sv_2mortal (AvARRAY (av)[i]));

    /* we have stolen the elements, so set length to zero and free */
    AvFILLp (av) = -1;
    av_undef (av);

    PUTBACK;
  }

  return 0;
}

static void
slf_init_rouse_wait (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  SV *cb;

  if (items)
    cb = arg [0];
  else
    {
      struct coro *coro = SvSTATE_current;

      if (!coro->rouse_cb)
        croak ("Coro::rouse_wait called without rouse callback, and no default rouse callback found either,");

      cb = sv_2mortal (coro->rouse_cb);
      coro->rouse_cb = 0;
    }

  if (!SvROK (cb)
      || SvTYPE (SvRV (cb)) != SVt_PVCV 
      || CvXSUB ((CV *)SvRV (cb)) != coro_rouse_callback)
    croak ("Coro::rouse_wait called with illegal callback argument,");

  {
    CV *cv = (CV *)SvRV (cb); /* for GENSUB_ARG */
    SV *data = (SV *)GENSUB_ARG;

    frame->data    = (void *)data;
    frame->prepare = SvTYPE (SvRV (data)) == SVt_PVAV ? prepare_nop : prepare_schedule;
    frame->check   = slf_check_rouse_wait;
  }
}

static SV *
coro_new_rouse_cb (pTHX)
{
  HV *hv = (HV *)SvRV (coro_current);
  struct coro *coro = SvSTATE_hv (hv);
  SV *data = newRV_inc ((SV *)hv);
  SV *cb = gensub (aTHX_ coro_rouse_callback, (void *)data);

  sv_magicext (SvRV (cb), data, CORO_MAGIC_type_rouse, 0, 0, 0);
  SvREFCNT_dec (data); /* magicext increases the refcount */

  SvREFCNT_dec (coro->rouse_cb);
  coro->rouse_cb = SvREFCNT_inc_NN (cb);

  return cb;
}

/*****************************************************************************/
/* schedule-like-function opcode (SLF) */

static UNOP slf_restore; /* restore stack as entersub did, for first-re-run */
static const CV *slf_cv;
static SV **slf_argv;
static int slf_argc, slf_arga; /* count, allocated */
static I32 slf_ax; /* top of stack, for restore */

/* this restores the stack in the case we patched the entersub, to */
/* recreate the stack frame as perl will on following calls */
/* since entersub cleared the stack */
static OP *
pp_restore (pTHX)
{
  int i;
  SV **SP = PL_stack_base + slf_ax;

  PUSHMARK (SP);

  EXTEND (SP, slf_argc + 1);

  for (i = 0; i < slf_argc; ++i)
    PUSHs (sv_2mortal (slf_argv [i]));

  PUSHs ((SV *)CvGV (slf_cv));

  RETURNOP (slf_restore.op_first);
}

static void
slf_prepare_transfer (pTHX_ struct coro_transfer_args *ta)
{
  SV **arg = (SV **)slf_frame.data;

  prepare_transfer (aTHX_ ta, arg [0], arg [1]);
}

static void
slf_init_transfer (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  if (items != 2)
    croak ("Coro::State::transfer (prev, next) expects two arguments, not %d,", items);

  frame->prepare = slf_prepare_transfer;
  frame->check   = slf_check_nop;
  frame->data    = (void *)arg; /* let's hope it will stay valid */
}

static void
slf_init_schedule (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  frame->prepare = prepare_schedule;
  frame->check   = slf_check_nop;
}

static void
slf_prepare_schedule_to (pTHX_ struct coro_transfer_args *ta)
{
  struct coro *next = (struct coro *)slf_frame.data;

  SvREFCNT_inc_NN (next->hv);
  prepare_schedule_to (aTHX_ ta, next);
}

static void
slf_init_schedule_to (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  if (!items)
    croak ("Coro::schedule_to expects a coroutine argument, caught");

  frame->data    = (void *)SvSTATE (arg [0]);
  frame->prepare = slf_prepare_schedule_to;
  frame->check   = slf_check_nop;
}

static void
slf_init_cede_to (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  api_ready (aTHX_ SvRV (coro_current));

  slf_init_schedule_to (aTHX_ frame, cv, arg, items);
}

static void
slf_init_cede (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  frame->prepare = prepare_cede;
  frame->check   = slf_check_nop;
}

static void
slf_init_cede_notself (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  frame->prepare = prepare_cede_notself;
  frame->check   = slf_check_nop;
}

/*
 * these not obviously related functions are all rolled into one
 * function to increase chances that they all will call transfer with the same
 * stack offset
 * SLF stands for "schedule-like-function".
 */
static OP *
pp_slf (pTHX)
{
  I32 checkmark; /* mark SP to see how many elements check has pushed */

  /* set up the slf frame, unless it has already been set-up */
  /* the latter happens when a new coro has been started */
  /* or when a new cctx was attached to an existing coroutine */
  if (expect_true (!slf_frame.prepare))
    {
      /* first iteration */
      dSP;
      SV **arg = PL_stack_base + TOPMARK + 1;
      int items = SP - arg; /* args without function object */
      SV *gv = *sp;

      /* do a quick consistency check on the "function" object, and if it isn't */
      /* for us, divert to the real entersub */
      if (SvTYPE (gv) != SVt_PVGV
          || !GvCV (gv)
          || !(CvFLAGS (GvCV (gv)) & CVf_SLF))
        return PL_ppaddr[OP_ENTERSUB](aTHX);

      if (!(PL_op->op_flags & OPf_STACKED))
        {
          /* ampersand-form of call, use @_ instead of stack */
          AV *av = GvAV (PL_defgv);
          arg = AvARRAY (av);
          items = AvFILLp (av) + 1;
        }

      /* now call the init function, which needs to set up slf_frame */
      ((coro_slf_cb)CvXSUBANY (GvCV (gv)).any_ptr)
        (aTHX_ &slf_frame, GvCV (gv), arg, items);

      /* pop args */
      SP = PL_stack_base + POPMARK;

      PUTBACK;
    }

  /* now that we have a slf_frame, interpret it! */
  /* we use a callback system not to make the code needlessly */
  /* complicated, but so we can run multiple perl coros from one cctx */

  do
    {
      struct coro_transfer_args ta;

      slf_frame.prepare (aTHX_ &ta);
      TRANSFER (ta, 0);

      checkmark = PL_stack_sp - PL_stack_base;
    }
  while (slf_frame.check (aTHX_ &slf_frame));

  slf_frame.prepare = 0; /* invalidate the frame, we are done processing it */

  /* exception handling */
  if (expect_false (CORO_THROW))
    {
      SV *exception = sv_2mortal (CORO_THROW);

      CORO_THROW = 0;
      sv_setsv (ERRSV, exception);
      croak (0);
    }

  /* return value handling - mostly like entersub */
  /* make sure we put something on the stack in scalar context */
  if (GIMME_V == G_SCALAR)
    {
      dSP;
      SV **bot = PL_stack_base + checkmark;

      if (sp == bot) /* too few, push undef */
        bot [1] = &PL_sv_undef;
      else if (sp != bot + 1) /* too many, take last one */
        bot [1] = *sp;

      SP = bot + 1;

      PUTBACK;
    }

  return NORMAL;
}

static void
api_execute_slf (pTHX_ CV *cv, coro_slf_cb init_cb, I32 ax)
{
  int i;
  SV **arg = PL_stack_base + ax;
  int items = PL_stack_sp - arg + 1;

  assert (("FATAL: SLF call with illegal CV value", !CvANON (cv)));

  if (PL_op->op_ppaddr != PL_ppaddr [OP_ENTERSUB]
      && PL_op->op_ppaddr != pp_slf)
    croak ("FATAL: Coro SLF calls can only be made normally, not via goto or any other means, caught");

  CvFLAGS (cv) |= CVf_SLF;
  CvXSUBANY (cv).any_ptr = (void *)init_cb;
  slf_cv = cv;

  /* we patch the op, and then re-run the whole call */
  /* we have to put the same argument on the stack for this to work */
  /* and this will be done by pp_restore */
  slf_restore.op_next   = (OP *)&slf_restore;
  slf_restore.op_type   = OP_CUSTOM;
  slf_restore.op_ppaddr = pp_restore;
  slf_restore.op_first  = PL_op;

  slf_ax   = ax - 1; /* undo the ax++ inside dAXMARK */

  if (PL_op->op_flags & OPf_STACKED)
    {
      if (items > slf_arga)
        {
          slf_arga = items;
          free (slf_argv);
          slf_argv = malloc (slf_arga * sizeof (SV *));
        }

      slf_argc = items;

      for (i = 0; i < items; ++i)
        slf_argv [i] = SvREFCNT_inc (arg [i]);
    }
  else
    slf_argc = 0;

  PL_op->op_ppaddr  = pp_slf;
  /*PL_op->op_type    = OP_CUSTOM; /* we do behave like entersub still */

  PL_op = (OP *)&slf_restore;
}

/*****************************************************************************/
/* dynamic wind */

static void
on_enterleave_call (pTHX_ SV *cb)
{
  dSP;

  PUSHSTACK;

  PUSHMARK (SP);
  PUTBACK;
  call_sv (cb, G_VOID | G_DISCARD);
  SPAGAIN;

  POPSTACK;
}

static SV *
coro_avp_pop_and_free (pTHX_ AV **avp)
{
  AV *av = *avp;
  SV *res = av_pop (av);

  if (AvFILLp (av) < 0)
    {
      *avp = 0;
      SvREFCNT_dec (av);
    }

  return res;
}

static void
coro_pop_on_enter (pTHX_ void *coro)
{
  SV *cb = coro_avp_pop_and_free (aTHX_ &((struct coro *)coro)->on_enter);
  SvREFCNT_dec (cb);
}

static void
coro_pop_on_leave (pTHX_ void *coro)
{
  SV *cb = coro_avp_pop_and_free (aTHX_ &((struct coro *)coro)->on_leave);
  on_enterleave_call (aTHX_ sv_2mortal (cb));
}

/*****************************************************************************/
/* PerlIO::cede */

typedef struct
{
  PerlIOBuf base;
  NV next, every;
} PerlIOCede;

static IV
PerlIOCede_pushed (pTHX_ PerlIO *f, const char *mode, SV *arg, PerlIO_funcs *tab)
{
  PerlIOCede *self = PerlIOSelf (f, PerlIOCede);

  self->every = SvCUR (arg) ? SvNV (arg) : 0.01;
  self->next  = nvtime () + self->every;

  return PerlIOBuf_pushed (aTHX_ f, mode, Nullsv, tab);
}

static SV *
PerlIOCede_getarg (pTHX_ PerlIO *f, CLONE_PARAMS *param, int flags)
{
  PerlIOCede *self = PerlIOSelf (f, PerlIOCede);

  return newSVnv (self->every);
}

static IV
PerlIOCede_flush (pTHX_ PerlIO *f)
{
  PerlIOCede *self = PerlIOSelf (f, PerlIOCede);
  double now = nvtime ();

  if (now >= self->next)
    {
      api_cede (aTHX);
      self->next = now + self->every;
    }

  return PerlIOBuf_flush (aTHX_ f);
}

static PerlIO_funcs PerlIO_cede =
{
  sizeof(PerlIO_funcs),
  "cede",
  sizeof(PerlIOCede),
  PERLIO_K_DESTRUCT | PERLIO_K_RAW,
  PerlIOCede_pushed,
  PerlIOBuf_popped,
  PerlIOBuf_open,
  PerlIOBase_binmode,
  PerlIOCede_getarg,
  PerlIOBase_fileno,
  PerlIOBuf_dup,
  PerlIOBuf_read,
  PerlIOBuf_unread,
  PerlIOBuf_write,
  PerlIOBuf_seek,
  PerlIOBuf_tell,
  PerlIOBuf_close,
  PerlIOCede_flush,
  PerlIOBuf_fill,
  PerlIOBase_eof,
  PerlIOBase_error,
  PerlIOBase_clearerr,
  PerlIOBase_setlinebuf,
  PerlIOBuf_get_base,
  PerlIOBuf_bufsiz,
  PerlIOBuf_get_ptr,
  PerlIOBuf_get_cnt,
  PerlIOBuf_set_ptrcnt,
};

/*****************************************************************************/
/* Coro::Semaphore & Coro::Signal */

static SV *
coro_waitarray_new (pTHX_ int count)
{
  /* a waitarray=semaphore contains a counter IV in $sem->[0] and any waiters after that */
  AV *av = newAV ();
  SV **ary;

  /* unfortunately, building manually saves memory */
  Newx (ary, 2, SV *);
  AvALLOC (av) = ary;
#if PERL_VERSION_ATLEAST (5,10,0)
  AvARRAY (av) = ary;
#else
  /* 5.8.8 needs this syntax instead of AvARRAY = ary, yet */
  /* -DDEBUGGING flags this as a bug, despite it perfectly working */
  SvPVX ((SV *)av) = (char *)ary;
#endif
  AvMAX   (av) = 1;
  AvFILLp (av) = 0;
  ary [0] = newSViv (count);

  return newRV_noinc ((SV *)av);
}

/* semaphore */

static void
coro_semaphore_adjust (pTHX_ AV *av, IV adjust)
{
  SV *count_sv = AvARRAY (av)[0];
  IV count = SvIVX (count_sv);

  count += adjust;
  SvIVX (count_sv) = count;

  /* now wake up as many waiters as are expected to lock */
  while (count > 0 && AvFILLp (av) > 0)
    {
      SV *cb;

      /* swap first two elements so we can shift a waiter */
      AvARRAY (av)[0] = AvARRAY (av)[1];
      AvARRAY (av)[1] = count_sv;
      cb = av_shift (av);

      if (SvOBJECT (cb))
        {
          api_ready (aTHX_ cb);
          --count;
        }
      else if (SvTYPE (cb) == SVt_PVCV)
        {
          dSP;
          PUSHMARK (SP);
          XPUSHs (sv_2mortal (newRV_inc ((SV *)av)));
          PUTBACK;
          call_sv (cb, G_VOID | G_DISCARD | G_EVAL | G_KEEPERR);
        }

      SvREFCNT_dec (cb);
    }
}

static void
coro_semaphore_on_destroy (pTHX_ struct coro *coro)
{
  /* call $sem->adjust (0) to possibly wake up some other waiters */
  coro_semaphore_adjust (aTHX_ (AV *)coro->slf_frame.data, 0);
}

static int
slf_check_semaphore_down_or_wait (pTHX_ struct CoroSLF *frame, int acquire)
{
  AV *av = (AV *)frame->data;
  SV *count_sv = AvARRAY (av)[0];

  /* if we are about to throw, don't actually acquire the lock, just throw */
  if (CORO_THROW)
    return 0;
  else if (SvIVX (count_sv) > 0)
    {
      SvSTATE_current->on_destroy = 0;

      if (acquire)
        SvIVX (count_sv) = SvIVX (count_sv) - 1;
      else
        coro_semaphore_adjust (aTHX_ av, 0);

      return 0;
    }
  else
    {
      int i;
      /* if we were woken up but can't down, we look through the whole */
      /* waiters list and only add us if we aren't in there already */
      /* this avoids some degenerate memory usage cases */

      for (i = 1; i <= AvFILLp (av); ++i)
        if (AvARRAY (av)[i] == SvRV (coro_current))
          return 1;

      av_push (av, SvREFCNT_inc (SvRV (coro_current)));
      return 1;
    }
}

static int
slf_check_semaphore_down (pTHX_ struct CoroSLF *frame)
{
  return slf_check_semaphore_down_or_wait (aTHX_ frame, 1);
}

static int
slf_check_semaphore_wait (pTHX_ struct CoroSLF *frame)
{
  return slf_check_semaphore_down_or_wait (aTHX_ frame, 0);
}

static void
slf_init_semaphore_down_or_wait (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  AV *av = (AV *)SvRV (arg [0]);

  if (SvIVX (AvARRAY (av)[0]) > 0)
    {
      frame->data    = (void *)av;
      frame->prepare = prepare_nop;
    }
  else
    {
      av_push (av, SvREFCNT_inc (SvRV (coro_current)));

      frame->data    = (void *)sv_2mortal (SvREFCNT_inc ((SV *)av));
      frame->prepare = prepare_schedule;

      /* to avoid race conditions when a woken-up coro gets terminated */
      /* we arrange for a temporary on_destroy that calls adjust (0) */
      SvSTATE_current->on_destroy = coro_semaphore_on_destroy;
    }
}

static void
slf_init_semaphore_down (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  slf_init_semaphore_down_or_wait (aTHX_ frame, cv, arg, items);
  frame->check = slf_check_semaphore_down;
}

static void
slf_init_semaphore_wait (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  if (items >= 2)
    {
      /* callback form */
      AV *av = (AV *)SvRV (arg [0]);
      CV *cb_cv = coro_sv_2cv (aTHX_ arg [1]);

      av_push (av, (SV *)SvREFCNT_inc_NN (cb_cv));

      if (SvIVX (AvARRAY (av)[0]) > 0)
        coro_semaphore_adjust (aTHX_ av, 0);

      frame->prepare = prepare_nop;
      frame->check   = slf_check_nop;
    }
  else
    {
      slf_init_semaphore_down_or_wait (aTHX_ frame, cv, arg, items);
      frame->check = slf_check_semaphore_wait;
    }
}

/* signal */

static void
coro_signal_wake (pTHX_ AV *av, int count)
{
  SvIVX (AvARRAY (av)[0]) = 0;

  /* now signal count waiters */
  while (count > 0 && AvFILLp (av) > 0)
    {
      SV *cb;

      /* swap first two elements so we can shift a waiter */
      cb = AvARRAY (av)[0];
      AvARRAY (av)[0] = AvARRAY (av)[1];
      AvARRAY (av)[1] = cb;

      cb = av_shift (av);

      api_ready (aTHX_ cb);
      sv_setiv (cb, 0); /* signal waiter */
      SvREFCNT_dec (cb);

      --count;
    }
}

static int
slf_check_signal_wait (pTHX_ struct CoroSLF *frame)
{
  /* if we are about to throw, also stop waiting */
  return SvROK ((SV *)frame->data) && !CORO_THROW;
}

static void
slf_init_signal_wait (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  AV *av = (AV *)SvRV (arg [0]);

  if (SvIVX (AvARRAY (av)[0]))
    {
      SvIVX (AvARRAY (av)[0]) = 0;
      frame->prepare = prepare_nop;
      frame->check   = slf_check_nop;
    }
  else
    {
      SV *waiter = newRV_inc (SvRV (coro_current)); /* owned by signal av */

      av_push (av, waiter);

      frame->data    = (void *)sv_2mortal (SvREFCNT_inc_NN (waiter)); /* owned by process */
      frame->prepare = prepare_schedule;
      frame->check   = slf_check_signal_wait;
    }
}

/*****************************************************************************/
/* Coro::AIO */

#define CORO_MAGIC_type_aio PERL_MAGIC_ext

/* helper storage struct */
struct io_state
{
  int errorno;
  I32 laststype; /* U16 in 5.10.0 */
  int laststatval;
  Stat_t statcache;
};

static void
coro_aio_callback (pTHX_ CV *cv)
{
  dXSARGS;
  AV *state = (AV *)GENSUB_ARG;
  SV *coro = av_pop (state);
  SV *data_sv = newSV (sizeof (struct io_state));

  av_extend (state, items - 1);

  sv_upgrade (data_sv, SVt_PV);
  SvCUR_set (data_sv, sizeof (struct io_state));
  SvPOK_only (data_sv);

  {
    struct io_state *data = (struct io_state *)SvPVX (data_sv);

    data->errorno     = errno;
    data->laststype   = PL_laststype;
    data->laststatval = PL_laststatval;
    data->statcache   = PL_statcache;
  }

  /* now build the result vector out of all the parameters and the data_sv */
  {
    int i;

    for (i = 0; i < items; ++i)
      av_push (state, SvREFCNT_inc_NN (ST (i)));
  }

  av_push (state, data_sv);

  api_ready (aTHX_ coro);
  SvREFCNT_dec (coro);
  SvREFCNT_dec ((AV *)state);
}

static int
slf_check_aio_req (pTHX_ struct CoroSLF *frame)
{
  AV *state = (AV *)frame->data;

  /* if we are about to throw, return early */
  /* this does not cancel the aio request, but at least */
  /* it quickly returns */
  if (CORO_THROW)
    return 0;

  /* one element that is an RV? repeat! */
  if (AvFILLp (state) == 0 && SvROK (AvARRAY (state)[0]))
    return 1;

  /* restore status */
  {
    SV *data_sv = av_pop (state);
    struct io_state *data = (struct io_state *)SvPVX (data_sv);

    errno          = data->errorno;
    PL_laststype   = data->laststype;
    PL_laststatval = data->laststatval;
    PL_statcache   = data->statcache;

    SvREFCNT_dec (data_sv);
  }

  /* push result values */
  {
    dSP;
    int i;

    EXTEND (SP, AvFILLp (state) + 1);
    for (i = 0; i <= AvFILLp (state); ++i)
      PUSHs (sv_2mortal (SvREFCNT_inc_NN (AvARRAY (state)[i])));

    PUTBACK;
  }

  return 0;
}

static void
slf_init_aio_req (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items)
{
  AV *state = (AV *)sv_2mortal ((SV *)newAV ());
  SV *coro_hv = SvRV (coro_current);
  struct coro *coro = SvSTATE_hv (coro_hv);

  /* put our coroutine id on the state arg */
  av_push (state, SvREFCNT_inc_NN (coro_hv));

  /* first see whether we have a non-zero priority and set it as AIO prio */
  if (coro->prio)
    {
      dSP;

      static SV *prio_cv;
      static SV *prio_sv;

      if (expect_false (!prio_cv))
        {
          prio_cv = (SV *)get_cv ("IO::AIO::aioreq_pri", 0);
          prio_sv = newSViv (0);
        }

      PUSHMARK (SP);
      sv_setiv (prio_sv, coro->prio);
      XPUSHs (prio_sv);

      PUTBACK;
      call_sv (prio_cv, G_VOID | G_DISCARD);
    }

  /* now call the original request */
  {
    dSP;
    CV *req = (CV *)CORO_MAGIC_NN ((SV *)cv, CORO_MAGIC_type_aio)->mg_obj;
    int i;

    PUSHMARK (SP);

    /* first push all args to the stack */
    EXTEND (SP, items + 1);

    for (i = 0; i < items; ++i)
      PUSHs (arg [i]);

    /* now push the callback closure */
    PUSHs (sv_2mortal (gensub (aTHX_ coro_aio_callback, (void *)SvREFCNT_inc_NN ((SV *)state))));

    /* now call the AIO function - we assume our request is uncancelable */
    PUTBACK;
    call_sv ((SV *)req, G_VOID | G_DISCARD);
  }

  /* now that the requets is going, we loop toll we have a result */
  frame->data    = (void *)state;
  frame->prepare = prepare_schedule;
  frame->check   = slf_check_aio_req;
}

static void
coro_aio_req_xs (pTHX_ CV *cv)
{
  dXSARGS;

  CORO_EXECUTE_SLF_XS (slf_init_aio_req);

  XSRETURN_EMPTY;
}

/*****************************************************************************/

#if CORO_CLONE
# include "clone.c"
#endif

MODULE = Coro::State                PACKAGE = Coro::State	PREFIX = api_

PROTOTYPES: DISABLE

BOOT:
{
#ifdef USE_ITHREADS
# if CORO_PTHREAD
        coro_thx = PERL_GET_CONTEXT;
# endif
#endif
        BOOT_PAGESIZE;

        cctx_current = cctx_new_empty ();

        irsgv    = gv_fetchpv ("/"     , GV_ADD|GV_NOTQUAL, SVt_PV);
        stdoutgv = gv_fetchpv ("STDOUT", GV_ADD|GV_NOTQUAL, SVt_PVIO);

        orig_sigelem_get = PL_vtbl_sigelem.svt_get;   PL_vtbl_sigelem.svt_get   = coro_sigelem_get;
        orig_sigelem_set = PL_vtbl_sigelem.svt_set;   PL_vtbl_sigelem.svt_set   = coro_sigelem_set;
        orig_sigelem_clr = PL_vtbl_sigelem.svt_clear; PL_vtbl_sigelem.svt_clear = coro_sigelem_clr;

        hv_sig      = coro_get_hv (aTHX_ "SIG", TRUE);
        rv_diehook  = newRV_inc ((SV *)gv_fetchpv ("Coro::State::diehook" , 0, SVt_PVCV));
        rv_warnhook = newRV_inc ((SV *)gv_fetchpv ("Coro::State::warnhook", 0, SVt_PVCV));

	coro_state_stash = gv_stashpv ("Coro::State", TRUE);

        newCONSTSUB (coro_state_stash, "CC_TRACE"     , newSViv (CC_TRACE));
        newCONSTSUB (coro_state_stash, "CC_TRACE_SUB" , newSViv (CC_TRACE_SUB));
        newCONSTSUB (coro_state_stash, "CC_TRACE_LINE", newSViv (CC_TRACE_LINE));
        newCONSTSUB (coro_state_stash, "CC_TRACE_ALL" , newSViv (CC_TRACE_ALL));

        main_mainstack = PL_mainstack;
        main_top_env   = PL_top_env;

        while (main_top_env->je_prev)
          main_top_env = main_top_env->je_prev;

        {
          SV *slf = sv_2mortal (newSViv (PTR2IV (pp_slf)));

          if (!PL_custom_op_names) PL_custom_op_names = newHV ();
          hv_store_ent (PL_custom_op_names, slf, newSVpv ("coro_slf", 0), 0);

          if (!PL_custom_op_descs) PL_custom_op_descs = newHV ();
          hv_store_ent (PL_custom_op_descs, slf, newSVpv ("coro schedule like function", 0), 0);
        }

        coroapi.ver         = CORO_API_VERSION;
        coroapi.rev         = CORO_API_REVISION;

        coroapi.transfer    = api_transfer;

        coroapi.sv_state             = SvSTATE_;
        coroapi.execute_slf          = api_execute_slf;
        coroapi.prepare_nop          = prepare_nop;
        coroapi.prepare_schedule     = prepare_schedule;
        coroapi.prepare_cede         = prepare_cede;
        coroapi.prepare_cede_notself = prepare_cede_notself;

        {
          SV **svp = hv_fetch (PL_modglobal, "Time::NVtime", 12, 0);

          if (!svp)          croak ("Time::HiRes is required");
          if (!SvIOK (*svp)) croak ("Time::NVtime isn't a function pointer");

          nvtime = INT2PTR (double (*)(), SvIV (*svp));
        }

        assert (("PRIO_NORMAL must be 0", !PRIO_NORMAL));
}

SV *
new (char *klass, ...)
	ALIAS:
        Coro::new = 1
        CODE:
{
        struct coro *coro;
        MAGIC *mg;
        HV *hv;
        CV *cb;
        int i;

        if (items > 1)
          {
            cb = coro_sv_2cv (aTHX_ ST (1));

            if (!ix)
              {
                if (CvISXSUB (cb))
                  croak ("Coro::State doesn't support XS functions as coroutine start, caught");

                if (!CvROOT (cb))
                  croak ("Coro::State doesn't support autoloaded or undefined functions as coroutine start, caught");
              }
          }

        Newz (0, coro, 1, struct coro);
        coro->args  = newAV ();
        coro->flags = CF_NEW;

        if (coro_first) coro_first->prev = coro;
        coro->next = coro_first;
        coro_first = coro;

        coro->hv = hv = newHV ();
        mg = sv_magicext ((SV *)hv, 0, CORO_MAGIC_type_state, &coro_state_vtbl, (char *)coro, 0);
        mg->mg_flags |= MGf_DUP;
        RETVAL = sv_bless (newRV_noinc ((SV *)hv), gv_stashpv (klass, 1));

        if (items > 1)
          {
            av_extend (coro->args, items - 1 + ix - 1);

            if (ix)
              {
                av_push (coro->args, SvREFCNT_inc_NN ((SV *)cb));
                cb = cv_coro_run;
              }

            coro->startcv = (CV *)SvREFCNT_inc_NN ((SV *)cb);

            for (i = 2; i < items; i++)
              av_push (coro->args, newSVsv (ST (i)));
          }
}
        OUTPUT:
        RETVAL

void
transfer (...)
        PROTOTYPE: $$
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_transfer);

bool
_destroy (SV *coro_sv)
	CODE:
	RETVAL = coro_state_destroy (aTHX_ SvSTATE (coro_sv));
	OUTPUT:
        RETVAL

void
_exit (int code)
        PROTOTYPE: $
	CODE:
	_exit (code);

SV *
clone (Coro::State coro)
	CODE:
{
#if CORO_CLONE
        struct coro *ncoro = coro_clone (aTHX_ coro);
        MAGIC *mg;
        /* TODO: too much duplication */
        ncoro->hv = newHV ();
        mg = sv_magicext ((SV *)ncoro->hv, 0, CORO_MAGIC_type_state, &coro_state_vtbl, (char *)ncoro, 0);
        mg->mg_flags |= MGf_DUP;
        RETVAL = sv_bless (newRV_noinc ((SV *)ncoro->hv), SvSTASH (coro->hv));
#else
        croak ("Coro::State->clone has not been configured into this installation of Coro, realised");
#endif
}
	OUTPUT:
        RETVAL

int
cctx_stacksize (int new_stacksize = 0)
	PROTOTYPE: ;$
	CODE:
        RETVAL = cctx_stacksize;
        if (new_stacksize)
          {
            cctx_stacksize = new_stacksize;
            ++cctx_gen;
          }
	OUTPUT:
        RETVAL

int
cctx_max_idle (int max_idle = 0)
	PROTOTYPE: ;$
	CODE:
        RETVAL = cctx_max_idle;
        if (max_idle > 1)
          cctx_max_idle = max_idle;
	OUTPUT:
        RETVAL

int
cctx_count ()
	PROTOTYPE:
	CODE:
        RETVAL = cctx_count;
	OUTPUT:
        RETVAL

int
cctx_idle ()
	PROTOTYPE:
	CODE:
        RETVAL = cctx_idle;
	OUTPUT:
        RETVAL

void
list ()
	PROTOTYPE:
	PPCODE:
{
  	struct coro *coro;
        for (coro = coro_first; coro; coro = coro->next)
          if (coro->hv)
            XPUSHs (sv_2mortal (newRV_inc ((SV *)coro->hv)));
}

void
call (Coro::State coro, SV *coderef)
	ALIAS:
        eval = 1
	CODE:
{
        if (coro->mainstack && ((coro->flags & CF_RUNNING) || coro->slot))
          {
            struct coro *current = SvSTATE_current;

            if (current != coro)
              {
                PUTBACK;
                save_perl (aTHX_ current);
                load_perl (aTHX_ coro);
                SPAGAIN;
              }

            PUSHSTACK;

            PUSHMARK (SP);
            PUTBACK;

            if (ix)
              eval_sv (coderef, 0);
            else
              call_sv (coderef, G_KEEPERR | G_EVAL | G_VOID | G_DISCARD);

            POPSTACK;
            SPAGAIN;

            if (current != coro)
              {
                PUTBACK;
                save_perl (aTHX_ coro);
                load_perl (aTHX_ current);
                SPAGAIN;
              }
          }
}

SV *
is_ready (Coro::State coro)
        PROTOTYPE: $
        ALIAS:
        is_ready     = CF_READY
        is_running   = CF_RUNNING
        is_new       = CF_NEW
        is_destroyed = CF_DESTROYED
        is_suspended = CF_SUSPENDED
	CODE:
        RETVAL = boolSV (coro->flags & ix);
	OUTPUT:
        RETVAL

void
throw (Coro::State self, SV *throw = &PL_sv_undef)
	PROTOTYPE: $;$
        CODE:
{
	struct coro *current = SvSTATE_current;
	SV **throwp = self == current ? &CORO_THROW : &self->except;
        SvREFCNT_dec (*throwp);
        *throwp = SvOK (throw) ? newSVsv (throw) : 0;
}

void
api_trace (SV *coro, int flags = CC_TRACE | CC_TRACE_SUB)
	PROTOTYPE: $;$
	C_ARGS: aTHX_ coro, flags

SV *
has_cctx (Coro::State coro)
        PROTOTYPE: $
	CODE:
        /* maybe manage the running flag differently */
        RETVAL = boolSV (!!coro->cctx || (coro->flags & CF_RUNNING));
	OUTPUT:
        RETVAL

int
is_traced (Coro::State coro)
        PROTOTYPE: $
	CODE:
        RETVAL = (coro->cctx ? coro->cctx->flags : 0) & CC_TRACE_ALL;
	OUTPUT:
        RETVAL

UV
rss (Coro::State coro)
        PROTOTYPE: $
        ALIAS:
        usecount = 1
        CODE:
        switch (ix)
	  {
            case 0: RETVAL = coro_rss (aTHX_ coro); break;
            case 1: RETVAL = coro->usecount;        break;
          }
	OUTPUT:
        RETVAL

void
force_cctx ()
	PROTOTYPE:
	CODE:
        cctx_current->idle_sp = 0;

void
swap_defsv (Coro::State self)
	PROTOTYPE: $
        ALIAS:
        swap_defav = 1
        CODE:
	if (!self->slot)
          croak ("cannot swap state with coroutine that has no saved state,");
        else
          {
            SV **src = ix ? (SV **)&GvAV (PL_defgv) : &GvSV (PL_defgv);
            SV **dst = ix ? (SV **)&self->slot->defav : (SV **)&self->slot->defsv;

            SV *tmp = *src; *src = *dst; *dst = tmp;
          }

void
cancel (Coro::State self)
	CODE:
	coro_state_destroy (aTHX_ self);
        coro_call_on_destroy (aTHX_ self); /* actually only for Coro objects */


MODULE = Coro::State                PACKAGE = Coro

BOOT:
{
        sv_pool_rss        = coro_get_sv (aTHX_ "Coro::POOL_RSS"  , TRUE);
        sv_pool_size       = coro_get_sv (aTHX_ "Coro::POOL_SIZE" , TRUE);
        cv_coro_run        =      get_cv (      "Coro::_coro_run" , GV_ADD);
        cv_coro_terminate  =      get_cv (      "Coro::terminate" , GV_ADD);
        coro_current       = coro_get_sv (aTHX_ "Coro::current"   , FALSE); SvREADONLY_on (coro_current);
        av_async_pool      = coro_get_av (aTHX_ "Coro::async_pool", TRUE);
        av_destroy         = coro_get_av (aTHX_ "Coro::destroy"   , TRUE);
        sv_manager         = coro_get_sv (aTHX_ "Coro::manager"   , TRUE);
        sv_idle            = coro_get_sv (aTHX_ "Coro::idle"      , TRUE);

        sv_async_pool_idle = newSVpv ("[async pool idle]", 0); SvREADONLY_on (sv_async_pool_idle);
        sv_Coro            = newSVpv ("Coro", 0); SvREADONLY_on (sv_Coro);
        cv_pool_handler    = get_cv ("Coro::pool_handler", GV_ADD); SvREADONLY_on (cv_pool_handler);
        cv_coro_state_new  = get_cv ("Coro::State::new", 0); SvREADONLY_on (cv_coro_state_new);

	coro_stash = gv_stashpv ("Coro", TRUE);

        newCONSTSUB (coro_stash, "PRIO_MAX",    newSViv (PRIO_MAX));
        newCONSTSUB (coro_stash, "PRIO_HIGH",   newSViv (PRIO_HIGH));
        newCONSTSUB (coro_stash, "PRIO_NORMAL", newSViv (PRIO_NORMAL));
        newCONSTSUB (coro_stash, "PRIO_LOW",    newSViv (PRIO_LOW));
        newCONSTSUB (coro_stash, "PRIO_IDLE",   newSViv (PRIO_IDLE));
        newCONSTSUB (coro_stash, "PRIO_MIN",    newSViv (PRIO_MIN));

        {
          SV *sv = coro_get_sv (aTHX_ "Coro::API", TRUE);

          coroapi.schedule     = api_schedule;
          coroapi.schedule_to  = api_schedule_to;
          coroapi.cede         = api_cede;
          coroapi.cede_notself = api_cede_notself;
          coroapi.ready        = api_ready;
          coroapi.is_ready     = api_is_ready;
          coroapi.nready       = coro_nready;
          coroapi.current      = coro_current;

          /*GCoroAPI = &coroapi;*/
          sv_setiv (sv, (IV)&coroapi);
          SvREADONLY_on (sv);
        }
}

void
terminate (...)
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_terminate);

void
schedule (...)
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_schedule);

void
schedule_to (...)
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_schedule_to);

void
cede_to (...)
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_cede_to);

void
cede (...)
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_cede);

void
cede_notself (...)
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_cede_notself);

void
_set_current (SV *current)
        PROTOTYPE: $
	CODE:
        SvREFCNT_dec (SvRV (coro_current));
        SvRV_set (coro_current, SvREFCNT_inc_NN (SvRV (current)));

void
_set_readyhook (SV *hook)
	PROTOTYPE: $
        CODE:
        SvREFCNT_dec (coro_readyhook);
        coro_readyhook = SvOK (hook) ? newSVsv (hook) : 0;

int
prio (Coro::State coro, int newprio = 0)
	PROTOTYPE: $;$
        ALIAS:
        nice = 1
        CODE:
{
        RETVAL = coro->prio;

        if (items > 1)
          {
            if (ix)
              newprio = coro->prio - newprio;

            if (newprio < PRIO_MIN) newprio = PRIO_MIN;
            if (newprio > PRIO_MAX) newprio = PRIO_MAX;

            coro->prio = newprio;
          }
}
	OUTPUT:
        RETVAL

SV *
ready (SV *self)
        PROTOTYPE: $
	CODE:
        RETVAL = boolSV (api_ready (aTHX_ self));
	OUTPUT:
        RETVAL

int
nready (...)
	PROTOTYPE:
        CODE:
        RETVAL = coro_nready;
	OUTPUT:
        RETVAL

void
suspend (Coro::State self)
	PROTOTYPE: $
	CODE:
        self->flags |= CF_SUSPENDED;

void
resume (Coro::State self)
	PROTOTYPE: $
	CODE:
        self->flags &= ~CF_SUSPENDED;

void
_pool_handler (...)
	CODE:
        CORO_EXECUTE_SLF_XS (slf_init_pool_handler);

void
async_pool (SV *cv, ...)
	PROTOTYPE: &@
        PPCODE:
{
	HV *hv = (HV *)av_pop (av_async_pool);
        AV *av = newAV ();
        SV *cb = ST (0);
        int i;

        av_extend (av, items - 2);
        for (i = 1; i < items; ++i)
          av_push (av, SvREFCNT_inc_NN (ST (i)));

        if ((SV *)hv == &PL_sv_undef)
          {
            PUSHMARK (SP);
            EXTEND (SP, 2);
            PUSHs (sv_Coro);
            PUSHs ((SV *)cv_pool_handler);
            PUTBACK;
            call_sv ((SV *)cv_coro_state_new, G_SCALAR);
            SPAGAIN;

            hv = (HV *)SvREFCNT_inc_NN (SvRV (POPs));
          }

        {
          struct coro *coro = SvSTATE_hv (hv);

          assert (!coro->invoke_cb);
          assert (!coro->invoke_av);
          coro->invoke_cb = SvREFCNT_inc (cb);
          coro->invoke_av = av;
        }

        api_ready (aTHX_ (SV *)hv);

        if (GIMME_V != G_VOID)
          XPUSHs (sv_2mortal (newRV_noinc ((SV *)hv)));
        else
          SvREFCNT_dec (hv);
}

SV *
rouse_cb ()
        PROTOTYPE:
	CODE:
        RETVAL = coro_new_rouse_cb (aTHX);
	OUTPUT:
        RETVAL

void
rouse_wait (...)
        PROTOTYPE: ;$
	PPCODE:
        CORO_EXECUTE_SLF_XS (slf_init_rouse_wait);

void
on_enter (SV *block)
	ALIAS:
        on_leave = 1
	PROTOTYPE: &
	CODE:
{
	struct coro *coro = SvSTATE_current;
  	AV **avp = ix ? &coro->on_leave : &coro->on_enter;

        block = (SV *)coro_sv_2cv (aTHX_ block);

        if (!*avp)
          *avp = newAV ();

        av_push (*avp, SvREFCNT_inc (block));

        if (!ix)
          on_enterleave_call (aTHX_ block);

        LEAVE; /* pp_entersub unfortunately forces an ENTER/LEAVE around XS calls */
        SAVEDESTRUCTOR_X (ix ? coro_pop_on_leave : coro_pop_on_enter, (void *)coro);
        ENTER; /* pp_entersub unfortunately forces an ENTER/LEAVE around XS calls */
}


MODULE = Coro::State                PACKAGE = PerlIO::cede

BOOT:
	PerlIO_define_layer (aTHX_ &PerlIO_cede);


MODULE = Coro::State                PACKAGE = Coro::Semaphore

SV *
new (SV *klass, SV *count = 0)
	CODE:
        RETVAL = sv_bless (
                   coro_waitarray_new (aTHX_ count && SvOK (count) ? SvIV (count) : 1),
                   GvSTASH (CvGV (cv))
                 );
	OUTPUT:
        RETVAL

# helper for Coro::Channel and others
SV *
_alloc (int count)
	CODE:
        RETVAL = coro_waitarray_new (aTHX_ count);
	OUTPUT:
        RETVAL

SV *
count (SV *self)
	CODE:
        RETVAL = newSVsv (AvARRAY ((AV *)SvRV (self))[0]);
	OUTPUT:
        RETVAL

void
up (SV *self, int adjust = 1)
	ALIAS:
        adjust = 1
        CODE:
        coro_semaphore_adjust (aTHX_ (AV *)SvRV (self), ix ? adjust : 1);

void
down (...)
        CODE:
        CORO_EXECUTE_SLF_XS (slf_init_semaphore_down);

void
wait (...)
        CODE:
        CORO_EXECUTE_SLF_XS (slf_init_semaphore_wait);

void
try (SV *self)
        PPCODE:
{
        AV *av = (AV *)SvRV (self);
        SV *count_sv = AvARRAY (av)[0];
        IV count = SvIVX (count_sv);

        if (count > 0)
          {
            --count;
            SvIVX (count_sv) = count;
            XSRETURN_YES;
          }
        else
          XSRETURN_NO;
}

void
waiters (SV *self)
    	PPCODE:
{
        AV *av = (AV *)SvRV (self);
        int wcount = AvFILLp (av) + 1 - 1;

        if (GIMME_V == G_SCALAR)
          XPUSHs (sv_2mortal (newSViv (wcount)));
        else
          {
            int i;
            EXTEND (SP, wcount);
            for (i = 1; i <= wcount; ++i)
              PUSHs (sv_2mortal (newRV_inc (AvARRAY (av)[i])));
          }
}

MODULE = Coro::State                PACKAGE = Coro::SemaphoreSet

void
_may_delete (SV *sem, int count, int extra_refs)
	PPCODE:
{
  	AV *av = (AV *)SvRV (sem);

        if (SvREFCNT ((SV *)av) == 1 + extra_refs
            && AvFILLp (av) == 0 /* no waiters, just count */
            && SvIV (AvARRAY (av)[0]) == count)
          XSRETURN_YES;

        XSRETURN_NO;
}

MODULE = Coro::State                PACKAGE = Coro::Signal

SV *
new (SV *klass)
	CODE:
        RETVAL = sv_bless (
                   coro_waitarray_new (aTHX_ 0),
                   GvSTASH (CvGV (cv))
                 );
        OUTPUT:
        RETVAL

void
wait (...)
        CODE:
        CORO_EXECUTE_SLF_XS (slf_init_signal_wait);

void
broadcast (SV *self)
        CODE:
{
  	AV *av = (AV *)SvRV (self);
        coro_signal_wake (aTHX_ av, AvFILLp (av));
}

void
send (SV *self)
        CODE:
{
	AV *av = (AV *)SvRV (self);

        if (AvFILLp (av))
          coro_signal_wake (aTHX_ av, 1);
        else
          SvIVX (AvARRAY (av)[0]) = 1; /* remember the signal */
}

IV
awaited (SV *self)
    	CODE:
        RETVAL = AvFILLp ((AV *)SvRV (self)) + 1 - 1;
	OUTPUT:
        RETVAL


MODULE = Coro::State                PACKAGE = Coro::AnyEvent

BOOT:
        sv_activity = coro_get_sv (aTHX_ "Coro::AnyEvent::ACTIVITY", TRUE);

void
_schedule (...)
	CODE:
{
  	static int incede;

        api_cede_notself (aTHX);

        ++incede;
        while (coro_nready >= incede && api_cede (aTHX))
          ;

        sv_setsv (sv_activity, &PL_sv_undef);
        if (coro_nready >= incede)
          {
            PUSHMARK (SP);
            PUTBACK;
            call_pv ("Coro::AnyEvent::_activity", G_KEEPERR | G_EVAL | G_VOID | G_DISCARD);
          }

        --incede;
}


MODULE = Coro::State                PACKAGE = Coro::AIO

void
_register (char *target, char *proto, SV *req)
	CODE:
{
        CV *req_cv = coro_sv_2cv (aTHX_ req);
        /* newXSproto doesn't return the CV on 5.8 */
        CV *slf_cv = newXS (target, coro_aio_req_xs, __FILE__);
        sv_setpv ((SV *)slf_cv, proto);
        sv_magicext ((SV *)slf_cv, (SV *)req_cv, CORO_MAGIC_type_aio, 0, 0, 0);
}

