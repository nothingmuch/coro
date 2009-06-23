#ifndef CORO_API_H
#define CORO_API_H

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#ifndef pTHX_
# define pTHX_
# define aTHX_
# define pTHX
# define aTHX
#endif

/* C-level coroutine struct, opaque, not used much */
struct coro;

/* used for schedule-like-function prepares */
struct coro_transfer_args
{
  struct coro *prev, *next;
};

/* this is the per-perl-coro slf frame info */
/* it is treated like other "global" interpreter data */
/* and unfortunately is copied around, so keep it small */
struct CoroSLF
{
  void (*prepare) (pTHX_ struct coro_transfer_args *ta); /* 0 means not yet initialised */
  int (*check) (pTHX_ struct CoroSLF *frame);
  void *data; /* for use by prepare/check */
};

/* needs to fill in the *frame */
typedef void (*coro_slf_cb) (pTHX_ struct CoroSLF *frame, CV *cv, SV **arg, int items);

/* private structure, always use the provided macros below */
struct CoroAPI
{
  I32 ver;
  I32 rev;
#define CORO_API_VERSION 7
#define CORO_API_REVISION 0

  /* Coro */
  int nready;
  SV *current;
  SV *except;
  void (*readyhook) (void);

  void (*schedule) (pTHX);
  void (*schedule_to) (pTHX_ SV *coro_sv);
  int (*cede) (pTHX);
  int (*cede_notself) (pTHX);
  int (*ready) (pTHX_ SV *coro_sv);
  int (*is_ready) (pTHX_ SV *coro_sv);

  /* Coro::State */
  void (*transfer) (pTHX_ SV *prev_sv, SV *next_sv); /* Coro::State */

  /* SLF */
  struct coro *(*sv_state) (pTHX_ SV *coro);
  void (*execute_slf) (pTHX_ CV *cv, coro_slf_cb init_cb, I32 ax);
  /* for use as CoroSLF.prepare */
  void (*prepare_nop)          (pTHX_ struct coro_transfer_args *ta);
  void (*prepare_schedule)     (pTHX_ struct coro_transfer_args *ta);
  void (*prepare_cede)         (pTHX_ struct coro_transfer_args *ta);
  void (*prepare_cede_notself) (pTHX_ struct coro_transfer_args *ta);
};

static struct CoroAPI *GCoroAPI;

/* public API macros */
#define CORO_TRANSFER(prev,next) GCoroAPI->transfer (aTHX_ (prev), (next))

#define CORO_SV_STATE(coro)      GCoroAPI->sv_state (aTHX_ (coro))
#define CORO_EXECUTE_SLF(cv,init,ax) GCoroAPI->execute_slf (aTHX_ (cv), (init), (ax))
#define CORO_EXECUTE_SLF_XS(init) CORO_EXECUTE_SLF (cv, (init), ax)

#define CORO_SCHEDULE            GCoroAPI->schedule (aTHX)
#define CORO_CEDE                GCoroAPI->cede (aTHX)
#define CORO_CEDE_NOTSELF        GCoroAPI->cede_notself (aTHX)
#define CORO_READY(coro)         GCoroAPI->ready (aTHX_ coro)
#define CORO_IS_READY(coro)      GCoroAPI->is_ready (coro)
#define CORO_NREADY              (GCoroAPI->nready)
#define CORO_THROW               (GCoroAPI->except)
#define CORO_CURRENT             SvRV (GCoroAPI->current)
#define CORO_READYHOOK           (GCoroAPI->readyhook)

#define I_CORO_API(YourName)                                                             \
STMT_START {                                                                             \
  SV *sv = perl_get_sv ("Coro::API", 0);                                                 \
  if (!sv) croak ("Coro::API not found");                                                \
  GCoroAPI = (struct CoroAPI*) SvIV (sv);                                                \
  if (GCoroAPI->ver != CORO_API_VERSION                                                  \
      || GCoroAPI->rev < CORO_API_REVISION)                                              \
    croak ("Coro::API version mismatch (%d.%d vs. %d.%d) -- please recompile %s",        \
           GCoroAPI->ver, GCoroAPI->rev, CORO_API_VERSION, CORO_API_REVISION, YourName); \
} STMT_END

#endif

