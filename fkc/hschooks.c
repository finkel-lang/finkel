/*
  This file is taken from ghc source. See the "main.c" and "finkel.cabal" in
  "finkel/finkel" directory for detail.
*/

#include "Rts.h"
#include "HsFFI.h"
#include <string.h>
#include <stdbool.h>

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

void
initGCStatistics(void)
{
  if (RtsFlags.GcFlags.giveStats == NO_GC_STATS) {
    RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
  }
}

void
defaultsHook (void)
{
    RtsFlags.GcFlags.heapSizeSuggestionAuto = true;
    RtsFlags.GcFlags.maxStkSize = 512*1024*1024 / sizeof(W_);
    initGCStatistics();
    RtsFlags.GcFlags.idleGCDelayTime = SecondsToTime(5);
}

void
StackOverflowHook (StgWord stack_size)    /* in bytes */
{
    fprintf(stderr,
            "fkc stack-space overflow: current limit is %zu bytes.\n"
            "Use the `-K<size>' option to increase it.\n",
            (size_t) stack_size);
}

int main (int argc, char *argv[])
{
    RtsConfig conf = defaultRtsConfig;
    conf.defaultsHook = defaultsHook;
    conf.rts_opts_enabled = RtsOptsAll;
    conf.stackOverflowHook = StackOverflowHook;
    extern StgClosure ZCMain_main_closure;

    hs_main(argc, argv, &ZCMain_main_closure, conf);
}
