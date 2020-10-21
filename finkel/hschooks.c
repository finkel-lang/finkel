/*
 This file contains the "main" function for the finkel executable.

 Contents of this file is taken from "hschooks.c" found in the ghc source
 code. For more detailed comments and links to relavant issues, see the referred
 C source in the ghc source code.  This file has few modifications compared to
 the original C source code in the ghc source code: one is the removals of the
 comments, and another is removal of the "#include" CPP macro for the line
 containing "../rts/PosixSource.h", since the finkel source codes does not
 contain the C header files of "rts".
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
            "Finkel stack-space overflow: current limit is %zu bytes.\n"
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
