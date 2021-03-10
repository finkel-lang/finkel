/**
 * Slim variant of "hschooks.c" found in ghc source
 */

#include "Rts.h"
#include "HsFFI.h"

void initGCStatistics (void)
{
  if (RtsFlags.GcFlags.giveStats == NO_GC_STATS) {
    RtsFlags.GcFlags.giveStats = COLLECT_GC_STATS;
  }
}
