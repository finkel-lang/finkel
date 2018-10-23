/**
 * CPP macros for HsSyn related codes, to support ghc version
 * compatibility.
 */

#pragma once

/*
From ghc 8.6.0, many of the data types used by the internal AST in GHC
were modified to take extension argument. Following `NOEXT' macro will
pass the `noExt' argument to such constructors, and behaves as empty
code when compiling with GHC version prior to 8.6.0.
*/

#if __GLASGOW_HASKELL__ >= 806
#define NOEXT noExt
#else
#define NOEXT {- empty -}
#endif
