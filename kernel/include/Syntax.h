/**
 * CPP macros for HsSyn related codes, to support ghc version
 * compatibility.
 */

#pragma once

/*
From ghc 8.10.1, modules for AST were moved under 'GHC.Hs.*'. Defining aliases
for import declarations.
*/

#if __GLASGOW_HASKELL__ >= 810
#define GHC_Hs           GHC.Hs
#define GHC_Hs_Binds     GHC.Hs.Binds
#define GHC_Hs_Decls     GHC.Hs.Decls
#define GHC_Hs_Doc       GHC.Hs.Doc
#define GHC_Hs_Dump      GHC.Hs.Dump
#define GHC_Hs_Expr      GHC.Hs.Expr
#define GHC_Hs_Extension GHC.Hs.Extension
#define GHC_Hs_ImpExp    GHC.Hs.ImpExp
#define GHC_Hs_Lit       GHC.Hs.Lit
#define GHC_Hs_Pat       GHC.Hs.Pat
#define GHC_Hs_Types     GHC.Hs.Types
#define GHC_Hs_Utils     GHC.Hs.Utils
#else
#define GHC_Hs           HsSyn
#define GHC_Hs_Binds     HsBinds
#define GHC_Hs_Doc       HsDoc
#define GHC_Hs_Decls     HsDecls
#define GHC_Hs_Dump      HsDumpAst
#define GHC_Hs_Expr      HsExpr
#define GHC_Hs_Extension HsExtension
#define GHC_Hs_ImpExp    HsImpExp
#define GHC_Hs_Lit       HsLit
#define GHC_Hs_Pat       HsPat
#define GHC_Hs_Types     HsTypes
#define GHC_Hs_Utils     HsUtils
#endif

/*
From ghc 8.6.0, many of the data types used by the internal AST in GHC
were modified to take extension argument.

Following `NOEXT' macro will pass the `noExt' argument to such
constructors, and behaves as empty code when compiling with GHC version
prior to 8.6.0.

The `_EXT' macro is used for ignoring extension field in pattern
matches.

From ghc 8.10.0, noExt was splitted to noExtField and noExtCon.
*/

#if __GLASGOW_HASKELL__ >= 810
#define NOEXT    noExtField
#define NOEXTCON noExtCon
#define _EXT _
#elif __GLASGOW_HASKELL__ >= 806
#define NOEXT    noExt
#define NOEXTCON noExt
#define _EXT  _
#else
#define NOEXT    {- noext -}
#define NOEXTCON {- noextcon -}
#define _EXT     {- _ext -}
#endif
