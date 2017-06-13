-- | Module re-exporting functions and types from ghc-8.0.2 package.
module SK.Core.GHC
  ( -- * GHC
    AnnotationComment(..),
    ExprLStmt,
    Ghc,
    GhcMonad(..),
    GhcT,
    ExceptionMonad(..),
    HsBind,
    HsExpr(..),
    HsDocString(..),
    HsLit(..),
    HsModule(..),
    HsDecl(..),
    HsStmtContext(..),
    HsType(..),
    HValue,
    LHsBind,
    LHsExpr,
    LHsDecl,
    LHsDocString,
    LHsSigWcType,
    LHsType,
    LImportDecl,
    LPat,
    ImportDecl(..),
    Pat(..),
    RdrName,
    Sig(..),

    compileParsedExpr,
    defaultErrorHandler,
    defaultFatalMessager,
    defaultFlushOut,
    emptyLocalBinds,
    getLoc,
    getSessionDynFlags,
    mkBindStmt,
    mkBodyStmt,
    mkHsDo,
    mkFunBind,
    mkHsIf,
    mkHsIntegral,
    mkHsLam,
    mkLHsSigWcType,
    mkMatch,
    mkModuleName,
    mkNPat,
    noLoc,
    placeHolderType,
    runGhc,
    setContext,
    setSessionDynFlags,
    srcSpanStartCol,
    srcSpanStartLine,

    -- * BasicTypes
    Boxity(..),

    -- * DynFlags
    DynFlags(..),
    GhcLink(..),
    HasDynFlags(..),
    HscTarget(..),
    xopt_set,
    xopt_unset,

    -- * FastString
    fsLit,

    -- * OccName
    srcDataName,
    tcClsName,

    -- * Outputable
    ppr,

    -- * OrdList
    toOL,

    -- * HsImpExp
    simpleImportDecl,

    -- * HsTypes
    HsConDetails(..),
    HsTupleSort(..),
    InteractiveImport(..),

    -- * RdrHsSyn
    cvTopDecls,

    -- * RdrName
    mkUnqual,
    mkVarUnqual,

    -- * SrcLoc
    GenLocated(..),
    Located,
    RealLocated,
    RealSrcSpan,
    SrcSpan(..),

    mkRealSrcLoc,
    mkRealSrcSpan,
    unLoc,

    -- * Internal
    setInterpretDynFlags
  ) where

import GHC
import BasicTypes
import DynFlags
import Exception
import FastString
import OccName
import OrdList
import Outputable
import RdrHsSyn
import RdrName
import SrcLoc

setInterpretDynFlags :: GhcMonad m => DynFlags -> m ()
setInterpretDynFlags flags = do
  _ <- setSessionDynFlags (flags { hscTarget = HscInterpreted
                                 , ghcLink = LinkInMemory })
  return ()
