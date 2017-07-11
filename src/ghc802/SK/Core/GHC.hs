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
    LConDecl,
    LHsBind,
    LHsExpr,
    LHsDecl,
    LHsDocString,
    LHsSigWcType,
    LHsType,
    LImportDecl,
    LPat,
    ImportDecl(..),
    ParsedModule(..),
    Pat(..),
    RdrName,
    Sig(..),
    TypecheckedModule(..),

    compileParsedExpr,
    defaultErrorHandler,
    defaultFatalMessager,
    defaultFlushOut,
    desugarModule,
    emptyLocalBinds,
    getLoc,
    getModuleInfo,
    getModSummary,
    getSessionDynFlags,
    loadModule,
    lookupModule,
    mkBindStmt,
    mkBodyStmt,
    mkHsDo,
    mkFunBind,
    mkHsFractional,
    mkHsIf,
    mkHsIntegral,
    mkHsLam,
    mkLHsSigWcType,
    mkMatch, -- signature changed in git HEAD
    mkMatchGroup,
    mkModuleName,
    mkNPat,
    modInfoTyThings,
    noLoc,
    placeHolderType,
    placeHolderNames,
    runGhc,
    setContext,
    setSessionDynFlags,
    srcSpanFile,
    srcSpanStartCol,
    srcSpanStartLine,
    typecheckModule,

    -- * Bag
    listToBag,

    -- * BasicTypes
    Boxity(..),
    FractionalLit(..),
    Origin(..),
    SourceText,
    SuccessFlag(..),

    -- * DriverPhases
    HscSource(..),
    Phase(..),
    startPhase,

    -- * DriverPipeline
    compileFile,
    compileOne',
    link,
    preprocess,
    oneShot,

    -- * DynFlags
    DynFlags(..),
    FatalMessager,
    FlushOut(..),
    GeneralFlag(..),
    GhcLink(..),
    GhcMode(..),
    HasDynFlags(..),
    HscTarget(..),
    Language(..),
    gopt,
    languageExtensions,
    parseDynamicFilePragma,
    xopt,
    xopt_set,
    xopt_unset,

    -- * ErrUtils
    pprErrMsgBagWithLoc,
    fatalErrorMsg'',

    -- * Exception
    ghandle,
    tryIO,

    -- * FastString
    fsLit,
    unpackFS,

    -- * Finder
    addHomeModuleToFinder,
    findObjectLinkable,
    mkHomeModLocation,

    -- * GhcMake
    topSortModuleGraph,

    -- * GhcMonad
    modifySession,

    -- * HeaderInfo
    getOptionsFromFile,

    -- * HsBinds
    HsBindLR(..),
    HsLocalBinds,
    HsLocalBindsLR(..),
    HsValBindsLR(..),

    -- * HsDecls
    ClsInstDecl(..),
    ConDecl(..),
    ConDeclField(..),
    HsConDeclDetails,
    HsDataDefn(..),
    HsDeriving,
    HsTyVarBndr(..),
    InstDecl(..),
    LConDeclField,
    LHsQTyVars,
    LHsTyVarBndr,
    NewOrData(..),
    TyClDecl(..),

    -- * HsExpr
    GRHSs(..),
    GRHS(..),
    GuardLStmt,
    Match(..),
    MatchFixity(..),
    MatchGroup(..),
    HsRecordBinds,
    HsTupArg (..),
    LGRHS,
    LMatch,
    StmtLR(..),

    -- * HsImpExp
    simpleImportDecl,

    -- * HsPat
    HsRecFields(..),
    LHsRecField,
    LHsRecField',
    HsRecField'(..),

    -- * HsTypes
    AmbiguousFieldOcc(..),
    FieldOcc(..),
    HsConDetails(..),
    HsTupleSort(..),
    HsRecField,
    HsRecUpdField,
    InteractiveImport(..),
    ModSummary(..),
    handleSourceError,
    mkFieldOcc,
    mkHsQTvs,
    srcErrorMessages,

    -- * HsUtils
    mkClassOpSigs,
    mkLHsSigType,
    mkHsIsString,

    -- * HscMain
    batchMsg,

    -- * HscTypes
    HscEnv(..),
    SourceModified(..),
    TyThing(..),
    ms_mod_name,
    pprHPT,

    -- * IfaceSyn
    IfaceDecl(..),

    -- * IfaceType
    IfaceType(..),
    IfaceTyCon(..),

    -- * InteractiveEval
    getContext,

    -- * Linker
    getHValue,

    -- * MkIface
    tyThingToIfaceDecl,

    -- * Module
    Module(..),
    ModLocation(..),
    ModuleName,
    mainUnitId,
    mkModule,
    moduleNameSlashes,
    moduleNameString,

    -- * MonadUtils
    MonadIO(..),

    -- * OccName
    clsName,
    srcDataName,
    tcName,
    tcClsName,
    tvName,

    -- * Outputable
    Outputable(..),
    showPpr,
    showSDoc,
    showSDocUnqual,

    -- * OrdList
    toOL,

    -- * Panic
    GhcException(..),
    handleGhcException,
    throwGhcException,

    -- * PlaceHolder
    PlaceHolder(..),

    -- * PprTyThing
    pprTyThing,

    -- * RdrHsSyn
    cvTopDecls,
    mkRdrRecordCon,
    mkRdrRecordUpd,

    -- * RdrName
    mkUnqual,
    mkVarUnqual,
    nameRdrName,

    -- * SrcLoc
    GenLocated(..),
    Located,
    RealLocated,
    RealSrcSpan,
    SrcSpan(..),

    combineLocs,
    mkRealSrcLoc,
    mkRealSrcSpan,
    mkSrcLoc,
    mkSrcSpan,
    unLoc,

    -- * StringBuffer
    stringToStringBuffer,

    -- * TyWiredIn
    consDataConName,

    -- * UniqFM

    -- Ghc version newer than 8.0.2 uses "addToHpt" instead of "addToUFM".
    addToUFM,

    -- * Util
    getModificationUTCTime,
    readRational,

    -- * Var
    varName,
    varType,

    -- * GHCi.RemoteTypes
    localRef,
    withForeignRef

  ) where

-- ghc
import GHC
import Bag
import BasicTypes
import DriverPhases
import DriverPipeline
import DynFlags
import ErrUtils
import Exception
import FastString
import Finder
import GhcMonad
import HeaderInfo
import HscMain
import HscTypes
import IfaceSyn
import Linker
import MkIface
import Module
import MonadUtils
import OccName hiding (varName)
import OrdList
import Outputable
import Panic
import PprTyThing
import RdrHsSyn
import RdrName
import SrcLoc
import StringBuffer
import TysWiredIn
import UniqFM
import Util
import Var

-- ghci
import GHCi.RemoteTypes
