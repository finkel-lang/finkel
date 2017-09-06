-- | Module re-exporting functions and types from ghc-8.2.x package.
module Language.SK.GHC
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
    getModuleGraph,
    getSessionDynFlags,
    isLoaded,
    loadModule,
    lookupModule,
    lookupName,
    mkBindStmt,
    mkBodyStmt,
    mkHsDo,
    mkFunBind,
    mkHsFractional,
    mkHsIf,
    mkHsIntegral,
    mkHsLam,
    mkLHsSigWcType,
    mkMatch,
    mkMatchGroup,
    mkModuleName,
    mkModuleNameFS,
    mkNPat,
    modInfoExports,
    modInfoTyThings,
    noLoc,
    placeHolderType,
    placeHolderNames,
    runGhc,
    setContext,
    setSessionDynFlags,
    typecheckModule,

    -- * ApiAnnotation
    ApiAnns,

    -- * Bag
    listToBag,
    unitBag,

    -- * BasicTypes
    Boxity(..),
    Fixity(..),
    FixityDirection(..),
    FractionalLit(..),
    InlinePragma(..),
    InlineSpec(..),
    LexicalFixity(..),
    Origin(..),
    SourceText(..),
    SuccessFlag(..),
    defaultInlinePragma,
    alwaysInlinePragma,
    neverInlinePragma,

    -- * DataCon
    HsSrcBang(..),
    SrcStrictness(..),
    SrcUnpackedness(..),

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
    gopt_unset,
    languageExtensions,
    parseDynamicFilePragma,
    xopt,
    xopt_set,
    xopt_unset,

    -- * ErrUtils
    fatalErrorMsg'',
    mkErrMsg,
    pprErrMsgBagWithLoc,

    -- * Exception
    ghandle,
    tryIO,
    throwIO,

    -- * FastString
    FastString,
    appendFS,
    fsLit,
    headFS,
    mkFastStringByteString,
    unpackFS,

    -- * FieldLabel
    FieldLbl(..),

    -- * Finder
    addHomeModuleToFinder,
    findImportedModule,
    findObjectLinkable,
    mkHomeModLocation,

    -- * GhcMake
    topSortModuleGraph,

    -- * GhcMonad
    modifySession,

    -- * HeaderInfo
    getOptionsFromFile,

    -- * HsBinds
    FixitySig(..),
    HsBindLR(..),
    HsLocalBinds,
    HsLocalBindsLR(..),
    HsValBindsLR(..),

    -- * HsDecls
    ClsInstDecl(..),
    ConDecl(..),
    ConDeclField(..),
    DefaultDecl(..),
    HsConDeclDetails,
    HsDataDefn(..),
    HsDeriving,
    HsDerivingClause(..),
    HsTyVarBndr(..),
    InstDecl(..),
    LConDeclField,
    LHsQTyVars,
    LHsTyVarBndr,
    NewOrData(..),
    TyClDecl(..),

    -- * HsExpr
    ArithSeqInfo(..),
    GRHSs(..),
    GRHS(..),
    GuardLStmt,
    Match(..),
    MatchGroup(..),
    HsMatchContext(..),
    HsRecordBinds,
    HsTupArg (..),
    LGRHS,
    LMatch,
    StmtLR(..),
    noPostTcExpr,

    -- * HsImpExp
    LIE,
    LIEWrappedName,
    IE(..),
    IEWildcard(..),
    IEWrappedName(..),
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
    HsParsedModule(..),
    HsTupleSort(..),
    HsRecField,
    HsRecUpdField,
    InteractiveImport(..),
    ModSummary(..),
    Promoted(..),
    addToHpt,
    handleSourceError,
    mkFieldOcc,
    mkHsQTvs,
    srcErrorMessages,

    -- * HsUtils
    looksLikeModuleName,
    mkClassOpSigs,
    mkHsApp,
    mkHsIsString,
    mkParPat,
    mkLHsPar,
    mkLHsSigType,
    mkPrefixFunRhs,

    -- * HscMain
    batchMsg,

    -- * HscTypes
    FindResult(..),
    GhcApiError,
    HscEnv(..),
    SourceModified(..),
    TyThing(..),
    mkSrcErr,
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
    neverQualify,
    showPpr,
    showSDoc,
    showSDocUnqual,
    text,

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
    mkQual,
    mkUnqual,
    mkVarUnqual,
    nameRdrName,

    -- * SrcLoc
    GenLocated(..),
    Located,
    RealLocated,
    RealSrcSpan,
    SrcLoc(..),
    SrcSpan(..),

    combineLocs,
    mkGeneralSrcSpan,
    mkRealSrcLoc,
    mkRealSrcSpan,
    mkSrcLoc,
    mkSrcSpan,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanFile,
    srcSpanFileName_maybe,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,

    -- * StringBuffer
    stringToStringBuffer,

    -- * TyWiredIn
    consDataConName,

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
import FieldLabel
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
import Util
import Var

-- ghci
import GHCi.RemoteTypes
