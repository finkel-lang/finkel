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
    HsDocString(..),
    HsLit(..),
    HsModule(..),
    HsDecl(..),
    HsStmtContext(..),
    HsType(..),
    HValue,
    LConDecl,
    LHsBind,
    LHsDecl,
    LHsDocString,
    LHsSigWcType,
    LHsType,
    LImportDecl,
    LPat,
    ModuleInfo,
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
    defaultDynFlags,
    gopt,
    gopt_unset,
    lang_set,
    languageExtensions,
    parseDynamicFilePragma,
    thisPackage,
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

    -- * ForeignCall
    CCallConv(..),
    CExportSpec(..),
    Safety(..),

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
    ForeignDecl(..),
    ForeignExport(..),
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
    noForeignExportCoercionYet,
    noForeignImportCoercionYet,

    -- * HsExpr
    ArithSeqInfo(..),
    GRHSs(..),
    GRHS(..),
    GuardLStmt,
    Match(..),
    MatchGroup(..),
    HsExpr(..),
    HsMatchContext(..),
    HsRecordBinds,
    HsTupArg (..),
    LGRHS,
    LHsExpr,
    LMatch,
    StmtLR(..),
    noPostTcExpr,

    -- * HsImpExp
    LIE,
    LIEWrappedName,
    IE(..),
    IEWildcard(..),
    IEWrappedName(..),
    ImportDecl(..),
    ieName,
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
    hscAddSptEntries,
    hscTcRnLookupRdrName,

    -- * HscTypes
    FindResult(..),
    GhcApiError,
    HscEnv(..),
    SourceError,
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
    installedUnitIdEq,
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
    placeHolderNames,
    placeHolderType,

    -- * PprTyThing
    pprTyThing,

    -- * RdrHsSyn
    cvTopDecls,
    mkRdrRecordCon,
    mkRdrRecordUpd,
    parseCImport,

    -- * RdrName
    getRdrName,
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
    noLoc,
    srcSpanEndCol,
    srcSpanEndLine,
    srcSpanFile,
    srcSpanFileName_maybe,
    srcSpanStartCol,
    srcSpanStartLine,
    unLoc,

    -- * StringBuffer
    stringToStringBuffer,

    -- * TysWiredIn
    consDataConName,
    listTyCon,

    -- * UniqSupply
    mkSplitUniqSupply,
    uniqFromSupply,

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
import ForeignCall
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
import UniqSupply
import Util
import Var

-- ghci
import GHCi.RemoteTypes
