/**
 * CPP macros to manage module names in ghc.
 */

#pragma once

/*
Changes happend from ghc 9.0 to 9.2. Also defining the module aliases for ghc
older than 9.0.
*/

#if __GLASGOW_HASKELL__ >= 902
#define GHC_Driver_Env             GHC.Driver.Env
#define GHC_Driver_Env_Types       GHC.Driver.Env.Types
#define GHC_Driver_Errors          GHC.Driver.Errors
#define GHC_Driver_Ppr             GHC.Driver.Ppr

#define GHC_Platform_Ways          GHC.Platform.Ways
#define GHC_Runtime_Context        GHC.Runtime.Context

#define GHC_Types_Error            GHC.Types.Error
#define GHC_Types_Fixity           GHC.Types.Fixity
#define GHC_Types_SourceError      GHC.Types.SourceError
#define GHC_Types_SourceFile       GHC.Types.SourceFile
#define GHC_Types_SourceText       GHC.Types.SourceText
#define GHC_Types_Target           GHC.Types.Target
#define GHC_Types_TyThing          GHC.Types.TyThing
#define GHC_Types_TyThing_Ppr      GHC.Types.TyThing.Ppr

#define GHC_Unit_Finder            GHC.Unit.Finder
#define GHC_Unit_Home_ModInfo      GHC.Unit.Home.ModInfo
#define GHC_Unit_Module_Deps       GHC.Unit.Module.Deps
#define GHC_Unit_Module_Graph      GHC.Unit.Module.Graph
#define GHC_Unit_Module_ModIface   GHC.Unit.Module.ModIface
#define GHC_Unit_Module_ModSummary GHC.Unit.Module.ModSummary

#elif __GLASGOW_HASKELL__ >= 900
#define GHC_Driver_Env             GHC.Driver.Types
#define GHC_Driver_Env_Types       GHC.Driver.Types
#define GHC_Driver_Errors          GHC.Utils.Error
#define GHC_Driver_Ppr             GHC.Utils.Outputable

#define GHC_Platform_Ways          GHC.Driver.Ways

#define GHC_Runtime_Context        GHC.Driver.Types

#define GHC_Types_Error            GHC.Utils.Error
#define GHC_Types_Fixity           GHC.Types.Basic
#define GHC_Types_SourceError      GHC.Driver.Types
#define GHC_Types_SourceFile       GHC.Driver.Phases
#define GHC_Types_SourceText       GHC.Types.Basic
#define GHC_Types_Target           GHC.Driver.Types
#define GHC_Types_TyThing          GHC.Driver.Types
#define GHC_Types_TyThing_Ppr      GHC.Core.Ppr.TyThing

#define GHC_Unit_Finder            GHC.Driver.Finder
#define GHC_Unit_Home_ModInfo      GHC.Driver.Types
#define GHC_Unit_Module_Deps       GHC.Driver.Types
#define GHC_Unit_Module_Graph      GHC.Driver.Types
#define GHC_Unit_Module_ModIface   GHC.Driver.Types
#define GHC_Unit_Module_ModSummary GHC.Driver.Types

#else /* __GLASGOW_HASKELL__ < 900 */
#define GHC_Driver_Env             HscTypes
#define GHC_Driver_Env_Types       HscTypes
#define GHC_Driver_Errors          ErrUtils
#define GHC_Driver_Ppr             Outputable

#define GHC_Platform_Ways          DynFlags

#define GHC_Runtime_Context        HscTypes

#define GHC_Types_Error            ErrUtils
#define GHC_Types_Fixity           BasicTypes
#define GHC_Types_SourceError      HscTypes
#define GHC_Types_SourceFile       DriverPhases
#define GHC_Types_SourceText       BasicTypes
#define GHC_Types_Target           HscTypes
#define GHC_Types_TyThing          HscTypes
#define GHC_Types_TyThing_Ppr      PprTyThing

#define GHC_Unit_Finder            Finder
#define GHC_Unit_Home_ModInfo      HscTypes
#define GHC_Unit_Module_Deps       HscTypes
#define GHC_Unit_Module_Graph      HscTypes
#define GHC_Unit_Module_ModIface   HscTypes
#define GHC_Unit_Module_ModSummary HscTypes
#endif

/*
Changes happened from ghc 8.10 to ghc 9.0 From ghc 9.0.1, ghc uses "GHC.*"
name space for its modules.
*/

#if __GLASGOW_HASKELL__ >= 900
#define GHC_Builtin_Types         GHC.Builtin.Types
#define GHC_Builtin_Types_Prim    GHC.Builtin.Types.Prim

#define GHC_Core_Class            GHC.Core.Class
#define GHC_Core_DataCon          GHC.Core.DataCon
#define GHC_Core_TyCo_Rep         GHC.Core.TyCo.Rep
#define GHC_Core_TyCo_Tidy        GHC.Core.TyCo.Tidy

#define GHC_Data_Bag              GHC.Data.Bag
#define GHC_Data_FastString       GHC.Data.FastString
#define GHC_Data_EnumSet          GHC.Data.EnumSet
#define GHC_Data_Maybe            GHC.Data.Maybe
#define GHC_Data_OrdList          GHC.Data.OrdList
#define GHC_Data_StringBuffer     GHC.Data.StringBuffer

#define GHC_Driver_Flags          GHC.Driver.Flags
#define GHC_Driver_Main           GHC.Driver.Main
#define GHC_Driver_Make           GHC.Driver.Make
#define GHC_Driver_Monad          GHC.Driver.Monad
#define GHC_Driver_Phases         GHC.Driver.Phases
#define GHC_Driver_Pipeline       GHC.Driver.Pipeline
#define GHC_Driver_Session        GHC.Driver.Session
#define GHC_Driver_Types          GHC.Driver.Types
#define GHC_Driver_Ways           GHC.Driver.Ways

#define GHC_Hs_Stats              GHC.Hs.Stats

#define GHC_Iface_Load            GHC.Iface.Load
#define GHC_Iface_Make            GHC.Iface.Make
#define GHC_Iface_Recomp          GHC.Iface.Recomp
#define GHC_Iface_Recomp_Binary   GHC.Iface.Recomp.Binary
#define GHC_Iface_Recomp_Flags    GHC.Iface.Recomp.Flags

#define GHC_IfaceToCore           GHC.IfaceToCore

#define GHC_Parser_Annotation     GHC.Parser.Annotation
#define GHC_Parser_CharClass      GHC.Parser.CharClass
#define GHC_Parser_Header         GHC.Parser.Header
#define GHC_Parser_Lexer          GHC.Parser.Lexer
#define GHC_Parser_PostProcess    GHC.Parser.PostProcess

#define GHC_Plugins               GHC.Plugins

#define GHC_Runtime_Eval          GHC.Runtime.Eval
#define GHC_Runtime_Linker        GHC.Runtime.Linker
#define GHC_Runtime_Loader        GHC.Runtime.Loader

#define GHC_Settings_Config       GHC.Settings.Config

#define GHC_Tc_Module             GHC.Tc.Module
#define GHC_Tc_Utils_Monad        GHC.Tc.Utils.Monad
#define GHC_Tc_Utils_Zonk         GHC.Tc.Utils.Zonk

#define GHC_Types_Basic           GHC.Types.Basic
#define GHC_Types_FieldLabel      GHC.Types.FieldLabel
#define GHC_Types_ForeignCall     GHC.Types.ForeignCall
#define GHC_Types_Name            GHC.Types.Name
#define GHC_Types_Name_Occurrence GHC.Types.Name.Occurrence
#define GHC_Types_Name_Reader     GHC.Types.Name.Reader
#define GHC_Types_SrcLoc          GHC.Types.SrcLoc
#define GHC_Types_Unique_Set      GHC.Types.Unique.Set
#define GHC_Types_Unique_Supply   GHC.Types.Unique.Supply
#define GHC_Types_Var             GHC.Types.Var
#define GHC_Types_Var_Env         GHC.Types.Var.Env

#define GHC_Unit_Module           GHC.Unit.Module
#define GHC_Unit_State            GHC.Unit.State
#define GHC_Unit_Types            GHC.Unit.Types

#define GHC_Utils_Encoding        GHC.Utils.Encoding
#define GHC_Utils_Error           GHC.Utils.Error
#define GHC_Utils_Exception       GHC.Utils.Exception
#define GHC_Utils_Fingerprint     GHC.Utils.Fingerprint
#define GHC_Utils_Lexeme          GHC.Utils.Lexeme
#define GHC_Utils_Misc            GHC.Utils.Misc
#define GHC_Utils_Outputable      GHC.Utils.Outputable
#define GHC_Utils_Panic           GHC.Utils.Panic
#define GHC_Utils_Ppr             GHC.Utils.Ppr

#else /* __GLASGOW_HASKELL__ < 900 */
#define GHC_Builtin_Types         TysWiredIn
#define GHC_Builtin_Types_Prim    TysPrim

#define GHC_Core_Class            Class
#define GHC_Core_DataCon          DataCon
#define GHC_Core_TyCo_Rep         TyCoRep
#define GHC_Core_TyCo_Tidy        TyCoTidy

#define GHC_Data_Bag              Bag
#define GHC_Data_FastString       FastString
#define GHC_Data_EnumSet          EnumSet
#define GHC_Data_Maybe            Maybes
#define GHC_Data_OrdList          OrdList
#define GHC_Data_StringBuffer     StringBuffer

#define GHC_Driver_Flags          DynFlags
#define GHC_Driver_Main           HscMain
#define GHC_Driver_Make           GhcMake
#define GHC_Driver_Monad          GhcMonad
#define GHC_Driver_Phases         DriverPhases
#define GHC_Driver_Pipeline       DriverPipeline
#define GHC_Driver_Session        DynFlags
#define GHC_Driver_Types          HscTypes

#define GHC_Hs_Stats              HscStats

#define GHC_Iface_Load            LoadIface
#define GHC_Iface_Make            MkIface
#define GHC_Iface_Recomp          MkIface
#define GHC_Iface_Recomp_Binary   BinFingerprint
#define GHC_Iface_Recomp_Flags    FlagChecker

#define GHC_IfaceToCore           TcIface

#define GHC_Parser_Annotation     ApiAnnotation
#define GHC_Parser_CharClass      Ctype
#define GHC_Parser_Header         HeaderInfo
#define GHC_Parser_Lexer          Lexer
#define GHC_Parser_PostProcess    RdrHsSyn

#define GHC_Plugins               Plugins

#define GHC_Runtime_Eval          InteractiveEval
#define GHC_Runtime_Linker        Linker
#define GHC_Runtime_Loader        DynamicLoading

#define GHC_Settings_Config       Config

#define GHC_Tc_Module             TcRnDriver
#define GHC_Tc_Utils_Monad        TcRnMonad
#define GHC_Tc_Utils_Zonk         TcHsSyn

#define GHC_Types_Basic           BasicTypes
#define GHC_Types_FieldLabel      FieldLabel
#define GHC_Types_ForeignCall     ForeignCall
#define GHC_Types_Name            Name
#define GHC_Types_Name_Occurrence OccName
#define GHC_Types_Name_Reader     RdrName
#define GHC_Types_SrcLoc          SrcLoc
#define GHC_Types_Unique_Set      UniqSet
#define GHC_Types_Unique_Supply   UniqSupply
#define GHC_Types_Var             Var
#define GHC_Types_Var_Env         VarEnv

#define GHC_Unit_Module           Module
#define GHC_Unit_State            Packages
#define GHC_Unit_Types            HscTypes

#define GHC_Utils_Encoding        Encoding
#define GHC_Utils_Error           ErrUtils
#define GHC_Utils_Exception       Exception
#define GHC_Utils_Fingerprint     Fingerprint
#define GHC_Utils_Lexeme          Lexeme
#define GHC_Utils_Misc            Util
#define GHC_Utils_Outputable      Outputable
#define GHC_Utils_Panic           Panic
#define GHC_Utils_Ppr             Pretty
#endif

/*
Modules which changed its name between 8.10.x and 9.0.x.
 */

#if __GLASGOW_HASKELL__ >= 900
#define GHC_Hs_Type         GHC.Hs.Type
#define GHC_Utils_CliOption GHC.Utils.CliOption
#elif __GLASGOW_HASKELL__ >= 810
#define GHC_Hs_Type         GHC.Hs.Types
#define GHC_Utils_CliOption CliOption
#else
#define GHC_Hs_Type         HsTypes
#define GHC_Utils_CliOption DynFlags
#endif

/*
From ghc 8.10.1, modules for AST were moved under 'GHC.Hs.*'. Defining aliases
for import declarations. For more info about module renaming, see:

  https://gitlab.haskell.org/ghc/ghc/issues/13009

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
#define GHC_Hs_Utils     HsUtils
#endif
