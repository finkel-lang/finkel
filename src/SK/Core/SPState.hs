-- | Module defining state for Lexer and source code parser.
module SK.Core.SPState
  ( SPState(..)
  , initialSPState
  ) where

import SK.Core.GHC

-- | Data type to hold comments found in source code.
data SPState = SPState {
  comments :: [Located AnnotationComment],
  annotation_comments :: [(SrcSpan, [Located AnnotationComment])],
  targetFile :: Maybe FilePath
}

-- | Initial empty state for 'SP'.
initialSPState :: SPState
initialSPState = SPState [] [] Nothing
