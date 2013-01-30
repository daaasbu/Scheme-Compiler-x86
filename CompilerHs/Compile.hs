module CompilerHs.Compile
  ( p423Compile
  ) where

import System.IO
import System.Cmd
import System.Process
import System.Exit
import Control.Exception (throw)

import FrameworkHs.Driver
import FrameworkHs.Prims
import FrameworkHs.Helpers
import FrameworkHs.SExpReader.LispData

import FrameworkHs.ParseL01                    (parseProg)
import qualified FrameworkHs.GenGrammars.L01VerifyScheme as L01
import CompilerHs.VerifyScheme                 (verifyScheme)
import CompilerHs.UncoverRegisterConflict      (uncoverRegisterConflict)
import CompilerHs.AssignRegisters              (assignRegisters)
import CompilerHs.DiscardCallLive              (discardCallLive)
import CompilerHs.FinalizeLocations            (finalizeLocations)
import CompilerHs.ExposeBasicBlocks            (exposeBasicBlocks)
import CompilerHs.ExposeFrameVar               (exposeFrameVar)
import CompilerHs.FlattenProgram               (flattenProgram)
import CompilerHs.GenerateX86_64               (generateX86_64)

import qualified Data.ByteString as B

vfs = P423Pass { pass = verifyScheme
               , passName = "verifyScheme"
               , wrapperName = "verify-scheme/wrapper"
               , trace = False
               }

urc = P423Pass { pass = uncoverRegisterConflict
               , passName = "uncoverRegisterConflict"
               , wrapperName = "uncover-register-conflict/wrapper"
               , trace = False
               }

asr = P423Pass { pass = assignRegisters
               , passName = "assignRegisters"
               , wrapperName = "assign-registers/wrapper"
               , trace = False
               }

dcl = P423Pass { pass = discardCallLive
               , passName = "discardCallLive"
               , wrapperName = "discard-call-live/wrapper"
               , trace = False
               }


fnl = P423Pass { pass = finalizeLocations
               , passName = "finalizeLocations"
               , wrapperName = "finalize-locations/wrapper"
               , trace = False
               }

efv = P423Pass { pass = exposeFrameVar
               , passName = "exposeFrameVar"
               , wrapperName = "expose-frame-var/wrapper"
               , trace = False
               }

ebb = P423Pass { pass = exposeBasicBlocks
               , passName = "exposeBasicBlocks"
               , wrapperName = "expose-basic-blocks/wrapper"
               , trace = False
               }

flp = P423Pass { pass = flattenProgram
               , passName = "flattenProgram"
               , wrapperName = "flatten-program/wrapper"
               , trace = False 
               }

-- | Compose the complete compiler as a pipeline of passes.
p423Compile :: LispVal -> CompileM String
p423Compile l = do
  p <- liftPassM$ parseProg l
  p <- runPass vfs p
  p <- runPass urc p
  p <- runPass asr p
  p <- runPass dcl p
  p <- runPass fnl p
  p <- runPass efv p
  p <- runPass ebb p
  p <- runPass flp p
  assemble$ generateX86_64 p
