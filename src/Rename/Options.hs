module Rename.Options
  ( MkDirs(..), Options(..), Quiet(..), ReplacementOpt(..)
  , inputs, mkdirs, overwr, parseOptions, quiet, repl_opt )
where

import Base1T

-- fpath -------------------------------

import FPath.File       ( File )
import FPath.Parseable  ( readM )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, eitherReader, flag, help, long
                                    , metavar, short, strOption )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader, parseNE, textualOption )

-- pcre --------------------------------

import PCRE              ( compRE )
import PCRE.Error        ( REParseError )
import PCRE.REPlacement  ( REPlacement( REPlacement ) )
import PCRE.ReplText     ( ReplText )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE )

-- stdmain -----------------------------

import StdMain  ( Overwrite( NoOverwrite, Overwrite ) )

-- text --------------------------------

import Data.Text  ( pack )

--------------------------------------------------------------------------------

data MkDirs = MkDirs | NoMkDirs
  deriving (Eq,Show)

------------------------------------------------------------

data Quiet = NoQuiet | Quiet
  deriving (Eq,Show)

------------------------------------------------------------

data ReplacementOpt = ROptRepl   REPlacement
                    | ROptREName 𝕋
                    | ROptFile1  File
                    | ROptFileN  File
  deriving Show

------------------------------------------------------------

data Options = Options { _repl_opt ∷ ReplacementOpt
                       , _inputs   ∷ NonEmpty File
                       , _mkdirs   ∷ MkDirs
                       , _overwr   ∷ Overwrite
                       , _quiet    ∷ Quiet
                       }
  deriving Show

--------------------

inputs ∷ Lens' Options (NonEmpty File)
inputs = lens _inputs (\ o is → o { _inputs = is })

--------------------

repl_opt ∷ Lens' Options ReplacementOpt
repl_opt = lens _repl_opt (\ o rp → o { _repl_opt = rp })

--------------------

mkdirs ∷ Lens' Options MkDirs
mkdirs = lens _mkdirs (\ o m → o { _mkdirs = m })

--------------------

overwr ∷ Lens' Options Overwrite
overwr = lens _overwr (\ o w → o { _overwr = w })

--------------------

quiet ∷ Lens' Options Quiet
quiet = lens _quiet (\ o q → o { _quiet = q })

------------------------------------------------------------

parseReplText ∷ Parser ReplText
parseReplText = argument parsecReader (metavar "REPLACEMENT")

parseRE ∷ Parser RE
parseRE = argument (eitherReader (first toString ∘ compRE @REParseError ∘ pack))
                   (metavar "REGEX")

parseREPlacement ∷ Parser REPlacement
parseREPlacement = REPlacement ⊳ parseRE ⊵ parseReplText

parseReplacementOpt ∷ Parser ReplacementOpt
parseReplacementOpt =
    ROptRepl ⊳ parseREPlacement
  ∤ ROptREName ⊳ (pack ⊳ strOption (ю [ short 'n', long "name", metavar "REName"
                                      , help "select this named RE from re file"
                                      ]))
  ∤ ROptFile1 ⊳ (textualOption (ю [ short 'f', long "file1"
                                  , help "use 1 RE from this file" ]))
  ∤ ROptFileN ⊳ (textualOption (ю [ short 'F', long "file-many"
                                  , help "use many REs from this file" ]))

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ parseReplacementOpt
          ⊵ parseNE (argument readM (metavar "FILENAME"))
          ⊵ flag NoMkDirs MkDirs (ю [ short 'M', long "mkdirs"
                                    , help "make missing directories" ])
          ⊵ flag NoOverwrite Overwrite (ю [ short 'O', long "overwrite"
                                          , help "overwrite extant files" ])
          ⊵ flag NoQuiet Quiet (ю [ short 'q', long "no-output"
                                  , help "don't output file moves" ])

-- that's all, folks! ----------------------------------------------------------
