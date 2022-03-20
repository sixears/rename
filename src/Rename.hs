{- | rename one or more files per regular expressions -}
module Rename
  ( main, tests )
where

-- XXX Use of $1 as shorthand for ${1}
-- XXX Use of first argument pure string (non-RE) (auto-quote?)
-- XXX Apply rename to non-path filename only?
-- XXX Add ability to build regexen in file (i.e., let bindings in regex file)
-- XXX add integration tests in temp dir
-- XXX add ability to pass through arguments (as _1, _2, etc.?)
-- XXX add counter (thing incrementing; one per attempted match, one per successful match)
-- XXX add basic arithmetic
-- XXX add ability to source a list from a file
-- XXX add count of arguments
-- XXX add RE fn args pre-check ?

import Base1T

-- base --------------------------------

import Data.List           ( nub )
import Data.List.NonEmpty  ( filter )

-- containers-plus ---------------------

import ContainersPlus.Map  ( AsRepeatedKeyError )

-- env-fpath ---------------------------

import Env.FPath  ( envRcAbsFile )

-- env-plus ----------------------------

import Env.Reader  ( runEnv )

-- fpath -------------------------------

import qualified FPath.Parseable

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Basename          ( basename )
import FPath.Dirname           ( dirname )
import FPath.Error.FPathError  ( AsFPathError, FPathError )
import FPath.FileLike          ( file )
import FPath.PathComponent     ( pc )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( relfile )

-- natural -----------------------------

import Natural  ( one )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog
                          , Severity( Debug, Informational, Notice )
                          , runPureLoggingT
                          )

-- log-plus ----------------------------

import Log  ( Log, logIO, logIOT )

-- mockio-log --------------------------

import MockIO.Log          ( DoMock( NoMock ), HasDoMock )
import MockIO.IOClass      ( HasIOClass )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Directory  ( mkdir )
import MockIO.File       ( lfexists', rename )

-- monaderror-io --------------------------------

import MonadError     ( mapMError' )
import MonadError.IO  ( ӝ )

-- monadio-plus ------------------------

import MonadIO                       ( warn )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.FPath                 ( pResolve )
import MonadIO.FStat                 ( FExists( FExists, NoFExists ) )
import MonadIO.Temp                  ( testsWithTempfiles' )

-- optparse-applicative ----------------

import Options.Applicative.Types  ( ParserResult( CompletionInvoked, Failure
                                                , Success ) )
-- optparse-plus -----------------------

import OptParsePlus  ( parseOptsPure )

-- parsec-plus -------------------------

import Parsec.Error  ( AsParseError )
import ParsecPlus    ( parsecFileUTF8 )

-- pcre --------------------------------

import PCRE        ( replace, replace1, replaceMany )
import PCRE.Error  ( AsREFnError, AsREGroupError )

-- stdmain -----------------------------

import StdMain             ( checkInputFiles, checkOutputFiles, stdMain
                           , throwUsageErrors )
import StdMain.StdOptions  ( options, parseStdOptions )
import StdMain.UsageError  ( AsUsageError )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertFailure )

-- tasty-plus --------------------------

import TastyPlus  ( ioTests )

-- text --------------------------------

import Data.Text     ( drop, unlines )
import Data.Text.IO  ( hPutStrLn, putStrLn )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  Rename.RegexFile

import Rename.Error      ( RenameError )
import Rename.Options    ( MkDirs( MkDirs ), Options, Quiet( NoQuiet )
                         , ReplacementOpt( ROptREName, ROptFile1
                                         , ROptFileN, ROptRepl )
                         , inputs, mkdirs, overwr, parseOptions, quiet, repl_opt
                         )
import Rename.RegexFile  ( NamedREPlacements, toList' )

--------------------------------------------------------------------------------

renameFile ∷ ∀ ε ω μ . (MonadIO μ,
                        HasDoMock ω, HasIOClass ω, Default ω,
                        MonadLog (Log ω) μ,
                        AsFPathError ε, AsREFnError ε, AsREGroupError ε,
                        AsIOError ε, AsParseError ε, AsRepeatedKeyError 𝕋 ε,
                        Printable ε, HasCallStack, MonadError ε μ) ⇒
             Options → AbsFile → μ (AbsFile,AbsFile)
renameFile opts fn = do
  let simple_replace replacement = do
        logIOT Informational $ [fmt|checking %T against %w|] fn replacement
        replace replacement (toText fn) ≫ \ case
          𝕹   → logIOT Notice ([fmt|no replacement: %T|] fn) ⪼ return (fn, fn)
          𝕵 f → do (fn,) ⊳ FPath.Parseable.parse @AbsFile f

      log_regex_read f c = do
        n ← pResolve @AbsFile f
        logIOT Informational $ [fmt|checking %T against %s RE from %T|] fn c n
        res ← parsecFileUTF8 @NamedREPlacements n
        return res

  logIO Debug def $ [fmtT|processing '%T'|] fn
  case opts ⊣ repl_opt of

    ROptRepl replacement → simple_replace replacement

    ROptREName n         → do
      regexen ← default_regexen_fn ≫ parsecFileUTF8 @NamedREPlacements
      let no_such_re = [fmt|no such regex '%T' found in '%T'|] n fn
       in case n ! regexen of
            𝕵 replacement → simple_replace replacement
            𝕹             → throwError $ userE no_such_re

    ROptFile1 f          → do
      res ← log_regex_read f "1"
      replace1 (toList' res) (toText fn) ≫ \ case
        𝕹 → return (fn, fn)
        𝕵 (x,g) → do logIOT Debug $ [fmt|[1] %T → %t (%w)|] fn g x
                     (fn,) ⊳ FPath.Parseable.parse @AbsFile g

    ROptFileN f          → do
      res ← log_regex_read f "*"
      replaceMany (toList' res) (toText fn) ≫ \ case
        ([], _) → return (fn, fn)
        (xs,g) → do logIOT Debug $ [fmt|[*] %T → %t %w|] fn g xs
                    (fn,) ⊳ FPath.Parseable.parse @AbsFile g

----------------------------------------

default_regexen_fn ∷ ∀ ε μ .
                     (MonadIO μ, AsIOError ε, AsFPathError ε,
                      HasCallStack, MonadError ε μ) ⇒
                     μ AbsFile
default_regexen_fn =
  runEnv (envRcAbsFile "RENAME_REGEXEN" [relfile|.rename/default-regexen.txt|])

----------------------------------------

doRename ∷ ∀ ε μ .
         (MonadIO μ, MonadLog (Log MockIOClass) μ,
          MonadError ε μ, HasCallStack, Printable ε,
          AsUsageError ε, AsIOError ε, AsParseError ε, AsFPathError ε,
          AsREFnError ε, AsRepeatedKeyError 𝕋 ε, AsREGroupError ε) ⇒
         Options → μ ([𝕋],([(AbsFile,AbsFile)],[AbsDir]))

doRename opts = do
  ins ∷ NonEmpty AbsFile ← sequence $ pResolve ⊳ opts ⊣ inputs

  input_errors ← checkInputFiles (toList ins)

  outputs ← filter (\ (i,o) → i ≢ o) ⊳ sequence (renameFile opts ⊳ ins)

  let outdirs   = nub $ [ f ⊣ dirname | (_,f) ← outputs ]
      d_exist d = lfexists' Informational FExists d NoMock
  outdir_es ∷ [(AbsDir,FExists)] ← forM outdirs $ \ d → (d,) ⊳ d_exist d
  let make_dirs = nub [ d | (d,e) ← outdir_es
                          , NoFExists ≡ e, MkDirs ≡ opts ⊣ mkdirs ]
  usage_errors ← checkOutputFiles (snd ⊳ outputs) make_dirs
                                  (opts ⊣ overwr)
  return (input_errors ⊕ usage_errors, (outputs, make_dirs))

----------------------------------------

-- A note on -v messages:
--   -) x → y is issued on stdout unless --quiet is given
--   -) default (Warning)
--   -) (Notice) show non-renamed files
--   -) (Informational) show replacement checks about to happen
--   -) (Debug)
myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsUsageError ε, AsIOError ε,AsParseError ε,
          AsProcExitError ε, AsCreateProcError ε, AsFPathError ε, AsREFnError ε,
          AsRepeatedKeyError 𝕋 ε, AsREGroupError ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock opts = do
  (errors, (outputs, make_dirs)) ← doRename opts

  let msg = "not continuing with rename in presence of errors"
  throwUsageErrors do_mock msg errors

  forM_ make_dirs (\ d → mkdir Notice d 0750 do_mock)
  forM_ outputs $ \ (infn,outfn) → when (infn ≢ outfn) $ do
    when (NoQuiet ≡ opts ⊣ quiet) $
      liftIO (putStrLn $ [fmt|%T → %T|] infn outfn)
    rename Notice infn outfn do_mock
  return $ if outputs ≡ [] then 1 else 0

------------------------------------------------------------

{- | Run the program (using `getArgs` to provide arguments) -}
main ∷ IO ()
main = do
  let progDesc = unlines [ "rename files per a regex."
                         , "Note that the regex always matches against an"
                         , "absolute filename; but only the matched portion is"
                         , "replaced.  Therefore it is common to use a positive"
                         , "look-behind assertion such as (?<=/) to anchor the"
                         , "match to the beginning of a directory; or even"
                         , "(?<=/)(?=[^/]+$) to match only the filename portion"
                         , "of each file."
                         ]
  getArgs ≫ (\ as → stdMain progDesc parseOptions (myMain @RenameError) as)

------------------------------------------------------------
--                         tests                          --
------------------------------------------------------------

type MoveSpec   = (AbsFile,AbsFile)
type RenameSpec = ([𝕋], ([MoveSpec],[AbsDir]))

{- | Run a rename; return action specifications -}
renameSpec ∷ (MonadIO μ, MonadError 𝕊 μ) ⇒
             ℕ → [𝕊] → μ (𝔼 RenameError RenameSpec, Log MockIOClass)
renameSpec width args = do
  case parseOptsPure args ф (parseStdOptions one parseOptions) width of
    Success           o → runPureLoggingT $
                            ѥ @RenameError @_ @(𝔼 _)$ doRename (o ⊣ options)
    Failure           f → throwError $ show f
    CompletionInvoked e → throwError $ show e

renameSpec' ∷ (MonadIO μ) ⇒ [𝕊] → μ (𝔼 RenameError RenameSpec, Log MockIOClass)
renameSpec' args = ӝ $ mapMError' SException $ renameSpec 80 args

data SException = SException 𝕊
  deriving Show
instance Exception SException

mainTests ∷ TestTree
mainTests =
  let checkFromTos exp =
        ("from-to", \ (x,_) → assertRight (\ (_, (fts,_)) → exp @=? fts) x)
      checkMkdirs exp =
        ("mkdir", \ (x,_) → assertRight (\ (_, (_,mks)) → exp @=? mks) x)

      -- "run" a cmdline invocation of rename; check that the file renames
      -- and made dirs are as expected
      checkRun ∷ TestName → [𝕊] → [(AbsFile,AbsFile)] → [AbsDir] → TestTree
      checkRun name args fromTos mkDirs =
        ioTests name [ checkFromTos fromTos, checkMkdirs mkDirs ]
                     (renameSpec' args)

      -- "run" a cmdline invocation of rename; with two tempfiles - the args
      -- used to 'rename' are given as a function.  The first
      -- is used as an argument to -F, and is populated with contents provided
      -- by a setup function (which is given the name of the second file).
      -- the second file is simply there to be renamed.
      checkRunFArgs ∷ ((AbsFile,AbsFile) → [𝕊]) → TestName → [𝕋]
                   → [(TestName, AbsFile→RenameSpec→Assertion)]
                   → TestTree
      checkRunFArgs argsf name pats ts =
          let nowt = const $ return ()
              setup ((f1∷AbsFile,h1),(f2,_)) = do
                forM_ pats $ hPutStrLn h1
                return (f2,f1)
              ts' = [ (nm,
                       \ (f1,f2) _ → do
                         (espec,_) ← renameSpec' (argsf (f1,f2))
                         case espec of
                           𝕷 e → assertFailure $ show e
                           𝕽 spec → t f2 spec
                      )
                    | (nm,t) ← ts ]
         in testsWithTempfiles' name (""∷𝕋,"") setup nowt nowt ts'

      checkRunf' mkd =
        checkRunFArgs (\ (f1,f2) → [ "-f", toString f1, toString f2 ]
                                 ⊕ if mkd then [ "--mkdirs" ] else [])

      checkRunf  = checkRunf' 𝕱
      checkRunfM = checkRunf' 𝕿

      checkRunF =
        checkRunFArgs (\ (f1,f2) → [ "-F", toString f1, toString f2 ])

   in testGroup "main" $
        [ checkRun "simple" ["/foo","/bar","/foo","/baz"]
                            [([absfile|/foo|],[absfile|/bar|])] []

          -- test that -f <file> works; simple case, with no order-specific
          -- issues; replacing the basename of a full path
          -- this test presumes that the temp file name doesn't end with 'foo'
        , checkRunf "-f (simple file)" [ "u\t^(/.*)/([^/]+)$\t${1}/foo" ]
                    [ (".../foo",
                       \ f2 s → ([],([(f2,f2 & file ⊢ [pc|foo|])],[])) @=? s)
                    ]

          -- test that -f <file> works; simple case, with no order-specific
          -- issues; inserting a new dir at the end of a full path.  We do this
          -- as a precursor to the -f order tests (to be sure that dir insertion
          -- works by itself).
        , checkRunfM "-f (insert dir)"
                    [ "t\t^(/.*)/([^/]+)$\t${1}/nosuchdir/${2}" ]
                    [ (".../foo",
                       \ f2 s →  do
                         -- just for debugging test failures during installation
                         when (s ≡ ([],([],[]))) $ warn $ [fmtT|f2: '%T'|] f2
                         let expd       = f2 ⊣ dirname ⫻ [reldir|nosuchdir/|]
                             expf       = expd ⫻ basename f2
                         ([],([(f2,expf)],[expd])) @=? s
                      )
                    ]


          -- test that -f <file> works; with order-specific patterns;
          -- first (effective) one affecting the basename,
          -- second (ineffective) one affecting the dir
        , checkRunf "-f order (basename not dir)"
                    [ "u\t^(/.*)/([^/]+)$\t${1}/foo"
                    -- this pattern should not effect, because the prior one did
                    , "t\t^(/.*)/([^/]+)$\t${1}/nosuchdir/${2}" ]
                    [ ("/foo",
                       \ f2 spec →
                             ([],([(f2,f2 & file ⊢ [pc|foo|])],[])) @=? spec
                      )]

          -- test that -f <file> works; with order-specific patterns;
          -- first (effective) one affecting the dir,
          -- second (ineffective) one affecting the basename
        , checkRunfM "-f order (dir not basename)"
                     [ "t\t^(/.*)/([^/]+)$\t${1}/nosuchdir/${2}"
                       -- this pattern should not effect, because the prior one
                       -- did
                     , "u\t^(/.*)/([^/]+)$\t${1}/foo" ]

                    [ ("/nosuchdir/"
                      , \ f2 s →  do
                         -- just for debugging test failures during installation
                         when (s ≡ ([],([],[]))) $ warn $ [fmtT|f2: '%T'|] f2
                         let expd       = f2 ⊣ dirname ⫻ [reldir|nosuchdir/|]
                             expf       = expd ⫻ basename f2
                         ([],([(f2,expf)],[expd])) @=? s
{-                         let (xs :⪭ b) = toSeqNE f2
                             lfe        = MonadIO.FStat.lfexists' @IOError
                         (expf,expd) ← case unsnoc xs of
                                 𝕹          → assertFailure $
                                                [fmt|no unsnoc of dir '%L'|] xs
                                 𝕵 (xs',_) → let d = fromSeqNE $ xs' :⪭ b
                                              in do ex ← ӝ $ lfe d
                                                    -- sanity check that
                                                    -- /foo/xyz does
                                                    -- not exist
                                                    NoFExists @=? ex
                                                    return (d ⫻ basename f2, d)

                         ([],([(f2,expf)],[expd])) @=? s
-}
                        )
                      ]

        -- Replace the first four characters of the filename with 'mclaren'
        -- of course, this presumes that the filename has at least four
        -- characters, which seems to be safe in practice.  But the whacky way
        -- of doing it means that it is sensitivel to the order of application;
        -- the REs should be applied in written order.
        , checkRunF "-F order"
                    [ "t\t^(/.*)/[^/]([^/]+)$\t${1}/mc${2}"
                    , "u\t^(/.*)/([^/]{2})[^/]([^/]+)$\t${1}/${2}la${3}"
                    , "v\t^(/.*)/([^/]{4})[^/]([^/]+)$\t${1}/${2}re${3}"
                    , "w\t^(/.*)/([^/]{6})[^/]([^/]+)$\t${1}/${2}n${3}"
                    ]
                    [ ("mclaren",
                       \ f2 s →
                         let f   = toText (f2 ⊣ file)
                             f'  = "mclaren" ⊕ drop 4 f
                             𝕽 p = FPath.Parseable.parse @_ @FPathError f'
                          in ([],([(f2,f2 & file ⊢ p)],[])) @=? s)
                    ]
        ]

--------------------------------------------------------------------------------

{- | unit tests -}
tests ∷ TestTree
tests = testGroup "Rename" [ Rename.RegexFile.tests, mainTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
