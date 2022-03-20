module Rename.Error
  ( RenameError )
where

import Base1T

-- containers-plus ---------------------

import ContainersPlus.Map  ( AsRepeatedKeyError( _RepeatedKeyError )
                           , RepeatedKeyError )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ) )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError ( AsCreateProcError( _CreateProcError ) )
import MonadIO.Error.ProcExitError   ( AsProcExitError( _ProcExitError ) )

-- parsec-plus -------------------------

import Parsec.Error  ( AsParseError( _ParseError ) )

-- pcre --------------------------------

import PCRE.Error  ( AsREFnError( _REFnError ), AsREGroupError( _REGroupError )
                   , REFnGroupError )

-- stdmain -----------------------------

import StdMain.UsageError  ( AsUsageError( _UsageError )
                           , UsageParseFPProcIOError )

--------------------------------------------------------------------------------

data RenameError = RE_UFPPIO_ERROR UsageParseFPProcIOError
                 | RE_REFNG_ERROR  REFnGroupError
                 | RE_RK_ERROR     (RepeatedKeyError 𝕋)
  deriving Eq

_RE_UFPPIO_ERROR ∷ Prism' RenameError UsageParseFPProcIOError
_RE_UFPPIO_ERROR = prism' (\ e → RE_UFPPIO_ERROR e)
                          (\ case RE_UFPPIO_ERROR e → 𝕵 e; _ → 𝕹)

_RE_REFNG_ERROR ∷ Prism' RenameError REFnGroupError
_RE_REFNG_ERROR = prism' (\ e → RE_REFNG_ERROR e)
                         (\ case RE_REFNG_ERROR e → 𝕵 e; _ → 𝕹)

_RE_RK_ERROR ∷ Prism' RenameError (RepeatedKeyError 𝕋)
_RE_RK_ERROR = prism' (\ e → RE_RK_ERROR e) (\ case RE_RK_ERROR e → 𝕵 e; _ → 𝕹)

----------------------------------------

instance Exception RenameError

--------------------

instance Show RenameError where
  show (RE_UFPPIO_ERROR e) = show e
  show (RE_REFNG_ERROR  e) = show e
  show (RE_RK_ERROR     e) = show e

--------------------

instance Printable RenameError where
  print (RE_UFPPIO_ERROR e) = print e
  print (RE_REFNG_ERROR  e) = print e
  print (RE_RK_ERROR     e) = print e

--------------------

instance HasCallstack RenameError where
  callstack =
    let
      getter (RE_UFPPIO_ERROR e) = e ⊣ callstack
      getter (RE_REFNG_ERROR  e) = e ⊣ callstack
      getter (RE_RK_ERROR     e) = e ⊣ callstack
      setter (RE_UFPPIO_ERROR e) cs =
        RE_UFPPIO_ERROR (e & callstack ⊢ cs)
      setter (RE_REFNG_ERROR  e) cs =
        RE_REFNG_ERROR (e & callstack ⊢ cs)
      setter (RE_RK_ERROR     e) cs =
        RE_RK_ERROR (e & callstack ⊢ cs)
    in
      lens getter setter

----------------------------------------

instance AsREGroupError RenameError where
  _REGroupError = _RE_REFNG_ERROR ∘ _REGroupError

instance AsREFnError RenameError where
  _REFnError = _RE_REFNG_ERROR ∘ _REFnError

instance AsFPathError RenameError where
  _FPathError  = _RE_UFPPIO_ERROR ∘ _FPathError

instance AsCreateProcError RenameError where
  _CreateProcError  = _RE_UFPPIO_ERROR ∘ _CreateProcError

instance AsIOError RenameError where
  _IOError = _RE_UFPPIO_ERROR ∘ _IOError

instance AsProcExitError RenameError where
  _ProcExitError  = _RE_UFPPIO_ERROR ∘ _ProcExitError

instance AsParseError RenameError where
  _ParseError = _RE_UFPPIO_ERROR ∘ _ParseError

instance AsUsageError RenameError where
  _UsageError  = _RE_UFPPIO_ERROR ∘ _UsageError

instance AsRepeatedKeyError 𝕋 RenameError where
  _RepeatedKeyError  = _RE_RK_ERROR ∘ _RepeatedKeyError

-- that's all, folks! ----------------------------------------------------------
