{-# LANGUAGE UnicodeSyntax #-}

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty           ( defaultIngredients )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

--------------------------------------------------------------------------------

import Rename  ( tests )

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients tests

-- that's all, folks! ----------------------------------------------------------
