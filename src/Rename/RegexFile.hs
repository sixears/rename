{-# LANGUAGE BangPatterns #-}

{- | A regex file contains a list of regular expressions, with their
     replacements.  It is line-oriented.

     Each line must be either:
     -) 3 (posibly multiple) tab-separated columns, being name,regex,replacement.
        The name must be a simple identifier ([[:alpha:]]([\w_]*).
        The names must be unique.
     -) A sequence of space/tab characters
     -) A comment, beginning with '#'.
 -}

module Rename.RegexFile
  ( NamedREPlacements, toList', tests )
where

import Base1T

-- base --------------------------------

import Data.Maybe          ( catMaybes )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map

-- containers-plus ---------------------

import ContainersPlus.MapUtils  ( AsMapDupKeyError, MapDupKeyError
                                , fromListDupsE )

-- monaderror-io -----------------------

import MonadError  ( leftFailP )

-- parsec ------------------------------

import Text.Parsec.Prim  ( Parsec, Stream )

-- parsec-plus -------------------------

import Parsec.Error  ( ParseError )
import ParsecPlus    ( Parsecable( parser ), parse, testParsecFile )

-- parsers -----------------------------

import Text.Parser.Char         ( char, newline, noneOf )
import Text.Parser.Combinators  ( eof, optional, sepBy, try )

-- parser-plus -------------------------

import ParserPlus  ( stringMaybeDQuoted, whitespaces )

-- pcre --------------------------------

import PCRE.REPlacement  ( REPlacement( REPlacement ) )
import PCRE.ReplText     ( ReplText, repltext )

-- regex-with-pcre ---------------------

import Text.RE.PCRE.Text  ( RE, re )

-- text --------------------------------

import Data.Text  ( pack, unlines, unpack )

--------------------------------------------------------------------------------

parseRegexLine ∷ Stream σ Identity ℂ ⇒ Parsec σ χ (𝕄 (𝕋,REPlacement))
parseRegexLine =
  let spc = whitespaces ⋪ optional (char '#' ⋪ many (noneOf "\n"))
      namedREPl ∷ Stream σ' Identity ℂ ⇒ Parsec σ' ξ (𝕋, REPlacement)
      namedREPl = (,) ⊳ (pack ⊳ stringMaybeDQuoted ⋪ many (char '\t')) ⊵ parser
  in
    try (𝕵 ⊳ (namedREPl) ⋪ spc) ∤ (pure 𝕹 ⊳ spc)


parseRegexLineTests ∷ TestTree
parseRegexLineTests =
  let checkN s =
        testCase (show s) $
          𝕽 𝕹 @=? parse @ParseError (parseRegexLine ⋪ eof) "test" (pack s)
      check ex@(nm,_) s =
        testCase (unpack nm) $
          𝕽 (𝕵 ex) @=? parse @ParseError (parseRegexLine ⋪ eof) "test" (pack s)
   in testGroup "parseRegexLine"
        [ checkN "", checkN "\t", checkN "\t  \t", checkN "  \t  "
        , check ("rep1_0",rep1_0) (ю [ "rep1_0\t\t${iggy}(fo+)${pop}(.ar)\t\""
                                     , ">>${pop}<< (${1}) [${0}]\"" ])
        , check ("rep3_3",rep3_3) (ю [ "rep3_3\t\"(?<=/)foo\\\\.*(.{3})\"\t\t"
                                     , "\"quux.${.title 1}/\"  # c" ])
        ]

{-| map of regexen, e.g., as found in a regex file -}
-- We maintain the input list here, as well as the map; to preserve order;
-- that is, toList' (NamedREPlacements (fromList xs) xs) ≡ xs.  This is so that,
-- e.g., when reading in a list(map) of named replacements from a file; we can
-- both index into the list (with dup detection) but also iterate across the
-- list (in its incoming order)
data NamedREPlacements = NamedREPlacements { _repMap  ∷ Map.Map 𝕋 REPlacement
                                           , _repList ∷ [(𝕋,REPlacement)]
                                           }
  deriving (Eq,Show)

namedREPlacements ∷ ∀ ε η . (AsMapDupKeyError 𝕋 REPlacement ε, MonadError ε η) ⇒
                    [(𝕋,REPlacement)] → η NamedREPlacements
namedREPlacements xs = (\ m → NamedREPlacements m xs) ⊳ fromListDupsE xs

namedREPlacements' ∷ MonadError (MapDupKeyError 𝕋 REPlacement) η ⇒
                     [(𝕋,REPlacement)] → η NamedREPlacements
namedREPlacements' = namedREPlacements

instance Parsecable NamedREPlacements where
  parser =
    namedREPlacements' ∘ catMaybes ⊳ parseRegexLine `sepBy` newline ≫ leftFailP

instance HasIndex NamedREPlacements where
  type Indexer NamedREPlacements = 𝕋
  type Elem    NamedREPlacements = REPlacement
  index t n = t `Map.lookup` (_repMap n)

toList' ∷ NamedREPlacements → [(𝕋,REPlacement)]
toList' = _repList

--------------------

{-| unit tests for parsec on a regex file -}
parsecableFileTests ∷ TestTree
parsecableFileTests =
  let txt = unlines [ "\t# hello, mum!"
                    , "# x\t\"(.*)\"\t\"${.tr 1}\""
                    , "foofoo\t(?<=/)foo\\.*(.{3})\t\"quux.${.title 1}/\""
                    ,   "stooge\t\"${iggy}(fo+)${pop}(.ar)\"\t\t"
                      ⊕ "\">>${pop}<< (${1}) [${0}]\""
                    ]
      lst = [("foofoo", rep3_3), ("stooge", rep1_0)]
   in testGroup "parsecFileUTF8" $
        [ testParsecFile txt $ NamedREPlacements (fromList lst) lst
        ]

------------------------------------ tests -------------------------------------

------------------------------------------------------------
--                       test data                        --
------------------------------------------------------------

re1 ∷ RE
re1 = [re|${iggy}(fo+)${pop}(.ar)|]
re3 ∷ RE
re3 = [re|(?<=/)foo\.*(.{3})|]

repl0 ∷ ReplText
repl0 = [repltext|>>${pop}<< (${1}) [${0}]|]

repl3 ∷ ReplText
repl3 = [repltext|quux.${.title 1}/|]

rep1_0 ∷ REPlacement -- s/${iggy}(fo+)${pop}(.ar)/>>${pop}<< (${1}) [${0}]/
rep1_0 = REPlacement re1 repl0

rep3_3 ∷ REPlacement -- s/(?<=/)foo\.*(.{3})$/quux.${.title 1}\//
rep3_3 = REPlacement re3 repl3

----------------------------------------

{- | unit tests -}
tests ∷ TestTree
tests = testGroup "Rename.RegexFile" [ parseRegexLineTests
                                     , parsecableFileTests
                                     ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
