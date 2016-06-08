module Python where

import Text.Parsec

import qualified Imperative as I
import qualified Lambda as L

compile = undefined

block = ("{ " ++) . (++ " }")
wrapParens = ('(':) . (++ ")")
wrapSpace = (' ':) . (++ " ")
