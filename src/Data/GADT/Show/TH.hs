{-# LANGUAGE CPP, TemplateHaskell #-}
module Data.GADT.Show.TH
    ( DeriveGShow(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Dependent.Sum.TH.Internal
import Data.GADT.Show
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Extras

class DeriveGShow t where
    deriveGShow :: t -> Q [Dec]

instance DeriveGShow Name where
    deriveGShow typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveGShow dec
            _ -> fail "deriveGShow: the name of a type constructor is required"

instance DeriveGShow Dec where
    deriveGShow = deriveForDec ''GShow (\t -> [t| GShow $t |]) $ \_ -> gshowFunction

instance DeriveGShow t => DeriveGShow [t] where
    deriveGShow [it] = deriveGShow it
    deriveGShow _ = fail "deriveGShow: [] instance only applies to single-element lists"

instance DeriveGShow t => DeriveGShow (Q t) where
    deriveGShow = (>>= deriveGShow)

gshowFunction = funD 'gshowsPrec . map gshowClause

gshowClause con = do
    let conName  = nameOfCon con
        argTypes = argTypesOfCon con
        nArgs    = length argTypes

        precName = mkName "p"

    argNames <- replicateM nArgs (newName "x")

    let precPat = if null argNames
          then wildP
          else varP precName

    clause [precPat, conP conName (map varP argNames)]
        (normalB (gshowBody (varE precName) conName argNames)) []

showsName name = [| showString $(litE . stringL $ nameBase name) |]

gshowBody prec conName [] = showsName conName
gshowBody prec conName argNames =
    [| showParen ($prec > 10) $( composeExprs $ intersperse [| showChar ' ' |]
        ( showsName conName
        : [ [| showsPrec 11 $arg |]
          | argName <- argNames, let arg = varE argName
          ]
        ))
     |]
