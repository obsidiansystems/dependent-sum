{-# LANGUAGE TemplateHaskell #-}
module Data.GADT.Show.TH
    ( DeriveGShow(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.GADT.Show
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Utils

class DeriveGShow t where
    deriveGShow :: t -> Q [Dec]

instance DeriveGShow Name where
    deriveGShow typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveGShow dec
            _ -> fail "deriveGShow: the name of a type constructor is required"

instance DeriveGShow Dec where
    deriveGShow (InstanceD cxt (AppT instType dataType) decs)
        | headOfType instType == ''GShow = do
            let dataTypeName = headOfType dataType
            dataTypeInfo <- reify dataTypeName
            case dataTypeInfo of
                TyConI (DataD dataCxt name bndrs cons _) -> do
                    gshowDec <- gshowFunction cons
                    return [InstanceD cxt (AppT instType dataType) [gshowDec]]
                _ -> fail "deriveGShow: the name of an algebraic data type constructor is required"
    deriveGShow (DataD dataCxt name bndrs cons _) = return <$> inst
        where
            inst = instanceD (cxt (map return dataCxt)) (appT (conT ''GShow) (conT name)) [gshowDec]
            gshowDec = gshowFunction cons

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
    
    clause [varP precName, conP conName (map varP argNames)]
        (normalB (gshowBody (varE precName) conName argNames)) []

showsName name = [| showString $(litE . stringL $ nameBase name) |]

gshowBody prec conName [] = showsName conName
gshowBody prec conName argNames = 
    [| showParen ($prec > 10) $( compose $ intersperse [| showChar ' ' |]
        ( showsName conName
        : [ [| showsPrec 11 $arg |]
          | argName <- argNames, let arg = varE argName
          ]
        )) 
     |]
