{-# LANGUAGE CPP, TemplateHaskell #-}
module Data.GADT.Show.TH
    ( DeriveGShow(..)
    ) where

import Control.Applicative
import Control.Monad
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
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
    deriveGShow (InstanceD overlaps cxt (AppT instType dataType) decs)
#else
    deriveGShow (InstanceD cxt (AppT instType dataType) decs)
#endif
        | headOfType instType == ''GShow = do
            let dataTypeName = headOfType dataType
            dataTypeInfo <- reify dataTypeName
            case dataTypeInfo of
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
                TyConI (DataD dataCxt name bndrs _ cons _) -> do
#else
                TyConI (DataD dataCxt name bndrs cons _) -> do
#endif
                    gshowDec <- gshowFunction cons
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
                    return [InstanceD overlaps cxt (AppT instType dataType) [gshowDec]]
#else
                    return [InstanceD cxt (AppT instType dataType) [gshowDec]]
#endif
                _ -> fail "deriveGShow: the name of an algebraic data type constructor is required"
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
    deriveGShow (DataD dataCxt name bndrs _ cons _) = return <$> inst
#else
    deriveGShow (DataD dataCxt name bndrs cons _) = return <$> inst
#endif
        where
            inst = instanceD (cxt (map return dataCxt)) (appT (conT ''GShow) (conT name)) [gshowDec]
            gshowDec = gshowFunction cons
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 612
#if __GLASGOW_HASKELL__ >= 800
    deriveGShow (DataInstD dataCxt name tyArgs _ cons _) = return <$> inst
#else
    deriveGShow (DataInstD dataCxt name tyArgs cons _) = return <$> inst
#endif
        where
            inst = instanceD (cxt (map return dataCxt)) (appT (conT ''GShow) (foldl1 appT (map return $ (ConT name : init tyArgs)))) [gshowDec]
            -- TODO: figure out proper number of family parameters vs instance parameters
            gshowDec = gshowFunction cons
#endif

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
