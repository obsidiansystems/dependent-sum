{-# LANGUAGE CPP, TemplateHaskell #-}
module Data.GADT.Show.TH
    ( DeriveGShow(..)
    , DeriveShowTagIdentity(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Dependent.Sum
import Data.Dependent.Sum.TH.Internal
import Data.Functor.Identity
import Data.GADT.Show
import Data.Traversable (for)
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

-- A type class purely for overloading purposes
class DeriveShowTagIdentity t where
    deriveShowTagIdentity :: t -> Q [Dec]

instance DeriveShowTagIdentity Name where
    deriveShowTagIdentity typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveShowTagIdentity dec
            _ -> fail "deriveShowTagIdentity: the name of a type constructor is required"

instance DeriveShowTagIdentity Dec where
    deriveShowTagIdentity = deriveForDec ''ShowTag (\t -> [t| ShowTag $t Identity |]) showTaggedFunction

instance DeriveShowTagIdentity t => DeriveShowTagIdentity [t] where
    deriveShowTagIdentity [it] = deriveShowTagIdentity it
    deriveShowTagIdentity _ = fail "deriveShowTagIdentity: [] instance only applies to single-element lists"

instance DeriveShowTagIdentity t => DeriveShowTagIdentity (Q t) where
    deriveShowTagIdentity = (>>= deriveShowTagIdentity)

showTaggedFunction bndrs cons = funD 'showTaggedPrec $
    map (showTaggedClause bndrs) cons

showTaggedClause bndrs con = do
    let argTypes = argTypesOfCon con
        needsGShow argType = any ((`occursInType` argType) . nameOfBinder) (bndrs ++ varsBoundInCon con)

    argVars <- for argTypes $ \argType ->
        if needsGShow argType
        then Just <$> newName "x"
        else pure Nothing

    let nonWildPs = [x | Just x <- argVars]
        doRecur = case nonWildPs of
            [] -> Nothing
            [var] -> Just var
            (_:_) -> error "deriveShowTagIdentity: Can have at most one nested GADT"

    clause [ conP conName (maybe wildP varP <$> argVars)
           ]
        ( normalB $ case doRecur of
            Nothing -> [| showsPrec |]
            Just x -> [| showTaggedPrec $(varE x) |]
        ) []
    where conName = nameOfCon con
