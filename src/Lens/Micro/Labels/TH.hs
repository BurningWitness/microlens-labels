{-# LANGUAGE TemplateHaskellQuotes #-}

module Lens.Micro.Labels.TH
  ( makeLabels
  ) where

import           Lens.Micro.Labels

import           Data.Function
import           Data.List
import           GHC.Records
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype



-- | Generates 'Label' instances for every record field in the datatype.
--
--   Unnamed fields are skipped without warning.
--
--   No transformations are applied to record field names.
makeLabels :: Name -> Q [Dec]
makeLabels name = do
  dt <- reifyDatatype name
  pure
    . fmap (uncurry . construct $ datatype dt)
    . nubBy ((==) `on` fst)
    . foldMap fields
    $ datatypeCons dt

datatype :: DatatypeInfo -> Type
datatype dt = foldl' AppT (ConT $ datatypeName dt) $ datatypeInstTypes dt

fields :: ConstructorInfo -> [(Name, Type)]
fields con =
  case constructorVariant con of
    RecordConstructor names -> zip names (constructorFields con)
    _                       -> []

construct :: Type -> Name -> Type -> Dec
construct dt fld typ =
  let str = LitT . StrTyLit $ nameBase fld
      f = mkName "f"
      s = mkName "s"
      b = mkName "b"

      cls = ConT ''Label
              `AppT` str
              `AppT` dt
              `AppT` typ

  in InstanceD Nothing [] cls
       [ PragmaD $ InlineP 'label Inline FunLike AllPhases
       , FunD 'label
           [ let app = LamE [VarP b] $ RecUpdE (ParensE $ SigE (VarE s) dt) [(fld, VarE b)]

                 expr = UInfixE
                          app
                          (VarE '(<$>))
                          (VarE f `AppE` (VarE 'getField `AppTypeE` str `AppE` VarE s))

             in Clause [VarP f, VarP s] (NormalB expr) []
           ]
       ]
