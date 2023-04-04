{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
module Language.Haskell.To.Elm.DataShape where

import Generics.SOP

type DataShape a = [(String, ConstructorShape a)]

data ConstructorShape a
  = ConstructorShape [a]
  | RecordConstructorShape [(String, a)]
  deriving Functor

nullary :: ConstructorShape a -> Bool
nullary (ConstructorShape []) = True
nullary _ = False

data Dict constraint where
  Dict :: constraint => Dict constraint

newtype ConstraintFun constraint a = ConstraintFun (forall t. Dict (constraint t) -> a)

dataShape
  :: forall typ constraint a
  . (All2 constraint (Code typ), HasDatatypeInfo typ)
  => Proxy typ 
  -> ConstraintFun constraint a
  -> DataShape a
dataShape _ f =
  constructorShapes @(Code typ) @constraint f $ constructorInfo $ datatypeInfo $ Proxy @typ

constructorShapes
  :: forall constrs constraint a
  . All2 constraint constrs
  => ConstraintFun constraint a
  -> NP ConstructorInfo constrs
  -> [(String, ConstructorShape a)]
constructorShapes f infos =
  case infos of
    Nil ->
      []

    info :* infos' ->
      constructorShape f info : constructorShapes f infos'

constructorShape
  :: forall constr constraint a
  . All constraint constr
  => ConstraintFun constraint a
  -> ConstructorInfo constr
  -> (String, ConstructorShape a)
constructorShape f info =
  case info of
    Constructor cname ->
      (cname, ConstructorShape $ constructorFieldShape f $ shape @_ @constr)

    Infix {} ->
      error "Infix constructors are not supported"

    Record cname fs ->
      (cname, RecordConstructorShape $ recordFieldShape f fs)

constructorFieldShape
  :: All constraint fields
  => ConstraintFun constraint a
  -> Shape fields
  -> [a]
constructorFieldShape f shape_ =
  case shape_ of
    ShapeNil ->
      []

    s@(ShapeCons _) -> go f s
      where
        go
          :: forall field fields constraint a
          . (constraint field, All constraint fields)
          => ConstraintFun constraint a
          -> Shape (field ': fields)
          -> [a]
        go f'@(ConstraintFun fun) (ShapeCons s') = fun @field Dict : constructorFieldShape f' s'

recordFieldShape
  :: forall fields constraint a
  . All constraint fields
  => ConstraintFun constraint a
  -> NP FieldInfo fields
  -> [(String, a)]
recordFieldShape f infos =
  case infos of
    Nil ->
      []

    info :* infos' ->
      go f info : recordFieldShape f infos'

      where
        go
          :: forall field
          . constraint field
          => ConstraintFun constraint a
          -> FieldInfo field
          -> (String, a)
        go (ConstraintFun fun) (FieldInfo fname) =
          (fname, fun @field Dict)
