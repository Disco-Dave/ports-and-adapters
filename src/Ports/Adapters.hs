{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ports.Adapters
  ( Adapters
  , CanAdd
  , CanCombine
  , Combine
  , CanFind
  , empty
  , push
  , append
  , combine
  , combineFlipped
  , find
  , update
  ) where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.TypeError (TypeError)
import GHC.TypeError qualified as TypeError
import GHC.TypeLits (KnownNat, natVal)
import Unsafe.Coerce (unsafeCoerce)

import Fcf
  ( Eval
  , Find
  , FindIndex
  , FromMaybe
  , IsNothing
  , TyEq
  , type (++)
  , type (=<<)
  )


type FoundError (port :: Type) (ports :: [Type]) =
  TypeError
    ( 'TypeError.Text "Type "
        'TypeError.:<>: 'TypeError.ShowType port
        'TypeError.:<>: 'TypeError.Text " already exists in "
        'TypeError.:<>: 'TypeError.ShowType ports
    )


type NotFoundError (port :: Type) (ports :: [Type]) =
  TypeError
    ( 'TypeError.Text "Type "
        'TypeError.:<>: 'TypeError.ShowType port
        'TypeError.:<>: 'TypeError.Text " cannot be found in "
        'TypeError.:<>: 'TypeError.ShowType ports
    )


-- | Allows you to add a 'port' in the list of 'ports'.
type CanAdd (port :: Type) (ports :: [Type]) =
  TypeError.Assert
    (Eval (IsNothing =<< Find (TyEq port) ports))
    (FoundError port ports)


-- | Allows you to combine two sets of adapters.
type family CanCombine (firstPorts :: [Type]) (secondPorts :: [Type]) :: Constraint where
  CanCombine '[] _ = ()
  CanCombine (port ': firstPorts) secondPorts =
    ( CanAdd port firstPorts
    , CanCombine firstPorts secondPorts
    )


type family Combine (nestedPorts :: [[Type]]) :: [Type] where
  Combine '[] = '[]
  Combine (p ': ps) = Eval (p ++ Combine ps)


type ToIndex (port :: Type) (ports :: [Type]) =
  Eval (FromMaybe (NotFoundError port ports) =<< FindIndex (TyEq port) ports)


-- | Allows to find a 'port' in the list of 'ports'.
type CanFind (port :: Type) (ports :: [Type]) =
  KnownNat (ToIndex port ports)


data Adapter where
  Adapter :: adapter -> Adapter


-- | Container of adapters.
newtype Adapters (ports :: [Type])
  = Adapters (Vector Adapter)


-- | Empty set of adapters.
empty :: Adapters '[]
empty =
  Adapters Vector.empty


-- | Bind a 'port' and add it to the beginning of the list.
push
  :: CanAdd port ports
  => port
  -> Adapters ports
  -> Adapters (port ': ports)
push port (Adapters ports) =
  Adapters $ Vector.cons (Adapter port) ports


-- | Bind a 'port' and add it to the back of the list.
append
  :: CanAdd port ports
  => port
  -> Adapters ports
  -> Adapters (Eval (ports ++ '[port]))
append port (Adapters ports) =
  Adapters $ Vector.snoc ports (Adapter port)


-- | Combine two sets of adapters.
combine
  :: CanCombine firstPorts secondPorts
  => Adapters firstPorts
  -> Adapters secondPorts
  -> Adapters (Combine '[firstPorts, secondPorts])
combine (Adapters firstPorts) (Adapters secondPorts) =
  Adapters $ firstPorts <> secondPorts


-- | Same as 'combine' but with its arguments flipped.
combineFlipped
  :: CanCombine firstPorts secondPorts
  => Adapters secondPorts
  -> Adapters firstPorts
  -> Adapters (Combine '[firstPorts, secondPorts])
combineFlipped =
  flip combine


toIndex
  :: forall port ports
   . CanFind port ports
  => Int
toIndex =
  fromIntegral @_ @Int . natVal $
    Proxy @(ToIndex port ports)


-- | Find the adapter for your 'port'.
find
  :: forall port ports
   . CanFind port ports
  => Adapters ports
  -> port
find (Adapters vector) =
  let
    unwrap (Adapter b) =
      unsafeCoerce b
   in
    unwrap . Vector.unsafeIndex vector $
      toIndex @port @ports


update
  :: forall port ports
   . CanFind port ports
  => Adapters ports
  -> (port -> port)
  -> Adapters ports
update ports@(Adapters vector) f =
  let
    updatedPort = f $ find @port ports
   in
    Adapters . Vector.unsafeUpd vector $
      [
        ( toIndex @port @ports
        , Adapter updatedPort
        )
      ]
