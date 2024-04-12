module Ports
  ( MonadPorts (..)
  , MonadLocalPorts (..)
  , Has
  , HasAll
  , ask
  ) where

import Ports.Adapters (Adapters)
import Ports.Adapters qualified as Adapters

import Control.Monad.Reader qualified as Reader
import Data.Kind (Constraint, Type)


class Monad m => MonadPorts m where
  type Ports m :: [Type]
  askAdapters :: m (Adapters (Ports m))


class MonadPorts m => MonadLocalPorts m where
  localAdapters :: (Adapters (Ports m) -> Adapters (Ports m)) -> m a -> m a


type Has m port =
  ( MonadPorts m
  , Adapters.CanFind port (Ports m)
  )


type family HasAll m portsYouWant :: Constraint where
  HasAll _ '[] = ()
  HasAll m (p ': ps) = (Has m p, HasAll m ps)


ask :: forall port m. Has m port => m port
ask =
  Adapters.find @port <$> askAdapters


instance Monad m => MonadPorts (Reader.ReaderT (Adapters ports) m) where
  type Ports (Reader.ReaderT (Adapters ports) m) = ports
  askAdapters = Reader.ask


instance Monad m => MonadLocalPorts (Reader.ReaderT (Adapters ports) m) where
  localAdapters = Reader.local
