module Control.Monad.RWS.Interface

import Control.Monad.RWS.CPS
import Control.Monad.Reader.Interface
import Control.Monad.State.Interface
import Control.Monad.Writer.Interface
import Control.Monad.Trans

%default total

public export
interface (MonadReader r m, MonadWriter w m, MonadState s m) =>
  MonadRWS r w s m | m where

public export
(Monoid w, Monad m) => MonadRWS r w s (RWST r w s m) where

public export
[Trans] MonadRWS r w s m => Monoid w => StrongMonadTrans t => Monad (t m) => MonadRWS r w s (t m)
  using Reader.Interface.Trans State.Interface.Trans Writer.Interface.Trans where
