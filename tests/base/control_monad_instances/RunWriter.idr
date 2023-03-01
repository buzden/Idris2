module RunWriter

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

inner : MonadWriter (List Nat) m => m String
inner = do
  tell [10, 20]
  pass $ do
    tell [100]
    pure ("inner", map S)

prog : MonadWriter (List Nat) m => m String
prog = do
  tell [1000, 2000, 3000]
  (x, w) <- listen inner
  tell [4000]
  pure "outer, \{x}, \{show w}"

main : IO ()
main = do
  putStrLn "--- alone ---"
  putStrLn $ show $ runWriter $ prog {m=Writer (List Nat)}
  putStrLn ""
  putStrLn "--- on top of 2 ---"
  putStrLn $ show $ runState Z $ runWriterT $ prog {m=WriterT (List Nat) $ State Nat}
  putStrLn ""
  putStrLn "--- on bottom of 2 ---"
  putStrLn $ show $ runWriter $ runStateT Z $ prog {m=StateT Nat $ Writer (List Nat)}
