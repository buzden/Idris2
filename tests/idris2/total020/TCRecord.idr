%default total

record Id a where
  constructor MkId
  field : a

data X = Stop Nat | Cont (Id X)

f : X -> Nat
f $ Stop n = n
f $ Cont x = f x.field
