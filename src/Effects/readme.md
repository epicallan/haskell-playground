# Design patterns

- On readerT based designs read [fp-complete guide](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) and
[Parsons blog](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)

- TO Read: https://slpopejoy.github.io/posts/Effectful02.html

Take aways from reading FP complete's guide

- Prefer ReaderT pattern and use local function incase you need to do some sort of mutation
- SnowMan suggests we can actually get rid of wrapping our app state in the ReaderT itself and use a MonadReader it self
- We should try to keep our code pure, don't un-necessarily use MonadThrow instead return a Maybe or Either.
One way of keeping code pure is to have 2 class instances of it. In which one we have a pure implentation and in the other an impure one. 

- So the take-away here is: if you can generalize your functions to mtl-style Monad constraints, do it, you'll regain a lot of the benfits you'd have with purity.


Take aways from [Micheal's blog](https://michaelxavier.net/posts/2016-04-03-Enterprise-Haskell-Pattern-Lensed-Reader.html)
___________


- Create “classy” lenses for your app’s state type and any subcomponents of it that you are likely to need to access independently.

- Use constraints throughout the code instead of concrete transformer stacks. You only end up specifying the stack near main where you actually run the thing.

- Try to use the minimal set of constraints needed for your functions. Low-level functions end up with smaller sets of constraints. Larger ones accumulate the combined constraints.

[Cardano guide to Monadic effects](https://github.com/input-output-hk/cardano-sl/blob/1d79a801936edeb4bde7f41187924bc59c7b9b20/docs/cardano-monads.rst)
___________

Notes: 


Side Notes
___________

```
data Env = Env
  { envLog :: !(String -> IO ())
  , envBalance :: !(TVar Int)
  }

makeLensesWith camelCaseFields ''Env
```

 ^^ here is another valid cool way of making lenses without adding '_' before every accessor

Usage:

```
modify :: (MonadReader env m, HasBalance env (TVar Int), MonadIO m)
       => (Int -> Int)
       -> m ()
modify f = do
  env <- ask
  liftIO $ atomically $ modifyTVar' (env^.balance) f
```

