[Serokell proposed ReaderT method](https://github.com/input-output-hk/cardano-sl/blob/1d79a801936edeb4bde7f41187924bc59c7b9b20/docs/cardano-monads.rst#readert-over-abstract-base)

Summary
_______

- Avoid data types parametrized by an abstract monad m (including explicit method dictionaries), because it makes reasoning about code complicated

- Avoid hard-coded IO, MonadIO, MonadBaseControl IO, etc, in order to have good mocking capabilities and separation of concerns

- Operationally, our monads can be ReaderT ctx IO for production code and ReaderT ctx (CatchT ST) (or similar) in pure tests. Both of these provide (1) passing context, (2) throwing/catching exceptions, (3) mutable state.

