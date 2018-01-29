Notes
_________

Type families allow us to declare type synonyms that do pattern-matching on their type arguments.

  type Foo m Int = [m]
  type Foo m Char = Maybe m
