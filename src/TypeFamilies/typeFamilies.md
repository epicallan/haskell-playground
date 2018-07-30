Notes
_________

Type families allow us to declare type synonyms that do pattern-matching on their type arguments.

  type Foo m Int = [m]
  type Foo m Char = Maybe m

Slide on [type families](https://cdepillabout.github.io/haskell-type-families-presentation)

Type families permit a program to compute what data constructors it will operate on, rather than having them fixed statically (as with simple type systems) or treated as opaque unknowns (as with parametrically polymorphic types).


In ordinary Haskell, a type class can associate a set of methods with a type. The type families extension will now allow us to associate types with a type.