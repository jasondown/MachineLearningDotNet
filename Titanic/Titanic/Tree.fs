namespace Titanic

module Tree =
    
    type Feature<'a> = 'a -> string Option
    type NamedFeature<'a> = string * Feature<'a>
    type Label<'a, 'b when 'b:equality> = 'a -> 'b

    type Tree<'a, 'b when 'b:equality> =
        | Answer of 'b
        | Stump of NamedFeature<'a> * string * Map<string, Tree<'a, 'b>>
