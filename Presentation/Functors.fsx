// FUNCTORS
// lots more from https://en.wikibooks.org/wiki/Haskell/Applicative_functors

// A functor is an augmented type - a container with context
type Maybe<'t> =
    | None
    | Some of 't

// For it to be a functor, the following functions should exist
type Maybe<'t> with
    member this.Map (f : 't -> 'r) : Maybe<'r> =
        match this with
        | None   -> Maybe<'r>.None
        | Some t -> f t |> Maybe<'r>.Some

// Strictly speaking, the map function needs to satisfy certain laws
// map id = id                   (1st functor law)
// map (g ∙ f) = map g ∙ map f (2nd functor law)
// You can work out that the given `map` function satisfies these laws

// now lets say we want to operate on a functor instance `ma`...
// ma :: Maybe<int>
let ma = Some 3

// ... with the function `double`
// double :: int -> int
let double x = x * 2

// this should not compile...
Some 3 |> double

// ...but this should work
let ma_doubled = ma.Map double

// BEING CUTE: let's define `map` as an infix operator
let (<|>) (ma : Maybe<'a>) (f : 'a -> 'b) : Maybe<'b> = ma.Map f

let ma_doubled' = ma <|> double

// so now we have an analogous operator for functor application as we do function application
let six    = 3       |> double
let ma_six = Some 3 <|> double

// APPLICATIVES
// or APPLICATIVE FUNCTORS

// If we want to apply a function of more than one argument on a functor
// we have a problem

let three = Some 3
let four  = Some 4

// this won't compile
let seven = Some 3 + Some 4

// this comes the closest
let addThree : Maybe<int -> int> = (three <|> (+))
let seven = addThree <|> four // still doesn't compile

// we need a function that looks like this:
// F<'a -> 'b> -> F<'a> -> F<'b>
// or with the arguments flipped
// F<'a> -> F<'a -> 'b> -> F<'b>

type Maybe<'t> with
    member this.Apply<'r> (f : Maybe<'t -> 'r>) : Maybe<'r> =
        match f with
        | Some op -> this.Map op
        | None    -> Maybe<'r>.None

// so now we can write
let seven = four.Apply (addThree)

// again, we can write an infix operator for apply
let (<*>) mf (ma : Maybe<'a>) = ma.Apply mf

// so we can write
let seven = addThree <*> four
// which is really
let seven = (three <|> (+)) <*> four

// ok now it's looking like operator soup...let's try to clean it up a notch

// let's add a function to "lift" a function into Maybe so we can clean up the (three <|> (+))
type Maybe<'t> with
    static member Pure (t : 't) : Maybe<'t> =
        Some t

// that way we can first lift (+) into Maybe
let mplus = Maybe<_>.Pure (+)

// now we can apply away
let seven = mplus <*> three <*> four

// notice that we actually didn't use <|> anymore. This is because <|> and <*> are related
// x <|> f = pure f <*> x

// MONADS
// "Railway Lines"
// let's add a function to allow us to operate on the inner value of the structure
type Maybe<'t> with
    member this.Bind<'r>(f : 't -> Maybe<'r>) : Maybe<'r> =
        match this with
        | Some t -> f t
        | None -> Maybe<'r>.None

// this allows us to operate on the inner value of the Maybe without having to extract it first
let four = three.Bind(fun x -> Some (x + 1))

// again, an infix version of this looks very interesting - and similar to chaining
let (>>=) (ma : Maybe<'a>) (f : 'a -> Maybe<'b>) : Maybe<'b> =
    ma.Bind f

// let's add a function to

// so consider these two functions
let square x = x * x
let double x = x + x

// chain-applied normally
let c32 =
    square 4
    |> double

// now with Maybe
let msquare   = Maybe<_>.Pure square
let mdouble x = Maybe<_>.Pure (double x)
let m32 =
    msquare <*> (Some 4)
    >>= mdouble

