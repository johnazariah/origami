// FUNCTIONS

// isEven : byte -> bool
let isEven (x : byte) = (x % 2uy = 0uy)

// Function Application
let thisShouldBeFalse    = isEven 3uy
let thisShouldBeTrue     = isEven 254uy
let thisShouldNotCompile = isEven 257uy

// Function Composition
//  square :: byte -> byte
let square (x : byte) = x * x

// Chained application
//  squareIsEven :: byte -> bool
let squareIsEven x =
    x
    |> square
    |> isEven

let thisShouldAlsoBeFalse = squareIsEven 3uy
let thisShouldAlsoBeTrue  = squareIsEven 4uy

// Composed function
//  squareIsEven' :: byte -> bool
let squareIsEven' x =
    x |> (square >> isEven)

let thisShouldAlsoBeFalse' = squareIsEven' 3uy
let thisShouldAlsoBeTrue'  = squareIsEven' 4uy

// Point-Free style
//  squareIsEven'' :: byte -> bool
// *** Use point-free style sparingly ***
let squareIsEven'' = (square >> isEven)
let thisShouldAlsoBeFalse'' = squareIsEven'' 3uy
let thisShouldAlsoBeTrue''  = squareIsEven'' 4uy

// There are some pros-and-cons to using composition
let input = [1uy..255uy]

// for n functions chained, there are n lists traversed
// intermediate lists need to be created and materialized
// functions are blocked and evaluated in sequence
let outputs =
    input
    |> List.map square
    |> List.map isEven

// for n functions composed, we get one list traversal
// no intermediate lists
// functions are evaluated together for each list item
// need to introduce another named function for readability
let outputs'' =
    input
    |> List.map squareIsEven''
