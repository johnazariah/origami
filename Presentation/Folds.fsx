// FOLDS
// "Origami"

// We typically encounter folds in the context of a collection,
// and generally use them for aggregation - like so
let result =
    [1..10000000]
    |> List.map float
    |> List.fold (+) 0.

// however. we can also consider them an augmented type
type Fold<'seed, 'item, 'result> =
    {
        Seed       : 'item
        Accumulate : 'item -> 'seed -> 'item
        Finish     : 'item -> 'result
    }
with
    // let's add a neater constructor...
    static member Apply (s, a, f) =
        { Seed = s; Accumulate = a; Finish = f; }

    // this is handy to do a final map of the folded value...
    member this.Map f =
        { Seed = this.Seed; Accumulate = this.Accumulate; Finish = this.Finish >> f }

    // this is how we combine two folds to form another fold...
    member this.Apply other =
        let accumulate result curr =
            ((this.Accumulate (fst result) curr), (other.Accumulate (snd result) curr))
        let finish result =
            ((fst >> this.Finish) result, (snd >> other.Finish) result)
        {
            Seed       = (this.Seed, other.Seed)
            Accumulate = accumulate
            Finish     = finish
        }

// now let's add some convenience functions and infix operators
let newFold s a f = Fold<_,_,_>.Apply (s, a, f)
let (<!>) (fold : Fold<_,_,_>) f = fold.Map f
let (<*>) (fold1 : Fold<_,_,_>) (fold2 : Fold<_,_,_>) = fold1.Apply fold2

// and finally, a convenience function to apply this structure to a List
let foldList fold = List.fold fold.Accumulate fold.Seed >> fold.Finish


// here are some test case functions

// increment by 1, float
let inc n _ = n + 1.

// divide two floats
let divide (n, d) = (float(n)/float(d))

// a summing Fold
let sum   = newFold 0. (+) id

// a counting Fold
let count = newFold 0. inc id

// a sumOfSquares Fold
let sumOfSquares = newFold 0. (fun result curr -> result + (curr ** 2.)) id

// a sigma function
let sigma ((sum, count), sumSq) = sqrt(sumSq / count - (sum / count)**2.)

// now compose an average Fold from the sum and count Folds
let avg = sum <*> count <!> divide

// and a stddev Fold from the sum, count and sumOfSquares Folds
let stdDev = sum <*> count <*> sumOfSquares <!> sigma

// at this point, all we have done is COMPOSE folds - we haven't run anything yet

// now iterate over the input *ONCE* and compute everything, outputting both stddev and avg
let stdDevAndAverage =
    [1..10000000]
    |> List.map float
    |> foldList (stdDev <*> avg)

printf "Standard Deviation, Average is %A" stdDevAndAverage