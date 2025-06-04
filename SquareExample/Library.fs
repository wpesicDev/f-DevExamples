module SquareExample

//robutste variante
let rec power (x: float) (n: int) = 
    match n with 
    | 0 -> 1.0 
    | _ when n > 0 -> x * power x (n - 1) 
    | _ when n < 0 -> 1.0 / power x (-n)
    
//einfache Variante
let rec powerEasy x n = 
    match n with 
    | 0 -> 1.0 
    | _   ->  x * power x (n - 1) 