let empty = []
let l1 = [1; 2; 3]
let l2  = [10 .. 20]
let l3 = [2 .. 2 .. 10]
let l4 = ["Hans"; "Fritz"]
let l5 = [1,2; 3,4; 4,4]

//element als head in Liste einfügen

let l10 = 20::l1

let l8 = 100::200::300::400::500::[]

//listen verknüpfen
let l11 = l2 @ l1
printfn $"{l11}"


//Alle werte in einer liste aufsummieren

let testlist = [2; 3; 5]
let rec summUp list =
    match list with
    | [] -> 0
    | head :: tail -> head + summUp tail
    
printfn $"{summUp testlist}"

//Mittelwert einer Liste berechnen

let rec count list =
    match list with
    | [] -> 0
    | head::tail -> 1 + count tail

let rec average list =  
    let sum = summUp list
    let count = count list
    float sum / float count
    
printfn $"Durchschnitt der Liste {average testlist}"

printfn $"Ungerade Zahlen von 3 - 11{[for z in [3 .. 11] do if z % 2 = 1 then yield z]}"
printfn $"7 reihe vorwärts {[for i in 1..10 do yield i * 7]}"
let reihe = [for i in 1..10 do yield i * 7]
let umgekehrteReihe = List.rev reihe

printfn $"7er Reihe rückwärts: {umgekehrteReihe}"

 //yield! fügt Liste in die neue Liste ein
let l23 = [for i in [1 .. 10] do yield! [i; i * i; i * i * i]]
printfn $"{l23}"
printfn $"a-z {['A' .. 'Z']}"

let zahlen = [1 .. 10]
let copy list = [ yield! list ]    //yield! fügt jeden Wert der Liste in die neue Liste ein
let zahlen1 = copy zahlen
printfn $"{zahlen1}"

//Eine Funktion, die aus einer Liste eine neue Liste mit Werten > n erstellt
let greaterThan n list  = [for y in list do if y > n then yield y]

// Eine Funktion, die aus einer Liste eine neue Liste mit Werten zwischen m und n erstellt

let betweenNM l n m = [for y in l do if y >= m && y <= n then yield y]

let geradeZahlen l = [ for y in l do if y % 2 = 0 then yield y]

let sumGeradeZahlen l =
    geradeZahlen l |> summUp
    
let ungeradeZahlen l = [for z in [3 .. 11] do if z % 2 = 1 then yield z]

let quadratZahlen l = [for y in l do yield y*y]

let sumKomplt l =
    ungeradeZahlen l |> quadratZahlen |>summUp
    
let rec quickSort = function
| [] -> []
| pivot::rest ->
    let linkeHälfte  = [for x in rest do if x <  pivot then yield x]
    let rechteHälfte = [for x in rest do if x >= pivot then yield x]
    
    (quickSort linkeHälfte) @ [pivot] @ (quickSort rechteHälfte)

let zahlen3 = [4; 1; 7; 6; 6; 1; 0; 3; 4]
let sortiert = quickSort zahlen3

let rec bubbleSort = function
| [] -> []
| [x] -> [x]
| x::y::rest ->
    if x > y then
        y :: bubbleSort (x :: rest)
    else
        x :: bubbleSort (y :: rest)

let zahlen4 = [4; 1; 7; 6; 6; 1; 0; 3; 4]
let sortiert2 = bubbleSort zahlen4

let l13 = [1.0 .. 0.5 .. 6.0]
printfn $"list with subtraktor {l13}"

let l14 = List.append [1; 2; 3] [4; 5; 6]
printfn $"list append {l14}"

//List Mapping

let l15 = List.map (fun x -> x*x ) l13
printfn $"list map and square it {l15}"

//Eine Funktion, die aus einer Liste eine neue Liste mit Werten > n erstellt 
let groesserAls n werte : int list = 
    werte |> List.filter (fun x -> x >= n) 

//Eine Funktion, die aus einer Liste eine neue Liste mit Werten zwischen m und n erstellt 
let zwischen m n werte : int list = 
    werte |> List.filter (fun x -> x >= m && x < n)
    
//Eine Funktion, die alle geraden Zahlen aus einer Liste summiert 
let summeGerade werte  = 
    werte |> List.filter (fun x -> x % 2 = 0) |> List.sum

//Eine Funktion, die aus einer Liste  eine neue Liste mit allen ungeraden Zahlen erstellt, dann eine neue Liste mit dem Quadrat der Zahlen erstellt, dann die Summe daraus ermittelt.  
let summeQuadrateUngerade (werte : int list) : int = 
    werte |> List.filter (fun x -> x % 2 <> 0) |> List.map (fun x -> x + x) |> List.sum

//normierte Liste from float
let l200 = [-5.0; 0.0; 10.0; 80.0; 100.0; 200.0]
let l201 = [-10.0; -5.0; 0.0]

let normList l1 =
    let max =  List.max l1
    match max with
    | 0.0 -> failwith "max ist 0"
    | _ ->  List.map (fun x -> x / max * 100.0) l1

printfn $"normierte liste1: {normList l200}"
printfn $"normierte liste2: {normList l201}"

//Wertebereich zwischen 2 Ganzzahlwerten
let filterRange l1 m n=
   List.filter (fun x -> x <= n && x >= m) l1

filterRange l200 10.0 100.0

//norm werteberiech mit gesamtzahlen
let normWertebereich l200 -5 


