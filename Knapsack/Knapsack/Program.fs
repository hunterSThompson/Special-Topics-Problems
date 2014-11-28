// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

type item = {
    weight : float;
    value : float;
}

type knapsack = {
    limit: float;
    itmes: item List;
}

let sortByWeight lst =  
    lst |> List.sortBy (fun x -> x.value / x.weight) |> List.rev

//let rec inits (lst : int List) (accum : int List) : (int List) List =
let initsOuter lst =
    let rec inits lst accum =
        match lst with
        | head :: tail -> 
            let tempLst = accum @ [head]
            [tempLst] @ inits tail tempLst 
        | [] -> []
    inits lst []

let rec last = function
    | hd :: [] -> hd
    | hd :: tl -> last tl
    | _ -> failwith "Empty list."

let getSum (items : item List) = List.sumBy (fun x -> x.weight) items 

let findBest2 (nap:knapsack) =
    nap.itmes
    |> sortByWeight
    |> initsOuter
    |> List.filter (fun x -> getSum x <= nap.limit) 
    |> last

// Recursive way

let i1 = {weight = 4.0; value = 10.0;}
let i2 = {weight = 5.0; value = 10.0;}
let i3 = {weight = 8.0; value = 10.0;}
let i4 = {weight = 9.0; value = 10.0;}

let lst = [i1; i2; i3; i4];

let k = {
    limit = 10.0;
    itmes = lst;
}

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
