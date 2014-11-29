//
// Hunt Graham
// Knapsack problem, greedy algorithm heuristic
//

type item = {
    weight : float;
    value : float;
}

type knapsack = {
    limit: float;
    itmes: item List;
}

//
// Sort items by the value to weight ratio.
//
let sortByRatio lst =  
    lst |> List.sortBy (fun x -> x.value / x.weight) |> List.rev


//
// Returns all initial segments of passed list
// For example [1, 2, 3] -> [ [1], [1,2], [1,2,3] ]
//
let inits lst =
    let rec initRec lst accum =
        match lst with
        | head :: tail -> 
            let tempLst = accum @ [head]
            [tempLst] @ initRec tail tempLst 
        | [] -> []
    initRec lst []

//
// Returns the last element of a list. Why is this not a standard library function?
//
let rec last = function
    | hd :: [] -> hd
    | hd :: tail -> last tail
    | _ -> failwith "Empty list."

let getSum = List.sumBy (fun x -> x.weight)

//
// Slower solve function. Wouldn't work for bigger solution sets.
//
let findBestSlow (nap:knapsack) =
    nap.itmes
    |> sortByRatio
    |> inits
    |> List.filter (fun x -> getSum x <= nap.limit) 
    |> last

//
// Solver function, orders items by value to weight ratio, then
// take items until weight limit is hit.
//
let findBestRec (nap:knapsack) = 
    let rec findRec items sum accum =
        match items with
        | head :: [] -> 
            if head.weight < nap.limit then (accum @ [head]) else accum
        | head :: tail ->  
            let newSum = sum + head.weight
            if newSum < nap.limit then findRec tail newSum (accum @ [head]) else accum
        | [] -> failwith "Empty list!"
    let sortedList = sortByRatio nap.itmes
    findRec sortedList 0.0 []
    

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
    let res = findBestRec k
    printfn "%A" argv
    0 // return an integer exit code
