﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

type Color = int option

type Node = {
    Color : Color
    // Maybe index too.
}

type Edge = {
    Src : Node;
    Dst : Node;
}

let isNeighbor edge index = 
    if edge.Src = index || edge.Dst = index then
        true 
    else 
        false

//
// Concat implementation because I can't figure out how to use List.concat (sigh)
//
let foldFunc state T = state @ T
let concat lst = List.fold foldFunc [] lst

//
// If this edge contains the node, return the neighbording node.
// If not, return an empty list
//
let getNeighbor (edge:Edge) (node:Node) =
    if edge.Src = node then
        [edge.Dst]
    elif edge.Dst = node then
        [edge.Src]
    else
        []

//
// This currently returns a list of lists. Can't figure out how to use List.concat. Sigh.
//
let getNebNodes target lst = lst |> List.map (fun x -> getNeighbor x target) |> concat




let getNumNeighbors target lst = lst |> getNebNodes target |> List.length

let getContainingEdges lst index = lst |> List.filter (fun x -> isNeighbor x index) 
let getNumNodes lst index = lst |> List.filter (fun x -> isNeighbor x index) |> List.length 


//
// Sort nodes by number of neighbors
//
let sortNodes (nodes:Node List) (es : Edge List) : Node List = 
    nodes |> List.sortBy (fun x -> getNumNodes es x) |> List.rev

//
// Check conflicts of neighbors if it is set to this Color.
// Will return true if there are conflics.
//
let checkConflicts (target:Node) (es:Edge List) (ns:Node List) (color:Color) : bool =
    let rec checkNodes nodes =
        match nodes with
        | head::tail -> 
            let nebs = getNebNodes head es
            if List.exists (fun x -> x.Color = target.Color) nebs then
                true
            else
                checkNodes tail
        | [] -> failwith "Unconnected node."
    checkNodes ns



//
// Solving Algorithm:
//
//  1) sort nodes by num of neighbors
//  2) foreach nodes...
//  3) If colored continue, otherwise set to first color
//  4) foreach neighbor of node
//  5)    Try each color... if no conflicts of this nodes neighbors then take it. If not continue. If none found w/ conflicts, take last color.
//


let n1 = { Color = Some 1; }
let n2 = { Color = Some 2; }
let n3 = { Color = Some 3; }
let n4 = { Color = Some 4; }
let n5 = { Color = Some 5; }
let n6 = { Color = Some 6; }

let e1 = { Src = n1; Dst = n2; }
let e2 = { Src = n1; Dst = n3; }
let e3 = { Src = n1; Dst = n4; }
let e4 = { Src = n1; Dst = n5; }
let e5 = { Src = n3; Dst = n4; }
let e6 = { Src = n2; Dst = n6; }

let ns = [n1; n2; n3; n4; n5; n6]
let es = [e1; e2; e3; e4; e5; e6]

//let v = getContainingEdges es n1
let v = getNebNodes n1 es 

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
    