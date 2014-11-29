// Learn more about F# at http://fsharp.net
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
let getNebNodes target = List.map (fun x -> getNeighbor x target)

let getContainingEdges lst index = lst |> List.filter (fun x -> isNeighbor x index) 


let col = Some 1
let n1 = { Color = col; }
let n2 = { Color = Some 2; }
let n3 = { Color = Some 3; }
let n4 = { Color = Some 4; }

let e1 = { Src = n1; Dst = n2; }
let e2 = { Src = n1; Dst = n3; }
let e3 = { Src = n1; Dst = n4; }

let es = [e1; e2; e3;]

//let v = getContainingEdges es n1
let v = getNebNodes n1 es 
let x = List.concat 

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
    