// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.IO;

type Color = int option

type Node = {
    mutable Color: Color
    Index: int
}

type Edge = {
    Src: Node ref;
    Dst: Node ref;
}

type Graph = {
    numColors: int;
    edges: Edge List
    nodes: Node ref List
}



//
// Take subset of a list by start/end index. Won't work with generic for some reason
//
let takeRange (lst: List<string>) start _end =
    if start < 0 || _end > lst.Length-1 then
        failwith "index out of range."
    else
        [start.._end] |> List.map (fun x -> lst.[x])
(*
let takeRange<'T> (lst: List<'T>) start _end =
    if start < 0 || _end > lst.Length-1 then
        failwith "index out of range."
    else
        [start.._end] |> List.map (fun x -> lst.[x])
*)


//
// Parse functions
//
let split (str: string) = str.Split [|' ';|] 
let trim (str: string) = str.Trim()

let parse line (nodes: Node ref List) =
    let splits = line |> split
    let src = splits.[0] |> trim |> int
    let dst = splits.[1] |> trim |> int
    {Src = nodes.[src]; Dst = nodes.[dst] }

let createNodes (n: int) =
    [0..n-1] |> List.map (fun x -> ref {Color = None; Index = x})

let readFile filename =
    let allLines = [for i in File.ReadAllLines filename -> i] // Read all lines
    let numColors = allLines.[0] |> int // Read number of colors
    let edgePairs = takeRange allLines 1 (allLines.Length-1) // Get remaining lines
    let nodes = createNodes (allLines.Length - 1) // Create node list
    let edges = edgePairs |> List.map (fun x -> parse x nodes) // Parse each line
    { numColors = numColors; edges = edges; nodes = nodes} // Create graph object

//let read = readFile "C:\Users\Hunt\Documents\Visual Studio 2013\Projects\Special Topic Problems\Greedy Coloring\Greedy Coloring\Node.txt"


//
// Check if this node is in this edge
//
let isNeighbor edge index = 
    edge.Src = index || edge.Dst = index

//
// Concat implementation because I can't figure out how to use List.concat (sigh)
//
let foldFunc state T = state @ T
let concat lst = List.fold foldFunc [] lst


//
// If this edge contains the node, return the neighbording node.
// If not, return an empty list
//
let getNeighbor (edge: Edge) (node: Node) =
    if !edge.Src = node then
        [edge.Dst]
    elif !edge.Dst = node then
        [edge.Src]
    else
        []

//
// Get neighboring nodes. Wonder if this works with big solution sets, we shall see.
//
let getNeighboringNodes target lst = lst |> List.map (fun x -> getNeighbor x target) |> concat

//
//  Get the number
//
let getNumNodes lst index = lst |> List.filter (fun x -> isNeighbor x index) |> List.length 


//
// Sort nodes by number of neighbors
//
let sortNodes (nodes: Node ref List) (es: Edge List) : Node ref List = 
    nodes |> List.sortBy (fun x -> getNumNodes es x) |> List.rev

//
// Check conflicts of neighbors if it is set to this Color.
// Will return true if there are conflics.
//
let checkConflicts (targetNode: Node) (edges: Edge List) (color: Color) : bool =
    edges
    |> getNeighboringNodes targetNode 
    |> List.exists (fun x -> (!x).Color = color) 

let getAllConflicts (edges: Edge List) =
    let edgesWithConfs = 
        edges
        |> List.filter (fun x -> x.Dst.contents.Color = x.Src.contents.Color)
        |> List.length
    edgesWithConfs / 2
    //nodes
    //|> List.map getNeighboringNodes 


//
// Pick first color to not cause any conflicts. If all conflicts
// cause conflicts, pick the last color. 
//
let findBestColor (targetNode: Node) (graph: Graph) =
    let edges = graph.edges
    let mutable nodes =  graph.nodes
    
    // Default the best color to the last color.
    let mutable bestColor = Some (graph.numColors-1)

    // foreach graph color...
    for i = 0 to graph.numColors-1 do
        // Set this color
        (!nodes.[targetNode.Index]).Color <- Some i  // Wait do I need to set this if we have func to 'try it out' first
        // Get neighbors
        let nebs = getNeighboringNodes targetNode graph.edges // Don't need this!!!
        // Check conflicts
        let hasConflicts = checkConflicts targetNode graph.edges (Some i)
        // If there aren't any conflicts, this is the the one
        if not hasConflicts then
            bestColor <- (Some i)
    bestColor

//
// Solving Algorithm:
//
//  1) sort nodes by num of neighbors
//  2) foreach node:
//  3) If colored continue, otherwise set to first color
//  4) foreach neighbor of node
//  5)    Try each color... if no conflicts of this nodes neighbors then take it. If not continue. If none found w/ conflicts, take last color.
//

//
// Solver function. Logic is right but need to figure out how to handle refs.
// 
// Problem:  Need to figure out a way so that nodes within edges get updated.
//
let solve (g: Graph) =
    let mutable sortedNodes = sortNodes g.nodes g.edges
    for node in sortedNodes do
        // If the node doesn't have a color...
        if (!node).Color = None then
            // Set to first color
            (!node).Color <- Some 0 // TODO check this. might not work
            // Get this nodes neighbors
            let neighbors = getNeighboringNodes !node g.edges
            // foreach neighbor, pick the color that will cause the least conflicts.
            for neb in neighbors do
                (!neb).Color <- findBestColor !neb g  //Pointers. Sigh
    g

            

//let mutfunc (x: mutable int) : () = x <- 1

//let thing = ref 1
//let mutnode = ref {Color = Some 1}

//let nodelist = [ref {Color = Some 1}; ref {Color = Some 2};]
//let mutable ndlst = [ref {Color = Some 1}; ref {Color = Some 2};]
//let mutable ndlst2 = [{Color = Some 1}; {Color = Some 2};]
//let item1 = ndlst2.[0]


(*
let checkConflicts (target:Node) (es:Edge List) (color:Color) : bool =
    let rec checkNodes nodes =
        match nodes with
        | head::tail -> 
            let nebs = getNebNodes head es
            if List.exists (fun x -> x.Color = target.Color) nebs then
                true
            else
                checkNodes tail
        | [] -> false
    checkNode ns 

*)





(*
let n1 = { Color = Some 1; Index = 1;}
let n2 = { Color = Some 2; Index = 2;}
let n3 = { Color = Some 3; Index = 3;}
let n4 = { Color = Some 4; Index = 4;}
let n5 = { Color = Some 5; Index = 5;}
let n6 = { Color = Some 6; Index = 6;}

let e1 = { Src = n1; Dst = n2; }
let e2 = { Src = n1; Dst = n3; }
let e3 = { Src = n1; Dst = n4; }
let e4 = { Src = n1; Dst = n5; }
let e5 = { Src = n3; Dst = n4; }
let e6 = { Src = n2; Dst = n6; }

let ns = [n1; n2; n3; n4; n5; n6]
let es = [e1; e2; e3; e4; e5; e6]

let v = getNebNodes n1 es 
*)

[<EntryPoint>]
let main argv = 
    //checkConflicts n1 es (Some 2) |> ignore
    let graph = readFile "C:\Users\Hunt\Documents\Visual Studio 2013\Projects\Special Topic Problems\Greedy Coloring\Greedy Coloring\Node.txt"
    let solution = solve graph
    let numconf = getAllConflicts solution.edges

    printfn "%A" argv
    let inp = System.Console.ReadLine()
    0 // return an integer exit code
    