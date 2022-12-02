let rec last =
    function
    | [] -> None
    | [ x ] -> Some x
    | _ :: rest -> last rest

printfn "%b" (last [] = None)
printfn "%b" (last [ 1 ] = Some 1)
printfn "%b" (last [ 1; 2 ] = Some 2)
printfn "%b" (last [ 1; 2; 3 ] = Some 3)

let rec lastTwo =
    function
    | []
    | [ _ ] -> None
    | [ x; y ] -> Some(x, y)
    | _ :: rest -> lastTwo rest

printfn "%b" (lastTwo [] = None)
printfn "%b" (lastTwo [ 1 ] = None)
printfn "%b" (lastTwo [ 1; 2 ] = Some(1, 2))
printfn "%b" (lastTwo [ 1; 2; 3 ] = Some(2, 3))

let rec at list n =
    match (list, n) with
    | ([], _) -> None
    | (first :: _, 0) -> Some first
    | (_ :: rest, _) -> at rest (n - 1)

printfn "%b" (at [] 0 = None)
printfn "%b" (at [ 1 ] 0 = Some 1)
printfn "%b" (at [ 1 ] 1 = None)
printfn "%b" (at [ 1; 2 ] 0 = Some 1)
printfn "%b" (at [ 1; 2 ] 1 = Some 2)
printfn "%b" (at [ 1; 2 ] 2 = None)
printfn "%b" (at [ 1; 2; 3 ] 0 = Some 1)
printfn "%b" (at [ 1; 2; 3 ] 1 = Some 2)
printfn "%b" (at [ 1; 2; 3 ] 2 = Some 3)
printfn "%b" (at [ 1; 2; 3 ] 3 = None)

let length list =
    let rec count acc =
        function
        | [] -> acc
        | _ :: rest -> count (acc + 1) rest in count 0 list

printfn "%b" (length [] = 0)
printfn "%b" (length [ 1 ] = 1)
printfn "%b" (length [ 1; 2 ] = 2)
printfn "%b" (length [ 1; 2; 3 ] = 3)

let reverse list =
    let rec helper acc =
        function
        | [] -> acc
        | first :: rest -> helper (first :: acc) rest in helper [] list

printfn "%b" (reverse [] = [])
printfn "%b" (reverse [ 1 ] = [ 1 ])
printfn "%b" (reverse [ 1; 2 ] = [ 2; 1 ])
printfn "%b" (reverse [ 1; 2; 3 ] = [ 3; 2; 1 ])
