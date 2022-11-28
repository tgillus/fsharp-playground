let rec last list =
    match list with
    | [] -> None
    | [x] -> Some x
    | _::rest -> last rest

let rec lastTwo list =
    match list with
    | [] | [_] -> None
    | [x; y] -> Some (x, y)
    | _::rest -> lastTwo rest

printfn "%A" (last [])
printfn "%A" (last [1])
printfn "%A" (last [1; 2])
printfn "%A" (last [1; 2; 3])
printfn "%A" (lastTwo [])
printfn "%A" (lastTwo [1])
printfn "%A" (lastTwo [1; 2])
printfn "%A" (lastTwo [1; 2; 3])
