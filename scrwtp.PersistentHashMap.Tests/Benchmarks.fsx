#I @".\bin\Debug\net472"
#r "scrwtp.PersistentHashMap.dll"
#r "FSharpX.Collections.dll"

#time "on"

open scrwtp.PersistentHashMap
open FSharpx.Collections

let upper = 1000000

let test1() =
    let mutable x1 = Map.empty
    for i in 1 .. upper do
        x1 <- x1 |> Map.add (string i) i
    x1

let test2() =
    let mutable x2 = HamtMap.empty
    for i in 1 .. upper do
        x2 <- x2 |> HamtMap.add (string i) i
    x2

let test3() =
    let mutable x1 = System.Collections.Generic.Dictionary<_,_>()
    for i in 1 .. upper do
        x1.Add(string i, i)
    x1

let test4() =
    let mutable x1 = PersistentHashMap.empty
    for i in 1 .. upper do
        x1 <- x1 |> PersistentHashMap.add (string i) i
    x1

let r1 = test1()
let r2 = test2()
let r3 = test3()

let lookupTest1 (x: Map<_,_>) =
    for i in 1 .. upper do
        x |> Map.tryFind (string i) |> Option.get |> ignore
    ()

let lookupTest2 (x: HamtMap.HamtMap<_,_>) =
    for i in 1 .. upper do
        x |> HamtMap.tryFind (string i) |> Option.get |> ignore
    ()

let lookupTest3 (x: System.Collections.Generic.Dictionary<_,_>) =
    for i in 1 .. upper do
        match x.TryGetValue(string i) with
        | true, v -> ()
        | false, _ -> failwith "not found"
    ()

lookupTest1(r1)
lookupTest2(r2)
lookupTest3(r3)