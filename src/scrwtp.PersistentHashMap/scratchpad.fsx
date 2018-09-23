#load "HamtMap.fs"

open System
open scrwtp.PersistentHashMap

let test = HamtMap.empty

let small = 
    test 
    |> HamtMap.add 42 "test" 
    |> HamtMap.add 66 "other"
    |> HamtMap.add 22 "ala"
    |> HamtMap.add 22 "ala ma kota"
    |> HamtMap.add Int32.MaxValue "max"
    |> HamtMap.add Int32.MinValue "min"

small
|> HamtMap.tryFind Int32.MinValue

small |> HamtMap.containsKey 42