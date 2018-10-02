namespace scrwtp.PersistentHashMap

(*
    A map implementation based on hash array mapped trie.
    This is a fairly direct F# port of Haskell implementation by Kevin Wu Won.
    https://github.com/exclipy/pdata/blob/master/Data/HamtMap.hs
*)

[<RequireQualifiedAccess>]
module HamtMap =
    open System.Collections.Generic

    [<AutoOpen>]
    module internal Prelude =

        module Option =
            let inline maybe (def: 'b) (f: 'a -> 'b) (opt: 'a option) =
                match opt with
                | Some v -> f v
                | None -> def

        module Array =
            let updateAssocs (assocs: (int * 'a) list) (arr: 'a array): 'a array =
                let arr = Array.copy arr
                for idx, e in assocs do
                    if idx >= 0 && idx < arr.Length then
                        arr.[idx] <- e
                arr

        let konst a b = a

    [<RequireQualifiedAccess>]
    module internal Consts =

        [<Literal>]
        let shiftStep = 5
        [<Literal>]
        let maxSizeOfBitmapIndexedNode = 16
        [<Literal>]
        let minSizeOfArrayNode = 8

    type Hash = int32
    type Bitmap = int32

    type HamtMap<'key,'value> =
        | EmptyNode
        | LeafNode of hash: Hash * key : 'key * value : 'value
        | HashCollisionNode of hash: Hash * pairs: ('key * 'value) list
        | BitmapIndexedNode of bitmap: Bitmap * subNodes: HamtMap<'key, 'value> array
        | ArrayNode of numChildren: int * subNodes: HamtMap<'key, 'value> array

    [<RequireQualifiedAccess>]
    module Hashing =

        let inline hash (arg: 'a) : Hash = Operators.hash arg

        let inline fragment shift (h : Hash) : Hash =
            (h >>> shift) &&& 0b11111

    [<RequireQualifiedAccess>]
    module Binary =

        let inline bitcount (i: int) =
            let i = i - ((i >>> 1) &&& 0x55555555)
            let i = (i &&& 0x33333333) + ((i >>> 2) &&& 0x33333333)
            (((i + (i >>> 4)) &&& 0x0F0F0F0F) * 0x01010101) >>> 24

        let inline toBitmap (subHash: Hash) =
            1 <<< subHash

        let inline fromBitmap (bitmap: Bitmap) (subHash: Hash) =
            bitcount (bitmap &&& (toBitmap subHash - 1))

        let bitmapToIndices (bitmap: Bitmap) : int list =
            let rec aux ix bitmap =
                match ix, bitmap with
                | _, 0 -> []
                | 32, _ -> []
                | ix, bitmap ->
                    if bitmap &&& 1 = 0 then
                        aux (ix+1) (bitmap >>> 1)
                    else
                        ix :: aux (ix+1) (bitmap >>> 1)
            aux 0 bitmap

    let private mkLeafNode hash key value =
        LeafNode (hash, key, value)

    let isEmptyNode (m: HamtMap<'k,'v>) =
        match m with
        | EmptyNode -> true
        | _ -> false

    let isTipNode (m: HamtMap<'k,'v>) =
        match m with
        | EmptyNode
        | LeafNode _
        | HashCollisionNode _ -> true
        | _ -> false

    let empty = EmptyNode

    let singleton key value = mkLeafNode (Hashing.hash key) key value

    module private AlterNodeImpl =

        type private Change =
            | Removed
            | Modified
            | Nil
            | Added

        let rec combineNodes shift (node1: HamtMap<'a,'b>) (node2: HamtMap<'a,'b>) : HamtMap<'a,'b> =
            match node1, node2 with
            | LeafNode (h1, k1, v1), LeafNode (h2, k2, v2) ->
                if h1 = h2 then
                    HashCollisionNode (h1, [(k1, v1); (k2, v2)])
                else
                    let subh1 = Hashing.fragment shift h1
                    let subh2 = Hashing.fragment shift h2

                    let bitmap =
                        Binary.toBitmap subh1 ||| Binary.toBitmap subh2

                    let subNodes =
                        if subh1 = subh2 then
                            [| combineNodes (shift + Consts.shiftStep) node1 node2 |]
                        elif subh1 < subh2 then
                            [| node1; node2 |]
                        else
                            [| node2; node1 |]

                    BitmapIndexedNode(bitmap, subNodes)
            | _, _ -> failwithf "combineNodes: unexpected node types: %A, %A" node1 node2

        let updateList (updateFn: 'v option -> 'v option) (key: 'k) (pairs: ('k * 'v) list) =
            List.fold (fun acc (k, v) ->
                if k = key then
                    Option.maybe acc (fun v -> (k, v) :: acc) (updateFn (Some v))
                else
                    (k, v) :: acc)
                (Option.maybe [] (fun v -> [ key, v ]) (updateFn None))
                pairs
            |> List.rev

        let expandBitmapNode subHash node bitmap subNodes =
            let assocs =
                let sn = List.zip (Binary.bitmapToIndices bitmap) (List.ofArray subNodes)
                (subHash, node) :: sn
            let numChildren = Binary.bitcount bitmap + 1
            let blank = Array.replicate 32 EmptyNode
            ArrayNode(numChildren, Array.updateAssocs assocs blank)

        let packArrayNode subHash (subNodes: _ array) =
            let elems =
                Array.init 32 (fun i ->
                    if i = subHash then EmptyNode else subNodes.[i])

            let subNodes =
                Array.filter (not << isEmptyNode) elems

            let bitmap : Bitmap =
                (Array.map (not << isEmptyNode) elems, 0)
                ||> Array.foldBack
                    (fun on bitmap ->
                        (bitmap <<< 1) ||| (if on then 1 else 0))

            BitmapIndexedNode(bitmap, subNodes)

        let rec alterNode shift (updateFn: 'v option -> 'v option) (hash: Hash) (key: 'k) (node: HamtMap<'k, 'v>) : HamtMap<'k, 'v> =
            match node with
            | EmptyNode ->
                Option.maybe EmptyNode (mkLeafNode hash key) (updateFn None)
            | LeafNode (h, k, v) as node ->
                if key = k then
                    Option.maybe EmptyNode (mkLeafNode h key) (updateFn (Some v))
                else
                    let node' =
                        alterNode shift updateFn hash key EmptyNode
                    if isEmptyNode node' then
                        node
                    else
                        combineNodes shift node node'
            | HashCollisionNode (h, pairs) ->
                match updateList updateFn key pairs with
                | [] -> failwith "alterNode: updating a hash collision node yielded an empty sequence of kvps"
                | [ k, v ] -> LeafNode(h, k, v)
                | pairs -> HashCollisionNode(h, pairs)
            | BitmapIndexedNode (bitmap, subNodes) ->
                let subHash = Hashing.fragment shift hash

                let ix = Binary.fromBitmap bitmap subHash
                let bit = Binary.toBitmap subHash

                let child, change =
                    let exists = (bitmap &&& bit) <> 0
                    let node = if exists then subNodes.[ix] else EmptyNode

                    let updated = alterNode (shift + Consts.shiftStep) updateFn hash key node

                    let change =
                        if exists then
                            if isEmptyNode updated then Removed else Modified
                        else
                            if isEmptyNode updated then Nil else Added

                    updated, change

                let updatedSubNodes =
                    match change with
                    | Removed ->
                        let left, right = Array.splitAt ix subNodes
                        [| yield! left; yield! Array.tail right |]
                    | Modified -> Array.updateAssocs [ ix, child ] subNodes
                    | Nil -> subNodes
                    | Added ->
                        let left, right = Array.splitAt ix subNodes
                        [| yield! left; yield child; yield! right |]

                let updatedBitmap =
                    match change with
                    | Removed   -> bitmap &&& (~~~ bit)
                    | Modified  -> bitmap
                    | Nil       -> bitmap
                    | Added     -> bitmap ||| bit

                if updatedBitmap = 0 then
                    EmptyNode
                elif updatedSubNodes.Length = 1 && isTipNode updatedSubNodes.[0] then
                    updatedSubNodes.[0]
                elif change = Added && subNodes.Length > Consts.maxSizeOfBitmapIndexedNode - 1 then
                    expandBitmapNode subHash child bitmap subNodes
                else
                    BitmapIndexedNode(updatedBitmap, updatedSubNodes)

            | ArrayNode (numChildren, subNodes) ->
                let subHash = Hashing.fragment shift hash

                let child, change =
                    let node = subNodes.[subHash]
                    let updated = alterNode (shift + Consts.shiftStep) updateFn hash key node

                    let change =
                        if not <| isEmptyNode node then
                            if isEmptyNode updated then Removed else Modified
                        else
                            if isEmptyNode updated then Nil else Added

                    updated, change

                let updatedNumChildren =
                    match change with
                    | Removed   -> numChildren - 1
                    | Modified
                    | Nil       -> numChildren
                    | Added     -> numChildren + 1

                if updatedNumChildren < Consts.minSizeOfArrayNode then
                    packArrayNode subHash subNodes
                else
                    let updated = Array.updateAssocs [ subHash, child ] subNodes
                    ArrayNode(updatedNumChildren, updated)

    let private alter updateFn key table =
        AlterNodeImpl.alterNode 0 updateFn (Hashing.hash key) key table

    let addWith mergeFn key value table =
        let f a b =
            match b with
            | None -> Some a
            | Some b -> Some (mergeFn a b)
        alter (f value) key table

    let add key value table =
        addWith konst key value table

    let remove key table =
        alter (konst None) key table

    let rec private lookupNode shift hash key node =
        match node with
        | EmptyNode -> None
        | LeafNode (h, k, v) ->
            if k = key then Some v else None
        | HashCollisionNode (h, pairs) ->
            pairs
            |> List.tryPick (fun (k, v) ->
                if k = key then Some v else None)
        | BitmapIndexedNode (bitmap, subNodes) ->
            let subHash = Hashing.fragment shift hash

            let ix = Binary.fromBitmap bitmap subHash
            let bit = Binary.toBitmap subHash

            let exists = (bitmap &&& bit) <> 0

            if exists then
                lookupNode (shift + Consts.shiftStep) hash key (subNodes.[ix])
            else
                None
        | ArrayNode (numChildren, subNodes) ->
            let subHash = Hashing.fragment shift hash
            lookupNode (shift + Consts.shiftStep) hash key (subNodes.[subHash])

    let tryFind key table =
        lookupNode 0 (Hashing.hash key) key table

    let find key table =
        match tryFind key table with
        | Some v -> v
        | None -> raise (KeyNotFoundException())

    let containsKey key table =
        Option.maybe false (konst true) (tryFind key table)