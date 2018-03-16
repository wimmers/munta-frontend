let groupBy key xs =
    let rec group = function
        | ([], [])  -> []
        | ([], grp) -> [grp]
        | (x :: xs, []) -> group (xs, [x])
        | (x :: xs, (y :: ys as zs)) when key x = key y -> group (xs, x :: zs)
        | (x :: xs, (y :: ys as zs)) -> zs :: group (xs, [x])
    in
    xs |> List.sort (fun a b -> key a - key b) |> fun xs -> group (xs, [])

let rec fill_groups key = function
    | ([], _) -> []
    | (_ :: xs, []) -> [] :: fill_groups key (xs, [])
    | (x :: xs, (y :: ys) :: zs) when key y = x -> (y :: ys) :: fill_groups key (xs, zs)
    | (x :: xs, (y :: ys) :: zs) -> [] :: fill_groups key (xs, (y :: ys) :: zs)

let upto l u =
    let rec upt l u = if l >= u then [] else u - 1 :: upt l (u - 1) in upt l u |> List.rev

let rec repeat x n = if n <= 0 then [] else x :: repeat x (n - 1)

let implode xs =
    String.init (List.length xs) (List.nth xs)