module type Element_set = sig
  type t

  type elt

  val singleton : elt -> t

  val dist : t -> t -> float

  val join : t -> t -> t
end

module Make =
  functor
    (E : Intf.Metric)
    (S : Element_set with type elt = E.t)
    ->
    struct
      type cluster = { set : S.t; tree : tree; index : int }

      and tree = Node of cluster * cluster | Leaf

      let mkcluster index set tree = { set; tree; index }

      let precompute_dist clusters =
        let len = List.length clusters * 2 - 1 in
        let table = Array.init len (fun i ->
            Array.init len (fun j ->
                if i = j
                then Some 0.0
                else None
              )
          )
        in
        fun c1 c2 ->
            match table.(c1.index).(c2.index) with
            | Some dist -> dist
            | None ->
              let dist = S.dist c1.set c2.set in
              table.(c1.index).(c2.index) <- Some dist;
              dist

      let minimum_pairwise_distance (dist : cluster -> cluster -> float) clusters =
        match clusters with
        | [] | [_] -> invalid_arg "cluster list must contain at least two clusters"
        | c1 :: c2 :: _tl ->
          let (acc, _) =
            List.fold_left
              (fun (acc, i) c1 ->
                 let (acc, _) =
                   List.fold_left
                     (fun (acc, j) c2 ->
                        let acc =
                          if j > i then
                            let (best_d, _, _) = acc in
                            let d = dist c1 c2 in
                            if d < best_d then (d, c1, c2) else acc
                          else acc
                        in
                        (acc, j + 1))
                     (acc, 0)
                     clusters
                 in
                 (acc, i + 1))
              ((dist c1 c2, c1, c2), 0)
              clusters
          in
          acc

      let rec iterate i dist clusters =
        match clusters with
        | [] -> invalid_arg "empty cluster list"
        | [c] -> c
        | _ ->
          let (_d, c, c') = minimum_pairwise_distance dist clusters in
          let clusters =
            List.filter (fun c0 -> c0.index <> c.index && c0.index <> c'.index) clusters
          in
          let joined = mkcluster i (S.join c.set c'.set) (Node (c, c')) in
          iterate (i+1) dist (joined :: clusters)

      let cluster_with_initial element_sets =
        let dist = precompute_dist element_sets in
        let clusters =
          List.mapi (fun i x -> mkcluster i x Leaf) element_sets
        in
        iterate (List.length element_sets) dist clusters

      let cluster elements =
        List.map (fun x -> S.singleton x) elements
        |> cluster_with_initial

      let truncate cluster depth =
        let rec truncate { set; tree; _ } depth queue acc =
          match tree with
          | Leaf -> (
              if depth > 0 then invalid_arg "truncate: tree too short"
              else
                let acc = set :: acc in
                match queue with
                | [] -> acc
                | (next, d) :: tl -> truncate next d tl acc)
          | Node (l, r) ->
            if depth = 0 then
              let acc = set :: acc in
              match queue with
              | [] -> acc
              | (next, d) :: tl -> truncate next d tl acc
            else truncate l (depth - 1) ((r, depth - 1) :: queue) acc
        in
        truncate cluster depth [] []

      let all_clusters cluster =
        let rec fold { set; tree; _ } depth acc =
          match tree with
          | Leaf -> (set, depth) :: acc
          | Node (l, r) ->
            fold r (depth + 1) (fold l (depth + 1) ((set, depth) :: acc))
        in
        fold cluster 0 []
    end
