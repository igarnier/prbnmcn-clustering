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
      type cluster = { index: int; set: S.t; children: (cluster * cluster) option; }

      let precompute_dist sets =
        let len = List.length sets * 2 - 1 in
        let table = Array.init len (fun i ->
            Array.init len (fun j ->
                if i = j
                then Some 0.0
                else None
              )
          )
        in
        fun c1 c2 ->
          let i,j = min c1.index c2.index, max c1.index c2.index in
          match table.(i).(j) with
          | Some dist -> dist
          | None ->
            let dist = S.dist (List.nth sets i) (List.nth sets j) in
            table.(i).(j) <- Some dist;
            table.(j).(i) <- Some dist;
            dist

      let minimum_pairwise_distance dist clusters =
        match clusters with
        | [] | [_] -> invalid_arg "cluster list must contain at least two clusters"
        | c1 :: c2 :: _tl ->
          let seq = List.to_seq clusters |> (fun x -> Seq.product x x) in
          Seq.fold_left (fun ((dist_min, _, _) as acc) (a,b) ->
              let dist_new = dist a b in
              if a.index = b.index || dist_new >= dist_min
              then acc
              else (dist_new, a, b)
            ) (dist c1 c2, c1, c2) seq

      let cluster_with_initial element_sets =
        let dist = precompute_dist element_sets in
        let len = List.length element_sets in
        let clusters =
          List.mapi (fun index set -> {index; set; children = None; }) element_sets
        in

        let rec iterate index clusters =
          match clusters with
          | [] -> failwith "No cluster left"
          | [c] -> c
          | cs ->
            let _, left, right = minimum_pairwise_distance dist cs in
            let set = S.join left.set right.set in
            let clusters = {index; set; children = Some (left, right); } :: List.filter (fun x -> x.index = left.index || x.index = right.index) clusters in
            iterate (index+1) clusters
        in
        iterate len clusters

      let cluster elements =
        List.map (fun x -> S.singleton x) elements
        |> cluster_with_initial

      let truncate cluster depth =
        let rec truncate { set; children; _ } depth queue acc =
          match children with
          | None -> (
              if depth > 0 then invalid_arg "truncate: tree too short"
              else
                let acc = set :: acc in
                match queue with
                | [] -> acc
                | (next, d) :: tl -> truncate next d tl acc)
          | Some (l, r) ->
            if depth = 0 then
              let acc = set :: acc in
              match queue with
              | [] -> acc
              | (next, d) :: tl -> truncate next d tl acc
            else truncate l (depth - 1) ((r, depth - 1) :: queue) acc
        in
        truncate cluster depth [] []

      let all_clusters cluster =
        let rec fold { set; children; _ } depth acc =
          match children with
          | None -> (set, depth) :: acc
          | Some (l, r) ->
            fold r (depth + 1) (fold l (depth + 1) ((set, depth) :: acc))
        in
        fold cluster 0 []
    end
