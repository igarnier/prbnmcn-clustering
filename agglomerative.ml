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
      type cluster = { index: int; merged_at: float; set: S.t; children: (cluster * cluster) option; }

      let precompute_dist len =
        let len_total = len * 2 - 1 in
        let table = Array.init len_total (fun i ->
            Array.init len_total (fun j ->
                if i = j
                then Some 0.0
                else None
              ))
        in
        fun clusters c1 c2 ->
          let i,j = min c1.index c2.index, max c1.index c2.index in
          match table.(i).(j) with
          | Some dist -> dist
          | None ->
            let dist = S.dist (List.nth clusters i).set (List.nth clusters j).set in
            table.(i).(j) <- Some dist;
            table.(j).(i) <- Some dist;
            dist

      let minimum_pairwise_distance (dist : cluster -> cluster -> float) clusters =
        match clusters with
        | [] | [_] -> invalid_arg "cluster list must contain at least two clusters"
        | c1 :: c2 :: _ ->
          let seq = List.to_seq clusters |> (fun x -> Seq.product x x) in
          Seq.fold_left (fun ((dist_min, _, _) as acc) (a,b) ->
              let dist_new = dist a b in
              if a.index = b.index || dist_new > dist_min
              then acc
              else (dist_new, a, b)
            ) (dist c1 c2, c1, c2) seq

      let cluster_with_initial element_sets =
        let dist = precompute_dist (List.length element_sets) in
        let len = List.length element_sets in
        let clusters =
          List.mapi (fun index set -> {index; merged_at = 0.; set; children = None; }) element_sets;
        in

        let rec iterate index clusters active_clusters =
          match active_clusters with
          | [] -> failwith "No cluster left."
          | [c] -> c
          | _ ->
            let merged_at, left, right = minimum_pairwise_distance (dist clusters) active_clusters in
            let set = S.join left.set right.set in
            let cluster_new = {index; merged_at; set; children = Some (left, right); } in
            let clusters = clusters @ [cluster_new] in
            let active_clusters = cluster_new :: List.filter (fun x -> x.index != left.index && x.index != right.index) active_clusters in
            Printf.printf "Joined %i %i\n" left.index right.index;
            iterate (index+1) clusters active_clusters
        in
        iterate len clusters clusters

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
