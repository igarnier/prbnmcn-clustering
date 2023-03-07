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
      type cluster = { uid: int; merged_at: float; set: S.t; children: (cluster * cluster) option; }

      let uid =
        let x = ref (-1) in
        fun () ->
          incr x ;
          !x

      let mkcluster set = { set; merged_at = 0.; children = None; uid = uid () }

      (* Hash distance computation between clusters. *)
      module Table = Hashtbl.Make (struct
          type t = cluster * cluster

          let equal (c1, c2) (c1', c2') =
            (c1.uid = c1'.uid && c2.uid = c2'.uid)
            || (c1.uid = c2'.uid && c2.uid = c1'.uid)

          let hash (c1, c2) =
            if c1.uid < c2.uid then Hashtbl.hash (c1.uid, c2.uid)
            else Hashtbl.hash (c2.uid, c1.uid)
        end)

      let dist sz =
        let table = Table.create sz in
        fun c1 c2 ->
          let c1, c2 = if c1.uid < c2.uid then c1, c2 else c2, c1 in
          if c1.uid = c2.uid then 0.0
          else
            match Table.find_opt table (c1, c2) with
            | Some dist -> dist
            | None ->
              let dist = S.dist c1.set c2.set in
              Table.add table (c1, c2) dist ;
              dist

      let minimum_pairwise_distance (dist : cluster -> cluster -> float) clusters =
        match clusters with
        | [] | [_] -> invalid_arg "cluster list must contain at least two clusters"
        | c1 :: c2 :: _ ->
          let seq = List.to_seq clusters |> (fun x -> Seq.product x x) in
          Seq.fold_left (fun ((dist_min, _, _) as acc) (a,b) ->
              let dist_new = dist a b in
              if a.uid = b.uid || dist_new > dist_min
              then acc
              else (dist_new, a, b)
            ) (dist c1 c2, c1, c2) seq

      let cluster_with_initial element_sets =
        (* The number of sets in total is |elements| * 2 - 1 because each new cluster combines two elements. *)
        (* We end with exactly one cluster remaining. *)
        let dist = dist (List.length element_sets * 2 - 1) in
        let len = List.length element_sets in
        let clusters = List.map mkcluster element_sets in

        let rec iterate index active_clusters =
          match active_clusters with
          | [] -> failwith "No cluster left."
          | [c] -> c
          | _ ->
            let merged_at, left, right = minimum_pairwise_distance dist active_clusters in
            let set = S.join left.set right.set in
            let cluster_new = {uid = uid (); merged_at; set; children = Some (left, right); } in
            let active_clusters =
              cluster_new
              :: List.filter (fun x -> x.uid != left.uid && x.uid != right.uid) active_clusters
            in
            iterate (index+1) active_clusters
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
