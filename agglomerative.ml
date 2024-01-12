module type Element_set = sig
  type t

  type elt

  val singleton : elt -> t

  val dist : t -> t -> float

  val join : t -> t -> t
end

module Make =
functor
  (S : Element_set)
  ->
  struct
    type cluster = { set : S.t; tree : tree; merged_at : float; uid : int }

    and tree = Node of cluster * cluster | Leaf

    module Table = struct
      type uid = int

      module Cluster_dist = struct
        type t = { dist : float } [@@ocaml.unboxed]

        let compare i1 i2 = Float.compare i1.dist i2.dist
      end

      module Prioqueue = Psq.Make (Int) (Cluster_dist)

      type cluster_info = { cluster : cluster; mutable others : Prioqueue.t }

      type t = (uid, cluster_info) Hashtbl.t

      let create () : t = Hashtbl.create 101

      let card t = Hashtbl.length t

      let min_dist { others; _ } = Prioqueue.min others

      let remove_cluster (table : t) uid =
        Hashtbl.remove table uid ;
        Hashtbl.iter
          (fun _ info -> info.others <- Prioqueue.remove uid info.others)
          table

      let remove_min (table : t) =
        let min = ref max_float in
        let minc = ref None in
        Hashtbl.iter
          (fun _uid info ->
            match min_dist info with
            | None -> assert false
            | Some (uid, cdist) ->
                if cdist.dist < !min then (
                  min := cdist.dist ;
                  minc := Some (info, uid))
                else ())
          table ;
        match !minc with
        | None -> None
        | Some (first_cluster_info, second_cluster_uid) ->
            let first_cluster = first_cluster_info.cluster in
            let second_cluster =
              (Hashtbl.find table second_cluster_uid).cluster
            in
            remove_cluster table first_cluster.uid ;
            remove_cluster table second_cluster.uid ;
            Some (first_cluster, second_cluster, !min)

      let add_new (table : t) (cluster : cluster) =
        assert (not (Hashtbl.mem table cluster.uid)) ;
        let new_cluster = { cluster; others = Prioqueue.empty } in
        Hashtbl.iter
          (fun _uid (info : cluster_info) ->
            let dist =
              Cluster_dist.{ dist = S.dist cluster.set info.cluster.set }
            in
            new_cluster.others <-
              Prioqueue.add info.cluster.uid dist new_cluster.others ;
            info.others <- Prioqueue.add cluster.uid dist info.others)
          table ;
        (*assert (not (new_cluster.min_cluster = -1)) ;*)
        Hashtbl.add table cluster.uid new_cluster
    end

    let uid =
      let x = ref (-1) in
      fun () ->
        incr x ;
        !x

    let mkcluster set tree merged_at = { set; tree; merged_at; uid = uid () }

    let rec iterate clusters =
      let card = Table.card clusters in
      if card = 0 then invalid_arg "empty cluster list"
      else if card = 1 then
        let info = Hashtbl.to_seq_values clusters |> Array.of_seq in
        info.(0).Table.cluster
      else
        match Table.remove_min clusters with
        | None -> assert false
        | Some (c, c', dist) ->
            let (c, c') = if c.uid < c'.uid then (c, c') else (c', c) in
            let joined = mkcluster (S.join c.set c'.set) (Node (c, c')) dist in
            Table.add_new clusters joined ;
            iterate clusters

    let cluster_with_initial element_sets =
      let clusters = Table.create () in
      List.iter
        (fun x -> Table.add_new clusters (mkcluster x Leaf 0.0))
        element_sets ;
      iterate clusters

    let cluster elements =
      List.map (fun x -> S.singleton x) elements |> cluster_with_initial

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
