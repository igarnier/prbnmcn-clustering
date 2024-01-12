(** Agglomerative clustering functor. *)

module type Element_set = sig
  (** [t] is the type of (multi)-sets of elements. *)
  type t

  (** [elt] is the type of elements to be clustered. *)
  type elt

  (** [singleton x] is the cluster containing [x] as only element. *)
  val singleton : elt -> t

  (** The user should provide [dist], aa distance function on clusters.
      A typical (costly) choice is the Hausdorff distance (see e.g. the [gromov] package).
      Other, non-metric choices are sometimes used.
  *)
  val dist : t -> t -> float

  (** One should be able to "join" clusters. This can be multiset union, set union
      or any sensible overapproximation - the algorithm will work anyway (think
      for instance of convex hulls in R^n) *)
  val join : t -> t -> t
end

(** [Make] takes as argument a module endowing sets of elements with the structure of
    a metric space. *)
module Make : functor (S : Element_set) -> sig
  type cluster =
    { set : S.t;  (** The set of elements corresponding to this cluster. *)
      tree : tree; (** The tree of subclusters contained in this cluster. *)
      merged_at : float; (** The distance between sub-clusters (0.0 in case [tree = Leaf]) *)
      uid : int
    }

  and tree = Node of cluster * cluster | Leaf

  val cluster : S.elt list -> cluster

  val cluster_with_initial : S.t list -> cluster

  (** [truncate c depth] returns all the sub-clusters at depth [depth].
      The depth of the root is 0. *)
  val truncate : cluster -> int -> S.t list

  (** Returns all clusters along their depths. *)
  val all_clusters : cluster -> (S.t * int) list
end
