(* Load the sklearn 'wine' dataset, in CSV format.

   Source: https://archive.ics.uci.edu/dataset/109/wine

   Classes: 3
   Samples per class: [59,71,48]
   Samples total: 178
   Dimensionality: 13
   Features: real, positive

   We'd need to renormalize each column to [0, 1] before clustering but we don't bother.
*)

module Fields = struct
  let class_ = 0

  let alcohol = 1

  let malic_acid = 2

  let ash = 3

  let alcalinity_of_ash = 4

  let magnesium = 5

  let total_phenols = 6

  let flavanoids = 7

  let nonflavonoid_phenols = 8

  let proanthocyanins = 9

  let color_intensity = 10

  let hue = 11

  let od280_od315_of_diluted_wines = 12

  let proline = 13
end

type wine_sample = float array

let with_open openfun s f =
  let ic = openfun s in
  Fun.protect ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () -> f ic)



let rec input_lines ic acc =
  match Stdlib.input_line ic with
  | line -> input_lines ic (line :: acc)
  | exception End_of_file -> List.rev acc

let load_wine filename =
  with_open open_in filename (fun file ->
      let lines = input_lines file [] in
      List.map
        (fun line ->
          let fields = String.split_on_char ',' line in
          List.map Float.of_string fields |> Array.of_list)
        lines)

open Clustering

module Metric = struct
  type t = wine_sample

  let dist (sample1 : wine_sample) (sample2 : wine_sample) =
    let acc = ref 0.0 in
    (* skip class *)
    for i = 1 to Array.length sample1 - 1 do
      acc := !acc +. ((sample1.(i) -. sample2.(i)) ** 2.0)
    done ;
    !acc |> Float.sqrt
end

module Cluster = struct
  type t = wine_sample array

  type elt = wine_sample

  let singleton x = [| x |]

  (* UPGMA linkage *)
  let dist (c1 : t) (c2 : t) =
    let acc = ref 0.0 in
    let n = Array.length c1 in
    let m = Array.length c2 in
    Array.iter
      (fun x ->
        Array.iter
          (fun y ->
            let d = Metric.dist x y in
            acc := !acc +. d)
          c2)
      c1 ;
    !acc /. (Float.of_int n *. Float.of_int m)

  let join (c1 : t) (c2 : t) = Array.concat [c1; c2]
end

module A = Agglomerative.Make (Cluster)

module Print_tree = struct
  (*
    Created by Martin Jambon and placed in the Public Domain on June 1, 2019.
    Print a tree or a DAG as tree, similarly to the 'tree' command.
  *)

  open Printf

  let rec iter f = function
    | [] -> ()
    | [x] -> f true x
    | x :: tl ->
        f false x ;
        iter f tl

  let to_buffer ?(line_prefix = "") ~get_name ~get_children buf x =
    let rec print_root indent x =
      bprintf buf "%s\n" (get_name x) ;
      let children = get_children x in
      iter (print_child indent) children
    and print_child indent is_last x =
      let line = if is_last then "└── " else "├── " in
      bprintf buf "%s%s" indent line ;
      let extra_indent = if is_last then "    " else "│   " in
      print_root (indent ^ extra_indent) x
    in
    Buffer.add_string buf line_prefix ;
    print_root line_prefix x

  let to_string ?line_prefix ~get_name ~get_children x =
    let buf = Buffer.create 1000 in
    to_buffer ?line_prefix ~get_name ~get_children buf x ;
    Buffer.contents buf
end

(* Check that clustering doesn't fail on a singleton list *)
let () = ignore (A.cluster [[|0.0|]])

let () =
  let data = load_wine "./wine.data" in
  let t0 = Unix.gettimeofday () in
  let cluster = A.cluster data in
  let t1 = Unix.gettimeofday () in
  Format.printf "Clustering took %f seconds\n" (t1 -. t0) ;
  let tree =
    Print_tree.to_string
      ~get_name:(fun ({ set; tree = _; merged_at; uid = _ } : A.cluster) ->
        Printf.sprintf "%d[%f]" (Array.length set) merged_at)
      ~get_children:(fun
          ({ set = _; tree; merged_at = _; uid = _ } : A.cluster) ->
        match tree with Node (left, right) -> [left; right] | Leaf -> [])
      cluster
  in
  print_endline tree
