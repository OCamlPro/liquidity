(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

module TYPES : sig
  type graph =
    { graph_name: string;
      mutable graph_nodes: node list;
      mutable graph_edges: edge list;
      mutable node_counter: int;
      mutable graph_attributes: graph_attributes list;
      mutable graph_subgraphs: graph list }
   and node =
     { mutable node_name: string;
       node_id: int;
       node_graph: graph;
       mutable node_attributes: node_attributes list }
   and edge =
     { edge_from: node;
       edge_to: node;
       mutable edge_attributes: edge_attributes list }
   and graph_attributes =
     | GraphSize of float * float
     | Ratio of graph_ratio
     | Compound of bool

   and graph_ratio =
     RatioFill
   and node_attributes =
     | NodeColor of string
     | NodeFontColor of string
     | NodeFontName of string
     | NodeShape of shape
     | NodeHeight of float
     | NodeWidth of float
     | NodeStyle of goptions
   and edge_attributes =
     | EdgeDirection of direction
     | EdgeLabel of string
     | EdgeStyle of goptions
     | EdgeWeight of int
   and direction = | Forward | Backward | Bothdir | Nodir
   and goptions = | Bold | Dotted | Filled
   and shape =
     | Ellipse
     | Box
     | Circle
     | DoubleCircle
     | Diamond
     | PlainText
     | Record
     | Polygon of int * polygon_options list
     | Epsf of string
   and polygon_options = | Skew of float | Distortion of float
end
open TYPES
val create : string -> graph_attributes list -> graph
val node : graph -> string -> node_attributes list -> node
val edge : node -> node -> edge_attributes list -> edge
val add_edge : node -> node -> edge_attributes list -> unit
val add_edges : node -> node list -> edge_attributes list -> unit
val add_path : node list -> edge_attributes list -> unit
val rename_node : node -> string -> unit
val add_subgraph : graph -> graph -> unit

val add_node_attrs : node -> node_attributes list -> unit
val add_edge_attrs : edge -> edge_attributes list -> unit

val dot2ps_cmd : string -> string -> string
val dot2pdf_cmd : string -> string -> string
val to_string : graph -> string


                                                       (*
val dot2ps : string -> string -> unit
val dot2pdf : string -> string -> unit
val save : graph -> string -> unit
val save_in : graph -> out_channel -> unit
val view : graph -> unit
                                                        *)
