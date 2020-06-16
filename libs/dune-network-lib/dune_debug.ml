(**************************************************************************)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  any later version.                                                    *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(**************************************************************************)

let eprintf x =
  Format.kfprintf (fun fmt -> Format.fprintf fmt "@.") Format.err_formatter x

let ignore_eprintf x = Format.ifprintf Format.err_formatter x

let debug =
  match
    Environment_variable.(
      get_opt @@ make "DUNE_DEBUG" ~description:"Debug flag for protocol")
  with
  | None ->
      0
  | Some "n" ->
      -1
  | Some s -> (
    match int_of_string s with exception _ -> 1 | n -> n )

let printf ?(n = 1) fmt =
  if n <= debug then eprintf fmt else ignore_eprintf fmt

let string_of_exn exn = Printexc.to_string exn

let polymorphic_compare = compare

module Array = struct
  type 'a t = 'a array

  include Array
end

(* let is_peer_id s =
 *   match P2p_peer_id.of_b58check s with Ok _ -> true | _ -> false *)
