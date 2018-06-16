(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

module List = struct
  include List
  let rec fold f = function [] -> fun acc -> acc | x :: xs -> f x % fold f xs
end

type t = string list

let init arg = [arg]

let string arg acc = arg :: acc
let int arg acc = string_of_int arg :: acc

let option ?slip f arg acc =
  (match slip, arg with
   | _, None -> acc
   | None, Some arg -> f arg acc
   | Some slip, Some arg -> f arg (string slip acc))

let list ?slip f args =
  (match slip with
   | None ->
      List.fold f args
   | Some slip ->
      List.fold (fun arg -> string slip %> f arg) args)

let finish acc =
  (match List.rev acc with
   | [] -> assert false
   | cmd :: _ as cmdline -> (cmd, Array.of_list cmdline))
