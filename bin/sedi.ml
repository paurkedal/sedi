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

open Lwt.Infix
open Printf

let rm_at_exit : (string, unit) Hashtbl.t = Hashtbl.create 37

let rec confirm question =
  Lwt_io.printf "%s %!" question >>= fun () ->
  (match%lwt Lwt_io.(read_line stdin) >|= String.(lowercase_ascii % trim) with
   | "y" | "yes" -> Lwt.return_true
   | "n" | "no" -> Lwt.return_false
   | _ -> confirm question)

let tmp_like fn = Filename.(concat (dirname fn) (".sedi-tmp." ^ basename fn))

let fail_with_process_status fn = function
 | Unix.WEXITED ec ->
    ksprintf Lwt.fail_with "%s exited with %d" fn ec
 | Unix.WSIGNALED sn | Unix.WSTOPPED sn ->
    ksprintf Lwt.fail_with "%s received signal %d" fn sn

let prepare scripts script_files wrap_length dst_path =
  let%lwt st = Lwt_unix.stat dst_path in
  let cmd = Cmdbuilder.(init "sed"
    |> string "-E"
    |> list ~slip:"-e" string scripts
    |> list ~slip:"-f" string script_files
    |> option ~slip:"-l" int wrap_length
    |> string dst_path
    |> finish
  ) in
  let tmp_path = tmp_like dst_path in
  let%lwt tmp_fd =
    Lwt_unix.(openfile tmp_path [O_WRONLY; O_CREAT; O_EXCL] st.Unix.st_perm) in
  Hashtbl.add rm_at_exit tmp_path ();
  Lwt_unix.fchown tmp_fd st.Unix.st_uid st.Unix.st_gid >>= fun () ->
  let stdout = `FD_move (Lwt_unix.unix_file_descr tmp_fd) in
  (match%lwt Lwt_process.exec ~stdout cmd with
   | Unix.WEXITED 0 -> Lwt.return_unit
   | process_status -> fail_with_process_status "sed" process_status)

let diff diff_style dst_path =
  let cmd = Cmdbuilder.(init "diff"
    |> string "--color=auto"
    |> (match diff_style with
        | `Default -> fun b -> b
        | `Context None -> string "-c"
        | `Context (Some n) -> string "-C" %> int n
        | `Unified None -> string "-u"
        | `Unified (Some n) -> string "-U" %> int n)
    |> string dst_path
    |> string (tmp_like dst_path)
    |> finish
  ) in
  (match%lwt Lwt_process.exec cmd with
   | Unix.WEXITED 0 -> Lwt.return_true
   | Unix.WEXITED 1 -> Lwt.return_false
   | process_status -> fail_with_process_status "diff" process_status)

let discard_change dst_path =
  let tmp_path = tmp_like dst_path in
  Lwt_unix.unlink tmp_path >|= fun () ->
  Hashtbl.remove rm_at_exit tmp_path

let apply_change dst_path =
  let tmp_path = tmp_like dst_path in
  Logs_lwt.info (fun p -> p "Patching %s." dst_path) >>= fun () ->
  Lwt_unix.rename tmp_path dst_path >|= fun () ->
  Hashtbl.remove rm_at_exit tmp_path

let cancel_on_signals signums p =
  let received_signum = ref None in
  let cancel_p signum = received_signum := Some signum; Lwt.cancel p in
  let hs = List.map (fun sn -> Lwt_unix.on_signal sn cancel_p) signums in
  Lwt.finalize
    (fun () -> p)
    (fun () ->
      List.iter Lwt_unix.disable_signal_handler hs;
      (match !received_signum with
       | None -> Lwt.return_unit
       | Some signum -> Unix.kill (Unix.getpid ()) signum; Lwt.return_unit))

let main scripts script_files wrap_length dst_paths diff_style =
  Logs.set_reporter (Logging.lwt_reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run @@ cancel_on_signals [1; 2; 15] begin
    Lwt.finalize begin fun () ->
      Lwt_list.iter_p (prepare scripts script_files wrap_length) dst_paths
        >>= fun () ->
      let%lwt unchanged, changed =
        Lwt_list.partition_s (diff diff_style) dst_paths in
      Lwt_list.iter_p discard_change unchanged >>= fun () ->
      if changed = [] then Logs_lwt.info (fun p -> p "No changes.") else
      if%lwt confirm "Apply changes?" then
        Lwt_list.iter_s apply_change changed
      else
        Lwt_list.iter_p discard_change changed
    end begin fun () ->
      Lwt.join
        (Hashtbl.fold (fun p () -> List.cons (Lwt_unix.unlink p)) rm_at_exit [])
    end
  end

let main_cmd =
  let open Cmdliner in

  let scripts =
    let doc = "Add $(i,EXPR) to the sed script to apply." in
    Arg.(value @@ opt_all string [] @@ info ~doc ~docv:"EXPR" ["e"]) in
  let script_files =
    let doc = "Add the contents of $(i,FILE) to the send script to apply." in
    Arg.(value @@ opt_all file [] @@ info ~doc ~docv:"FILE" ["f"]) in
  let dst_paths =
    let doc = "File which will be modified in place. \
               You will be asked before any files are overwritten." in
    Arg.(non_empty @@ pos_all file [] @@ info ~doc ~docv:"FILE" []) in
  let wrap_length =
    let doc = "Line-wrap length for the $(b,l) command." in
    Arg.(value @@ opt (some int) None @@ info ~doc ~docv:"WRAP-LENGTH" ["l"]) in

  let diff_context =
    let doc = "Use split context diff." in
    Arg.(value @@ flag @@ info ~doc ["c"]) in
  let diff_context_lines =
    let doc = "Use split context diff with $(i,N) context lines. \
               This option implies $(b,-c)." in
    Arg.(value @@ opt (some int) None @@ info ~docv:"N" ~doc ["C"]) in
  let diff_unified =
    let doc = "Use unified context diff." in
    Arg.(value @@ flag @@ info ~doc ["u"]) in
  let diff_unified_lines =
    let doc = "Use unified context diff with $(i,N) context lines. \
               This option implies $(b,-u)." in
    Arg.(value @@ opt (some int) None @@ info ~docv:"N" ~doc ["U"]) in

  let check_diff_args context context_lines unified unified_lines =
    (match (context, context_lines), (unified, unified_lines) with
     | (false, None), (false, None) -> Ok `Default
     | (true, _ | _, Some _), (false, None) -> Ok (`Context context_lines)
     | (false, None), (true, _ | _, Some _) -> Ok (`Unified unified_lines)
     | _ -> Error (`Msg "Inconsistent diff options.")) in
  let diff_args =
    Term.(const check_diff_args $
          diff_context $ diff_context_lines $
          diff_unified $ diff_unified_lines) in

  let term = Term.(
    const main $ scripts $ script_files $ wrap_length $ dst_paths $
    term_result diff_args
  ) in
  let info = Term.info ~doc:"An interative stream editor." "sedi" in
  (term, info)

let () = Cmdliner.Term.(eval main_cmd |> exit)
