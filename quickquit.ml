
type process = {
  pid: int;
  basename: string;
} [@@deriving show]

type window = {
  id: string;
  title: string;
  proc: process;
  hidden: bool;
} [@@deriving show]

module Target = struct
    type t = window -> bool Lwt.t

    let const: bool -> t = fun c _ -> Lwt.return c

    let exact_basename: string -> t = fun basename w ->
      Lwt.return (CCString.equal w.proc.basename basename)

    let _or_: t -> t -> t = fun l r w ->
      let%lwt l = l w
      and r = r w
      in Lwt.return (l || r)
end

module Targets = struct

  open Target

  let none: t = const false

  let dolphin: t = exact_basename "dolphin"

  let firefox: t = _or_ (exact_basename "firefox") (exact_basename "firefox-esr")

  let chrome: t = exact_basename "chrome"

  let term: t = _or_ (exact_basename "termite") (exact_basename "xfce4-terminal")

  let emacs: t = _or_ (exact_basename "emacs") (exact_basename "emacs-26.1")

  let get_target (name: string): t =
    match CCString.lowercase name with
    | "dolphin" -> dolphin
    | "firefox" -> firefox
    | "chrome" -> chrome
    | "term" -> term
    | "emacs" -> emacs
    | _ -> none (* TODO: add warning *)

end

module type S_platform = sig

  val get_windows: unit -> (window list) Lwt.t
  val switch_to_window_id: string -> unit Lwt.t
  val get_active_window_id: unit -> string Lwt.t

end

module Platform_Linux_X11 = struct

  let sprintf = CCFormat.sprintf

  (* construct Lwt_process command *)
  let cmd cmd = ("", CCString.split ~by:" " cmd |> CCArray.of_list)

  let str_append (s: string) (c: char): string = s ^ String.make 1 c

  (* split one line in text output from shell command to columns *)
  let split_output (src: string) (seg: int): string list =
    let f ((cur, l) : string * string list) (c: char): string * string list =
      if List.length l + 1 >= seg then (str_append cur c, l)
      else
        match c with
        | ' ' | '\t' -> if String.length cur > 0 then ("", cur :: l) else (cur, l)
        | c -> (str_append cur c, l)
    in
    let (cur, l) = CCString.fold f ("", []) src in
    List.rev (cur :: l)

  let get_process (pid: int): process Lwt.t =
    let execpath = Unix.readlink (sprintf "/proc/%u/exe" pid) in
    Lwt.return { pid; basename = Filename.basename execpath }

  let get_hidden (id: string): bool Lwt.t =
    let%lwt t = Lwt_process.pread (cmd (sprintf "xprop -id %s _NET_WM_STATE" id)) in
    let t = String.trim t in
    match t with
    | "_NET_WM_STATE:  not found."
    | "_NET_WM_STATE(ATOM) =" -> Lwt.return false
    | "_NET_WM_STATE(ATOM) = _NET_WM_STATE_HIDDEN" -> Lwt.return true
    (* | otherwise -> print_endline id; print_endline otherwise; Lwt.return true *)

  let get_windows (): (window list) Lwt.t =
    Lwt_process.pread_lines (cmd "wmctrl -lGpx") |>
    Lwt_stream.map_s (fun s ->
      let id :: desk :: pid :: l :: t :: w :: h :: klass :: hostname :: title :: _ =
        split_output s 10
        (* CCString.split ~by:" " s *)
      in
      let%lwt proc = get_process (int_of_string pid)
      and hidden = get_hidden id in
        Lwt.return { id; title; proc; hidden }) |>
    Lwt_stream.to_list

  let switch_to_window_id (id: string): unit Lwt.t =
    let%lwt _ = Lwt_process.exec (cmd (sprintf "wmctrl -ia %s" id)) in
    Lwt.return ()

  let get_active_window_id (): string Lwt.t =
    let%lwt t = Lwt_process.pread (cmd "xprop -root 32x \t$0 _NET_ACTIVE_WINDOW") in
    let _ :: id :: _ = split_output t 2 in
    Lwt.return id
end

let get_platform (): (module S_platform) = (module Platform_Linux_X11: S_platform)

(* let lwt_main () =
 *   let%lwt windows = Platform_Linux_X11.get_windows () in
 *   CCList.iter (fun w -> w |> show_window |> print_endline) windows;
 *   Lwt.return () *)

module Cmds = struct

  module Window_compare = struct
    type t = {
      hidden: bool;
      id: string;
    } [@@deriving ord]

    let of_window (src: window): t =
      { hidden = src.hidden; id = src.id }
  end

  let switch (target: string) (): unit Lwt.t =
    let filter = Targets.get_target target in
    let module Platform = (val get_platform ()) in
    let%lwt windows = Platform.get_windows ()
    and active_id = Platform.get_active_window_id () in
    let%lwt filtered_windows = Lwt_list.filter_p filter windows in
    let filtered_windows = List.sort
        (fun x y -> Window_compare.(
          compare (of_window x) (of_window y)))
        filtered_windows in
    CCList.iter (fun w -> w |> show_window |> print_endline) filtered_windows;
    active_id |> print_endline;
    let%lwt _ =
      if List.length filtered_windows > 0
      then
        let target = List.nth filtered_windows 0 in
        Platform.switch_to_window_id target.id
      else Lwt.return ()
    in
    Lwt.return ()

end

module Cli = struct
  open Cmdliner

  let cmd_switch =
    let arg_target =
      let doc = "Switch to a specific target" in
      Arg.(required & pos 0 (some string) None (info ~doc []))
    in
    (Term.(const Cmds.switch $ arg_target $ const ()),
     Term.info "switch")

  let cmds: ('a Term.t * Term.info) list = List.map
      (fun (c, i) -> (Term.(const Lwt_main.run $ c), i))
      [cmd_switch]

  let main () = Term.(exit @@ eval_choice (List.nth cmds 0) cmds)
end

let () =
  Printexc.record_backtrace true;
  Cli.main ()
