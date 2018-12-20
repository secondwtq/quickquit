
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

    let exact_basename: string -> t = fun basename w ->
      Lwt.return (CCString.equal w.proc.basename basename)
end

module Targets = struct

  open Target

  let dolphin: t = exact_basename "dolphin"

  let firefox: t = exact_basename "firefox"

  let chrome: t = exact_basename "chrome"

  let termite: t = exact_basename "termite"

  let emacs: t = exact_basename "emacs"

end

module type S_platform = sig

  val get_windows: unit -> (window list) Lwt.t
  val switch_to_window: window -> unit Lwt.t

end

module Platform_Linux_X11 = struct
  let cmd cmd = ("", CCString.split ~by:" " cmd |> CCArray.of_list)

  let get_process (pid: int): process Lwt.t =
    let execpath = Unix.readlink (CCFormat.sprintf "/proc/%u/exec" pid) in
    let basename = Filename.basename execpath in
    Lwt.return { pid; basename }

  let get_hidden (id: string): bool Lwt.t =
    let cmdline = CCFormat.sprintf "xprop -id %s _NET_WM_STATE" id in
    let%lwt t = Lwt_process.pread (cmd cmdline) in
    Lwt.return false

  let get_windows (): (window list) Lwt.t =
    Lwt_process.pread_lines (cmd "wmctrl -lGpx") |>
    Lwt_stream.map_s (fun s ->
      print_endline s;
      let id :: desk :: pid :: l :: t :: w :: h :: klass :: hostname :: title :: _ =
        CCString.split ~by:" " s
      in
      let%lwt proc = get_process (int_of_string pid)
      and hidden = get_hidden id in
        Lwt.return { id; title; proc; hidden }) |>
    Lwt_stream.to_list

  let switch_to_window (w: window): unit Lwt.t =
    let%lwt _ = Lwt_process.exec (cmd (CCFormat.sprintf "wmctrl -ia %s" w.id)) in
    Lwt.return ()
end

let lwt_main () =
  let%lwt windows = Platform_Linux_X11.get_windows () in
  CCList.iter (fun w -> w |> show_window |> print_endline) windows;
  Lwt.return ()

let () =
  Printexc.record_backtrace true;
  Lwt_main.run (lwt_main ())
