open Machine;;
open Utils;;
let _ = Printf.printf "Program:\n" in
let _ = StrMap.iter (fun _ v -> Printf.printf "%s\n\n" (layout_func v)) default_g in
let initial_state = CallState {fd = StrMap.find "initial error" default_g "main"; v = []; k = Stop} in
machine (ref default_g) initial_state;;
