open Adder

let%test _ = addlist [] = 0
let%test _ = addlist [3] = 3
let%test _ = addlist [1;2] = 3
let%test _ = addlist [1;2;3] = 6
let%test _ = addlist [11; 15; 24] = 50
let%test _ = addlist [7;16;9;27] = 59
let%test _ = addlist [836; 111; 2222] = 3169