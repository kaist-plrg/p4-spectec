type mode = [ `Full | `Concise ]

let current : mode ref = ref `Concise
let set mode = current := mode
let get () = !current
