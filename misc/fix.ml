let fix f' =
  let r = ref (fun _ -> failwith "fix") in
  let f x = f' !r x in
  r := f; !r;;
