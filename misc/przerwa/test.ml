let rec gen n acc = if n == 0 then acc else gen (n - 1) (n :: acc);;

PrzerwaSort.przerwa (gen 1000000 []);;
