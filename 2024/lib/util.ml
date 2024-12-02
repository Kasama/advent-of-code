let (--) start finish =
    let rec range n acc =
      if n < start then acc else range (n-1) (n :: acc)
    in range finish []
