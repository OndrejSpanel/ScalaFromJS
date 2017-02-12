def forShow() = {
  var s = 0
  for (a <- 0 until 10) yes()
  var b = 0
  while (b + 1 < 10) {
    yes()
    b ++
  }
  var c = 0
  while (s < 10) {
    no()
    c ++
  }
  var d = 0
  while (d < 10) {
    no()
    d ++;
    s ++
  }
}
