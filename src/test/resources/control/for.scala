def forShow() = {
  var s = 0
  for (a <- 0 until 10) yes()
  var b = 0
  while (b + 1 < 10) {
    yes()
    b += 1
  }
  var c = 0
  while (s < 10) {
    no()
    c += 1
  }
  var d = 0
  while (d < 10) {
    no()
    d += 1
    s += 1
  }
}
