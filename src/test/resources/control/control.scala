def control(a: Double, b: Boolean) = {
  var aa = a
  if (b) {
    do aa += 1
    while (!b)
    {
      while (!b) {
        aa += 2
        true
      }
    }
  }
  else {
    if (!b) aa = 100
    for (i <- 0 until 3) a += 10
  }
  return aa
}
def f() = {
  control(10, true)
  control(15, false)
}
