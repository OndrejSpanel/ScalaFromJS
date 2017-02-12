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

    {
      var i = 0
      while (i < 3) {
        aa += 10
        i += 1
      }
    }
  }
  return aa
}
def f() = {
  control(10, true)
  control(15, false)
}
