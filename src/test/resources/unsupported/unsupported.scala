def d() = {
  var a = 123
  debuggger
  for (x <- 0 until 3) if (1 == x) /* Break */ break;
  return a
}
