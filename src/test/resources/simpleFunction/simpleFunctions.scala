// just type inference and expressions, no calls or parameters
def firstFunction = {
  var s: Any = null
  val a = "A"
  s = a + a
  s
}

def secondFunction = {
  var x: Any = null
  val x1 = 1
  x = x1 + x1
  x
}

