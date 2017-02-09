/**
  * @param first {string}
  * @param last  {string}
  * @return string
  **/
def concatenate(first: String, last: String) = {
  var full: String = null
  full = first + last
  full
}

def secondFunction = {
  var result: String = null
  result = concatenate("Zara", "Ali")
  document.write(result)
}