def foo() = {
}
def bar() = {
  foo()
  foo()
  while (true) {
    foo()
    foo()
  }
}
