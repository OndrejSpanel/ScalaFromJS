// from http://stackoverflow.com/a/387733/16673
// Define a class like this
class Person(name: String, gender: String) {
  // Add methods like this.  All Person objects will be able to invoke this
  def speak(): Unit = alert("Howdy, my name is" + this.name)
}

// Instantiate new objects with 'new'
var person = new Person("Bob", "M")

// Invoke methods like this
person.speak() // alerts "Howdy, my name is Bob"