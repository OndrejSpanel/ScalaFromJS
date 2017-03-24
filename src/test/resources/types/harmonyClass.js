class Person {
    constructor(name,age) {
        this.name = name;
        this.age= age;
    }

    nameFunc() {return this.name}

    get nameGetter() {return this.name}
}

class Employee extends Person {
    constructor(name,age, salary ) {
        super(name, age);
        this.salary = salary;
        var localVar = age;
        
    }

    printEmployeeDetails(){
        console.log(this.name + ' is ' + this.age + ' years old, and earns ' + this.salary);
    }
}

class Janitor extends Employee {
    constructor(name) {
        super(name, 66, 5000);
    }
}

let bob = new Janitor('Bob');
bob.printEmployeeDetails();

var x, y;

if (true) x = bob.nameFunc();
if (true) y = bob.nameGetter();