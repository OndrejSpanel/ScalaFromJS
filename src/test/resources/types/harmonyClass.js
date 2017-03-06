class Person {
    constructor(name,age) {
        this.name = name;
        this.age= age;
    }
}

class Employee extends Person {
    constructor(name,age, salary ) {
        super(name, age);
        this.salary = salary;
    }

    printEmployeeDetails(){
        console.log(this.name + ' is ' + this.age + ' years old, and earns ' + this.salary);
    }
}

class Janitor extends Employee { //inheritance
    constructor(name) {
        super(name);
        this.age = 66;
        this.salary = 5000;
    }
}

let bob = new Janitor('Bob');
bob.printEmployeeDetails() // Bob is 66 years old, and earns 5000
