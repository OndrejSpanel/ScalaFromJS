class Employee {
    constructor(name,age, salary ) { //constructors!
        this.name = name;
        this.age= age;
        this.salary = salary;
    }

    printEmployeeDetails(){
        console.log(this.name + ' is ' + this.age + ' years old, and earns ' + this.salary);
    }
}

class Janitor extends Employee { //inheritance
    constructor(name) {
        super(name); //call the parent constructor with super
        this.age = 66;
        this.salary = 5000;
    }
}

let bob = new Janitor('Bob');
bob.printEmployeeDetails() // Bob is 66 years old, and earns 5000