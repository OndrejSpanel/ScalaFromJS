export interface I1 {

}

export interface I2 extends I1 {

}

export class A implements I2 {

}

export class B implements I2 {

}

export class C implements I1 {

}
