export class Base {
    constructor(a: string, b: ArrayLike<any>, c: ArrayLike<any>, d?: number);

}

export class Derived extends Base {
    constructor(a: string, b: any[], c: any[], d?: number);
}
