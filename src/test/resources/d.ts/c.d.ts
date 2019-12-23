export class C {
    constructor(cn: number, cs: string, cb: boolean);
}

export class CX {
    constructor(cn: number, cs: string, cb: boolean);
}

export class CS {
    f(a: number): number;
    g(a: string): string;

    static f(a: boolean, b: boolean): boolean;
    static g(a: boolean, b: boolean): boolean;
}