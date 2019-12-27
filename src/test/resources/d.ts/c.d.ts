export class C {
    constructor(cn: number, cs: string, cb: boolean);
}

export class CX {
    constructor(cn: number, cs: string, cb: boolean);
}

export interface CSI {
    f(a: number): number;
}

export interface CSF {
    fff(a: number): number;
    fv: number;
    fs: string;
}

export class CS implements CSI {
    f(a: number): number;
    g(a: string): string;

    static f(a: boolean, b: boolean): boolean;
    static g(a: boolean, b: boolean): boolean;
}

export function createCSI(): CSI;