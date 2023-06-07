export const A0: 0;
export const A1: 1;
export const A2: 2;

export type A = typeof A0 | typeof A1 | typeof A2;

export var aVar: A;

export function processE(a: A): string;
