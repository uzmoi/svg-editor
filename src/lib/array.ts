interface Imm<T> {
  imm(f: (xs: T[]) => void): T[];
}

declare global {
  interface Array<T> extends Imm<T> {}
  interface ReadonlyArray<T> extends Imm<T> {}
}

Array.prototype.imm = function <T>(this: readonly T[], f: (xs: T[]) => void) {
  const arr = this.slice();
  f(arr);
  return arr;
};

export const moveEl = <T>(arr: readonly T[], i: number, j: number, count = 1) => {
  if (i === j) {
    return arr;
  }
  return arr.imm(arr => {
    arr.splice(j, 0, ...arr.splice(i, count));
  });
};
