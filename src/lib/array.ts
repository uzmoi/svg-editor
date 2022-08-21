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

export const modifyById = <T extends { id: unknown }>(arr: readonly T[], el: T) =>
  arr.map(x => (x.id === el.id ? el : x));

export const deletedAt = <T extends unknown>(xs: readonly T[], i: number) =>
  xs.imm(xs => {
    xs.splice(i, 1);
  });

export const insertAt = <T extends unknown>(xs: readonly T[], i: number, x: T) =>
  xs.imm(xs => {
    xs.splice(i, 0, x);
  });

export const updatedAt = <T extends unknown>(xs: readonly T[], i: number, x: T) =>
  xs.imm(xs => {
    xs.splice(i, 1, x);
  });

export const replacedAt = <T extends unknown>(
  xs: readonly T[],
  i: number,
  zs: readonly T[],
) =>
  xs.imm(xs => {
    xs.splice(i, zs.length, ...zs);
  });
