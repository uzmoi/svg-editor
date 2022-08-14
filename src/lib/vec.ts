export class Vec2<T> {
  constructor(public x: T, public y: T) {}
  add(this: Vec2<number>, v: Vec2<number>) {
    return new Vec2(this.x + v.x, this.y + v.y);
  }
  sub(this: Vec2<number>, v: Vec2<number>) {
    return new Vec2(this.x - v.x, this.y - v.y);
  }
  mul(this: Vec2<number>, v: Vec2<number>) {
    return new Vec2(this.x * v.x, this.y * v.y);
  }
  div(this: Vec2<number>, v: Vec2<number>) {
    return new Vec2(this.x / v.x, this.y / v.y);
  }
  map<U>(this: Vec2<T>, f: (value: T) => U) {
    return new Vec2(f(this.x), f(this.y));
  }
  ap<U, V>(this: Vec2<(x: U) => V>, v: Vec2<U>) {
    return new Vec2(this.x(v.x), this.y(v.y));
  }
  static square<T>(x: T) {
    return new Vec2(x, x);
  }
}
