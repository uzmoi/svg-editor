import type { Vec2 } from "~/lib/vec";

export type PathCommand =
  | { type: "M"; to: Vec2<number> }
  | { type: "L"; to: Vec2<number> }
  | { type: "T"; to: Vec2<number> }
  | { type: "Q"; v1: Vec2<number>; to: Vec2<number> }
  | { type: "S"; v2: Vec2<number>; to: Vec2<number> }
  | { type: "C"; v1: Vec2<number>; v2: Vec2<number>; to: Vec2<number> }
  | { type: "Z" };
