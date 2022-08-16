import type { Vec2 } from "~/lib/vec";

export type PathCommand =
  | {
      type: "Move" | "Line";
      to: Vec2<number>;
    }
  | {
      type: "Bezier2";
      v1: Vec2<number> | null;
      to: Vec2<number>;
    }
  | {
      type: "Bezier3";
      v1: Vec2<number> | null;
      v2: Vec2<number>;
      to: Vec2<number>;
    }
  | {
      type: "Arc";
      radius: Vec2<number>;
      rotation: number;
      largeArc: 0 | 1;
      sweep: 0 | 1;
      to: Vec2<number>;
    }
  | { type: "Close" };
