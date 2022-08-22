import { Vec2 } from "~/lib/vec";
import type { PathItem } from "./path-item";

export interface Layer {
  id: string;
  name: string;
  show: boolean;
  path: readonly PathItem[];
}

export const layer = (id: string): Layer => ({
  id,
  name: `Layer (${id})`,
  show: true,
  path: [
    {
      id: "START",
      type: "command",
      command: {
        type: "Move",
        to: Vec2.square(0),
      },
    },
  ],
});
