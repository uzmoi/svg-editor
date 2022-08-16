import { Vec2 } from "~/lib/vec";
import type { PathCommand } from "./path-command";

export interface PathItem {
  id: string;
  command: PathCommand;
}

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
      command: {
        type: "Move",
        to: Vec2.square(0),
      },
    },
  ],
});
