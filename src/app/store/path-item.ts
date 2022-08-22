import { has } from "emnorst";
import type { Vec2 } from "~/lib/vec";
import type { PathCommand } from "./path-command";

export type PathItem = { id: string } & (
  | { type: "command"; command: PathCommand }
  | { type: never }
);

export const getPoints = (pathItem: PathItem): ["v1" | "v2" | "to", Vec2<number>][] => {
  switch (pathItem.type) {
    case "command": {
      const command = pathItem.command;
      const points: ["v1" | "v2" | "to", Vec2<number>][] = [];
      if (has(command, "v1") && command.v1 != null) {
        points.push(["v1", command.v1]);
      }
      if (has(command, "v2") && command.v2 != null) {
        points.push(["v2", command.v2]);
      }
      if (has(command, "to")) {
        points.push(["to", command.to]);
      }
      return points;
    }
  }
};
