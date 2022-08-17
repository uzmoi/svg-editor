import { has } from "emnorst";
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

const printPathCommandArgs = (...commandArgs: (Vec2<number> | number)[]) =>
  commandArgs.flatMap(arg => (typeof arg === "number" ? arg : [arg.x, arg.y])).join(" ");

export const printPathCommand = (command: PathCommand): string => {
  switch (command.type) {
    case "Move":
      return "M" + printPathCommandArgs(command.to);
    case "Line":
      return "L" + printPathCommandArgs(command.to);
    case "Bezier2":
      if (command.v1 === null) {
        return "T" + printPathCommandArgs(command.to);
      } else {
        return "Q" + printPathCommandArgs(command.v1, command.to);
      }
    case "Bezier3":
      if (command.v1 === null) {
        return "S" + printPathCommandArgs(command.v2, command.to);
      } else {
        return "C" + printPathCommandArgs(command.v1, command.v2, command.to);
      }
    case "Arc":
      return (
        "A" +
        printPathCommandArgs(
          command.radius,
          command.rotation,
          command.largeArc,
          command.sweep,
          command.to,
        )
      );
    case "Close":
      return "Z";
  }
};

export const getPoints = (command: PathCommand): ["v1" | "v2" | "to", Vec2<number>][] => {
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
};
