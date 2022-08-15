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

export const layer = (id: string, path: readonly PathItem[]): Layer => ({
  id,
  name: "Layer",
  show: true,
  path,
});
