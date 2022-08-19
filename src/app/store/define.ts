import { writable } from "svelte/store";

export interface GradientStop {
  offset: number;
  color: string;
  opacity: number;
}

export type Define = {
  type: "Gradient";
  id: string;
  mode: "linear" | "radial";
  stops: readonly GradientStop[];
};

export const defines = writable<readonly Define[]>([]);
