import { derived, writable } from "svelte/store";
import { selector } from "~/lib/store/selector";
import type { Layer } from "./layer";

export interface Bounds {
  top: number;
  bottom: number;
  left: number;
  right: number;
}

export interface BoxSize {
  width: number;
  height: number;
}

export interface SvgState {
  size: BoxSize;
  layers: readonly Layer[];
}

export const svg = writable<SvgState>({
  size: { width: 100, height: 100 },
  layers: [],
});

export const svgSize = derived(svg, svg => svg.size);

export const layers = selector(
  svg,
  svg => svg.layers,
  layers => {
    svg.update(svg => ({ ...svg, layers }));
  },
);
