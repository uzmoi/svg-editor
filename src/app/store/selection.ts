import { derived, writable } from "svelte/store";
import { layers } from "./svg";

export const selectedLayerId = writable<string | null>(null);

export const selectedLayer = derived(
  [selectedLayerId, layers],
  ([id, layers]) => layers.find(layer => layer.id === id) ?? null,
);
