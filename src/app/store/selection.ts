import { writable } from "svelte/store";
import { selector } from "~/lib/store/selector";
import { layers } from "./svg";

export const selectedLayerId = writable<string | null>(null);

export const selectedLayer = selector(
  [layers, selectedLayerId],
  ([layers, id]) => layers.find(layer => layer.id === id) ?? null,
  layer_ => {
    if (layer_ == null) {
      selectedLayerId.set(null);
    } else {
      layers.update(layers =>
        layers.map(layer => (layer.id === layer_.id ? layer_ : layer)),
      );
    }
  },
);
