<script lang="ts">
  import { modifyById } from "~/lib/array";
  import SortableList from "~/lib/SortableList.svelte";
  import type { Layer } from "../store/layer";
  import { selectedLayerId } from "../store/selection";
  import { layers, svg } from "../store/svg";

  const setLayers = (layers: readonly Layer[]) => {
    $svg = { ...$svg, layers };
  };
  const toggleShow = (layer: Layer) => {
    setLayers(modifyById($layers, { ...layer, show: !layer.show }));
  };
</script>

<SortableList
  values={$layers}
  key={layer => layer.id}
  on:change={e => setLayers(e.detail)}
  let:value={layer}
  let:mousedown
>
  <div class="layer" data-selected={layer.id === $selectedLayerId}>
    <p
      on:click={() => {
        $selectedLayerId = layer.id;
      }}
    >
      {layer.name}
    </p>
    <button on:click={() => toggleShow(layer)}>
      <!-- <Icon name={layer.show ? "visibility" : "visibility_off"} /> -->
    </button>
    <span on:mousedown={mousedown}>
      <!-- <Icon name="drag_handle" /> -->
    </span>
  </div>
</SortableList>

<style lang="scss">
  .layer {
    display: flex;
    > :first-child {
      flex-grow: 1;
    }
    &[data-selected="true"] {
      background-color: var(--accent);
    }
  }
</style>
