<script lang="ts">
  import { modifyById } from "~/lib/array";
  import DragHandle from "~/lib/DragHandle.svelte";
  import Icon from "~/lib/Icon.svelte";
  import SortableList from "~/lib/SortableList.svelte";
  import type { Layer } from "../store/layer";
  import { selectedLayerId } from "../store/selection";
  import { layers } from "../store/svg";

  const toggleShow = (layer: Layer) => {
    $layers = modifyById($layers, { ...layer, show: !layer.show });
  };
  const handleChange = (e: CustomEvent<readonly Layer[]>) => {
    $layers = e.detail;
  };
</script>

<SortableList
  values={$layers}
  key={layer => layer.id}
  on:change={handleChange}
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
    <button class="show-button" on:click={() => toggleShow(layer)}>
      <Icon name={layer.show ? "visibility" : "visibility_off"} />
    </button>
    <DragHandle {mousedown} />
  </div>
</SortableList>

<style lang="scss">
  .layer {
    display: flex;
    > :first-child {
      flex: 1;
      overflow: hidden;
      white-space: nowrap;
      text-overflow: ellipsis;
      cursor: pointer;
    }
    padding: 0.4em;
    user-select: none;
    &[data-selected="true"] {
      background-color: var(--accent);
    }
  }
  .show-button {
    border: none;
    background-color: transparent;
    color: inherit;
    font-size: 1em;
  }
</style>
