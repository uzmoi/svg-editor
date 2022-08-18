<script lang="ts">
  import Icon from "~/lib/Icon.svelte";
  import SortableList from "~/lib/SortableList.svelte";
  import type { PathItem } from "../store/layer";
  import { getPoints } from "../store/path-command";

  export let path: readonly PathItem[];
</script>

<SortableList
  values={path}
  key={pathItem => pathItem.id}
  on:change={() => {}}
  let:value={pathItem}
  let:mousedown
>
  <div class="path-item">
    <div class="path-item-header">
      <p>{pathItem.command.type}</p>
      <p class="drag-handle" on:mousedown={mousedown}>
        <Icon name="drag_handle" />
      </p>
    </div>
    <ul class="path-item-points">
      {#each getPoints(pathItem.command) as [_pointKey, point]}
        <li>
          <input type="number" value={point.x} />
          <input type="number" value={point.y} />
        </li>
      {/each}
    </ul>
  </div>
  <button class="add-path-item-button">+ add path item</button>
</SortableList>

<style lang="scss">
  .path-item-header {
    display: flex;
    font-size: 0.9em;
    > :first-child {
      flex-grow: 1;
    }
  }
  .path-item-points {
    padding-left: 1em;
  }
  .add-path-item-button {
    border: none;
    background-color: transparent;
    color: var(--pale-text);
    opacity: 1;
    transition: opacity 0.2s;
    :global([data-transform]) & {
      opacity: 0;
    }
  }
</style>
