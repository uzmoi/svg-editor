<script lang="ts">
  import { randString } from "emnorst";
  import type { Writable } from "svelte/store";
  import { insertAt } from "~/lib/array";
  import Button from "~/lib/Button.svelte";
  import DragHandle from "~/lib/DragHandle.svelte";
  import Icon from "~/lib/Icon.svelte";
  import SortableList from "~/lib/SortableList.svelte";
  import { Vec2 } from "~/lib/vec";
  import { getPoints, type PathItem } from "../store/path-item";

  export let path: Writable<readonly PathItem[]>;

  const handleChange = (e: CustomEvent<readonly PathItem[]>) => {
    $path = e.detail;
  };

  const addPathItem = (index: number) => {
    const pathItem: PathItem = {
      id: randString(8),
      type: "command",
      command: {
        type: "Line",
        to: Vec2.square(0),
      },
    };
    $path = insertAt($path, index, pathItem);
  };
  const deletePathItem = (id: string) => {
    $path = $path.filter(pathItem => pathItem.id !== id);
  };
</script>

<SortableList
  values={$path}
  key={pathItem => pathItem.id}
  on:change={handleChange}
  let:value={pathItem}
  let:index
  let:mousedown
>
  <div class="path-item">
    <div class="path-item-header">
      <p>{pathItem.type}</p>
      <Button variant="ghost" on:click={() => deletePathItem(pathItem.id)}>
        <Icon name="delete" />
      </Button>
      <DragHandle {mousedown} />
    </div>
    <ul class="path-item-points">
      {#each getPoints(pathItem) as [_pointKey, point]}
        <li>
          <input type="number" value={point.x} />
          <input type="number" value={point.y} />
        </li>
      {/each}
    </ul>
  </div>
  <div class="path-item-button">
    <Button variant="ghost" on:click={() => addPathItem(index + 1)}>
      + add path item
    </Button>
  </div>
</SortableList>

<style lang="scss">
  .path-item {
    background-color: var(--bg-primary);
  }
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
  .path-item-button {
    color: var(--pale-text);
    opacity: 1;
    transition: opacity 0.2s;
    :global([data-transform]) & {
      opacity: 0;
    }
  }
</style>
