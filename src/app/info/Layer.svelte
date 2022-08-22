<script lang="ts">
  import type { Writable } from "svelte/store";
  import Icon from "~/lib/Icon.svelte";
  import Radio from "~/lib/radio/Radio.svelte";
  import RadioGroup from "~/lib/radio/RadioGroup.svelte";
  import { selector } from "~/lib/store/selector";
  import type { Layer } from "../store/layer";
  import Path from "./Path.svelte";

  export let layer: Writable<Layer>;

  const tabs = ["Styles", "Commands", "Attributes"] as const;
  let tab: typeof tabs[number] = "Styles";

  const path = selector(
    layer,
    layer => layer.path,
    path => {
      layer.update(layer => ({ ...layer, path }));
    },
  );
</script>

<div class="layer-info">
  <div class="layer-profile">
    <input type="text" class="layer-name-input" value={$layer.name} />
    <button class="delete-button">
      <Icon name="delete" />
    </button>
  </div>
  <RadioGroup name="layer-info-tab" bind:value={tab}>
    <div class="tabs">
      {#each tabs as value}
        <div class="tab">
          <Radio {value} let:selected>
            <p class="tab-name" data-selected={selected}>
              {value}
            </p>
          </Radio>
        </div>
      {/each}
    </div>
  </RadioGroup>
  <div class="tab-contents">
    {#if tab === "Styles"}
      Styles
    {:else if tab === "Commands"}
      <Path {path} />
    {:else if tab === "Attributes"}
      Attributes
    {/if}
  </div>
</div>

<style lang="scss">
  .layer-info {
    height: 100%;
    display: flex;
    flex-direction: column;
  }
  .layer-profile {
    padding: 0.8em;
  }
  .delete-button {
    border: none;
    background-color: transparent;
    color: inherit;
    font-size: 1em;
  }
  .tabs {
    display: flex;
    flex-wrap: wrap;
    > .tab {
      flex: 1 0;
    }
  }
  .tab-name {
    padding: 0.2em;
    padding-inline: 0.5em;
    transition: 0.4s;
    border-bottom: 0.2em solid var(--pale-text);
    &[data-selected="true"] {
      border-color: var(--accent);
    }
  }
  .tab-contents {
    flex: 1 0 0;
    overflow-y: auto;
    padding: 0.8em;
  }
</style>
