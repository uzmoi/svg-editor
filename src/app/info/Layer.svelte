<script lang="ts">
  import Radio from "~/lib/radio/Radio.svelte";
  import RadioGroup from "~/lib/radio/RadioGroup.svelte";
  import { selectedLayer } from "../store/selection";
  import Path from "./Path.svelte";

  const tabs = ["Styles", "Commands", "Attributes"] as const;
  let tab: typeof tabs[number] = "Styles";
</script>

{#if $selectedLayer != null}
  <div class="layer-profile">
    <input type="text" class="layer-name-input" value={$selectedLayer.name} />
    <button>
      <!-- <Icon name="delete" /> -->
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
      <Path path={$selectedLayer.path} />
    {:else if tab === "Attributes"}
      Attributes
    {/if}
  </div>
{/if}

<style lang="scss">
  .layer-profile {
    padding: 0.8em;
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
</style>
