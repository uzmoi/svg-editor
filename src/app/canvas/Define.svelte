<script lang="ts">
  import { defines } from "../store/define";
</script>

<defs>
  {#each $defines as define}
    {#if define.type === "Gradient"}
      {@const elementName = `${define.mode}Gradient`}
      <svelte:element this={elementName} id={define.id}>
        {#each define.stops as stop}
          <stop
            offset="{stop.offset}%"
            stop-color={stop.color}
            stop-opacity={stop.opacity}
          />
        {/each}
      </svelte:element>
    {/if}
  {/each}
</defs>
