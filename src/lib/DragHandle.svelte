<script lang="ts">
  import Icon from "./Icon.svelte";

  export let mousedown: svelte.JSX.MouseEventHandler<HTMLSpanElement>;
</script>

<span on:mousedown={mousedown}>
  <Icon name="drag_handle" />
</span>

<style lang="scss">
  span {
    cursor: grab;
    :global([data-transform]) & {
      cursor: grabbing;
    }
  }
</style>
