<script lang="ts">
  import { scale, translate } from "../store/canvas-transform";
  import { layers, svgSize } from "../store/svg";
  import Layer from "./Layer.svelte";

  export let container: HTMLDivElement | undefined = undefined;

  $: size = 100 / $scale;
  $: transform = [
    `translate(${$translate.x}px, ${$translate.y}px)`,
    `scale(${$scale / 100})`,
  ].join(" ");
</script>

<div
  bind:this={container}
  class="canvas-container transparent-checkered"
  style:--checker-size="{size * 80}px"
  style:transform
>
  <svg class="canvas" width={$svgSize.width} height={$svgSize.height}>
    {#each $layers.filter(layer => layer.show) as layer}
      <Layer {layer} />
    {/each}
    <!-- defines -->
  </svg>
  <svg class="overlay-canvas" width={$svgSize.width} height={$svgSize.height}>
    <!-- overlay -->
  </svg>
</div>

<style lang="scss">
  @use "../../styles/transparent-checkered.scss";

  .canvas-container {
    display: inline-block;
    position: relative;
    transform-origin: 0 0;
    > * {
      position: absolute;
      top: 0;
      left: 0;
      bottom: 0;
      right: 0;
    }
  }
  svg {
    vertical-align: top;
  }
  .canvas {
    position: static;
  }
  .overlay-canvas {
    overflow: visible;
  }
</style>
