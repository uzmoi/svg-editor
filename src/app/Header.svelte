<script lang="ts">
  import type { Vec2 } from "~/lib/vec";
  import { scale } from "./store/canvas-transform";

  export let cursorPos: Vec2<number>;
</script>

<header>
  <h1>Svg Editor</h1>
  <div class="menu-bar">
    <p>{$scale}%</p>
    <p>{cursorPos.x.toFixed(1)}, {cursorPos.y.toFixed(1)}</p>
  </div>
</header>

<style lang="scss">
  h1 {
    margin: 0.3em;
    font-size: 1.5em;
  }
  .menu-bar {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    font-size: 0.9em;
    > * {
      margin: 0.2em;
      & + * {
        margin-left: 0.4em;
      }
    }
  }
</style>
