<script lang="ts">
  import type { Vec2 } from "../lib/vec";

  export let cursorPos: Vec2<number>;
</script>

<header>
  <h1>Svg Editor</h1>
  <p>{cursorPos.x.toFixed(1)}, {cursorPos.y.toFixed(1)}</p>
</header>

<style lang="scss">
  h1 {
    margin: 0.3em;
    font-size: 1.5em;
  }
</style>
