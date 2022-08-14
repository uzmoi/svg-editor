<script lang="ts">
  import { Vec2 } from "../lib/vec";
  import Header from "./Header.svelte";
  import Canvas from "./canvas/Canvas.svelte";

  let canvasContainer: HTMLDivElement;

  let cursorPos = Vec2.square(0);

  const handleCenterPanelMouseMove: svelte.JSX.MouseEventHandler<HTMLDivElement> = e => {
    const canvasRect = canvasContainer.getBoundingClientRect();
    const offset = new Vec2(e.clientX - canvasRect.left, e.clientY - canvasRect.top);
    const width = 100;
    const height = 100;
    const canvasRate = new Vec2(width / canvasRect.width, height / canvasRect.height);
    cursorPos = offset.mul(canvasRate);
  };
</script>

<div class="root">
  <div class="header">
    <Header {cursorPos} />
  </div>
  <div class="main">
    <div class="center-panel" on:mousemove={handleCenterPanelMouseMove}>
      <Canvas bind:container={canvasContainer} />
    </div>
    <div class="right-panel">
      <!-- layer info -->
    </div>
  </div>
</div>

<style lang="scss">
  .root {
    display: flex;
    flex-direction: column;
    width: 100vw;
    height: 100vh;
    overflow: hidden;

    @media (prefers-color-scheme: light) {
      --bg-primary: #eee;
      --bg-secondary: #ddd;
      --accent: #88d;
      --text: #333;
      --error: #d44;
    }
    @media (prefers-color-scheme: dark) {
      --bg-primary: #333;
      --bg-secondary: #222;
      --accent: #448;
      --text: #eee;
      --error: #c44;
    }

    font-family: "Noto Sans JP", sans-serif;
    color: var(--text);
    background-color: var(--bg-secondary);
  }
  .header {
    height: 8em;
    background-color: var(--bg-primary);
  }
  .main {
    display: flex;
    flex-grow: 1;
  }
  .center-panel {
    flex-grow: 1;
  }
  .right-panel {
    display: flex;
    flex-direction: column;
    width: 16em;
    background-color: var(--bg-primary);
  }
</style>
