<script lang="ts">
  import { Vec2 } from "../lib/vec";
  import Header from "./Header.svelte";
  import Canvas from "./canvas/Canvas.svelte";
  import { changeScale, translate } from "./store/canvas-transform";

  let canvasContainer: HTMLDivElement;

  let cursorPos = Vec2.square(0);
  let cursorPosInCanvas = Vec2.square(0);
  $: if (canvasContainer) {
    const canvasRect = canvasContainer.getBoundingClientRect();
    const offset = cursorPos.sub(new Vec2(canvasRect.left, canvasRect.top));
    const width = 100;
    const height = 100;
    const canvasRate = new Vec2(width / canvasRect.width, height / canvasRect.height);
    cursorPosInCanvas = offset.mul(canvasRate);
  }

  type Dragging = { target: "canvas"; startTranslate: Vec2<number> };
  let dragging: Dragging | null = null;
  $: if (dragging != null) {
    switch (dragging.target) {
      case "canvas":
        $translate = dragging.startTranslate.add(cursorPos);
        break;
    }
  }

  const handleCenterPanelMouseMove: svelte.JSX.MouseEventHandler<HTMLDivElement> = e => {
    cursorPos = new Vec2(e.clientX, e.clientY);
  };

  const handleCenterPanelMouseDown: svelte.JSX.MouseEventHandler<HTMLDivElement> = e => {
    switch (e.button) {
      case 1:
        dragging = {
          target: "canvas",
          startTranslate: $translate.sub(cursorPos),
        };
        break;
    }
  };

  const handleCenterPanelWheel: svelte.JSX.WheelEventHandler<HTMLDivElement> = e => {
    const canvasRect = canvasContainer.getBoundingClientRect();
    const offset = new Vec2(e.clientX - canvasRect.left, e.clientY - canvasRect.top);
    const isEnlarge = Math.sign(e.deltaY) === -1;
    changeScale(isEnlarge, offset);
  };
</script>

<svelte:window
  on:mouseup={() => {
    dragging = null;
  }}
/>

<div class="root">
  <div class="header">
    <Header cursorPos={cursorPosInCanvas} />
  </div>
  <div class="main">
    <div
      class="center-panel"
      on:wheel|preventDefault|stopPropagation={handleCenterPanelWheel}
      on:mousedown={handleCenterPanelMouseDown}
      on:mousemove={handleCenterPanelMouseMove}
    >
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
    z-index: 2;
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
    z-index: 1;
    display: flex;
    flex-direction: column;
    width: 16em;
    background-color: var(--bg-primary);
  }
</style>
