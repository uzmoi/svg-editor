<script lang="ts">
  import { Vec2 } from "~/lib/vec";
  import Header from "./Header.svelte";
  import Canvas from "./canvas/Canvas.svelte";
  import { changeScale, translate } from "./store/canvas-transform";
  import LayerList from "./info/LayerList.svelte";
  import Layer from "./info/Layer.svelte";
  import { layers } from "./store/svg";
  import { layer } from "./store/layer";
  import { randString } from "emnorst";
  import { selectedLayer } from "./store/selection";
  import type { Writable } from "svelte/store";
  import Button from "~/lib/Button.svelte";

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

  const addLayer = () => {
    const id = randString(8);
    $layers = [...$layers, layer(id)];
  };

  const asNonNullStore = <T extends unknown>(store: Writable<T | null>) =>
    store as Writable<T>;
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
      <div class="layer-list">
        <LayerList />
      </div>
      <div class="layer-actions">
        <Button on:click={addLayer}>add layer</Button>
      </div>
      <div class="layer-info">
        {#if $selectedLayer != null}
          <Layer layer={asNonNullStore(selectedLayer)} />
        {/if}
      </div>
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
      --pale-text: #666;
      --error: #d44;
    }
    @media (prefers-color-scheme: dark) {
      --bg-primary: #333;
      --bg-secondary: #222;
      --accent: #448;
      --text: #eee;
      --pale-text: #bbb;
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
    > .center-panel {
      flex-grow: 1;
    }
  }
  .right-panel {
    z-index: 1;
    display: flex;
    flex-direction: column;
    width: 16em;
    background-color: var(--bg-primary);
    > .layer-list {
      overflow-y: scroll;
      height: 16em;
    }
    > .layer-info {
      flex-grow: 1;
    }
  }
</style>
