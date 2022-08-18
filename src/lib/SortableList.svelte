<script lang="ts">
  import { clamp } from "emnorst";
  import { createEventDispatcher } from "svelte";
  import { slide } from "svelte/transition";
  import { moveEl } from "./array";

  const dispath = createEventDispatcher<{
    change: readonly T[];
  }>();

  type T = $$Generic;
  export let values: readonly T[];
  export let key: (value: T) => unknown;

  let containerEl: HTMLUListElement;
  let draggingIndex = -1;
  let dropDestinationIndex = -1;
  let dragHandleOffset = 0;
  let dragOffset = 0;
  let draggingElementHeight = 0;

  const handleMouseMove = (e: MouseEvent) => {
    if (draggingIndex === -1) return;
    const canvasRect = containerEl.getBoundingClientRect();
    const offsetY = e.clientY - canvasRect.top - dragHandleOffset;
    const children = Array.from(containerEl.children) as HTMLLIElement[];
    const draggingElementTop = children[draggingIndex].offsetTop - containerEl.offsetTop;
    dragOffset =
      clamp(offsetY, 0, canvasRect.height - draggingElementHeight) - draggingElementTop;
    const i = children.findIndex(element => {
      const elementTop = element.offsetTop - containerEl.offsetTop;
      return elementTop + element.clientHeight / 2 > offsetY;
    });
    // -1の場合は最後の要素(length - 1)にする
    dropDestinationIndex = (i + children.length) % children.length;
  };
  type MouseEventOnHTMLElement = MouseEvent & { currentTarget: HTMLElement };
  const handleMouseDown = (index: number) => (e: MouseEventOnHTMLElement) => {
    dropDestinationIndex = draggingIndex = index;
    const dragElement = containerEl.children[draggingIndex] as HTMLLIElement;
    draggingElementHeight = dragElement.offsetHeight;
    const dragHandleElement = e.currentTarget;
    const dragHandleElementTop = dragHandleElement.offsetTop - dragElement.offsetTop;
    dragHandleOffset = dragHandleElementTop + dragHandleElement.clientHeight / 2;
  };
  const handleMouseUp = () => {
    dispath("change", moveEl(values, draggingIndex, dropDestinationIndex));
    dragOffset = 0;
    dropDestinationIndex = draggingIndex = -1;
  };
  $: itemState = (index: number) => {
    if (draggingIndex === -1) {
      return;
    }
    if (index === draggingIndex) {
      return "dragging";
    }
    if (draggingIndex < index && index <= dropDestinationIndex) {
      return "up";
    }
    if (dropDestinationIndex <= index && index < draggingIndex) {
      return "down";
    }
    return "none";
  };
</script>

<svelte:window on:mousemove={handleMouseMove} on:mouseup={handleMouseUp} />

<ul bind:this={containerEl} style:--dragging-element-height="{draggingElementHeight}px">
  {#each values as value, index (key(value))}
    <li
      transition:slide|local
      data-transform={itemState(index)}
      style:--drag-offset={draggingIndex === index ? dragOffset + "px" : undefined}
    >
      <slot {value} {index} mousedown={handleMouseDown(index)} />
    </li>
  {/each}
</ul>

<style lang="scss">
  [data-transform] {
    transition: transform 0.1s;
    user-select: none;
  }
  [data-transform="dragging"] {
    transition: transform 0s;
    transform: translateY(var(--drag-offset));
  }
  [data-transform="up"] {
    transform: translateY(calc(0px - var(--dragging-element-height)));
  }
  [data-transform="down"] {
    transform: translateY(var(--dragging-element-height));
  }
</style>
