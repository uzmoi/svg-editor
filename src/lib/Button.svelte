<script lang="ts">
  interface $$Props extends svelte.JSX.HTMLProps<HTMLButtonElement> {
    variant?: Variant;
  }
  type Variant = "solid" | "outline" | "ghost";
  export let variant: Variant = "solid";
</script>

<button {...$$restProps} data-variant={variant} on:click>
  <slot />
</button>

<style lang="scss">
  button {
    padding-block: 0.3em;
    padding-inline: 0.8em;
    border: none;
    border-radius: 0.3em;
    background-color: transparent;
    cursor: pointer;
    user-select: none;
    color: inherit;
    font: inherit;
    font-size: 0.8em;
    transition-duration: 0.2s;
  }
  [data-variant="solid"] {
    background-color: var(--bg-secondary);
    transition-property: background-color;
    &:hover {
      background-color: var(--accent);
    }
  }
  [data-variant="outline"] {
    outline: 1px solid var(--bg-secondary);
    transition-property: outline;
    &:hover {
      outline-color: var(--accent);
    }
  }
  [data-variant="ghost"] {
    text-shadow: 0 0 0.2em var(--bg-secondary);
    transition-property: text-shadow;
    &:hover {
      text-shadow: 0 0 0.2em var(--accent);
    }
  }
</style>
