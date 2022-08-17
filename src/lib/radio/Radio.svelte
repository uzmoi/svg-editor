<script lang="ts">
  import { getRadioContext } from "./RadioGroup.svelte";

  export let value: string;

  const { name, valueStore, onChange } = getRadioContext();

  let bind: string | undefined;
  $: if (bind !== undefined) {
    onChange(bind);
    bind = undefined;
  }
</script>

<input type="radio" id="radio-{name}-{value}" {name} {value} bind:group={bind} />
<label for="radio-{name}-{value}">
  <slot selected={$valueStore === value} />
</label>

<style lang="scss">
  input {
    opacity: 0;
    position: absolute;
  }
</style>
