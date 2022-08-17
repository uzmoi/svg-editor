<script lang="ts" context="module">
  import { createEventDispatcher, getContext, setContext } from "svelte";
  import { writable, type Readable } from "svelte/store";

  export interface RadioContext {
    name: string;
    valueStore: Readable<string>;
    onChange(value: string): void;
  }

  const radioContextKey = Symbol();
  export const getRadioContext = (): RadioContext => getContext(radioContextKey);
  const setRadioContext = (context: RadioContext) => setContext(radioContextKey, context);
</script>

<script lang="ts">
  const dispath = createEventDispatcher<{
    change: string;
  }>();

  export let value: string;
  export let name: string;

  const valueStore = writable(value);
  $: $valueStore = value;

  setRadioContext({
    name,
    valueStore,
    onChange(newValue) {
      value = newValue;
      dispath("change", newValue);
    },
  });
</script>

<slot />
