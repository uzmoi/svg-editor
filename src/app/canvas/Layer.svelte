<script lang="ts">
  import type { Layer } from "../store/layer";
  import { printPathCommand } from "../store/path-command";

  export let layer: Layer;

  $: d = layer.path.map(pathItem => printPathCommand(pathItem.command)).join("");
</script>

<path {d} />
