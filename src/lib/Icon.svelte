<script lang="ts">
  export let name: string;
</script>

<svelte:head>
  <link
    rel="stylesheet"
    href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@48,400,0,0"
  />
</svelte:head>

<span class="material-symbols-outlined">{name}</span>

<style lang="scss">
  span {
    font-size: 1.4em;
    vertical-align: middle;
  }
</style>
