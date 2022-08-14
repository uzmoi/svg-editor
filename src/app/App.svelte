<script lang="ts">
  import Header from "./Header.svelte";
  import Canvas from "./canvas/Canvas.svelte";
</script>

<div class="root">
  <div class="header">
    <Header />
  </div>
  <div class="main">
    <div class="center-panel">
      <Canvas />
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
