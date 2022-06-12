import { defineConfig } from "vite";
import { viteCommonjs } from "@originjs/vite-plugin-commonjs";

export default defineConfig({
    plugins: [
        viteCommonjs(),
    ],
    base: "/svg-editor/",
});
