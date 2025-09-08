import { defineConfig } from "vite";
import { resolve } from "path";

export default defineConfig({
  base: '/network_fields_aggregate/',
  build: {
    outDir: "dist",
    rollupOptions: {
      input: {
        page1: resolve(__dirname, 'index.html'),       // first page
        page2: resolve(__dirname, 'index_2.html')       // second page
      }
    }
  }
});

