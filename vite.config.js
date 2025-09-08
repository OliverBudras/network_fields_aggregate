import { defineConfig } from "vite";

export default defineConfig({
  root: ".",             // Projekt-Root = rolling_network/
  publicDir: "public",   // JSON-Files aus public/windows werden geladen
  build: {
    outDir: "dist"
  },
  server: {
    open: "/index_rolling_agg.html"  // startet direkt diese HTML-Datei
  }
});
