import { defineConfig } from "vite";

export default defineConfig({
  base: '/network_fields_aggregate/',
 publicDir: "public",   // JSON-Files aus public/windows werden geladen
  build: {
    outDir: "dist"
  },
  server: {
    open: "/index.html"  // startet direkt diese HTML-Datei
  }
});
