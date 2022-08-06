import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';
import analyze from 'rollup-plugin-analyzer';
export default defineConfig({
  plugins: [elmPlugin()],
  build: {
    rollupOptions: {
      plugins: [analyze({ summaryOnly: true })],
    },
  },
});
