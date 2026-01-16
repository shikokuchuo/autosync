import * as esbuild from "esbuild"
import path from "path"

// Plugin to redirect automerge imports to base64-inlined WASM version
const automergeBase64Plugin = {
  name: "automerge-base64",
  setup(build) {
    // Redirect @automerge/automerge to the base64 version
    build.onResolve({ filter: /^@automerge\/automerge$/ }, args => {
      return {
        path: path.resolve("node_modules/@automerge/automerge/dist/mjs/entrypoints/fullfat_base64.js")
      }
    })
    // Redirect @automerge/automerge/slim to the slim version
    build.onResolve({ filter: /^@automerge\/automerge\/slim$/ }, args => {
      return {
        path: path.resolve("node_modules/@automerge/automerge/dist/mjs/entrypoints/slim.js")
      }
    })
  }
}

await esbuild.build({
  entryPoints: ["src/index.js"],
  bundle: true,
  format: "iife",
  outfile: "../htmlwidgets/autosyncEditor.js",
  platform: "browser",
  plugins: [automergeBase64Plugin],
  define: {
    "process.env.NODE_ENV": '"production"'
  },
  minify: true
})
