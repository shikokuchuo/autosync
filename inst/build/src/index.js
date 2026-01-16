import { Repo } from "@automerge/automerge-repo"
import { BrowserWebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket"
import { automergeSyncPlugin } from "@automerge/automerge-codemirror"
import { EditorView, basicSetup } from "codemirror"

HTMLWidgets.widget({
  name: 'autosyncEditor',
  type: 'output',

  factory: function(el, width, height) {
    let repo = null
    let view = null
    let handle = null
    let wsAdapter = null
    let initId = 0  // Track initialization to handle race conditions

    function showError(message) {
      el.innerHTML = '<div style="color: #c00; padding: 10px; border: 1px solid #c00; border-radius: 4px;">' +
        message + '</div>'
    }

    function cleanup() {
      if (view) {
        view.destroy()
        view = null
      }
      if (wsAdapter) {
        wsAdapter.disconnect()
        wsAdapter = null
      }
      repo = null
      handle = null
    }

    return {
      renderValue: function(x) {
        // Increment init ID to invalidate any pending async operations
        const currentInitId = ++initId

        // Clean up previous instance
        cleanup()
        el.innerHTML = ''

        // Create new repo with WebSocket connection
        wsAdapter = new BrowserWebSocketClientAdapter(x.serverUrl)
        repo = new Repo({
          network: [wsAdapter]
        })

        // Set up timeout for connection
        const timeout = x.timeout || 10000
        const timeoutId = setTimeout(() => {
          if (initId === currentInitId && !view) {
            showError('Connection timeout - server may be unavailable')
          }
        }, timeout)

        // repo.find() is async in v2.x - returns Promise<DocHandle>
        repo.find(`automerge:${x.docId}`).then((docHandle) => {
          clearTimeout(timeoutId)
          handle = docHandle

          // Check if this initialization is still current (handles race condition)
          if (initId !== currentInitId) return

          // Verify document has text field
          const doc = handle.doc()
          if (!doc || typeof doc.text === 'undefined') {
            showError('Document missing "text" field')
            return
          }

          // Get initial text content from the automerge document
          const initialText = doc.text.toString()

          view = new EditorView({
            doc: initialText,
            extensions: [
              basicSetup,
              automergeSyncPlugin({ handle, path: ["text"] }),
              EditorView.theme({ "&": { height: "100%" } }),
              EditorView.updateListener.of(update => {
                if (update.docChanged && HTMLWidgets.shinyMode) {
                  Shiny.setInputValue(el.id + "_content", update.state.doc.toString())
                }
              })
            ],
            parent: el
          })
        }).catch(err => {
          clearTimeout(timeoutId)
          if (initId === currentInitId) {
            showError('Failed to connect: ' + (err.message || 'Unknown error'))
          }
        })
      },

      resize: function(width, height) {
        // CodeMirror handles resize automatically
      },

      // Called when widget is removed from DOM
      destroy: function() {
        initId++  // Invalidate pending operations
        cleanup()
      }
    }
  }
})
