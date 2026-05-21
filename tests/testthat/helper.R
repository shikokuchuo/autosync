# Test helpers

# Drain any leftover later callbacks from prior tests so they don't fire
# inside the next test's run_now and trip up nanonext's external-pointer
# bookkeeping. Bounded so a perpetually-rescheduling callback can't hang.
drain_later <- function(max_iters = 10L) {
  tryCatch(
    for (i in seq_len(max_iters)) {
      if (later::loop_empty()) {
        break
      }
      later::run_now(0.1)
    },
    error = function(e) NULL
  )
}
