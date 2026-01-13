# Test helpers

# Counter for unique port allocation
.port_counter <- new.env(parent = emptyenv())
.port_counter$value <- 4000L

# Get a unique port for each test
get_test_port <- function() {
  .port_counter$value <- .port_counter$value + 1L
  .port_counter$value
}
