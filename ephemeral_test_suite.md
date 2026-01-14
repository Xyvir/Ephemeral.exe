# Ephemeral Master Test Suite

## PART 1: MARKDOWN SYNTAX TESTS
(Instructions: Copy the entire codeblock, including the top and bottom backticks.)

--- Standard Interpreted ---

```python
import sys
# Active check: Python version and platform
print(f"Markdown: Python {sys.version.split()[0]} running on {sys.platform} - OK")
```

```node
// Active check: Node version and platform
console.log(`Markdown: Node ${process.version} running on ${process.platform} - OK`);
```

```bash
# Active check: Bash version and current user
echo "Markdown: Bash $BASH_VERSION running as $(whoami) - OK"
```

```ruby
# Active check: Ruby version and host OS
puts "Markdown: Ruby #{RUBY_VERSION} on #{RUBY_PLATFORM} - OK"
```

--- Science & Data ---

```science
import numpy as np
import sys
# Active check: Python version + Numpy array creation
arr = np.array([1, 2, 3])
print(f"Markdown: Science (Anaconda) {sys.version.split()[0]} | Numpy Sum: {arr.sum()} - OK")
```

```octave
% Active check: Octave version
v = version();
printf("Markdown: Octave %s - OK\n", v);
```

```r
# Active check: R Version string
cat(sprintf("Markdown: %s - OK\n", R.version.string))
```

```julia
# Active check: Julia Version
println("Markdown: Julia $VERSION - OK")
```

--- Systems & Compiled ---

```c
#include <stdio.h>
// Active check: Standard C Version Macro
int main() {
    #ifdef __STDC_VERSION__
        printf("Markdown: C Standard Version %ld - OK\n", __STDC_VERSION__);
    #else
        printf("Markdown: C (Standard Macro not set, but compiled) - OK\n");
    #endif
    return 0;
}
```

```cpp
#include <iostream>
// Active check: C++ Standard Version Macro
int main() {
    std::cout << "Markdown: C++ Standard " << __cplusplus << " - OK" << std::endl;
    return 0;
}
```

```fortran
program test
  use iso_fortran_env
  implicit none
  ! Active check: Compiler version (F2008 standard)
  print *, "Markdown: Fortran Compiler: ", compiler_version()
end program test
```

```rust
use std::env;
fn main() {
    // Active check: Runtime OS Architecture
    println!("Markdown: Rust running on {} architecture - OK", env::consts::ARCH);
}
```

```go
package main
import (
    "fmt"
    "runtime"
)
func main() {
    // Active check: Go Runtime Version
    fmt.Printf("Markdown: Go %s - OK\n", runtime.Version())
}
```

--- Golfing & Modern Compiled ---

```crystal
# Active check: Crystal Version
puts "Markdown: Crystal #{Crystal::VERSION} - OK"
```

```nim
# Active check: Nim Version and OS
import os
echo "Markdown: Nim ", NimVersion, " on ", hostOS, " - OK"
```

--- Lisp Family ---

```lisp
;; Active check: Implementation Version (SBCL)
(format t "Markdown: Common Lisp ~a - OK~%" (lisp-implementation-version))
