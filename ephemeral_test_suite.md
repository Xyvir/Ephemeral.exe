# Ephemeral Test Suite


## PART 1: MARKDOWN SYNTAX TESTS
(Copy the codeblock including the backticks)

````
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

--- Hardware (HDL) ---

```verilog
module test;
  reg [7:0] a = 10;
  reg [7:0] b = 32;
  initial begin
    // Active check: Arithmetic and Simulation Time
    $display("Markdown: Verilog (Icarus) | 10 + 32 = %d | Time: %0t - OK", a + b, $time);
    $finish;
  end
endmodule
```

--- Functional & Scripting ---

```haskell
import System.Info (os, arch)
-- Active check: OS and Architecture from standard library
main = putStrLn $ "Markdown: Haskell running on " ++ os ++ "-" ++ arch ++ " - OK"
```

```lua
-- Active check: Lua Version global
print("Markdown: " .. _VERSION .. " - OK")
```

```perl
# Active check: Perl Version
printf "Markdown: Perl v%vd - OK\n", $^V;
```

```php
<?php
// Active check: PHP Version
echo "Markdown: PHP " . phpversion() . " - OK";
?>
```

--- Windows-like ---

```pwsh
# Active check: PowerShell Version Table
Write-Output "Markdown: PowerShell $($PSVersionTable.PSVersion.ToString()) - OK"
```
````


## PART 2: SHEBANG SYNTAX TESTS
(Copy the raw text, ensuring the #! line is first)


--- Standard Interpreted ---

```
#!python
import sys
print(f"Shebang: Python {sys.version.split()[0]} - OK")
```

```
#!node
console.log(`Shebang: Node ${process.version} - OK`);
```

```
#!bash
echo "Shebang: Bash $BASH_VERSION - OK"
```

```
#!ruby
puts "Shebang: Ruby #{RUBY_VERSION} - OK"
```

--- Science & Data ---

```
#!science
import sys
print(f"Shebang: Science (Anaconda) {sys.version.split()[0]} - OK")
```

```
#!octave
printf("Shebang: Octave %s - OK\n", version());
```

```
#!R
cat(sprintf("Shebang: %s - OK\n", R.version.string))
```

```
#!julia
println("Shebang: Julia $VERSION - OK")
```

--- Systems & Compiled ---

```
#!c
#include <stdio.h>
int main() { printf("Shebang: C compiled at %s %s - OK\n", __DATE__, __TIME__); return 0; }
```

```
#!cpp
#include <iostream>
int main() { std::cout << "Shebang: C++ compiled at " << __DATE__ << " - OK" << std::endl; return 0; }
```

```
#!fortran
program test
  use iso_fortran_env
  print *, "Shebang: Fortran Compiler: ", compiler_version()
end program test
```

```
#!rust
use std::env;
fn main() { println!("Shebang: Rust on {} - OK", env::consts::OS); }
```

```
#!go
package main
import ("fmt"; "runtime")
func main() { fmt.Printf("Shebang: Go %s - OK\n", runtime.Version()) }
```

--- Hardware (HDL) ---

```
#!verilog
module test;
  initial begin
    $display("Shebang: Verilog sim time %0t - OK", $time);
    $finish;
  end
endmodule
```
--- Functional & Scripting ---

```
#!haskell
import System.Info (os, arch)
main = putStrLn $ "Shebang: Haskell on " ++ os ++ " - OK"
```

```
#!lua
print("Shebang: " .. _VERSION .. " - OK")
```

```
#!perl
printf "Shebang: Perl v%vd - OK\n", $^V;
```

```
#!php
<?php echo "Shebang: PHP " . phpversion() . " - OK"; ?>
```

--- Windows-like ---
```
#!pwsh
Write-Output "Shebang: PowerShell $($PSVersionTable.PSVersion.ToString()) - OK"
```
