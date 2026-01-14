# Ephemeral Master Test Suite 

## PART 1: MARKDOWN SYNTAX TESTS
(Instructions: Copy the entire codeblock, including the top and bottom backticks.)

````

--- Standard & Scripting ---

```python
import sys
print(f"Markdown: Python {sys.version.split()[0]} on {sys.platform} - OK")
```

```node
console.log(`Markdown: Node ${process.version} on ${process.platform} - OK`);
```

```ruby
puts "Markdown: Ruby #{RUBY_VERSION} on #{RUBY_PLATFORM} - OK"
```

```bash
echo "Markdown: Bash $BASH_VERSION running as $(whoami) - OK"
```

```lua
print("Markdown: Lua " .. _VERSION .. " - OK")
```

```perl
printf "Markdown: Perl v%vd - OK\n", $^V;
```

```php
<?php echo "Markdown: PHP " . phpversion() . " - OK"; ?>
```

--- Systems & Compiled ---

```c
#include <stdio.h>
int main() { printf("Markdown: C (GCC) - OK\n"); return 0; }
```

```cpp
#include <iostream>
int main() { std::cout << "Markdown: C++ (G++) - OK" << std::endl; return 0; }
```

```rust
fn main() { println!("Markdown: Rust - OK"); }
```

```go
package main
import ("fmt"; "runtime")
func main() { fmt.Printf("Markdown: Go %s - OK\n", runtime.Version()) }
```

```fortran
program test
  print *, "Markdown: Fortran (GFortran) - OK"
end program test
```

```zig
const std = @import("std");
pub fn main() !void {
    std.debug.print("Markdown: Zig - OK\n", .{});
}
```

```v
fn main() { println('Markdown: Vlang - OK') }
```

```java
public class Main {
    public static void main(String[] args) {
        System.out.println("Markdown: Java " + System.getProperty("java.version") + " - OK");
    }
}
```

--- Modern & Golfing ---

```crystal
puts "Markdown: Crystal #{Crystal::VERSION} - OK"
```

```nim
echo "Markdown: Nim ", NimVersion, " - OK"
```

--- Science, Data & Engineering ---

```science
import sys
import numpy as np
print(f"Markdown: Science (Anaconda) | Numpy: {np.__version__} - OK")
```

```r
cat(sprintf("Markdown: R %s - OK\n", R.version.string))
```

```julia
println("Markdown: Julia $VERSION - OK")
```

```octave
printf("Markdown: Octave %s - OK\n", version());
```

```verilog
module test;
  initial begin
    $display("Markdown: Verilog (Icarus) - OK");
    $finish;
  end
endmodule
```

--- Functional & Lisp ---

```haskell
main = putStrLn "Markdown: Haskell - OK"
```

```lisp
(format t "Markdown: Common Lisp (SBCL) - OK~%")
```

```clojure
(println (str "Markdown: Clojure " (clojure-version) " - OK"))
```

```elixir
IO.puts "Markdown: Elixir #{System.version} - OK"
```

```ocaml
Printf.printf "Markdown: OCaml - OK\n";;
```

--- Logic, Stack & Retro ---

```prolog
:- initialization(main).
main :- write('Markdown: Prolog (SWI) - OK'), nl, halt.
```

```forth
." Markdown: Forth (Gforth) - OK" CR bye
```

```brainfuck
++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>++.>+.+++++++..+++.<<++.>+++++++++++++.>
```

```basic
PRINT "Markdown: BASIC (Bywater) - OK"
```

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           DISPLAY "Markdown: COBOL (GnuCOBOL) - OK".
           STOP RUN.
```

```pascal
program Test;
begin
  WriteLn('Markdown: Pascal (FreePascal) - OK');
end.
```

--- Web & Shells ---

```wat
(module
  (func $main (export "_start")
    ;; Output logic depends heavily on WASI implementation
    ;; This is a syntax check for the Wat parser
  )
)
```

```pwsh
Write-Output "Markdown: PowerShell $($PSVersionTable.PSVersion) - OK"
```

````

## PART 2: SHEBANG SYNTAX TESTS
(Instructions: Copy ONLY the code inside the block, not the backticks.)

--- Standard & Scripting ---

```text
#!python
import sys
print(f"Shebang: Python {sys.version.split()[0]} - OK")
```

```text
#!node
console.log(`Shebang: Node ${process.version} - OK`);
```

```text
#!ruby
puts "Shebang: Ruby #{RUBY_VERSION} - OK"
```

```text
#!bash
echo "Shebang: Bash $BASH_VERSION - OK"
```

```text
#!lua
print("Shebang: Lua " .. _VERSION .. " - OK")
```

```text
#!perl
printf "Shebang: Perl v%vd - OK\n", $^V;
```

```text
#!php
<?php echo "Shebang: PHP " . phpversion() . " - OK"; ?>
```

--- Systems & Compiled ---

```text
#!c
#include <stdio.h>
int main() { printf("Shebang: C - OK\n"); return 0; }
```

```text
#!cpp
#include <iostream>
int main() { std::cout << "Shebang: C++ - OK" << std::endl; return 0; }
```

```text
#!rust
fn main() { println!("Shebang: Rust - OK"); }
```

```text
#!go
package main
import ("fmt"; "runtime")
func main() { fmt.Printf("Shebang: Go %s - OK\n", runtime.Version()) }
```

```text
#!fortran
program test
  print *, "Shebang: Fortran - OK"
end program test
```

```text
#!zig
const std = @import("std");
pub fn main() !void {
    std.debug.print("Shebang: Zig - OK\n", .{});
}
```

```text
#!v
fn main() { println('Shebang: Vlang - OK') }
```

```text
#!java
public class Main {
    public static void main(String[] args) {
        System.out.println("Shebang: Java " + System.getProperty("java.version") + " - OK");
    }
}
```

--- Modern & Golfing ---

```text
#!crystal
puts "Shebang: Crystal #{Crystal::VERSION} - OK"
```

```text
#!nim
echo "Shebang: Nim ", NimVersion, " - OK"
```

--- Science, Data & Engineering ---

```text
#!science
import sys
print(f"Shebang: Science (Anaconda) {sys.version.split()[0]} - OK")
```

```text
#!r
cat(sprintf("Shebang: R %s - OK\n", R.version.string))
```

```text
#!julia
println("Shebang: Julia $VERSION - OK")
```

```text
#!octave
printf("Shebang: Octave %s - OK\n", version());
```

```text
#!verilog
module test;
  initial begin
    $display("Shebang: Verilog - OK");
    $finish;
  end
endmodule
```

--- Functional & Lisp ---

```text
#!haskell
main = putStrLn "Shebang: Haskell - OK"
```

```text
#!lisp
(format t "Shebang: Common Lisp - OK~%")
```

```text
#!clojure
(println (str "Shebang: Clojure " (clojure-version) " - OK"))
```

```text
#!elixir
IO.puts "Shebang: Elixir #{System.version} - OK"
```

```text
#!ocaml
Printf.printf "Shebang: OCam
