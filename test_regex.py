import re

content = """````text
```python unsafe
import urllib.request
response = urllib.request.urlopen('http://httpbin.org/get')
print('Successfully connected to the internet!')
```
````"""

# Pre-process: strip lines with 4 or more backticks
content = re.sub(r"(?m)^\s*`{4,}.*$\n?", "", content)
print("--- STRIPPED CONTENT ---")
print(content)
print("------------------------")

pattern = r"```(.*?)\n(.*?)```"
matches = list(re.finditer(pattern, content, re.DOTALL))
for m in matches:
    print('Header:', repr(m.group(1).strip()))
    print('Content:', repr(m.group(2).strip()))
