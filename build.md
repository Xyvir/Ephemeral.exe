# Ephemeral Build Script

This script automatically cross-compiles Ephemeral for Windows using PyWine.

```pywine unsafe
# Download the exact commit source code
curl -L -o main.zip https://github.com/Xyvir/Ephemeral.exe/archive/main.zip || (apt-get update && apt-get install -y curl && curl -L -o main.zip https://github.com/Xyvir/Ephemeral.exe/archive/main.zip)
unzip main.zip
cd Ephemeral.exe-main

# Install dependencies and build
wine python -m pip install -r requirements.txt pyinstaller Pillow
wine python -c "from PIL import Image, ImageDraw; img=Image.new('RGB', (64, 64), (30, 30, 30)); img.save('ephemeral.ico')"
sed -i "s/Version number (injected from the github workflow)/LOCAL_$(date +%Y%m%d-%H%M%S)/g" ephemeral.py
wine pyinstaller --noconsole --onefile --name Ephemeral --icon=ephemeral.ico ephemeral.py

# Export artifact
cp dist/Ephemeral.exe /output/Ephemeral.exe
```
