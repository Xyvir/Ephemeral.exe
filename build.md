# Ephemeral Build Script

This script automatically cross-compiles Ephemeral for Windows using PyWine.

```pywine unsafe
# Download the exact commit source code
wget https://github.com/Xyvir/Ephemeral.exe/archive/BRANCH_OR_SHA.zip
unzip BRANCH_OR_SHA.zip
cd Ephemeral.exe-BRANCH_OR_SHA

# Install dependencies and build
wine python -m pip install -r requirements.txt pyinstaller Pillow
wine python -c "from PIL import Image, ImageDraw; img=Image.new('RGB', (64, 64), (30, 30, 30)); img.save('ephemeral.ico')"
sed -i "s/Version number (injected from the github workflow)/LOCAL_$(date +%Y%m%d-%H%M%S)/g" ephemeral.py
wine pyinstaller --noconsole --onefile --name Ephemeral --icon=ephemeral.ico ephemeral.py

# Export artifact
cp dist/Ephemeral.exe /output/Ephemeral.exe
```
