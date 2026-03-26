import subprocess

script = b"""sh -c 'cat > /tmp/out.txt && cat /tmp/out.txt' << 'EOF'
hello
EOF
"""
p = subprocess.run(['podman', 'run', '--rm', '-i', '--entrypoint', '', 'alpine', 'sh'], input=script, capture_output=True)
print("STDOUT:", p.stdout)
print("STDERR:", p.stderr)
