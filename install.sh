#!/bin/bash
FILE_URL="https://api.github.com/repos/lento-lang/lento/releases/latest"

# Install the latest Lento toolchain on Linux or macOS.

echo "Installing Lento toolchain..."

# Check if the OS is Linux or macOS.
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux
    echo "Installing on Linux..."
    # Download the latest release from GitHub.
    FILE_NAME="lento-linux.zip"
    FILE_URL="$FILE_URL/$FILE_NAME"
    curl -s $FILE_URL
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    echo "Installing on macOS..."
    # Download the latest release from GitHub.
    FILE_NAME="lento-macos.zip"
    FILE_URL="$FILE_URL/$FILE_NAME"
    curl -s $FILE_URL
else
    echo "Unknown OS. Please install manually."
    exit 1
fi

# Create the Lento directory.
mkdir -p ~/.lento
pushd "~/.lento"
mv $FILE_NAME .
unzip $FILE_NAME
rm $FILE_NAME

# Ask the user if they want to add Lento to their PATH in their .bashrc or .zshrc.
echo "Do you want to add Lento to your PATH? (y/n)"
read -r response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])+$ ]]; then
    # Add Lento to the PATH.
    echo "export PATH=$PATH:~/.lento" >>~/.bashrc
    echo "export PATH=$PATH:~/.lento" >>~/.zshrc
    echo "Lento has been added to your PATH. Restart your terminal to use Lento."
else
    echo "Lento has not been added to your PATH. You can still use Lento by running ~/.lento/lt."
fi
